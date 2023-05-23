
# ------------------------ libraries ------------------------

library("tidyverse")
# remotes::install_github("paleolimbot/tidyphreeqc")
library("tidyphreeqc")
# remotes::install_github("paleolimbot/chemr")
library("chemr")
library("furrr")
plan("multisession")

# ------------------------ read ------------------------

swaters <- read_csv("data-clean/data-sw-clean.csv") %>% 
  mutate(
    pH = coalesce(pH, `pH, lab`), 
    sulfate_mg_l = coalesce(`Sulfate_Dissolved_mg/L`, `Sulfate_Total_mg/L`)
  ) %>% 
  # remove rows (surface waters) containing missing values:
  filter(
    !is.na(`Magnesium_Total_mg/L`), 
    !is.na(`Alkalinity, total_as CaCO3_mg/L`)
  )

# ------------------------ functions ------------------------

add_dolomite <- function(river, q_dol, pco2, add_so4 = 0, db = phreeqc::phreeqc.dat, ...) {
  with(
    river,
    phr_input(
      phr_solution(
        Alkalinity = `Alkalinity, total_as CaCO3_mg/L` / mass("Ca0.5(CO3)0.5"),
        Al = `Aluminum_Total_mg/L` / mass("Al"),
        Ca = `Calcium_Total_mg/L` / mass("Ca"),
        Cu = `Copper_Total_mg/L` / mass("Cu"),
        Fe = `Iron_Total_mg/L` / mass("Fe"),
        Mg = `Magnesium_Total_mg/L` / mass("Mg"),
        Mn = `Manganese_Total_mg/L` / mass("Mn"),
        Na = `Sodium_Total_mg/L` / mass("Na"),
        pH = pH,
        `S(6)` = (sulfate_mg_l + add_so4) / mass("SO4"),
        Zn = `Zinc_Total_mg/L` / mass("Zn"),
        units = "mmol/kgw"
      ),
      phr_equilibrium_phases(
        Dolomite = c("0", q_dol),
        "CO2(g)" = pco2
      ),
      phr_selected_output(
        alkalinity = TRUE,
        temp = TRUE,
        molalities = "HCO3-",
        si = "CO2(g)",
        equilibrium_phases = "Dolomite",
        ...
      ),
      phr_use_db(db),
      phr_end()
    ) %>% 
      phr_run()
  ) %>% 
    as_tibble() 
}

# ------------------------ model ------------------------

index <- seq_len(nrow(swaters))

# fine grid:

pco2_range <- log10(1e-6 * c(316, 5e4))

grid <- crossing(
  dolomite = seq(0, 2.5e-3, length.out = 50), # moles
  pco2 = seq(pco2_range[1], pco2_range[2], length.out = 200) # log10(partial pressure, atm)
) %>% 
  rowwise()

out <- map(index, ~ grid) %>% 
  set_names(swaters$location) %>% 
  future_map2(
    index,
  ~ .x %>% 
    mutate(out = list(add_dolomite(swaters[.y,], dolomite, pco2))) %>% 
    ungroup() %>% 
    unnest(out),
  .progress = TRUE
) %>% 
  list_rbind(names_to = "location")
  
# ------------------------ write ------------------------

out %>% 
  # add "River" to the rivers:
  mutate(
    location = if_else(
      str_detect(location, "Lake"),
      location,
      paste(location, "River")
    )
  ) %>% 
  write_csv("data-clean/model-out.csv")
