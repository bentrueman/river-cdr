
# ------------------------ libraries ------------------------

library("tidyverse")
library("glue")
library("ggtext")

# ------------------------ read ------------------------

# run R/model.R first to generate CSV containing model output
# source("R/model.R")

out <- read_csv("data-clean/model-out.csv")

# ------------------------ setup ------------------------

theme_set(
  theme_bw()
)

# ------------------------ plot ------------------------

fig_4 <- out %>% 
  ggplot(aes(dolomite, `m_HCO3-(mol/kgw)`, col = pco2, group = pco2)) + 
  facet_wrap(vars(location), ncol = 2) +
  scale_x_continuous(label = \(x) 1e3 * x) +
  scale_y_continuous(label = \(x) 1e3 * x) +
  scale_color_viridis_c(option = "mako") +
  guides(col = guide_colorbar()) +
  geom_label(
    data = . %>% 
      filter(state == "i_soln") %>% 
      distinct(location, `m_HCO3-(mol/kgw)`) %>% 
      mutate(
        x = .65e-3, 
        y = 6e-3, 
        label = glue("y=4x+{round(1e3*`m_HCO3-(mol/kgw)`, 2)}")
      ),
    label.r = unit(0, "lines"),
    label.size = 0,
    aes(x, y, label = label),
    inherit.aes = FALSE,
    alpha = .7,
    col = "grey35",
    size = 2
  ) +
  geom_line(data = . %>% filter(state == "react")) +
  geom_abline(
    data = . %>% 
      filter(state == "i_soln") %>% 
      distinct(location, `m_HCO3-(mol/kgw)`),
    aes(slope = 4, intercept = `m_HCO3-(mol/kgw)`),
    col = "grey35", linetype = 3
  ) +
  labs(
    x = expression("Dolomite (CaMg(CO"[3]*")"[2]*", mmol kg"^-1*")"),
    y = expression("[HCO"[3]^"-"*"] (mmol kg"^-1*")"),
    col = expression("log"[10]*"(pCO"[2]*", atm)"),
  )

# ------------------------ save ------------------------

ggsave("figures/fig-4.png", fig_4, dpi = 600, width = 5.5, height = 5)
