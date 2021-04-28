library(tidyverse)
library(bbplot2)
library(grDevices)

# Обновляем датасет по этой ссылке данными из телеграма оперштаба Москвы 
# https://docs.google.com/spreadsheets/d/1lHcjxwMNJBDCLIoaBl5Fm3VOfelKnprO67JVTCrUrvo/edit?usp=sharing
# Затем загружаем датасет в формате csv в папку raw_data

msk <- read_csv("raw_data/вирус covid госпитализации москва - Sheet1.csv")

msk <- msk %>%
  pivot_longer(cols = c("hospitalizations", "msk_new_cases"),
               names_to = "indicator", 
               values_to = "value")

msk$indicator <- str_replace(msk$indicator, "hospitalizations", "Новые госпитализации")
msk$indicator <- str_replace(msk$indicator, "msk_new_cases", "Новые случаи")

msk_plot <- 
  msk %>%
  ggplot() +
  geom_line(aes(x = date, y = value, color = indicator), size = 1) +
  geom_hline(yintercept = 0, size = 1, color = "#333333") +
  bbc_style() + 
  theme(
    panel.grid.major.y = element_line(size = .3, color = "#dddddd"),
    panel.grid.major.x = element_line(size = .3, color = "#dddddd")
  ) +
  labs(
    title = "Новые случаи и госпитализации в Москве"
  ) +
  scale_color_manual(
    values = c("#FFA31E", "#1380a1")
  ) +
  scale_x_date(labels = c(
    "1 апреля 2020",
    "",
    "1 октября 2020",
    "",
    "1 апреля 2021"
  ), breaks = c(
    as.Date("2020-04-01", origin='1970-01-01'),
    as.Date("2020-07-01", origin='1970-01-01'),
    as.Date("2020-10-01", origin='1970-01-01'),
    as.Date("2021-01-01", origin='1970-01-01'),
    as.Date("2021-04-01", origin='1970-01-01')
  ))

finalise_plot(
  plot_name = msk_plot,
  save_filepath = "msk.png",
  source_name = "Источник: оперштаб Москвы"
)

cairo_pdf("msk.pdf", width = 670/72, height = 480/72)
print(msk_plot)
dev.off()