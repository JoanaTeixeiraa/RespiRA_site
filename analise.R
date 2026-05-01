library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(gtsummary)

respira <- read_csv(
  here::here("data", "raw", "respira.csv"),
  show_col_types = FALSE
) |>
  mutate(
    grupo = factor(
      grupo,
      levels = c("controlo", "intervencao"),
      labels = c("Controlo", "Intervenção")
    ),
    sexo = factor(
      sexo,
      levels = c("F", "M"),
      labels = c("Feminino", "Masculino")
    )
  )

tabela <- respira |>
  select(
    grupo,
    idade,
    sexo,
    act_baseline,
    peak_flow_baseline
  ) |>
  tbl_summary(
    by = grupo,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      idade ~ "Idade",
      sexo ~ "Sexo",
      act_baseline ~ "ACT no baseline",
      peak_flow_baseline ~ "Peak flow no baseline"
    ),
    missing = "no"
  ) |>
  add_overall() |>
  bold_labels()

grafico <- ggplot(respira, aes(x = grupo, y = act_semana12, fill = grupo)) +
  geom_boxplot(width = 0.55, alpha = 0.75, outlier.shape = NA) +
  geom_jitter(width = 0.08, alpha = 0.75, size = 2) +
  labs(
    title = "ACT às 12 semanas por grupo",
    x = "Grupo",
    y = "ACT às 12 semanas"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
