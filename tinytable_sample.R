library(dplyr)
library(tidyr)
library(tinytable)

dir_post <- here::here()
data <- arrow::read_parquet(file.path(dir_post, "data", "cleaned.parquet")) |>
  mutate(
    is_died = injury8 == "Died within 24 hours",
    is_hospitalized = injury8 %in% c(
      "Hospitalization after 24 hours",
      "Hospitalization within 24 hours",
      "Died within 24 hours"
    )
  )

tab_count <- data |>
  filter(!is.na(weather), !is.na(gender)) |>
  summarize(n = n(), .by = c(year, gender, weather)) |>
  pivot_wider(names_from = c(gender, year), values_from = n) |>
  arrange(weather) |>
  select(weather, starts_with("Men"), starts_with("Women"))

tt_count <- tab_count |>
  `colnames<-`(c("", rep(2019:2023, 2))) |>
  tt() |>
  group_tt(
    i = list("Good Weather" = 1, "Bad Weather" = 3),
    j = list("Men" = 2:6, "Women" = 7:11)
  ) |>
  style_tt(i = c(1, 4), bold = TRUE) |>
  format_tt(replace = "-")

tt_count |>
  theme_tt("tabular") |>
  save_tt(file.path(dir_post, "tex", "table_count.tex"),
    overwrite = TRUE
  )

tt_count |>
  save_tt(file.path(dir_post, "img", "table_count.pdf"),
    overwrite = TRUE
  )

tt_count |>
  save_tt(file.path(dir_post, "doc", "table_count.docx"),
    overwrite = TRUE
  )


library(modelsummary)
library(fixest)

setFixest_fml(..ctrl = ~ type_person + positive_alcohol + positive_drug | age_c + gender)

models <- list(
  "(1)" = feglm(xpd(is_hospitalized ~ ..ctrl),
    family = binomial(logit), data = data
  ),
  "(2)" = feglm(xpd(is_hospitalized ~ ..ctrl + type_vehicle),
    family = binomial(logit), data = data
  ),
  "(3)" = feglm(xpd(is_hospitalized ~ ..ctrl + type_vehicle + weather),
    family = binomial(logit), data = data
  ),
  "(4)" = feglm(xpd(is_died ~ ..ctrl),
    family = binomial(logit), data = data
  ),
  "(5)" = feglm(xpd(is_died ~ ..ctrl + type_vehicle),
    family = binomial(logit), data = data
  ),
  "(6)" = feglm(xpd(is_died ~ ..ctrl + type_vehicle + weather),
    family = binomial(logit), data = data
  )
)

modelsummary(models)

cm <- c(
  "type_personPassenger" = "Passenger",
  "type_personPedestrian" = "Pedestrian",
  "positive_alcoholTRUE" = "Positive Alcohol"
)

gm <- tibble(
  raw = c(
    "nobs", "FE: age_c", "FE: gender",
    "FE: type_vehicle", "FE: weather"
  ),
  clean = c(
    "Observations", "FE: Age Group", "FE: Gender",
    "FE: Type of Vehicle", "FE: Weather"
  ),
  fmt = c(0, 0, 0, 0, 0)
)

tt_reg <- modelsummary(models,
  stars = c("+" = .1, "*" = .05, "**" = .01),
  coef_map = cm,
  gof_map = gm
) |>
  group_tt(j = list(
    "Hospitalization" = 2:4,
    "Died within 24 hours" = 5:7
  ))

tt_reg |>
  save_tt(file.path(dir_post, "img", "table_reg.pdf"),
    overwrite = TRUE
  )
