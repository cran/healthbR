## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
# library(healthbR)
# library(dplyr)

## -----------------------------------------------------------------------------
# sim_years()
# 
# # include preliminary data
# sim_years(status = "all")

## -----------------------------------------------------------------------------
# sim_info()

## -----------------------------------------------------------------------------
# deaths_ac <- sim_data(year = 2022, uf = "AC")

## -----------------------------------------------------------------------------
# deaths_se <- sim_data(year = 2020:2022, uf = c("SP", "RJ", "MG"))

## -----------------------------------------------------------------------------
# # downloads all 27 states -- may take several minutes
# deaths_all <- sim_data(year = 2022)

## -----------------------------------------------------------------------------
# # Acute myocardial infarction (I21)
# mi <- sim_data(year = 2022, uf = "SP", cause = "I21")
# 
# # All cardiovascular diseases (Chapter IX)
# cardio <- sim_data(year = 2022, uf = "SP", cause = "I")
# 
# # All neoplasms (Chapter II)
# cancer <- sim_data(year = 2022, uf = "SP", cause = "C")

## -----------------------------------------------------------------------------
# deaths <- sim_data(
#   year = 2022,
#   uf = "SP",
#   vars = c("CAUSABAS", "DTOBITO", "SEXO", "IDADE", "RACACOR", "CODMUNRES")
# )

## -----------------------------------------------------------------------------
# deaths <- sim_data(year = 2022, uf = "AC")
# deaths$age_years  # numeric age in years
# 
# # disable decoding
# deaths_raw <- sim_data(year = 2022, uf = "AC", decode_age = FALSE)

## -----------------------------------------------------------------------------
# sim_dictionary()
# sim_dictionary("SEXO")
# sim_dictionary("RACACOR")

## -----------------------------------------------------------------------------
# sim_variables()
# sim_variables(search = "causa")

## -----------------------------------------------------------------------------
# deaths <- sim_data(year = 2022, uf = "SP")
# 
# deaths |>
#   mutate(chapter = substr(CAUSABAS, 1, 1)) |>
#   count(chapter, sort = TRUE)

## -----------------------------------------------------------------------------
# # deaths by age group
# deaths <- sim_data(year = 2022, uf = "SP") |>
#   filter(!is.na(age_years)) |>
#   mutate(age_group = cut(age_years,
#     breaks = c(0, 1, 5, 15, 30, 45, 60, 80, Inf),
#     right = FALSE
#   )) |>
#   count(age_group, name = "deaths")
# 
# # population from Census 2022
# pop <- censo_populacao(year = 2022, territorial_level = "state", geo_code = "35")
# 
# # join and calculate rates per 100,000

## -----------------------------------------------------------------------------
# # parsed types (default)
# deaths <- sim_data(year = 2022, uf = "AC")
# class(deaths$DTOBITO)  # Date
# 
# # all character (backward-compatible)
# deaths_raw <- sim_data(year = 2022, uf = "AC", parse = FALSE)

## -----------------------------------------------------------------------------
# sim_cache_status()
# sim_clear_cache()
# 
# # lazy query (requires arrow)
# lazy <- sim_data(year = 2022, uf = "SP", lazy = TRUE)
# lazy |>
#   filter(CAUSABAS >= "I20", CAUSABAS < "I26") |>
#   collect()

