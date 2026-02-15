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
# sinasc_years()
# 
# # include preliminary data
# sinasc_years(status = "all")

## -----------------------------------------------------------------------------
# sinasc_info()

## -----------------------------------------------------------------------------
# births <- sinasc_data(year = 2022, uf = "AC")

## -----------------------------------------------------------------------------
# births <- sinasc_data(year = 2020:2022, uf = c("SP", "RJ"))

## -----------------------------------------------------------------------------
# # Down syndrome (Q90)
# down <- sinasc_data(year = 2022, uf = "SP", anomaly = "Q90")
# 
# # All congenital anomalies (Chapter XVII)
# anomalies <- sinasc_data(year = 2022, uf = "SP", anomaly = "Q")

## -----------------------------------------------------------------------------
# births <- sinasc_data(
#   year = 2022,
#   uf = "SP",
#   vars = c("DTNASC", "SEXO", "PESO", "IDADEMAE", "GESTACAO",
#            "PARTO", "CONSULTAS", "CODMUNRES")
# )

## -----------------------------------------------------------------------------
# sinasc_dictionary()
# sinasc_dictionary("PARTO")
# sinasc_dictionary("GESTACAO")

## -----------------------------------------------------------------------------
# sinasc_variables()
# sinasc_variables(search = "mae")
# sinasc_variables(search = "peso")

## -----------------------------------------------------------------------------
# births <- sinasc_data(year = 2022, uf = c("SP", "RJ", "MG", "BA", "RS"))
# 
# lbw <- births |>
#   filter(!is.na(PESO), PESO != "0") |>
#   mutate(
#     weight = as.numeric(PESO),
#     low_weight = weight < 2500
#   ) |>
#   group_by(uf_source) |>
#   summarise(
#     total = n(),
#     low_weight_n = sum(low_weight),
#     low_weight_pct = low_weight_n / total * 100
#   )

## -----------------------------------------------------------------------------
# births <- sinasc_data(year = 2018:2022, uf = "SP",
#                       vars = c("PARTO", "CODMUNRES"))
# 
# cesarean <- births |>
#   filter(PARTO %in% c("1", "2")) |>
#   group_by(year) |>
#   summarise(
#     vaginal = sum(PARTO == "1"),
#     cesarean = sum(PARTO == "2"),
#     cesarean_rate = cesarean / (vaginal + cesarean) * 100
#   )

## -----------------------------------------------------------------------------
# births <- sinasc_data(year = 2022, uf = "SP")
# 
# teen <- births |>
#   filter(!is.na(IDADEMAE)) |>
#   mutate(
#     mother_age = as.integer(IDADEMAE),
#     teen_mother = mother_age < 20
#   ) |>
#   summarise(
#     total = n(),
#     teen_n = sum(teen_mother, na.rm = TRUE),
#     teen_pct = teen_n / total * 100
#   )

## -----------------------------------------------------------------------------
# # parsed types (default)
# births <- sinasc_data(year = 2022, uf = "AC")
# class(births$DTNASC)  # Date
# class(births$PESO)    # integer
# 
# # all character
# births_raw <- sinasc_data(year = 2022, uf = "AC", parse = FALSE)

## -----------------------------------------------------------------------------
# sinasc_cache_status()
# sinasc_clear_cache()
# 
# # lazy query
# lazy <- sinasc_data(year = 2022, uf = "SP", lazy = TRUE)
# lazy |>
#   filter(PARTO == "2") |>
#   collect()

