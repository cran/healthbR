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
# sih_years()
# sih_years(status = "all")

## -----------------------------------------------------------------------------
# sih_info()

## -----------------------------------------------------------------------------
# # All months of 2022 for Acre
# admissions <- sih_data(year = 2022, uf = "AC")

## -----------------------------------------------------------------------------
# # First semester only
# admissions <- sih_data(year = 2022, uf = "SP", month = 1:6)
# 
# # Single month
# admissions <- sih_data(year = 2022, uf = "SP", month = 3)

## -----------------------------------------------------------------------------
# # Acute myocardial infarction (I21)
# mi <- sih_data(year = 2022, uf = "SP", diagnosis = "I21")
# 
# # All respiratory diseases (Chapter X)
# respiratory <- sih_data(year = 2022, uf = "SP", diagnosis = "J")
# 
# # Dengue (A90-A91)
# dengue_hosp <- sih_data(year = 2022, uf = "SP", diagnosis = "A9")

## -----------------------------------------------------------------------------
# admissions <- sih_data(
#   year = 2022,
#   uf = "SP",
#   month = 1,
#   vars = c("DIAG_PRINC", "DT_INTER", "DT_SAIDA", "SEXO",
#            "MORTE", "MUNIC_RES", "VAL_TOT")
# )

## -----------------------------------------------------------------------------
# sih_dictionary()
# sih_dictionary("SEXO")
# sih_dictionary("MORTE")

## -----------------------------------------------------------------------------
# sih_variables()
# sih_variables(search = "diag")
# sih_variables(search = "valor")

## -----------------------------------------------------------------------------
# admissions <- sih_data(year = 2022, uf = "SP", month = 1:6)
# 
# mortality <- admissions |>
#   mutate(chapter = substr(DIAG_PRINC, 1, 1)) |>
#   group_by(chapter) |>
#   summarise(
#     total = n(),
#     deaths = sum(MORTE == "1", na.rm = TRUE),
#     mortality_rate = deaths / total * 100
#   ) |>
#   arrange(desc(mortality_rate))

## -----------------------------------------------------------------------------
# admissions <- sih_data(year = 2022, uf = "SP", month = 1)
# 
# costs <- admissions |>
#   mutate(
#     chapter = substr(DIAG_PRINC, 1, 1),
#     cost = as.numeric(VAL_TOT)
#   ) |>
#   group_by(chapter) |>
#   summarise(
#     admissions = n(),
#     total_cost = sum(cost, na.rm = TRUE),
#     mean_cost = mean(cost, na.rm = TRUE)
#   ) |>
#   arrange(desc(total_cost))

## -----------------------------------------------------------------------------
# # respiratory admissions across all months
# resp <- sih_data(year = 2022, uf = "SP", diagnosis = "J")
# 
# seasonal <- resp |>
#   count(month, name = "admissions") |>
#   arrange(month)

## -----------------------------------------------------------------------------
# # parsed types (default)
# admissions <- sih_data(year = 2022, uf = "AC", month = 1)
# class(admissions$DT_INTER)  # Date
# class(admissions$VAL_TOT)   # double
# 
# # all character
# admissions_raw <- sih_data(year = 2022, uf = "AC", month = 1, parse = FALSE)

## -----------------------------------------------------------------------------
# sih_cache_status()
# sih_clear_cache()
# 
# # lazy query
# lazy <- sih_data(year = 2022, uf = "SP", lazy = TRUE)
# lazy |>
#   filter(MORTE == "1") |>
#   select(DIAG_PRINC, DT_INTER, SEXO, MUNIC_RES) |>
#   collect()

