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
# vigitel_years()

## -----------------------------------------------------------------------------
# # load a single year
# df <- vigitel_data(2023)
# 
# # load multiple years
# df <- vigitel_data(2021:2023)

## -----------------------------------------------------------------------------
# dict <- vigitel_dictionary()
# dict

## -----------------------------------------------------------------------------
# # find weight-related variables
# dict |>
#   filter(stringr::str_detect(variable_name, "peso"))
# 
# # find diabetes-related variables
# dict |>
#   filter(stringr::str_detect(variable_name, "diab"))

## -----------------------------------------------------------------------------
# vigitel_variables(2023)

## -----------------------------------------------------------------------------
# library(srvyr)
# 
# # create survey design object
# vigitel_svy <- df |>
#   as_survey_design(weights = pesorake)
# 
# # calculate weighted prevalence of diabetes by city
# vigitel_svy |>
#   group_by(cidade) |>
#   summarize(
#     prevalence = survey_mean(diab == 1, na.rm = TRUE),
#     n = unweighted(n())
#   )

## -----------------------------------------------------------------------------
# # one-time conversion
# vigitel_convert_to_parquet(2015:2023)
# 
# # subsequent loads use parquet automatically
# df <- vigitel_data(2015:2023)

## -----------------------------------------------------------------------------
# # downloads happen in parallel (2-4 workers)
# df <- vigitel_data(2015:2023)

## -----------------------------------------------------------------------------
# # returns Arrow Dataset (not loaded into RAM)
# df_lazy <- vigitel_data(2015:2023, lazy = TRUE)
# 
# # operations are executed lazily
# result <- df_lazy |>
#   filter(cidade == 1, q8_anos >= 18) |>
#   select(q6, q8_anos, pesorake, diab, hart, imc) |>
#   collect()
# # only now data is loaded

## -----------------------------------------------------------------------------
# library(healthbR)
# library(dplyr)
# library(srvyr)
# 
# # 1. load data
# df <- vigitel_data(2023)
# 
# # 2. create survey design
# svy <- df |>
#   as_survey_design(weights = pesorake)
# 
# # 3. calculate prevalence by sex
# diabetes_by_sex <- svy |>
#   group_by(q6) |>
#   summarize(
#     prevalence = survey_mean(diab == 1, na.rm = TRUE, vartype = "ci"),
#     n = unweighted(n())
#   )
# 
# diabetes_by_sex

