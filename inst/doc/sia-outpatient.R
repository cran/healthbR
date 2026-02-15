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
# sia_years()
# sia_years(status = "all")

## -----------------------------------------------------------------------------
# sia_info()

## -----------------------------------------------------------------------------
# outpatient <- sia_data(year = 2022, uf = "AC")

## -----------------------------------------------------------------------------
# # chemotherapy/radiotherapy APAC records
# chemo <- sia_data(year = 2022, uf = "SP", type = "AM")

## -----------------------------------------------------------------------------
# outpatient <- sia_data(year = 2022, uf = "SP", month = 1:3)

## -----------------------------------------------------------------------------
# # Medical consultations (group 03.01)
# consults <- sia_data(year = 2022, uf = "SP", month = 1, procedure = "0301")
# 
# # Imaging exams (group 02.04)
# imaging <- sia_data(year = 2022, uf = "SP", month = 1, procedure = "0204")

## -----------------------------------------------------------------------------
# # Diabetes-related outpatient care (E10-E14)
# diabetes <- sia_data(year = 2022, uf = "SP", month = 1, diagnosis = "E1")

## -----------------------------------------------------------------------------
# outpatient <- sia_data(
#   year = 2022,
#   uf = "SP",
#   month = 1,
#   vars = c("PA_PROC_ID", "PA_CIDPRI", "PA_SEXO", "PA_IDADE",
#            "PA_MUNPCN", "PA_VALAPR")
# )

## -----------------------------------------------------------------------------
# sia_dictionary()
# sia_dictionary("PA_SEXO")

## -----------------------------------------------------------------------------
# sia_variables()
# sia_variables(search = "valor")
# 
# # variables for a specific type
# sia_variables(type = "AM")

## -----------------------------------------------------------------------------
# outpatient <- sia_data(year = 2022, uf = "SP", month = 1)
# 
# top_procedures <- outpatient |>
#   count(PA_PROC_ID, sort = TRUE) |>
#   head(20)

## -----------------------------------------------------------------------------
# outpatient <- sia_data(year = 2022, uf = "SP", month = 1)
# 
# spending <- outpatient |>
#   filter(!is.na(PA_CIDPRI), PA_CIDPRI != "") |>
#   mutate(
#     chapter = substr(PA_CIDPRI, 1, 1),
#     value = as.numeric(PA_VALAPR)
#   ) |>
#   group_by(chapter) |>
#   summarise(
#     records = n(),
#     total_value = sum(value, na.rm = TRUE)
#   ) |>
#   arrange(desc(total_value))

## -----------------------------------------------------------------------------
# chemo <- sia_data(year = 2022, uf = "SP", type = "AM", month = 1:6)
# 
# chemo |>
#   count(month, name = "records") |>
#   arrange(month)

## -----------------------------------------------------------------------------
# # parsed types (default)
# outpatient <- sia_data(year = 2022, uf = "AC", month = 1)
# class(outpatient$PA_VALAPR)  # double
# 
# # all character
# outpatient_raw <- sia_data(year = 2022, uf = "AC", month = 1, parse = FALSE)

## -----------------------------------------------------------------------------
# sia_cache_status()
# sia_clear_cache()
# 
# # lazy query
# lazy <- sia_data(year = 2022, uf = "SP", lazy = TRUE)
# lazy |>
#   filter(PA_CIDPRI >= "E10", PA_CIDPRI <= "E14") |>
#   collect()

