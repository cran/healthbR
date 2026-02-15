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
# sinan_years()
# #> [1] 2007 2008 2009 ... 2022
# 
# sinan_years(status = "all")
# #> [1] 2007 2008 ... 2022 2023 2024

## -----------------------------------------------------------------------------
# sinan_info()

## -----------------------------------------------------------------------------
# # all available diseases
# sinan_diseases()
# 
# # search by name or code
# sinan_diseases(search = "dengue")
# sinan_diseases(search = "sifilis")
# sinan_diseases(search = "tuberculose")

## -----------------------------------------------------------------------------
# dengue_2022 <- sinan_data(year = 2022)
# dengue_2022

## -----------------------------------------------------------------------------
# tb <- sinan_data(year = 2020:2022, disease = "TUBE")
# tb

## -----------------------------------------------------------------------------
# # only key variables (faster and less memory)
# dengue_key <- sinan_data(
#   year = 2022,
#   disease = "DENG",
#   vars = c("DT_NOTIFIC", "CS_SEXO", "NU_IDADE_N",
#            "CS_RACA", "ID_MUNICIP", "CLASSI_FIN")
# )

## -----------------------------------------------------------------------------
# sinan_variables()
# sinan_variables(search = "sexo")
# sinan_variables(search = "municipio")

## -----------------------------------------------------------------------------
# # filter by UF
# dengue_sp <- sinan_data(year = 2022) |>
#   filter(SG_UF_NOT == "35")  # 35 = Sao Paulo
# 
# # filter by municipality
# dengue_rj_capital <- sinan_data(year = 2022) |>
#   filter(ID_MUNICIP == "330455")  # Rio de Janeiro capital

## -----------------------------------------------------------------------------
# # all coded variables
# sinan_dictionary()
# 
# # specific variable
# sinan_dictionary("CS_SEXO")
# sinan_dictionary("EVOLUCAO")
# sinan_dictionary("CLASSI_FIN")

## -----------------------------------------------------------------------------
# # final data only (default)
# sinan_years(status = "final")
# 
# # preliminary data
# sinan_years(status = "preliminary")
# 
# # both
# sinan_years(status = "all")

## -----------------------------------------------------------------------------
# dengue <- sinan_data(year = 2022, disease = "DENG") |>
#   filter(CLASSI_FIN %in% c("1", "5")) |>  # confirmed cases
# 
#   mutate(month = as.integer(format(DT_NOTIFIC, "%m")))
# 
# cases_by_month <- dengue |>
#   count(month) |>
#   arrange(month)
# 
# cases_by_month

## -----------------------------------------------------------------------------
# tb <- sinan_data(year = 2022, disease = "TUBE")
# 
# # decode age: 4th digit means years
# tb_age <- tb |>
#   filter(CLASSI_FIN == "1") |>
#   mutate(
#     age_unit = substr(NU_IDADE_N, 1, 1),
#     age_value = as.integer(substr(NU_IDADE_N, 2, 3)),
#     age_years = ifelse(age_unit == "4", age_value, NA_integer_),
#     age_group = cut(age_years,
#                     breaks = c(0, 15, 30, 45, 60, Inf),
#                     labels = c("<15", "15-29", "30-44", "45-59", "60+"),
#                     right = FALSE)
#   )
# 
# tb_age |>
#   filter(!is.na(age_group)) |>
#   count(CS_SEXO, age_group) |>
#   tidyr::pivot_wider(names_from = CS_SEXO, values_from = n)

## -----------------------------------------------------------------------------
# # step 1: confirmed dengue by UF
# dengue_uf <- sinan_data(year = 2022, disease = "DENG") |>
#   filter(CLASSI_FIN %in% c("1", "5")) |>
#   count(SG_UF_NOT, name = "cases")
# 
# # step 2: population from Census 2022
# pop <- censo_populacao(year = 2022, territorial_level = "state")
# 
# # step 3: calculate incidence rate per 100,000
# # incidence <- dengue_uf |>
# #   left_join(pop, by = ...) |>
# #   mutate(rate_100k = (cases / population) * 100000) |>
# #   arrange(desc(rate_100k))

## -----------------------------------------------------------------------------
# # parsed types (default)
# dengue <- sinan_data(year = 2022, disease = "DENG")
# class(dengue$DT_NOTIFIC)  # Date
# class(dengue$NU_ANO)      # integer
# 
# # raw character columns (backward-compatible)
# dengue_raw <- sinan_data(year = 2022, disease = "DENG", parse = FALSE)
# 
# # override specific columns
# dengue_custom <- sinan_data(
#   year = 2022,
#   col_types = list(DT_NOTIFIC = "character")
# )

## -----------------------------------------------------------------------------
# # check cache status
# sinan_cache_status()
# 
# # clear cache if needed
# sinan_clear_cache()

## -----------------------------------------------------------------------------
# # lazy query (requires arrow)
# dengue_lazy <- sinan_data(year = 2022, disease = "DENG", lazy = TRUE)
# dengue_lazy |>
#   filter(CLASSI_FIN == "1") |>
#   select(DT_NOTIFIC, CS_SEXO, NU_IDADE_N, ID_MUNICIP) |>
#   collect()

