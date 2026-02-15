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
# pnadc_info()

## -----------------------------------------------------------------------------
# pnadc_modules()
# #> # A tibble: 4 x 4
# #>   module       name_pt                            name_en                years
# #>   <chr>        <chr>                              <chr>                  <list>
# #> 1 deficiencia  Pessoas com deficiencia             Persons with disab... <int>
# #> 2 habitacao    Caracteristicas dos domicilios      Housing character...  <int>
# #> 3 moradores    Caracteristicas gerais dos morad... General character...  <int>
# #> 4 aps          Atencao primaria a saude            Primary health care   <int>
# 
# # Years for a specific module
# pnadc_years("deficiencia")
# #> [1] 2019 2022 2024

## -----------------------------------------------------------------------------
# # Disability module, 2022
# df <- pnadc_data(module = "deficiencia", year = 2022)

## -----------------------------------------------------------------------------
# df <- pnadc_data(
#   module = "deficiencia",
#   year = 2022,
#   vars = c("UF", "V2007", "V2009", "V2010", "G001", "G003", "G006")
# )

## -----------------------------------------------------------------------------
# # Housing conditions across all available years
# df <- pnadc_data(module = "habitacao")

## -----------------------------------------------------------------------------
# # List variable names
# pnadc_variables(module = "deficiencia", year = 2022)
# 
# # Full dictionary with positions and widths
# pnadc_dictionaries(module = "deficiencia", year = 2022)

## -----------------------------------------------------------------------------
# # Requires srvyr package
# svy <- pnadc_data(
#   module = "deficiencia",
#   year = 2022,
#   as_survey = TRUE
# )
# 
# # Use srvyr verbs for proper variance estimation
# library(srvyr)
# svy |>
#   group_by(UF) |>
#   survey_mean(G001 == "1", na.rm = TRUE)  # disability prevalence by state

## -----------------------------------------------------------------------------
# df <- pnadc_data(module = "deficiencia", year = 2022)
# 
# # G001: "Tem dificuldade permanente de enxergar" (vision difficulty)
# # 1 = Sim, nao consegue de modo algum (cannot at all)
# # 2 = Sim, muita dificuldade (great difficulty)
# # 3 = Sim, alguma dificuldade (some difficulty)
# # 4 = Nao, nenhuma dificuldade (no difficulty)
# vision <- df |>
#   filter(G001 %in% c("1", "2", "3", "4")) |>
#   count(G001) |>
#   mutate(pct = n / sum(n) * 100)

## -----------------------------------------------------------------------------
# df <- pnadc_data(module = "habitacao", year = c(2016, 2019, 2022))
# 
# # Analyze water supply and sanitation trends
# # Variables vary by year -- check pnadc_variables() for each edition

## -----------------------------------------------------------------------------
# # APS module only available for 2022 Q2
# df <- pnadc_data(module = "aps", year = 2022)

## -----------------------------------------------------------------------------
# # Check cache
# pnadc_cache_status()
# 
# # Clear cache
# pnadc_clear_cache()
# 
# # Lazy evaluation
# lazy_df <- pnadc_data(
#   module = "deficiencia",
#   year = 2022,
#   lazy = TRUE,
#   backend = "arrow"
# )

