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
# #> [1] 2006 2007 2008 ... 2023 2024

## -----------------------------------------------------------------------------
# vigitel_info()

## -----------------------------------------------------------------------------
# df <- vigitel_data()

## -----------------------------------------------------------------------------
# df <- vigitel_data(year = 2020:2024)

## -----------------------------------------------------------------------------
# df <- vigitel_data(year = 2024, vars = c("cidade", "sexo", "idade", "pesorake",
#                                           "q6", "q7", "q9"))

## -----------------------------------------------------------------------------
# df_dta <- vigitel_data(format = "dta")  # default, with labels
# df_csv <- vigitel_data(format = "csv")  # alternative

## -----------------------------------------------------------------------------
# vigitel_dictionary()

## -----------------------------------------------------------------------------
# vigitel_variables()

## -----------------------------------------------------------------------------
# # Download smoking-related variables
# df <- vigitel_data(
#   year = 2006:2024,
#   vars = c("ano", "cidade", "sexo", "pesorake", "q6")
# )
# 
# # q6: "Atualmente, o(a) sr(a) fuma?" (1 = sim, 2 = nao)
# smoking <- df |>
#   filter(q6 %in% c("1", "2")) |>
#   group_by(ano) |>
#   summarise(
#     smokers = sum(pesorake[q6 == "1"], na.rm = TRUE),
#     total = sum(pesorake, na.rm = TRUE),
#     prevalence = smokers / total * 100
#   )

## -----------------------------------------------------------------------------
# df <- vigitel_data(
#   year = 2024,
#   vars = c("cidade", "sexo", "pesorake", "q8", "q9")
# )
# 
# # q8 = weight (kg), q9 = height (cm)
# # BMI >= 30 = obesity
# obesity <- df |>
#   filter(!is.na(q8), !is.na(q9), q9 > 0) |>
#   mutate(
#     bmi = as.numeric(q8) / (as.numeric(q9) / 100)^2,
#     obese = bmi >= 30
#   ) |>
#   group_by(cidade) |>
#   summarise(
#     prevalence = weighted.mean(obese, as.numeric(pesorake), na.rm = TRUE) * 100
#   ) |>
#   arrange(desc(prevalence))

## -----------------------------------------------------------------------------
# # First call downloads (~30 seconds)
# df <- vigitel_data(year = 2024)
# 
# # Second call loads from cache (instant)
# df <- vigitel_data(year = 2024)
# 
# # Check cache status
# vigitel_cache_status()
# 
# # Clear cache if needed
# vigitel_clear_cache()

## -----------------------------------------------------------------------------
# lazy_df <- vigitel_data(lazy = TRUE, backend = "arrow")

