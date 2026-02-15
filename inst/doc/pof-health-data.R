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
# pof_years()
# #> [1] "2002-2003" "2008-2009" "2017-2018"

## -----------------------------------------------------------------------------
# pof_info("2017-2018")

## -----------------------------------------------------------------------------
# # all registers
# pof_registers("2017-2018")
# 
# # only health-related registers
# pof_registers("2017-2018", health_only = TRUE)

## -----------------------------------------------------------------------------
# # list all variables in the domicilio register
# pof_variables("2017-2018", "domicilio")
# 
# # search for food security variables
# pof_variables("2017-2018", search = "ebia")
# 
# # search for weight-related variables
# pof_variables("2017-2018", "morador", search = "peso")

## -----------------------------------------------------------------------------
# domicilio <- pof_data("2017-2018", "domicilio")

## -----------------------------------------------------------------------------
# domicilio <- domicilio |>
#   mutate(
#     ebia = factor(
#       V6199,
#       levels = 1:4,
#       labels = c(
#         "Food security",
#         "Mild insecurity",
#         "Moderate insecurity",
#         "Severe insecurity"
#       )
#     )
#   )
# 
# # frequency table
# domicilio |>
#   count(ebia) |>
#   mutate(pct = n / sum(n) * 100)

## -----------------------------------------------------------------------------
# library(srvyr)
# 
# domicilio_svy <- pof_data("2017-2018", "domicilio", as_survey = TRUE)
# 
# # add EBIA categories
# domicilio_svy <- domicilio_svy |>
#   mutate(
#     ebia = factor(
#       V6199,
#       levels = 1:4,
#       labels = c(
#         "Food security",
#         "Mild insecurity",
#         "Moderate insecurity",
#         "Severe insecurity"
#       )
#     )
#   )
# 
# # weighted prevalence
# domicilio_svy |>
#   group_by(ebia) |>
#   summarize(
#     prevalence = survey_mean(na.rm = TRUE, vartype = "ci"),
#     n = unweighted(n())
#   )

## -----------------------------------------------------------------------------
# # food insecurity by state
# domicilio_svy |>
#   group_by(UF, ebia) |>
#   summarize(
#     prevalence = survey_mean(na.rm = TRUE, vartype = "ci"),
#     n = unweighted(n())
#   ) |>
#   filter(ebia == "Severe insecurity") |>
#   arrange(desc(prevalence))

## -----------------------------------------------------------------------------
# consumo <- pof_data("2017-2018", "consumo_alimentar")

## -----------------------------------------------------------------------------
# # total daily caloric intake per person
# consumo |>
#   group_by(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE) |>
#   summarize(
#     total_kcal = sum(ENERGIA_KCAL, na.rm = TRUE),
#     total_protein = sum(PROTEINA, na.rm = TRUE),
#     total_carb = sum(CARBOIDRATO, na.rm = TRUE),
#     total_fat = sum(LIPIDIO, na.rm = TRUE),
#     .groups = "drop"
#   ) |>
#   summarize(
#     mean_kcal = mean(total_kcal, na.rm = TRUE),
#     mean_protein = mean(total_protein, na.rm = TRUE),
#     mean_carb = mean(total_carb, na.rm = TRUE),
#     mean_fat = mean(total_fat, na.rm = TRUE)
#   )

## -----------------------------------------------------------------------------
# despesas <- pof_data("2017-2018", "despesa_individual")

## -----------------------------------------------------------------------------
# # explore expense categories
# despesas |>
#   count(QUADRO) |>
#   arrange(desc(n))

## -----------------------------------------------------------------------------
# # download morador (demographic data) and domicilio (household data)
# morador <- pof_data("2017-2018", "morador")
# domicilio <- pof_data("2017-2018", "domicilio")
# 
# # merge: add household-level EBIA to individual-level data
# morador_ebia <- morador |>
#   left_join(
#     domicilio |> select(COD_UPA, NUM_DOM, NUM_UC, V6199),
#     by = c("COD_UPA", "NUM_DOM", "NUM_UC")
#   ) |>
#   mutate(
#     ebia = factor(
#       V6199,
#       levels = 1:4,
#       labels = c(
#         "Food security",
#         "Mild insecurity",
#         "Moderate insecurity",
#         "Severe insecurity"
#       )
#     )
#   )
# 
# # food insecurity by age group
# morador_ebia |>
#   mutate(age_group = cut(V0403, breaks = c(0, 5, 12, 18, 30, 60, Inf))) |>
#   count(age_group, ebia) |>
#   group_by(age_group) |>
#   mutate(pct = n / sum(n) * 100)

## -----------------------------------------------------------------------------
# # check health modules by edition
# pof_info("2017-2018")  # EBIA + food consumption
# pof_info("2008-2009")  # anthropometry + food consumption
# pof_info("2002-2003")  # expenses only

## -----------------------------------------------------------------------------
# # check cached files
# pof_cache_status()
# 
# # clear cache if needed
# pof_clear_cache()

## -----------------------------------------------------------------------------
# # install arrow for optimized caching (recommended)
# install.packages("arrow")

