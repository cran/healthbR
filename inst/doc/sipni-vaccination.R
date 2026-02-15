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
# sipni_years()
# #> [1] 1994 1995 ... 2024 2025

## -----------------------------------------------------------------------------
# sipni_info()

## -----------------------------------------------------------------------------
# # doses applied in Acre, 2019
# ac_doses <- sipni_data(year = 2019, uf = "AC")
# ac_doses

## -----------------------------------------------------------------------------
# # vaccine codes
# sipni_dictionary("IMUNO")
# 
# # dose types
# sipni_dictionary("DOSE")
# 
# # age groups
# sipni_dictionary("FX_ETARIA")

## -----------------------------------------------------------------------------
# # vaccination coverage in Acre, 2019
# ac_coverage <- sipni_data(year = 2019, type = "CPNI", uf = "AC")
# ac_coverage

## -----------------------------------------------------------------------------
# # microdata for Acre, January 2024
# ac_micro <- sipni_data(year = 2024, uf = "AC", month = 1)
# ac_micro

## -----------------------------------------------------------------------------
# # DPNI variables (FTP)
# sipni_variables()
# 
# # CPNI variables (FTP)
# sipni_variables(type = "CPNI")
# 
# # API/CSV variables (2020+)
# sipni_variables(type = "API")
# 
# # search
# sipni_variables(search = "dose")

## -----------------------------------------------------------------------------
# # single month
# jan <- sipni_data(year = 2024, uf = "AC", month = 1)
# 
# # first quarter
# q1 <- sipni_data(year = 2024, uf = "AC", month = 1:3)
# 
# # all 12 months (default, downloads ~17 GB total)
# full_year <- sipni_data(year = 2024, uf = "AC")

## -----------------------------------------------------------------------------
# ac_2019 <- sipni_data(year = 2019, uf = "AC")
# 
# # decode immunobiological names
# imuno_labels <- sipni_dictionary("IMUNO") |>
#   select(code, label)
# 
# doses_by_vaccine <- ac_2019 |>
#   group_by(IMUNO) |>
#   summarize(total_doses = sum(as.integer(QT_DOSE), na.rm = TRUE),
#             .groups = "drop") |>
#   left_join(imuno_labels, by = c("IMUNO" = "code")) |>
#   arrange(desc(total_doses))
# 
# doses_by_vaccine

## -----------------------------------------------------------------------------
# # coverage data for Sao Paulo, 2015-2019
# sp_cov <- sipni_data(
#   year = 2015:2019,
#   type = "CPNI",
#   uf = "SP"
# )
# 
# # average coverage by year
# sp_cov |>
#   group_by(year) |>
#   summarize(
#     mean_coverage = mean(as.numeric(COBERT), na.rm = TRUE),
#     .groups = "drop"
#   )

## -----------------------------------------------------------------------------
# # COVID-19 vaccinations in Acre, January 2024
# ac_jan <- sipni_data(year = 2024, uf = "AC", month = 1)
# 
# # vaccines administered
# ac_jan |>
#   count(descricao_vacina, sort = TRUE)
# 
# # doses by sex
# ac_jan |>
#   count(tipo_sexo_paciente)
# 
# # age distribution
# ac_jan |>
#   mutate(age = as.integer(numero_idade_paciente)) |>
#   filter(!is.na(age)) |>
#   mutate(age_group = cut(age,
#                          breaks = c(0, 5, 12, 18, 30, 60, Inf),
#                          right = FALSE)) |>
#   count(age_group)

## -----------------------------------------------------------------------------
# # this downloads FTP (2019) + CSV (2024)
# mixed <- sipni_data(year = c(2019, 2024), uf = "AC", month = 1)
# 
# # columns from FTP (UPPERCASE) and CSV (snake_case) are combined
# # with NAs where columns don't overlap
# names(mixed)

## -----------------------------------------------------------------------------
# # parsed types (default)
# ac <- sipni_data(year = 2019, uf = "AC")
# class(ac$QT_DOSE)  # integer
# 
# # raw character columns
# ac_raw <- sipni_data(year = 2019, uf = "AC", parse = FALSE)

## -----------------------------------------------------------------------------
# # check cache status
# sipni_cache_status()
# 
# # clear cache if needed
# sipni_clear_cache()

## -----------------------------------------------------------------------------
# # lazy query for FTP data (requires arrow)
# sipni_lazy <- sipni_data(year = 2019, uf = "AC", lazy = TRUE)
# sipni_lazy |>
#   filter(QT_DOSE > 0) |>
#   select(IMUNO, DOSE, QT_DOSE) |>
#   collect()

