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
# sisab_years()

## -----------------------------------------------------------------------------
# sisab_info()

## -----------------------------------------------------------------------------
# # APS coverage by state, January 2024
# aps_jan <- sisab_data(year = 2024, month = 1)
# aps_jan

## -----------------------------------------------------------------------------
# # APS coverage, all months of 2023
# aps_2023 <- sisab_data(year = 2023)
# aps_2023

## -----------------------------------------------------------------------------
# sb <- sisab_data(year = 2024, type = "sb", month = 6)
# sb

## -----------------------------------------------------------------------------
# acs <- sisab_data(year = 2023, type = "acs")
# acs

## -----------------------------------------------------------------------------
# pns <- sisab_data(year = 2022, type = "pns")
# pns

## -----------------------------------------------------------------------------
# national <- sisab_data(year = 2024, level = "brazil", month = 1)
# national

## -----------------------------------------------------------------------------
# regions <- sisab_data(year = 2024, level = "region", month = 1)
# regions

## -----------------------------------------------------------------------------
# states <- sisab_data(year = 2024, level = "uf", month = 1)
# states

## -----------------------------------------------------------------------------
# # filter by UF to avoid large downloads
# sp_munic <- sisab_data(
#   year = 2024,
#   level = "municipality",
#   uf = "SP",
#   month = 1
# )
# sp_munic

## -----------------------------------------------------------------------------
# # single state, single month
# sp_jan <- sisab_data(year = 2024, uf = "SP", month = 1)
# 
# # specific months
# sp_q1 <- sisab_data(year = 2024, uf = "SP", month = 1:3)
# 
# # multiple years
# sp_multi <- sisab_data(year = 2022:2024, uf = "SP", month = 6)

## -----------------------------------------------------------------------------
# # APS variables (default)
# sisab_variables()
# 
# # oral health variables
# sisab_variables(type = "sb")
# 
# # community agents
# sisab_variables(type = "acs")
# 
# # PNS variables
# sisab_variables(type = "pns")
# 
# # search
# sisab_variables(search = "cobertura")
# sisab_variables(search = "equipe")

## -----------------------------------------------------------------------------
# aps <- sisab_data(year = 2024, level = "uf", month = 6)
# 
# # coverage by state
# aps |>
#   select(sgUf, qtPopulacao, qtCobertura) |>
#   arrange(desc(qtCobertura))

## -----------------------------------------------------------------------------
# # monthly APS coverage, national level, 2020-2024
# trend <- sisab_data(
#   year = 2020:2024,
#   level = "brazil"
# )
# 
# trend |>
#   select(year, nuComp, qtCobertura) |>
#   arrange(year, nuComp)

## -----------------------------------------------------------------------------
# # municipality-level APS coverage in Minas Gerais
# mg_munic <- sisab_data(
#   year = 2024,
#   level = "municipality",
#   uf = "MG",
#   month = 6
# )
# 
# # distribution of coverage
# mg_munic |>
#   summarize(
#     n_municipalities = n(),
#     mean_coverage = mean(qtCobertura, na.rm = TRUE),
#     median_coverage = median(qtCobertura, na.rm = TRUE),
#     min_coverage = min(qtCobertura, na.rm = TRUE),
#     max_coverage = max(qtCobertura, na.rm = TRUE)
#   )

## -----------------------------------------------------------------------------
# # check cache status
# sisab_cache_status()
# 
# # clear cache if needed
# sisab_clear_cache()

