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
# cnes_years()
# #> [1] 2005 2006 ... 2023
# 
# cnes_years(status = "all")
# #> [1] 2005 2006 ... 2023 2024

## -----------------------------------------------------------------------------
# cnes_info()

## -----------------------------------------------------------------------------
# # all establishments in Acre, January 2023
# ac_jan <- cnes_data(year = 2023, month = 1, uf = "AC")
# ac_jan

## -----------------------------------------------------------------------------
# leitos <- cnes_data(year = 2023, month = 1, uf = "AC", type = "LT")
# leitos

## -----------------------------------------------------------------------------
# prof <- cnes_data(year = 2023, month = 1, uf = "AC", type = "PF")
# prof

## -----------------------------------------------------------------------------
# # single month
# jan <- cnes_data(year = 2023, month = 1, uf = "AC")
# 
# # first semester
# sem1 <- cnes_data(year = 2023, month = 1:6, uf = "AC")
# 
# # specific months
# q1_q3 <- cnes_data(year = 2023, month = c(3, 6, 9), uf = "AC")
# 
# # all 12 months (default when month = NULL)
# full_year <- cnes_data(year = 2023, uf = "AC")

## -----------------------------------------------------------------------------
# # only key variables (faster)
# cnes_data(
#   year = 2023, month = 1, uf = "AC",
#   vars = c("CNES", "CODUFMUN", "TP_UNID", "VINC_SUS")
# )

## -----------------------------------------------------------------------------
# # all coded variables
# cnes_dictionary()
# 
# # facility types (22 categories)
# cnes_dictionary("TP_UNID")
# 
# # administrative sphere
# cnes_dictionary("ESFERA_A")

## -----------------------------------------------------------------------------
# # get facility type labels
# tp_unid_labels <- cnes_dictionary("TP_UNID") |>
#   select(code, label)
# 
# # join to data
# ac_facilities <- cnes_data(year = 2023, month = 1, uf = "AC") |>
#   left_join(tp_unid_labels, by = c("TP_UNID" = "code")) |>
#   rename(facility_type = label)
# 
# ac_facilities |>
#   count(facility_type, sort = TRUE)

## -----------------------------------------------------------------------------
# ac <- cnes_data(year = 2023, month = 1, uf = "AC")
# 
# sus_by_type <- ac |>
#   filter(VINC_SUS == "1") |>
#   count(TP_UNID, sort = TRUE)
# 
# # add labels
# tp_labels <- cnes_dictionary("TP_UNID") |>
#   select(code, label)
# 
# sus_by_type |>
#   left_join(tp_labels, by = c("TP_UNID" = "code"))

## -----------------------------------------------------------------------------
# # step 1: count beds by UF (December snapshot)
# beds <- cnes_data(year = 2023, month = 12, type = "LT") |>
#   group_by(uf_source) |>
#   summarize(total_beds = n(), .groups = "drop")
# 
# # step 2: population from Census 2022
# pop <- censo_populacao(year = 2022, territorial_level = "state")
# 
# # step 3: calculate beds per 1,000 inhabitants
# # beds_rate <- beds |>
# #   left_join(pop, by = ...) |>
# #   mutate(beds_per_1000 = (total_beds / population) * 1000) |>
# #   arrange(desc(beds_per_1000))

## -----------------------------------------------------------------------------
# # quarterly snapshots for Sao Paulo
# sp_quarterly <- cnes_data(
#   year = 2020:2023,
#   month = c(3, 6, 9, 12),
#   uf = "SP"
# )
# 
# facility_trend <- sp_quarterly |>
#   group_by(year, month) |>
#   summarize(
#     total = n(),
#     sus_linked = sum(VINC_SUS == "1", na.rm = TRUE),
#     .groups = "drop"
#   ) |>
#   arrange(year, month)
# 
# facility_trend

## -----------------------------------------------------------------------------
# # parsed types (default)
# ac <- cnes_data(year = 2023, month = 1, uf = "AC")
# class(ac$COMPETEN)  # Date
# 
# # raw character columns
# ac_raw <- cnes_data(year = 2023, month = 1, uf = "AC", parse = FALSE)

## -----------------------------------------------------------------------------
# # check cache status
# cnes_cache_status()
# 
# # clear cache if needed
# cnes_clear_cache()

## -----------------------------------------------------------------------------
# # lazy query (requires arrow)
# cnes_lazy <- cnes_data(year = 2023, uf = "AC", lazy = TRUE)
# cnes_lazy |>
#   filter(VINC_SUS == "1", month == 1L) |>
#   select(CNES, CODUFMUN, TP_UNID) |>
#   collect()

