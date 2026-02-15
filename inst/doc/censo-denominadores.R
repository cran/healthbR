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
# censo_years()
# #> [1] "1970" "1980" "1991" "2000" "2010" "2022"

## -----------------------------------------------------------------------------
# censo_info(2022)

## -----------------------------------------------------------------------------
# # total population by state, Census 2022
# pop_state <- censo_populacao(year = 2022, territorial_level = "state")
# pop_state

## -----------------------------------------------------------------------------
# # population by sex, Brazil level
# pop_sex <- censo_populacao(
#   year = 2022,
#   variables = "sex",
#   territorial_level = "brazil"
# )
# pop_sex

## -----------------------------------------------------------------------------
# # population by age and sex
# pop_age_sex <- censo_populacao(
#   year = 2022,
#   variables = "age_sex",
#   territorial_level = "brazil"
# )
# pop_age_sex

## -----------------------------------------------------------------------------
# # population by race, 2022
# pop_race <- censo_populacao(
#   year = 2022,
#   variables = "race",
#   territorial_level = "state"
# )
# pop_race

## -----------------------------------------------------------------------------
# # population estimates 2015-2021
# estimates <- censo_estimativa(
#   year = 2015:2021,
#   territorial_level = "state"
# )
# estimates

## -----------------------------------------------------------------------------
# # step 1: get population denominator
# pop_2010 <- censo_populacao(
#   year = 2010,
#   variables = "total",
#   territorial_level = "state"
# )
# 
# # step 2: suppose you have mortality data (from SIM or other source)
# # deaths_by_state <- sim_data(year = 2010) |> count(state)
# 
# # step 3: calculate crude mortality rate
# # mortality_rate <- deaths_by_state |>
# #   left_join(pop_2010, by = "state") |>
# #   mutate(rate_per_100k = (n / population) * 100000)

## -----------------------------------------------------------------------------
# # list all available tables
# censo_sidra_tables()
# 
# # filter by theme
# censo_sidra_tables(theme = "disability")
# censo_sidra_tables(theme = "indigenous")
# 
# # search by keyword
# censo_sidra_search("quilombola")
# censo_sidra_search("saneamento")

## -----------------------------------------------------------------------------
# # population by race from table 9605
# pop_race_raw <- censo_sidra_data(
#   table = 9605,
#   territorial_level = "state",
#   year = 2022,
#   variable = 93,
#   classifications = list("86" = "allxt")
# )
# pop_race_raw

## -----------------------------------------------------------------------------
# # compare population across census years
# pop_2010 <- censo_populacao(year = 2010, territorial_level = "brazil")
# pop_2022 <- censo_populacao(year = 2022, territorial_level = "brazil")
# 
# # or use estimates for intercensitary years
# pop_series <- censo_estimativa(
#   year = 2001:2021,
#   territorial_level = "brazil"
# )
# pop_series

