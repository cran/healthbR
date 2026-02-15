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
# pns_years()
# #> [1] "2013" "2019"

## -----------------------------------------------------------------------------
# pns_info(2019)

## -----------------------------------------------------------------------------
# pns_modules(year = 2019)
# #> # A tibble: 20 x 3
# #>    code  name_pt                          name_en
# #>    <chr> <chr>                            <chr>
# #>  1 A     Informacoes do domicilio         Household information
# #>  2 C     Caracteristicas dos moradores    Resident characteristics
# #>  3 ...

## -----------------------------------------------------------------------------
# # All modules for 2019
# df <- pns_data(year = 2019)
# 
# # Select specific variables
# df <- pns_data(year = 2019, vars = c("C006", "C008", "C009", "Q002", "Q00201"))

## -----------------------------------------------------------------------------
# # List all variables
# pns_variables(year = 2019)
# 
# # Filter by module
# pns_variables(year = 2019, module = "Q")
# 
# # Data dictionary
# pns_dictionary(year = 2019)

## -----------------------------------------------------------------------------
# # Browse all tables
# pns_sidra_tables()
# 
# # Filter by theme
# pns_sidra_tables(theme = "Chronic diseases")
# 
# # Search by keyword
# pns_sidra_search("diabetes")
# pns_sidra_search("tabagismo")

## -----------------------------------------------------------------------------
# # Table 7666: Self-reported diabetes prevalence
# diabetes <- pns_sidra_data(
#   table = 7666,
#   territorial_level = "state",
#   year = 2019
# )

## -----------------------------------------------------------------------------
# # National level
# pns_sidra_data(table = 7666, territorial_level = "brazil")
# 
# # By state
# pns_sidra_data(table = 7666, territorial_level = "state")
# 
# # By capital city
# pns_sidra_data(table = 7666, territorial_level = "capital")
# 
# # Specific state (e.g., Sao Paulo = 35)
# pns_sidra_data(table = 7666, territorial_level = "state", geo_code = "35")

## -----------------------------------------------------------------------------
# # Self-reported hypertension by state
# hypertension <- pns_sidra_data(
#   table = 7659,
#   territorial_level = "state",
#   year = 2019
# )

## -----------------------------------------------------------------------------
# df <- pns_data(
#   year = 2019,
#   vars = c("C006", "C008", "C009", "J001", "J007", "J009", "V0024", "UPA_PNS")
# )
# 
# # J001: Had a medical visit in the last 12 months?
# # C006: Sex, C008: Age, C009: Race
# access <- df |>
#   filter(J001 %in% c("1", "2")) |>
#   group_by(C006) |>
#   summarise(
#     visited = sum(J001 == "1"),
#     total = n(),
#     pct = visited / total * 100
#   )

## -----------------------------------------------------------------------------
# # Check cache
# pns_cache_status()
# 
# # Clear cache
# pns_clear_cache()
# 
# # Lazy evaluation for large datasets
# lazy_df <- pns_data(year = 2019, lazy = TRUE, backend = "arrow")

