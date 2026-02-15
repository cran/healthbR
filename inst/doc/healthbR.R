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
# list_sources()
# #> # A tibble: 13 x 5
# #>    source  name                         description                    years       status
# #>    <chr>   <chr>                        <chr>                          <chr>       <chr>
# #>  1 vigitel VIGITEL                      Telephone survey on chronic... 2006-2024   available
# #>  2 pns     PNS - Pesquisa Nacional ...  National health survey (IBGE) 2013, 2019  available
# #>  ...

## -----------------------------------------------------------------------------
# # check available years
# vigitel_years()
# 
# # download data for a single year
# df <- vigitel_data(year = 2024)
# 
# # explore the data dictionary
# dict <- vigitel_dictionary()
# 
# # list variables
# vigitel_variables()

## -----------------------------------------------------------------------------
# # weighted prevalence of diabetes by city using srvyr
# library(srvyr)
# 
# df |>
#   as_survey_design(weights = pesorake) |>
#   group_by(cidade) |>
#   summarize(
#     prevalence = survey_mean(diab == 1, na.rm = TRUE),
#     n = unweighted(n())
#   )

## -----------------------------------------------------------------------------
# # microdata
# pns <- pns_data(year = 2019)
# 
# # explore modules and variables
# pns_modules(year = 2019)
# pns_dictionary(year = 2019)
# 
# # tabulated indicators from SIDRA API (no download needed)
# pns_sidra_search("diabetes")
# pns_sidra_data(table = 4487, territorial_level = "state", year = 2019)

## -----------------------------------------------------------------------------
# # list available modules
# pnadc_modules()
# 
# # download disability supplement
# defic <- pnadc_data(module = "deficiencia", year = 2019, quarter = 1)
# 
# # explore variables
# pnadc_variables(module = "deficiencia")

## -----------------------------------------------------------------------------
# # list available registers
# pof_registers(year = 2018)
# 
# # food consumption microdata
# consumo <- pof_data(year = 2018, register = "consumo_alimentar")
# 
# # food insecurity scale (EBIA)
# morador <- pof_data(year = 2018, register = "morador")
# 
# # variable dictionary for a specific register
# pof_dictionary(year = 2018, register = "morador")

## -----------------------------------------------------------------------------
# # population by state, sex, and age group (2022 Census)
# pop <- censo_populacao(year = 2022, territorial_level = "state")
# 
# # intercensitary population estimates (for years between censuses)
# est <- censo_estimativa(year = 2020, territorial_level = "state")
# 
# # query any Census SIDRA table directly
# censo_sidra_search("populacao")
# censo_sidra_data(table = 9514, territorial_level = "brazil", year = 2022)

## -----------------------------------------------------------------------------
# # deaths in Acre, 2022
# obitos <- sim_data(year = 2022, uf = "AC")
# 
# # filter by cause of death (CID-10 prefix)
# obitos_cardio <- sim_data(year = 2022, uf = "AC", cause = "I")
# 
# # explore variables and categories
# sim_variables()
# sim_dictionary("CAUSABAS")

## -----------------------------------------------------------------------------
# # births in Acre, 2022
# nascimentos <- sinasc_data(year = 2022, uf = "AC")
# 
# # filter by congenital anomaly (CID-10 prefix)
# anomalias <- sinasc_data(year = 2022, uf = "AC", anomaly = "Q")
# 
# sinasc_variables()
# sinasc_dictionary("PARTO")

## -----------------------------------------------------------------------------
# # admissions in Acre, January 2022
# internacoes <- sih_data(year = 2022, month = 1, uf = "AC")
# 
# # filter by principal diagnosis (CID-10 prefix)
# intern_resp <- sih_data(year = 2022, month = 1, uf = "AC", diagnosis = "J")
# 
# sih_variables()
# sih_dictionary("DIAG_PRINC")

## -----------------------------------------------------------------------------
# # outpatient production in Acre, January 2022 (default type: PA)
# ambulatorial <- sia_data(year = 2022, month = 1, uf = "AC")
# 
# # high-cost medications (APAC)
# medicamentos <- sia_data(year = 2022, month = 1, uf = "AC", type = "AM")
# 
# # filter by procedure or diagnosis
# sia_data(year = 2022, month = 1, uf = "AC", procedure = "0301")
# sia_data(year = 2022, month = 1, uf = "AC", diagnosis = "E11")

## -----------------------------------------------------------------------------
# # list available diseases
# sinan_diseases()
# 
# # search for a specific disease
# sinan_diseases(search = "dengue")
# 
# # dengue notifications, 2022
# dengue <- sinan_data(year = 2022, disease = "DENG")
# 
# # tuberculosis, 2020-2022
# tb <- sinan_data(year = 2020:2022, disease = "TUBE")
# 
# # explore variables and categories
# sinan_variables()
# sinan_dictionary("EVOLUCAO")

## -----------------------------------------------------------------------------
# dengue |>
#   filter(SG_UF_NOT == "35")  # Sao Paulo

## -----------------------------------------------------------------------------
# # see all file types
# cnes_info()
# 
# # establishments in Acre, January 2023
# estab <- cnes_data(year = 2023, month = 1, uf = "AC")
# 
# # hospital beds
# leitos <- cnes_data(year = 2023, month = 1, uf = "AC", type = "LT")
# 
# # health professionals
# prof <- cnes_data(year = 2023, month = 1, uf = "AC", type = "PF")
# 
# # explore variables and categories
# cnes_variables()
# cnes_dictionary("TP_UNID")

## -----------------------------------------------------------------------------
# # module overview
# sipni_info()
# 
# # FTP: doses applied in Acre, 2019 (aggregated)
# doses <- sipni_data(year = 2019, uf = "AC")
# 
# # FTP: vaccination coverage
# cobertura <- sipni_data(year = 2019, type = "CPNI", uf = "AC")
# 
# # API: microdata for Acre, January 2024
# micro <- sipni_data(year = 2024, uf = "AC", month = 1)
# 
# # explore variables for each data source
# sipni_variables()                  # FTP DPNI variables
# sipni_variables(type = "API")      # API microdata variables
# sipni_dictionary("IMUNO")          # FTP dictionary

## -----------------------------------------------------------------------------
# # module overview
# sisab_info()
# 
# # APS coverage by state, January 2024
# cobertura <- sisab_data(year = 2024, month = 1)
# 
# # national total, full year 2023
# sisab_data(year = 2023, level = "brazil")
# 
# # oral health coverage
# sisab_data(year = 2024, type = "sb", month = 6)
# 
# # municipality level for Sao Paulo
# sisab_data(year = 2024, level = "municipality", uf = "SP", month = 1)
# 
# # explore variables
# sisab_variables()
# sisab_variables(type = "sb")

## -----------------------------------------------------------------------------
# install.packages("arrow")

## -----------------------------------------------------------------------------
# # check what is cached
# sim_cache_status()
# vigitel_cache_status()
# 
# # clear a module's cache
# sim_clear_cache()
# 
# # use a custom cache directory (e.g., for temporary use)
# sim_data(year = 2022, uf = "AC", cache_dir = tempdir())

## -----------------------------------------------------------------------------
# library(dplyr)
# 
# # deaths by state
# deaths <- sim_data(year = 2022, uf = "AC") |>
#   count(name = "deaths")
# 
# # population by state
# pop <- censo_populacao(year = 2022, territorial_level = "state") |>
#   filter(nivel_territorial_codigo == "12")  # Acre
# 
# # crude mortality rate
# deaths$deaths / pop$valor * 100000

