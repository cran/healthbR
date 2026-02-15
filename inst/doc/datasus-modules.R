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
# # available years
# sim_years()
# #> [1] 1996 1997 1998 ... 2023
# 
# # module information (data source, key variables, usage tips)
# sim_info()
# 
# # list all variables with descriptions
# sim_variables()
# 
# # search for a specific variable
# sim_variables(search = "causa")
# 
# # data dictionary with category labels
# sim_dictionary("SEXO")

## -----------------------------------------------------------------------------
# # all deaths in Acre, 2022
# obitos_ac <- sim_data(year = 2022, uf = "AC")
# obitos_ac

## -----------------------------------------------------------------------------
# # deaths from acute myocardial infarction (I21)
# obitos_iam <- sim_data(year = 2022, uf = "AC", cause = "I21")
# 
# # all cardiovascular deaths (chapter I)
# obitos_cardio <- sim_data(year = 2022, uf = "AC", cause = "I")

## -----------------------------------------------------------------------------
# obitos_ac <- sim_data(year = 2022, uf = "AC")
# 
# obitos_ac |>
#   mutate(chapter = substr(CAUSABAS, 1, 1)) |>
#   count(chapter, sort = TRUE)

## -----------------------------------------------------------------------------
# nasc_ac <- sinasc_data(year = 2022, uf = "AC")
# nasc_ac

## -----------------------------------------------------------------------------
# # births with any congenital anomaly (chapter Q)
# anomalias <- sinasc_data(year = 2022, uf = "AC", anomaly = "Q")

## -----------------------------------------------------------------------------
# nasc_ac <- sinasc_data(year = 2022, uf = "AC")
# 
# nasc_ac |>
#   mutate(peso_num = as.numeric(PESO)) |>
#   filter(!is.na(peso_num), peso_num > 0) |>
#   mutate(weight_group = case_when(
#     peso_num < 1500 ~ "Very low (<1500g)",
#     peso_num < 2500 ~ "Low (1500-2499g)",
#     peso_num < 4000 ~ "Normal (2500-3999g)",
#     TRUE            ~ "High (>=4000g)"
#   )) |>
#   count(weight_group)

## -----------------------------------------------------------------------------
# # admissions in Acre, January 2022
# intern_jan <- sih_data(year = 2022, month = 1, uf = "AC")
# intern_jan

## -----------------------------------------------------------------------------
# # single month
# sih_data(year = 2022, month = 6, uf = "AC")
# 
# # first semester
# sih_data(year = 2022, month = 1:6, uf = "AC")
# 
# # all 12 months (default when month = NULL -- downloads 12 files per UF)
# sih_data(year = 2022, uf = "AC")

## -----------------------------------------------------------------------------
# # respiratory admissions (chapter J)
# resp <- sih_data(year = 2022, month = 1, uf = "AC", diagnosis = "J")
# 
# # pneumonia specifically (J12-J18)
# pneum <- sih_data(year = 2022, month = 1, uf = "AC",
#                   diagnosis = c("J12", "J13", "J14", "J15", "J16", "J17", "J18"))

## -----------------------------------------------------------------------------
# intern <- sih_data(year = 2022, month = 1, uf = "AC")
# 
# intern |>
#   mutate(chapter = substr(DIAG_PRINC, 1, 1)) |>
#   count(chapter, sort = TRUE)

## -----------------------------------------------------------------------------
# # outpatient production in Acre, January 2022 (default type = "PA")
# ambul_jan <- sia_data(year = 2022, month = 1, uf = "AC")
# ambul_jan
# 
# # different file type: high-cost medications
# med <- sia_data(year = 2022, month = 1, uf = "AC", type = "AM")

## -----------------------------------------------------------------------------
# # filter by SIGTAP procedure code (prefix match on PA_PROC_ID)
# consult <- sia_data(year = 2022, month = 1, uf = "AC", procedure = "0301")
# 
# # filter by CID-10 diagnosis (prefix match on PA_CIDPRI)
# resp <- sia_data(year = 2022, month = 1, uf = "AC", diagnosis = "J")

## -----------------------------------------------------------------------------
# ambul <- sia_data(year = 2022, month = 1, uf = "AC")
# 
# ambul |>
#   mutate(proc_group = substr(PA_PROC_ID, 1, 2)) |>
#   count(proc_group, sort = TRUE)

## -----------------------------------------------------------------------------
# sinan_diseases()
# #> # A tibble: 31 x 3
# #>    code  name                      description
# #>    <chr> <chr>                     <chr>
# #>  1 DENG  Dengue                    Dengue
# #>  2 CHIK  Chikungunya               Febre de Chikungunya
# #>  3 ZIKA  Zika                      Zika virus
# #>  4 TUBE  Tuberculose               Tuberculose
# #>  ...
# 
# # search for a specific disease
# sinan_diseases(search = "sifilis")

## -----------------------------------------------------------------------------
# # dengue notifications, 2022 (default disease)
# dengue <- sinan_data(year = 2022)
# 
# # tuberculosis notifications, 2020-2022
# tb <- sinan_data(year = 2020:2022, disease = "TUBE")
# 
# # select specific variables
# sinan_data(year = 2022, disease = "DENG",
#            vars = c("DT_NOTIFIC", "CS_SEXO", "NU_IDADE_N",
#                     "ID_MUNICIP", "CLASSI_FIN"))

## -----------------------------------------------------------------------------
# dengue <- sinan_data(year = 2022)
# 
# # filter by state of notification
# dengue_sp <- dengue |>
#   filter(SG_UF_NOT == "35")  # Sao Paulo (IBGE code)
# 
# # or by municipality
# dengue_rio <- dengue |>
#   filter(substr(ID_MUNICIP, 1, 2) == "33")  # Rio de Janeiro state

## -----------------------------------------------------------------------------
# dengue <- sinan_data(year = 2022, disease = "DENG")
# 
# dengue |>
#   filter(CLASSI_FIN %in% c("1", "5")) |>  # confirmed cases
#   mutate(month = substr(DT_NOTIFIC, 4, 5)) |>
#   count(month, sort = TRUE)

## -----------------------------------------------------------------------------
# # establishments in Acre, January 2023
# estab <- cnes_data(year = 2023, month = 1, uf = "AC")
# 
# # hospital beds
# leitos <- cnes_data(year = 2023, month = 1, uf = "AC", type = "LT")
# 
# # health professionals
# prof <- cnes_data(year = 2023, month = 1, uf = "AC", type = "PF")

## -----------------------------------------------------------------------------
# estab <- cnes_data(year = 2023, month = 1, uf = "AC")
# 
# estab |>
#   count(TP_UNID, sort = TRUE) |>
#   left_join(
#     cnes_dictionary("TP_UNID") |> select(code, label),
#     by = c("TP_UNID" = "code")
#   )

## -----------------------------------------------------------------------------
# # FTP: doses applied in Acre, 2019 (default type = "DPNI")
# doses_ac <- sipni_data(year = 2019, uf = "AC")
# doses_ac
# 
# # FTP: vaccination coverage
# cob_ac <- sipni_data(year = 2019, type = "CPNI", uf = "AC")
# 
# # API: individual-level microdata, Acre, January 2024
# micro_ac <- sipni_data(year = 2024, uf = "AC", month = 1)
# micro_ac

## -----------------------------------------------------------------------------
# doses <- sipni_data(year = 2019, uf = "AC")
# 
# doses |>
#   group_by(IMUNO) |>
#   summarize(total_doses = sum(as.numeric(QT_DOSE), na.rm = TRUE)) |>
#   arrange(desc(total_doses)) |>
#   left_join(
#     sipni_dictionary("IMUNO") |> select(code, label),
#     by = c("IMUNO" = "code")
#   )

## -----------------------------------------------------------------------------
# # step 1: count cardiovascular deaths in Sao Paulo, 2022
# obitos_cardio <- sim_data(year = 2022, uf = "SP", cause = "I")
# n_obitos <- nrow(obitos_cardio)
# 
# # step 2: get population denominator from Census 2022
# pop_sp <- censo_populacao(year = 2022, territorial_level = "state") |>
#   filter(grepl("Paulo", territorial_unit))
# 
# # step 3: calculate rate
# taxa_mortalidade <- n_obitos / pop_sp$population * 100000
# taxa_mortalidade

## -----------------------------------------------------------------------------
# # births and deaths in Acre, 2022
# nascimentos <- sinasc_data(year = 2022, uf = "AC")
# obitos <- sim_data(year = 2022, uf = "AC")
# 
# razao <- nrow(nascimentos) / nrow(obitos)
# razao
# #> ratio > 1 means more births than deaths (population growth)

## -----------------------------------------------------------------------------
# # hospital admissions for respiratory diseases, January 2022
# intern_resp <- sih_data(year = 2022, month = 1, uf = "AC", diagnosis = "J")
# 
# # outpatient production for respiratory diseases, January 2022
# ambul_resp <- sia_data(year = 2022, month = 1, uf = "AC", diagnosis = "J")
# 
# # compare volumes
# n_internacoes <- nrow(intern_resp)
# n_ambulatorial <- nrow(ambul_resp)
# 
# # compare costs
# custo_intern <- sum(as.numeric(intern_resp$VAL_TOT), na.rm = TRUE)
# custo_ambul <- sum(as.numeric(ambul_resp$PA_VALAPR), na.rm = TRUE)
# 
# tibble::tibble(
#   setting = c("Hospital (SIH)", "Outpatient (SIA)"),
#   records = c(n_internacoes, n_ambulatorial),
#   total_cost_brl = c(custo_intern, custo_ambul)
# )

## -----------------------------------------------------------------------------
# # install arrow for optimized caching (recommended)
# install.packages("arrow")

## -----------------------------------------------------------------------------
# # check what is cached
# sim_cache_status()
# sih_cache_status()
# sia_cache_status()
# 
# # clear cache for a specific module
# sim_clear_cache()

