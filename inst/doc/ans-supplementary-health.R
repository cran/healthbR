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
# # beneficiaries (default)
# ans_years()
# 
# # complaints
# ans_years(type = "complaints")
# 
# # financial statements
# ans_years(type = "financial")

## -----------------------------------------------------------------------------
# ans_info()

## -----------------------------------------------------------------------------
# # Acre, December 2023
# ac <- ans_data(year = 2023, month = 12, uf = "AC")
# ac

## -----------------------------------------------------------------------------
# # first quarter 2024, two states
# ne <- ans_data(year = 2024, month = 1:3, uf = c("CE", "PE"))
# 
# # full year (month = NULL downloads all 12 months)
# ac_2023 <- ans_data(year = 2023, uf = "AC")

## -----------------------------------------------------------------------------
# ans_data(
#   year = 2023, month = 12, uf = "AC",
#   vars = c("CD_OPERADORA", "SG_UF", "TP_SEXO",
#            "DE_FAIXA_ETARIA", "QT_BENEFICIARIO_ATIVO")
# )

## -----------------------------------------------------------------------------
# # complaints filed in 2022
# nip <- ans_data(year = 2022, type = "complaints")
# nip
# 
# # multiple years
# nip_multi <- ans_data(year = 2020:2023, type = "complaints")

## -----------------------------------------------------------------------------
# # Q1 2023
# fin_q1 <- ans_data(year = 2023, type = "financial", quarter = 1)
# 
# # all 4 quarters of 2023
# fin_2023 <- ans_data(year = 2023, type = "financial")
# 
# # specific quarters
# fin_q12 <- ans_data(year = 2023, type = "financial", quarter = 1:2)

## -----------------------------------------------------------------------------
# # active operators
# active <- ans_operators()
# 
# # cancelled operators
# cancelled <- ans_operators(status = "cancelled")
# 
# # both combined
# all_ops <- ans_operators(status = "all")

## -----------------------------------------------------------------------------
# # beneficiaries variables (default)
# ans_variables()
# 
# # complaints variables
# ans_variables(type = "complaints")
# 
# # financial variables
# ans_variables(type = "financial")
# 
# # search across variables
# ans_variables(search = "operadora")
# ans_variables(search = "beneficiario")

## -----------------------------------------------------------------------------
# # December 2023, all states
# ben <- ans_data(year = 2023, month = 12)
# 
# # active beneficiaries by state and coverage type
# ben |>
#   group_by(SG_UF, COBERTURA_ASSIST_PLAN) |>
#   summarize(
#     total = sum(as.integer(QT_BENEFICIARIO_ATIVO), na.rm = TRUE),
#     .groups = "drop"
#   ) |>
#   arrange(desc(total))

## -----------------------------------------------------------------------------
# nip <- ans_data(year = 2023, type = "complaints")
# 
# # top complaint subjects
# nip |>
#   count(ASSUNTO, sort = TRUE) |>
#   head(10)
# 
# # operators with most complaints
# nip |>
#   count(NOME_OPERADORA, sort = TRUE) |>
#   head(10)

## -----------------------------------------------------------------------------
# # Q4 2023 financial data
# fin <- ans_data(year = 2023, type = "financial", quarter = 4)
# 
# # join with operator registry for names
# ops <- ans_operators()
# 
# fin |>
#   filter(grepl("^41", CD_CONTA_CONTABIL)) |>  # revenue accounts
#   group_by(REG_ANS) |>
#   summarize(
#     revenue = sum(as.numeric(VL_SALDO_FINAL), na.rm = TRUE),
#     .groups = "drop"
#   ) |>
#   left_join(
#     ops |> select(REGISTRO_OPERADORA, Razao_Social),
#     by = c("REG_ANS" = "REGISTRO_OPERADORA")
#   ) |>
#   arrange(desc(revenue)) |>
#   head(10)

## -----------------------------------------------------------------------------
# # private coverage (ANS)
# ben <- ans_data(year = 2023, month = 12,
#                 vars = c("SG_UF", "QT_BENEFICIARIO_ATIVO"))
# 
# private_by_uf <- ben |>
#   group_by(SG_UF) |>
#   summarize(
#     private_beneficiaries = sum(as.integer(QT_BENEFICIARIO_ATIVO), na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # total population (Census 2022)
# pop <- censo_populacao(year = 2022, territorial_level = "state")
# 
# # private coverage rate
# # private_by_uf |>
# #   left_join(pop, by = ...) |>
# #   mutate(coverage_pct = (private_beneficiaries / population) * 100) |>
# #   arrange(desc(coverage_pct))

## -----------------------------------------------------------------------------
# # lazy query (requires arrow)
# lazy_ben <- ans_data(year = 2023, uf = "AC", lazy = TRUE)
# lazy_ben |>
#   filter(month == 12L, TP_SEXO == "F") |>
#   select(CD_OPERADORA, DE_FAIXA_ETARIA, QT_BENEFICIARIO_ATIVO) |>
#   collect()

## -----------------------------------------------------------------------------
# # check cache status
# ans_cache_status()
# 
# # clear cache if needed
# ans_clear_cache()

