## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
# library(healthbR)
# library(dplyr)
# 
# anvisa_types()

## -----------------------------------------------------------------------------
# medicines <- anvisa_data(type = "medicines")
# 
# # explore the data
# nrow(medicines)
# names(medicines)
# 
# # filter active medicines
# active <- medicines |>
#   filter(SITUACAO_REGISTRO == "ATIVO")
# 
# # count by therapeutic class
# active |>
#   count(CLASSE_TERAPEUTICA, sort = TRUE)

## -----------------------------------------------------------------------------
# devices <- anvisa_data(type = "medical_devices")
# 
# # count by risk class
# devices |>
#   count(CLASSE_RISCO, sort = TRUE)

## -----------------------------------------------------------------------------
# food <- anvisa_data(type = "food")
# cosmetics <- anvisa_data(type = "cosmetics")

## -----------------------------------------------------------------------------
# # only keep product name and active ingredient
# med_slim <- anvisa_data(
#   type = "medicines",
#   vars = c("NOME_PRODUTO", "PRINCIPIO_ATIVO", "SITUACAO_REGISTRO")
# )

## -----------------------------------------------------------------------------
# pesticides <- anvisa_data(type = "pesticides")
# 
# # search for a specific substance
# anvisa_variables(type = "pesticides", search = "substancia")
# 
# # substances authorized for coffee
# coffee <- pesticides |>
#   filter(NO_CULTURA == "Cafe")

## -----------------------------------------------------------------------------
# hemo <- anvisa_data(type = "hemovigilance")
# 
# # count by reaction type
# hemo |>
#   count(TIPO_REACAO_TRANSFUSIONAL, sort = TRUE)
# 
# # count by state
# hemo |>
#   count(UF_NOTIFICACAO, sort = TRUE)

## -----------------------------------------------------------------------------
# techno <- anvisa_data(type = "technovigilance")
# 
# # count by notification type
# techno |>
#   count(TIPO_NOTIFICACAO, sort = TRUE)

## -----------------------------------------------------------------------------
# # notifications (patient info + event summary)
# notif <- anvisa_data(type = "vigimed_notifications")
# 
# # medicines involved
# meds <- anvisa_data(type = "vigimed_medicines")
# 
# # adverse reactions (MedDRA coded)
# reactions <- anvisa_data(type = "vigimed_reactions")
# 
# # link notifications to reactions
# linked <- notif |>
#   select(IDENTIFICACAO_NOTIFICACAO, SEXO, IDADE_MOMENTO_REACAO, GRAVE) |>
#   inner_join(reactions, by = "IDENTIFICACAO_NOTIFICACAO")
# 
# # most common reactions
# linked |>
#   count(PT, sort = TRUE) |>
#   head(20)

## -----------------------------------------------------------------------------
# # download January 2020 data
# sngpc_jan <- anvisa_data(type = "sngpc", year = 2020, month = 1)
# 
# # top prescribed controlled substances
# sngpc_jan |>
#   count(DS_PRINCIPIO_ATIVO, sort = TRUE) |>
#   head(10)
# 
# # sales by state
# sngpc_jan |>
#   count(SG_UF_VENDA, sort = TRUE)

## -----------------------------------------------------------------------------
# # download Q1 2020 (Jan-Mar)
# sngpc_q1 <- anvisa_data(type = "sngpc", year = 2020, month = 1:3)
# 
# # monthly trend
# sngpc_q1 |>
#   count(month, name = "sales")

## -----------------------------------------------------------------------------
# # compounded (manipulated) controlled substances
# manip <- anvisa_data(type = "sngpc_compounded", year = 2020, month = 1)
# 
# # most common active ingredients
# manip |>
#   count(NO_PRINCIPIO_ATIVO, sort = TRUE) |>
#   head(10)

## -----------------------------------------------------------------------------
# # lazy query (requires arrow package)
# lazy_sngpc <- anvisa_data(
#   type = "sngpc", year = 2020, month = 1:12,
#   lazy = TRUE
# )
# 
# # filter and collect only what you need
# result <- lazy_sngpc |>
#   filter(SG_UF_VENDA == "SP") |>
#   collect()

## -----------------------------------------------------------------------------
# # all medicines variables
# anvisa_variables(type = "medicines")
# 
# # search across descriptions
# anvisa_variables(type = "hemovigilance", search = "paciente")
# 
# # SNGPC variables
# anvisa_variables(type = "sngpc")

## -----------------------------------------------------------------------------
# # check cache status
# anvisa_cache_status()
# 
# # clear all cached ANVISA data
# anvisa_clear_cache()

## -----------------------------------------------------------------------------
# # full module overview
# anvisa_info()

