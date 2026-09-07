## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(healthbR)
# library(microdatasus)
# library(dplyr)
# 
# # healthbR ---------------------------------------------------------------
# rr_h <- sih_data(year = 2023, month = 1, uf = "RR")
# #> ℹ Reading SIH data from R2: 2023/01 (1 UF(s))...
# 
# dim(rr_h)
# #> [1] 4734  116
# table(vapply(rr_h, function(x) class(x)[1], ""))
# #> character      Date   integer   numeric
# #>       101         3         7         5
# 
# # microdatasus -------------------------------------------------------------
# rr_m <- fetch_datasus(year_start = 2023, month_start = 1,
#                       year_end = 2023, month_end = 1,
#                       uf = "RR", information_system = "SIH-RD")
# dim(rr_m)
# #> [1] 4734  113
# table(vapply(rr_m, function(x) class(x)[1], ""))
# #> character
# #>       113
# 
# rr_mp <- process_sih(rr_m)
# dim(rr_mp)
# #> [1] 4734  121

## -----------------------------------------------------------------------------
# cols <- c("N_AIH", "SEXO", "COD_IDADE", "IDADE", "MORTE", "DT_INTER",
#           "DIAG_PRINC", "MUNIC_RES", "VAL_TOT")
# 
# as.data.frame(head(rr_h[cols], 5))
# #>           N_AIH SEXO COD_IDADE IDADE MORTE   DT_INTER DIAG_PRINC MUNIC_RES  VAL_TOT
# #> 1 1423100411392    3         4    18     0 2022-12-13       O809    140017   568.80
# #> 2 1423100911661    1         4    83     0 2022-12-26       J189    140010   994.59
# #> 3 1423100911672    3         4    65     0 2022-12-03       I219    140010   620.12
# #> 4 1423100911683    1         4    58     0 2022-12-26       I248    140020   333.08
# #> 5 1423100911694    1         4    35     0 2022-11-01       A499    140010 11202.59
# 
# as.data.frame(head(rr_m[cols], 5))
# #>           N_AIH SEXO COD_IDADE IDADE MORTE DT_INTER DIAG_PRINC MUNIC_RES  VAL_TOT
# #> 1 1423100411392    3         4    18     0 20221213       O809    140017    568.8
# #> 2 1423100911661    1         4    83     0 20221226       J189    140010   994.59
# #> 3 1423100911672    3         4    65     0 20221203       I219    140010   620.12
# #> 4 1423100911683    1         4    58     0 20221226       I248    140020   333.08
# #> 5 1423100911694    1         4    35     0 20221101       A499    140010 11202.59
# 
# as.data.frame(head(rr_mp[c(cols, "munResNome")], 5))
# #>           N_AIH      SEXO COD_IDADE IDADE MORTE   DT_INTER DIAG_PRINC MUNIC_RES  VAL_TOT munResNome
# #> 1 1423100411392  Feminino      Anos    18   Não 2022-12-13       O809    140017    568.8      Cantá
# #> 2 1423100911661 Masculino      Anos    83   Não 2022-12-26       J189    140010   994.59  Boa Vista
# #> 3 1423100911672  Feminino      Anos    65   Não 2022-12-03       I219    140010   620.12  Boa Vista
# #> 4 1423100911683 Masculino      Anos    58   Não 2022-12-26       I248    140020   333.08  Caracaraí
# #> 5 1423100911694 Masculino      Anos    35   Não 2022-11-01       A499    140010 11202.59  Boa Vista

## -----------------------------------------------------------------------------
# sum(rr_h$MORTE == 1)          # healthbR: integer code
# #> [1] 113
# sum(rr_m$MORTE == "1")        # microdatasus, raw: character code
# #> [1] 113
# sum(rr_mp$MORTE == 1)         # microdatasus, processed: the code is gone
# #> [1] 0
# table(rr_mp$MORTE)
# #>  Não  Sim
# #> 4621  113

## -----------------------------------------------------------------------------
# sih_dictionary("MORTE")
# #> # A tibble: 2 × 4
# #>   variable description              code  label
# #>   <chr>    <chr>                    <chr> <chr>
# #> 1 MORTE    Óbito durante internação 0     Não
# #> 2 MORTE    Óbito durante internação 1     Sim
# 
# rr_h |>
#   count(MORTE) |>
#   left_join(sih_dictionary("MORTE"), by = c("MORTE" = "code"))

## -----------------------------------------------------------------------------
# attr(rr_h, "healthbr_source")
# #> [1] "r2"
# 
# attr(rr_h, "healthbr_provenance") |>
#   select(year, month, uf, records, source_hash_md5, source_size_bytes,
#          processing_timestamp)
# #> # A tibble: 1 × 7
# #>    year month uf    records source_hash_md5                  source_size_bytes processing_timestamp
# #>   <int> <int> <chr>   <dbl> <chr>                                        <dbl> <chr>
# #> 1  2023     1 RR       4734 12e74d4b059589ceb47e4136e3b2ce5f            349244 2026-03-09 03:05:34.463049

## -----------------------------------------------------------------------------
# st <- sih_status()
# nrow(st)
# #> [1] 11157
# attr(st, "last_updated")
# #> [1] "2026-08-18T12:33:31"
# range(st$year)
# #> [1] 1992 2026
# st |> filter(uf == "RR", year == 2023, month == 1) |>
#   select(records, source_hash_md5, parquet_sha256, pipeline_version)
# #> # A tibble: 1 × 4
# #>   records source_hash_md5                  parquet_sha256                                                   pipeline_version
# #>     <dbl> <chr>                            <chr>                                                            <chr>
# #> 1    4734 12e74d4b059589ceb47e4136e3b2ce5f bab1c2fdda7b09a6117169156f51846e095de0bc502fec9adecf90d2e1f08474 1.0.0

## -----------------------------------------------------------------------------
# sih_data(year = 2023, uf = "RR", lazy = TRUE) |>
#   filter(MORTE == 1) |>
#   count(month) |>
#   collect() |>
#   arrange(month)
# #> # A tibble: 12 × 2
# #>    month     n
# #>    <int> <int>
# #>  1     1   113
# #>  2     2   120
# #>  3     3   175
# #>  4     4   166
# #>  5     5   118
# #>  6     6    78
# #>  7     7    97
# #>  8     8   144
# #>  9     9   105
# #> 10    10   104
# #> 11    11   128
# #> 12    12   102

## -----------------------------------------------------------------------------
# # healthbR
# ac_h <- sim_data(year = 2022, uf = "AC")
# #> ℹ Downloading SIM data: AC 2022...
# dim(ac_h)
# #> [1] 4159   90
# 
# # microdatasus
# ac_m  <- fetch_datasus(year_start = 2022, year_end = 2022, uf = "AC",
#                        information_system = "SIM-DO")
# ac_mp <- process_sim(ac_m)
# dim(ac_m); dim(ac_mp)
# #> [1] 4159   87
# #> [1] 4159  100

## -----------------------------------------------------------------------------
# sim_dictionary("IDADE")
# #> # A tibble: 6 × 4
# #>   variable description                 code  label
# #>   <chr>    <chr>                       <chr> <chr>
# #> 1 IDADE    Idade (1º dígito = unidade) 0     Minutos (< 1 hora)
# #> 2 IDADE    Idade (1º dígito = unidade) 1     Horas
# #> 3 IDADE    Idade (1º dígito = unidade) 2     Dias
# #> 4 IDADE    Idade (1º dígito = unidade) 3     Meses
# #> 5 IDADE    Idade (1º dígito = unidade) 4     Anos (0-99)
# #> 6 IDADE    Idade (1º dígito = unidade) 5     Anos (100+)
# 
# # six infant deaths, the same rows in both
# #>   IDADE healthbR_age_years md_IDADEanos md_IDADEmeses md_IDADEdias md_IDADEhoras
# #> 1   021       3.992699e-05         <NA>          <NA>         <NA>          <NA>
# #> 2   101       1.140771e-04         <NA>          <NA>         <NA>             1
# #> 3   201       2.737851e-03         <NA>          <NA>            1          <NA>
# #> 4   302       1.666667e-01         <NA>             2         <NA>          <NA>
# #> 5   102       2.281542e-04         <NA>          <NA>         <NA>             2
# #> 6   108       9.126169e-04         <NA>          <NA>         <NA>             8

## -----------------------------------------------------------------------------
# # healthbR
# ac_n <- sinasc_data(year = 2022, uf = "AC")
# dim(ac_n)
# #> [1] 14483    63
# class(ac_n$PESO); sum(is.na(ac_n$PESO)); median(ac_n$PESO, na.rm = TRUE)
# #> [1] "integer"
# #> [1] 94
# #> [1] 3230
# 
# # microdatasus
# ac_nm  <- fetch_datasus(year_start = 2022, year_end = 2022, uf = "AC",
#                         information_system = "SINASC")
# ac_nmp <- process_sinasc(ac_nm)
# sum(is.na(ac_nm$PESO)); sum(is.na(ac_nmp$PESO))
# #> [1] 94
# #> [1] 14483

## -----------------------------------------------------------------------------
# sum(is.na(microdatasus::sinasc_sample$PESO))
# #> [1] 2
# sum(is.na(process_sinasc(microdatasus::sinasc_sample)$PESO))
# #> [1] 100

## -----------------------------------------------------------------------------
# # read with healthbR (cache, provenance), label with microdatasus
# sih_data(year = 2023, month = 1, uf = "RR", parse = FALSE) |>
#   process_sih()

## -----------------------------------------------------------------------------
# install.packages(c("microdatasus", "arrow"))
# # install.packages("pak"); pak::pak("SidneyBissoli/healthbR")   # dev version
# 
# sih_clear_cache(); sim_clear_cache(); sinasc_clear_cache()
# system.time(sih_data(year = 2023, month = 1, uf = "RR"))
# system.time(fetch_datasus(2023, 1, 2023, 1, uf = "RR",
#                           information_system = "SIH-RD"))

