## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# # default: R2 mirror with automatic DATASUS fallback
# sipni_data(year = 2024, uf = "AC", month = 1)
# 
# # pin a single source (no fallback)
# sipni_data(year = 2019, uf = "AC", source = "datasus")
# 
# # invert the priority (DATASUS first, R2 as fallback)
# sipni_data(year = 2019, uf = "AC", source = c("datasus", "r2"))

## -----------------------------------------------------------------------------
# data <- sipni_data(year = 2024, uf = "AC", month = 1)
# attr(data, "healthbr_source")
# #>  microdata
# #>       "r2"
# attr(data, "healthbr_provenance")
# #> # A tibble: 1 x 7  (partition, processing timestamp, Ministry source URL...)

## -----------------------------------------------------------------------------
# # everything the mirror holds
# sipni_status()
# 
# # which 2026 microdata months are published so far?
# sipni_status("microdados") |>
#   filter(year == 2026)

## -----------------------------------------------------------------------------
# sipni_years()
# #> [1] 1994 1995 ... 2025 2026

## -----------------------------------------------------------------------------
# sipni_info()

## -----------------------------------------------------------------------------
# # doses applied in Acre, 2019
# ac_doses <- sipni_data(year = 2019, uf = "AC")
# ac_doses

## -----------------------------------------------------------------------------
# # published form (code = .cnv category, source_codes = data codes)
# sipni_dictionary("IMUNO")
# 
# # join-ready lookup: one row per data code
# sipni_dictionary("IMUNO", lookup = TRUE)
# 
# # dose types and age groups
# sipni_dictionary("DOSE", lookup = TRUE)
# sipni_dictionary("FX_ETARIA", lookup = TRUE)
# 
# # the built-in offline copy (already in data-code form)
# sipni_dictionary("IMUNO", source = "datasus")

## -----------------------------------------------------------------------------
# # vaccination coverage in Acre, 2019
# ac_coverage <- sipni_data(year = 2019, type = "CPNI", uf = "AC")
# ac_coverage

## -----------------------------------------------------------------------------
# # microdata for Acre, January 2024
# ac_micro <- sipni_data(year = 2024, uf = "AC", month = 1)
# ac_micro

## -----------------------------------------------------------------------------
# # DPNI variables
# sipni_variables()
# 
# # CPNI variables
# sipni_variables(type = "CPNI")
# 
# # microdata variables, R2 mirror (default; 56 fields)
# sipni_variables(type = "API")
# 
# # microdata variables, OpenDataSUS CSV (~47 fields)
# sipni_variables(type = "API", source = "datasus")
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
# # all 12 months (default)
# full_year <- sipni_data(year = 2024, uf = "AC")

## -----------------------------------------------------------------------------
# ac_2019 <- sipni_data(year = 2019, uf = "AC")
# 
# # decode immunobiological names: lookup = TRUE gives data-code rows
# imuno_labels <- sipni_dictionary("IMUNO", lookup = TRUE) |>
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
# # vaccinations in Acre, January 2024
# ac_jan <- sipni_data(year = 2024, uf = "AC", month = 1)
# 
# # vaccines administered
# ac_jan |>
#   count(ds_vacina, sort = TRUE)
# 
# # doses by sex
# ac_jan |>
#   count(tp_sexo_paciente)
# 
# # age distribution
# ac_jan |>
#   mutate(age = as.integer(nu_idade_paciente)) |>
#   filter(!is.na(age)) |>
#   mutate(age_group = cut(age,
#                          breaks = c(0, 5, 12, 18, 30, 60, Inf),
#                          right = FALSE)) |>
#   count(age_group)

## -----------------------------------------------------------------------------
# # aggregated (2019) + microdata (2024)
# mixed <- sipni_data(year = c(2019, 2024), uf = "AC", month = 1)
# 
# # aggregated (UPPERCASE) and microdata columns are combined
# # with NAs where columns don't overlap
# names(mixed)

## -----------------------------------------------------------------------------
# ds <- sipni_data(year = 2019, uf = "AC", lazy = TRUE)
# ds |>
#   filter(IMUNO == "02") |>   # data code 02 = BCG (see sipni_dictionary)
#   select(MUNIC, DOSE, QT_DOSE) |>
#   collect()

## -----------------------------------------------------------------------------
# # parsed types (default)
# ac <- sipni_data(year = 2019, uf = "AC")
# class(ac$QT_DOSE)  # integer
# 
# # raw character columns, exactly as published
# ac_raw <- sipni_data(year = 2019, uf = "AC", parse = FALSE)

## -----------------------------------------------------------------------------
# # check cache status
# sipni_cache_status()
# 
# # clear cache if needed
# sipni_clear_cache()

