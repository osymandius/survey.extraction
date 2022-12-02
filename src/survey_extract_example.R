hello
library(tidyverse)
library(rdhs)

source("src/extract_funs.R")

ssa_iso3 <- c("BDI", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")

variable_recode = readxl::read_excel("data/hivdata_survey_datasets.xlsx", sheet = "variable_recode", na = "NA")
value_recode = readxl::read_excel("data/hivdata_survey_datasets.xlsx", sheet = "value_recode", na = "NA")

survey_has_circ <- dhs_surveys(surveyCharacteristicIds = 59) %>%
  filter(!SurveyId %in% c("LB2019DHS", "GN2012DHS")) %>%
  mutate(survey_id = paste0(
    dhscc_to_iso3(DHS_CountryCode),
    SurveyYear,
    SurveyType
  ))

#' Men's recode datasets
mrd <- dhs_datasets(fileType = "MR", fileFormat = "FL")

#' Get Individual recode datasets with circumcision characteristic and bind in MR datasets
combined_datasets <- dhs_datasets(fileType = "IR", fileFormat = "FL") %>%
  filter(SurveyId %in% dhs_surveys(surveyCharacteristicId = 11)$SurveyId) %>%
  filter(!SurveyId %in% mrd$SurveyId) %>%
  bind_rows(mrd
            # filter(SurveyId %in% survey_has_circ$SurveyId)
  ) %>%
  filter(dhscc_to_iso3(DHS_CountryCode) %in% ssa_iso3,
         as.integer(SurveyYear) > 1999) %>%
  mutate(survey_id = paste0(
    dhscc_to_iso3(DHS_CountryCode),
    SurveyYear,
    SurveyType
  )) %>%
  filter(!survey_id %in% c("LSO2014DHS") # Jeff: Variables for both medical and traditional
  )

circ_raw <- get_datasets(combined_datasets %>% filter(survey_id == "BFA2003DHS"), clear_cache = TRUE) %>%
  setNames("BFA2003DHS") %>%
  .[grepl("\\.rds$", .)] %>%
  lapply(readRDS)

## For many surveys
file_type <- c(
  c("Individual Recode" = "ir", "Men's Recode" = "mr")[combined_datasets$FileType] %>% setNames(combined_datasets$survey_id)
)


## For many surveys
circ_extracted <- circ_raw %>%
  Map(extract_survey_vars,
      df = .,
      survey_id = names(.),
      list(variable_recode),
      file_type[names(.)],
      analysis = "circ")

## Testing for one survey
debugonce(extract_survey_vars)
circ_extracted <- extract_survey_vars(circ_raw[[1]], "BFA2003DHS", variable_recode, "mr", "circ")

## For many surveys
circ_recoded <- circ_extracted  %>%
  Map(recode_survey_variables,
      df = .,
      survey_id = names(.),
      list(value_recode),
      file_type[names(.)],
      analysis = "circ"
  )


## Testing for one survey
debugonce(val_recode)
circ_extracted %>%
  mutate(circ_age = as.numeric(circ_age),
         circ_age = val_recode(circ_age, "circ_age", "BFA2003DHS", "mr", "circ"))

