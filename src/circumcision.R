## Circumcision Survey Extraction ##

#### Load Packages ####

library(dplyr)
library(stringr)
library(purrr)
library(haven)
library(rdhs)

source("src/extract_funs.R")


#### Set Metadata ####

ssa_iso3 <- c(
  "BDI", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", 
  "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", 
  "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB"
)

# url and site for sharepoint
url <- Sys.getenv("SHAREPOINT_URL")
site <- Sys.getenv("SHAREPOINT_SITE")

# specific path to PHIA datasets
phia_sharepoint_path <- "Shared Documents/Data/household surveys/PHIA/datasets/"

# path to save circumcision data
save_path <- "Shared Documents/Circumcision coverage/raw/Survey extract/"

variable_recode = readxl::read_excel("data/hivdata_survey_datasets.xlsx", sheet = "variable_recode", na = "NA")
value_recode = readxl::read_excel("data/hivdata_survey_datasets.xlsx", sheet = "value_recode", na = "NA")


#### Load Recoding Datasets ####

# recoding excel sheet
variable_recode = readxl::read_excel(
  "data/hivdata_survey_datasets.xlsx", sheet = "variable_recode", na = "NA"
)
value_recode = type.convert(readxl::read_excel(
  "data/hivdata_survey_datasets.xlsx", sheet = "value_recode", na = "NA"
), as.is = TRUE)

# pull characteristic ID for DHS surveys for circumcision data
characteristic_id <- rdhs::dhs_survey_characteristics() %>%
  filter(grepl("circumcision", SurveyCharacteristicName)) %>% 
  pull(SurveyCharacteristicID)

# Pull surveys with circumcision information
survey_has_circ <- rdhs::dhs_surveys(
  surveyCharacteristicIds = characteristic_id
) %>%
  filter(!SurveyId %in% c("LB2019DHS", "GN2012DHS")) %>%
  mutate(
    survey_id = paste0(
    dhscc_to_iso3(DHS_CountryCode),
    SurveyYear,
    SurveyType
  ))

# Men's recode datasets
mrd <- rdhs::dhs_datasets(fileType = "MR", fileFormat = "FL")


# Get Individual recode datasets w/ circ characteristic and bind in MR datasets

combined_datasets <- dhs_datasets(fileType = "IR", fileFormat = "FL") %>%
  filter(SurveyId %in% dhs_surveys(surveyCharacteristicId = 11)$SurveyId) %>%
  filter(!SurveyId %in% mrd$SurveyId) %>%
  bind_rows(
    mrd
    # filter(SurveyId %in% survey_has_circ$SurveyId)
  ) %>%
  filter(
    dhscc_to_iso3(DHS_CountryCode) %in% ssa_iso3,
    as.integer(SurveyYear) > 1999
  ) %>%
  mutate(
    survey_id = paste0(
      dhscc_to_iso3(DHS_CountryCode), SurveyYear, SurveyType)
  ) %>%
  # Jeff: Variables for both medical and traditional
  filter(!survey_id %in% c("LSO2014DHS"))


# Jeff: Required to get around rdhs cache bug - as you will have different 
# surveys, highly likely that you will encounter surveys that are not in the 
# variable codebook and will be extracted/recoded incorrectly.
# The code will probably crash as a result.
#
# dl <- rdhs::get_downloaded_datasets()
# dl <- names(dl)
# dl <- paste0(dl, ".ZIP")
#
# For lack of a better solution - read in my downloaded survey list to 
# replicate what I've had access to..
#
# dl <- read.csv("rdhs_bug_dl.csv")[,1]
#
# combined_datasets <- combined_datasets %>%
#   filter(FileName %in% dl)


circ_raw <- get_datasets(combined_datasets, clear_cache = TRUE) %>%
  setNames(combined_datasets$survey_id) %>%
  .[grepl("\\.rds$", .)] %>%
  lapply(readRDS)


#### PHIA surveys ####

# load nested PHIA data from sharepoint
phia_dat <- load_phia_nested_sharepoint_data(phia_sharepoint_path)

# lapply(dat, function(dat) {
#   grep(x = colnames(dat), pattern = "mc", value = TRUE)
# })

names(phia_dat) <- c(
  "CIV2017PHIA",
  "ETH2017PHIA",
  "KEN2018PHIA",
  "LSO2016PHIA",
  "MWI2015PHIA",
  "NAM2017PHIA",
  "RWA2018PHIA",
  "SWZ2016PHIA",
  "TZA2016PHIA",
  "UGA2016PHIA",
  "ZWE2015PHIA",
  "ZMB2016PHIA",
  "CMR2017PHIA"
)

# check if names are assigned correctly
check_phia_names(phia_dat)

circ_raw <- c(circ_raw, phia_dat)

phia_file_type <- rep("phia", length(phia_dat))
names(phia_file_type) <- names(phia_dat)


#### MICS surveys ####

# I haven't included the code for how I found these surveys. Should this be 
# stored elsewhere? Along with Jeff's rdhs::search_variable_label code


# load mics surveys with circumcision information
mics_surveys_with_circ <- c(
  "ZWE2014MICS", "GHA2017MICS", "BEN2014MICS", "SWZ2014MICS", "MWI2013MICS", 
  "GMB2018MICS", "SWZ2010MICS", "NGA2016MICS", "TCD2019MICS"
)
mics_dat <- load_sharepoint_data(
  path = Sys.getenv("MICS_ORDERLY_PATH"), 
  pattern = paste0(tolower(mics_surveys_with_circ), collapse = "|"),
  load_fun = readRDS
)

# There needs to be some additional code to rename the datasets themselves when 
# they are non-standard in the MICS files.
# e.g. using similar logic to renaming variables. This excel segment is 
# currently in the "variable recode" tab, but strictly these are not variables.
# Should be moved to a new tab I think.
# _default_mics	dataset_rename	woman_dataset	          wm
# _default_mics	dataset_rename	household_dataset	      hh
# _default_mics	dataset_rename	birth_dataset	          bh
# SWZ2000MICS	  dataset_rename	woman_dataset	          wmsw
# CIV2000MICS	  dataset_rename	woman_dataset	          CIwm
# CMR2000MICS	  dataset_rename	woman_dataset	          wmca
# SWZ2000MICS	  dataset_rename	household_dataset	      hhsw
# CIV2000MICS	  dataset_rename	household_dataset	      CIhh
# CMR2000MICS	  dataset_rename	household_dataset	      hhca
#
# Currently surveys with custom dataset names are not extracted

# pull mn datasets, name appropriately
mics_dat <- unlist(lapply(mics_dat, "[", "mn"), recursive = FALSE)
names(mics_dat) <- mics_surveys_with_circ
# change colnames to lowercase
mics_dat <- lapply(mics_dat, function(x) {
    colnames(x) <- tolower(names(x))
    return(x)
})

circ_raw <- c(circ_raw, mics_dat)

mics_file_type <- rep("mn", length(mics_dat)) 
names(mics_file_type) <- names(mics_dat)


#### Extract and recode variables ####

# relevant filetype (e.g. mr, phia, mn) for each survey
file_type <- c(
    "Individual Recode" = "ir", 
    "Men's Recode" = "mr"
  )[combined_datasets$FileType] %>% 
    setNames(combined_datasets$survey_id),
  phia_file_type,
  mics_file_type
)

circ_extracted <- Map(
  extract_survey_vars,
  df = circ_raw,
  survey_id = names(circ_raw),
  list(variable_recode),
  file_type[names(circ_raw)],
  analysis = "circ"
)

#' Note on the value_recode tab of the excel file

#' There are several cases where though the variable name is custom to the 
#' survey, the value coding is the same as the default.
#' The value recode entries for those surveys can be removed, but for speed I 
#' just added them all. 
#' I'm not sure removing them is any better than leaving them in though. 
#' The size of the value recode book is immaterial.
circ_recoded <- Map(
  recode_survey_variables,
  df = circ_extracted,
  survey_id = names(circ_extracted),
  list(value_recode),
  file_type[names(circ_extracted)],
  analysis = "circ"
)

# remove dfs with less than rows and/or who have all NA for circ_status
circ_recoded_save <- purrr::compact(lapply(circ_recoded, function(x) {
  if (ncol(x) < 6) {
    NULL
  } else if (
    length(unique(x$circ_status)) == 1 && is.na(unique(x$circ_status))
  ) {
    NULL
  } else {
    x
  }
}))

# mr_surveys <- names(circ_recoded_save)[names(circ_recoded_save) %in% names(file_type[file_type == "mr"])]

# no_circ_id <- mr_surveys[!mr_surveys %in% survey_has_circ$survey_id]
#
# dhs_cc <- no_circ_id %>%
#   str_sub(1, 3) %>%
#   countrycode::countrycode(., "iso3c", "iso2c")

# dhs_surv_id <- paste0(dhs_cc, str_sub(no_circ_id, 4, 10))
#
# dhs_survey_characteristics(surveyIds = dhs_surv_id) %>%
#   filter(SurveyCharacteristicID == 59)

# upload circumcision data to Sharepoint
upload_sharepoint_data(
  circ_recoded_save, filename = "circ_recoded.rds", save_path = save_path
)

# int <- circ_recoded %>%
#   bind_rows()
#
# int %>%
#   select(survey_id, starts_with("circ"), -circ_age) %>%
#   mutate(across(starts_with("circ"), as.character)) %>%
#   distinct(survey_id, circ_status, circ_who, circ_where) %>%
#   pivot_longer(-survey_id) %>%
#   group_by(survey_id) %>%
#   filter(n() == 3)
#
# data.frame(lapply(circ_raw$TZA2012AIS, function(x) {attr(x, "label")})) %>%
#   t() %>%
#   as.data.frame() %>%
#   tibble::rownames_to_column(.)  %>%
#   View()

# 
# debugonce(extract_survey_vars)
# foo <- extract_survey_vars(circ_raw$SEN2005DHS, "SEN2005DHS", variable_recode, "mr", "circ")
# 

# debugonce(recode_survey_variables)
# foo2 <- recode_survey_variables(circ_extracted$TZA2012AIS, "TZA2012AIS", value_recode, "mr", "circ")
#
# circ_recoded$ZWE2014MICS %>%
#   View()
#
# df <- df %>%
#   mutate(across(everything(), as.numeric),
#          across(any_of(recode_columns), ~val_recode(.x, cur_column(), survey_id_c, dataset_type, analysis_c)),
#          survey_id = survey_id_c,
#          individual_id = dhs_individual_id(cluster_id, household, line)
#   ) %>%
#   type.convert(as.is = TRUE) %>%
#   select(survey_id, individual_id, everything())
#
# debugonce(val_recode)
#
# df %>%
#   mutate(circ_who = as.numeric(circ_who),
#          circ_who = val_recode(circ_who, "circ_who", "TZA2012AIS", "mr", "circ"))
#
# df %>%
#   mutate(across(everything(), as.numeric),
#          across(any_of(recode_columns), ~val_recode(.x, cur_column(), survey_id_c, dataset_type, analysis_c))
#   )
#
