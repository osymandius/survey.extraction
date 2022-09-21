## Survey Individuals Extraction ##

#### Load Packages ####

library(dplyr)
library(stringr)
library(purrr)
library(haven)
library(rdhs)

source("src/extract_funs.R")

#### Set Metadata ####

ssa_iso3 <- sort(c(
  "BDI", "BEN", "BFA", "CAF", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO",
  "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH",
  "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB"
))


#### Load Recoding Datasets ####

# recoding excel sheet
variable_recode = readxl::read_excel(
  "data/hivdata_survey_datasets.xlsx", sheet = "variable_recode", na = "NA"
)
value_recode = type.convert(readxl::read_excel(
  "data/hivdata_survey_datasets.xlsx", sheet = "value_recode", na = "NA"
))

# load mics data 
mics_dat <- load_sharepoint_data(
  path = Sys.getenv("MICS_ORDERLY_PATH"), 
  pattern = paste0(tolower(ssa_iso3), collapse = "|"),
  load_fun = readRDS
)
# change names from e.g. ner2000mics.rds to NER2000MICS
names(mics_dat) <- toupper(stringr::str_sub(names(mics_dat), 0, 11))

mics_dat <- purrr::compact(
  lapply(
    lapply(mics_dat, "[", c("mn", "wm")),
    purrr::compact
  )
)

# mics_mr <- mics_dat %>%
#   lapply("[", "mn") %>%
#   unlist(recursive = FALSE) %>%
#   setNames(str_sub(names(.), 0, -4)) %>%
#   compact

# pull  data for women
mics_wm <- unlist(lapply(mics_dat, "[", "wm"), recursive = FALSE)
names(mics_wm) <- stringr::str_sub(names(mics_wm), 0, -4)
mics_wm <- compact(mics_wm)

# mics_raw <- c(mics_wm, mics_mr)
mics_raw <- c(mics_wm)

# file_type <- c(rep("wm", length(mics_wm)), rep("mn", length(mics_mr)))
file_type <- c(rep("wm", length(mics_wm)))

mics_extracted <- Map(
  extract_survey_vars,
  df = mics_raw,
  survey_id = names(mics_raw),
  list(variable_recode),
  # file_type[names(.)],
  file_type,
  analysis = "individual"
)

mics_recoded <- Map(
  recode_survey_variables,
  df = mics_extracted,
  survey_id = names(mics_extracted),
  list(value_recode),
  file_type,
  analysis = "individual"
)