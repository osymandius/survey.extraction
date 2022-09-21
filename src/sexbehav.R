## Sexual Behaviour Survey Extraction ##

#### Load Packages ####

library(dplyr)
library(stringr)
library(purrr)
library(haven)
library(rdhs)
library(countrycode)

source("src/extract_funs.R")


#### Set Metadata ####

ssa_iso3 <- sort(c(
  "BDI", "BEN", "BFA", "CAF", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO",
  "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH",
  "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB"
))

# url and site for sharepoint
url <- Sys.getenv("SHAREPOINT_URL")
site <- Sys.getenv("SHAREPOINT_SITE")

# specific path to PHIA datasets
phia_sharepoint_path <- "Shared Documents/Data/household surveys/PHIA/datasets/"

# path for PHIA geo datasets
phia_geo_path <- file.path(
  "Shared Documents/Data/household surveys/PHIA/geospatial",
  "All PHIA1 PR Geospatial Data for Download"
)


#### Load Recoding Datasets ####

# recoding excel sheet
variable_recode = readxl::read_excel(
  "data/hivdata_survey_datasets.xlsx", sheet = "variable_recode", na = "NA"
)
value_recode = type.convert(readxl::read_excel(
  "data/hivdata_survey_datasets.xlsx", sheet = "value_recode", na = "NA"
), as.is = TRUE)

search_vars <- variable_recode %>% 
  filter(str_detect(variable, "sexbehav")) %>% 
  distinct(var_raw) %>% 
  pull()


#### Download DHS Data ####

mrd <- dhs_datasets(fileType = "MR", fileFormat = "FL")

combined_datasets <- dhs_datasets(fileType = "IR", fileFormat = "FL") %>%
  bind_rows(mrd) %>%
  mutate(
    iso3 = dhscc_to_iso3(DHS_CountryCode),
    survey_id = paste0(iso3, SurveyYear, SurveyType)
  ) %>%
  filter(iso3 %in% ssa_iso3)

dl <- rdhs::get_downloaded_datasets()
dl <- names(dl)
dl <- paste0(dl, ".ZIP")

combined_datasets <- filter(combined_datasets, FileName %in% dl)

surveys <- search_variables(combined_datasets, search_vars) %>%
  distinct(survey_id)

combined_datasets <- combined_datasets %>%
  filter(SurveyId %in% surveys$survey_id) %>%
  mutate(
    dataset = ifelse(
      FileType == "Individual Recode", 
      "ir",
      ifelse(FileType == "Men's Recode", "mr", NA)
    )
  )

sexbehav_raw <- get_datasets(combined_datasets) %>%
  setNames(paste0(
    combined_datasets$survey_id, "-", combined_datasets$dataset
  )) %>%
  # select(matches("\\.rds$")) %>% # would this work?
  .[grepl("\\.rds$", .)] %>% 
  lapply(readRDS)

sexbehav_raw$`UGA2011AIS-ir` <- sexbehav_raw$`UGA2011AIS-ir` %>%
  mutate(v791a = ifelse(s448b == 1 | s448e == 1, 1, 0))

names(sexbehav_raw) <- combined_datasets$survey_id

# MWI2010DHS: s631cc, cb, cc. Check survey. 


#### Extract and Recode DHS Data ####

sexbehav_extracted <- Map(
  extract_survey_vars,
  df = sexbehav_raw,
  survey_id = names(sexbehav_raw),
  list(variable_recode),
  # file_type[names(sexbehav_raw)],
  combined_datasets$dataset,
  analysis = "sexbehav"
)

# debugonce(extract_survey_vars)
# extract_survey_vars(sexbehav_raw$COG2009AIS, "COG2009AIS", variable_recode, "ir", "sexbehav")

sexbehav_recoded <- Map(
  recode_survey_variables,
  df = sexbehav_extracted,
  survey_id = names(sexbehav_extracted),
  list(value_recode),
  combined_datasets$dataset,
  analysis = "sexbehav"
)


#### MICS ####

# load mics data 
mics_dat <- load_sharepoint_data(
  path = Sys.getenv("MICS_ORDERLY_PATH"), 
  pattern = paste0(tolower(ssa_iso3), collapse = "|"),
  load_fun = readRDS
)
# change names from e.g. ner2000mics.rds to NER2000MICS
names(mics_dat) <- toupper(stringr::str_sub(names(mics_dat), 0, 11))

# pull male and female mics surveys, remove empty elements of list
mics_dat <- purrr::compact(
  lapply(
    lapply(mics_dat, "[", c("mn", "wm")),
    purrr::compact
  )
)

mics_mr <- unlist(lapply(mics_dat, "[", "mn"), recursive = FALSE) %>%
  setNames(str_sub(names(.), 0, -4)) %>%
  compact()
mics_wm <- unlist(lapply(mics_dat, "[", "wm"), recursive = FALSE) %>%
  unlist(recursive = FALSE) %>%
  setNames(str_sub(names(.), 0, -4)) %>%
  compact()
mics_raw <- c(mics_wm, mics_mr)

#### Extract and Recode MICS Data ####

file_type <- c(rep("wm", length(mics_wm)), rep("mn", length(mics_mr)))

mics_extracted <- Map(
  extract_survey_vars,
  df = mics_raw,
  survey_id = names(mics_raw),
  list(variable_recode),
  # file_type[names(.)],
  file_type,
  analysis = "sexbehav"
)

# What to do about MS3U and MS3N? 
# No pre-calculated imputed variable into months. 
# If constant can pick and calculate. 
# If changing variable name: annoying!

# TGO2017MICS: MSB2U/N
# TGO2010MICS: SH3U/N
# Same for wm datasets

## Sex behaviour relationship/other1/other2 is a risky recode.. 
# Examples where factor numbers are the same but val labels are different. 
# Won't be picked up. 
# Need to manually confirm all surveys conform to the _default_mics template
mics_recoded <- Map(
  recode_survey_variables,
  df = mics_extracted,
  survey_id = names(mics_extracted),
  list(value_recode),
  file_type,
  analysis = "sexbehav"
  )

# write.csv(sexbehav_ir %>% mutate(dataset = "ir"), "~/Downloads/dhs_sexbehav_ir.csv")

#### PHIA surveys ####

# load nested PHIA data from sharepoint
phia_dat <- load_phia_nested_sharepoint_data(phia_sharepoint_path)

# lapply(dat, function(dat) {
#   grep(x = colnames(dat), pattern = "mc", value=TRUE)
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
  "ZMB2016PHIA",
  "ZWE2015PHIA",
  "CMR2017PHIA"
)

# check if names are assigned correctly
check_phia_names(phia_dat)

colnames(phia_dat[[4]]) # ?


#### Extract and Recode PHIA Data ####

## Again - PHIA surveys missing centroids? Assume in separate geodata set?
phia_extracted <- Map(
  extract_survey_vars,
  df = phia_dat,
  survey_id = names(phia_dat),
  list(variable_recode),
  "phia",
  analysis = "sexbehav"
)

## Paddy: Where should this be saved now?
# readr::write_csv(
#   bind_rows(phia_extracted, .id = "survey_id"), 
#   "~/Downloads/phia_sexbehav.csv"
# )

phia_dat[[4]][1, c(1:5)] # ?

## What to do about receiving money/gifts for sex variables in PHIA surveys?
phia_recoded <- Map(
  recode_survey_variables,
  df = phia_extracted,
  survey_id = names(phia_extracted),
  list(value_recode),
  "phia",
  analysis = "sexbehav"
)

## Paddy: Again, where should this be saved?
# write.csv(bind_rows(phia_recoded), "~/Downloads/phia_sexbehav.csv")


#### PHIA geo data ####

# create temp directory
tmp <- tempdir()

# download phia Geo zip files
phia_geo_files <- load_sharepoint_data(path = phia_geo_path)

# unzip files
phia_geo_files <- unlist(lapply(phia_geo_files, function(x) {
  message(x)
  unzip(x, exdir = tmp)
}))

# subset
phia_geo_files <- phia_geo_files[
  grepl("Geospatial", phia_geo_files) & 
    grepl("centroids.csv", phia_geo_files)
]

# change names to survey naems
# names(phia_geo_files) <- stringr::str_remove_all(basename(names(phia_geo_files)), "%20")


names(phia_geo_files) <- c(
  "NAM2017PHIA",
  "ZWE2015PHIA",
  "UGA2016PHIA",
  "SWZ2016PHIA",
  "TZA2016PHIA",
  "ETH2017PHIA",
  "LSO2016PHIA",
  "CMR2017PHIA",
  "CIV2017PHIA",
  "RWA2018PHIA",
  "ZMB2016PHIA",
  "MWI2015PHIA"
)

# load geo data
phia_geo_data <- lapply(phia_geo_files, readr::read_csv, show_col_types = FALSE) %>% 
  bind_rows(.id = "surv_name")

# check surv_name matches centroidid
stopifnot(
  all(
    substr(phia_geo_data$surv_name, 0, 2) == 
      substr(phia_geo_data$centroidid, 0, 2)
  )
)

## Paddy: Is phia_geo_data ever saved? Why is it pulled in?
