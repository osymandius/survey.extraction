## Sexual Behaviour Survey Extraction ##

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


#### Load Recoding Datasets ####

# recoding excel sheet
variable_recode = readxl::read_excel(
  "data/hivdata_survey_datasets.xlsx", sheet = "variable_recode", na = "NA"
)
value_recode = type.convert(readxl::read_excel(
  "data/hivdata_survey_datasets.xlsx", sheet = "value_recode", na = "NA"
), as.is = TRUE)


#### ??? ####

# pull in DHS survey information
ird <- dhs_datasets(fileType = "IR", fileFormat = "FL") %>%
  mutate(
    iso3 = dhscc_to_iso3(DHS_CountryCode),
    survey_id = paste0(iso3, SurveyYear, SurveyType)
  ) %>%
  filter(iso3 %in% ssa_iso3)

dl <- rdhs::get_downloaded_datasets()
dl <- names(dl)
dl <- paste0(dl, ".ZIP")

ird <- filter(ird, FileName %in% dl)

# ?
results <- search_variable_labels(ird, "ART") %>%
  filter(
    stringr::str_detect(description, "ART") # ,
    # str_detect(description, "12|(?i)twelve|year|(?i)month"),
    # !str_detect(variable, "v781"),
    # !str_detect(description, "(?i)Place")
  )
