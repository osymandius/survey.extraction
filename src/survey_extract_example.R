library(RDS)
library(tidyverse)
library(rdhs)

#source("~/Documents/GitHub/survey-extraction/src/kp_recoding_functions.R")
source("C:/Users/rla121/OneDrive - Imperial College London/Documents/GitHub/survey-extraction/src/kp_recoding_functions.R")
ssa_iso3 <- c("BDI", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")

### Recoding vars / values
      ## Analysis and file_type redundant for the time being
recoding_sheet <-  read_csv("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/00Admin/recoding_sheet.csv")
#recoding_sheet <-  read_csv("~/Downloads/recoding_sheet.csv")

variable_recode <- recoding_sheet %>% 
  select(survey_id, variable, var_raw, study_type) %>% 
  rename(var_label_raw = var_raw) %>% 
  mutate(survey_id2 = survey_id) %>% 
  separate(survey_id2, c(NA, "file_type")) %>% 
  distinct() %>%
  mutate(analysis = "kp") %>% 
  filter(!variable == "cdm_location") %>% 
  rename(var_raw = var_label_raw)

# variable_recode$file_type[variable_recode$file_type == "PLACE"] <- "all"


value_recode <- recoding_sheet %>% 
  rename(value = val_recode) %>% 
  filter(!is.na(val_raw))

#survey_id <- variable_recode$survey_id


## Sample survey for trialing functions

path2 <- list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "BEN2002BBS_FSW.rds", full.names = TRUE, recursive = TRUE)
#path2 <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "BEN2002BBS_FSW.rds", full.names = TRUE, recursive = TRUE)

bendat <- lapply(path2, readRDS)
## Trying new_extract_fun
wow <-  new_extract_fun(bendat[[1]], "BEN2002BBS_FSW", variable_recode)

## Trying for PLACE 
placepath <- list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "AGO2018PLACE_TGW.rds", full.names = TRUE, recursive = TRUE)
#placepath <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "AGO2018PLACE_TGW.rds", full.names = TRUE, recursive = TRUE)
placedat <- lapply(placepath, readRDS)
place_recode <- new_extract_fun(placedat[[1]], "AGO2018PLACE_TGW", variable_recode)

## Trying new_val_recode
wow2 <- wow %>%
  mutate(cdm_last_paid = as.numeric(cdm_last_paid),
         cdm_last_paid = new_val_recode(cdm_last_paid, "cdm_last_paid", "BEN2002BBS_FSW"))
## Trying new_recode_survey_variables
wow3 <- new_recode_survey_variables(wow, "BEN2002BBS_FSW", value_recode)
placevals <- new_recode_survey_variables(place_recode, "AGO2018PLACE_TGW", value_recode)

### Trying RDS -> this is not working --> going wrong with new_recode_survey_variables - it's not dealing with strings very well. 
swzpath <- list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "SWZ2020BBS_FSW.rds", full.names = TRUE, recursive = TRUE)
#swzpath <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "SWZ2020BBS_FSW.rds", full.names = TRUE, recursive = TRUE)

swzdat <- lapply(swzpath, readRDS)
swzwow <- new_extract_fun(swzdat[[1]], "SWZ2020BBS_FSW", variable_recode)
# debugonce(new_recode_survey_variables)
swzwow2 <- new_recode_survey_variables(swzwow, "SWZ2020BBS_FSW", value_recode)
debugonce(rds_adjust2)
rds_trial <- rds_adjust2(swzwow2, "SWZ2020BBS_FSW")

#### Surveys to be recoded

#paths <- intersect(list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = paste(survey_id, collapse = "|")  , full.names = TRUE, recursive = TRUE), list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = ".rds"  , full.names = TRUE, recursive = TRUE))

paths <- intersect(list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = paste(unique(variable_recode$survey_id), collapse = "|")  , full.names = TRUE, recursive = TRUE), list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = ".rds"  , full.names = TRUE, recursive = TRUE))

paths <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", recursive = TRUE, pattern = ".rds", full.names = TRUE) %>%
  lapply(., grep, pattern= "code", value = TRUE, invert = TRUE) %>%
  unlist()
  

combined_datasets <- lapply(paths, readRDS)

short_paths <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", recursive = TRUE, pattern = ".rds") %>%
  lapply(., grep, pattern= "code", value = TRUE, invert = TRUE) %>%
  unlist()

surv_ids <- short_paths %>%
  str_split("/") %>%
  lapply("[[", 2) %>%
  unlist()

names(combined_datasets) <- surv_ids



#### This is doing weird things.... the recoded datasets are in there amongst the chaos, I think. 
all_extracted <- combined_datasets %>%
  Map(new_extract_fun,
      df = .,
      survey_id = names(.),
      list(variable_recode)
      )

all_extracted[["BEN2005BBS_FSW"]]

all_recoded <- all_extracted %>%
  Map(new_recode_survey_variables,
      df = .,
      survey_id = names(.),
      list(value_recode))

########################## old DHS #################################
debugonce(new_extract_fun)
circ_extracted <- combined_datasets %>%
  Map(extract_survey_vars,
      df = .,
      survey_id = combined_datasets$survey,
      list(variable_recode),
      file_type[keypop],
      analysis = "kp")


variable_recode = variable_recode
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

