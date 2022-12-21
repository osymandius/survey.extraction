library(RDS)
library(tidyverse)
library(rdhs)

#source("~/Documents/GitHub/survey-extraction/src/kp_recoding_functions.R")
source("src/kp_recoding_functions.R")
ssa_iso3 <- c("BDI", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")

### Recoding vars / values
      ## Analysis and file_type redundant for the time being
# recoding_sheet <-  read_csv("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/00Admin/recoding_sheet.csv")
recoding_sheet <-  read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/00Admin/recoding_sheet.csv")

variable_recode <- recoding_sheet %>% 
  select(survey_id, variable, var_raw, study_type) %>% 
  rename(var_label_raw = var_raw) %>% 
  mutate(survey_id2 = survey_id) %>% 
  separate(survey_id2, c(NA, "file_type")) %>% 
  distinct() %>%
  mutate(analysis = "kp") %>% 
  filter(!variable == "cdm_location",
         !variable == "subject_id") %>% 
  rename(var_raw = var_label_raw) 

value_recode <- recoding_sheet %>% 
  rename(value = val_recode) %>% 
  filter(!is.na(val_raw))

## Sample survey for trialing functions

path2 <- list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "MWI2019BBS_FSW.rds", full.names = TRUE, recursive = TRUE)
#path2 <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "BEN2002BBS_FSW.rds", full.names = TRUE, recursive = TRUE)

mwidat <- lapply(path2, readRDS)
## Trying new_extract_fun
debugonce(new_extract_fun)
wow <-  new_extract_fun(mwidat[[1]], "MWI2019BBS_FSW", variable_recode)

## Trying for PLACE 
placepath <- list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "AGO2018PLACE_TGW.rds", full.names = TRUE, recursive = TRUE)
#placepath <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "AGO2018PLACE_TGW.rds", full.names = TRUE, recursive = TRUE)
placedat <- lapply(placepath, readRDS)
place_recode <- new_extract_fun(placedat[[1]], "AGO2018PLACE_TGW", variable_recode)

## Trying new_val_recode
#wow2 <- wow %>%
 # mutate(cdm_last_paid = as.numeric(cdm_last_paid),
  #       cdm_last_paid = new_val_recode(cdm_last_paid, "cdm_last_paid", "BEN2002BBS_FSW"))
## Trying new_recode_survey_variables
wow3 <- new_recode_survey_variables(wow, "MWI2019BBS_FSW", value_recode)
placevals <- new_recode_survey_variables(place_recode, "AGO2018PLACE_TGW", value_recode)

### Trying RDS -> this is not working --> going wrong with new_recode_survey_variables - it's not dealing with strings very well. 
swzpath <- list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "SWZ2020BBS_FSW.rds", full.names = TRUE, recursive = TRUE)
#swzpath <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "SWZ2020BBS_FSW.rds", full.names = TRUE, recursive = TRUE)

swzdat <- lapply(swzpath, readRDS)
swzwow <- new_extract_fun(swzdat[[1]], "SWZ2020BBS_FSW", variable_recode)
# debugonce(new_recode_survey_variables)
swzwow2 <- new_recode_survey_variables(swzwow, "SWZ2020BBS_FSW", value_recode)
debugonce(rds_adjust2)
rds_trial <- rds_adjust2(wow3, "MWI2019BBS_FSW")

#### Surveys to be recoded

#paths <- intersect(list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = paste(survey_id, collapse = "|")  , full.names = TRUE, recursive = TRUE), list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = ".rds"  , full.names = TRUE, recursive = TRUE))

#paths <- intersect(list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = paste(unique(variable_recode$survey_id), collapse = "|")  , full.names = TRUE, recursive = TRUE), list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = ".rds"  , full.names = TRUE, recursive = TRUE))

# paths <- list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", recursive = TRUE, pattern = ".rds", full.names = TRUE) %>%
paths <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", recursive = TRUE, pattern = ".rds", full.names = TRUE) %>%
  lapply(., grep, pattern= "code", value = TRUE, invert = TRUE) %>%
  unlist()
  

combined_datasets <- lapply(paths, readRDS)

surv_ids <- str_split(paths, "/") %>%
  lapply(tail, 1) %>%
  unlist()

surv_ids[surv_ids == "uga2021_fsw_bbs.rdsobj"] <- "UGA2021BBS_FSW.rds"
surv_ids <- str_remove(surv_ids, ".rds")

names(combined_datasets) <- surv_ids

combined_datasets <- combined_datasets[!names(combined_datasets) %in% c("NAM2019BBS_FSW", "NAM2019BBS_MSM", "SWZ2020BBS_FSW")]

#### This is doing weird things.... the recoded datasets are in there amongst the chaos, I think. 
all_extracted <- combined_datasets %>%
  Map(new_extract_fun,
      df = .,
      survey_id = names(.),
      list(variable_recode)
      )

all_extracted <- compact(all_extracted)

#all_extracted[["BEN2005BBS_FSW"]]

all_recoded <- all_extracted %>%
  Map(new_recode_survey_variables,
      df = .,
      survey_id = names(.),
      list(value_recode))

all_rds <- all_recoded %>% 
  Map(rds_adjust,
      df = .,
      survey_id = names(.))

debugonce(rds_adjust)
rds_adjust(all_recoded$NAM2019BBS_FSW, "NAM2019BBS_FSW")

#
## Testing for one survey
debugonce(new_recode_survey_variables)
new_recode_survey_variables(wow, "MWI2019BBS_FSW", value_recode)
debugonce(new_val_recode)
df %>%
  mutate(onart = new_val_recode(onart, "onart", survey_id_c))

