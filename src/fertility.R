## Fertility Survey Extraction ##

#### Load Packages ####

library(dplyr)
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

# Data variables for DHS (AIS, MIS) surveys
# rather than there being 1 variable for child dob, there are 20
birth_cmc_vars <- c(paste0("b3_0", 1:9), paste0("b3_", 11:20))
# same for child sex, is child alive, child age at death (?)
child_sex_vars <- c(paste0("b4_0", 1:9), paste0("b4_", 11:20)) 
child_alive_vars <- c(paste0("b5_0", 1:9), paste0("b5_", 11:20)) 
## or b7? Age at death (months-imputed)
child_age_death_vars <- c(paste0("b7_0", 1:9), paste0("b7_", 11:20))

# data directory
# dir_loc <- file.path(
#   "~/Imperial College London/HIV Inference Group - WP - Documents/",
#   "Circumcision coverage/raw/Survey extract"
# )

#### Load Recoding Datasets ####

# recoding excel sheet
variable_recode = readxl::read_excel(
  "data/hivdata_survey_datasets.xlsx", sheet = "variable_recode", na = "NA"
)
value_recode = readxl::read_excel(
  "data/hivdata_survey_datasets.xlsx", sheet = "value_recode", na = "NA"
)

#### DHS, AIS, MIS ####

# pull list of DHS (and AIS, MIS) datasets
ird <- rdhs::dhs_datasets(fileType = "IR", fileFormat = "FL") %>%
  filter(
    dhscc_to_iso3(DHS_CountryCode) %in% ssa_iso3,
    as.integer(SurveyYear) > 1994
  ) %>%
  mutate(
    survey_id = paste0(dhscc_to_iso3(DHS_CountryCode), SurveyYear, SurveyType)
  )

# dl <- rdhs::get_downloaded_datasets()
# dl <- names(dl)
# dl <- paste0(dl, ".ZIP")
#
# ird <- ird %>%
#   filter(FileName %in% dl)

## Paddy: These are used to create recode_fert, but this isn't used again, so 
# commented out as loading these surveys is very memory intesive!
# download surveys from DHS website (not currently saved)
# fert_raw <- get_datasets(ird, clear_cache = TRUE)
# names(fert_raw) <- ird$survey_id
# fert_raw <- fert_raw[grepl("\\.rds$", fert_raw)]
# # load surveys
# fert_raw <- lapply(fert_raw, readRDS)

# idx <- c(paste0("_0", 1:9), paste0("_1", 1:9), "_20") # original comment

## Oli: I don't currently know where this has come from - id_vars and
# women_vars aren't exposed in the global environment.... pause this for now...

# recode_fert <- function(.data, id_vars, women_vars, b_vars) {
#  .data %>% 
#     select(all_of(c(id_vars, women_vars)), any_of(b_vars)) %>%
#     mutate(individual_id = dhs_individual_id(cluster, household, line))
# }
# fert_recoded <- lapply(fert_raw, recode_fert, id_vars, women_vars, b_vars) %>% 
#   purrr::imap(~mutate(.x, survey_id = .y))

#### MICS ####

# Download and load MICS surveys from Sharepoint
mics_dat <- load_sharepoint_data(
  path     = Sys.getenv("MICS_ORDERLY_PATH"),
  pattern  = paste0(tolower(ssa_iso3), collapse = "|"),
  load_fun = readRDS
)
# change names from e.g. ner2000mics.rds to NER2000MICS
names(mics_dat) <- toupper(stringr::str_sub(names(mics_dat), 0, 11))

# mics_dat_renamed <- mics_dat %>%
#   Map(rename_datasets,
#       df = .,
#       survey_id = names(.),
#       list(dataset_recode),
#       NULL
#       NULL
#   )

# pull bh sheets
mics_bh <- lapply(mics_dat, "[", "bh") %>%
  unlist(recursive = FALSE) %>%
  setNames(names(mics_dat)) %>%
  purrr::compact()

# add "wm" data to MOZ2008MICS survey
mics_bh$MOZ2008MICS <- mics_bh$MOZ2008MICS %>%
  left_join(
    (mics_dat$MOZ2008MICS$wm %>%
      select(MEMID, HH1, HH2, LN)),
    by = "MEMID"
  )

#### MICS - Extract and Recode ####

# Extract survey variables for "bh" sheets
bh_extracted <- Map(
  extract_survey_vars,
  df        = mics_bh,
  survey_id = names(mics_bh),
  list(variable_recode),
  "bh",
  "birth_history"
)

bh_recoded <- Map(
  recode_survey_variables,
  df        = bh_extracted,
  survey_id = names(bh_extracted),
  list(value_recode),
  "bh",
  "birth_history"
)

## Paddy: Is this safe to remove?
# debugonce(recode_survey_variables)
# recode_survey_variables(
#   bh_extracted[[1]],
#   "GHA2011MICS",
#   value_recode,
#   dataset_type = "wm",
#   analysis = "sexbehav"
# )

# debugonce(val_recode)

# mics_bh <- mics_bh %>%
#   imap(function(.x, .y) {
#
#     message(.y)
#     colnames(.x) <- tolower(colnames(.x))
#
#     .x %>%
#       select(all_of(c("hh1", "hh2", "ln")))
#   })
