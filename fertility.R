library(tidyverse)
library(rdhs)

source("extract_funs.R")

variable_recode = readxl::read_excel("~/Imperial College London/HIV Inference Group - WP - Documents/Circumcision coverage/raw/Survey extract/hivdata_survey_datasets.xlsx", sheet = "variable_recode", na = "NA")
value_recode = readxl::read_excel("~/Imperial College London/HIV Inference Group - WP - Documents/Circumcision coverage/raw/Survey extract/hivdata_survey_datasets.xlsx", sheet = "value_recode", na = "NA")

ssa_iso3 <- sort(c("BDI", "BEN", "BFA", "CAF", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB"))

ird <- dhs_datasets(fileType = "IR", fileFormat = "FL") %>%
  filter(dhscc_to_iso3(DHS_CountryCode) %in% ssa_iso3,
         as.integer(SurveyYear) > 1994) %>%
  mutate(survey_id = paste0(
    dhscc_to_iso3(DHS_CountryCode),
    SurveyYear,
    SurveyType
  ))

# dl <- rdhs::get_downloaded_datasets()
# dl <- names(dl)
# dl <- paste0(dl, ".ZIP")
# 
# ird <- ird %>%
#   filter(FileName %in% dl)

fert_raw <- get_datasets(ird, clear_cache = TRUE) %>%
  setNames(ird$survey_id) %>%
  .[grepl("\\.rds$", .)] %>%
  lapply(readRDS)

# idx <- c(paste0("_0", 1:9), paste0("_1", 1:9), "_20")

birth_cmc_vars <- c(paste0("b3_0", 1:9), paste0("b3_1", 1:9), "b3_20")
child_sex_vars <- c(paste0("b4_0", 1:9), paste0("b4_1", 1:9), "b4_20")
child_alive_vars <- c(paste0("b5_0", 1:9), paste0("b5_1", 1:9), "b5_20")
child_age_death_vars <- c(paste0("b7_0", 1:9), paste0("b7_1", 1:9), "b7_20") ## or b7? Age at death (months-imputed)


## Paddy: I don't currently know where this has come from - id_vars and women_vars aren't exposed in the global environment.... pause this for now...
fert_recoded <- fert_raw %>%
  lapply(function(x) {
    x %>%
      select(all_of(c(id_vars, women_vars)), any_of(b_vars)) %>%
      mutate(individual_id = dhs_individual_id(cluster, household, line))
  }) %>%
  imap(~mutate(.x, survey_id = .y))

### MICS data

sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))
folder <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = Sys.getenv("MICS_ORDERLY_PATH"))

mics_sharepoint_df <- folder$list() %>%
  dplyr::filter(str_detect(name, paste0(tolower(ssa_iso3), collapse = "|")))

mics_paths <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), Sys.getenv("MICS_ORDERLY_PATH"), mics_sharepoint_df$name)
mics_files <- lapply(mics_paths, spud::sharepoint_download, sharepoint_url = Sys.getenv("SHAREPOINT_URL"))

mics_dat <- lapply(mics_files, readRDS) %>%
  setNames(toupper(str_sub(mics_sharepoint_df$name, 0, 11)))

# mics_dat_renamed <- mics_dat %>%
#   Map(rename_datasets,
#       df = .,
#       survey_id = names(.),
#       list(dataset_recode),
#       NULL
#       NULL
#   )


  
mics_bh <- lapply(mics_dat, "[", "bh") %>%
  unlist(recursive = FALSE) %>%
  setNames(names(mics_dat)) %>%
  compact()

mics_bh$MOZ2008MICS <- mics_bh$MOZ2008MICS %>%
  left_join(mics_dat$MOZ2008MICS$wm %>%
    select(MEMID, HH1, HH2, LN)
  , by="MEMID")

bh_extracted <- mics_bh %>%
  Map(extract_survey_vars,
      df = .,
      survey_id = names(.),
      list(variable_recode),
      "bh",
      "birth_history"
  )

bh_recoded <- bh_extracted %>%
  Map(recode_survey_variables,
      df = .,
      survey_id = names(.),
      list(value_recode),
      "bh",
      "birth_history")

debugonce(recode_survey_variables)
recode_survey_variables(mics_extracted[[1]], "GHA2011MICS", value_recode, dataset_type = "wm", analysis = "sexbehav")

debugonce(val_recode)

df %>%
  mutate(across(everything(), as.numeric),
         across(contains("bh"), ~val_recode(.x, cur_column(), survey_id_c, dataset_type, analysis)),
         survey_id = survey_id_c,
         individual_id = dhs_individual_id(cluster, household, line)
  )
# 
# mics_bh <- mics_bh %>%
#   imap(function(.x, .y) {
#     
#     message(.y)
#     colnames(.x) <- tolower(colnames(.x))
#     
#     .x %>%
#       select(all_of(c("hh1", "hh2", "ln")))
#   })



