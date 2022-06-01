library(tidyverse)
library(rdhs)
library(magrittr)

source("extract_funs.R")

sharepoint_loc <- 
  "~/Imperial College London/HIV Inference Group - WP - Documents/"

ssa_iso <- c(
  "BDI", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", 
  "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", 
  "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB"
)

# data directory
dir_loc <- "~/Dropbox/oli backup/Survey extraction/"

# recoding excel sheet
recode_xlsx <- file.path(dir_loc, "hivdata_survey_datasets.xlsx")
variable_recode = readxl::read_excel(
  recode_xlsx, sheet = "variable_recode", na = "NA"
)
value_recode = type.convert(readxl::read_excel(
  recode_xlsx, sheet = "value_recode", na = "NA"
))

search_vars <- variable_recode %>% 
  filter(str_detect(variable, "sexbehav")) %>% 
  distinct(var_raw) %>% 
  pull()


mrd <- dhs_datasets(fileType = "MR", fileFormat = "FL")

combined_datasets <- dhs_datasets(fileType = "IR", fileFormat = "FL") %>%
  bind_rows(mrd) %>%
  mutate(
    iso3 = dhscc_to_iso3(DHS_CountryCode),
    survey_id = paste0(iso3, SurveyYear, SurveyType)
  ) %>%
  filter(iso3 %in% ssa_iso)

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
  survey_id = names(sexbehav_extracted,),
  list(value_recode),
  combined_datasets$dataset,
  analysis = "sexbehav"
)

#################

sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))
folder <- sharepoint$folder(
  site = Sys.getenv("SHAREPOINT_SITE"), path = Sys.getenv("MICS_ORDERLY_PATH")
)

mics_sharepoint_df <- folder$list() %>%
  dplyr::filter(str_detect(name, paste0(tolower(ssa_iso), collapse = "|")))

mics_paths <- file.path(
  "sites", 
  Sys.getenv("SHAREPOINT_SITE"), 
  Sys.getenv("MICS_ORDERLY_PATH"), 
  mics_sharepoint_df$name
)
mics_files <- lapply(
  mics_paths, 
  spud::sharepoint_download, 
  sharepoint_url = Sys.getenv("SHAREPOINT_URL")
)

mics_dat <- lapply(mics_files, readRDS) %>%
  setNames(toupper(str_sub(mics_sharepoint_df$name, 0, 11))) %>%
  lapply("[", c("mn", "wm")) %>%
  lapply(compact) %>%
  compact()

mics_mr <- mics_dat %>%
  lapply("[", "mn") %>%
  unlist(recursive = FALSE) %>%
  setNames(str_sub(names(.), 0, -4)) %>%
  compact()

mics_wm <- mics_dat %>%
  lapply("[", "wm") %>%
  unlist(recursive = FALSE) %>%
  setNames(str_sub(names(.), 0, -4)) %>%
  compact()

mics_raw <- c(mics_wm, mics_mr)

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

###############

tmp <- tempdir()

phia_path <- file.path(
  "~/Imperial College London/HIV Inference Group - WP - Documents",
  "Data/household surveys/PHIA/datasets"
)

# Come back to this and functionalise
phia_paths <- lapply(
  list.files(phia_path, full.names = TRUE), 
  list.files, full.names = TRUE, pattern = "dataset"
  ) %>%
  lapply(list.files, full.names = TRUE, pattern = "Interview") %>%
  lapply(grep, pattern = "CSV).zip", value = TRUE) %>%
  lapply(grep, pattern = "Child", value = TRUE, invert = TRUE) %>%
  unlist()

# phia_paths <- list.files(phia_paths, full.names = TRUE)
# phia_paths <- lapply(
#   phia_paths, list.files, full.names = TRUE, pattern = "dataset"
# )
# phia_paths <- lapply(
#   phia_paths, list.files, full.names = TRUE, pattern = "Interview"
# )
# phia_paths <- lapply(phia_paths, grep, pattern = "CSV).zip", value = TRUE)
# 


lapply(phia_paths, function(x) {
  message(x)
  unzip(x, exdir = tmp)
})

phia_files <- list.files(tmp, full.names = TRUE)
# Only finding 11 PHIA paths from 12. CAMPHIA is in a nested folder
phia_path <- grep("adultind", phia_files, value = TRUE)

phia_path <- c(
  phia_path, 
  file.path(
    tmp, 
    "203 CAMPHIA 2017-2018 Adult Interview Dataset (CSV)/camphia2017adultind.csv"
  )
)

phia_dat <- lapply(phia_path, function(x) {
  dat <- read.csv(x)
  dat %>%
    mutate(
      across(everything(), str_trim),
      across(
        everything(), 
        str_replace_all, 
        pattern = "\\.", 
        replacement = NA_character_
      )
    ) %>%
    type.convert()
})

# lapply(dat, function(dat) {
#   grep(x = colnames(dat), pattern = "mc", value=TRUE)
# })

names(phia_dat) <- c(
  "CIV2017PHIA",
  "ETH2017PHIA",
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

colnames(phia_dat[[4]])

## Again - PHIA surveys missing centroids? Assume in separate geodata set?
phia_extracted <- phia_dat %>%
  Map(extract_survey_vars,
    df = .,
    survey_id = names(.),
    list(variable_recode),
    # file_type[names(.)],
    "phia",
    analysis = "sexbehav"
  )

write.csv(
  bind_rows(phia_extracted, .id = "survey_id"), 
  "~/Downloads/phia_sexbehav.csv"
)

phia_geo_path <- file.path(
  sharepoint_loc, 
  "Data/household surveys/PHIA/geospatial/",
  "All PHIA1 PR Geospatial Data for Download"
)
phia_geo_paths <- list.files(phia_geo_path, full.names = TRUE)
phia_surv_name <- str_sub(phia_geo_paths, start = 161) %>%
  str_split(pattern = " ") %>%
  lapply(head, 1) %>%
  unlist()

lapply(phia_geo_paths, function(x) {
  message(x)
  unzip(x, exdir = tmp)
})

# phia_geo_data <- lapply(list.files(tmp, pattern="Geospatial", full.names = TRUE), list.files, full.names=TRUE) %>%
#   lapply(grep, pattern = "centroids.csv", value=TRUE) %>%
#   lapply(read.csv) %>%
#   setNames(phia_surv_name)
#
# phia_geo_data %>%
#   bind_rows(.id = "surv_name")

phia_dat[[4]][1, c(1:5)]

## What to do about receiving money/gifts for sex variables in PHIA surveys?
phia_recoded <- Map(
  recode_survey_variables,
  df = phia_extracted,
  survey_id = names(phia_extracted),
  list(value_recode),
  "phia",
  analysis = "sexbehav"
)

write.csv(bind_rows(phia_recoded), "~/Downloads/phia_sexbehav.csv")
