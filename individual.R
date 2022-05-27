library(tidyverse)
library(rdhs)

source("extract_funs.R")

variable_recode = readxl::read_excel("~/Dropbox/oli backup/Survey extraction/hivdata_survey_datasets.xlsx", sheet = "variable_recode", na = "NA")
value_recode = readxl::read_excel("~/Dropbox/oli backup/Survey extraction/hivdata_survey_datasets.xlsx", sheet = "value_recode", na = "NA") %>% type.convert()


sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))
folder <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = Sys.getenv("MICS_ORDERLY_PATH"))

mics_sharepoint_df <- folder$list() %>%
  dplyr::filter(str_detect(name, paste0(tolower(ssa_iso), collapse = "|")))

mics_paths <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), Sys.getenv("MICS_ORDERLY_PATH"), mics_sharepoint_df$name)
mics_files <- lapply(mics_paths, spud::sharepoint_download, sharepoint_url = Sys.getenv("SHAREPOINT_URL"))

mics_dat <- lapply(mics_files, readRDS) %>%
  setNames(toupper(str_sub(mics_sharepoint_df$name, 0, 11))) %>%
  lapply("[", c("mn", "wm")) %>%
  lapply(compact) %>%
  compact

# mics_mr <- mics_dat %>%
#   lapply("[", "mn") %>%
#   unlist(recursive = FALSE) %>%
#   setNames(str_sub(names(.), 0, -4)) %>%
#   compact

mics_wm <- mics_dat %>%
  lapply("[", "wm") %>%
  unlist(recursive = FALSE) %>%
  setNames(str_sub(names(.), 0, -4)) %>%
  compact

# mics_raw <- c(mics_wm, mics_mr)
mics_raw <- c(mics_wm)

# file_type <- c(rep("wm", length(mics_wm)), rep("mn", length(mics_mr)))
file_type <- c(rep("wm", length(mics_wm)))

mics_extracted <- mics_raw %>%
  Map(extract_survey_vars,
      df = .,
      survey_id = names(.),
      list(variable_recode),
      # file_type[names(.)],
      file_type,
      analysis = "individual")

mics_recoded <- mics_extracted  %>%
  Map(recode_survey_variables,
      df = .,
      survey_id = names(.),
      list(value_recode),
      file_type,
      analysis = "individual"
  )
