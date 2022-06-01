library(tidyverse)
library(rdhs)

source("extract_funs.R")

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

# mics_mr <- mics_dat %>%
#   lapply("[", "mn") %>%
#   unlist(recursive = FALSE) %>%
#   setNames(str_sub(names(.), 0, -4)) %>%
#   compact

mics_wm <- mics_dat %>%
  lapply("[", "wm") %>%
  unlist(recursive = FALSE) %>%
  setNames(str_sub(names(.), 0, -4)) %>%
  compact()

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