dhs_individual_id <- function(cluster, household, line) {
  sprintf("%4d%4d%3d", cluster, household, line)
}

dhscc_to_iso3 <- function(dhscc) {
  dhsc <- rdhs::dhs_countries()
  dictionary <- setNames(dhsc$ISO3_CountryCode, dhsc$DHS_CountryCode)
  val <- dplyr::recode(dhscc, !!!dictionary)

  if (any(!val %in% dictionary)) {
    stop(
      "DHS Country Code not found: ", 
      paste(val[!val %in% dictionary], collapse = ", ")
    )
  }
  val
}

#' Recode ID variables in MICS surveys
#' @param variable_recode Variable recode book
#' @param survey_id_c Survey ID
#' @param dataset_type Dataset type e.g.....
#' @description MICS surveys do not have consistent cluster, household, and 
#' line number variables. This standardises them.
#' @export


recode_mics_id_vars <- function(variable_recode, survey_id_c, dataset_type) {

  # Find custom var names associated with this survey ID & dataset type
  custom_recode <- variable_recode %>% 
    filter(
      survey_id == survey_id_c, 
      dataset == dataset_type
    )

  # Find default var names for MICS surveys and the specified dataset type
  default_recode <- variable_recode %>% 
    filter(
      survey_id == "_default_mics",
      dataset == dataset_type
    )

  variable_df <- data.frame(variable = c("cluster_id", "household", "line"))

  variable_df <- left_join(variable_df, custom_recode, by = "variable")

  # Combine custom & default var name dfs 
  # Use defaults where custom vars are missing
  variable_df <- variable_df %>%
    filter(!is.na(var_raw)) %>%
    bind_rows(
      (variable_df %>%
        filter(is.na(var_raw)) %>%
        select(variable) %>%
        left_join(default_recode, by = "variable"))
    ) %>%
    filter(!is.na(var_raw))

  id_vars <- variable_df %>% 
    filter(dataset == dataset_type) %>% 
    pull(var_raw)
  names(id_vars) <- variable_df %>% 
    filter(dataset == dataset_type) %>% 
    pull(variable)

  id_vars
}

#' Extract survey variables
#' @param df Raw survey dataframe
#' @param survey_id_c Survey ID
#' @param variable_recode Variable recode book
#' @param dataset_type Dataset type e.g.....
#' @param analysis One of "circ", "sexbehav" etc
#'
#' @export

extract_survey_vars <- function(
  df, survey_id_c, variable_recode, dataset_type, analysis_c
) {
  
  message(survey_id_c)

  if(str_detect(survey_id_c, "FSW|PWID|MSM|TGW|TG"))
    surv_type <- "KP"
  else
    surv_type <- substr(survey_id_c, 8, stringr::str_length(survey_id_c))

  ## If individual ID is to be used as primary key - this needs editing
  if (surv_type %in% c("DHS", "AIS", "MIS")) {
    id_vars <- switch(
      dataset_type,
      "ir"= c(cluster_id = "v001", household = "v002", line = "v003"),
      "mr" = switch(
        survey_id_c,
        "TZA2012AIS" =  c(
          cluster_id = "v001", household = "v002", line = "v003"
        ),
        c(cluster_id = "mv001", household = "mv002", line = "mv003")
      ),
      stop("Dataset type not recognised")
    )
  
    if (surv_type == "AIS") {
      if (analysis_c == "circ" & dataset_type == "ir") {
        df <- filter(df, aidsex == 1)
      }
      surv_type <- "DHS"
    }
    
  } else if (surv_type == "PHIA") {
    id_vars <- c(
      cluster_id = "centroidid", household = "householdid", line = "personid"
    )
  } else if (surv_type == "MICS") {
    id_vars <- recode_mics_id_vars(variable_recode, survey_id_c, dataset_type)
    stopifnot(length(id_vars) > 0)
    colnames(df) <- tolower(colnames(df))
  } else if(surv_type == "KP"){
    
    if(!survey_id_c %in% variable_recode$survey_id) {
      stop(survey_id_c, ": No KP survey found in recode sheet")
    }
    
    id_vars <- c("survey_id", "unique_id")
    
  } else {
    stop("Survey type not recognised")
  }
  

  # find custom var names associated with this survey ID & dataset type
  custom_recode <- filter(
    variable_recode, survey_id == survey_id_c, dataset == dataset_type
  )
  
  if(dataset_type == "kp") {
    
    df <- df %>% 
      ungroup() %>% 
      mutate(survey_id = survey_id_c, 
             unique_id = row_number())
    
    variable_df <- custom_recode
    
  } else {
    
    # Finds the default variable names for specified survey and dataset types
    default_recode <- filter(
      variable_recode,
      dataset == dataset_type,
      survey_id == paste0("_default_", tolower(surv_type)),
      !variable %in% c("cluster_id", "household", "line")
    )
    
    variable_df <- variable_recode %>%
      filter(
        analysis == analysis_c,
        stringr::str_detect(survey_id, "_default")
      ) %>%
      distinct(variable) %>%
      left_join(custom_recode, by = "variable")
    
    # Combine custom and default variable name dfs
    # Use defaults in the absence of custom vars
    variable_df <- variable_df %>%
      filter(!is.na(var_raw)) %>%
      bind_rows(
        (variable_df %>%
           filter(is.na(var_raw)) %>%
           select(variable) %>%
           left_join(default_recode, by = "variable"))
      ) %>%
      filter(!is.na(var_raw))
  
    
  }
  
  opt_var <- variable_df %>% 
    pull(var_raw) %>% 
    setNames(
      variable_df %>% 
        filter() %>% 
        pull(variable)
    )



  # Mandatory variables are cluster_id, household, line number 
  # (to be replaced with individual ID). Any "analysis variables" are optional.
  df <- df %>%
    select(
      all_of(id_vars),
      any_of(opt_var)
    )
}

#' Recode survey variables
#' @param col Column passed from across()
#' @param col_name Column name passed by dplyr::cur_column()
#' @param survey_id_c Survey ID
#' @param dataset_type Dataset type e.g.....
#' @param analysis One of "circ", "sexbehav" etc
#' @export

val_recode <- function(col, col_name, survey_id_c, dataset_type, analysis_c) {
  
  value_recode <- filter(value_recode, variable == col_name)

  # All values in the value book come one excel col which includes strings
  # => NAs are read as _NA_character which adversely effects later recoding
  
  # If, for a given variable, the values are only 0-9 and NA, force to be numeric.
  if (!length(value_recode$value[!value_recode$value %in% c(0:9, NA)])) {
    value_recode$value <- as.numeric(value_recode$value)
  }

  surv_type <- substr(survey_id_c, 8, stringr::str_length(survey_id_c))

  # Required because the AIS default variables are from _default_dhs
  if (surv_type == "AIS") {
    surv_type <- "DHS"
  }

  # Same logic applies as with extracting variables using custom and defaults.
  custom_recode <- filter(
    value_recode,
    dataset == dataset_type,
    survey_id == survey_id_c
  )

  default_recode <- filter(
    value_recode,
    dataset == dataset_type,
    analysis == analysis_c,
    survey_id == paste0("_default_", tolower(surv_type))
  )

  value_df <- default_recode %>%
    distinct(variable) %>%
    left_join(custom_recode, by = "variable")

  value_recode <- value_df %>%
    filter(!is.na(survey_id)) %>%
    bind_rows(
      (value_df %>%
      filter(is.na(survey_id)) %>%
      select(variable) %>%
      left_join(default_recode, by = "variable"))
    ) %>%
    filter(variable == col_name) %>%
    droplevels()

  if (nrow(value_recode) == 0) {
    value_recode <- custom_recode
  }

  vec <- value_recode$value
  names(vec) <- value_recode$val_raw

  recode(col, !!!vec, .default = NA_integer_)
}

recode_survey_variables <- function(
  df, survey_id_c, value_recode, dataset_type, analysis_c
) {
  
  message(survey_id_c)

  if(str_detect(survey_id_c, "FSW|PWID|MSM|TGW|TG")) {
    surv_type <- "KP"
  } else {
    surv_type <- substr(survey_id_c, 8, stringr::str_length(survey_id_c))
  }
  
  if (surv_type =="AIS")
    surv_type <- "DHS"
  
  if(dataset_type != "kp")
    recode_columns <- value_recode %>% 
      filter(
        survey_id == paste0("_default_", tolower(surv_type)),
        dataset   == dataset_type,
        analysis  == analysis_c
      ) %>% 
      distinct(variable) %>% 
      pull() %>% 
      as.character()
  else
    recode_columns <- c()
  
  survey_specific_columns <- value_recode %>% 
    filter(
      survey_id == survey_id_c,
      dataset   == dataset_type,
      !is.na(val_raw), ## Removes any variables that don't need recoding and can be passed through as is
      if(dataset_type == "kp") {
        analysis = TRUE
      } else {
        analysis == analysis_c
      }
    ) %>% 
    distinct(variable) %>% 
    pull() 

  recode_columns <- unique(c(recode_columns, survey_specific_columns))

  df <- df %>%
    ungroup() %>%
    mutate(
      # across(everything(), as.numeric), ## This seems like a bad idea for the main code too...
      across(any_of(recode_columns), ~ val_recode(
        .x, cur_column(), survey_id_c, dataset_type, analysis_c
      )),
      survey_id = survey_id_c,
      individual_id = ifelse(dataset_type == "kp", row_number(), dhs_individual_id(cluster_id, household, line))
    ) %>%
    type.convert(as.is = TRUE) %>%
    select(survey_id, individual_id, everything())
}

#### Load data from specific dir on Sharepoint ####
load_sharepoint_data <- function(
  path, 
  pattern  = NULL, 
  url      = Sys.getenv("SHAREPOINT_URL"),
  site     = Sys.getenv("SHAREPOINT_SITE")
) {
  
  # create connection to Sharepoint
  sharepoint <- spud::sharepoint$new(url)
  
  # List files in Sharepoint folder 
  folder <- sharepoint$folder(site = site, path = URLencode(path))
  
  # pull urls for each file
  urls <- URLencode(file.path("sites", site, path, folder$files()$name))
  
  # may only require certain files 
  if (!is.null(pattern)) urls <- urls[grepl(pattern, urls)]
  
  # download files, name with urls so we know order of temp files
  files = lapply(urls, sharepoint$download)
  if (length(files) == 0) stop("No files found at supplied path")
  names(files) <- basename(urls)
  
  # if desired, can specify function to load files with (e.g. readRDS)
  if (!is.null(load_fun)) files <- lapply(files, load_fun)
  
  return(files)
}

#### Load nested PHIA datasets from Sharepoint ####
load_phia_nested_sharepoint_data <- function(
    path, 
    url  = Sys.getenv("SHAREPOINT_URL"),
    site = Sys.getenv("SHAREPOINT_SITE")
) {
  
  # create temp file to download data to
  tmp <- tempdir()
  
  # create connection to Sharepoint folder
  sharepoint <- spud::sharepoint$new(url)
  
  # List cntry/datasets folders in PHIA Sharepoint folder 
  phia_folder <- sharepoint$folder(
    site = site, 
    path = URLencode(path)
  )
  cntry_folders <- phia_folder$folders()$name 
  folders_list <- lapply(cntry_folders, function(x) {
    sharepoint$folder(
      site = site, 
      path = URLencode(file.path(path, x, "/datasets"))
    )
  })
  
  # pull urls from each folder for each file
  urls <- unlist(lapply(seq_along(folders_list), function(i) {
    URLencode(file.path(
      "sites", 
      site, 
      # path, 
      path,   
      cntry_folders[[i]], 
      "datasets/",
      folders_list[[i]]$files()$name
    ))
  }))
  
  # filter URL
  urls <- urls[
    grepl("Interview", urls) & 
      grepl("CSV).zip", urls) & 
      !grepl("Child", urls)
  ]
  
  # download files, name with urls so we know order of temp files
  message("downloading...")
  files = lapply(urls, sharepoint$download)
  if (length(files) == 0) stop("No files found at supplied path")
  
  # unzip files
  message("unzipping...")
  lapply(files, function(x) {
    message(x)
    unzip(x, exdir = tmp)
  })
  
  phia_files <- list.files(tmp, full.names = TRUE)
  phia_path <- grep("adultind", phia_files, value = TRUE) 
  # Only finding 11 PHIA paths from 12. CAMPHIA is in a nested folder
  phia_path <- c(
    phia_path, 
    file.path(
      tmp, 
      "203 CAMPHIA 2017-2018 Adult Interview Dataset (CSV)/camphia2017adultind.csv"
    )
  )
  
  message("reading files ...")
  phia_dat <- lapply(phia_path, function(x) {
    dat <- readr::read_csv(x, show_col_types = FALSE)
    dat %>%
      mutate(
        across(everything(), stringr::str_trim),
        across(
          everything(), 
          stringr::str_replace_all, 
          pattern = "\\.", 
          replacement = NA_character_
        )
      ) %>%
      type.convert(as.is = TRUE)
  })
  
  message("done!")
  return(phia_dat)
}

#### Upload data to Sharepoint ####

upload_sharepoint_data <- function(
    .data,
    filename, 
    save_path,
    url      = Sys.getenv("SHAREPOINT_URL"),
    site     = Sys.getenv("SHAREPOINT_SITE"),
    save_fun = saveRDS
) {
  
  # save data locally using specified function
  save_fun(.data, file = filename)
  
  sharepoint <- spud::sharepoint$new(url)
  folder <- sharepoint$folder(site, save_path, verify = TRUE)
  # folder$upload(path = "circ_recoded.rds", dest = "circ_recoded_test.rds")
  message("uploading...")
  folder$upload(path = filename, dest = filename)
  
  # remove local file
  system(paste0("rm ", filename))
}

#### check if PHIA survey names are assigned correctly ####
check_phia_names <- function(.data) {
  stopifnot(
    all(
      # pull country names from dataframes and convert to iso3c
      vapply(.data, function(x) {
        countrycode::countrycode(
          x$country[1], 
          origin = "country.name", 
          destination = "iso3c"
        )
      }, character(1)) == 
        # compare with survey names
        substr(names(.data), 0, 3)
    )
  )
}