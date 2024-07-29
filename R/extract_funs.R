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

#' Recode survey column
#' @param col Column passed from across()
#' @param col_name Column name passed by dplyr::cur_column()
#' @param survey_id_c Survey ID
#' @param dataset_type Dataset type e.g.....
#' @param analysis One of "circ", "sexbehav" etc

val_recode <- function(col, col_name, survey_id_c, dataset_type, analysis_c) {
  
  value_recode <- filter(value_recode, variable == col_name, survey_id == survey_id_c)

  # All values in the value book come one excel col which includes strings
  # => NAs are read as _NA_character which adversely effects later recoding
  
  # If, for a given variable, the values are only 0-9 and NA, force to be numeric.
  # if (!length(grep("[^0-9]", value_recode$value))) {
  #   value_recode$value <- as.numeric(value_recode$value)
  # }
  # 
  # if (!length(grep("[^0-9]", value_recode$val_raw))) {
  #   value_recode$val_raw <- as.numeric(value_recode$val_raw)
  # }

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

  recode(col, !!!vec)
}

#' Recode survey variables
#' @param df Survey dataset
#' @param survey_id_c Survey ID
#' @param value_recode Value recode excel sheet
#' @param dataset_type Dataset type e.g.....
#' @param analysis One of "circ", "sexbehav" etc
#' @export

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
    # type.convert(as.is = T) %>%
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

# 
# single_year_to_five_year <- function (df, fifteen_to_49 = TRUE, warning = T) 
# {
#   
#   message(unique(df$survey_id))
#   
#   if("age_group" %in% colnames(df)) {
#     warning(paste0(unique(df$survey_id), ": Age group already exists"))
#     return(df)
#   }
#   
#   if(!"age" %in% colnames(df)) {
#     if(warning == T) {
#       warning(paste0(unique(df$survey_id), ": No age column"))
#       return(df)
#     } else {
#       stop(paste0(unique(df$survey_id), ": No age column")) 
#     }
#   }
#   
#   df <- df %>% 
#     dplyr::mutate(age_group_label = cut(age, c(0, seq(5, 85, 5) - 1), c(paste0(seq(0, 79, 5), "-", seq(5, 80, 5) - 1), "80+"), include.lowest = TRUE)) %>%
#     dplyr::left_join(naomi::get_age_groups() %>% 
#                        select(age_group, age_group_label),
#                      by = "age_group_label") %>% 
#     dplyr::select(-age_group_label)
#   
#   if (fifteen_to_49) {
#     df %>% dplyr::filter(age %in% 15:49) %>% dplyr::select(-age)
#   }
#   else {
#     df %>% dplyr::select(-age)
#   }
# }

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
