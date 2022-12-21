new_extract_fun <- function(df, survey_id_c, variable_recode) {
  
  message(survey_id_c)
  
  #if(!survey_id_c %in% variable_recode$survey_id)
   # stop("No survey found in recode sheet")
  
  surv_type <- str_sub(survey_id_c, 8, -5)
  
  # if(str_detect(survey_id_c, "PLACE")) {
  #   
  #   # surv_type <- "PLACE"
  #   
  #   custom_recode <- filter(variable_recode, survey_id == str_sub(survey_id_c, end = -5))
  #   
  #   variable_df <- variable_recode %>%
  #     
  #     distinct(variable) %>%
  #     left_join(custom_recode, by="variable") %>% 
  #     filter(!is.na(var_raw))
  #   
  #   
  #   opt_var <- filter(variable_df)$var_raw %>% setNames(filter(variable_df)$variable)
  #   
  #   
  #   df <- df %>% 
  #     select(any_of(opt_var))
  #   
  # }
  # else if (surv_type == "BBS" | surv_type == "ACA")
  # {
    custom_recode <- filter(variable_recode, survey_id == survey_id_c)
    
    variable_df <- variable_recode %>%
      
      distinct(variable) %>%
      left_join(custom_recode, by="variable") %>% 
      filter(!is.na(var_raw))
    
    
    opt_var <- filter(variable_df)$var_raw %>% setNames(filter(variable_df)$variable)
    
    
    df <- df %>% 
      mutate(survey_id = survey_id_c) %>%
      select(survey_id, any_of(opt_var)) %>% 
      ungroup() %>%
      mutate(subject_id = row_number())
    
  # }
}


new_val_recode <- function(col, col_name, survey_id_c) {
  
  # surv_type <- str_sub(survey_id_c, 8, -5)
  # if(str_detect(survey_id_c, "PLACE")) {
  #   value_recode <- value_recode %>%
  #     filter(variable == col_name,
  #            survey_id == str_sub(survey_id_c, end = -5))
  #   
  #   if(!length(value_recode$value[!value_recode$value %in% c(0:9, NA)]))
  #     value_recode$value <- as.numeric(value_recode$value)
  #   
  #   vec <- value_recode$value
  #   names(vec) <- value_recode$val_raw
  #   
  #   recode(col, !!!vec)
  # }
  # 
  # else if (surv_type == "BBS" | surv_type == "ACA") {
  #   
    value_recode <- value_recode %>%
      filter(variable == col_name,
             survey_id == survey_id_c)
    
    if(!length(value_recode$value[!value_recode$value %in% c(0:9, NA)]))
      value_recode$value <- as.numeric(value_recode$value)
    
    vec <- value_recode$value
    names(vec) <- value_recode$val_raw
    
    unchanged_vec <- unique(col[!col %in% names(vec)])
    uv_no_na <-unchanged_vec[!is.na(unchanged_vec)]
    names(uv_no_na) <- uv_no_na
    
    
    # unchanged_vec_names <- unchanged_vec
    # unchanged_vec_names[is.na(unchanged_vec_names)] <- "none"
    # names(unchanged_vec) <- unchanged_vec_names
    
    vec <- c(vec, uv_no_na)
    
    recode(col, !!!vec)
    
  }


new_recode_survey_variables <- function(df, survey_id_c, value_recode) {
  
  message(survey_id_c)
  
  surv_type <- str_sub(survey_id_c, 8, -5)
  
  # if(surv_type == "PLACE") {
  #   
  #   recode_columns <- unique(filter(value_recode, survey_id == str_sub(survey_id_c, end = -5))$variable 
  #   )
  #   
  #   
  #   df <- df %>%
  #     mutate(across(everything(), as.numeric),
  #            across(any_of(recode_columns), ~new_val_recode(.x, cur_column(), survey_id_c)),
  #            survey_id = survey_id_c 
  #     ) %>%
  #     type.convert() %>%
  #     select(survey_id, everything())
  #   
  #   
  # }
  # 
  # else if (surv_type == "BBS" | surv_type == "ACA")
  # {
    recode_columns <- unique(filter(value_recode, survey_id == survey_id_c)$variable
                             
    )
    
    df <- df %>%
      mutate(
        # across(everything(), as.numeric),
             across(any_of(recode_columns), ~new_val_recode(.x, cur_column(), survey_id_c)),
             survey_id = survey_id_c 
      ) %>%
      type.convert(as.is = TRUE) %>%
      select(survey_id, everything()) %>% 
      ungroup() %>% 
      mutate(subject_id = row_number()) # this is a bodge to deal with NA's in subject_id # deal with that later
  
    
  }



### Slimmed down rds weighting --> will update this to include the code that breaks down things like age to individual levles.
## will also roll into new_recode_survey_variables I think
rds_adjust <- function(df, survey_id_c) {
  
  message(survey_id_c)
  
  surv_type <- str_sub(survey_id_c, 8, -5)
  
  if (surv_type=="BBS" & "coupon1" %in% colnames(df) & "age" %in% colnames(df) ) {
  
    df <- df %>% 
      mutate(age1 = factor(age)) %>% 
      filter(!is.na(coupon1),
             coupon1 != "")
    
    
    nboot <- 30
    
    #vars <- colnames(df)
    vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
    #df <- df %>% 
    #filter(!is.na(subject_id))
    
    
    
    df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                        subject.coupon='own_coupon', 
                                        coupon.variables=c("coupon1","coupon2","coupon3"))
    
    df <- as.rds.data.frame(df, id='subject_id', 
                            recruiter.id='recruiter.id',
                            network.size='network_size',
                            population.size=c(NA,NA,NA), 
                            max.coupons=3, notes=NULL)
    
    df$seed <- get.seed.id(df)
    df$wave <- get.wave(df)
    
    
    df <- lapply(vars, function(x) {
      
      freq <- RDS.bootstrap.intervals(df, outcome.variable=x,
                                      weight.type="RDS-II", uncertainty="Salganik", 
                                      confidence.level=0.95, 
                                      number.of.bootstrap.samples=nboot)
      
      cat <- length(freq$estimate)
      
      df <- data.frame(matrix(freq$interval, nrow = cat))
      colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
      
      df$category <- attr(freq$estimate, "names")
      
      df$var <- x
      
      df
    }) %>%
      bind_rows()
    
  }

}

rds_adjust2 <- function(df, survey_id_c) {
  
  message(survey_id_c)
  
  surv_type <- str_sub(survey_id_c, 8, -5)
  
  if (surv_type=="BBS") {
    
    subject.id = "subject_id" 
    subject.coupon = "own_coupon" 
    coupon.variables=c("coupon1", "coupon2", "coupon3")
    network_size = "network_size"
    
  # df <- df %>%
   #     dplyr::mutate(age_group_label = cut(age, c(0, seq(5, 85, 5)-1), c(paste0(seq(0, 79, 5), "-", seq(5, 80, 5)-1), "80+"), include.lowest=TRUE)) %>%
    #  dplyr::left_join(naomi::get_age_groups() %>% select(age_group, age_group_label)) %>%
     #  dplyr::select(-age_group_label) %>% 
    #filter(!(coupon1 == own_coupon))  
  
 # df <- df %>%
  #  dplyr::mutate(age_group_label = cut(age, c(0, seq(2, 81, 2)-1), c(paste0(seq(0, 77, 2), "-", seq(2, 79, 2)-1), "80+"), include.lowest=TRUE)) %>%
   #dplyr::left_join(naomi::get_age_groups() %>% select(age_group, age_group_label)) %>%
  #dplyr::select(-age_group_label) %>% 
#  filter(!(coupon1 == own_coupon)) 
    df <- df %>%
      dplyr::mutate(age_group_label = factor(age)) %>%
      dplyr::left_join(naomi::get_age_groups() %>% 
      select(age_group, age_group_label)) %>%
      #dplyr::select(-age_group_label) %>% 
      filter(!(coupon1 == own_coupon))  
   
    #df1b <- df %>% 
     # mutate(recruitor.id = rid.from.coupons(df, subject.id=subject.id, 
      #                                       subject.coupon=subject.coupon, 
       #                                      coupon.variables=coupon.variables))
    recruitor.id = "recruitor.id"
    
    #df <- as.rds.data.frame(df, id = subject.id, 
     #                 recruiter.id=recruitor.id,
      #                network.size = network_size,
       #               population.size=c(NA,NA,NA), 
        #              max.coupons=3, notes=NULL) 
    
  #    df$seed = get.seed.id(df)
   #          df$wave = get.wave(df)
    
    vars <- intersect(c("hiv", "age_fs","hepb", "syphilis", "age_first_paid"), colnames(df))
    
    df2 <- df %>% 
      group_by(age_group_label) %>% 
      group_split()
    
    names(df2) <- sort(unique(df$age_group_label))
    
    vars_by_age <- Map(function(x,y) {
      
      ## Once you've found which group fails, I use this to step through the function manually by specifying x and y
      #x <- df2[[7]]
      #y <- names(df2[7])
      
      message(y) ## This is useful in a Map loop so you can see where your error occurs
      
    nboot <- 30
    
    #vars <- colnames(df)
        #df <- df %>% 
    #filter(!is.na(subject_id))
    
    #x$recruiter.id <- rid.from.coupons(x, subject.id=subject.id, 
     #                                                      subject.coupon=subject.coupon, 
      #                                                     coupon.variables=coupon.variables)
    x$recruitor.id <- rid.from.coupons(x, subject.id=subject.id, 
                                                      subject.coupon=subject.coupon, 
                                                      coupon.variables=coupon.variables)
    
    x <- as.rds.data.frame(x, id = subject.id, 
                           recruiter.id=recruitor.id,
                          network.size = network_size,
                         population.size=c(NA,NA,NA), 
                           max.coupons=3, notes=NULL)
    
    x$seed <- get.seed.id(x)
    x$wave <- get.wave(x)
    
    possibly_bootstrap <- purrr::possibly(RDS.bootstrap.intervals, otherwise = NULL)
    
    freq <- possibly_bootstrap(x, outcome.variable="hiv",
                                    weight.type="RDS-II",
                                    uncertainty="Salganik",
                                    confidence.level=0.95, 
                                    number.of.bootstrap.samples=nboot)
    
    ## Then something that does 
    
    # if(is.null(freq)) {
    #   something sensible to return e.g. unweighted estimate? 
    #   and then some dataframe that labels which estimates are RDS adjusted and which are raw?
    # }

    
    cat <- length(freq$estimate)
    
    df <- data.frame(matrix(freq$interval, nrow = cat))
    colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
    
    df$category <- attr(freq$estimate, "names")
    
    df$var <- "hiv"
    
    df$age_group <- y
    
    df
    
    }, x = df2[13:19], y = names(df2[13:19])) %>%
      bind_rows()
  }
}
    