new_extract_fun <- function(df, survey_id_c, variable_recode) {
  
  message(survey_id_c)
  
  if(!survey_id_c %in% variable_recode$survey_id) {
    warning("No survey found in recode sheet")
    return(NULL)
  }
  
  
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
      mutate(subject_id = row_number()) %>% 
      select(-any_of("old_subject_id")) # this is a bodge to deal with NA's in subject_id # deal with that later
    
    
  # }
}

#### haven::zap_labels()

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
  
  recode_columns <- unique(filter(value_recode, survey_id == survey_id_c)$variable)
  
  df <- haven::zap_labels(df)
    
  df <- df %>%
    mutate(
      # across(everything(), haven::zap_labels(df)),
      across(any_of(recode_columns), ~new_val_recode(.x, cur_column(), survey_id_c)),
      survey_id = survey_id_c 
    ) %>%
    type.convert(as.is = TRUE) %>%
    select(survey_id, everything())
  
    
  }



### Slimmed down rds weighting --> will update this to include the code that breaks down things like age to individual levles.
## will also roll into new_recode_survey_variables I think
rds_adjust <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
  
  message(survey_id_c)
  
  surv_type <- str_sub(survey_id_c, 8, -5)
  
  if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "duration_yr" %in% colnames(df) ) {
  
    df <- df %>% 
      mutate(duration1 = factor(duration_yr)) %>% 
      filter(!is.na(coupon1),
             coupon1 != "")
    
    median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
    
    df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
    
    nboot <- 30
    
    # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
    # vars <- "age1"
    vars <- "duration1"
    coupons <- colnames(df)[grep("^coupon", colnames(df))]
    
    num_coupons <- length(coupons)
    
    df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                        subject.coupon='own_coupon', 
                                        # coupon.variables=c("coupon1","coupon2","coupon3"),
                                        coupon.variables = coupons)
    
    df <- as.rds.data.frame(df, id='subject_id', 
                            recruiter.id='recruiter.id',
                            network.size='network_size',
                            population.size=c(NA,NA,NA), 
                            # max.coupons=3, 
                            max.coupons = num_coupons,
                            notes=NULL)
    
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
      bind_rows() %>% 
      mutate(survey_id = survey_id_c)
    
  } else {
    NULL
  }

}}

rds_adjust_dur4 <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age_startsw" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(duration = age - age_startsw) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(duration4 = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "duration4"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
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
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}

rds_adjust_dur3 <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age_fs_paidorgift" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(duration = age - age_fs_paidorgift) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(duration3 = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "duration3"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
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
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}



rds_adjust_dur2 <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age_fs_paid" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(age = as.numeric(age), 
               age_fs_paid= as.numeric(age_fs_paid),
          duration = age - age_fs_paid) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(duration2 = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "duration2"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
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
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}


rds_adjust_durmsm <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age_fs_man" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(duration = age - age_fs_man) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(durationmsm = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "durationmsm"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
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
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}



rds_adjust_durmsm2 <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age_fs_man_anal" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(duration = age - age_fs_man_anal) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(durationmsm2 = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "durationmsm2"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
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
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}



rds_adjust_pwid <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age_inject" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(duration = age - age_inject) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(durationpwid = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "durationpwid"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
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
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}

rds_adjust_dur1 <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "inject_dur" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(duration = inject_dur) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(duration1 = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "duration1"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
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
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}


rds_adjust_age <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        # mutate(duration = inject_dur) %>% 
        filter(!age < 0,
               !age > 80) %>% 
        mutate(age1 = factor(age)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "age1"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
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
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}


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


multinomial_model <- function(formula, model_name, S = 1000) {
  
  message(paste0("Begin fitting ", model_name, "."))
  
  fit <- inla(formula, data = df, family = 'xPoisson',
              control.predictor = list(link = 1),
              control.compute = list(dic = TRUE, waic = TRUE,
                                     cpo = TRUE, config = TRUE),
              inla.mode = "experimental")
  
  message(paste0("Completed fitting ", model_name, "."))
  
  message("Begin post-processing")
  
  #' Full R-INLA samples
  full_samples <- inla.posterior.sample(n = S, result = fit)
  
  #' Just the latent field
  eta_samples <- lapply(full_samples, "[", "latent")
  
  #' For some reason "latent" is comprised of more than only the latent field
  eta_samples <- lapply(eta_samples, function(eta_sample) {
    data.frame(eta_sample) %>%
      tibble::rownames_to_column() %>%
      rename(eta = 2) %>%
      filter(substr(rowname, 1, 10) == "Predictor:") %>%
      select(-rowname)
  })
  
  #' Into a matrix with a row for each observation and a column for each sample
  eta_samples_matrix <- matrix(unlist(eta_samples), ncol = S)
  eta_samples_df <- data.frame(eta_samples_matrix)
  
  samples <- eta_samples_df  %>%
    mutate(
      #' To split by
      obs_idx = df$id.ref,
      #' To sample predictive
      #' When n_eff_kish is missing there is no survey for that observation,
      #' so the posterior predictive is meaningless. Setting to zero may save
      #' some computation, but probably better to filter out entirely.
      n_eff_kish_new = floor(ifelse(is.na(df$n_eff_kish), 0, df$n_eff_kish))
    ) %>%
    split(.$obs_idx) %>%
    mclapply(function(x) {
      n_eff_kish_new <- x$n_eff_kish_new
      #' Remove the obs_idx and n_eff_kish_new columns
      x_samples <- x[1:(length(x) - 2)]
      #' Normalise each column (to avoid overflow of softmax)
      # x_samples <- apply(x_samples, MARGIN = 2, FUN = function(x) x - max(x))
      #' Exponentiate (can be done outside apply)
      #' WARNING: That these are samples from lambda posterior isn't true! Come back to this
      lambda_samples <- exp(x_samples)
      #' Calculate samples from posterior of probabilites
      prob_samples <- apply(lambda_samples, MARGIN = 2, FUN = function(x) x / sum(x))
      #' Calculate predictive samples (including sampling variability)
      prob_predictive_samples <- apply(prob_samples, MARGIN = 2, FUN = function(x) {
        stats::rmultinom(n = 1, size = n_eff_kish_new, prob = x) / n_eff_kish_new
      })
      #' Return list, allowing extraction of each set of samples
      list(
        lambda = data.frame(lambda_samples),
        prob = data.frame(prob_samples),
        prob_predictive = data.frame(prob_predictive_samples))
    })
  
  lambda_samples_df <- bind_rows(lapply(samples, "[[", "lambda"))
  prob_samples_df <- bind_rows(lapply(samples, "[[", "prob"))
  prob_predictive_samples_df <- bind_rows(lapply(samples, "[[", "prob_predictive"))
  
  #' Helper functions
  row_summary <- function(df, ...) unname(apply(df, MARGIN = 1, ...))
  median <- function(x) quantile(x, 0.5, na.rm = TRUE)
  lower <- function(x) quantile(x, 0.025, na.rm = TRUE)
  upper <- function(x) quantile(x, 0.975, na.rm = TRUE)
  
  #' Quantile of the observation within posterior predictive
  prob_predictive_quantile <- prob_predictive_samples_df %>%
    mutate(estimate = df$estimate[!is.na(df$estimate)]) %>%
    apply(MARGIN = 1, function(x) {
      estimate <- x[S + 1]
      samples <- x[1:S]
      if(all(is.na(samples))) return(NA)
      else ecdf(samples)(estimate)
    })
  
  #' Calculate mean, median, lower and upper for each set of samples
  df <- df %>%
    mutate(
      lambda_mean = row_summary(lambda_samples_df, mean),
      lambda_median = row_summary(lambda_samples_df, median),
      lambda_lower = row_summary(lambda_samples_df, lower),
      lambda_upper = row_summary(lambda_samples_df, upper),
      prob_mean = row_summary(prob_samples_df, mean),
      prob_median = row_summary(prob_samples_df, median),
      prob_lower = row_summary(prob_samples_df, lower),
      prob_upper = row_summary(prob_samples_df, upper),
      prob_predictive_mean = row_summary(prob_predictive_samples_df, mean),
      prob_predictive_median = row_summary(prob_predictive_samples_df, median),
      prob_predictive_lower = row_summary(prob_predictive_samples_df, lower),
      prob_predictive_upper = row_summary(prob_predictive_samples_df, upper),
      prob_predictive_quantile = prob_predictive_quantile,
      model = model_name
    )
  
  message("Completed post-processing")
  
  return(list(df = df, fit = fit))
}







rds_adjust_partnerage <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  partneragedf = list()
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    agevars <- c("age_3rdlastpartner", "age_2ndlastpartner", "age_4thlastpartner", "age_5thlastpartner", "age_lastpartner", "client_age", "client_agediff", "partner1_age", "partner2_age", "partner3_age", "partner4_age", "partner5_age")
    
    survey_age_vars <- agevars[agevars %in% colnames(df)]
    
    test_fun <- function(agevar) {
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & agevar %in% colnames(df) ) {
      
      
      df <- df %>% 
        mutate(partner_age = factor(!!sym(agevar))) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "partner_age"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
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
        bind_rows() %>% 
        mutate(survey_id = survey_id_c,
               agequestion = agevar)
      
      partneragedf[[agevar]] <- df
      
    } else {
      NULL
    }
    }
    
    df <- lapply(survey_age_vars, test_fun) %>%
      bind_rows()
    
  }}

    