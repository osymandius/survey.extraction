library(RDS)
library(tidyverse)
library(rdhs)
library(sf)
library(spdep)
library(countrycode)
library(stringdist)
library(multi.utils)
library(moz.utils)

setwd("C:/Users/rla121/OneDrive - Imperial College London/Documents/GitHub/survey-extraction/")
source("src/kp_recoding_functions_21_02.R")
ssa_iso3 <- moz.utils::ssa_iso3()

recoding_sheet <-  read_csv("data/recoding_sheet.csv", locale = locale(encoding = "ISO-8859-1"))

## RDS variables
rds_vars <- c( "survey_city", "network_size", "coupon1", "coupon2", "coupon3", "coupon4", "coupon5", "coupon6", "coupon7", "coupon8", "own_coupon", "age", "network_size2", "network_size3", "hiv")

variable_recode <- recoding_sheet %>% 
  select(survey_id, variable, var_raw, study_type) %>% 
  mutate(survey_id2 = survey_id) %>% 
  separate(survey_id2, c(NA, "file_type")) %>% 
  distinct() %>%
  mutate(analysis = "kp") %>% 
  filter(variable %in% c(rds_vars)) %>% 
  filter(survey_id %in% c("CIV2012ACA_FSW", "NAM2019BBS_FSW", "NAM2019BBS_MSM", "SSD2016BBS_FSW", "SSD2017BBS_FSW", "SSD2019BBS_FSW", "SSD2019BBS2_FSW", "UGA2012BBS_FSW", "UGA2012BBS_MSM", "UGA2012BBS_PWUD", "UGA2021BBS_FSW", "UGA2021BBS_MSM", "ZAF2017BBS_MSM", "ZMB2017BBS_FSW", "ZMB2021BBS_MSM", "ZMB2021BBS_PWID", "ZMB2023BBS_FSW", "ZAF2014BBS_FSW", "ZAF2019BBS_MSM", "ZAF2019BBS_MSM"))


value_recode <- recoding_sheet %>% 
  rename(value = val_recode) %>% 
  filter(!is.na(val_raw)) 


paths <- list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/", recursive = TRUE, pattern = ".rds", full.names = TRUE) %>%
  lapply(., grep, pattern = "[A-Z]{3}[0-9]{4}[A-Z]{3,5}\\_(FSW|PWID|MSM|TGW|TG).rds", value= T) %>%
  unlist()

combined_datasets <- lapply(paths, readRDS)

surv_ids <- str_split(paths, "/") %>%
  lapply(tail, 1) %>%
  unlist() 

surv_ids <- str_remove(surv_ids, ".rds")

names(combined_datasets) <- surv_ids

all_extracted <- combined_datasets %>% 
  Map(new_extract_fun,
      df = .,
      survey_id = names(.),
      list(variable_recode)
  )

all_extracted <- compact(all_extracted)
# 
# debugonce(new_extract_fun)
# extrac <- new_extract_fun()

all_recoded <- all_extracted[!names(all_extracted) %in% c("BEN2008ACA_FSW")] %>% 
  Map(new_recode_survey_variables,
      df = .,
      survey_id = names(.),
      list(value_recode))


all_recoded <- lapply(all_recoded, as.data.frame)


# all_recoded$UGA2021BBS_FSW <- data.frame(result$UGA2021BBS_FSW)

all_recoded$UGA2021BBS_MSM <- all_recoded$UGA2021BBS_MSM %>% 
  rownames_to_column() %>% 
  filter(!((own_coupon == coupon3)),
         !(coupon1 == coupon2),
         !(rowname == "599"),  
         !(subject_id %in%  c("1371", "899")))


split_by_survey_city <- function(df_list) {
  result <- list()
  
  for (name in names(df_list)) {
    df <- df_list[[name]]
    
    if ("survey_city" %in% names(df)) {
      split_dfs <- split(df, df$survey_city)
      
      for (city in names(split_dfs)) {
        new_name <- paste0(name, "_", city)
        result[[new_name]] <- split_dfs[[city]]
      }
    } else {
      result[[name]] <- df
    }
  }
  
  return(result)
}

# Apply the function to the list of data frames
split_list_of_dfs <- split_by_survey_city(all_recoded)

split_list_of_dfs$UGA2012BBS_MSM_Kampala <- split_list_of_dfs$UGA2012BBS_MSM_Kampala %>% 
  mutate(across(starts_with("coupon"), ~extract_numeric(.))) 
split_list_of_dfs$UGA2012BBS_FSW_Kampala <- split_list_of_dfs$UGA2012BBS_FSW_Kampala %>% 
  mutate(across(starts_with("coupon"), ~extract_numeric(.))) 

rds_hiv <- split_list_of_dfs %>% 
# [!names(split_list_of_dfs) %in% c("UGA2012BBS_FSW_Kampala", "UGA2012BBS_MSM_Kampala", "ZMB2017BBS_FSW_Solwezi", "ZMB2017BBS_FSW_Ndola", "ZMB2017BBS_FSW_Livingstone")] %>% 
  Map(rds_adjust_hiv, #hiv
      df = .,
      # survey_id = unique(df$survey_id),
      variable_recode = list(variable_recode))
# 
# debugonce(rds_adjust_hiv)
# rds <- rds_adjust_hiv(data.frame(split_list_of_dfs$ZAF2014BBS_FSW_Durban), variable_recode = variable_recode)
# # 
# rds <- rds_adjust_hiv(data.frame(split_list_of_dfs$SSD2017BBS_FSW_Nimule), variable_recode = variable_recode)
# # 
# rds <- split_list_of_dfs$UGA2012BBS_MSM_Kampala %>%
#   # mutate(across(starts_with("coupon"), ~extract_numeric(.))) %>%
#   rds_adjust_hiv(variable_recode = variable_recode)

# rds_hiv <- all_recoded[!names(all_recoded) %in% c("UGA2012BBS_FSW", "UGA2012BBS_MSM")] %>% 
#   Map(group_split(survey_city)) %>% 
#   Map(rds_adjust_hiv, #hiv
#       df = .,
#       survey_id = names(.),
#       list(variable_recode))

rdshivs <- Map(function(df, name) {
  df$df_name <- name
  return(df)
}, rds_hiv, names(rds_hiv)) %>% 
  bind_rows() %>% 
  separate(df_name, into = c("survey", "kp", "survey_city"), sep = "_", remove = F) %>% 
  select(-df_name, -kp, -survey, -var, -se, -des_effect) %>% 
  filter(category == 1)



write_csv(rdshivs, "C:/Users/rla121/Downloads/rds_hiv6.csv")


rdshiv2 <- read_csv("C:/Users/rla121/Downloads/rds_hiv6.csv")  %>% 
  mutate(rds_estimate = estimate*100, 
         lower = lower*100,
         upper = upper*100) %>% 
  select(-estimate) %>% 
  filter(!is.na(report_value)) %>% 
  mutate(ratio = rds_estimate/report_value,
         lower_ratio = lower/lower_ci,
         upper_ratio = upper/upper_ci,
         survey = paste0(survey_id, " ", survey_city))
  # pivot_longer(cols = c("report_value", "rds_estimate"), names_to = "estimate", values_to = "value") %>% 
  # mutate(lower = ifelse(estimate == "report_value", lower_ci, lower),
  #        upper = ifelse(estimate == "report_value", upper_ci, upper)
  #        )

rdshiv2 %>% 
  pivot_longer(cols = c("report_value", "rds_estimate"), names_to = "estimate", values_to = "value") %>%
  mutate(lower = ifelse(estimate == "report_value", lower_ci, lower),
         upper = ifelse(estimate == "report_value", upper_ci, upper)
         ) %>% 
  ggplot(aes(y = survey,  x = value)) + 
  geom_col(aes(fill = estimate), position = position_dodge()) + 
  # theme(axis.text.x =element_text(angle = 45)) + 
  geom_errorbar(aes(xmin = lower, xmax = upper, color = estimate), position = position_dodge()) + 
  labs(x = "HIV Prevalence: our RDS weighting (rds_estimate), compared with \nthe published report (report_value)")
  
rdshiv2 %>% 
  ggplot(aes(y = survey, x = ratio)) + 
  geom_col(fill = "lightblue") + 
  # theme(axis.text.x =element_text(size = 8, angle = 45)) +
  geom_errorbar(aes(xmin = lower_ratio, xmax = upper_ratio), position = position_dodge()) +
  coord_cartesian(x = c(0, 2)) + 
  # scale_y_discrete(labels = wrap_format(10)) + 
  labs(y ="survey", x = "Ratio between reported and self-calculated RDS")

rdshiv2 %>% 
  pivot_longer(cols = c("n", "report_n"), names_to = "Denominator Source", values_to = "HIVdenom") %>% 
  ggplot() +
  # geom_point(aes(x = n, y = survey), color ="blue") +
  geom_point(aes(x = HIVdenom, y = survey, color = `Denominator Source`, shape = `Denominator Source`), size = 3) + 
  labs(x = "Number of HIV+ Individuals, \naccording to the individual level dataset (n) vs the report (report_n)")
  # scale_x_continuous()

debugonce(rds_adjust_hiv)
debugonce(rid.from.coupons)
rds <- data.frame(all_recoded$UGA2021BBS_FSW) 
rds <- rds_adjust_hiv(data.frame(all_recoded$UGA2021BBS_FSW), "UGA2021BBS_FSW", variable_recode)

debugonce(rds_adjust_hiv)
rds <- rds_adjust_hiv(all_recoded$UGA2021BBS_MSM, "UGA2021BBS_MSM", variable_recode)


debugonce(rds_adjust_hiv)
debugonce(get.seed.id)
rds <- all_recoded$UGA2021BBS_MSM %>%
  rownames_to_column() %>% 
  filter(!((own_coupon == coupon3)),
         !(coupon1 == coupon2),
         !(rowname == "599"), # Getting a Subjects can not recruit themselves. error for this one but not clear why 
         !(subject_id %in%  c("1371", "899"))) %>% # Error in get.seed(rec.id, history = c(history, i)) : Loop found in recruitment tree with id: 899/1371 etc - tried debugging and don't understand why/where the loop is
  rds_adjust_hiv("UGA2021BBS_MSM", variable_recode)




# Age 

make_age_groups <- function(df) {

    df <- df %>% 
      single_year_to_five_year(fifteen_to_49 = F) %>% 
      mutate(age_group = ifelse((age_group %in% c("Y015_019", "Y020_024") & survey_id %in% c("UGA2021BBS_MSM", "UGA2021BBS_FSW", "ZMB2017BBS_FSW" )), "Y015_024", age_group),
             age_group = ifelse((age_group %in% c("Y025_029", "Y030_034") & survey_id %in% c("UGA2021BBS_MSM", "ZMB2021BBS_PWID", "SSD2016BBS_FSW", "SSD2017BBS_FSW") ), "Y025_034", age_group),
             age_group = ifelse((age_group %in% c("Y035_039", "Y040_044", "Y045_049") & survey_id %in% c("SSD2016BBS_FSW", "SSD2017BBS_FSW")), "Y035_049", age_group), 
             age_group = ifelse((age_group %in% c("Y050_054", "Y055_059", "Y060_064", "Y065_069", "Y070_074", "Y075_079", "Y080_084") & survey_id %in% c("SSD2016BBS_FSW", "SSD2017BBS_FSW")), "Y50+", age_group), 
             age_group = ifelse((age_group %in% c("Y040_044", "Y045_049", "Y050_054", "Y055_059", "Y060_064", "Y065_069", "Y070_074", "Y075_079", "Y080_084") & survey_id %in% c("SSD2019BBS_FSW")), "Y40+", age_group), 
             age_group = ifelse((age_group %in% c("Y035_039", "Y040_044", "Y045_049", "Y050_054", "Y055_059", "Y060_064", "Y065_069", "Y070_074", "Y075_079", "Y080_084")), "Y035+", age_group), 
             age_group = ifelse((age_group %in% c("Y030_034","Y035+") & survey_id %in% c("ZMB2021BBS_MSM", "NAM2019BBS_FSW", "NAM2019BBS_MSM" )), "Y030+", age_group))
}

debugonce(make_age_groups)

grouped <- Map(function(df) {
  make_age_groups(df)
}, split_list_of_dfs)

rds_aggp <- grouped[!names(grouped) %in% c("UGA2012BBS_FSW_Kampala", "UGA2012BBS_MSM_Kampala", "ZMB2017BBS_FSW_Solwezi", "ZMB2017BBS_FSW_Ndola", "ZMB2017BBS_FSW_Livingstone")] %>%  
  Map(rds_adjust_age_group, 
      df = .,
      # survey_id = names(.),
      list(variable_recode))

rdsaggps <- Map(function(df, name) {
  df$df_name <- name
  return(df)
}, rds_aggp, names(rds_aggp)) %>% 
  bind_rows() %>% 
  separate(df_name, into = c("survey", "kp", "survey_city"), sep = "_", remove = F)

debugonce(rds_adjust_age_group)
rds <- rds_adjust_age_group(grouped$UGA2012BBS_FSW_Kampala, variable_recode)

rds_aggp2 <- bind_rows(rds_aggp)

write_csv(rds_aggp2, "C:/Users/rla121/Downloads/rds_ages.csv")



comparing_report <- read_csv("C:/Users/rla121/Downloads/rdsaggps.csv") %>% 
  mutate(rds_estimate = rds_estimate*100, 
         lower = lower*100,
         upper = upper*100) %>% 
  filter(!is.na(report_value)) %>% 
  pivot_longer(cols = c("report_value", "rds_estimate"), names_to = "estimate", values_to = "value") %>% 
  mutate(lower = ifelse(estimate == "report_value", report_low, lower),
         upper = ifelse(estimate == "report_value", report_upper, upper))
  # pivot_longer(cols = c("report_low", "lower"), names_to = "lower_est", values_to = "lowci_value") %>% 
  # pivot_longer(cols = c("report_upper", "upper"), names_to = "upper_est", values_to = "upci_value")

comparing_report %>% 
  ggplot(aes(x = category)) +
  geom_col(aes(y = value, fill = estimate), position = position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = upper, x = category, color = estimate), position = position_dodge()) + 
  facet_wrap(~survey_id + survey_city) + 
    moz.utils::standard_theme()

  

