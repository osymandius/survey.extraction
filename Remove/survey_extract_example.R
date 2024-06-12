library(RDS)
library(tidyverse)
library(rdhs)
# library(INLA)
library(sf)
library(spdep)
library(countrycode)
library(stringdist)
# library(splines)
library(multi.utils)
library(moz.utils)

setwd("C:/Users/rla121/OneDrive - Imperial College London/Documents/GitHub/survey-extraction/")
source("src/kp_recoding_functions_21_02.R")
ssa_iso3 <- moz.utils::ssa_iso3()


### Recoding vars / values
      ## Analysis and file_type redundant for the time being
# recoding_sheet <-  read_csv("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/00Admin/recoding_sheet.csv")

## Sample survey for trialing functions

# path2 <- list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "MWI2019BBS_FSW.rds", full.names = TRUE, recursive = TRUE)
# #path2 <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "BEN2002BBS_FSW.rds", full.names = TRUE, recursive = TRUE)
# 
# mwidat <- lapply(path2, readRDS)
# ## Trying new_extract_fun
# debugonce(new_extract_fun)
# wow <-  new_extract_fun(mwidat[[1]], "MWI2019BBS_FSW", variable_recode)
# 
# ## Trying for PLACE 
# placepath <- list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "AGO2018PLACE_TGW.rds", full.names = TRUE, recursive = TRUE)
# #placepath <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "AGO2018PLACE_TGW.rds", full.names = TRUE, recursive = TRUE)
# placedat <- lapply(placepath, readRDS)
# place_recode <- new_extract_fun(placedat[[1]], "AGO2018PLACE_TGW", variable_recode)
# 
# ## Trying new_val_recode
# #wow2 <- wow %>%
#  # mutate(cdm_last_paid = as.numeric(cdm_last_paid),
#   #       cdm_last_paid = new_val_recode(cdm_last_paid, "cdm_last_paid", "BEN2002BBS_FSW"))
# ## Trying new_recode_survey_variables
# wow3 <- new_recode_survey_variables(wow, "MWI2019BBS_FSW", value_recode)
# placevals <- new_recode_survey_variables(place_recode, "AGO2018PLACE_TGW", value_recode)
# 
# ### Trying RDS -> this is not working --> going wrong with new_recode_survey_variables - it's not dealing with strings very well. 
# swzpath <- list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "SWZ2020BBS_FSW.rds", full.names = TRUE, recursive = TRUE)
# #swzpath <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = "SWZ2020BBS_FSW.rds", full.names = TRUE, recursive = TRUE)
# 
# swzdat <- lapply(swzpath, readRDS)
# swzwow <- new_extract_fun(swzdat[[1]], "SWZ2020BBS_FSW", variable_recode)
# # debugonce(new_recode_survey_variables)
# swzwow2 <- new_recode_survey_variables(swzwow, "SWZ2020BBS_FSW", value_recode)
# debugonce(rds_adjust2)
# rds_trial <- rds_adjust2(wow3, "MWI2019BBS_FSW")
# 
# #### Surveys to be recoded

#age_fs <- age_cleaning(all_extracted[[29]], "NAM2019BBS_FSW")
#paths <- intersect(list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = paste(survey_id, collapse = "|")  , full.names = TRUE, recursive = TRUE), list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = ".rds"  , full.names = TRUE, recursive = TRUE))

#paths <- intersect(list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = paste(unique(variable_recode$survey_id), collapse = "|")  , full.names = TRUE, recursive = TRUE), list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", pattern = ".rds"  , full.names = TRUE, recursive = TRUE))

# paths <- list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/", recursive = TRUE, pattern = ".rds", full.names = TRUE) %>%


recoding_sheet <-  read_csv("data/recoding_sheet.csv", locale = locale(encoding = "ISO-8859-1"), show_col_types = F)

#######################
## Age and duration variables
# c( "survey_city", "network_size", "coupon1", "coupon2", "coupon3", "coupon4", "coupon5", "coupon6", "coupon7", "coupon8", "own_coupon", "age", "inject_yr", "network_size", "age_fs_paid", "age_fs_paidfor", "age_fs_paidorgift", "age_inject", "age_startsw", "age_startsw_cat", "duration_yr", "inject_dur" , "sex", "age_fs_man_anal", "age_fs_man" , "age_fs_woman", "age_fs_vag", "hiv"))

## Older MSM
# c( "network_size", "coupon1", "coupon2", "coupon3", "coupon4", "coupon5", "coupon6", "coupon7", "coupon8", "own_coupon", "age", "age_cat", "hiv", "age_2ndlastpartner", "age_3rdlastpartner", "age_4thlastpartner", "age_5thlastpartner", "age_lastpartner", "client_age", "client_agediff", "partner1_age", "partner2_age", "partner3_age", "partner4_age", "partner5_age", "sw_age", "sw_agediff", "partners_male_Y010_014", "partners_male_Y015_019", "partners_male_Y020_024", "partners_male_Y025_029", "partners_male_Y030_034", "partners_male_Y030_39", "partners_male_Y040_044", "partners_male_Y045_049", "partners_male_Y040_049", "partners_male_Y050up"))

# Intersection 
# c("age_inject", "inject_3mnths", "inject_6mnths", "inject_dur", "inject_ever", "inject_freq") 
# c("age_startsw", "age_startsw_cat", "identifies_sw", "primary_income_sw", "sw", "age_fs_paid", "age_fs_paidfor", "paid_by_lastpartner", "paid_by_2ndlastpartner", "paid_by_3rdlastpartner", "paid_by_4thlastpart", "paid_by_5thlastpart", "paidbywoman_6mnths", "paidbywoman_12mnths", "paidorgifted_by_man", "paidorgiftsbywoman_12mnths", "paidorgiftsex_man_3mnth", "paidorgiftsex_man_6mnth", "paidorgiftsex_man_lastmnth", "drugs_for_sex", "female_clients_6mnths", "giftsfromwoman", "duration_yr", "duration_mnth", "clients_yr", "clients_pw", "clients_6mnths", "clients_lastwk", "clients_lastmnth")

# Julia
# Time engaging in sex work
# "age_fs_paid", "age_fs_paidfor", "age_fs_paidorgift", "age_inject", "age_startsw", "age_startsw_cat", "duration_yr", "age"
# age_fs, age_fs_vag, age_fs_man, age_fs_anal, age_fs_man_anal
# "children", "children_under5", "children_alive", "children_dead", "children_young" [this is kids under 12]
# "cdm_consistency", "cdm_last", "cdm_last_3events_client", "cdm_last_3events_nppp", "cdm_last_anal", "cdm_last_anal_c1", "cdm_last_anal_c2", "cdm_last_anal_c3", "cdm_last_anal_nc", "cdm_last_anal_npp1", "cdm_last_anal_npp2", "cdm_last_anal_rc", "cdm_last_client", "cdm_last_client1", "cdm_last_paid", "cdm_last_paid1", "cdm_last_sex", "cdm_last_vag_ltp", "cdm_last_vag_nc", "cdm_last_vag_rc", "cdm_lastclient1", "cdm_lastclient2", "cdm_lastclient2", "cdm_lastclient3", "cdm_lastnpp1", "cdm_lastnpp2", "cdm_often", "cdm_often_anal", "cdm_often_anal_client1", "cdm_often_anal_client2", "cdm_often_anal_client3", "cdm_often_anal_npp1", "cdm_often_anal_npp2", "cdm_often_c1_prop", "cdm_often_c2_prop", "cdm_often_c3_prop", "cdm_often_client_prop", "cdm_often_npp", "cdm_often_npp_prop", "cdm_often_npp1_prop", "cdm_often_npp2_prop", "cdm_often_vag"

# Intersection work
# Demographic variables
demvars <- c("age", "sex", "gender", "tg","education", "marital", "children", "abroad", "religion", "pregnant", "away_places_yr", "away_abroad_yr", "away_othertown", "local_travel")    

# Identity variables
identityvars <- c("sex_samegender_mnth", "sex_lastpartner", "sex_2ndlastpartner",  "sex_3rdlastpartner", "sex_5thlastpartner" , "sex_4thlastpartner", "partners_currentsex", "msm_only", "msm_andwomen")

# SW variables
swvars <- c("workplace_street", "workplace", "swabroad" , "sw_locations", "swabroad_6mnths" , "swothercity_6mnths",  "transac", "primary_income_sw", "paidorgiftsex_man_lastmnth", "paidorgiftsex_man_3mnth", "paidorgiftsex_man_6mnth", "paidorgifted_by_man", "paidbywoman_6mnths", "paidbywoman_12mnths" , "paid_transac_sex_6mnths", "paid_sex_ever", "paid_sex_man_12mnths", "paid_sex_12mnths" , "paid_sex_6mnth", "giftsfromwoman", "drugs_for_sex", "drugs_for_sex_12mnth",  "age_startsw" , "age_startsw_cat", "paidby_lastpartnerman", "paidbyman_mnth_count","paidbyman_wk_count", "paidbyman_6mnth_count", "paidbyman_3mnth_count") 

# MSM variables
msmvars <- c("sexabroad", "circ_status", "spouse_gender", "msm_6mnth")

# Drug variables
drugvars <- c("share_needleyr" , "share_needle6mnth" , "share_needle", "share_needle_norm", "share_needle_last", "share_needle_first_inj", "share_needle_hormones", "inject_yr", "inject_6mnths", "inject_ever", "inject_freq", "inject_dur", "noninject_lastmnth", "drug_lastmnth","noninject_6mnths","inject_hormones_yr", "share_needle_hormones_yr")

# Partner variables
partnervars <- c("partners_np_wk", "partners_male_6mnths", "partners_6mnths", "partners_msw_6mnth", "partners_tsw_6mnths" ,"numregpart","partners_regular_6mnths", "casual_partners_6mnths", "numcasualpart_12mnth", "np_partners_wk" , "np_part_mnth", "np_partners_3mnth", "np_part_12mnths", "partners_npp_6mnths", "np_clients_6mnths" , "no_swpartners_yr" , "newclients_mnth", "male_partners_regular_currently", "male_partners_currently", "male_partners_mnth", "male_partners_lastmnth", "male_partners_6mnths", "male_partners_3mnths", "male_partners_12mnths" , "male_partners", "male_partners_lastwk", "female_partners_lastwk" ,"female_partners_12mnth", "female_clients_6mnths", "female_partners_6mnths", "partners_femalenpp_6mnths", "clients_yr", "clients_yesterday", "clients_mnth", "clients_lastwk", "clients_lastmnth", "clients_6mnths", "clients_3mnths", "allpartners_6mnths" , "allpartners_yr", "regclients_wk", "newclients_wk", "clients_trans_6mnths")


# Programme variables
programvars <- c("programme_access", "anc_attended", "sw_program", "msm_program", "peer_educator", "hiv_activities")

# Cdm use
cdmvars <- c("cdm_lastpart", "cdm_lastnpp2", "cdm_lastnpp1", "cdm_lastclient3", "cdm_lastclient2", "cdm_lastclient1", "cdm_last_paid", "cdm_last_gifter", "cdm_last_main", "cdm_last_gift", "cdm_last_gifter", "cdm_last_funded", "cdm_last_female", "cdm_last_casual", "cdm_last_3events_nppp" , "cdm_last_anal", "cdm_last_3events_client", "cdm_last_reg_client", "cdm_last_new_client", "cdm_last_npp", "cdm_last_reg", "cdm_last_tsw" , "cdm_last_msw", "cdm_last_transclient", "cdm_last_femalenpp", "cdm_last_regclient")

# We don't generally have cdm use at last ins/rec - we have cdm_often_insertive / cdm_often_receptive for two surveys presently
# Abuse
stigmavars <- c("verbal_abuse", "verbal_abuser", "sexual_abuse", "sexual_abuser", "rape", "prison_ever", "prison_12mnths", "arrested_msm", "minor_physical_abuse", "minor_phsyical_abuser" , "major_physical_abuser", "major_physical_abuse", "fgm", "abuse_client_6mnth", "beaten_client", "beaten_client_count_mth", "insulted_client", "rape_client_6mnth", "raped_ever", "raped_age", "raped_yr", "raped_police_yr", "rape_police_often", "rape_police_ever","rape_npp", "raped_firstsex", "raped_becausemsm", "beaten_npp_mnth","beaten_npp_ever" , "beaten_npp_count_mnth" , "police_threaten_sw", "police_abuse" , "healthc_stigma_fear", "hc_avoid", "hc_refused_msm", "stigmafear_sticentre", "stigmafear_healthcare")

# Alcohol 
alcoholvars <- c("alcohol_wk_count", "alcohol_often_mnth", "alcohol")

# HIV services
hivvars <- c("prep_ever", "prep_6mnths", "pep_ever", "pep_6mnths", "onart", "hivtest_whenlast", "hivtest_whenlastcat", "hivtest_whenlastcat2", "hivtest_12mnths", "sti_treat", "kp_hivcare")

# Biomarkers
biomarkvars <- c("syphilis", "hiv", "hepb", "hepc", "gon", "chlam", "vl_result_suppressed", "vl_result_detectable", "vl_result_cat", "vl_result_count")

# Ins rec
insrecvars <- c("insertive_wk", "insertive_unpr_6mnths", "insertive_lastpart_6mnth", "insertive_6mnth", "ins_rec_pref", "insertive_12mnth", "ins_rec", "insertive_mnth", "receptive_mnth", "ins_rec_reg", "ins_rec_casual", "ins_rec_client", "ins_rec_main", "ins_rec_npp", "ins_rec_tgw", "ins_rec_tsw", "ins_rec_transclient")

# ins_rec_casual + ins_rec_client + ins_rec_reg -_> all last time you had sex

#########
variable_recode <- recoding_sheet %>% 
  select(survey_id, variable, var_raw, study_type) %>% 
  # mutate(survey_id2 = survey_id) %>% 
  # separate(survey_id2, c(NA, "file_type")) %>% ## What is this doing without a sep = position argument?
  distinct()
  # mutate(analysis = "kp") 
  # filter(variable %in% c(demvars, drugvars, alcoholvars, biomarkvars, hivvars, identityvars, insrecvars, msmvars, partnervars, programvars, swvars, stigmavars))
  # filter(variable %in% c("survey_city", "network_size", "coupon1", "coupon2", "coupon3", "coupon4", "coupon5", "coupon6", "coupon7", "coupon8", "own_coupon", "age", "hiv")) 

value_recode <- recoding_sheet %>% 
  rename(value = val_recode) %>% 
  filter(!is.na(val_raw)) 
  

# paths <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/", recursive = TRUE, pattern = ".rds", full.names = TRUE) %>%
#   lapply(., grep, pattern = "[A-Z]{3}[0-9]{4}[A-Z]{3,5}\\_(FSW|PWID|MSM|TGW|TG).rds", value= T) %>%
#   unlist()

paths <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/", recursive = TRUE, pattern = ".rds", full.names = TRUE) %>%
  lapply(., grep, pattern = "[A-Z]{3}[0-9]{4}[A-Z]{3,5}_(FSW|PWID|MSM|TGW|TG).rds", value= T) %>%
  unlist()

combined_datasets <- lapply(paths, readRDS)

# combined_datasets <- list(readRDS(paths[5]))

surv_ids <- str_split(paths, "/") %>%
  lapply(tail, 1) %>%
  lapply(function(x) str_remove(x, ".rds")) %>%
  unlist()
  
names(combined_datasets) <- surv_ids

old <- new_extract_fun(combined_datasets[[5]], survey_id = names(combined_datasets[5]), variable_recode)

variable_recode <- readxl::read_excel("data/hivdata_survey_datasets.xlsx", sheet = "variable_recode", col_types = "text") %>%
  filter(!is.na(var_raw), ## @Rebecca: AGO2018PLACE_FSW has NA for a var_raw entry.. There may be others too
         is.na(notes) ## Check this col in the sheet please!
         ) 

debugonce(extract_survey_vars)
new <- extract_survey_vars(combined_datasets[[1]], survey_id = names(combined_datasets[1]), variable_recode, dataset_type = "kp", analysis_c = NA)

all_extracted <- 
  # combined_datasets %>% 
  combined_datasets[!names(combined_datasets) %in% c("MLI2017ACA_FSW" # This survey isn't in the recode sheet
  )] %>%
  Map(extract_survey_vars,
      df = .,
      survey_id = names(.),
      list(variable_recode),
      dataset_type = "kp",
      analysis_c = NA
      )

duplicated <- names(all_extracted)[duplicated(names(all_extracted))]

if(length(duplicated))
  stop(paste0(names(all_extracted)[duplicated(names(all_extracted))], " duplicated"))

all_extracted <- all_extracted[!names(all_extracted) %in% duplicated]


all_recoded <- all_extracted %>% 
# [!names(all_extracted) %in% c("MWI2006BBS_FSW", "GIN2022BBS_FSW")]  %>% #[!names(all_extracted) %in% c("BEN2008ACA_FSW", "COD2019BBS_MSM", "MWI2006BBS_FSW", "GHA2015BBS_FSW")] 
  Map(new_recode_survey_variables,
      df = .,
      survey_id = names(.),
      list(value_recode))

debugonce(new_recode_survey_variables)
new_recode_survey_variables(all_recoded$BEN2008ACA_FSW, "BEN2008ACA_FSW", value_recode)
## Fix please :)
all_recoded$GIN2022BBS_FSW$age_fs_paid[all_recoded$GIN2022BBS_FSW$age_fs_paid == "Pas de réponse"] <- NA
all_recoded$ZWE2022ACA_PWID$age_inject[all_recoded$ZWE2022ACA_PWID$age_inject == "don't know"] <- NA

# To clean up our data! 
newlyclean <- all_recoded %>% 
  Map(cleaning_fun2,
      df = .,
      survey_id_c = names(.))

newlyclean$BDI2021BBS_FSW$pregnant[newlyclean$BDI2021BBS_FSW$pregnant == "Pas de réponse"] <- NA
newlyclean$BDI2021BBS_FSW$anc_attended[newlyclean$BDI2021BBS_FSW$anc_attended == "Pas de réponse"] <- NA
newlyclean$BDI2021BBS_FSW$marital[newlyclean$BDI2021BBS_FSW$marital == "Séparée/Divorcé"] <- 3
newlyclean$MOZ2021BBS_FSW$paid_sex_6mnth[newlyclean$MOZ2021BBS_FSW$paid_sex_6mnth == "Nao"] <- 0
newlyclean$MOZ2021BBS_FSW$paid_sex_6mnth[newlyclean$MOZ2021BBS_FSW$paid_sex_6mnth == "n/a"] <- NA
newlyclean$TZA2022BBS_FSW$vl_result_suppressed[newlyclean$TZA2022BBS_FSW$vl_result_suppressed == "unsuppressed"] <- 0
newlyclean$ZWE2022ACA_PWID$onart[newlyclean$ZWE2022ACA_PWID$onart == "I’m on and off ARV treatment"] <- 0

newlyclean <- lapply(newlyclean, function(df) {
  df %>%
    mutate(across(starts_with("coupon"), ~as.integer(extract_numeric(.))),
           across(starts_with("own"), ~as.integer(extract_numeric(.))),
           across(starts_with("age_fs"), ~as.integer(extract_numeric(.))),
           across(starts_with("sex"), ~as.integer(extract_numeric(.))),
           across(starts_with("hiv"), ~as.integer(extract_numeric(.))),
           across(starts_with("age"), ~as.integer(extract_numeric(.))),
           across(starts_with("age"), ~as.integer(extract_numeric(.))),
           across(starts_with("inject"), ~as.integer(extract_numeric(.))),
           across(starts_with("primary"), ~as.integer(extract_numeric(.))),
           across(starts_with("cdm"), ~as.integer(extract_numeric(.))),
           across(starts_with("religion"), ~as.integer(extract_numeric(.))),
           across(starts_with("marital"), ~as.integer(extract_numeric(.))),
           across(starts_with("anc"), ~as.integer(extract_numeric(.))),
           across(starts_with("education"), ~as.integer(extract_numeric(.))),
           across(starts_with("paid"), ~as.integer(extract_numeric(.))),
           across(starts_with("pregnant"), ~as.integer(extract_numeric(.))),
           across(starts_with("on"), ~as.integer(extract_numeric(.))),
           across(starts_with("vl"), ~as.integer(extract_numeric(.))))
})
  # %>%
    # select(own_coupon = if ("own_coupon" %in% names(.))
    #   as.integer(extract_numeric(own_coupon)),


# all_recoded$BDI2021BBS_FSW$education

newlyclean <- newlyclean %>% 
  bind_rows()



# all_extracted <- all_extracted[!names(all_extracted) %in% c("BEN2012ACA_FSW", "CIV2020BBS_MSM", "GHA2011BBS_FSW", "GHA2015BBS_FSW", "GHA2019BBS_FSW", "UGA2021BBS_MSM", "ZAF2014BBS_FSW" , "ZAF2017BBS_MSM")]
library(RDS)
all_recoded2 <- all_recoded[!names(all_recoded) %in% c("UGA2021BBS_MSM", "ZAF2017BBS_MSM", "UGA2012BBS_FSW", "UGA2012BBS_MSM", "ZAF2018BBS_FSW", "MOZ2011BBS_PWID", "MOZ2014BBS_PWID", "COG2017BBS_MSM", "COG2017BBS_PWID", "UGA2021BBS_FSW", "BDI2021BBS_FSW", "COD2022PSE_FSW", "COD2022PSE_PWID", "TZA2022BBS_PWID")] # UGA2021 - rec.id loop; ZAF2017 - net.size, UGA2012BBS_FSW + UGA2012BBS_MSM + MOZ2014BBS_PWID + TZA2022BBS_PWID  - Error in 1:(wave.one.start - 1) : NA/NaN argument , ZAF2018BBS_FSW - invalid network_size, COG2017BBS_MSM/PWID - no seeds in data; UGA2021BBS_FSW - Error in if (is.data.frame(res) && (id %in% names(res)) && (rid %in% names(res))) { : missing value where TRUE/FALSE needed ; COD2022PSE_FSW + COD2022PSE_PWID - Error in assert.valid.rds.data.frame(x) : Subjects can not recruit themselves.

all_recoded_spare <- all_recoded2[!names(all_recoded2) %in% c("NAM2019BBS_MSM")] 
all_recoded_spare1 <- all_recoded2[!names(all_recoded2) %in% c("TZA2022BBS_PWID", "ZMB2017BBS_FSW")]
# ZMB2017BBS_FSW Error in matrix(freq$interval, nrow = cat) : 'data' must be of a vector type, was 'NULL' In addition: Warning message:  In RDS.bootstrap.intervals.local(rds.data, outcome.variable, weight.type,  : duration1 has only one level. Skipping...


# Make coupons numeric
# extract_numeric <- function(column) {
  # str_extract(column, "\\d+")

# 
all_recoded_basic <- lapply(all_recoded, function(df) {
  df %>%
    mutate(across(starts_with("coupon"), ~as.integer(extract_numeric(.))),
           across(starts_with("age_fs"), ~as.integer(extract_numeric(.))),
           across(starts_with("sex"), ~as.integer(extract_numeric(.))),
           across(starts_with("hiv"), ~as.integer(extract_numeric(.))),
           across(starts_with("age"), ~as.integer(extract_numeric(.)))) %>%
    mutate(own_coupon = if ("own_coupon" %in% names(.))
      as.integer(extract_numeric(own_coupon)),
      ) 
})

unweighted_dat <- all_recoded_basic %>% 
    bind_rows() %>% 
  moz.utils::separate_survey_id() %>% 
  group_by(survey_id) %>% 
  mutate(sample_size_surv = n()) %>% 
  ungroup()


write_csv(unweighted_dat, "C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/unweighted_dat_0210.csv")



system.time(rds_dur1 <- all_recoded2 %>% 
  Map(rds_adjust, #duration_yr
      df = .,
      survey_id = names(.),
      list(variable_recode)))

rds_dur2 <- all_recoded2 %>% 
  Map(rds_adjust_dur4, #age - age_startsw
      df = .,
      survey_id = names(.),
      list(variable_recode))

rds_dur3 <- all_recoded2 %>% 
  Map(rds_adjust_dur3, #age - age_fs_paidorgift
      df = .,
      survey_id = names(.),
      list(variable_recode))

rds_dur4 <- all_recoded_spare %>% 
  Map(rds_adjust_dur2, #age - age_fs_paid
      df = .,
      survey_id = names(.),
      list(variable_recode))

rds_dur5 <- all_recoded2 %>% 
  Map(rds_adjust_durmsm, #age - age_fs_man
      df = .,
      survey_id = names(.),
      list(variable_recode))

rds_dur6 <- all_recoded2 %>% 
  Map(rds_adjust_durmsm2, #age - age_fs_man_anal
      df = .,
      survey_id = names(.),
      list(variable_recode))

rds_dur7 <- all_recoded_spare1 %>% 
  Map(rds_adjust_pwid, #age - age_inject
      df = .,
      survey_id = names(.),
      list(variable_recode))


rds_dur9 <- all_recoded_spare1 %>% 
  Map(rds_adjust_dur1, #inject_dur ##this didn't work
      df = .,
      survey_id = names(.),
      list(variable_recode))

rds_age <- all_recoded2 %>% 
  Map(rds_adjust_age, #age
      df = .,
      survey_id = names(.),
      list(variable_recode))

rds_hivbyage <- all_recoded2 %>% 
              Map(rds_adjust2, #duration_yr
                  df = .,
                  survey_id = names(.),
                  list(variable_recode))

debugonce(rds_adjust2)
trial <- rds_adjust2(all_recoded2$BDI2021BBS_MSM, "BDI2021BBS_MSM", variable_recode)
# rds_partnerage<- all_recoded2 %>% 
  # Map(rds_adjust_partnerage,
  #     df = .,
  #     survey_id = names(.),
  #     list(variable_recode))


debugonce(rds_adjust_partnerage)
trial <- rds_adjust_partnerage(all_recoded2$NAM2019BBS_MSM, "NAM2019BBS_MSM", variable_recode)

trial <- rds_adjust_age(all_recoded$ZMB2017BBS_FSW, "ZMB2017BBS_FSW", variable_recode)

single_year_to_five_year <- function (df, fifteen_to_49 = TRUE) 
{
  df <- df %>% dplyr::mutate(age_group_label = cut(age, c(0, 
                                                          seq(5, 85, 5) - 1), c(paste0(seq(0, 79, 5), "-", seq(5, 
                                                                                                               80, 5) - 1), "80+"), include.lowest = TRUE)) %>% dplyr::left_join(naomi::get_age_groups() %>% 
                                                                                                                                                                                   select(age_group, age_group_label)) %>% dplyr::select(-age_group_label)
  if (fifteen_to_49) {
    df %>% dplyr::filter(age %in% 15:49) %>% dplyr::select(-age)
  }
  else {
    df %>% dplyr::select(-age)
  }
}



data <- all_recoded$ZMB2017BBS_FSW %>% 
  single_year_to_five_year() %>% 
  mutate(new_age = case_when(age_group %in% c("Y015_019", "Y020_024") ~ "18-24",
                             age_group %in% c("Y025_029") ~ "25-29",
                             age_group %in% c("Y030_034") ~ "30-34",
                             age_group %in% c("Y035_039", "Y040_044", "Y045_049") ~ "35+"),
         city_age = paste0(survey_city, " ", new_age)) %>% 
  select(survey_city, age_group, new_age, city_age, everything())

debugonce(rds_adjust_cityage)
trial <- rds_adjust_cityage(data, "ZMB2017BBS_FSW", variable_recode)


rds_adjust_cityage <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "city_age" %in% colnames(df) ) {
      
      df <- df %>% 
        # mutate(duration = inject_dur) %>% 
        # filter(!age < 0,
        #        !age > 80) %>% 
        mutate(age1 = factor(city_age)) %>% 
        filter(!is.na(coupon2),
               coupon2 != "")
      
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



system.time(wow <- rds_adjust(all_recoded2[[15]], "CIV2020BBS_FSW", variable_recode))

# 
# #
# ## Testing for one survey
# 
# wow <-  new_extract_fun(all_extracted[[3]], "BEN2012ACA_FSW", variable_recode)
# 
# 
# debugonce(rds_adjust)
# red <- new_recode_survey_variables(wow, "BEN2012ACA_FSW", value_recode)
# debugonce(new_val_recode)
# df %>%
#   mutate(onart = new_val_recode(onart, "onart", survey_id_c))
# 
# 


##############################
#### Making lists data frames 


  objects <- ls(envir = .GlobalEnv)

# to_match <- c("^rds_dur", "^rds_age")
# rds_sets <- grep("^rds_dur", objects, value = T)
rds_sets <- c("rds_dur1","rds_dur2","rds_dur3","rds_dur4","rds_dur5","rds_dur6","rds_dur7","rds_dur9", "rds_age")

  rds_dfs <- list()



for(i in rds_sets){
    df <- get(i, envir = .GlobalEnv)

    df <- bind_rows(df)
    
    rds_dfs[[i]] <- df
}
  
rds_df <- bind_rows(rds_dfs)


rds_df <- bind_rows(rds_dur1) %>% 
  mutate(survey_id2 = survey_id) 

#all_recoded <- all_recoded[names(all_recoded) != "UGA2021BBS_FSW"]
nonrds_list <- all_recoded[!names(all_recoded) %in% rds_df$survey_id]
  
non_rdsdat <- nonrds_list %>% lapply(function(x) {
  # if(is.null(unique(x$age)))
  #   x
  # else
  #    # x %>% mutate(age = as.numeric(age))
    x %>% mutate_if(is.character, as.numeric)
  }) %>% lapply(select, any_of(c("age", "inject_yr", "age_fs_paid", "age_fs_paidfor", "age_fs_paidorgift", "age_inject", "age_startsw", "age_startsw_cat", "duration_yr", "inject_dur" , "age_inject", "age_fs_paidorgift", "age_fs_man_anal", "age_fs_man", "hiv"))) %>% 
  bind_rows(.id = "survey_id") %>% 
  mutate(age = ifelse(age > 87 | age < 10, NA_integer_, age),
         age_fs_paid = ifelse(age_fs_paid < 10, NA_integer_, age - age_fs_paid), 
         age_fs_paidorgift = ifelse(age_fs_paidorgift < 10, NA_integer_, age - age_fs_paidorgift),
         age_fs_man = ifelse(age_fs_man >= age, NA_integer_, age - age_fs_man),
         age_fs_man_anal = ifelse(age_fs_man_anal >= age, NA_integer_, age - age_fs_man_anal),
         age_startsw = ifelse(age_startsw < 10, NA_integer_, age - age_startsw),
         duration_yr = ifelse(age - duration_yr <10, NA_integer_, duration_yr),
         inject_dur = ifelse(age - inject_dur < 10, NA_integer_, inject_dur),
         age_inject = ifelse(age_inject < 10, NA_integer_, age - age_inject)) %>% 
  pivot_longer(cols = age:inject_dur, names_to = "vars" ) %>% 
  group_by(survey_id, vars, value) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  distinct() %>% 
  filter(!is.na(value)) %>% 
  group_by(survey_id, vars) %>% 
  mutate(estimate = n/sum(n)) %>% 
  ungroup() %>% 
  rename(category = value) %>% 
  bind_rows(rds_df %>% mutate(category = as.numeric(category)))

# non_rdsdat <- non_rdsdat %>%
#   filter(!is.na(age)) %>%
#   group_by(survey_id) %>%
#   count(age) %>%
#   mutate(estimate = n/sum(n))

non_rdsdat <- non_rdsdat %>% 
  separate_survey_id() %>% 
  mutate(vars = case_when(var == "age1" ~ "age",
                          var == "duration1" ~ "duration_yr",
                          var == "duration2" ~ "age_fs_paid",
                          var == "duration3" ~ "age_fs_paidorgift", 
                          var == "duration4" ~ "age_startsw",
                          var == "durationmsm" ~ "age_fs_man",
                          var == "durationmsm2" ~ "age_fs_man_anal",
                          var == "durationpwid" ~ "age_inject",
                          T ~ vars
                          ))


saveRDS(non_rdsdat, "C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/00Admin/Data extracts/fullweightedandunweighteddata_0210.rds")


#### FIN 
  
age_dat <- rds_df %>%
  rename(age = category) %>%
  type.convert(as.is = TRUE) %>%
  bind_rows(non_rdsdat) %>%
  separate(
    col = survey_id,
    into = c("survey_id", "kp"),
    sep = "_",
    convert = TRUE
  )

#write_csv(age_dat, "C:/Users/rla121/Downloads/age_dat.csv")

surveys <- age_dat %>% 
  mutate(survey_name = paste(survey_id, kp)) %>% 
  mutate(iso3 = substr(survey_id2, 1,3)) %>% 
  select(survey_name, iso3, kp) %>% 
  distinct()
write_csv(surveys, "C:/Users/rla121/Downloads/surveysforage_dat.csv")

(age_plot <- age_dat %>% 
    filter(!age>49) %>% 
  group_by(kp) %>% 
  ggplot() +
  geom_line(aes(x = age, y = estimate), se = FALSE, show.legend = TRUE) +
    geom_ribbon(aes(x = age, ymin = lower, ymax = upper, fill = kp), alpha = 0.4, show.legend = FALSE) +
    moz.utils::standard_theme() +
  xlab("age") +
  ylab("proportion") +
    ggtitle("RDS Studies") + 
    facet_wrap(~kp, ncol =5))

(age_plot <- age_dat %>%
    ungroup() %>% 
    filter(!kp %in% c("CFSW", "PWUD"),
           !age>89) %>% 
    mutate(estimate2 = estimate + 0.00000001) %>% 
    ggplot() +
    geom_density(aes(x = age, y = estimate2, color = survey_id), show.legend = TRUE) +
    geom_point(aes(x = age, y = estimate)) +
    # geom_ribbon(aes(x = age, ymin = lower, ymax = upper, fill = kp), alpha = 0.4, show.legend = FALSE) +
    moz.utils::standard_theme() +
    lims(x = c(10,50), y = c(0,0.25)) +
    xlab("age") +
    ylab("proportion") +
    ggtitle("Age distributions") + 
    facet_wrap(~kp, ncol =5))
age_dat %>% 
  filter(survey_id == "BDI2019ACA") %>% 
  ggplot() +
  geom_density(aes(x = age, y = estimate), show.legend = TRUE)
# spec_paths <- c(list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2022 final shared/Non-Generalized", pattern = "Niger_2022_v2_22032022.pjnz", ignore.case = TRUE, full.names = TRUE),
#                 list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2021 final shared/ESA", pattern = "Moz", ignore.case = TRUE, full.names = TRUE),
#   list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2022 final shared/EPP-Gen", pattern = "PJNZ", ignore.case = TRUE, full.names = TRUE),
#   list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2022 final shared/WCA/Nigeria 2022  03 09", pattern = "PJNZ", ignore.case = TRUE, full.names = TRUE),
#   list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2022 final shared/Non-Generalized", pattern = "senegal_2022_04_22_corrigee.pjnz" , ignore.case = TRUE, full.names = TRUE)
# )
# 
# spectrum_data <- lapply(spec_paths, naomi::extract_pjnz_naomi)

#save(spectrum_data, file = "C:/Users/rla121/Dropbox/KP/Useful/spec_data.RData")

load("C:/Users/rla121/Dropbox/KP/Useful/spec_data.RData")

# Year matched genpop

genpop <- bind_rows(spectrum_data) %>% 
  group_by(iso3, sex, year, age) %>% 
  summarise(totpop = sum(totpop)) %>% 
  ungroup() %>% 
  group_by(iso3, sex, year) %>% 
  mutate(genpop_estimate = totpop/sum(totpop))
  # mutate(year = as.numeric(year)) %>% 
  # ungroup() %>% 
  # select(iso3, year, sex, age, genpop_estimate, national_totpop, totpop) %>% 
  # distinct()

age_dat2 <- age_dat %>%
  mutate(sex = ifelse(kp == "FSW", "female", "male")) %>% 
  mutate(survey_id3 = survey_id) %>% 
  separate(survey_id3, into = c("iso3", "year"), remove = TRUE,  sep = c(3, 7) ) %>% 
  mutate(year = as.numeric(year)) %>% 
  #select(-study_type) %>% 
  left_join(genpop) %>% 
  mutate(ageratio = estimate/genpop_estimate)


write_csv(age_dat2, "C:/Users/rla121/Downloads/age_dat.csv")
# 
# 
# (age_rat_plot <- age_dat2 %>% 
#     filter(sex == "female") %>% 
#     mutate(decade = case_when(
#       year %in% c(2000:2009) ~ "2000",
#       year %in% c(2010:2019) ~ "2010",
#       year %in% c(2020:2029) ~ "2020",
#     )) %>%
#     ggplot() +
#     geom_smooth(aes(x = age, y = ageratio, color = survey_id),se = FALSE, show.legend = TRUE) +
#     facet_wrap(~decade) +
#     lims(x = c(0,90)))
# (age_rat_plot <- age_dat2 %>% 
#     filter(sex == "male") %>% 
#     ggplot() +
#     geom_smooth(aes(x = age, y = ageratio, color = iso3),se = FALSE, show.legend = TRUE) +
#     lims(x = c(0,90)))
# 
# (KEN_plot <- age_dat2 %>% 
#     filter(sex == "female" & iso3 == "KEN") %>% 
#     ggplot() +
#     geom_smooth(aes(x = age, y = estimate, color = survey_id),se = FALSE, show.legend = TRUE) +
#     geom_smooth(aes(x = age, y = genpop_estimate, color = survey_id),se = FALSE, show.legend = TRUE) +
#     lims(x = c(0,90)))
# 
# genpop %>% 
#   filter(sex == "female" & iso3 %in% c("KEN", "UGA", "TZA", "SSD") & year == 2021) %>% 
#   ggplot() +
#   geom_smooth(aes(x = age, y = genpop_estimate, color = iso3),se = FALSE, show.legend = TRUE) +
#   lims(x = c(0,90))
# age_dat2 %>% 
#   filter(sex == "female" & iso3 %in% c("KEN", "UGA", "TZA", "SSD")) %>% 
#   ggplot() +
#   geom_smooth(aes(x = age, y = estimate, color = survey_id),se = FALSE, show.legend = TRUE) +
#   lims(x = c(0,90))

# 
# inla_df <- age_dat %>%
#   mutate(id.iso3 = factor(group_indices(., iso3)),
#          id.ref = factor(group_indices(., survey_id2))
#   ) 

# formula <- ageratio ~ bs(age,df  = 5) + f(id.ref)
# formula2 <- log_ageratio ~ f(age_jitter, model = "rw2") + f(id.ref, model = "iid") + f(id.iso3, model = "iid")
# formula3 <- log_ageratio ~ (id.agegroup, model = "rw2") #f(id.agegroup = "rw2") #presume this error is because I'm trying to rw2 a factor?
# 
# female_df %>%
#   filter(is.na(ageratio))
# 
# # df <- inla_df %>%
#   # mutate(age2 = age) %>%
#   # moz.utils::single_year_to_five_year()
# 
# write_csv(female_df, "C:/Users/rla121/Downloads/dat.csv")
# 
# female_df <- age_dat %>% 
#   filter(sex == "female",
#          !is.na(ageratio)
#          # age %in% 15:49
#          ) %>%
#   # group_by(age) %>%
#   mutate(id.age = factor(group_indices(., age)),
#          log_ageratio = log(ageratio))
#   # group_by(survey_id3) %>%
#   # mutate(study_n = sum(n))
# 
# pred_df <- data.frame(id.age = factor(1:35),
#                       age = 15:49,
#                       n = 1) %>%
#   bind_rows(female_df %>%
#               mutate(id.ref = factor(group_indices(., survey_id2)))
#             )%>%
#   select(id.age, age, ageratio, n, survey_id2, id.ref)
# 
# 
# mod <- lme4::lmer(ageratio ~ bs(age, df = 5) + (1|survey_id2), data = female_df, weight = n)
# 
# data.frame(age = 15:49,
#            ageratio = predict(mod,
#                           data.frame(age = 15:49), 
#                           re.form = NA,
#                           # type = "link"
#                           )
# ) %>%
#   ggplot(aes(x=age, y=ageratio)) +
#   geom_line() +
#   geom_point(data = female_df)
# 
# 
# inla_mod <- INLA::inla(formula,
#                        data = pred_df,
#                        family = "xpoisson",
#                        #weights = pys,
#                        #Ntrials = df2$pys,
#                        # E = pred_df$n,
#                        # offset = log(n),
#                        E = n,
#                        #E=E,
#                        control.compute = list(config = TRUE,
#                                               dic = TRUE),
#                        control.predictor=list(compute=TRUE,
#                                               link = 1)
# )
# 
# 
# 
# m = get("inla.models", inla.get.inlaEnv())
# m$latent$rw2$min.diff = NULL
# assign("inla.models", m, inla.get.inlaEnv())
# 
# summary(inla_mod)
# 
# filt_df <- pred_df %>%
#   filter(!is.na(ageratio))
# 
# samples <- inla.posterior.sample(1000, inla_mod)
# 
# contents = inla_mod$misc$configs$contents
# effect = "Predictor"
# id.effect = which(contents$tag==effect)
# ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])
# 
# ind.effect <- 1:(nrow(pred_df) - nrow(filt_df))
# 
# samples.effect = lapply(samples, function(x) x$latent[ind.effect])
# 
# incidence_samples <- matrix(sapply(samples.effect, cbind), ncol=1000)
# 
# 
# ident <- pred_df[ind.effect, ]
# 
# qtls <- apply(incidence_samples, 1, quantile, c(0.025, 0.5, 0.975))
# 
# dat <- ident %>%
#   ungroup() %>%
#   mutate(
#     lower = qtls[1,],
#     median = qtls[2,],
#     upper = qtls[3,]
#   )
# 
# dat
# 
# dat %>% type.convert(as.is = TRUE) %>% 
#   ggplot(aes(x=id.age, y=exp(median))) + geom_line() +
#   geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), alpha = 0.4)
# 
# data <- df2 %>%
#   filter(!is.na(log_irr)) 
# 
# (IRR <- dat %>%
#     ggplot(aes(x=year, y=exp(median))) +
#     geom_line() +
#     geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), alpha = 0.4) +
#     geom_point(data = data, aes(y=IRR, x=year, size=pys)) +
#     lims(y = c(0,25)) +
#     moz.utils::standard_theme() +
#     ggtitle("Natural scale") +
#     ylab("IRR"))
# (log_irr <- dat %>% 
#     ggplot(aes(x=year, y=median)) +
#     geom_line() +
#     geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4) +
#     geom_point(data=data, aes(y=log_irr, x=year, size = pys)) +
#     moz.utils::standard_theme() +
#     ggtitle("Log scale") +
#     ylab("Log IRR"))
# ggpubr::ggarrange(log_irr, IRR)
# 
# 

# non_rds_df <- bind_rows(all_recoded) %>% 
#   filter(!age > 95) %>% 
#   group_by(survey_id) 
#   #mutate(frequency = count(age))
# 
# 
# non_rds_frequency <- as.data.frame(count(non_rds_df, age)) %>% 
#   group_by(survey_id) %>% 
#   mutate(proportion = n/sum(n)) %>% 
#   mutate(survey_id2 = survey_id) %>% 
#   separate(
#     col = survey_id2,
#     into = c("survey", "kp"),
#     sep = "_",
#     convert = TRUE
#   )
#   
# 
# (non_rds_density<- non_rds_frequency %>% 
#   filter(!kp == "CFSW",
#          !kp == "TGW",
#          !age>49) %>% 
#   ggplot(aes(x = age, y = proportion)) +
#     geom_smooth(aes(color = survey_id), se = FALSE, show.legend = FALSE) +
#     moz.utils::standard_theme() +
# facet_wrap(~kp, ncol = 5) +
#     xlab("age") +
#     ylab("proportion") +
#     ggtitle("PLACE & ACA studies"))
# 
# 
# ggpubr::ggarrange(non_rds_density, age_plot, nrow = 2)

# 
# (non_rds_msm <- non_rds_frequency %>% 
#     filter(kp == "MSM") %>% 
#     ggplot() +
#     geom_density(aes(x = age, y = proportion, color = survey_id)) +
#     moz.utils::standard_theme())
# (non_rds_msm <- non_rds_frequency %>% 
#     filter(kp == "PWID") %>% 
#     ggplot() +
#     geom_point(aes(x = age, y = proportion, color = survey_id)) +
#     moz.utils::standard_theme())



#####
# (non_rds_frequency2 <- non_rds_frequency %>% 
# plyr::mutate(age_group_label = cut(age, c(0, seq(5, 85, 5)-1), c(paste0(seq(0, 79, 5), "-", seq(5, 80, 5)-1), "80+"), include.lowest=TRUE)) %>%
#   dplyr::left_join(naomi::get_age_groups() %>% select(age_group, age_group_label)) %>%
#   dplyr::select(-age_group_label) %>% 
#   group_by(kp, survey_id, age_group) %>% 
#   ggplot() +
#   geom_density(aes(x = age_group, y = proportion, color = survey_id), stat = "identity") +
#   facet_wrap(~kp) +
#   moz.utils::standard_theme())
# 
# 
#  (rds_df2 <- rds_df %>% 
#     mutate(age = as.numeric(category)) %>% 
#   plyr::mutate(age_group_label = cut(age, c(0, seq(5, 85, 5)-1), c(paste0(seq(0, 79, 5), "-", seq(5, 80, 5)-1), "80+"), include.lowest=TRUE)) %>%
#   dplyr::left_join(naomi::get_age_groups() %>% select(age_group, age_group_label)) %>%
#   dplyr::select(-age_group_label) %>% 
#   group_by(age_group) %>% 
#   ggplot(aes(x = age_group, y = estimate, color = survey_id)) +
#   geom_point() +
#   # facet_wrap(non_rds_frequency$survey_id) +
#   moz.utils::standard_theme())


### non-rds HIV pos

# hiv_non_rds <- non_rds_df %>% 
#   filter(hiv == 1) 
#   
# hiv_non_rds <-  as.data.frame(count(hiv_non_rds, age)) %>% 
#   group_by(survey_id) %>% 
#   mutate(proportion = n/sum(n)) %>% 
#   mutate(survey_id2 = survey_id) %>% 
#   separate(
#     col = survey_id2,
#     into = c("survey", "kp"),
#     sep = "_",
#     convert = TRUE )
# 
# 
# (hiv_non_rds_age_plots <- hiv_non_rds %>% 
#    # filter(kp == "FSW") %>% 
#     ggplot() +
#     geom_density(aes(x = age, y = proportion, color = survey_id), stat = "identity") +
#     moz.utils::standard_theme())
# 
# (hiv_non_rds_frequency2 <- hiv_non_rds %>% 
#     plyr::mutate(age_group_label = cut(age, c(0, seq(5, 85, 5)-1), c(paste0(seq(0, 79, 5), "-", seq(5, 80, 5)-1), "80+"), include.lowest=TRUE)) %>%
#     dplyr::left_join(naomi::get_age_groups() %>% select(age_group, age_group_label)) %>%
#     dplyr::select(-age_group_label) %>% 
#     group_by(age_group) %>% 
#     ggplot(aes(x = age_group, y = proportion, color = survey_id)) +
#     geom_density( stat = "identity") +
#     facet_wrap(~kp) +
#     moz.utils::standard_theme())
# 
# ggpubr::ggarrange(non_rds_frequency2, hiv_non_rds_frequency2, nrow = 2)
# 
# 
# female_df <- read_csv("~/Downloads/dat.csv")


##Spatial

ssa_names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- countrycode::countrycode(ssa_names, "country.name", "iso3c")

# grey <- read_sf("~/Downloads/Longitude_Graticules_and_World_Countries_Boundaries-shp/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp") %>%
#   filter(CNTRY_NAME %in% c("Western Sahara", "Mauritania", "Morocco", "Algeria", "Libya", "Tunisia", "Egypt", "Somalia", "Djibouti", "Eritrea")) %>%
#   bind_rows(read_sf("~/Downloads/ssd_adm_imwg_nbs_20220121/ssd_admbnda_adm0_imwg_nbs_20210924.shp"))

ssd_boundary <- read_sf("C:/Users/rla121/Dropbox/KP/Useful/Geographies/ssd_adm_imwg_nbs_20220121/ssd_admbnda_adm0_imwg_nbs_20210924.shp")
ssd_boundary_simple <- rmapshaper::ms_simplify(ssd_boundary, 0.05)

geographies <- read_sf("C:/Users/rla121/Dropbox/KP/Useful/Geographies/Longitude_Graticules_and_World_Countries_Boundaries-shp/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp") %>%
  bind_rows(
    ssd_boundary_simple %>%
      transmute(CNTRY_NAME = "South Sudan")
  ) %>%
  mutate(iso3 = countrycode(CNTRY_NAME, "country.name", "iso3c"),
         area_name = countrycode(iso3, "iso3c", "country.name")) %>%
  filter(iso3 %in% ssa_iso3)

geographies <- geographies %>% 
  filter(iso3 %in% c("BDI", "BEN" ,"BFA" ,"CIV", "CMR" ,"COD", "COG", "GMB" ,"KEN" ,"LSO" ,"MLI" ,"MOZ", "MWI", "NGA", "SLE", "SWZ" ,"TCD", "TGO", "ZWE" ,"AGO" ,"ETH", "GAB" ,"GHA" ,"GIN", "LBR", "NAM" ,"NER", "RWA", "SEN" ,"TZA", "UGA" ,"ZMB", "ZAF", "SSD"))
geographies <- geographies %>%
  arrange(iso3) %>%
  mutate(id.iso3 = as.numeric(factor(iso3))) %>%
  select(iso3, area_name, id.iso3) %>%
  st_make_valid() 

## JE: trim Prince Edward Islands from map

bbox <- c(xmin = -17.5327797,
          ymin = -35, ## -46.9697266,
          xmax = 51.4113159179688, 
          ymax = 37.3404121398926)

geographies <- st_crop(geographies, bbox)

temp_geog2 <- poly2nb(geographies)

nb2INLA("geog.adj", temp_geog2)
geog.adj <- paste(getwd(), "/geog.graph2", sep="")

GG <- inla.read.graph(filename = "geog.graph2")
image(inla.graph2matrix(GG), xlab = "", ylab="")



### Prepping data

female_df <- age_dat2 %>% 
  filter(sex == "female",
         !is.na(ageratio)
  )



pred2 <- crossing(iso3 = ssa_iso3,
         age = 15:49) %>%
  # mutate(age_group = naomi::cut_naomi_age_group(age)) %>%
  # distinct(iso3, age_group) %>%
  bind_rows(female_df %>%
              # filter(age %in% 15:49) %>%
              mutate(id.ref = multi.utils::to_int(survey_id),
                     id.ref2 = id.ref,
                     age_group = naomi::cut_naomi_age_group(age)) %>%
              # group_by(iso3, survey_id, id.ref, id.ref2, age_group) %>%
              # summarise(n = sum(n)) %>%
              group_by(survey_id) %>%
              mutate(n_eff_kish = sum(n)) %>%
              select(x_eff = n, n_eff_kish, any_of(c("age_group", "age")), survey_id, iso3, id.ref, id.ref2, genpop_estimate, ageratio, totpop)
            ) %>%
  left_join(geographies %>% st_drop_geometry() %>% select(iso3, id.iso3)) %>%
  mutate(id.obs = multi.utils::to_int(row_number()),
         id.age = multi.utils::to_int(age),
         # id.age = multi.utils::to_int(age_group),
         id.age2 = id.age,
         estimate = x_eff/n_eff_kish) %>%
  type.convert(as.is = TRUE) %>%
  filter(!iso3 == "HTI",
         !iso3 == "ERI") %>%
  ungroup()
  

write_csv(pred2, "C:/Users/rla121/Downloads/pred2.csv")

# formula <- n ~ totpop + f(age, model = "ar1") + f(id.ref) + f(id.iso3, model="besag", graph=geog.adj, scale.model = TRUE)
# formula <- n ~ genpop_estimate + f(age, model = "ar1") + f(id.ref) + f(id.iso3, model="besag", graph=geog.adj, scale.model = TRUE)
# formula <- n ~ genpop_estimate + f(age, model = "ar1") + f(id.ref)
# formula2 <- n ~ f(id.idx, model = "iid", hyper = multi.utils::tau_fixed(0.000001)) + f(id.age, model = "iid", constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u =2.5, alpha = 0.01)) + f(age, model = "ar1")

ar1_group_prior <- list(
  rho = list(rho = "pc.cor1", param = c(0, 0.75)),
  prec = list(prec = "pc.prec", param = c(2.5, 0.01), initial = log(0.001))
)

formula <- x_eff ~ -1 + f(id.ref, model = "iid" , hyper = multi.utils::tau_fixed(1E-6)) +
  bs(id.age, df = 5)  +
  # f(id.age, model = "ar1") +
  f(id.iso3, model="besag", graph="geog.adj", scale.model = TRUE, group = id.age, control.group = list(model = "rw2"), constr = TRUE
    # hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)
    )
 # f(id.ref2, model="iid", group = id.age, control.group = list(model = "iid"),
 #    constr = TRUE
 #   # hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)
 #   )
formula <- ageratio ~ -1 + f(id.ref, model = "iid" , hyper = multi.utils::tau_fixed(1E-6)) +
  bs(id.age, df = 5)  +
  # f(id.age, model = "ar1") +
  f(id.iso3, model="besag", graph="geog.adj", scale.model = TRUE, group = id.age, control.group = list(model = "rw2"), constr = TRUE
    # hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)
  )
# df <- pred2 %>%
#   filter(!is.na(x_eff)) 

#debugonce(multinomial_model)
# int <- multinomial_model(formula, "test", 1000)







fit <- inla(formula, 
            data = pred2, 
            family = "xPoisson",
            # offset = x_eff, - added this in 
            control.predictor = list(link = 1),
            control.compute = list(dic = TRUE, waic = TRUE,
                                   cpo = TRUE, config = TRUE),
            inla.mode = "experimental")

df <- pred2 %>%
  filter(across(all_of("x_eff"), ~!is.na(.x)))

samples <- inla.posterior.sample(1000, fit)
ind.effect <- 1:(nrow(pred2) - nrow(df))
samples.effect = lapply(samples, function(x) x$latent[ind.effect])
prev_samples <- matrix(sapply(samples.effect, cbind), ncol=1000)

ident <- pred2[ind.effect, ]

qtls <- apply(prev_samples, 1, quantile, c(0.025, 0.5, 0.975))

prev <- ident %>%
  ungroup() %>%
  mutate(
    lower = qtls[1,],
    median = qtls[2,],
    upper = qtls[3,]
  )

# int[["df"]] %>% View
# 
# int$df %>% 
#   group_by(survey_id) %>%
#   mutate(lambda_mean = lambda_mean/sum(lambda_mean)) %>%
#   ggplot(aes(x=age, group = survey_id)) + 
#   geom_point(aes(y=(estimate))) + 
#   geom_line(aes(y=(lambda_mean))) + 
#   # geom_ribbon(aes(ymin = (prob_lower), ymax = (prob_upper)), alpha = 0.4) + 
#   facet_wrap(~survey_id) +
#   standard_theme()

prev %>%
  group_by(iso3) %>%
  mutate(rate = exp(median)/sum(exp(median))) %>%
  ggplot(aes(x=age, group = survey_id)) +
  geom_line(data = pred2, aes(y=estimate, color=survey_id), show.legend = FALSE) +
  geom_line(aes(y=rate)) +
  # geom_ribbon(aes(ymin = (prob_lower), ymax = (prob_upper)), alpha = 0.4) +
  facet_wrap(~iso3) +
  moz.utils::standard_theme()




# Poisson model that technically works... uncertainty isn't a thing though 


formulas <- list(
 # x_eff ~ totpop + f(age, model = "ar1") + f(id.ref) + f(id.iso3, model="besag", graph=geog.adj, scale.model = TRUE),
 x_eff ~ totpop + bs(id.age, df = 5) + f(id.ref),
 x_eff ~ totpop + f(age, model = "ar", order = 2) + f(id.ref),
 x_eff ~ totpop + f(age, model = "rw2") + f(id.ref)
  #x_eff ~ totpop + bs(age, df = 5) + f(id.ref) + f(id.iso3, model="besag", graph=geog.adj, scale.model = TRUE)
  # n ~ -1 + f(id.obs, model = "iid", hyper = multi.utils::tau_fixed(0.000001)) +
  #   f(id.age, model = "iid", constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01)) +
  #   f(id.iso3, model = "besag", graph = geog.adj, scale.model = TRUE, group = id.age, control.group = list(model = "iid"),
  #     constr = TRUE, hyper = multi.utils::tau_pc(x = 0.001, u = 2.5, alpha = 0.01))
  )

# mod <- lme4::lmer(ageratio ~ bs(age, df = 5) + (1|survey_id2), data = female_df, weight = n)
# 
# data.frame(age = 15:49,
#            ageratio = predict(mod,
#                               data.frame(age = 15:49), 
#                               re.form = NA,
#                               # type = "link"
#            )
# ) %>%
#   ggplot(aes(x=age, y=ageratio)) +
#   geom_line() +
#   geom_point(data = female_df)

for (i in seq_along(formulas)) {
  
  formula <- formulas[[i]] 
  
  inla_mod <- INLA::inla(formula,
                       data = pred2,
                       family = "poisson",
                       #weights = pys,
                       #Ntrials = df2$pys,
                       # E = pred_df$n,
                       # offset = log(n),
                       E = n_eff_kish,
                       #E=n,
                       control.compute = list(config = TRUE
                       #                        dic = TRUE
                       ),
                       control.predictor=list(compute=TRUE,
                                              link = 1),
                       verbose = TRUE
                       )
  

summary(inla_mod)


filt_df <- pred2 %>%
  filter(!is.na(x_eff))

samples <- inla.posterior.sample(1000, inla_mod)

contents = inla_mod$misc$configs$contents
effect = "Predictor"
id.effect = which(contents$tag==effect)
ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])

ind.effect <- 1:(nrow(pred2) - nrow(filt_df))

samples.effect = lapply(samples, function(x) x$latent[ind.effect])

incidence_samples <- matrix(sapply(samples.effect, cbind), ncol=1000)

ident <- pred2[ind.effect, ]

qtls <- apply(incidence_samples, 1, quantile, c(0.025, 0.5, 0.975))

dat <- ident %>%
  ungroup() %>%
  mutate(
    lower = qtls[1,],
    median = qtls[2,],
    upper = qtls[3,]
  )
dat

# dat <- dat %>%
#   mutate(age_ratio = exp(median)/genpop_estimate)

# (plot_model <- dat %>% 
#   # filter(year == 2015) %>% 
#    #type.convert(as.is = TRUE) %>% 
#   ggplot(aes(x=age, y=age_ratio)) + geom_line())
#   #geom_ribbon(aes(ymin = (exp(lower)/genpop_estimate), ymax = (exp(upper)/genpop_estimate)), alpha = 0.4) +
#   geom_point(data = female_df, aes(x = age, y = age_ratio, color = iso3)) +
#   lims(y = c(0,25)) +
#     facet_wrap(~iso3)))

(plot_model <- dat %>% ggplot(aes(x=age)) + geom_line(aes(y=exp(median))) + geom_ribbon(aes(ymin = (exp(lower)), ymax = (exp(upper))), alpha = 0.4) + geom_point(data = female_df, aes(x = age, y = estimate))+ facet_wrap(~iso3))

plot_name <- paste0("model_plot", i)
assign(plot_name, plot_model)

}

ggpubr::ggarrange(model_plot1, model_plot2, model_plot3, nrow = 3)



