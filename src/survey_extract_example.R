library(RDS)
library(tidyverse)
library(rdhs)
library(INLA)
library(sf)
library(spdep)
library(countrycode)
library(stringdist)
library(splines)
library(multi.utils)

# source("C:/Users/rla121/OneDrive - Imperial College London/Documents/GitHub/survey-extraction/src/kp_recoding_functions_21_02.R")
# setwd("C:/Users/rla121/OneDrive - Imperial College London/Documents/GitHub/survey-extraction")
source("src/kp_recoding_functions_21_02.R")
ssa_iso3 <- c("BDI", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")


### READ IN THE age_dat.csv (in survey-extraction/data) here and feel free to then hop on down to line 527 #### 
age_dat2 <- read_csv("data/age_dat.csv")
age_dat2 <- read_csv("~/Downloads/age_dat.csv")





### Recoding vars / values
      ## Analysis and file_type redundant for the time being
# recoding_sheet <-  read_csv("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/Individual KP/00Admin/recoding_sheet.csv")
recoding_sheet <-  read_csv("data/recoding_sheet.csv")

variable_recode <- recoding_sheet %>% 
  select(survey_id, variable, var_raw, study_type) %>% 
  rename(var_label_raw = var_raw) %>% 
  mutate(survey_id2 = survey_id) %>% 
  separate(survey_id2, c(NA, "file_type")) %>% 
  distinct() %>%
  mutate(analysis = "kp") %>% 
  filter(!variable == "cdm_location",
         !variable == "subject_id",
         !variable == c("residence", "hivtest_whenlast", "when_positive")) %>% ### removing residence is to get round the character/integer binding row problem later. A bridge to cross when we start looking at spatial issues
  rename(var_raw = var_label_raw) 

value_recode <- recoding_sheet %>% 
  rename(value = val_recode) %>% 
  filter(!is.na(val_raw))

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
paths <- list.files("C:/Users/rla121/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Individual level data/", recursive = TRUE, pattern = ".rds", full.names = TRUE) %>%
  lapply(., grep, pattern= "PWID|PLACE|CFSW|TG|NPP|TGW|TGM|MSW|code|Anne|v1|v2|cpt_fin|dbn_fin|jhb_fin|old", value = TRUE, invert = TRUE) %>%
  unlist()
  

combined_datasets <- lapply(paths, readRDS)

surv_ids <- str_split(paths, "/") %>%
  lapply(tail, 1) %>%
  unlist()

surv_ids[surv_ids == "uga2021_fsw_bbs.rdsobj"] <- "UGA2021BBS_FSW.rds"
surv_ids <- str_remove(surv_ids, ".rds")

names(combined_datasets) <- surv_ids

# combined_datasets <- combined_datasets[!names(combined_datasets) %in% c("NAM2019BBS_FSW", "NAM2019BBS_MSM", "SWZ2020BBS_FSW", 
#                                                                         "CIV2020BBS_FSW", "MOZ2012BBS_FSW", "ZAF2013BBS_MSM", #This line is extract error
#                                                                         )]

#### This is doing weird things.... the recoded datasets are in there amongst the chaos, I think. 
all_extracted <- combined_datasets %>%
  Map(new_extract_fun,
      df = .,
      survey_id = names(.),
      list(variable_recode)
      )

all_extracted <- compact(all_extracted)

#all_extracted[["BEN2005BBS_FSW"]]

#all_extracted <- all_extracted[!names(all_extracted) %in% c( "CIV2020BBS_MSM", "GHA2011BBS_FSW", "GHA2015BBS_FSW", "GHA2019BBS_FSW", "UGA2021BBS_MSM", "ZAF2014BBS_FSW" , "ZAF2017BBS_MSM", "ZAF2013BBS_MSM")] #"BEN2012ACA_FSW",
#Two copies of UGA 2021 BBS FSW??

all_recoded <- all_extracted %>%
  Map(new_recode_survey_variables,
      df = .,
      survey_id = names(.),
      list(value_recode))

# all_extracted <- all_extracted[!names(all_extracted) %in% c("BEN2012ACA_FSW", "CIV2020BBS_MSM", "GHA2011BBS_FSW", "GHA2015BBS_FSW", "GHA2019BBS_FSW", "UGA2021BBS_MSM", "ZAF2014BBS_FSW" , "ZAF2017BBS_MSM")]
library(RDS)
all_recoded2 <- all_recoded[!names(all_recoded) %in% c("UGA2021BBS_MSM", "ZAF2017BBS_MSM")] # UGA2021 - rec.id loop; ZAF2017 - net.size

all_rds <- all_recoded2 %>% 
  Map(rds_adjust,
      df = .,
      survey_id = names(.),
      list(variable_recode))



# debugonce(rds_adjust)
# rds_adjust(all_recoded$UGA2021BBS_MSM, "UGA2021BBS_MSM", variable_recode)
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

rds_df <- bind_rows(all_rds) %>% 
  mutate(survey_id2 = survey_id) 

#all_recoded <- all_recoded[names(all_recoded) != "UGA2021BBS_FSW"]
nonrds_list <- all_recoded[!names(all_recoded) %in% rds_df$survey_id2]
  
non_rdsdat <- nonrds_list %>% lapply(function(x) {
  if(is.null(unique(x$age)))
    x
  else
     x %>% mutate(age = as.numeric(age))
  }) %>% lapply(select, any_of(c("age", "subject_id"))) %>% bind_rows(.id = "survey_id")

non_rdsdat <- non_rdsdat %>%
  filter(!is.na(age)) %>%
  group_by(survey_id) %>%
  count(age) %>%
  mutate(estimate = n/sum(n))
  
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

#write_csv(age_dat, "C:/Users/rla121/Dropbox/KP/Individual KP/age_dat.csv")



(age_plot <- age_dat %>% 
    filter(!age>49) %>% 
  group_by(survey_id) %>% 
  ggplot() +
  geom_line(aes(x = age, y = estimate, color = survey_id), se = FALSE, show.legend = TRUE) +
   # geom_ribbon(aes(x = age, ymin = lower, ymax = upper, fill = survey_id), alpha = 0.4, show.legend = FALSE) +
    moz.utils::standard_theme() +
  xlab("age") +
  ylab("proportion") +
    ggtitle("RDS Studies") + 
    facet_wrap(~kp, ncol =5))


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


write_csv(age_dat2, "C:/Users/rla121/Dropbox/KP/Individual KP/age_dat.csv")
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

ssd_boundary <- read_sf("~/Downloads/ssd_adm_imwg_nbs_20220121/ssd_admbnda_adm0_imwg_nbs_20210924.shp")
ssd_boundary_simple <- rmapshaper::ms_simplify(ssd_boundary, 0.05)

geographies <- read_sf("~/Downloads/Longitude_Graticules_and_World_Countries_Boundaries-shp/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp") %>%
  bind_rows(
    ssd_boundary_simple %>%
      transmute(CNTRY_NAME = "South Sudan")
  ) %>%
  mutate(iso3 = countrycode(CNTRY_NAME, "country.name", "iso3c"),
         area_name = countrycode(iso3, "iso3c", "country.name")) %>%
  filter(iso3 %in% ssa_iso3)

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

# temp_geog2 <- poly2nb(geographies)
# 
# nb2INLA("geog.adj", temp_geog2)
# geog.adj <- paste(getwd(), "/geog.graph2", sep="")
# 
# GG <- inla.read.graph(filename = "geog.graph2")
# image(inla.graph2matrix(GG), xlab = "", ylab="")



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

# df <- pred2 %>%
#   filter(!is.na(x_eff)) 

#debugonce(multinomial_model)
# int <- multinomial_model(formula, "test", 1000)

fit <- inla(formula, data = pred2, family = 'xPoisson',
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
  standard_theme()




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



