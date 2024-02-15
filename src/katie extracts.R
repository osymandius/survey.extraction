# all_recoded <- bind_rows(all_recoded)

# Run survey extract example up to line 205. 

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
all_recoded_basic <- bind_rows(all_recoded_basic) %>% 
  select(-subject_id)

agecounts <- all_recoded_basic %>% 
  select(survey_id, age) %>% 
  group_by(survey_id, age) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  group_by(survey_id) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  distinct() %>% 
  group_by(survey_id) %>% 
  mutate(prop = n/count) %>% 
  ungroup() %>% 
  select(-count)

hivbyage <- all_recoded_basic %>% 
  filter(!is.na(hiv)) %>% 
  group_by(survey_id, age) %>% 
  mutate(age_denominator = n()) %>%
  ungroup() %>% 
  moz.utils::separate_survey_id() %>% 
  select(-year) %>% 
  group_by(survey_id, age, kp, age_denominator) %>% 
  summarise(prev = mean(hiv))  %>% 
  ungroup() %>% 
  filter(!survey_id == "KEN2021ACA_FSW")

saveRDS(agecounts, "C:/Users/rla121/Downloads/agecount.rds")
saveRDS(hivbyage, "C:/Users/rla121/Downloads/hivbyage.rds")

katie_age <- readRDS("C:/Users/rla121/Downloads/agecount.rds") %>% 
  moz.utils::separate_survey_id() %>% 
  group_by(survey_id) %>% 
  mutate(total_sample = sum(n)) %>% 
  select(-iso3, -year)

saveRDS(new_katie_age, "C:/Users/rla121/Downloads/agecount.rds")

new_katie_age <- readRDS("C:/Users/rla121/Downloads/hivbyage.rds") %>% 
  filter(!survey_id == "KEN2021ACA_FSW")

hivbyage %>% 
ggplot() +
  geom_line(aes(x = age, y = prev, color = survey_id), show.legend = F) + 
  facet_wrap(~survey_id)

all_recoded_basic %>% 
  filter(!is.na(hiv)) %>% 
  group_by(survey_id, age) %>% 
  mutate(age_denominator = n()) %>%
  ungroup() %>% 
  moz.utils::separate_survey_id() %>% 
  select(-year) %>% 
  group_by(survey_id, age, kp, age_denominator) %>% 
  summarise(prev = mean(hiv))  %>% 
  ungroup()