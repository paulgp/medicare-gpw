
library(tidyverse)
library(haven)
library(usmap)
final_county_level <- read_dta("~/Dropbox/GPW/data/final_county_level.dta")
state_fips <- read_dta("~/Dropbox/GPW/data/State_FIPS_Codes.dta")
sahie <- read_dta("~/Dropbox/GPW/data/SAHIE/pctui_county_year2.dta") %>%
  mutate(fips = statefips *1000 + countyfips) %>%
  filter(year == 2006) %>%
  select(fips, pctui)

  
final_county_level <- final_county_level %>% select(-starts_with("total")) %>% 
  mutate(age = age - 1) %>%
  filter(age >= 50 & age <= 80)

final_county_level <- final_county_level %>% 
  left_join(state_fips, by=c("state" = "State"))
final_county_level <- final_county_level %>% 
  mutate(fips = as.integer(State_FIPS)*1000 + census_code)
  
final_county_level <- final_county_level %>% 
  left_join(sahie) 

final_county_level <- final_county_level %>% 
  left_join(insurance_data2) %>% mutate(acs_pctui = case_when(age <= 64 ~ `Age_55 to 64 years`,
                                                              age > 64 ~ `Age_65 to 74 years`)) %>%
  select(-`Age_55 to 64 years`, -`Age_65 to 74 years`)

age_level <- final_county_level %>% 
  group_by(age) %>% 
  summarise_at(vars(starts_with("balance"), q_coll_12, q_coll_12_gt0, n_coll_12_gt0,
                    n_coll_12, avg_riskscore, new_bpt, new_fc),
                    funs(weighted.mean(., population, na.rm=TRUE))) 

age_level_pctui <- final_county_level %>% 
  group_by(age) %>% 
  mutate(pop2 = sum(population)) %>%
  summarise_at(vars(acs_pctui),
               funs(weighted.mean(., pop2, na.rm=TRUE)))  

age_sahie_level <- final_county_level %>% 
  mutate(pctui_bin = ntile(pctui, 4)) %>%
  group_by(age, pctui_bin) %>% 
  summarise_at(vars(starts_with("balance"),  q_coll_12, q_coll_12_gt0, n_coll_12_gt0,
                    n_coll_12, avg_riskscore, new_bpt, new_fc),
               funs(weighted.mean(., population, na.rm=TRUE)))  %>%
  filter(is.na(pctui_bin) == FALSE)

age_sahie_level_pctui <- final_county_level %>% 
  mutate(pctui_bin = ntile(pctui, 4)) %>%
  group_by(age, pctui_bin) %>% 
  mutate(pop2 = sum(population)) %>%
  summarise_at(vars(acs_pctui),
               funs(weighted.mean(., pop2, na.rm=TRUE)))  %>%
  filter(is.na(pctui_bin) == FALSE) 


age_level <- age_level %>% left_join(age_level_pctui)
age_sahie_level <- age_sahie_level %>% left_join(age_sahie_level_pctui)

age_sahie_level2 <- age_sahie_level %>% 
  bind_rows(age_level %>% mutate(pctui_bin = 0)) 

## Four main variables to construct graphs for: 
# First stage: PCTUI
# Debt In Collections (q_coll_12)
# Total debt in delinquency
# Bankruptcy
# Mortality

ggplot(data = age_sahie_level2) + 
  geom_point(aes(x = age, y = acs_pctui, color=as.factor(pctui_bin))) +
  geom_vline(xintercept = 65) +
  scale_color_viridis_d(name = "By level of \ncounty uninsurance", 
                        labels = c("Overall", "1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile"))+ 
  labs(x = "Age", y= "", 
       title = "Uninsurance Rate by age group") +
  theme_minimal() +
  theme(text = element_text(size=18)) 

ggplot(data = age_sahie_level2) + 
  geom_point(aes(x = age, y = q_coll_12, color=as.factor(pctui_bin))) +
  geom_vline(xintercept = 65) +
  scale_color_viridis_d(name = "By level of \ncounty uninsurance", 
                        labels = c("Overall", "1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile"))+ 
  labs(x = "Age", y= "", 
       title = "Collections") +
  theme_minimal() +
  theme(text = element_text(size=18)) 

ggplot(data = age_sahie_level) + 
  geom_point(aes(x = age, y = new_bpt, color=as.factor(pctui_bin))) +
  geom_point(data = age_level, aes(x = age, y = new_bpt)) +
  scale_fill_viridis_d() + 
  geom_vline(xintercept = 65) +
  labs(x = "Age", y= "", 
       title = "Bankruptcy") +
  theme_minimal() +
  theme(text = element_text(size=18)) 

ggplot(data = age_level) + geom_point(aes(x = age, y = balance_cc))
