
load_data_ccp = function(ccp_varlist) {
  ccp_data <- fread("/Users/psg24/Dropbox/GPW/data/final_county_level.csv")
  ccp_data$year = floor(ccp_data$quarter / 100)
  ccp_data <- ccp_data %>% 
    rename(pop = population) %>% 
    filter(state != "GU" & state != "MH" & state != "FM" & state != "MP" & state != "PW" & state != "PR" & state != "VI" & state != "AS" & state != "OA") %>%
    filter(year >=2008)
  ccp_data_dist = fread("/Users/psg24/Dropbox/GPW/data/final_county_level_dist.csv") 
  ccp_data_dist$year = floor(ccp_data_dist$quarter / 100)
  ccp_data_dist = ccp_data_dist %>%
    rename(pop = population)  %>%
    filter(state != "GU" & state != "MH" & state != "FM" & state != "MP" & state != "PW" & state != "PR" & state != "VI" & state != "AS" & state != "OA") %>%
    filter(year >=2008) 
  
  State_FIPS_Codes <- read_dta("/Users/psg24/Dropbox/GPW/data/State_FIPS_Codes.dta") %>% 
    rename(state = State) %>%  zap_labels()
  cz_county_xwalk <- read_dta("/Users/psg24/Dropbox/GPW/data/cw_cty_czone.dta")  %>%
    rename(county_fips = cty_fips) %>%  zap_labels()
  
  ccp_data_year <- ccp_data %>% 
    select(state,census_code, year, age, pop, all_of(ccp_varlist)) %>%
    mutate(age = age - 1,
           new_fc = new_fc * 100,
           new_bpt = new_bpt * 100) %>%
    filter(age > 54 & age < 76 & age != 65) %>% 
    gather(variable, outcome, -year, -age, -pop, -year, -census_code, -state) %>%
    left_join(State_FIPS_Codes)
  ccp_data_year_dist = ccp_data_dist %>% 
    select(state,census_code, year, age, pop, ends_with("collect")) %>%
    select(state,census_code, year, age, pop, starts_with("share")) %>%
    mutate(age = age - 1) %>%
    filter(age > 54 & age < 76 & age != 65) %>% 
    gather(variable, outcome, -year, -age, -pop, -year, -census_code, -state) %>%
    left_join(State_FIPS_Codes)

  ccp_data_year = ccp_data_year %>% 
    filter(variable %in% c("balance_cc" , "balance_mort", "balance_ttl")) %>% 
    rename(denom = outcome) %>%
    mutate(variable = paste0(variable, "_delinq")) %>%
    group_by(State_FIPS, age, variable, year) %>% 
    summarize(denom = weighted.mean(denom, pop)) %>%
    ungroup() %>%
    left_join(ccp_data_year) %>%
    mutate(outcome = outcome / denom) %>%
    mutate(variable = paste0(variable, "_scale")) %>%
    bind_rows(ccp_data_year) %>%
    bind_rows(ccp_data_year_dist)

  ccp_data_collapsed_county <- ccp_data_year %>% group_by(state,census_code, age, variable) %>%
    summarize(outcome = weighted.mean(outcome, pop),
              pop = sum(pop)) %>% 
    left_join(State_FIPS_Codes) %>%
    mutate(county_fips = paste0(State_FIPS, str_pad(census_code, 3, "left",  pad = "0")))
  
  ccp_data_collapsed_county_yearly <- ccp_data_year %>%
    left_join(State_FIPS_Codes) %>%
    mutate(county_fips = paste0(State_FIPS, str_pad(census_code, 3, "left",  pad = "0"))) 
  
  ccp_data_collapsed <- ccp_data_year %>% 
    group_by(age, variable) %>%
    summarize(outcome = weighted.mean(outcome, pop)) %>%
    filter(age > 54 & age < 76 & age != 65)
  
  ccp_data_collapsed_yearly <- ccp_data_year %>% 
    group_by(age, variable, year) %>%
    summarize(outcome = weighted.mean(outcome, pop)) 
  
  rm(ccp_data)
  rm(ccp_data_year)
  
  
  return(list(ccp_data_collapsed_county = ccp_data_collapsed_county,
              ccp_data_collapsed_county_yearly = ccp_data_collapsed_county_yearly,
              ccp_data_collapsed_yearly = ccp_data_collapsed_yearly,
              ccp_data_collapsed = ccp_data_collapsed))
}
load_data_acs = function(acs_varlist) {
  acs_state_data <- read_csv("/Users/psg24/Dropbox/GPW/data/state_acs_characteristics.csv")
  acs_state_data <- acs_state_data %>% 
    mutate(black_quartile   = case_when(BlackShareNearElderly <= wtd.quantile(BlackShareNearElderly, q = 0.25, weight = NearElderlyPopulation) ~ 1,
                                        BlackShareNearElderly <= wtd.quantile(BlackShareNearElderly, q = 0.50, weight = NearElderlyPopulation) ~ 2,
                                        BlackShareNearElderly <= wtd.quantile(BlackShareNearElderly, q = 0.75, weight = NearElderlyPopulation) ~ 3,
                                        TRUE ~ 4),
           poverty_quartile = case_when(PovShareNearElderly <= wtd.quantile(PovShareNearElderly, q = 0.25, weight = NearElderlyPopulation) ~ 1,
                                        PovShareNearElderly <= wtd.quantile(PovShareNearElderly, q = 0.50, weight = NearElderlyPopulation) ~ 2,
                                        PovShareNearElderly <= wtd.quantile(PovShareNearElderly, q = 0.75, weight = NearElderlyPopulation) ~ 3,
                                        TRUE ~ 4),
           ui_quartile      = case_when(UninsuranceNearElderly <= wtd.quantile(UninsuranceNearElderly, q = 0.25, weight = NearElderlyPopulation) ~ 1,
                                        UninsuranceNearElderly <= wtd.quantile(UninsuranceNearElderly, q = 0.50, weight = NearElderlyPopulation) ~ 2,
                                        UninsuranceNearElderly <= wtd.quantile(UninsuranceNearElderly, q = 0.75, weight = NearElderlyPopulation) ~ 3,
                                        TRUE ~ 4),
           ui_median      = case_when(UninsuranceNearElderly <= wtd.quantile(UninsuranceNearElderly, q = 0.5, weight = NearElderlyPopulation) ~ 1,
                                      TRUE ~ 2)) %>%
    rename(State_FIPS = state_fips)
  insurance_state_year_data = read_csv("/Users/psg24/Dropbox/GPW/derived/import_state_age_year_health_insurance_2008_to_2017/output/ACS_2008_to_2017_state_by_age_collapsed.csv")
  first_stage_data = insurance_state_year_data %>% 
    gather(variable, outcome, -statefip, -year, -age, -population) %>%
    filter(age > 54 & age < 75 & age != 65) %>%
    rename(pop = population, State_FIPS = statefip) %>%
    filter(variable %in% c("has_medicare", "has_insurance", "has_multiple",
                           c("has_private_insurance", "has_employer_insurance",
                             "has_dir_purch_insurance", "has_medicaid",
                             "has_tricare", "has_va", "has_ihs"))) %>%
    mutate(State_FIPS = str_pad(as.character(State_FIPS), 2, "left",  pad = "0")) %>%
    left_join(acs_state_data)
  
  first_stage_data_ols = insurance_state_year_data %>% 
    gather(variable, outcome, -statefip, -year, -age, -population) %>%
    filter(age > 24 & age < 65 & age != 65) %>%
    rename(pop = population, State_FIPS = statefip) %>%
    filter(variable %in% c("has_medicare", "has_insurance", "has_multiple",
                           c("has_private_insurance", "has_employer_insurance",
                             "has_dir_purch_insurance", "has_medicaid",
                             "has_tricare", "has_va", "has_ihs"))) %>%
    mutate(State_FIPS = str_pad(as.character(State_FIPS), 2, "left",  pad = "0"))
  first_stage_data_ols2 = insurance_state_year_data %>% 
    gather(variable, outcome, -statefip, -year, -age, -population) %>%
    filter(age > 54 & age < 65 & age != 65) %>%
    rename(pop = population, State_FIPS = statefip) %>%
    filter(variable %in% c("has_medicare", "has_insurance", "has_multiple",
                           c("has_private_insurance", "has_employer_insurance",
                             "has_dir_purch_insurance", "has_medicaid",
                             "has_tricare", "has_va", "has_ihs"))) %>%
    mutate(State_FIPS = str_pad(as.character(State_FIPS), 2, "left",  pad = "0"))
  covariates <- read_csv("~/Dropbox/GPW/derived/import_state_age_year_covariates_2000_to_2017/output/ACS_covariates_2000_to_2017_state_by_age_collapsed.csv")
  
  covariates2 <- covariates %>% select(statefip, age, population, acs_varlist, year) %>%
    rename(pop = population) %>%
    filter(age > 54 & age < 76  & age != 65) %>% 
    mutate(medicare = as.numeric(age >= 65)) %>%
    filter(year >=2008) %>%
    gather(variable, outcome, -year, -age, -pop, -statefip) %>%
    mutate(State_FIPS = str_pad(as.character(statefip), 2, "left",  pad = "0")) %>%
    left_join(acs_state_data) %>% select(-statefip) %>% 
    rbind(first_stage_data)
  
  covariates_collapsed = covariates2 %>% 
    group_by(age, variable) %>%
    summarize(outcome = weighted.mean(outcome, w = pop),
              pop = sum(pop))
  
  covariates_collapsed_ui = covariates2 %>% 
    group_by(age, variable, ui_quartile) %>%
    summarize(outcome = weighted.mean(outcome, w = pop),
              pop = sum(pop))
  covariates_collapsed_pov = covariates2 %>% 
    group_by(age, variable, poverty_quartile) %>%
    summarize(outcome = weighted.mean(outcome, w = pop),
              pop = sum(pop))
  return(list(covariates2 = covariates2,
              covariates_collapsed = covariates_collapsed,
              covariates_collapsed_ui = covariates_collapsed_ui,
              covariates_collapsed_pov = covariates_collapsed_pov,
              first_stage_data = first_stage_data,
              first_stage_data_ols = first_stage_data_ols,
              first_stage_data_ols2 = first_stage_data_ols2))
}
load_data_acs_puma = function() {
  acs_puma_data2 = read_dta("raw/ACS/ACS 1YR PUMA INSURANCE/usa_00014.dta") %>%
    filter(age > 54 & age < 76)  %>%  zap_labels()
  acs_puma_data2 = acs_puma_data2 %>% 
    mutate(has_insurance = as.numeric(hcovany==2),
           has_private_insurance = as.numeric(hcovpriv==2),
           has_employer_insurance = as.numeric(hinsemp==2),
           has_purch_insurance = as.numeric(hinspur==2),
           has_tricare = as.numeric(hinstri==2),
           has_public_insurance= as.numeric(hcovpub==2),
           has_medicaid= as.numeric(hinscaid==2),
           has_medicare= as.numeric(hinscare==2)) %>% 
    select(age, statefip, puma, perwt, has_insurance, has_private_insurance,
           has_employer_insurance, has_purch_insurance, has_tricare, has_public_insurance,
           has_medicaid, has_medicare, year) %>% 
    mutate(statefip = f_pad_zero(as.character(statefip)),
           puma     = f_pad_zero(as.character(puma))) %>%
    filter(age != 65)
  
  acs_puma_data2 = acs_puma_data2 %>% mutate(puma = paste0(statefip, puma)) %>%
    group_by(puma, age, year) %>% 
    summarize(has_insurance = weighted.mean(has_insurance, perwt),
              has_private_insurance = weighted.mean(has_private_insurance, perwt),
              has_employer_insurance = weighted.mean(has_employer_insurance, perwt),
              has_purch_insurance = weighted.mean(has_purch_insurance, perwt),
              has_tricare = weighted.mean(has_tricare, perwt),
              has_public_insurance = weighted.mean(has_public_insurance, perwt),
              has_medicaid = weighted.mean(has_medicaid, perwt),
              has_medicare= weighted.mean(has_medicare, perwt),
              pop = sum(perwt)
    )
  acs_puma_data_collapsed = acs_puma_data2 %>%
    group_by(age) %>%
    summarize(has_insurance = weighted.mean(has_insurance, pop),
              has_private_insurance = weighted.mean(has_private_insurance, pop),
              has_employer_insurance = weighted.mean(has_employer_insurance, pop),
              has_purch_insurance = weighted.mean(has_purch_insurance, pop),
              has_tricare = weighted.mean(has_tricare, pop),
              has_public_insurance = weighted.mean(has_public_insurance, pop),
              has_medicaid = weighted.mean(has_medicaid, pop),
              has_medicare= weighted.mean(has_medicare, pop),
              pop = sum(pop)
    ) %>%
    ungroup()
    
  
  cov_varlist = c("own_dwelling", "fam_size_1", "fam_size_2", 
                  "fam_size_more", "married",  "educ_high_school", 
                  "educ_some_college", "educ_full_college", "educ_gtr_college", 
                  "employed", "female", "inctot", "uhrswork", "incss", 
                  "poverty", "in_poverty", "moved_in_last_year")
  acs_puma_data_cov = read_dta("raw/ACS/ACS 1 YR PUMA Covariates/usa_00015.dta") %>%
    zap_labels() %>%
    filter(age > 54 & age < 76 & age != 65) %>% filter(year > 2007 & year < 2018) %>%
    mutate(own_dwelling = as.numeric(ownershp == 1),
           fam_size_1   = as.numeric(famsize==1),
           fam_size_2   = as.numeric(famsize==2),
           fam_size_more= as.numeric(famsize!=2 & famsize != 1),
           married      = as.numeric(marst ==1 | marst == 2),
           female       = as.numeric(sex == 2),
           educ_high_school = as.numeric(educ == 6),
           educ_some_college= as.numeric(educ == 7 | educ == 8),
           educ_full_college= as.numeric(educ == 10),
           educ_gtr_college = as.numeric(educ == 11),
           employed         = as.numeric(empstat == 1),
           race_black       = as.numeric(race == 2),
           uhrswork         = as.numeric(uhrswork), 
           moved_in_last_year = as.numeric( migrate1 == 2 | migrate1 == 3 | migrate1 == 4),
           in_poverty       = as.numeric( poverty <= 100)) %>%
    select(year, statefip, puma, all_of(cov_varlist), perwt, age) 
  
  
  acs_puma_data_cov = acs_puma_data_cov %>% 
    group_by(age, statefip, puma, year) %>%
    summarize(own_dwelling       = weighted.mean(own_dwelling, perwt),
              fam_size_1         = weighted.mean(fam_size_1, perwt),
              fam_size_2         = weighted.mean(fam_size_2, perwt),
              fam_size_more      = weighted.mean(fam_size_more, perwt), 
              married            = weighted.mean(married, perwt),
              female             = weighted.mean(female,  perwt),
              educ_high_school   = weighted.mean(educ_high_school,  perwt),
              educ_some_college  = weighted.mean(educ_some_college,  perwt),
              educ_full_college  = weighted.mean(educ_full_college,  perwt),
              educ_gtr_college   = weighted.mean(educ_gtr_college,  perwt),
              employed           = weighted.mean(employed, perwt),
              inctot             = weighted.mean(inctot, perwt),
              uhrswork           = weighted.mean(uhrswork, perwt),
              incss              = weighted.mean(incss, perwt), 
              poverty            = weighted.mean(poverty, perwt),
              in_poverty         = weighted.mean(in_poverty, perwt),
              moved_in_last_year = weighted.mean(moved_in_last_year, perwt), 
              pop                = sum(perwt)
    ) %>% 
    ungroup() %>%
    mutate(statefip = f_pad_zero(as.character(statefip)),
           puma     = f_pad_zero(as.character(puma))) %>%
    mutate(puma = paste0(statefip, puma)) 
  
  acs_puma_data_cov_collapsed = acs_puma_data_cov %>% 
    group_by(age) %>%
    summarize(own_dwelling       = weighted.mean(own_dwelling, pop),
              fam_size_1         = weighted.mean(fam_size_1, pop),
              fam_size_2         = weighted.mean(fam_size_2, pop),
              fam_size_more      = weighted.mean(fam_size_more, pop), 
              married            = weighted.mean(married, pop),
              female             = weighted.mean(female,  pop),
              educ_high_school   = weighted.mean(educ_high_school,  pop),
              educ_some_college  = weighted.mean(educ_some_college,  pop),
              educ_full_college  = weighted.mean(educ_full_college,  pop),
              educ_gtr_college   = weighted.mean(educ_gtr_college,  pop),
              employed           = weighted.mean(employed, pop),
              inctot             = weighted.mean(inctot, pop),
              uhrswork           = weighted.mean(uhrswork, pop),
              incss              = weighted.mean(incss, pop), 
              poverty            = weighted.mean(poverty, pop),
              in_poverty         = weighted.mean(in_poverty, pop),
              moved_in_last_year = weighted.mean(moved_in_last_year, pop), 
              pop                = sum(pop)
    ) %>%
    ungroup()
  
  return(list(acs_puma_data_cov = acs_puma_data_cov,
              acs_puma_data = acs_puma_data2,
              acs_puma_data_collapsed = acs_puma_data_collapsed,
              acs_puma_data_cov_collapsed = acs_puma_data_cov_collapsed))
}

make_czone_data = function(ccp_data, acs_puma_data) {
  
  ccp_data_collapsed_county = ccp_data$ccp_data_collapsed_county
  ccp_data_collapsed_county_yearly = ccp_data$ccp_data_collapsed_county_yearly
  cz_county_xwalk <- read_dta("/Users/psg24/Dropbox/GPW/data/cw_cty_czone.dta")  %>%
    zap_labels() %>%
    rename(county_fips = cty_fips) %>%
    mutate(county_fips = f_pad_zero(as.character(county_fips)))
  
  cz_puma_xwalk <- read_dta("data/cw_puma2010_czone.dta") %>%
    rename(puma = puma2010) %>%  zap_labels()
  
  czone_data = ccp_data_collapsed_county %>% 
    mutate(county_fips = case_when(county_fips == "12086" ~"12025",
                                   TRUE ~ county_fips)) %>%
    left_join(cz_county_xwalk) %>%
    group_by(age, variable, czone) %>%
    summarize(outcome = weighted.mean(outcome, w=pop), pop =sum(pop)) %>%
    ungroup()
  
  czone_data_prepost = ccp_data_collapsed_county_yearly %>% 
    mutate(county_fips = case_when(county_fips == "12086" ~ "12025",
                                   TRUE ~ county_fips)) %>%
    left_join(cz_county_xwalk) %>%
    mutate(aca = year >= 2014) %>%
    group_by(age, variable, czone, aca) %>%
    summarize(outcome = weighted.mean(outcome, w=pop), pop =sum(pop)) %>%
    ungroup()
  
  czone_acs_data = acs_puma_data$acs_puma_data %>% ungroup() %>%
    mutate(puma = as.numeric(puma)) %>%
    left_join(cz_puma_xwalk) %>% mutate(pop = pop*afactor) %>%
    group_by(czone, age) %>%
    summarize(has_insurance = weighted.mean(has_insurance, pop),
              has_private_insurance = weighted.mean(has_private_insurance, pop),
              has_employer_insurance = weighted.mean(has_employer_insurance, pop),
              has_purch_insurance = weighted.mean(has_purch_insurance, pop),
              has_tricare = weighted.mean(has_tricare, pop),
              has_public_insurance = weighted.mean(has_public_insurance, pop),
              has_medicaid = weighted.mean(has_medicaid, pop),
              has_medicare= weighted.mean(has_medicare, pop),
              pop = sum(pop)
    ) %>%
    gather(variable, outcome, -czone, -pop, -age) %>%
    filter(!is.na(czone))
  
  czone_acs_pop_nearelderly = czone_acs_data %>% filter(age < 65) %>% 
    filter(variable == "has_insurance") %>% select(czone, pop) %>%
    summarize(pop_nearelderly = sum(pop))
  czone_acs_pop_nearelderly %>% write_csv("data/czone_acs_pop_nearelderly.csv")
  
  
  czone_acs_data_prepost = acs_puma_data$acs_puma_data %>% ungroup() %>%
    mutate(puma = as.numeric(puma)) %>%
    left_join(cz_puma_xwalk) %>% mutate(pop = pop*afactor,
                                        aca = year >= 2014) %>%
    group_by(czone, age, aca) %>%
    summarize(has_insurance = weighted.mean(has_insurance, pop),
              has_private_insurance = weighted.mean(has_private_insurance, pop),
              has_employer_insurance = weighted.mean(has_employer_insurance, pop),
              has_purch_insurance = weighted.mean(has_purch_insurance, pop),
              has_tricare = weighted.mean(has_tricare, pop),
              has_public_insurance = weighted.mean(has_public_insurance, pop),
              has_medicaid = weighted.mean(has_medicaid, pop),
              has_medicare= weighted.mean(has_medicare, pop),
              pop = sum(pop)
    ) %>%
    gather(variable, outcome, -czone, -pop, -age, -aca) %>%
    filter(!is.na(czone))
  
  czone_acs_data_cov = acs_puma_data$acs_puma_data_cov %>% ungroup() %>% 
    mutate(puma = as.numeric(puma)) %>%
    left_join(cz_puma_xwalk) %>% mutate(pop = pop*afactor) %>%
    group_by(czone, age) %>%
    summarize(own_dwelling       = weighted.mean(own_dwelling, pop),
              fam_size_1         = weighted.mean(fam_size_1, pop),
              fam_size_2         = weighted.mean(fam_size_2, pop),
              fam_size_more      = weighted.mean(fam_size_more, pop), 
              married            = weighted.mean(married, pop),
              female             = weighted.mean(female,  pop),
              educ_high_school   = weighted.mean(educ_high_school,  pop),
              educ_some_college  = weighted.mean(educ_some_college,  pop),
              educ_full_college  = weighted.mean(educ_full_college,  pop),
              educ_gtr_college   = weighted.mean(educ_gtr_college,  pop),
              employed           = weighted.mean(employed, pop),
              inctot             = weighted.mean(inctot, pop),
              uhrswork           = weighted.mean(uhrswork, pop),
              incss              = weighted.mean(incss, pop), 
              poverty            = weighted.mean(poverty, pop),
              in_poverty         = weighted.mean(in_poverty, pop),
              moved_in_last_year = weighted.mean(moved_in_last_year, pop), 
              pop                = sum(pop)
    )  %>%
    gather(variable, outcome, -czone, -pop, -age) %>%
    filter(!is.na(czone))
  
  czone_acs_data_cov_prepost = acs_puma_data$acs_puma_data_cov %>% ungroup() %>% 
    mutate(puma = as.numeric(puma),
           aca = year >= 2014) %>%
    left_join(cz_puma_xwalk) %>% mutate(pop = pop*afactor) %>%
    group_by(czone, age, aca) %>%
    summarize(own_dwelling       = weighted.mean(own_dwelling, pop),
              fam_size_1         = weighted.mean(fam_size_1, pop),
              fam_size_2         = weighted.mean(fam_size_2, pop),
              fam_size_more      = weighted.mean(fam_size_more, pop), 
              married            = weighted.mean(married, pop),
              female             = weighted.mean(female,  pop),
              educ_high_school   = weighted.mean(educ_high_school,  pop),
              educ_some_college  = weighted.mean(educ_some_college,  pop),
              educ_full_college  = weighted.mean(educ_full_college,  pop),
              educ_gtr_college   = weighted.mean(educ_gtr_college,  pop),
              employed           = weighted.mean(employed, pop),
              inctot             = weighted.mean(inctot, pop),
              uhrswork           = weighted.mean(uhrswork, pop),
              incss              = weighted.mean(incss, pop), 
              poverty            = weighted.mean(poverty, pop),
              in_poverty         = weighted.mean(in_poverty, pop),
              moved_in_last_year = weighted.mean(moved_in_last_year, pop), 
              pop                = sum(pop)
    )  %>%
    gather(variable, outcome, -czone, -pop, -age, -aca) %>%
    filter(!is.na(czone))
  
  
  return(list(czone_data = czone_data,
              czone_data_prepost = czone_data_prepost, 
              czone_acs_data = czone_acs_data,
              czone_acs_data_prepost = czone_acs_data_prepost,
              czone_acs_data_cov = czone_acs_data_cov,
              czone_acs_data_cov_prepost = czone_acs_data_cov_prepost))
}

make_state_data = function(ccp_data, acs_puma_data) {
  
  ccp_data_collapsed_county = ccp_data$ccp_data_collapsed_county
  ccp_data_collapsed_county_yearly = ccp_data$ccp_data_collapsed_county_yearly
  State_FIPS_Codes <- read_dta("/Users/psg24/Dropbox/GPW/data/State_FIPS_Codes.dta") %>% 
    rename(state = State) %>% zap_labels()

  state_data = ccp_data_collapsed_county %>% 
    mutate(county_fips = case_when(county_fips == "12086" ~"12025",
                                   TRUE ~ county_fips)) %>%
    group_by(age, variable, state) %>%
    summarize(outcome = weighted.mean(outcome, w=pop), pop =sum(pop)) %>%
    ungroup()
  
  state_data_prepost = ccp_data_collapsed_county_yearly %>% 
    mutate(county_fips = case_when(county_fips == "12086" ~ "12025",
                                   TRUE ~ county_fips)) %>%
    mutate(aca = year >= 2014) %>%
    group_by(age, variable, state, aca) %>%
    summarize(outcome = weighted.mean(outcome, w=pop), pop =sum(pop)) %>%
    ungroup()
  
  state_acs_data = acs_puma_data$acs_puma_data %>% ungroup() %>%
    mutate(state_code = substring(puma,1,2)) %>%
    group_by(state_code, age) %>%
    summarize(has_insurance = weighted.mean(has_insurance, pop),
              has_private_insurance = weighted.mean(has_private_insurance, pop),
              has_employer_insurance = weighted.mean(has_employer_insurance, pop),
              has_purch_insurance = weighted.mean(has_purch_insurance, pop),
              has_tricare = weighted.mean(has_tricare, pop),
              has_public_insurance = weighted.mean(has_public_insurance, pop),
              has_medicaid = weighted.mean(has_medicaid, pop),
              has_medicare= weighted.mean(has_medicare, pop),
              pop = sum(pop)
    ) %>%
    gather(variable, outcome, -state_code, -pop, -age) %>%
    filter(!is.na(state_code)) %>%
    rename(State_FIPS = state_code) %>%
    left_join(State_FIPS_Codes)
  
  state_acs_data_prepost = acs_puma_data$acs_puma_data %>% ungroup() %>%
    mutate(state_code = substring(puma,1,2),
           aca = year >= 2014) %>%
    group_by(state_code, age, aca) %>%
    summarize(has_insurance = weighted.mean(has_insurance, pop),
              has_private_insurance = weighted.mean(has_private_insurance, pop),
              has_employer_insurance = weighted.mean(has_employer_insurance, pop),
              has_purch_insurance = weighted.mean(has_purch_insurance, pop),
              has_tricare = weighted.mean(has_tricare, pop),
              has_public_insurance = weighted.mean(has_public_insurance, pop),
              has_medicaid = weighted.mean(has_medicaid, pop),
              has_medicare= weighted.mean(has_medicare, pop),
              pop = sum(pop)
    ) %>%
    gather(variable, outcome, -state_code, -pop, -age, -aca) %>%
    filter(!is.na(state_code)) %>%
    rename(State_FIPS = state_code) %>%
    left_join(State_FIPS_Codes)
  
  state_acs_data_cov = acs_puma_data$acs_puma_data_cov %>% ungroup() %>% 
    mutate(state_code = substring(puma,1,2)) %>%
    group_by(state_code, age) %>%
    summarize(own_dwelling       = weighted.mean(own_dwelling, pop),
              fam_size_1         = weighted.mean(fam_size_1, pop),
              fam_size_2         = weighted.mean(fam_size_2, pop),
              fam_size_more      = weighted.mean(fam_size_more, pop), 
              married            = weighted.mean(married, pop),
              female             = weighted.mean(female,  pop),
              educ_high_school   = weighted.mean(educ_high_school,  pop),
              educ_some_college  = weighted.mean(educ_some_college,  pop),
              educ_full_college  = weighted.mean(educ_full_college,  pop),
              educ_gtr_college   = weighted.mean(educ_gtr_college,  pop),
              employed           = weighted.mean(employed, pop),
              inctot             = weighted.mean(inctot, pop),
              uhrswork           = weighted.mean(uhrswork, pop),
              incss              = weighted.mean(incss, pop), 
              poverty            = weighted.mean(poverty, pop),
              in_poverty         = weighted.mean(in_poverty, pop),
              moved_in_last_year = weighted.mean(moved_in_last_year, pop), 
              pop                = sum(pop)
    )  %>%
    gather(variable, outcome, -state_code, -pop, -age) %>%
    filter(!is.na(state_code)) %>%
    rename(State_FIPS = state_code) %>%
    left_join(State_FIPS_Codes)
  
  state_acs_data_cov_prepost = acs_puma_data$acs_puma_data_cov %>% ungroup() %>% 
    mutate(state_code = substring(puma,1,2),
           aca = year >= 2014) %>%
    group_by(state_code, age, aca) %>%
    summarize(own_dwelling       = weighted.mean(own_dwelling, pop),
              fam_size_1         = weighted.mean(fam_size_1, pop),
              fam_size_2         = weighted.mean(fam_size_2, pop),
              fam_size_more      = weighted.mean(fam_size_more, pop), 
              married            = weighted.mean(married, pop),
              female             = weighted.mean(female,  pop),
              educ_high_school   = weighted.mean(educ_high_school,  pop),
              educ_some_college  = weighted.mean(educ_some_college,  pop),
              educ_full_college  = weighted.mean(educ_full_college,  pop),
              educ_gtr_college   = weighted.mean(educ_gtr_college,  pop),
              employed           = weighted.mean(employed, pop),
              inctot             = weighted.mean(inctot, pop),
              uhrswork           = weighted.mean(uhrswork, pop),
              incss              = weighted.mean(incss, pop), 
              poverty            = weighted.mean(poverty, pop),
              in_poverty         = weighted.mean(in_poverty, pop),
              moved_in_last_year = weighted.mean(moved_in_last_year, pop), 
              pop                = sum(pop)
    )  %>%
    gather(variable, outcome, -state_code, -pop, -age, -aca) %>%
    filter(!is.na(state_code)) %>%
    rename(State_FIPS = state_code) %>%
    left_join(State_FIPS_Codes)
  
  
  return(list(state_data = state_data,
              state_data_prepost = state_data_prepost, 
              state_acs_data = state_acs_data,
              state_acs_data_prepost = state_acs_data_prepost,
              state_acs_data_cov = state_acs_data_cov,
              state_acs_data_cov_prepost = state_acs_data_cov_prepost))
  
}



estimate_model <- function(reg_data, K_scale = 4, bw = 5) {
  modelfit <- lm(outcome ~ age + I(age^2),
                 data = reg_data %>%
                   filter(age < 65 & age > 55),
                 weights = pop)
  B_reg <- K_scale*abs(modelfit$coefficients[3])
  rdh.all <- RDHonest(outcome ~ age, 
                      data = reg_data, 
                      kern = "triangular",  
                      weight=pop,
                      opt.criterion = "MSE",
                      M = B_reg, 
                      h= bw,
                      cutoff = 65)
  lpp.all.lower <- LPPHonest(outcome ~ age,
                             data = reg_data %>% filter(age < 65),
                             weight = pop,
                             point = 65,
                             opt.criterion = "MSE",
                             M = B_reg, 
                             h= bw)
  lpp.all.upper <- LPPHonest(outcome ~ age,
                             data = reg_data %>% filter(age > 65),
                             weight = pop,
                             point = 65,
                             opt.criterion = "MSE",
                             M = B_reg, 
                             h= bw)
  return(list(tau        = rdh.all$estimate,
              se         = rdh.all$sd,
              model_left = lpp.all.lower$estimate[[1]], 
              model_right = lpp.all.upper$estimate[[1]],
              model_left_se = lpp.all.lower$sd[[1]],
              model_right_se = lpp.all.upper$sd[[1]],
              bias_left = lpp.all.lower$maxbias,
              bias_right = lpp.all.upper$maxbias,
              ci_lower = rdh.all$lower,
              ci_upper = rdh.all$upper,
              bw = rdh.all$hp))
}



estimate_model_fuzzy <- function(frd, K_scale = 4) {
  modelfit <- lm(endog ~ age + I(age^2),
                 data = frd %>%
                   filter(age < 65 & age > 55),
                 weights = pop)
  B_reg1 <- K_scale*abs(modelfit$coefficients[3])
  modelfit <- lm(outcome ~ age + I(age^2),
                 data = frd %>%
                   filter(age < 65 & age > 55),
                 weights = pop)
  B_reg2 <- K_scale*abs(modelfit$coefficients[3])
  r <- FRDHonest(endog ~ outcome | age, 
                 data = frd, 
                 kern = "triangular", 
                 weights = pop,
                 cutoff = 65,
                 h = 5,
                 M = c(B_reg1, B_reg2), 
                 opt.criterion = "MSE",
                 sclass = "H", 
                 T0 = 0)
  rdh.all <- FRDHonest(endog ~ outcome | age, 
                       data = frd, 
                       kern = "triangular", 
                       weights = pop,
                       cutoff = 65,
                       h = 5,
                       M = c(B_reg1, B_reg2), 
                       opt.criterion = "MSE",
                       sclass = "H", 
                       T0 = r$estimate)
  return(list(tau        = rdh.all$estimate,
              se         = rdh.all$sd,
              ci_lower = rdh.all$lower,
              ci_upper = rdh.all$upper,
              bw = rdh.all$hp))
}
weighted.var <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm =
                                       na.rm)
}

construct_variance_ratio = function(var_data) {

  variance_65minus = weighted.var(var_data$lower_estimate, var_data$pop)
  variance_65plus = weighted.var(var_data$upper_estimate, var_data$pop)
  L = sum(var_data$pop)
  w = var_data$pop / L
  
  phi_65minus = ((t(var_data$lower_estimate) %*% diag(w) %*% var_data$lower_estimate) - 
                   (t(var_data$lower_estimate) %*% diag(w) %*% rep(1,length(var_data$lower_estimate)))^2) 
  phi_65plus = ((t(var_data$upper_estimate) %*% diag(w) %*% var_data$upper_estimate) - 
                  (t(var_data$upper_estimate) %*% diag(w) %*% rep(1,length(var_data$upper_estimate)))^2) 
  phi = 1 - (phi_65plus / phi_65minus)
  
  phi_deriv_65minus = -2*(t(var_data$lower_estimate) %*% diag(w)  - 
                           (t(var_data$lower_estimate) %*% diag(w) %*% 
                              rep(1,length(var_data$lower_estimate))) %*%
                           t(rep(1,length(var_data$lower_estimate))) %*% diag(w))
  phi_deriv_65plus = -2*(t(var_data$upper_estimate)  %*% diag(w)  - 
                          (t(var_data$upper_estimate) %*% diag(w) %*% 
                             rep(1,length(var_data$upper_estimate))) %*%
                          t(rep(1,length(var_data$upper_estimate))) %*% diag(w))
  
  phi_deriv_f = phi_deriv_65plus / phi_65minus[1,1]
  phi_deriv_g = (phi_65plus[1,1] / (phi_65minus[1,1])^2) *phi_deriv_65plus
  phi_deriv = c(phi_deriv_f, phi_deriv_g)
  Sigma = diag(c(var_data$upper_estimate_se^2, var_data$lower_estimate_se^2))
  variance_phi = t(phi_deriv) %*% Sigma %*% phi_deriv
  phi_se = sqrt(variance_phi)
  bias_phi = sum(abs(t(phi_deriv) * 
                       c( var_data$upper_estimate_bias, var_data$lower_estimate_bias )))
  
  t = bias_phi/ phi_se
  
  if( t >= 2) {
    cv_95 = t + qnorm(0.975)
  }
  else {
    cv_95 = qfoldnorm(0.95, mean = t)
  }
  
  ci_lower = phi - cv_95 * phi_se
  ci_upper = phi + cv_95 * phi_se
  ci_lower_noadj = phi -  qnorm(0.975) * phi_se
  ci_upper_noadj = phi +  qnorm(0.975)  * phi_se
  
  return(list(phi = phi,
              phi_se = phi_se,
              bias_phi = bias_phi,
              ci_lower = ci_lower,
              ci_upper = ci_upper,
              ci_lower_noadj = ci_lower_noadj,
              ci_upper_noadj = ci_upper_noadj)) 
}


test_sig = function(top, bottom) {
  significance = (top < 0 & bottom < 0) | (top > 0 & bottom > 0)
  return(significance)
}

make_national_estimates = function(ntl_data) {
  ntl_estimates = ntl_data %>% ungroup() %>%
    nest(data = c(age, outcome, pop, czone) ) %>%
    mutate(estimate = future_map(data, estimate_model, .progress = TRUE))
  
  ntl_estimates =  ntl_estimates %>% 
    mutate(tau      = purrr::map(estimate, "tau"),
           se       = purrr::map(estimate, "se"),
           ci_lower = purrr::map(estimate, "ci_lower"),
           ci_upper = purrr::map(estimate, "ci_upper"),
           lower_estimate = purrr::map(estimate, "model_left"),
           upper_estimate = purrr::map(estimate, "model_right"),
           lower_estimate_se = purrr::map(estimate, "model_left_se"),
           upper_estimate_se = purrr::map(estimate, "model_right_se"),
           lower_estimate_bias = purrr::map(estimate, "bias_left"),
           upper_estimate_bias = purrr::map(estimate, "bias_right"))   %>%
    unnest(c(tau, se, ci_lower, ci_upper, 
             lower_estimate, upper_estimate, 
             lower_estimate_se, upper_estimate_se,
             lower_estimate_bias, upper_estimate_bias))  %>%
    select(-data, -estimate)
  return( ntl_estimates)
}

make_national_robustness_estimates = function(ntl_data, bw_list, K_list,
                                              default_bw = 5,
                                              default_K = 4) {

  ntl_estimates = ntl_data %>% ungroup() %>%
    nest(data = c(age, outcome, pop, czone) )  %>%
    expand_grid(tibble(bw = c( bw_list))) %>%
    mutate(estimate = future_map2(data, bw, 
                                  ~ estimate_model(reg_data = .x, bw = .y), 
                                  .progress = TRUE)) %>%
    mutate(K_scale = default_K) %>%
    bind_rows(
      ntl_data %>% ungroup() %>%
        nest(data = c(age, outcome, pop, czone) )  %>%
        expand_grid(tibble(K_scale = c( K_list))) %>%
        mutate(estimate = future_map2(data, K_scale, 
                                      ~ estimate_model(reg_data = .x, K_scale = .y), 
                                      .progress = TRUE)) %>%
        mutate(bw = default_bw) 
    )
  
  
  ntl_estimates =  ntl_estimates %>% 
    mutate(tau      = purrr::map(estimate, "tau"),
           se       = purrr::map(estimate, "se"),
           ci_lower = purrr::map(estimate, "ci_lower"),
           ci_upper = purrr::map(estimate, "ci_upper"),
           lower_estimate = purrr::map(estimate, "model_left"),
           upper_estimate = purrr::map(estimate, "model_right"),
           lower_estimate_se = purrr::map(estimate, "model_left_se"),
           upper_estimate_se = purrr::map(estimate, "model_right_se"),
           lower_estimate_bias = purrr::map(estimate, "bias_left"),
           upper_estimate_bias = purrr::map(estimate, "bias_right"))   %>%
    unnest(c(tau, se, ci_lower, ci_upper, 
             lower_estimate, upper_estimate, 
             lower_estimate_se, upper_estimate_se,
             lower_estimate_bias, upper_estimate_bias))  %>%
    select(-data, -estimate)
  return( ntl_estimates)
}

make_regional_estimates = function(cz_data) {
  czone_estimates = cz_data %>% ungroup() %>%
    nest(data = c(age, outcome, pop) ) %>%
    mutate(estimate = future_map(data, estimate_model, .progress = TRUE))
  
  czone_estimates = czone_estimates %>% 
    mutate(tau      = purrr::map(estimate, "tau"),
           se       = purrr::map(estimate, "se"),
           ci_lower = purrr::map(estimate, "ci_lower"),
           ci_upper = purrr::map(estimate, "ci_upper"),
           lower_estimate = purrr::map(estimate, "model_left"),
           upper_estimate = purrr::map(estimate, "model_right"),
           lower_estimate_se = purrr::map(estimate, "model_left_se"),
           upper_estimate_se = purrr::map(estimate, "model_right_se"),
           lower_estimate_bias = purrr::map(estimate, "bias_left"),
           upper_estimate_bias = purrr::map(estimate, "bias_right"))   %>%
    unnest(c(tau, se, ci_lower, ci_upper, 
             lower_estimate, upper_estimate, 
             lower_estimate_se, upper_estimate_se,
             lower_estimate_bias, upper_estimate_bias))  %>%
    select(-data, -estimate)
  return(czone_estimates)
}

make_regional_robustness_estimates = function(cz_data, bw_list, K_list,
                                              default_bw = 5,
                                              default_K = 4) {
  
  czone_estimates = cz_data %>% ungroup() %>%
    nest(data = c(age, outcome, pop) ) %>%
    expand_grid(tibble(bw = c( bw_list))) %>%
    mutate(estimate = future_map2(data, bw, 
                                  ~ estimate_model(reg_data = .x, bw = .y), 
                                  .progress = TRUE)) %>%
    mutate(K_scale = default_K) %>%
    bind_rows(
      cz_data %>% ungroup() %>%
        nest(data = c(age, outcome, pop) ) %>%
        expand_grid(tibble(K_scale = c( K_list))) %>%
        mutate(estimate = future_map2(data, K_scale, 
                                      ~ estimate_model(reg_data = .x, K_scale = .y), 
                                      .progress = TRUE)) %>%
        mutate(bw = default_bw) 
    )
  
  czone_estimates = czone_estimates %>% 
    mutate(tau      = purrr::map(estimate, "tau"),
           se       = purrr::map(estimate, "se"),
           ci_lower = purrr::map(estimate, "ci_lower"),
           ci_upper = purrr::map(estimate, "ci_upper"),
           lower_estimate = purrr::map(estimate, "model_left"),
           upper_estimate = purrr::map(estimate, "model_right"),
           lower_estimate_se = purrr::map(estimate, "model_left_se"),
           upper_estimate_se = purrr::map(estimate, "model_right_se"),
           lower_estimate_bias = purrr::map(estimate, "bias_left"),
           upper_estimate_bias = purrr::map(estimate, "bias_right"))   %>%
    unnest(c(tau, se, ci_lower, ci_upper, 
             lower_estimate, upper_estimate, 
             lower_estimate_se, upper_estimate_se,
             lower_estimate_bias, upper_estimate_bias))  %>%
    select(-data, -estimate)
  return(czone_estimates)
}

make_czone_iv_estimates = function(cz_data) {
  czone_iv_estimates = cz_data %>% ungroup()  %>%
    nest(data = c(age, outcome, endog, pop, total_pop)) %>%
    mutate(estimate = future_map(data, estimate_model_fuzzy, 
                                 .progress = TRUE))
  czone_iv_estimates = czone_iv_estimates %>% 
    mutate(beta      = purrr::map(estimate, "tau"),
           beta_se       = purrr::map(estimate, "se"),
           beta_ci_lower = purrr::map(estimate, "ci_lower"),
           beta_ci_upper = purrr::map(estimate, "ci_upper"))   %>%
    unnest(c(beta, beta_se, beta_ci_lower, beta_ci_upper)) %>%
    select(-data, -estimate) 
  return(czone_iv_estimates)
}

make_national_iv_estimates = function(cz_data) {
    czone_iv_estimates = cz_data %>% ungroup()  %>%
      nest(data = c(czone, age, outcome, endog, pop, total_pop)) %>%
      mutate(estimate = future_map(data, estimate_model_fuzzy, 
                                   .progress = TRUE))
    czone_iv_estimates = czone_iv_estimates %>% 
      mutate(beta      = purrr::map(estimate, "tau"),
             beta_se       = purrr::map(estimate, "se"),
             beta_ci_lower = purrr::map(estimate, "ci_lower"),
             beta_ci_upper = purrr::map(estimate, "ci_upper"))   %>%
      unnest(c(beta, beta_se, beta_ci_lower, beta_ci_upper)) %>%
      select(-data, -estimate) 
    return(czone_iv_estimates)
  }

shrink_czone_estimates = function(cz_data) {
  czone_estimates_shrink = cz_data %>% 
    mutate(significant =  test_sig(ci_lower, ci_upper)) %>%
    nest(data = c(czone, tau, se, ci_lower, ci_upper, 
                  lower_estimate, upper_estimate, 
                  lower_estimate_se, upper_estimate_se,
                  lower_estimate_bias, upper_estimate_bias,
                  significant)) %>% 
    mutate(shrunk = purrr::map(data,shrink)) %>%
    unnest(shrunk) %>%
    select(-data)
  return(czone_estimates_shrink)
}
estimate_comparison <- function(reg_data) {
  modelfit = felm(data = reg_data,
                  tau ~ covariate | 0 | 0 | czone , weight = reg_data$pop)
  modeloutput3 = broom::tidy(modelfit) %>% 
    filter(term == "covariate") %>%
    select(term,estimate, p.value, std.error) %>%
    mutate(term = "tau") %>%
    gather(stat_name, value, -term) %>%
    unite(term2, term, stat_name) %>%
    spread(term2, value)
  return(modeloutput3)
}

estimate_comparison_iv <- function(reg_data) {
  modelfit = felm(data = reg_data,
                  beta ~ covariate | 0 | 0 | czone , weight = reg_data$pop)
  modeloutput3 = broom::tidy(modelfit) %>% 
    filter(term == "covariate") %>%
    select(term,estimate, p.value, std.error) %>%
    mutate(term = "beta") %>%
    gather(stat_name, value, -term) %>%
    unite(term2, term, stat_name) %>%
    spread(term2, value)
  return(modeloutput3)
}


estimate_comparison_fe_state <- function(reg_data) {
  modelfit = felm(data = reg_data, 
                  tau ~ covariate | statename | 0 | czone, weight = reg_data$pop)
  modeloutput = broom::tidy(modelfit) %>% 
    filter(term == "covariate") %>%
    select(term,estimate, p.value, std.error) %>%
    mutate(term = "tau") %>%
    gather(stat_name, value, -term) %>%
    unite(term2, term, stat_name) %>%
    spread(term2, value)
  return(modeloutput)
}
estimate_comparison_fe_division <- function(reg_data) {
  modelfit = felm(data = reg_data, 
                  tau ~ covariate |  division | 0 | czone , weight = reg_data$pop)
  modeloutput = broom::tidy(modelfit) %>% 
    filter(term == "covariate") %>%
    select(term,estimate, p.value, std.error) %>%
    mutate(term = "tau") %>%
    gather(stat_name, value, -term) %>%
    unite(term2, term, stat_name) %>%
    spread(term2, value)
  return(modeloutput)
}
estimate_comparison_fe_region <- function(reg_data) {
  modelfit = felm(data = reg_data, 
                  tau ~ covariate | region | 0 | czone , weight = reg_data$pop)
  modeloutput = broom::tidy(modelfit) %>% 
    filter(term == "covariate") %>%
    select(term,estimate, p.value, std.error) %>%
    mutate(term = "tau") %>%
    gather(stat_name, value, -term) %>%
    unite(term2, term, stat_name) %>%
    spread(term2, value)
  return(modeloutput)
}

get_LASSO_coefficients <- function(LASSO_fit){
  coeff_values <- LASSO_fit %>% 
    broom::tidy() %>% 
    as_tibble() %>% 
    select(-c(step, dev.ratio)) %>% 
    tidyr::complete(lambda, nesting(term), fill = list(estimate = 0)) %>% 
    arrange(desc(lambda)) %>% 
    select(term, estimate, lambda) 
  return(coeff_values)
}

shrink = function(df) {
  m = MASS::fitdistr(df$tau, "normal")
  m2 = MASS::fitdistr(df$lower_estimate, "normal")
  m3 = MASS::fitdistr(df$upper_estimate, "normal")
  df = df %>% 
    mutate(b = se^2/(m$estimate[2]^2 + se^2)) %>%
    mutate(tau = tau * (1-b) + m$estimate[1]*b) %>%
    mutate(b2 = lower_estimate_se^2/(m2$estimate[2]^2 + lower_estimate_se^2)) %>%
    mutate(lower_estimate = lower_estimate * (1-b2) + m2$estimate[1]*b2) %>%
    mutate(b3 = upper_estimate_se^2/(m3$estimate[2]^2 + upper_estimate_se^2)) %>%
    mutate(upper_estimate = upper_estimate * (1-b3) + m3$estimate[1]*b3) %>%
    select(-b, -b2, -b3)
  return(df)
}

make_fig1 = function(national_estimates,
                     national_data,
                     regional_estimates,
                     regional_data,
                     varname
                     ) {
  estimated = national_estimates %>% 
    filter(variable == varname) %>%
    mutate(age = 65)
  plot_data = national_data %>% 
    filter(variable == varname) %>%
    mutate(outcome = outcome / estimated$lower_estimate)
  estimated_all = national_estimates %>% 
    filter(variable == varname) %>%
    mutate(state = "US") %>%
    bind_rows(regional_estimates  %>% 
                filter(variable == varname)) %>%
    mutate(age = 65) %>%
    mutate(significant = test_sig(ci_lower, ci_upper)) 
  plot_data_all = national_data %>% 
    filter(variable == varname) %>%
    mutate(outcome = outcome ) %>%
    mutate(state = "US") %>%
    bind_rows(regional_data %>% 
                filter(variable == varname) %>%
                left_join(estimated_all %>% 
                            select(state, lower_estimate))
    )
  plot_data_all2 = plot_data_all %>% mutate(infer = "data") %>%
    bind_rows(
    estimated_all  %>% select(variable, lower_estimate, upper_estimate, state) %>% 
      mutate(upper_estimate = upper_estimate, 
             lower_estimate = lower_estimate) %>%
      gather(key, outcome, -variable, -state) %>%
      mutate(age = case_when(key == "lower_estimate" ~  64.9, TRUE ~ 65.1)) %>%
      mutate(infer = case_when(key == "lower_estimate" ~ "infer_left",
                               key == "upper_estimate" ~ "infer_right"))
  )
  
  

  
  g = ggplot() + 
    geom_line(data = plot_data_all2, 
              aes(y = outcome, x=age, group = factor(state)),  
              color = "grey",  alpha = 0.4, size = 1) +
    geom_point(data = plot_data_all2, 
               aes(y = outcome, x=age, group = factor(state),
                   shape = factor(infer)),  
               color = "grey",  alpha = 0.4, 
               size = 3, show.legend = FALSE) +
    geom_line(data = plot_data_all2 %>% filter(state == "US"), 
              aes(y = outcome, x=age, group = factor(state)),  
              color = dred,size = 1) +
    geom_point(data = plot_data_all2 %>% filter(state == "US"), 
               aes(y = outcome, x=age, group = factor(state),
                   shape = factor(infer)),  
               color = dred,size = 4, show.legend = FALSE) +
    scale_shape_manual(values =c("circle", "triangle", "triangle filled")) +
    #geom_vline(xintercept = 65, linetype="dashed", color = "grey") +
    theme_classic() +
    labs(y = "",
         x = "Age") +
    theme(text = element_text(size=28)) 
  return(g)  
}
make_paste_vals = function(pt_estimates, var_estimates, varname,
                           unit="") {
  paste_val = paste0("National change at 65:\n",
                     unit,
                     format(pt_estimates$tau[pt_estimates$variable == varname], 
                            nsmall = 1, digits =1),
                     " ",
                     "[", format(pt_estimates$ci_lower[pt_estimates$variable == varname], 
                                            nsmall = 1, digits =1),
                     ",",
                     "",
                     "",
                     format(pt_estimates$ci_upper[pt_estimates$variable == varname], 
                            nsmall = 1, digits =1),
                     "]")
  paste_val2 = paste0("Variance reduction at 65:\n", 
                     format(100*var_estimates$phi[var_estimates$variable == varname], 
                            nsmall = 1, digits =1),"%",
                     " ",
                     "[", format(100*var_estimates$phi_ci_lower[var_estimates$variable == varname], 
                                            nsmall = 1, digits =1),
                     ",",
                     "",
                     "",
                     format(100*var_estimates$phi_ci_upper[var_estimates$variable == varname], 
                            nsmall = 1, digits =1), 
                     "]")
  
  return(list(paste_val = paste_val,
              paste_val2 = paste_val2))
}


lasso_predict = function(fm, predictor_matrix, outcome, weight) {
  alpha_grid <- seq(1, 1, 0.05)
  search <- foreach(i = alpha_grid, .combine = rbind) %dopar% {
    cv <- cv.glmnet(x=predictor_matrix, y=outcome,  nfold = 50, parallel = TRUE, alpha = i,  weight=weight)
    data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.min], lambda.min = cv$lambda.min, lambda.1se = cv$lambda.1se, alpha = i)
  }
  cv3 <- search[search$cvm == min(search$cvm), ]
  LASSO_fit <- glmnet(x=predictor_matrix, y=outcome, alpha = cv3$alpha,
                      lambda=cv3$lambda.min, weight=weight)
  LASSO_coefficients <- get_LASSO_coefficients(LASSO_fit)
  
  nonzero_vars = LASSO_coefficients %>% filter(lambda == cv3$lambda.min & estimate != 0 ) %>% 
    select(term) %>% filter(term != "(Intercept)") %>% as_vector()
  names(nonzero_vars) <- NULL
  y_hat_predict =  predict(LASSO_fit, newx = predictor_matrix, s=cv3$lambda.min)
  
  LASSO_coefficients %>% filter(lambda == cv3$lambda.min & estimate != 0 ) 
  return(list(y_hat_predict = y_hat_predict,
              LASSO_coefficients = LASSO_coefficients))
}

construct_lasso_predict = function(reg_data, fm) {
  
  reg_data2_lasso = reg_data  %>%
    mutate(sampling_var = se^2) %>% 
    mutate(sampling_weight = 1/sampling_var) %>%
    mutate(tau_mean = weighted.mean(tau , w=sampling_weight)) %>%
    mutate(tau_tilde = tau - tau_mean)
  
  predictor_matrix <- model.matrix(fm, data = reg_data2_lasso)[, -1]
  lasso_output_full_tilde = 
    lasso_predict(fm, predictor_matrix, 
                  reg_data2_lasso$tau_tilde, 
                  reg_data2_lasso$sampling_weight)
  lasso_tau_hat = lasso_output_full_tilde$y_hat_predict - 
    weighted.mean(lasso_output_full_tilde$y_hat_predict, weight = reg_data2_lasso$sampling_weight)
  forecast_ch = lm(reg_data2_lasso$tau_tilde ~ lasso_tau_hat, weight = reg_data2_lasso$sampling_weight)
  forecast_ch = forecast_ch$coefficients[2]
  chi_l = weighted.var(reg_data2_lasso$tau_tilde - forecast_ch * lasso_tau_hat, 
                       w=reg_data2_lasso$sampling_weight) -
    weighted.mean(reg_data2_lasso$se^2, w= reg_data2_lasso$sampling_weight)
  
  tau_full_hat = reg_data2_lasso %>% 
    bind_cols(tibble(tau_lasso = lasso_output_full_tilde$y_hat_predict) ) %>%
    mutate(tau_lasso = tau_lasso[,"1"]) %>%
    mutate(ch_weight = sampling_var / (chi_l + sampling_var)) %>%
    mutate(rmse = sqrt(1 / ((1/chi_l) + (1/sampling_var)))) %>% 
    mutate(tau_ch = ch_weight*(forecast_ch*tau_lasso) + (1-ch_weight) * tau_tilde) %>%
    mutate(tau_ch = tau_ch + tau_mean) %>%
    select(czone, aca, tau_ch, tau_lasso, tau_mean, tau_tilde, ch_weight, variable, rmse)
  
  return(tau_full_hat)
}


construct_lasso_predict_iv = function(reg_data, fm) {
  
  reg_data2_lasso = reg_data  %>%
    mutate(sampling_var = (beta_se^2)) %>% 
    mutate(sampling_weight = 1/sampling_var) %>%
    mutate(beta_mean = weighted.mean(beta , w=sampling_weight)) %>%
    mutate(beta_tilde = beta - beta_mean) 
  
  predictor_matrix <- model.matrix(fm, data = reg_data2_lasso)[, -1]
  lasso_output_full_tilde = 
    lasso_predict(fm, predictor_matrix, reg_data2_lasso$beta_tilde, reg_data2_lasso$sampling_weight)
  lasso_beta_hat = lasso_output_full_tilde$y_hat_predict - 
    weighted.mean(lasso_output_full_tilde$y_hat_predict, weight = reg_data2_lasso$sampling_weight)
  forecast_ch = lm(reg_data2_lasso$beta_tilde ~ lasso_beta_hat, weight = reg_data2_lasso$sampling_weight)
  forecast_ch = forecast_ch$coefficients[2]
  chi_l = weighted.var(reg_data2_lasso$beta_tilde - forecast_ch * lasso_beta_hat, 
                       w=reg_data2_lasso$sampling_weight) -
    weighted.mean(reg_data2_lasso$se^2, w= reg_data2_lasso$sampling_weight)
  
  beta_full_hat = reg_data2_lasso %>% 
    bind_cols(tibble(beta_lasso = lasso_output_full_tilde$y_hat_predict) ) %>%
    mutate(beta_lasso = beta_lasso[,"1"]) %>%
    mutate(ch_weight = sampling_var / (chi_l + sampling_var)) %>%
    mutate(rmse = sqrt(1 / ((1/chi_l) + (1/sampling_var)))) %>% 
    mutate(beta_ch = ch_weight*(forecast_ch*beta_lasso) + (1-ch_weight) * beta_tilde) %>%
    mutate(beta_ch = beta_ch + beta_mean) %>%
    select(czone, aca, beta_ch, beta_lasso, beta_mean, beta_tilde, ch_weight, variable, rmse)
  
  return(beta_full_hat)
}


mv_estimate = function(varlist, reg_data) {
  fm = as.formula(paste("value ~ factor(side) + ", 
                        paste(varlist, "+ ", 
                              paste("(",varlist, "):factor(side)",
                                    "| 0 | 0 | czone"))))
  modelfit = felm(fm, weight = reg_data$pop, 
                  data = reg_data)
  estimates = broom::tidy(modelfit) %>% 
    filter(term != "(Intercept)" & term != "factor(side)upper_estimate") %>%
    select(term, estimate, p.value) %>%
    separate(term, into=c("term1", "term2"), sep = ":") %>%
    filter(!is.na(term2) & !is.na(term1)) %>%
    mutate(term1 = case_when(!is.na(term2) ~ "diff_beta")) 
  fm = as.formula(paste("value ~ factor(side) + ", 
                        paste(paste("(",varlist, "):factor(side)",
                                    "| 0 | 0 | czone"))))
  modelfit2 = felm(fm , weight = reg_data$pop, 
                   data = reg_data)
  estimates2 = broom::tidy(modelfit2) %>% 
    filter(term != "(Intercept)" & term != "factor(side)upper_estimate") %>%
    select(term, estimate, p.value) %>%
    separate(term, into=c("term1", "term2"), sep = ":") %>%
    filter(!is.na(term2)) %>%
    mutate(term1 = case_when(term1 == "factor(side)lower_estimate" ~ "pre65_beta",
                             term1 == "factor(side)upper_estimate" ~ "post65_beta")) 
  estimates_all = estimates2 %>% bind_rows(estimates)
  return(estimates_all)
}


mv_estimate_diff = function(fm, reg_data) {
  modelfit = felm(fm, weight = reg_data$pop, 
                  data = reg_data)
  estimates =broom::tidy(modelfit) %>% 
    filter(term != "(Intercept)") %>%
    select(term, estimate, p.value, std.error)  %>%
    rename(covariate = term) %>%
    mutate(significant = p.value < 0.05)
  
  return(estimates = estimates)
}






make_mv_diff_data= function(data, covariates) {
  varlist = paste(covariates, collapse="+")
  fm = as.formula(paste("tau ~ ", varlist, 
                        "| 0 | 0 | czone"))
  mv_estimate_tau = mv_estimate_diff(fm, data)
  return(mv_estimate_tau)
}

make_mv_diff_data_iv = function(data, covariates) {
  varlist = paste(covariates, collapse="+")
  fm = as.formula(paste("beta ~ ", varlist, 
                        "| 0 | 0 | czone"))
  mv_estimate_tau = mv_estimate_diff(fm, data)
  return(mv_estimate_tau)
}

make_mv_diff_data_fe= function(data, covariates, fe, clus) {
  varlist = paste(covariates, collapse="+")
  fm = as.formula(paste("tau ~ ", varlist, 
                        "|", fe, "|0|", clus))
  mv_estimate_tau = mv_estimate_diff(fm, data)
  return(mv_estimate_tau)
}

make_uni_results = function(reg_data_uni, ntl_estimates,
                            ntl_iv_estimates,
                            variable_labels, fe = NA) {
  
  ## Univariate comparisons for Figure 3

  estimates_uni_tau = reg_data_uni %>% 
    filter(variable == "q_coll_12") %>%
    filter(side == "lower_estimate") %>%
    nest(data = c(czone, statename, region, division, czname, tau, se, side, value, pop, all_inpat_days, covariate, 
                  tau_ui, value_ui, beta, beta_se, beta_ci_lower, beta_ci_upper)) 
  
  if(is.na(fe)) {
    estimates_uni_tau = estimates_uni_tau %>%
    mutate(estimate = purrr::map(data, estimate_comparison)) 
  }
  else if (fe == "state") {
    estimates_uni_tau = estimates_uni_tau %>%
      mutate(estimate = purrr::map(data, estimate_comparison_fe_state)) 
  }
  else if (fe == "division") {
    estimates_uni_tau = estimates_uni_tau %>%
      mutate(estimate = purrr::map(data, estimate_comparison_fe_division)) 
  }
  else if (fe == "region") {
    estimates_uni_tau = estimates_uni_tau %>%
      mutate(estimate = purrr::map(data, estimate_comparison_fe_region)) 
  }
  estimates_uni_tau = estimates_uni_tau %>%
    unnest(estimate) %>% select(-data) %>%
    left_join(ntl_estimates %>% 
                filter(variable == "q_coll_12") %>% 
                select(variable, tau)) %>%
    mutate(tau_estimate2 = tau_estimate/abs(tau),
           tau_std.error2 = tau_std.error/abs(tau))
  
  
  estimates_uni_tau = estimates_uni_tau %>% 
    mutate(significant = tau_p.value < 0.05) %>%
    left_join(variable_labels) %>%
    mutate(cov_name = as.factor(cov_name)) %>%
    mutate(cov_name = factor(cov_name, levels = c(rev(variable_labels[variable_labels$panel == "A. Area-level demographic characteristics", ]$cov_name),
                                                  rev(variable_labels[variable_labels$panel == "B. Healthcare market characteristics", ]$cov_name))))
  
  estimates_uni_tau = estimates_uni_tau %>% 
    filter(aca == "Full Sample")
  
  estimates_uni_beta = reg_data_uni %>% 
    filter(variable == "q_coll_12") %>%
    filter(side == "lower_estimate") %>%
    nest(data = c(czone, statename, region, division, czname, tau, se, side, value, pop, all_inpat_days, covariate, 
                  tau_ui, value_ui, beta, beta_se, beta_ci_lower, beta_ci_upper)) %>% 
    mutate(estimate = purrr::map(data, estimate_comparison_iv)) %>% 
    unnest(estimate) %>% select(-data) %>%
    left_join(ntl_iv_estimates %>% 
                filter(variable == "q_coll_12") %>% 
                select(variable, beta)) %>%
    mutate(beta_estimate2 = beta_estimate/abs(beta),
           beta_std.error2 = beta_std.error/abs(beta))
  
  estimates_uni_beta = estimates_uni_beta %>% 
    mutate(significant = beta_p.value < 0.05) %>%
    left_join(variable_labels) %>%
    mutate(cov_name = as.factor(cov_name)) %>%
    mutate(cov_name = factor(cov_name, levels = c(rev(variable_labels[variable_labels$panel == "A. Area-level demographic characteristics", ]$cov_name),
                                                  rev(variable_labels[variable_labels$panel == "B. Healthcare market characteristics", ]$cov_name))))
  
  estimates_uni_beta = estimates_uni_beta %>% filter(aca == "Full Sample")
  
  return(list(estimates_uni_tau = estimates_uni_tau,
              estimates_uni_beta = estimates_uni_beta))
  
}

make_mv_results = function(reg_data_mv, rhs_all,
                           ntl_estimates, ntl_iv_estimates, variable_labels) {
  estimates_mv_tau =  make_mv_diff_data(reg_data_mv %>% 
                                          filter(side == "lower_estimate" & aca == "Full Sample"),
                                        rhs_all)  %>%
    mutate(tau = ntl_estimates$tau[ntl_estimates$variable == "q_coll_12"],) %>%
    mutate(estimate2 = estimate/abs(tau),
           std.error2 = std.error/abs(tau))
  
  estimates_mv_tau = estimates_mv_tau %>%
    rename(cov_label = covariate) %>%
    left_join(variable_labels)  %>%
    mutate(cov_name = as.factor(cov_name)) %>%
    mutate(cov_name = factor(cov_name, levels = c(rev(variable_labels[variable_labels$panel == "A. Area-level demographic characteristics", ]$cov_name),
                                                  rev(variable_labels[variable_labels$panel == "B. Healthcare market characteristics", ]$cov_name))))
  
  estimates_mv_beta =  make_mv_diff_data_iv(reg_data_mv %>% 
                                              filter(side == "lower_estimate" & aca == "Full Sample"),
                                            rhs_all)  %>%
    mutate(beta = ntl_iv_estimates$beta[ntl_iv_estimates$variable == "q_coll_12"],) %>%
    mutate(estimate2 = estimate/abs(beta),
           std.error2 = std.error/abs(beta))
  
  estimates_mv_beta = estimates_mv_beta %>%
    rename(cov_label = covariate) %>%
    left_join(variable_labels)  %>%
    mutate(cov_name = as.factor(cov_name)) %>%
    mutate(cov_name = factor(cov_name, levels = c(rev(variable_labels[variable_labels$panel == "A. Area-level demographic characteristics", ]$cov_name),
                                                  rev(variable_labels[variable_labels$panel == "B. Healthcare market characteristics", ]$cov_name))))
  
  return(list(estimates_mv_tau = estimates_mv_tau,
              estimates_mv_beta = estimates_mv_beta))
  
}

construct_full_panel = function(uni_results,
                                mv_results,
                                mv_lasso_results) {
  estimates_tau_full_panel = mv_results$estimates_mv_tau %>% 
    select(estimate, std.error, 
           estimate2, std.error2, cov_name, panel)  %>% 
    mutate(group = "Multivariate") %>% bind_rows(
      uni_results$estimates_uni_tau %>% 
        select(estimate = tau_estimate, std.error = tau_std.error, 
               estimate2 = tau_estimate2, std.error2 = tau_std.error2,
               cov_name, panel) %>% 
        mutate(group = "Bivariate")
    ) %>% bind_rows(
      mv_lasso_results$estimates_mv_tau %>% select(estimate, std.error, 
                                        estimate2, std.error2, 
                                        cov_name, panel)  %>% 
        mutate(group = "Post-Lasso") 
    ) %>%
    mutate(group = factor(group, levels = c("Bivariate", "Multivariate", "Post-Lasso")))
  
  estimates_beta_full_panel = mv_results$estimates_mv_beta %>% 
    select(estimate, std.error, 
           estimate2, std.error2, cov_name, panel) %>%
    mutate(group = "Multivariate") %>% bind_rows(
      uni_results$estimates_uni_beta %>% select(estimate = beta_estimate, 
                                    std.error = beta_std.error, 
                                    estimate2 = beta_estimate2,
                                    std.error2 = beta_std.error2,
                                    cov_name, panel) %>% 
        mutate(group = "Bivariate")
    ) %>% bind_rows(
      mv_lasso_results$estimates_mv_beta %>% select(estimate, std.error, 
                                         estimate2, std.error2,
                                         cov_name, panel)  %>% 
        mutate(group = "Post-Lasso") 
    ) %>%
    mutate(group = factor(group, levels = c("Bivariate", "Multivariate", "Post-Lasso")))
  
  estimates_full_panel = estimates_tau_full_panel %>% 
    mutate(estimate_type = "Per Capita") %>%
    bind_rows(estimates_beta_full_panel %>% 
                mutate(estimate_type = "Per Newly Insured"))
  
  return(list(estimates_full_panel = estimates_full_panel,
              estimates_tau_full_panel = estimates_tau_full_panel,
              estimates_beta_full_panel = estimates_beta_full_panel))
  
}

make_uni_results_fe_panel = function(reg_data_uni, national_estimates,
                                     national_iv_estimates, variable_labels) {
  uni_results_fe_state = make_uni_results(reg_data_uni, 
                                          national_estimates,
                                          national_iv_estimates,
                                          variable_labels, "state")
  estimates_uni_tau_fe_state = uni_results_fe_state$estimates_uni_tau %>%
    mutate(fe = "statename")
  uni_results_fe_division = make_uni_results(reg_data_uni, 
                                             national_estimates,
                                             national_iv_estimates,
                                             variable_labels, "division")
  estimates_uni_tau_fe_division = uni_results_fe_division$estimates_uni_tau %>%
    mutate(fe = "division")
  uni_results_fe_region = make_uni_results(reg_data_uni, 
                                           national_estimates,
                                           national_iv_estimates,
                                           variable_labels, "region")
  estimates_uni_tau_fe_region = uni_results_fe_region$estimates_uni_tau %>%
  mutate(fe = "region")
  
  uni_results_fe_none = make_uni_results(reg_data_uni, 
                                           national_estimates,
                                           national_iv_estimates,
                                           variable_labels)
  estimates_uni_tau_fe_none = uni_results_fe_none$estimates_uni_tau %>%
    mutate(fe = "none")
  
  estimates_uni_tau_fe =
    estimates_uni_tau_fe_region %>%
    bind_rows(estimates_uni_tau_fe_state) %>%
    bind_rows(estimates_uni_tau_fe_division) %>%
    bind_rows(estimates_uni_tau_fe_none) 
  estimates_uni_tau_fe$fe <- factor(estimates_uni_tau_fe$fe,
                                    levels = c("none", "region", "division", "statename"),
                                    labels = c("No FE", "Region", "Division", "State"))  
  
  return(estimates_uni_tau_fe)
}

make_mv_results_fe_panel = function(reg_data_mv, reg_var_list,
                                    ntl_estimates) {
  
  estimates_mv_tau_fe_state =
    make_mv_diff_data_fe(reg_data_mv %>%
                           filter(side == "lower_estimate" & aca == "Full Sample"),
                         reg_var_list, "statename", "czone") %>%
    mutate(fe = "statename")
  estimates_mv_tau_fe_division =
    make_mv_diff_data_fe(reg_data_mv %>%
                           filter(side == "lower_estimate" & aca == "Full Sample"),
                         reg_var_list, "division", "czone") %>%
    mutate(fe = "division")
  estimates_mv_tau_fe_region =
    make_mv_diff_data_fe(reg_data_mv %>%
                           filter(side == "lower_estimate" & aca == "Full Sample"),
                         reg_var_list, "region", "czone") %>%
    mutate(fe = "region")
  
  estimates_mv_tau_none =
    make_mv_diff_data(reg_data_mv %>%
                           filter(side == "lower_estimate" & aca == "Full Sample"),
                         reg_var_list) %>%
    mutate(fe = "none")
  estimates_mv_tau_fe =  estimates_mv_tau_fe_region %>%
    bind_rows(estimates_mv_tau_fe_state) %>%
    bind_rows(estimates_mv_tau_fe_division) %>%
    bind_rows(estimates_mv_tau_none)  %>%
    mutate(tau = ntl_estimates$tau[ntl_estimates$variable == "q_coll_12"]) %>%
    mutate(estimate2 = estimate/abs(tau),
           std.error2 = std.error/abs(tau)) %>%
    rename(cov_label = covariate) %>%
    left_join(variable_labels)  %>%
    mutate(cov_name = as.factor(cov_name)) %>%
    mutate(cov_name = factor(cov_name, levels = c(rev(variable_labels[variable_labels$panel == "A. Area-level demographic characteristics", ]$cov_name),
                                                  rev(variable_labels[variable_labels$panel == "B. Healthcare market characteristics", ]$cov_name)))) 
    
  estimates_mv_tau_fe$fe <- factor(estimates_mv_tau_fe$fe,
                                   levels = c("none", "region", "division", "statename"),
                                   labels = c("No FE", "Region", "Division", "State"))
  return(estimates_mv_tau_fe)
}

construct_full_panel_fe = function(estimates_uni_tau_fe, 
                                   estimates_mv_tau_fe, 
                                   estimates_mv_tau_lasso_fe,
                                   ntl_estimates) {
  estimates_tau_full_panel_fe = estimates_mv_tau_fe %>%
    select(estimate, estimate2, std.error2, std.error, cov_name, panel, fe)  %>%
    mutate(group = "Multivariate") %>% bind_rows(
      estimates_uni_tau_fe %>%
        select(estimate = tau_estimate, std.error = tau_std.error,
               estimate2 = tau_estimate2, std.error2 = tau_std.error2,
               cov_name, panel, fe)  %>%
        mutate(group = "Bivariate")
    ) %>% bind_rows(
      estimates_mv_tau_lasso_fe %>%
        select(estimate, std.error, estimate2, std.error2, cov_name, panel, fe)  %>%
        mutate(group = "Post-Lasso")
    ) %>%
    mutate(group = factor(group, levels = c("Bivariate", "Multivariate", "Post-Lasso")))
  
  return(estimates_tau_full_panel_fe)
  
}



estimate_model_naive <- function(reg_data) {
  reg_data = reg_data %>% mutate(medicare = age > 65) %>%
    filter(age < 75 & age > 55) %>% ungroup() %>% mutate(age = age - 65)
  weights = reg_data$pop
  modelfit_linear <- felm(outcome ~ age +  factor(medicare) + factor(medicare):age ,
                          data = reg_data,
                          weights = weights)
  modelfit_quad <- felm(outcome ~ age + I(age^2) +  factor(medicare) + factor(medicare):age + factor(medicare):I(age^2),
                        data = reg_data,
                        weights = weights)
  modelfit_cubic <- felm(outcome ~ age + I(age^2)+ I(age^3)  +  factor(medicare) + 
                           factor(medicare):age + factor(medicare):I(age^2) + factor(medicare):I(age^3),
                         data = reg_data,
                         weights = weights)
  
  
  tau_linear_robust        = as_tibble(summary(modelfit_linear, robust = TRUE)$coef )[3,]
  tau_quad_robust          = as_tibble(summary(modelfit_quad, robust = TRUE)$coef )[4,]
  tau_cubic_robust         = as_tibble(summary(modelfit_cubic, robust = TRUE)$coef )[5,]
  modelfit_linear <- felm(outcome ~ age +  factor(medicare) + factor(medicare):age | 0 | 0 | age,
                          data = reg_data,
                          weights = weights)
  modelfit_quad <- felm(outcome ~ age + I(age^2) +  factor(medicare) + factor(medicare):age + factor(medicare):I(age^2)  | 0 | 0 | age,
                        data = reg_data,
                        weights = weights)
  modelfit_cubic <- felm(outcome ~ age + I(age^2)+ I(age^3)  +  factor(medicare) + 
                           factor(medicare):age + factor(medicare):I(age^2) + factor(medicare):I(age^3)  | 0 | 0 | age,
                         data = reg_data,
                         weights = weights)
  
  rdrobust_linear = rdrobust(y=reg_data$outcome  , x= reg_data$age, c = 0, 
                             weights = weights)
  
  tau_linear_cluster        = as_tibble(summary(modelfit_linear)$coef )[3,]
  tau_quad_cluster          = as_tibble(summary(modelfit_quad)$coef )[4,]
  tau_cubic_cluster         = as_tibble(summary(modelfit_cubic)$coef )[5,]
  
  return(list(tau_linear_robust     = tau_linear_robust$Estimate,
              se_linear_robust      = tau_linear_robust$`Robust s.e`,
              cilower_linear_robust = tau_linear_robust$Estimate - 1.96*tau_linear_robust$`Robust s.e`,
              ciupper_linear_robust = tau_linear_robust$Estimate + 1.96*tau_linear_robust$`Robust s.e`,
              tau_quad_robust   = tau_quad_robust$Estimate,
              se_quad_robust    = tau_quad_robust$`Robust s.e`,
              cilower_quad_robust = tau_quad_robust$Estimate - 1.96*tau_quad_robust$`Robust s.e`,
              ciupper_quad_robust = tau_quad_robust$Estimate + 1.96*tau_quad_robust$`Robust s.e`,
              tau_cubic_robust   = tau_cubic_robust$Estimate,
              se_cubic_robust    = tau_cubic_robust$`Robust s.e`,
              cilower_cubic_robust = tau_cubic_robust$Estimate - 1.96*tau_cubic_robust$`Robust s.e`,
              ciupper_cubic_robust = tau_cubic_robust$Estimate + 1.96*tau_cubic_robust$`Robust s.e`,
              tau_linear_cluster = tau_linear_cluster$Estimate,
              se_linear_cluster  = tau_linear_cluster$`Cluster s.e.`,
              cilower_linear_cluster = tau_linear_cluster$Estimate - 1.96*tau_linear_cluster$`Cluster s.e.`,
              ciupper_linear_cluster = tau_linear_cluster$Estimate + 1.96*tau_linear_cluster$`Cluster s.e.`,
              tau_quad_cluster   = tau_quad_cluster$Estimate,
              se_quad_cluster    = tau_quad_cluster$`Cluster s.e.`,
              cilower_quad_cluster = tau_quad_cluster$Estimate - 1.96*tau_quad_cluster$`Cluster s.e.`,
              ciupper_quad_cluster = tau_quad_cluster$Estimate + 1.96*tau_quad_cluster$`Cluster s.e.`,
              tau_cubic_cluster   = tau_cubic_cluster$Estimate,
              se_cubic_cluster    = tau_cubic_cluster$`Cluster s.e.`,
              cilower_cubic_cluster = tau_cubic_cluster$Estimate - 1.96*tau_cubic_cluster$`Cluster s.e.`,
              ciupper_cubic_cluster = tau_cubic_cluster$Estimate + 1.96*tau_cubic_cluster$`Cluster s.e.`,
              tau_linear_rdrobust       = rdrobust_linear$coef[3,1],
              se_linear_rdrobust        = rdrobust_linear$se[3,1],
              cilower_linear_rdrobust   = rdrobust_linear$ci[3,1],
              ciupper_linear_rdrobust   = rdrobust_linear$ci[3,2]
  ))
  
}



make_table_naive <- function(estimates) {
  output_table = estimates %>% select(var_label, tau, se, ci_lower, ci_upper) %>%
    mutate(significant = case_when((ci_lower < 0 & ci_upper < 0) |  (ci_lower > 0 & ci_upper > 0) ~ "*",
                                   TRUE ~ "")) %>%
    mutate( tau = paste0(round(tau, digits = 3), significant),
            se  =  paste0("(", round(se,  digits = 3), ")")) %>%
    select(-significant) %>%
    mutate( ci_lower = paste0("[", round(ci_lower,digits=3), ","),
            ci_upper = paste0(round(ci_upper,digits=3), "]")) %>%
    mutate(varorder = row_number()) %>%
    gather(row, outcome, -var_label,  -varorder) %>%
    arrange(varorder) %>%
    select(-row, -varorder) %>%
    group_by(var_label) %>%
    mutate(a = row_number()) %>% ungroup() %>%
    mutate(var_label = replace(var_label, a != 1, NA)
    )   %>%
    select(-a) %>%
    mutate(row = row_number()) 
  return(output_table)
}

combine_table_cols = function(data) {
  table_output = make_table_naive(data %>% 
                                       filter(model_name == "linear_robust")) %>% 
    select(linear_robust = outcome,row) %>%
    right_join(make_table_naive(data %>% 
                                  filter(model_name == "quad_robust")), by= c("row"="row")) %>% 
    select(quad_robust = outcome,linear_robust, row) %>%
    right_join(make_table_naive(data %>% 
                                  filter(model_name == "cubic_robust")), by= c("row"="row")) %>% 
    select(cubic_robust = outcome, quad_robust, linear_robust, row) %>%
    right_join(make_table_naive(data %>% 
                                  filter(model_name == "linear_cluster")), by= c("row"="row")) %>% 
    select(linear_cluster = outcome, cubic_robust, quad_robust, linear_robust, row) %>%
    right_join(make_table_naive(data %>% 
                                  filter(model_name == "quad_cluster")), by= c("row"="row")) %>% 
    select(quad_cluster = outcome, linear_cluster, cubic_robust, quad_robust, linear_robust, row) %>%
    right_join(make_table_naive(data %>% 
                                  filter(model_name == "cubic_cluster")), by= c("row"="row")) %>% 
    select(cubic_cluster = outcome, quad_cluster,linear_cluster, cubic_robust, quad_robust, linear_robust, row) %>%
    right_join(make_table_naive(data %>% 
                                  filter(model_name == "linear_rdrobust")), by= c("row"="row")) %>% 
    select(linear_rdrobust = outcome, cubic_cluster, quad_cluster,linear_cluster, cubic_robust, quad_robust, linear_robust, row) %>%
    right_join(make_table_naive(data %>% 
                                  filter(model_name == "main_main")), by= c("row"="row")) %>% 
    rename(estimates = outcome) %>%
    select(var_label,  estimates, linear_robust, quad_robust, cubic_robust, linear_cluster, quad_cluster,  cubic_cluster, linear_rdrobust ) 
  return(table_output)
}
