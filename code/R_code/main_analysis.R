

library(tidyverse)
library(gtools)
library(data.table)
library(RDHonest)
library(kableExtra)
library(haven)
library(ggthemes)
library(reldist)
library(purrr)
library(furrr)
library(lfe)
library(broom)
library(numform)
library(rdrobust)
library(glmnet)
library(binsreg)
library(doParallel)
library(scales)
library(VGAM)
plan(multiprocess)
options(knitr.kable.NA = '')
dblue = rgb(0,114,178,maxColorValue = 255)
dred = rgb(213,94,0,maxColorValue = 255)

# Exhibits and analysis for new short-paper draft
source("code/R_code/functions.R")

### Load + Clean Main Data
ccp_varlist = c("q_coll_12", "n_coll_12", "q_coll_12_gt0", 
                "balance_ttl", "balance_cc", "balance_mort",
                "balance_ttl_delinq","balance_cc_delinq", "balance_mort_delinq",
                "avg_riskscore", 
                "new_fc", "new_bpt")
ccp_data = load_data_ccp(ccp_varlist)
acs_puma_data = load_data_acs_puma()

czone_data_list = make_czone_data(ccp_data, acs_puma_data)
state_data_list = make_state_data(ccp_data, acs_puma_data)

czone_iv_data = czone_data_list$czone_acs_data %>% filter(variable == "has_insurance") %>% 
  rename(outcome2 = outcome) %>% select(-pop, -variable) %>% inner_join(czone_data_list$czone_data) %>%
  rename(endog = outcome, outcome = outcome2) %>%
  group_by(czone) %>% mutate(total_pop = sum(pop)) %>% ungroup() %>% 
  filter(total_pop > 300) %>%
  filter(variable == "q_coll_12")
czone_iv_data_prepost = czone_data_list$czone_acs_data_prepost %>% filter(variable == "has_insurance") %>% 
  rename(outcome2 = outcome) %>% select(-pop, -variable) %>% inner_join(czone_data_list$czone_data_prepost) %>%
  rename(endog = outcome, outcome = outcome2) %>%
  group_by(czone, aca, variable) %>% 
  mutate(total_pop = sum(pop)) %>% ungroup() %>% 
  filter(total_pop > 300) %>%
  filter(variable == "q_coll_12")

## Make National Estimates
national_estimates = make_national_estimates(czone_data_list$czone_data)
national_estimates_prepost = make_national_estimates(czone_data_list$czone_data_prepost)
national_acs_estimates = make_national_estimates(czone_data_list$czone_acs_data %>% zap_labels())
national_acs_estimates_prepost = make_national_estimates(czone_data_list$czone_acs_data_prepost  %>% zap_labels())
national_acs_estimates_cov = make_national_estimates(czone_data_list$czone_acs_data_cov  %>% zap_labels())
national_iv_estimates = make_national_iv_estimates(czone_iv_data  %>% zap_labels())

## Make National Robustness Estimates
bw_list = seq(3,10)
K_list = seq(1,10)
national_estimates_robust = make_national_robustness_estimates(czone_data_list$czone_data, bw_list, K_list)
national_acs_estimates_robust = make_national_robustness_estimates(czone_data_list$czone_acs_data, bw_list, K_list)

## Make State Estimates
state_estimates = make_regional_estimates(state_data_list$state_data)
state_estimates_prepost = make_regional_estimates(state_data_list$state_data_prepost)
state_acs_estimates = make_regional_estimates(zap_labels(state_data_list$state_acs_data))
state_acs_estimates_prepost = make_regional_estimates(zap_labels(state_data_list$state_acs_data_prepost))
state_acs_estimates_cov = make_regional_estimates(zap_labels(state_data_list$state_acs_data_cov))

state_acs_estimates_prepost_dind = make_regional_estimates(state_data_list$state_acs_data_prepost %>% 
                                                             filter(variable == "has_insurance") %>%
                                                             mutate(outcome = case_when(aca == FALSE ~  -outcome, 
                                                                                        TRUE ~ outcome)) %>% 
                                                             group_by(State_FIPS, age, state) %>% 
                                                             summarize(outcome = sum(outcome), pop = max(pop)) %>%
                                                             ungroup())


## Make State Robustness Estimates
bw_list = seq(3,10)
K_list = seq(1,10)
state_estimates_robust = make_regional_robustness_estimates(state_data_list$state_data, bw_list, K_list)
state_acs_estimates_robust = make_regional_robustness_estimates(state_data_list$state_acs_data, bw_list, K_list)

## Make Czone estimates
czone_estimates = make_regional_estimates(czone_data_list$czone_data)
czone_estimates_prepost = make_regional_estimates(czone_data_list$czone_data_prepost)
czone_acs_estimates = make_regional_estimates(czone_data_list$czone_acs_data)
czone_acs_estimates_prepost = make_regional_estimates(czone_data_list$czone_acs_data_prepost)
czone_acs_estimates_cov = make_regional_estimates(czone_data_list$czone_acs_data_cov)
czone_iv_estimates = make_czone_iv_estimates(czone_iv_data)
czone_iv_estimates_prepost = make_czone_iv_estimates(czone_iv_data_prepost)

czone_acs_estimates_prepost_dind = make_regional_estimates(czone_data_list$czone_acs_data_prepost %>% 
                                                             filter(variable == "has_insurance") %>%
                                                             mutate(outcome = case_when(aca == FALSE ~  -outcome, 
                                                                                        TRUE ~ outcome)) %>% 
                                                             group_by(czone, age) %>% 
                                                             summarize(outcome = sum(outcome), pop = max(pop)) %>%
                                                             ungroup())

## Make Czone Robustness Estimates
# bw_list = seq(3,10)
# K_list = seq(1,10)
# czone_estimates_robust = make_regional_robustness_estimates(czone_data_list$czone_data, bw_list, K_list)
# czone_acs_estimates_robust = make_regional_robustness_estimates(czone_data_list$czone_acs_data, bw_list, K_list)

## Make Shrinkage estimates
czone_estimates_shrink = shrink_czone_estimates(czone_estimates)
czone_estimates_prepost_shrink = shrink_czone_estimates(czone_estimates_prepost)
czone_acs_estimates_shrink = shrink_czone_estimates(czone_acs_estimates)
czone_acs_estimates_prepost_shrink = shrink_czone_estimates(czone_acs_estimates_prepost)

czone_acs_estimates_prepost_dind_shrink = shrink_czone_estimates(czone_acs_estimates_prepost_dind)
czone_acs_estimates_prepost_dind_shrink %>%
  write_csv("data/czone_acs_estimates_prepost_dind_shrink.csv")

czone_iv_estimates_shrink = shrink_czone_estimates(
  czone_iv_estimates %>%
    rename(tau = beta,
           se = beta_se,
           ci_lower = beta_ci_lower,
           ci_upper = beta_ci_upper) %>%
    mutate(lower_estimate = 0, upper_estimate = 0, 
           lower_estimate_se = 0, upper_estimate_se = 0,
           lower_estimate_bias = 0, upper_estimate_bias = 0)) %>%
  select(-lower_estimate, -upper_estimate, 
         -lower_estimate_se, -upper_estimate_se) %>%
  rename(beta = tau, beta_se = se, 
         beta_ci_lower = ci_lower,
         beta_ci_upper = ci_upper)

czone_iv_estimates_prepost_shrink = shrink_czone_estimates(
  czone_iv_estimates_prepost %>%
    rename(tau = beta,
           se = beta_se,
           ci_lower = beta_ci_lower,
           ci_upper = beta_ci_upper) %>%
    mutate(lower_estimate = 0, upper_estimate = 0, 
           lower_estimate_se = 0, upper_estimate_se = 0,
           lower_estimate_bias = 0, upper_estimate_bias = 0)) %>%
  select(-lower_estimate, -upper_estimate, 
         -lower_estimate_se, -upper_estimate_se) %>%
  rename(beta = tau, beta_se = se, 
         beta_ci_lower = ci_lower,
         beta_ci_upper = ci_upper)



czone_estimates_all = czone_estimates_prepost %>% 
  bind_rows(czone_estimates) %>%
  mutate(aca = case_when(aca == TRUE ~ "Post-ACA",
                         aca == FALSE ~ "Pre-ACA",
                         is.na(aca)   ~ "Full Sample"))
czone_acs_estimates_all = czone_acs_estimates_prepost %>% 
  bind_rows(czone_acs_estimates) %>%
  mutate(aca = case_when(aca == TRUE ~ "Post-ACA",
                         aca == FALSE ~ "Pre-ACA",
                         is.na(aca)   ~ "Full Sample"))
czone_iv_estimates_all = czone_iv_estimates_prepost %>% 
  bind_rows(czone_iv_estimates) %>%
  mutate(aca = case_when(aca == TRUE ~ "Post-ACA",
                         aca == FALSE ~ "Pre-ACA",
                         is.na(aca)   ~ "Full Sample"))

czone_estimates_all_shrink = czone_estimates_prepost_shrink %>% 
  bind_rows(czone_estimates_shrink) %>%
  mutate(aca = case_when(aca == TRUE ~ "Post-ACA",
                         aca == FALSE ~ "Pre-ACA",
                         is.na(aca)   ~ "Full Sample"))
czone_acs_estimates_all_shrink = czone_acs_estimates_prepost_shrink %>% 
  bind_rows(czone_acs_estimates_shrink) %>%
  mutate(aca = case_when(aca == TRUE ~ "Post-ACA",
                         aca == FALSE ~ "Pre-ACA",
                         is.na(aca)   ~ "Full Sample"))
czone_iv_estimates_all_shrink = czone_iv_estimates_prepost_shrink %>% 
  bind_rows(czone_iv_estimates_shrink) %>%
  mutate(aca = case_when(aca == TRUE ~ "Post-ACA",
                         aca == FALSE ~ "Pre-ACA",
                         is.na(aca)   ~ "Full Sample"))

# 
# czone_estimates_all_shrink %>% write_dta("data/czone_estimates_shrunk.dta")
# czone_estimates_all %>% write_dta("data/czone_estimates.dta")
# 
# czone_acs_estimates_all_shrink %>% write_dta("data/czone_acs_estimates_shrunk.dta")
# czone_acs_estimates_all %>% write_dta("data/czone_acs_estimates.dta")
# 
# czone_iv_estimates_shrink   %>% write_dta("data/czone_beta_estimates_shrunk.dta")
# czone_iv_estimates   %>% write_dta("data/czone_beta_estimates.dta")

### END ESTIMATION STEP
### SETUP CZ CHARACTERISTICS

czone_char = read_dta("data/ipums_2012_cz_parsed.dta") %>% mutate(inctot = log(inctot)) %>%
  select(-has_insurance)

hcris_vars = c("share_hosp_forprofit", "share_hosp_nonprofit", "share_hosp_public", 
               "share_hosp_teaching", "occupancy", "hosp_beds", "uncomp_charity_patients", 
               "pymt_charity_patients", "pymt_insured_charity", 
               "nonmdcr_uncomp_cost", "unreimb_uncomp_cost", "all_inpat_days")

chetty_vars = c("cs_educ_ba", "cs_born_foreign", "cs_frac_black", "cs_frac_hisp","puninsured2010",
                "cs_fam_wkidsinglemom", "median_house_value", "primcarevis_10",
                "poor_share", "hhinc00", "cs00_seg_inc_pov25",
                "amb_disch_per1000_10", "reimb_penroll_adj10")
czone_hcris = read_dta("raw/RAND_HCRIS_county/data/cz_hcris.dta") 
czone_hcris = czone_hcris %>% filter(year > 2011 & year < 2015) %>%
  group_by(czone) %>%
  select(all_of(hcris_vars), czone) %>%  
  summarize_all(mean) %>% ungroup()

variable_labels = read_csv("data/cov_label_names.csv")  %>% 
  filter(!is.na(cov_label))

czone_chetty = read_dta("data/health_ineq_online_table_10.dta") %>% 
  rename(czone = cz) %>% select(all_of(chetty_vars), czone)

cz_names = read_dta("data/health_ineq_online_table_10.dta") %>% 
  rename(czone = cz) %>% select(czone, czname, statename)

cbsa_HMI = read_csv("~/Dropbox/GPW/raw/HCCI/orig/HMI_2018-2019_Price_Use_Machine_Readable_Data.csv")
cbsa_fips_xw = read_dta("data/cbsa2fipsxw.dta") %>% filter(cbsacode != "") %>%
  mutate(cbsa = as.numeric(cbsacode))
cz_county_xwalk <- read_dta("/Users/psg24/Dropbox/GPW/data/cw_cty_czone.dta")  %>%
  rename(county_fips = cty_fips)
czone_HMI = cbsa_HMI %>% filter(file == "Overall" & measure == "price") %>% 
  select(year, cbsa, pct_natl_med) %>%
  left_join(cbsa_fips_xw) %>%
  mutate(county_fips = paste0(fipsstatecode, fipscountycode)) %>%
  select(county_fips, year, cbsa, pct_natl_med) %>%
  mutate(county_fips = as.numeric(county_fips)) %>%
  left_join(cz_county_xwalk) %>%
  group_by(czone) %>%
  summarize(pct_natl_med = mean(pct_natl_med))


pop_data = czone_data_list$czone_data %>% 
  filter(variable == "q_coll_12") %>% 
  group_by(czone)  %>%
  summarize(pop = sum(pop))
pop_data_64 = czone_data_list$czone_data %>% 
  filter(age == 64) %>% 
  filter(variable == "q_coll_12") %>% 
  group_by(czone)  %>%
  summarize(pop_64 = sum(pop))

pop_data_all = czone_data_list$czone_data_prepost %>% 
  filter(variable == "q_coll_12") %>% group_by(czone, aca)  %>%
  summarize(pop = sum(pop)) %>% bind_rows(pop_data) %>%
  mutate(aca = case_when(aca == TRUE ~ "Post-ACA",
                         aca == FALSE ~ "Pre-ACA",
                         is.na(aca)   ~ "Full Sample"))

pop_data_64_all = czone_data_list$czone_data_prepost %>% 
  filter(age == 64) %>% 
  filter(variable == "q_coll_12") %>% group_by(czone, aca)  %>%
  summarize(pop_64 = sum(pop)) %>% bind_rows(pop_data_64) %>%
  mutate(aca = case_when(aca == TRUE ~ "Post-ACA",
                         aca == FALSE ~ "Pre-ACA",
                         is.na(aca)   ~ "Full Sample"))

state_pop_data = pop_data %>% left_join(cz_names) %>% 
  inner_join(tibble(statename = state.name, state = state.abb)) %>%
  group_by(state) %>%
  summarize(pop = sum(pop))

czone_estimates_w_data = czone_estimates_all %>% 
  select(-ci_upper, -ci_lower, -lower_estimate_se, -upper_estimate_se)

insurance_cov = czone_acs_estimates_shrink %>%
  select(variable, czone, lower_estimate) %>%
  spread(variable, lower_estimate) 

cov_data =   czone_hcris %>% 
  left_join(czone_chetty) %>% 
  left_join(czone_HMI) %>% 
  left_join(czone_char) %>%
  left_join(insurance_cov, by=c("czone")) %>%
  select(-pop) %>%
  gather(cov_label, covariate, -czone, -all_inpat_days)

### END SETUP OF CZ CHRACTERISTICS

# plot_figures <- function(model_left, model_right, plot_data, fn, 
#                          range = NA, split_var) 

## Construct Variance Reduction Estimates:

### Variance reduction
czone_acs_estimates_var = czone_acs_estimates %>% 
  left_join(pop_data_64) %>%
  rename(pop = pop_64) %>%
  nest(data = c(czone, tau, se, ci_lower, ci_upper, 
                lower_estimate, upper_estimate, 
                lower_estimate_se, upper_estimate_se, 
                lower_estimate_bias, upper_estimate_bias,
                pop)) %>%
  mutate(estimates = map(data,construct_variance_ratio)) %>%
  mutate(phi = purrr::map_dbl(estimates, "phi"),
         phi_se = purrr::map_dbl(estimates, "phi_se"),
         phi_bias = purrr::map_dbl(estimates, "bias_phi"),
         phi_ci_lower = purrr::map_dbl(estimates, "ci_lower"),
         phi_ci_upper = purrr::map_dbl(estimates, "ci_upper"),
         phi_ci_lower_noadj = purrr::map_dbl(estimates, "ci_lower_noadj"),
         phi_ci_upper_noadj = purrr::map_dbl(estimates, "ci_upper_noadj")) %>%
  select(-estimates, -data)
    
czone_estimates_var = czone_estimates %>% 
  left_join(pop_data_64) %>%
  rename(pop = pop_64) %>%
  nest(data = c(czone, tau, se, ci_lower, ci_upper, 
                lower_estimate, upper_estimate, 
                lower_estimate_se, upper_estimate_se, 
                lower_estimate_bias, upper_estimate_bias,
                pop)) %>%
  mutate(estimates = map(data,construct_variance_ratio)) %>%
  mutate(phi = purrr::map_dbl(estimates, "phi"),
         phi_se = purrr::map_dbl(estimates, "phi_se"),
         phi_bias = purrr::map_dbl(estimates, "bias_phi"),
         phi_ci_lower = purrr::map_dbl(estimates, "ci_lower"),
         phi_ci_upper = purrr::map_dbl(estimates, "ci_upper"),
         phi_ci_lower_noadj = purrr::map_dbl(estimates, "ci_lower_noadj"),
         phi_ci_upper_noadj = purrr::map_dbl(estimates, "ci_upper_noadj")) %>%
  select(-estimates, -data)

state_acs_estimates_var = state_acs_estimates %>% 
  left_join(state_data_list$state_acs_data %>% 
              filter(variable == "has_insurance") %>%
              filter( age == 64) %>% ungroup() %>%
              select(state, pop)) %>%
  nest(data = c(state, State_FIPS, tau, se, ci_lower, ci_upper, 
                lower_estimate, upper_estimate, 
                lower_estimate_se, upper_estimate_se, 
                lower_estimate_bias, upper_estimate_bias,
                pop)) %>%
  mutate(estimates = map(data,construct_variance_ratio)) %>%
  mutate(phi = purrr::map_dbl(estimates, "phi"),
         phi_se = purrr::map_dbl(estimates, "phi_se"),
         phi_bias = purrr::map_dbl(estimates, "bias_phi"),
         phi_ci_lower = purrr::map_dbl(estimates, "ci_lower"),
         phi_ci_upper = purrr::map_dbl(estimates, "ci_upper"),
         phi_ci_lower_noadj = purrr::map_dbl(estimates, "ci_lower_noadj"),
         phi_ci_upper_noadj = purrr::map_dbl(estimates, "ci_upper_noadj")) %>%
  select(-estimates, -data)

state_acs_estimates_cov_var = state_acs_estimates_cov %>% 
  left_join(state_data_list$state_acs_data %>% 
              filter(variable == "has_insurance") %>%
              filter( age == 64) %>% ungroup() %>%
              select(state, pop)) %>%
  nest(data = c(state, State_FIPS, tau, se, ci_lower, ci_upper, 
                lower_estimate, upper_estimate, 
                lower_estimate_se, upper_estimate_se, 
                lower_estimate_bias, upper_estimate_bias,
                pop)) %>%
  mutate(estimates = map(data,construct_variance_ratio)) %>%
  mutate(phi = purrr::map_dbl(estimates, "phi"),
         phi_se = purrr::map_dbl(estimates, "phi_se"),
         phi_bias = purrr::map_dbl(estimates, "bias_phi"),
         phi_ci_lower = purrr::map_dbl(estimates, "ci_lower"),
         phi_ci_upper = purrr::map_dbl(estimates, "ci_upper"),
         phi_ci_lower_noadj = purrr::map_dbl(estimates, "ci_lower_noadj"),
         phi_ci_upper_noadj = purrr::map_dbl(estimates, "ci_upper_noadj")) %>%
  select(-estimates, -data)

state_estimates_var = state_estimates %>% 
  left_join(state_data_list$state_data %>% 
              filter(variable == "q_coll_12") %>%
              filter( age == 64) %>% ungroup() %>%
              select(state, pop)) %>%
  nest(data = c(state, tau, se, ci_lower, ci_upper, 
                lower_estimate, upper_estimate, 
                lower_estimate_se, upper_estimate_se, 
                lower_estimate_bias, upper_estimate_bias,
                pop)) %>%
  mutate(estimates = map(data,construct_variance_ratio)) %>%
  mutate(phi = purrr::map_dbl(estimates, "phi"),
         phi_se = purrr::map_dbl(estimates, "phi_se"),
         phi_bias = purrr::map_dbl(estimates, "bias_phi"),
         phi_ci_lower = purrr::map_dbl(estimates, "ci_lower"),
         phi_ci_upper = purrr::map_dbl(estimates, "ci_upper"),
         phi_ci_lower_noadj = purrr::map_dbl(estimates, "ci_lower_noadj"),
         phi_ci_upper_noadj = purrr::map_dbl(estimates, "ci_upper_noadj")) %>%
  select(-estimates, -data)

### Robustness check of Variance
state_estimates_var_robust = state_estimates_robust %>% unique() %>%
  filter(!is.na(se)) %>%
  left_join(state_data_list$state_data %>% 
              filter(variable == "q_coll_12") %>%
              filter( age == 64) %>% ungroup() %>%
              select(state, pop)) %>%
  nest(data = c(state, tau, se, ci_lower, ci_upper, 
                lower_estimate, upper_estimate, 
                lower_estimate_se, upper_estimate_se, 
                lower_estimate_bias, upper_estimate_bias,
                pop)) %>%
  mutate(estimates = map(data,construct_variance_ratio)) %>%
  mutate(phi = purrr::map_dbl(estimates, "phi"),
         phi_se = purrr::map_dbl(estimates, "phi_se"),
         phi_bias = purrr::map_dbl(estimates, "bias_phi"),
         phi_ci_lower = purrr::map_dbl(estimates, "ci_lower"),
         phi_ci_upper = purrr::map_dbl(estimates, "ci_upper"),
         phi_ci_lower_noadj = purrr::map_dbl(estimates, "ci_lower_noadj"),
         phi_ci_upper_noadj = purrr::map_dbl(estimates, "ci_upper_noadj")) %>%
  select(-estimates, -data)
state_acs_estimates_var_robust = state_acs_estimates_robust %>% unique() %>%
  filter(!is.na(se)) %>%
  left_join(state_data_list$state_acs_data %>% 
              filter(variable == "has_insurance") %>%
              filter( age == 64) %>% ungroup() %>%
              select(state, pop)) %>%
  nest(data = c(state, State_FIPS, tau, se, ci_lower, ci_upper, 
                lower_estimate, upper_estimate, 
                lower_estimate_se, upper_estimate_se, 
                lower_estimate_bias, upper_estimate_bias,
                pop)) %>%
  mutate(estimates = map(data,construct_variance_ratio)) %>%
  mutate(phi = purrr::map_dbl(estimates, "phi"),
         phi_se = purrr::map_dbl(estimates, "phi_se"),
         phi_bias = purrr::map_dbl(estimates, "bias_phi"),
         phi_ci_lower = purrr::map_dbl(estimates, "ci_lower"),
         phi_ci_upper = purrr::map_dbl(estimates, "ci_upper"),
         phi_ci_lower_noadj = purrr::map_dbl(estimates, "ci_lower_noadj"),
         phi_ci_upper_noadj = purrr::map_dbl(estimates, "ci_upper_noadj")) %>%
  select(-estimates, -data)

# 

# 
# ## Aca Variance Reduction
# state_estimates_var_aca = state_estimates_prepost %>% 
#   left_join(state_data_list$state_data %>% 
#               filter(variable == "q_coll_12") %>%
#               filter( age == 64) %>% ungroup() %>%
#               select(state, pop)) %>%
#   left_join(State_FIPS_Codes) %>%
#   left_join(tibble(state = c(as.character(state.abb), "DC"), 
#                    region = c(as.character(state.region), "Northeast"))) %>%
#   mutate(south = region == "South") %>%
#   select(-state_fips, -State_FIPS, -region) %>%
#   nest(data = c(state, tau, se, ci_lower, ci_upper, 
#                 lower_estimate, upper_estimate, 
#                 lower_estimate_se, upper_estimate_se, 
#                 lower_estimate_bias, upper_estimate_bias,
#                 pop)) %>%
#   mutate(estimates = map(data,construct_variance_ratio)) %>%
#   mutate(phi = purrr::map_dbl(estimates, "phi"),
#          phi_se = purrr::map_dbl(estimates, "phi_se"),
#          phi_bias = purrr::map_dbl(estimates, "bias_phi"),
#          phi_ci_lower = purrr::map_dbl(estimates, "ci_lower"),
#          phi_ci_upper = purrr::map_dbl(estimates, "ci_upper"),
#          phi_ci_lower_noadj = purrr::map_dbl(estimates, "ci_lower_noadj"),
#          phi_ci_upper_noadj = purrr::map_dbl(estimates, "ci_upper_noadj")) %>%
#   select(-estimates, -data)

## in the south
# State_FIPS_Codes <- read_dta("/Users/psg24/Dropbox/GPW/data/State_FIPS_Codes.dta") %>% 
#   rename(state = State)
# state_estimates_var_aca = state_estimates_prepost %>% 
#   left_join(state_data_list$state_data %>% 
#               filter(variable == "q_coll_12") %>%
#               filter( age == 64) %>% ungroup() %>%
#               select(state, pop)) %>%
#   left_join(State_FIPS_Codes) %>%
#   left_join(tibble(state = c(as.character(state.abb), "DC"), 
#                    region = c(as.character(state.region), "Northeast"))) %>%
#   mutate(south = region == "South") %>%
#   select(-state_fips, -State_FIPS, -region) %>%
#   nest(data = c(state, tau, se, ci_lower, ci_upper, 
#                 lower_estimate, upper_estimate, 
#                 lower_estimate_se, upper_estimate_se, 
#                 lower_estimate_bias, upper_estimate_bias,
#                 pop)) %>%
#   mutate(estimates = map(data,construct_variance_ratio)) %>%
#   mutate(phi = purrr::map_dbl(estimates, "phi"),
#          phi_se = purrr::map_dbl(estimates, "phi_se"),
#          phi_bias = purrr::map_dbl(estimates, "bias_phi"),
#          phi_ci_lower = purrr::map_dbl(estimates, "ci_lower"),
#          phi_ci_upper = purrr::map_dbl(estimates, "ci_upper"),
#          phi_ci_lower_noadj = purrr::map_dbl(estimates, "ci_lower_noadj"),
#          phi_ci_upper_noadj = purrr::map_dbl(estimates, "ci_upper_noadj")) %>%
#   select(-estimates, -data)
# 
# 
# ### This is a cool variance result i think for Figure 5
# state_estimates_prepost %>% 
#   left_join(state_data_list$state_data %>% 
#               filter(variable == "q_coll_12") %>%
#               filter( age == 64) %>% ungroup() %>%
#               select(state, pop)) %>%
#   left_join(State_FIPS_Codes) %>%
#   left_join(tibble(state = c(as.character(state.abb), "DC"), 
#                    region = c(as.character(state.region), "Northeast"))) %>%
#   mutate(south = region == "South") %>%
#   select(-state_fips, -State_FIPS, -region) %>% 
#   group_by(south, aca, variable) %>% 
#   filter(!is.na(lower_estimate_se) & !is.na(lower_estimate) & lower_estimate_se > 0 ) %>% 
#   summarize(variance_lower_est = weighted.var(lower_estimate, 1/lower_estimate_se, na.rm = TRUE), 
#             variance_upper_est = weighted.var(upper_estimate, 1/upper_estimate_se, na.rm = TRUE)) %>% 
#   filter(variable == "q_coll_12")

### PLOT FIGURE 1
varname = "q_coll_12"
g = make_fig1(national_estimates,
          ccp_data$ccp_data_collapsed, 
          state_estimates %>% filter(state != "DC"),
          state_data_list$state_data %>% filter(state != "DC"),
          varname)
paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                            varname, "$")
g + 
  annotate("text" , x = 75, y = 325, 
             label=paste_vals$paste_val, size = 4, 
             hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 250, 
             label=paste_vals$paste_val2, size = 4, 
             hjust = 1, vjust = 1)
ggsave("graphs/insights_fig1_q_coll_12.pdf", width = 6, height = 4)

### CZone level for main outcome:
varname = "q_coll_12"
g = make_fig1(national_estimates,
              ccp_data$ccp_data_collapsed , 
              czone_estimates %>% mutate(state=as.character(czone)),
              czone_data_list$czone_data %>% mutate(state=as.character(czone)),
              varname)
paste_vals = make_paste_vals(national_estimates, czone_estimates_var,
                             varname, "$")
g + 
  annotate("text" , x = 75, y = 1000, 
           label=paste_vals$paste_val, size = 4, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 750, 
           label=paste_vals$paste_val2, size = 4, 
           hjust = 1, vjust = 1) +
  ylim(0,1000)
ggsave("graphs/insights_fig1_q_coll_12_cz.pdf", width = 6, height = 4)

varname = "new_bpt"
g = make_fig1(national_estimates,
          ccp_data$ccp_data_collapsed, 
          state_estimates %>% filter(state != "DC"),
          state_data_list$state_data %>% filter(state != "DC"),
          varname)
paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                             varname, "%")
g + 
  annotate("text" , x = 75, y = 0.3, 
           label=paste_vals$paste_val, size = 4, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 0.24, 
           label=paste_vals$paste_val2, size = 4, 
           hjust = 1, vjust = 1)
ggsave("graphs/insights_fig1_new_bpt.pdf", width = 6, height = 4)

varname = "avg_riskscore"
g = make_fig1(national_estimates,
          ccp_data$ccp_data_collapsed, 
          state_estimates %>% filter(state != "DC"),
          state_data_list$state_data %>% filter(state != "DC"),
          varname)
paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                             varname, "")
g + 
  annotate("text" , x = 75,  y = 720,
           label=paste_vals$paste_val, size = 4, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 700, 
           label=paste_vals$paste_val2, size = 4, 
           hjust = 1, vjust = 1)
ggsave("graphs/insights_fig1_avg_riskscore.pdf", width = 6, height = 4)


varname = "has_insurance"
g = make_fig1(national_acs_estimates,
          acs_puma_data$acs_puma_data_collapsed %>% 
            gather(variable, outcome, -age) %>%
            filter(age != 65) %>%
            zap_labels(), 
          state_acs_estimates %>% filter(state != "DC"),
          state_data_list$state_acs_data %>%
            filter(age != 65) %>%
            zap_labels() %>% filter(state != "DC"),
          varname)
paste_val = make_paste_vals(national_acs_estimates, 
                            state_acs_estimates_var,
                            varname)
g + 
  annotate("text" , x = 75, y = 0.95, 
             label=paste_val$paste_val, size = 4, 
             hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 0.91, 
           label=paste_val$paste_val2, size = 4, 
           hjust = 1, vjust = 1)
ggsave("graphs/insights_fig1_has_insurance.pdf", width = 6, height = 4)

varname = "employed"
g =make_fig1(national_acs_estimates_cov,
          acs_puma_data$acs_puma_data_cov_collapsed %>% 
            gather(variable, outcome, -age) %>%
            filter(age != 65) %>%
            zap_labels(), 
          state_acs_estimates_cov %>% filter(state != "DC"),
          state_data_list$state_acs_data_cov %>%
            filter(age != 65) %>%
            zap_labels() %>% filter(state != "DC"),
          varname)
paste_val = make_paste_vals(national_acs_estimates_cov, 
                            state_acs_estimates_cov_var,
                            varname)
g + 
  annotate("text" , x = 75, y = 0.75, 
           label=paste_val$paste_val, size = 4, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 0.65, 
           label=paste_val$paste_val2, size = 4, 
           hjust = 1, vjust = 1)
ggsave("graphs/insights_fig1_employed.pdf", width = 6, height = 4)

varname = "inctot"
g =make_fig1(national_acs_estimates_cov,
          acs_puma_data$acs_puma_data_cov_collapsed %>% 
            gather(variable, outcome, -age) %>%
            filter(age != 65) %>%
            zap_labels(), 
          state_acs_estimates_cov %>% filter(state != "DC"),
          state_data_list$state_acs_data_cov %>%
            filter(age != 65) %>%
            zap_labels() %>% filter(state != "DC"),
          varname)
paste_val = make_paste_vals(national_acs_estimates_cov, 
                            state_acs_estimates_cov_var,
                            varname)
g + 
  annotate("text" , x = 75, y = 70000, 
           label=paste_val$paste_val, size = 4, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 62500, 
           label=paste_val$paste_val2, size = 4, 
           hjust = 1, vjust = 1)
ggsave("graphs/insights_fig1_inctot.pdf", width = 6, height = 4)

### Appendix Figure 1

variable_names = c("share_1_500_collect", "share_501_1000_collect",
                   "share_1001_2500_collect", "share_2501_5k_collect",
                   "share_5k_10k_collect", "share_10kplus_collect")
collect_height1 = c(0.08, 0.03, 0.03, 0.0125, 0.006, 0.004)
collect_height2 = c(0.065, 0.0225, 0.02, 0.01, 0.0045, 0.0025)
for (i in 1:length(variable_names)) {
  varname = variable_names[i]
  g=make_fig1(national_estimates,
              ccp_data$ccp_data_collapsed, 
              state_estimates %>% filter(state != "DC"),
              state_data_list$state_data %>% filter(state != "DC"),
              varname)
  paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                               varname, "")
  g + 
    annotate("text" , x = 75,  y = collect_height1[i],
             label=paste_vals$paste_val, size = 4, 
             hjust = 1, vjust = 1) +
    annotate("text" , x = 75, y = collect_height2[i], 
             label=paste_vals$paste_val2, size = 4, 
             hjust = 1, vjust = 1)
  ggsave(paste0("graphs/insights_fig1_", variable_names[i], ".pdf"), width = 6, height = 4)
}
### Appendix Figure 2

varname = "new_fc"
g=make_fig1(national_estimates,
            ccp_data$ccp_data_collapsed, 
            state_estimates %>% filter(state != "DC"),
            state_data_list$state_data %>% filter(state != "DC"),
            varname)
paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                             varname, "%")
g + 
  annotate("text" , x = 75,  y = 0.3,
           label=paste_vals$paste_val, size = 4, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 0.24, 
           label=paste_vals$paste_val2, size = 4, 
           hjust = 1, vjust = 1)
ggsave("graphs/insights_fig1_new_fc.pdf", width = 6, height = 4)

varname = "balance_ttl_delinq"
g = make_fig1(national_estimates,
              ccp_data$ccp_data_collapsed, 
              state_estimates %>% filter(state != "DC"),
              state_data_list$state_data %>% filter(state != "DC"),
              varname)
paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                             varname, "$")
g + 
  annotate("text" , x = 75,  y = 9500,
           label=paste_vals$paste_val, size = 4, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 7500, 
           label=paste_vals$paste_val2, size = 4, 
           hjust = 1, vjust = 1)

ggsave("graphs/insights_fig1_balance_ttl_delinq.pdf", width = 6, height = 4)


varname = "balance_mort_delinq"
g = make_fig1(national_estimates,
              ccp_data$ccp_data_collapsed, 
              state_estimates %>% filter(state != "DC"),
              state_data_list$state_data %>% filter(state != "DC"),
              varname)
paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                             varname, "$")
g + 
  annotate("text" , x = 75,  y = 7500,
           label=paste_vals$paste_val, size = 4, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 6250, 
           label=paste_vals$paste_val2, size = 4, 
           hjust = 1, vjust = 1)

ggsave("graphs/insights_fig1_balance_mort_delinq.pdf", width = 6, height = 4)


varname = "balance_cc_delinq"
g = make_fig1(national_estimates,
              ccp_data$ccp_data_collapsed, 
              state_estimates %>% filter(state != "DC"),
              state_data_list$state_data %>% filter(state != "DC"),
              varname)
paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                             varname, "$")
g + 
  annotate("text" , x = 75,  y = 700,
           label=paste_vals$paste_val, size = 4, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = 600, 
           label=paste_vals$paste_val2, size = 4, 
           hjust = 1, vjust = 1)

ggsave("graphs/insights_fig1_balance_cc_delinq.pdf", width = 6, height = 4)


varname = "balance_cc_delinq_scale"
g = make_fig1(national_estimates,
              ccp_data$ccp_data_collapsed, 
              state_estimates %>% filter(state != "DC"),
              state_data_list$state_data %>% filter(state != "DC"),
              varname)
paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                             varname, "")
g + 
  annotate("text" , x = 75,  y = .14,
           label=paste_vals$paste_val, size = 4, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = .12, 
           label=paste_vals$paste_val2, size = 4, 
           hjust = 1, vjust = 1)

ggsave("graphs/insights_fig1_balance_cc_delinq_scale.pdf", width = 6, height = 4)


varname = "balance_mort_delinq_scale"
g = make_fig1(national_estimates,
              ccp_data$ccp_data_collapsed, 
              state_estimates %>% filter(state != "DC"),
              state_data_list$state_data %>% filter(state != "DC"),
              varname)
paste_vals = make_paste_vals(national_estimates, state_estimates_var,
                             varname, "")
g + 
  annotate("text" , x = 75,  y = .1,
           label=paste_vals$paste_val, size = 4, 
           hjust = 1, vjust = 1) +
  annotate("text" , x = 75, y = .08, 
           label=paste_vals$paste_val2, size = 4, 
           hjust = 1, vjust = 1)

ggsave("graphs/insights_fig1_balance_mort_delinq_scale.pdf", width = 6, height = 4)

### Appendix Figure 3
variable_names = c("own_dwelling",  "married", "uhrswork", "incss")
y_height1 = c(0.94, 0.78, 30, 10000 )
y_height2 = c(0.89, 0.74, 25, 7500)
for (i in 1:length(variable_names)) {
  varname = variable_names[i]
  g=make_fig1(national_acs_estimates_cov,
              acs_puma_data$acs_puma_data_cov_collapsed %>% 
                gather(variable, outcome, -age) %>%
                filter(age != 65) %>%
                zap_labels(), 
              state_acs_estimates_cov %>% filter(state != "DC"),
              state_data_list$state_acs_data_cov %>%
                filter(age != 65) %>%
                zap_labels() %>% filter(state != "DC"),
              varname)
  paste_vals = make_paste_vals(national_acs_estimates_cov, 
                              state_acs_estimates_cov_var,
                              varname, "")
  g + 
    annotate("text" , x = 75,  y = y_height1[i],
             label=paste_vals$paste_val, size = 4, 
             hjust = 1, vjust = 1) +
    annotate("text" , x = 75, y = y_height2[i], 
             label=paste_vals$paste_val2, size = 4, 
             hjust = 1, vjust = 1)
  ggsave(paste0("graphs/insights_fig1_", variable_names[i], ".pdf"), width = 6, height = 4)
}


### Appendix Figure 4
variable_names = c("inctot", "employed", "own_dwelling",  "married", "uhrswork", "incss")

for (varname in variable_names) {
  ggplot(data = state_acs_estimates_cov %>% filter(variable == varname) %>%
           filter(state != "DC") %>%
           mutate(significant = ci_lower > 0 | ci_upper < 0)) +
    geom_pointrange(aes(x = reorder(state, -tau), ymin = ci_lower, ymax=ci_upper, y = tau, color=significant)) +
    coord_flip() +
    geom_hline(yintercept = 0, linetype="solid", color = "black") +
    scale_color_manual(values = c(dblue, dred)) +
    theme_classic() +
    labs(y = "",
         x = "",
         color = "Sig. Coef") +
    theme(text = element_text(size=11),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)
    ) 
  ggsave(paste0("graphs/insights_coef_smoothness_", varname, "_state.pdf"),  width = 6, height = 6)
}


### Appendix Figure 5
variable_names = c("inctot", "employed", "own_dwelling",  "married", "uhrswork", "incss")

for (varname in variable_names) {
  share_sig = czone_acs_estimates_cov %>% filter(variable == varname) %>%
    mutate(significant = ci_lower > 0 | ci_upper < 0,
           norm = tau / se) %>%
    summarize(share = mean(significant)) %>% pull(share)
  ggplot(data = czone_acs_estimates_cov %>% filter(variable == varname) %>%
           mutate(significant = ci_lower > 0 | ci_upper < 0,
                  norm = tau / se)) +
    geom_histogram(aes(x =  tau, fill=significant)) +
    geom_hline(yintercept = 0, linetype="solid", color = "black") +
    scale_fill_manual(values = c(dblue, dred)) +
    theme_classic() +
    labs(y = "",
         x = "Estimated Effect",
         fill = "Sig. Coef") +
    theme(text = element_text(size=24),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)
    ) +
    annotate("text", x = -Inf, y = Inf, vjust=1, hjust=0,
             label = paste0("Share Sig.: ", round(share_sig, digits=3)),
             size = 6)
  ggsave(paste0("graphs/insights_hist_smoothness_", varname, "_czone.pdf"),  width = 6, height = 4)
}

### Appendix Figures 6-10
varlist = c("q_coll_12", "avg_riskscore", "new_bpt", 
            "balance_ttl_delinq", "balance_mort_delinq", "balance_cc_delinq",
            "balance_ttl_delinq_scale", "new_fc")
varname = "has_insurance"
ggplot(data = national_acs_estimates_robust %>% filter(variable == varname) %>%
         unique() %>% filter(K_scale == 4)) +
  geom_pointrange(aes(x =  bw, ymin = ci_lower, ymax = ci_upper, y = tau)) +
  geom_hline(yintercept = 0, linetype="solid", color = "black") +
  theme_classic() +
  labs(y = "",
       x = "Bandwidth",
       fill = "Sig. Coef") +
  theme(text = element_text(size=24))
ggsave(paste0("graphs/insights_robust_bw_", varname, ".pdf"),  width = 6, height = 4)
ggplot(data = national_acs_estimates_robust %>% filter(variable == varname) %>%
         unique() %>% filter(bw == 5)) +
  geom_pointrange(aes(x =  K_scale, ymin = ci_lower, ymax = ci_upper, y = tau)) +
  geom_hline(yintercept = 0, linetype="solid", color = "black") +
  theme_classic() +
  labs(y = "",
       x = "Bound Scaling Factor",
       fill = "Sig. Coef") +
  theme(text = element_text(size=24))
ggsave(paste0("graphs/insights_robust_Kscale_", varname, ".pdf"),  width = 6, height = 4)
ggplot(data = state_acs_estimates_var_robust %>% filter(variable == varname) %>%
         unique() %>% filter(K_scale == 4)) +
  geom_pointrange(aes(x =  bw, ymin = phi_ci_lower, ymax = phi_ci_upper, y = phi)) +
  geom_hline(yintercept = 0, linetype="solid", color = "black") +
  theme_classic() +
  labs(y = "",
       x = "Bandwidth",
       fill = "Sig. Coef") +
  theme(text = element_text(size=24))
ggsave(paste0("graphs/insights_robust_bw_", varname, "_varreduction.pdf"),  width = 6, height = 4)
ggplot(data = state_acs_estimates_var_robust %>% filter(variable == varname) %>%
         unique() %>% filter(bw == 5)) +
  geom_pointrange(aes(x =  K_scale, ymin = phi_ci_lower, ymax = phi_ci_upper, y = phi)) +
  geom_hline(yintercept = 0, linetype="solid", color = "black") +
  theme_classic() +
  labs(y = "",
       x = "Bound Scaling Factor",
       fill = "Sig. Coef") +
  theme(text = element_text(size=24))
ggsave(paste0("graphs/insights_robust_Kscale_", varname, "_varreduction.pdf"),  width = 6, height = 4)
for (varname  in varlist) {
  ggplot(data = national_estimates_robust %>% filter(variable == varname) %>%
           unique() %>% filter(K_scale == 4)) +
    geom_pointrange(aes(x =  bw, ymin = ci_lower, ymax = ci_upper, y = tau)) +
    geom_hline(yintercept = 0, linetype="solid", color = "black") +
    theme_classic() +
    labs(y = "",
         x = "Bandwidth",
         fill = "Sig. Coef") +
    theme(text = element_text(size=24))
  ggsave(paste0("graphs/insights_robust_bw_", varname, ".pdf"),  width = 6, height = 4)
  ggplot(data = national_estimates_robust %>% filter(variable == varname) %>%
           unique() %>% filter(bw == 5)) +
    geom_pointrange(aes(x =  K_scale, ymin = ci_lower, ymax = ci_upper, y = tau)) +
    geom_hline(yintercept = 0, linetype="solid", color = "black") +
    theme_classic() +
    labs(y = "",
         x = "Bound Scaling Factor",
         fill = "Sig. Coef") +
    theme(text = element_text(size=24))
  ggsave(paste0("graphs/insights_robust_Kscale_", varname, ".pdf"),  width = 6, height = 4)
  ggplot(data = state_estimates_var_robust %>% filter(variable == varname) %>%
           unique() %>% filter(K_scale == 4)) +
    geom_pointrange(aes(x =  bw, ymin = phi_ci_lower, ymax = phi_ci_upper, y = phi)) +
    geom_hline(yintercept = 0, linetype="solid", color = "black") +
    theme_classic() +
    labs(y = "",
         x = "Bandwidth",
         fill = "Sig. Coef") +
    theme(text = element_text(size=24))
  ggsave(paste0("graphs/insights_robust_bw_", varname, "_varreduction.pdf"),  width = 6, height = 4)
  ggplot(data = state_estimates_var_robust %>% filter(variable == varname) %>%
           unique() %>% filter(bw == 5)) +
    geom_pointrange(aes(x =  K_scale, ymin = phi_ci_lower, ymax = phi_ci_upper, y = phi)) +
    geom_hline(yintercept = 0, linetype="solid", color = "black") +
    theme_classic() +
    labs(y = "",
         x = "Bound Scaling Factor",
         fill = "Sig. Coef") +
    theme(text = element_text(size=24))
  ggsave(paste0("graphs/insights_robust_Kscale_", varname, "_varreduction.pdf"),  width = 6, height = 4)
}

## Appendix Figure (OLS)
varlist = c("q_coll_12", "new_bpt", "avg_riskscore", "balance_ttl_delinq_scale")
ols_data = state_data_list$state_data %>% filter(age < 65) %>%
  filter(variable %in% varlist ) %>%
  group_by(state, variable) %>% summarize(outcome = weighted.mean(outcome, pop)) %>%
  left_join(state_data_list$state_acs_data %>% filter(age < 65 & variable == "has_insurance") %>%
              group_by(state) %>% summarize(has_insurance = weighted.mean(outcome, pop), 
                                            pop = sum(pop)))

for (varname in varlist) {
  ypos = Inf
  vpos = 1
  if(varname == "avg_riskscore") {
    ypos = -Inf
    vpos = 0
  }
  m = summary(lm(data = ols_data %>% filter(variable == varname) %>% mutate(uninsurance = 1-has_insurance), 
     formula = outcome ~ uninsurance, weight= pop), robust = TRUE)
  b = signif(m$coefficients[2,1], digits = 3)
  se = signif(m$coefficients[2,2], digits = 3)
  r2 = signif(m$r.squared, digits = 3)
  ggplot(data = ols_data %>% filter(variable == varname), aes(y = outcome, x = 1-has_insurance, label = state)) +
    geom_text(color = dblue, size = 6) +
    geom_smooth(method="lm", se=FALSE, color = "black", linetype = "dashed") +
    theme_classic() +
    theme(text = element_text(size=24)) +
    labs(x = "Uninsurance Rate", 
         y ="") +
    annotate("text", x = -Inf, y = ypos, hjust=0, vjust=vpos, size = 5,
             label = paste0("Slope: ", b, "\n", "SE:", se, "\n", "R-squared:", r2))
  ggsave(paste0("graphs/insights_ols_appendix_", varname, ".pdf"),  width = 6, height = 4)
}

## Appendix Figure 11

plot_data = state_estimates %>% left_join(state_acs_estimates %>% filter(variable == "has_insurance") %>%
                                select(state, insurance_tau = tau)) %>%
  filter(variable == "q_coll_12") %>% left_join(state_pop_data)

m = summary(lm(data = plot_data %>% mutate(outcome = -tau), 
               formula = outcome ~ insurance_tau, weight= pop), robust = TRUE)
b = signif(m$coefficients[2,1], digits = 3)
se = signif(m$coefficients[2,2], digits = 3)
r2 = signif(m$r.squared, digits = 3)
ggplot(data = plot_data , aes(y = -tau, x = insurance_tau, label = state)) +
  geom_text(color = dblue, size = 6) +
  geom_smooth(method="lm", se=FALSE, color = "black", linetype = "dashed") +
  theme_classic() +
  theme(text = element_text(size=24)) +
  labs(x = "Estimated Effect on Insurance Rate", 
       y ="") +
  annotate("text", x = -Inf, y = ypos, hjust=0, vjust=vpos, size = 6,
           label = paste0("Slope: ", b, "\n", "SE:", se, "\n", "R-squared:", r2))
ggsave(paste0("graphs/insights_effect_on_effect_appendix_state.pdf"),  width = 8, height = 6)

plot_data = czone_estimates %>% left_join(czone_acs_estimates %>% filter(variable == "has_insurance") %>%
                                            select(czone, insurance_tau = tau)) %>%
  filter(variable == "q_coll_12") %>% left_join(pop_data) %>%
  filter(!is.na(insurance_tau)) %>% mutate(outcome = -tau, weight = 1/se) 
m = summary(lm(data = plot_data , 
               formula = outcome ~ insurance_tau, weight= pop), robust = TRUE)

a = signif(m$coefficients[1,1], digits = 3)
b = signif(m$coefficients[2,1], digits = 3)
se = signif(m$coefficients[2,2], digits = 3)
r2 = signif(m$r.squared, digits = 3)
bin_plot = binsreg(y = plot_data$outcome, x = plot_data$insurance_tau, weights = plot_data$pop)
bin_plot$bins_plot +
  geom_point(data = plot_data , aes(y = -tau, x = insurance_tau), color = "gray", size = 1, alpha = 0.4) +
  geom_abline(slope=b, intercept = a, linetype = "dashed") +
  theme_classic() +
  theme(text = element_text(size=24)) +
  labs(x = "Estimated Effect on Insurance Rate", 
       y ="") +
  annotate("text", x = -Inf, y = ypos, hjust=0, vjust=vpos, size = 6,
           label = paste0("Slope: ", b, "\n", "SE:", se, "\n", "R-squared:", r2)) +
  ylim(0, 200) + xlim(0, 0.2)
ggsave(paste0("graphs/insights_effect_on_effect_appendix_cz.pdf"),  width = 8, height = 6)

### Figure 2
czone_estimates_shrink %>% filter(variable == "q_coll_12") %>%
  write_csv("data/czone_estimates_fig2.csv")
# Then Run main_analysis_maps.do

### Figure 3
## Correlates with tau
# First set up regression data
ui_effect_data = czone_acs_estimates_all %>% 
  filter(variable == "has_insurance") %>% 
  select(tau_ui = tau, lower_estimate, upper_estimate, czone, aca) %>%
  mutate(lower_estimate = 1-lower_estimate, upper_estimate = 1 - upper_estimate)  %>%
  gather(side, value_ui, -czone, -tau_ui, -aca)

pop_norm_vars = c("hosp_beds", 
                  "uncomp_charity_patients", 
                  "unreimb_uncomp_cost",
                  "pymt_charity_patients",
                  "nonmdcr_uncomp_cost")
reg_data2 = czone_estimates_w_data %>% 
  bind_rows(czone_acs_estimates_all %>% select(czone_estimates_w_data %>% colnames()) %>%
              filter(variable == "has_insurance")) %>%
  gather(side, value, -czone, -se,-tau, -variable, -aca) %>%
  left_join(pop_data)  %>%
  left_join(cov_data) %>%
  left_join(ui_effect_data) %>%
  left_join(czone_iv_estimates_all) %>%
  select(-tau_ui, -value_ui) %>%
  mutate(covariate = case_when(cov_label %in% pop_norm_vars  ~ covariate/all_inpat_days,
                               TRUE ~ covariate)) %>%
  filter(cov_label %in% variable_labels$cov_label) %>%
  group_by(side, cov_label, variable, aca) %>%
  mutate(covariate = (covariate - weighted.mean(covariate, pop, na.rm = TRUE)) / sqrt(weighted.var(covariate, pop, na.rm = TRUE))) %>%
  filter(!is.na(covariate)) %>%
  ungroup() %>%
  spread(cov_label, covariate) %>%
  filter(variable == "q_coll_12") %>%
  filter(se != 0) %>% 
  left_join(czone_estimates_all_shrink %>%
              select(aca, czone, variable, tau_shrink = tau)) 

rhs_all = c("black", "gt_hs_educ", "has_insurance", "has_medicaid", 
            "hosp_beds", "inctot", "median_house_value", "occupancy", 
            "physical_disab", "poor_share", "pymt_charity_patients", 
            "reimb_penroll_adj10", "share_hosp_forprofit", 
            "share_hosp_teaching", "uncomp_charity_patients")
varlist = paste(rhs_all, collapse="+")
fm = as.formula(paste("tau_tilde ~ ", varlist))
## Full Sample
tau_full_hat = construct_lasso_predict(reg_data2 %>% 
                          filter(side == "lower_estimate" & 
                                   aca == "Full Sample"), 
                        fm)
tau_full_hat %>% left_join(czone_estimates_all_shrink) %>% filter(variable == "q_coll_12") %>% 
  mutate(tau_lasso = tau_lasso + tau_mean) %>%
  select(czone, tau_shrink = tau, aca, variable, everything()) %>%
  write_csv("data/czone_lasso_predicted_full.csv", na= "")

## Pre-ACA
tau_pre_hat = construct_lasso_predict(reg_data2 %>% 
                                         filter(side == "lower_estimate" & 
                                                  aca == "Pre-ACA"), 
                                       fm)
tau_pre_hat %>% right_join(czone_estimates_all_shrink %>% 
                             filter(variable == "q_coll_12" & 
                                      aca == "Pre-ACA")) %>% 
  filter(variable == "q_coll_12") %>% 
  mutate(tau_lasso = tau_lasso + tau_mean) %>%
  select(czone, tau_shrink = tau, aca, variable, everything()) %>%
  write_csv("data/czone_lasso_predicted_pre.csv", na= "")
## Post-ACA
tau_post_hat = construct_lasso_predict(reg_data2 %>% 
                                        filter(side == "lower_estimate" & 
                                                 aca == "Post-ACA"), 
                                      fm)
tau_post_hat %>% right_join(czone_estimates_all_shrink %>% 
                              filter(variable == "q_coll_12" & 
                                       aca == "Post-ACA") ) %>%
  filter(variable == "q_coll_12") %>% 
  mutate(tau_lasso = tau_lasso + tau_mean) %>%
  select(czone, tau_shrink = tau, aca, variable, everything()) %>%
  write_csv("data/czone_lasso_predicted_post.csv", na= "")

### Same exercise for insurance:
reg_data2_health = czone_estimates_w_data %>% 
  bind_rows(czone_acs_estimates_all %>% select(czone_estimates_w_data %>% colnames()) %>%
              filter(variable == "has_insurance")) %>%
  gather(side, value, -czone, -se,-tau, -variable, -aca) %>%
  left_join(pop_data)  %>%
  left_join(cov_data) %>%
  left_join(ui_effect_data) %>%
  left_join(czone_iv_estimates_all) %>%
  select(-tau_ui, -value_ui) %>%
  mutate(covariate = case_when(cov_label %in% pop_norm_vars  ~ covariate/all_inpat_days,
                               TRUE ~ covariate)) %>%
  filter(cov_label %in% variable_labels$cov_label) %>%
  group_by(side, cov_label, variable, aca) %>%
  mutate(covariate = (covariate - weighted.mean(covariate, pop, na.rm = TRUE)) / sqrt(weighted.var(covariate, pop, na.rm = TRUE))) %>%
  filter(!is.na(covariate)) %>%
  filter(cov_label !=  "has_insurance") %>%
  ungroup() %>%
  spread(cov_label, covariate) %>%
  filter(variable == "has_insurance") %>%
  filter(se != 0) %>% 
  left_join(czone_estimates_all_shrink %>%
              select(aca, czone, variable, tau_shrink = tau)) 

rhs_all = c("black", "gt_hs_educ",  "has_medicaid", 
            "hosp_beds", "inctot", "median_house_value", "occupancy", 
            "physical_disab", "poor_share", "pymt_charity_patients", 
            "reimb_penroll_adj10", "share_hosp_forprofit", 
            "share_hosp_teaching", "uncomp_charity_patients")
varlist = paste(rhs_all, collapse="+")
fm = as.formula(paste("tau_tilde ~ ", varlist))
## Full Sample
tauh_full_hat = construct_lasso_predict(reg_data2_health %>% 
                                         filter(side == "lower_estimate" & 
                                                  aca == "Full Sample"), 
                                       fm)
tauh_full_hat %>% right_join(czone_acs_estimates_all_shrink) %>% 
  filter(variable == "has_insurance") %>% 
  mutate(tau_lasso = tau_lasso + tau_mean) %>%
  select(czone, tau_shrink = tau, aca, variable, everything()) %>%
  write_csv("data/czone_acs_lasso_predicted_full.csv", na= "")
## Pre-ACA
tauh_pre_hat = construct_lasso_predict(reg_data2_health %>% 
                                        filter(side == "lower_estimate" & 
                                                 aca == "Pre-ACA"), 
                                      fm)
tauh_pre_hat %>% right_join(czone_acs_estimates_all_shrink) %>% 
  filter(variable == "has_insurance") %>% 
  mutate(tau_lasso = tau_lasso + tau_mean) %>%
  select(czone, tau_shrink = tau, aca, variable, everything()) %>%
  write_csv("data/czone_acs_lasso_predicted_pre.csv", na= "")
## Post-ACA
tauh_post_hat = construct_lasso_predict(reg_data2_health %>% 
                                         filter(side == "lower_estimate" & 
                                                  aca == "Post-ACA"), 
                                       fm)
tauh_post_hat %>% right_join(czone_acs_estimates_all_shrink) %>% 
  filter(variable == "has_insurance") %>% 
  mutate(tau_lasso = tau_lasso + tau_mean) %>%
  select(czone, tau_shrink = tau, aca, variable, everything()) %>%
  write_csv("data/czone_acs_lasso_predicted_post.csv", na= "")


rhs_all = c("black", "gt_hs_educ", "has_insurance", "has_medicaid", 
            "hosp_beds", "inctot", "median_house_value", "occupancy", 
            "physical_disab", "poor_share", "pymt_charity_patients", 
            "reimb_penroll_adj10", "share_hosp_forprofit", 
            "share_hosp_teaching", "uncomp_charity_patients")
varlist = paste(rhs_all, collapse="+")
fm = as.formula(paste("beta_tilde ~ ", varlist))
## Full Sample
beta_full_hat = construct_lasso_predict_iv(reg_data2 %>% 
                                         filter(side == "lower_estimate" & 
                                                  aca == "Full Sample"), 
                                       fm)
beta_full_hat %>% left_join(czone_iv_estimates_all_shrink) %>% 
  filter(variable == "q_coll_12") %>% 
  mutate(beta_lasso = beta_lasso + beta_mean) %>%
  select(czone, beta_shrink = beta, aca, variable, everything()) %>%
  write_csv("data/czone_lasso_predicted_full_beta.csv", na= "")

## Pre-ACA
beta_pre_hat = construct_lasso_predict_iv(reg_data2 %>% 
                                             filter(side == "lower_estimate" & 
                                                      aca == "Pre-ACA") %>%
                                            filter(!is.na(beta)), 
                                           fm)
beta_pre_hat %>% left_join(czone_iv_estimates_all_shrink) %>% 
  filter(variable == "q_coll_12") %>% 
  mutate(beta_lasso = beta_lasso + beta_mean) %>%
  write_csv("data/czone_lasso_predicted_pre_beta.csv", na= "")
## Post-ACA
beta_post_hat = construct_lasso_predict_iv(reg_data2 %>% 
                                            filter(side == "lower_estimate" & 
                                                     aca == "Post-ACA") %>%
                                            filter(!is.na(beta)), 
                                          fm)
beta_post_hat %>% left_join(czone_iv_estimates_all_shrink) %>% 
  filter(variable == "q_coll_12") %>% 
  mutate(beta_lasso = beta_lasso + beta_mean) %>%
  write_csv("data/czone_lasso_predicted_post_beta.csv", na= "")

# Then run main_analysis_maps.do
## Identify Post-Lasso variables
reg_data2_lasso = reg_data2   %>% 
  filter(side == "lower_estimate" & 
           aca == "Full Sample") %>%
  mutate(sampling_var = (se^2)) %>% 
  mutate(sampling_weight = 1/sampling_var) %>%
  mutate(sampling_var_iv = (beta_se^2)) %>% 
  mutate(sampling_weight_iv = 1/sampling_var_iv) %>%
  mutate(tau_mean = weighted.mean(tau , w=sampling_weight)) %>%
  mutate(tau_tilde = tau - tau_mean) %>% 
  mutate(beta_mean = weighted.mean(beta , w=sampling_weight_iv)) %>%
  mutate(beta_tilde = beta - beta_mean) 

varlist = paste(rhs_all, collapse="+")
fm = as.formula(paste("tau_tilde ~ ", varlist))
predictor_matrix <- model.matrix(fm, data = reg_data2_lasso)[, -1]
lasso_output_full_tilde = 
  lasso_predict(fm, predictor_matrix, 
                reg_data2_lasso$tau_tilde, 
                reg_data2_lasso$sampling_weight)
nonzero_vars_full = lasso_output_full_tilde$LASSO_coefficients$term
nonzero_vars_full = nonzero_vars_full[2:length(nonzero_vars_full)]

varlist = paste(rhs_all, collapse="+")
fm_iv = as.formula(paste("beta_tilde ~ ", varlist))
predictor_matrix <- model.matrix(fm_iv, data = reg_data2_lasso)[, -1]
lasso_output_full_tilde_iv = 
  lasso_predict(fm_iv, predictor_matrix, 
                reg_data2_lasso$beta_tilde, 
                reg_data2_lasso$sampling_weight_iv)
nonzero_vars_full_iv = lasso_output_full_tilde$LASSO_coefficients$term
nonzero_vars_full_iv = nonzero_vars_full[2:length(nonzero_vars_full)]


## Make Regression Comparisons for Figure 3
## Setup Data
### Tau Univariate
reg_data_uni = czone_estimates_w_data %>% 
  gather(side, value, -czone, -tau, -se, -variable, -aca) %>%
  left_join(pop_data_all)  %>%
  left_join(cov_data) %>%
  left_join(ui_effect_data) %>%
  left_join(czone_iv_estimates_all) %>%
  mutate(covariate = case_when(cov_label %in% pop_norm_vars  ~ covariate/all_inpat_days,
                               TRUE ~ covariate)) %>%
  filter(cov_label %in% variable_labels$cov_label) %>%
  group_by(side, cov_label, variable, aca) %>%
  mutate(covariate = (covariate - weighted.mean(covariate, pop, na.rm = TRUE)) / sqrt(weighted.var(covariate, pop, na.rm = TRUE))) %>%
  filter(!is.na(covariate)) %>%
  ungroup() %>%
  left_join(cz_names) %>%
  left_join(tibble(statename = state.name,
                   region = state.region,
                   division = state.division))


### Tau Multivariate + Post-LASSO
reg_data_mv =  czone_estimates_w_data %>% 
  select(lower_estimate, upper_estimate,  czone, tau, variable, aca)  %>%
  gather(side, value, -czone, -tau, -variable, -aca) %>%
  left_join(pop_data)  %>%
  left_join(cov_data) %>%
  left_join(ui_effect_data) %>%
  left_join(czone_iv_estimates_all) %>%
  select(-tau_ui, -value_ui) %>%
  mutate(covariate = case_when(cov_label %in% pop_norm_vars  ~ covariate/all_inpat_days,
                               TRUE ~ covariate)) %>%
  filter(cov_label %in% variable_labels$cov_label) %>%
  group_by(side, cov_label, variable, aca) %>%
  mutate(covariate = (covariate - weighted.mean(covariate, pop, na.rm = TRUE)) / sqrt(weighted.var(covariate, pop, na.rm = TRUE))) %>%
  filter(!is.na(covariate)) %>%
  ungroup() %>%
  spread(cov_label, covariate) %>%
  filter(variable == "q_coll_12") %>%
  left_join(cz_names) %>%
  left_join(tibble(statename = state.name,
                   region = state.region,
                   division = state.division))

### Tau Univariate
uni_results = make_uni_results(reg_data_uni, 
                               national_estimates,
                               national_iv_estimates,
                               variable_labels)

### TEMPORARY FOR POTENTIAL FIG
temp = make_uni_results(reg_data_uni %>% mutate(tau = value)  %>% filter(side == "lower_estimate"), 
                 national_estimates,
                 national_iv_estimates,
                 variable_labels)
temp2 = make_uni_results(reg_data_uni %>% mutate(tau = value)  %>% filter(side == "upper_estimate") %>%
                           mutate(side = "lower_estimate"), 
                         national_estimates,
                         national_iv_estimates,
                         variable_labels)
temp$estimates_uni_tau %>% select(cov_label, 
                                  tau_estimate_lower = tau_estimate) %>%
  left_join(temp2$estimates_uni_tau %>% select(cov_label, 
                                           tau_estimate_upper = tau_estimate))
temp3 = reg_data_uni %>% filter(aca == "Full Sample" & 
                                  cov_label == "black" & 
                                  variable == "q_coll_12" &
                                  side %in% c("lower_estimate", "upper_estimate")) %>%
  select(value, side, czone, covariate)
binsreg(y = temp3$value, x = temp3$covariate, by = temp3$side)
temp3 = reg_data_uni %>% filter(aca == "Full Sample" & 
                                  cov_label == "physical_disab" & 
                                  variable == "q_coll_12" &
                                  side %in% c("lower_estimate", "upper_estimate")) %>%
  select(value, side, czone, covariate)
binsreg(y = temp3$value, x = temp3$covariate, by = temp3$side)
temp3 = reg_data_uni %>% filter(aca == "Full Sample" & 
                                  cov_label == "share_hosp_forprofit" & 
                                  variable == "q_coll_12" &
                                  side %in% c("lower_estimate", "upper_estimate")) %>%
  select(value, side, czone, covariate)
binsreg(y = temp3$value, x = temp3$covariate, by = temp3$side)

temp3 = reg_data_uni %>% filter(aca == "Full Sample" & 
                                  cov_label == "gt_hs_educ" & 
                                  variable == "q_coll_12" &
                                  side %in% c("lower_estimate", "upper_estimate")) %>%
  select(value, side, czone, covariate)
binsreg(y = temp3$value, x = temp3$covariate, by = temp3$side)


### Tau multivariate for Figure 3
mv_results = make_mv_results(reg_data_mv, rhs_all,
                              national_estimates, national_iv_estimates, 
                              variable_labels)

## Lasso MV
mv_lasso_results = make_mv_results(reg_data_mv, nonzero_vars_full,
                             national_estimates, national_iv_estimates, 
                             variable_labels)
## Tau Lasso MV for Figure 3
estimate_panel = construct_full_panel(uni_results,mv_results,
                     mv_lasso_results)

## Make Figure 3
panellist = c("A. Area-level demographic characteristics",
              "B. Healthcare market characteristics")
filesavelist = c("panelA", "panelB")
for (i in seq(1,2)) {
ggplot(data = estimate_panel$estimates_full_panel %>% 
         filter(panel == panellist[i])) +
  geom_pointrange(aes(y = -estimate2, 
                      ymin=-estimate2-1.96*std.error2, 
                      ymax=-estimate2+1.96*std.error2,
                      x = cov_name,
                      color = estimate_type,
                      shape = estimate_type), 
                  position = position_dodge2(width=0.5, reverse = TRUE)) +
  scale_y_continuous(labels=scales::number_format(accuracy = 0.1)) +
  scale_color_manual(values = c(dred, dblue)) +
  coord_flip() + theme_classic() +
  geom_hline(aes(yintercept=0))+
  facet_wrap(~group, ncol = 3) +
  labs(y = "Impact of 1 SD Change in Covariate", x = "", color = "", shape = "") +
  theme(text = element_text(size=18),
        legend.position="bottom")
ggsave(paste0("graphs/multivariate_q_coll_12_tau_beta_cz_fullonly_", filesavelist[i], ".pdf"),
       width = 10, height =5)
}
estimate_panel$estimates_full_panel %>% 
  select(estimate, std.error, cov_name, group, estimate_type) %>% 
  mutate(estimate = signif(estimate, digits = 3),
         std.error = paste0("(", as.character(signif(std.error, digits = 3)), ")")) %>% 
  pivot_wider(names_from = group, values_from = c(estimate, std.error)) %>%
  select(cov_name, estimate_type, 
         estimate_Bivariate, std.error_Bivariate, 
         estimate_Multivariate, std.error_Multivariate,
         `estimate_Post-Lasso`, `std.error_Post-Lasso` ) %>%
  kable(format = "latex", booktabs = TRUE, digits = 2) %>%
  cat(file = "tables/appendix_table_for_fig3.tex")


## Figure 3 for Appendix with Fixed Effects
# Univariate
estimates_uni_tau_fe = make_uni_results_fe_panel(reg_data_uni, national_estimates,
                                                 national_iv_estimates, variable_labels)


# Multivariate
estimates_mv_tau_fe = make_mv_results_fe_panel(reg_data_mv, rhs_all, national_estimates)

# Post-Lasso
estimates_mv_tau_lasso_fe = make_mv_results_fe_panel(reg_data_mv, nonzero_vars_full, national_estimates)

estimates_tau_full_panel_fe = construct_full_panel_fe(estimates_uni_tau_fe,
                                                      estimates_mv_tau_fe, 
                                                      estimates_mv_tau_lasso_fe,
                                                      national_estimates)
panellist = c("A. Area-level demographic characteristics",
              "B. Healthcare market characteristics")
filesavelist = c("panelA", "panelB")
for (i in seq(1,2)) {
  ggplot(data = estimates_tau_full_panel_fe %>%
           filter(panel == panellist[i])) +
    geom_pointrange(aes(y = -estimate2,
                        ymin=-estimate2-1.96*std.error2,
                        ymax=-estimate2+1.96*std.error2,
                        x = cov_name,
                        color = fe,
                        shape = fe),
                    position = position_dodge2(width=1, reverse = TRUE)) +
    coord_flip() + theme_classic() +
    geom_hline(aes(yintercept=0))+
    facet_wrap(~group, ncol = 3) +
    labs(y = "Impact of 1 SD Change in Covariate", x = "", color = "Fixed Effects",
         shape = "Fixed Effects") +
    theme(text = element_text(size=14),
          legend.position="bottom")
  ggsave(paste0("graphs/multivariate_q_coll_12_tau_cz_fullonly_", filesavelist[i], "_fe.pdf"),
         width = 10, height =5)
}

## Figure 5 Scatterplot
State_FIPS_Codes <- read_dta("/Users/psg24/Dropbox/GPW/data/State_FIPS_Codes.dta") %>% 
  rename(state = State) %>%
  mutate(state_fips = as.numeric(State_FIPS))


scatter_data2 = czone_acs_estimates_prepost_shrink  %>%
  filter(variable == "has_insurance") %>%
  select(czone, aca, lower_estimate) %>%
  spread(aca, lower_estimate) %>%
  left_join(cz_county_xwalk) %>%
  mutate(state_fips = floor(county_fips/1000)) %>%
  select(-county_fips) %>%
  rename(preaca = `FALSE`,
         postaca = `TRUE`) %>%
  left_join(State_FIPS_Codes) %>%
  left_join(tibble(state = c(as.character(state.abb), "DC"), 
                   region = c(as.character(state.region), "Northeast"))) %>%
  select(-state_fips, -state, -State_FIPS) %>%
  unique() %>%
  left_join(pop_data_64) %>%
  mutate(south = region == "South") %>%
  group_by(czone) %>%
  mutate(south2 = max(south)) %>%
  select(-south, -region) %>%
  unique() %>%
  mutate(south = factor(south2, levels = c(1,0),
                        labels = c( "South","All Others"))) 
scatter_data = tau_post_hat %>% bind_rows(tau_full_hat) %>%
  bind_rows(tau_pre_hat) %>%
  select(czone, aca, tau_ch) %>%
  spread(aca, tau_ch) %>%
  left_join(tau_post_hat %>% select(czone, rmse)) %>%
  left_join(cz_county_xwalk) %>%
  mutate(state_fips = floor(county_fips/1000)) %>%
  select(-county_fips) %>%
  unique() %>%
  left_join(State_FIPS_Codes) %>%
  left_join(tibble(state = c(as.character(state.abb), "DC"), 
                   region = c(as.character(state.region), "Northeast"))) %>%
  select(-state_fips, -state, -State_FIPS) %>%
  unique() %>%
  left_join(pop_data_64) %>%
  mutate(south = region == "South") %>%
    group_by(czone) %>%
    mutate(south2 = max(south)) %>%
    select(-south, -region) %>%
  unique() %>%
  mutate(south = factor(south2, levels = c(1,0),
                        labels = c( "South","All Others"))) 
scatter_data_beta = beta_post_hat %>% bind_rows(beta_full_hat) %>%
  bind_rows(beta_pre_hat) %>%
  select(czone, aca, beta_ch) %>%
  spread(aca, beta_ch) %>%
  left_join(cz_county_xwalk) %>%
  mutate(state_fips = floor(county_fips/1000)) %>%
  select(-county_fips) %>%
  unique() %>%
  left_join(State_FIPS_Codes) %>%
  left_join(tibble(state = c(as.character(state.abb), "DC"), 
                   region = c(as.character(state.region), "Northeast"))) %>%
  select(-state_fips, -state, -State_FIPS) %>%
  unique() %>%
  left_join(pop_data_64) %>%
  mutate(south = region == "South") %>%
  group_by(czone) %>%
  mutate(south2 = max(south)) %>%
  select(-south, -region) %>%
  unique() %>%
  mutate(south = factor(south2, levels = c(1,0),
                        labels = c( "South","All Others"))) 

scatter_data_tauh = tauh_post_hat %>% bind_rows(tauh_full_hat) %>%
  bind_rows(tauh_pre_hat) %>%
  select(czone, aca, tauh_ch =tau_ch) %>%
  spread(aca, tauh_ch) %>%
  left_join(cz_county_xwalk) %>%
  mutate(state_fips = floor(county_fips/1000)) %>%
  select(-county_fips) %>%
  unique() %>%
  left_join(State_FIPS_Codes) %>%
  left_join(tibble(state = c(as.character(state.abb), "DC"), 
                   region = c(as.character(state.region), "Northeast"))) %>%
  select(-state_fips, -state, -State_FIPS) %>%
  unique() %>%
  left_join(pop_data_64) %>%
  mutate(south = region == "South") %>%
  group_by(czone) %>%
  mutate(south2 = max(south)) %>%
  select(-south, -region) %>%
  unique() %>%
  mutate(south = factor(south2, levels = c(1,0),
                        labels = c( "South","All Others"))) 


model_fit_scatter = lm(data = scatter_data, 
                       formula =`Post-ACA` ~ 0  +`Pre-ACA`:south + `Pre-ACA`, 
                       weight = 1/rmse)
model_fit_scatter_nocons = lm(data = scatter_data %>% mutate(`Pre-ACA` = -`Pre-ACA`,
                                                      `Post-ACA` = -`Post-ACA`), 
                       formula =`Post-ACA` ~  -1 +`Pre-ACA`:south +`Pre-ACA`)
model_fit_scatter = lm(data = scatter_data %>% mutate(`Pre-ACA` = -`Pre-ACA`,
                                                      `Post-ACA` = -`Post-ACA`), 
                       formula =`Post-ACA` ~  south +`Pre-ACA`:south +`Pre-ACA`)

slope_south = model_fit_scatter_nocons$coefficients[1] + model_fit_scatter$coefficients[2]
slope_nonsouth = model_fit_scatter_nocons$coefficients[1] 
intercept_south = 0
intercept_nonsouth = 0
slope_nonsouth_print = format(slope_nonsouth, 
                              nsmall = 2, digits =2)
slope_south_print = format(slope_south, 
                           nsmall = 2, digits =2)
slope_nonsouth = model_fit_scatter$coefficients[3] + model_fit_scatter$coefficients[4]
slope_south = model_fit_scatter$coefficients[3] 
slope_nonsouth_print = format(slope_nonsouth, 
                     nsmall = 2, digits =2)
slope_south_print = format(slope_south, 
                        nsmall = 2, digits =2)
intercept_south = model_fit_scatter$coefficients[1] 
intercept_nonsouth = model_fit_scatter$coefficients[1]  + model_fit_scatter$coefficients[2] 
ggplot(data = scatter_data) + 
  geom_point(aes(x = -`Pre-ACA`, 
                 y =  -`Post-ACA`,
                 color = south,
                 shape = south), alpha = 0.5) +
  geom_abline(slope=1, intercept=0) +
  geom_abline(slope= slope_south, 
              intercept = intercept_south, color = dred) +
  geom_abline(slope= slope_nonsouth,
              intercept = intercept_nonsouth, color = dblue) +
  scale_color_manual(values = c(dred, dblue))+ theme_minimal() +
  theme(legend.position = c(0.2, 0.8),
        text = element_text(size=18),
        plot.title.position = "plot") +
  labs(color = "",
       shape = "",
       x = "Pre-ACA Effects",
       y = "",
       subtitle = "Post-ACA Effects") +
  xlim(-5, 110) + ylim(-5,110) +
  annotate("text", x = 93, y = 58, 
           label = paste("Slope = ",slope_south_print), size = 5, 
           hjust = 0, vjust = 1,
           color = dred) +
  annotate("text", x = 93, y = 25, 
           label = paste("Slope = ",slope_nonsouth_print),size = 5, 
           hjust = 0, vjust = 1,
           color = dblue)
ggsave("graphs/scatter_pre_post_effect.pdf", width = 8, height=8, unit = "in")

beta_means = scatter_data_beta %>% group_by(south) %>% summarize(beta_post = mean(-`Post-ACA`, na.rm = TRUE, w = pop_64),
                                                                 beta_pre = mean(-`Pre-ACA`, na.rm = TRUE, w = pop_64))
tauh_means = scatter_data_tauh %>% group_by(south) %>% summarize(tauh_post = mean(`Post-ACA`, na.rm = TRUE, w = pop_64),
                                                                 tauh_pre = mean(`Pre-ACA`, na.rm = TRUE, w = pop_64))
tau_means = scatter_data %>% group_by(south) %>% summarize(tau_post = mean(-`Post-ACA`, na.rm = TRUE, w = pop_64),
                                               tau_pre = mean(-`Pre-ACA`, na.rm = TRUE, w = pop_64))
covariance_data = scatter_data_beta %>% select(czone, south, pop_64, beta_post = `Post-ACA`, beta_pre = `Pre-ACA`) %>%
  left_join(scatter_data_tauh %>% select(czone, south, pop_64, tauh_post = `Post-ACA`, tauh_pre = `Pre-ACA`)) %>%
  filter(!is.na(beta_post ))  %>% ungroup()

cov_south = cov(covariance_data %>% 
                  filter(south == "South") %>% 
                  select(beta_post, tauh_post, beta_pre, tauh_pre)) 
cov_south_post = cov_south[1,2]
cov_south_pre = cov_south[3,4]
cov_north = cov(covariance_data %>% 
                  filter(south == "All Others") %>% 
                  select(beta_post, tauh_post, beta_pre, tauh_pre), 
                use="complete.obs") 
cov_north_post = cov_north[1,2]
cov_north_pre = cov_north[3,4]
kappa = tau_means$tau_pre[1]/tau_means$tau_pre[2]

testing_diff_post = (beta_means$beta_post[1] - beta_means$beta_post[2])*tauh_means$tauh_post[1] +
  beta_means$beta_post[2]*(tauh_means$tauh_post[1] - tauh_means$tauh_post[2]) 
piece1_post = (beta_means$beta_post[1] - beta_means$beta_post[2])*tauh_means$tauh_post[1] / ( tau_means$tau_post[1] -  tau_means$tau_post[2])
piece2_post = beta_means$beta_post[2]*(tauh_means$tauh_post[1] - tauh_means$tauh_post[2])  / ( tau_means$tau_post[1] -  tau_means$tau_post[2])

testing_diff_pre = (beta_means$beta_pre[1] - beta_means$beta_pre[2])*tauh_means$tauh_pre[1] +
  beta_means$beta_pre[2]*(tauh_means$tauh_pre[1] - tauh_means$tauh_pre[2]) 
piece1_pre = (beta_means$beta_pre[1] - beta_means$beta_pre[2])*tauh_means$tauh_pre[1] / ( tau_means$tau_pre[1] -  tau_means$tau_pre[2])
piece2_pre = beta_means$beta_pre[2]*(tauh_means$tauh_pre[1] - tauh_means$tauh_pre[2])  / ( tau_means$tau_pre[1] -  tau_means$tau_pre[2])

testing_diff_north = (tau_means$tau_post[2] - tau_means$tau_pre[2] )/ tau_means$tau_pre[2] 
piece1_north = (beta_means$beta_post[2] - beta_means$beta_pre[2])*tauh_means$tauh_post[2] / tau_means$tau_pre[2] 
piece2_north = beta_means$beta_pre[2]*(tauh_means$tauh_post[2] - tauh_means$tauh_pre[2]) / tau_means$tau_pre[2] 

testing_diff_south = (tau_means$tau_post[1] - tau_means$tau_pre[1] )/ tau_means$tau_pre[1]
piece1_south = (beta_means$beta_post[1] - beta_means$beta_pre[1])*tauh_means$tauh_post[1] / tau_means$tau_pre[1]
piece2_south = beta_means$beta_pre[1]*(tauh_means$tauh_post[1] - tauh_means$tauh_pre[1]) / tau_means$tau_pre[1]

decomp_table = tau_means %>% mutate(diff = (tau_post - tau_pre)/tau_pre) %>% 
  bind_cols(tibble(A = c(piece1_south, piece1_north), 
                   B = c(piece2_south, piece2_north))) %>%
  mutate(C = diff - A - B) %>% 
  mutate(tau_diff = tau_post - tau_pre) %>% 
  left_join(tauh_means) %>% mutate(tauh_diff = tauh_post - tauh_pre) %>%
  left_join(beta_means)  %>% mutate(beta_diff = beta_post - beta_pre) %>%
  bind_cols(tibble(cov_pre = c(cov_south_pre, cov_north_pre), 
                   cov_post = c(cov_south_post, cov_north_post)) %>%
              mutate(cov_diff = cov_post - cov_pre) ) %>%
  select(south, tau_pre, tau_post, tau_diff, tauh_pre, tauh_post, tauh_diff,
         beta_pre, beta_post, beta_diff, cov_pre, cov_post, cov_diff, diff, B, A, C) %>% 
  gather(stat , val, -south) %>% group_by(stat) %>%
  spread(south, val) %>% mutate(diff = `All Others` - South) %>% 
  gather(south, val, -stat) %>%
  spread(stat, val)

decomp_table$south = factor(decomp_table$south, levels = c("South", "All Others", "diff"),
                            labels = c("South", "All Others", "Difference")) 

decomp_table %>% 
  arrange(south) %>% 
  select(south, tau_pre, tau_post, tau_diff, tauh_pre, tauh_post, tauh_diff,
         beta_pre, beta_post, beta_diff, cov_pre, cov_post, cov_diff, diff, B, A, C) %>% 
  kable(format = "latex" , booktabs = TRUE, digits = 2)
decomp_data = tau_means %>% mutate(diff = (tau_post - tau_pre)/tau_pre) %>% 
  bind_cols(tibble(A = c(piece1_south, piece1_north), B = c(piece2_south, piece2_north))) %>%
  mutate(C = diff - A - B) %>%
  gather(chunk, value, -tau_post, -tau_pre, -south) 
decomp_data$chunk <- factor(decomp_data$chunk, levels = rev(c("diff", "A", "B", "C")))

bracketsGrob <- function(...){
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}
library(grid)
library(pBrackets) 
b1 <- bracketsGrob(0.35, .85, 0.05, .85, h=0.025, lwd=2, col="black")
val1 = signif(decomp_data$value[2] - decomp_data$value[1], digits = 2)
b2 <- bracketsGrob(0.225, 0.61, 0.15, 0.61, h=0.025,  lwd=2, col="black")
val2 = signif(decomp_data$value[6] - decomp_data$value[5], digits =2)
b3 <- bracketsGrob(0.82, 0.39, 0.955, 0.39, h=0.025,  lwd=2, col="black")
val3 = signif(decomp_data$value[4] - decomp_data$value[3], digits = 2)
b4 <- bracketsGrob(0.83, 0.14, 0.735, 0.14, h=0.025,  lwd=2, col="black")
val4 = signif(decomp_data$value[8] - decomp_data$value[7], digits = 2)
#%>% bind_rows(tibble(chunk = "D", value = 0, south = factor("All Others")))
ggplot() +
  geom_col(data = decomp_data %>% mutate(overall = chunk != "diff") , aes(y = value, x = chunk,
                                   fill = south, color = south, alpha = south), 
           position=position_dodge2(preserve = 'single')) +
  scale_fill_manual(values = c(dred, dblue))+ 
  scale_alpha_manual(values = c(1, 0.5))+
  scale_color_manual(values = c(dred, dblue))+ 
  theme_minimal() +
  theme(legend.position = "none") +
  #scale_x_discrete(labels =c(expression(eta[1]), expression(eta[2]), expression(eta[3]),expression(eta))) +
  scale_x_discrete(labels =rev(c(expression(paste(Delta, " Per Capita Effect")),
                              expression(paste(Delta, " Insurance Effect")), 
                             expression(paste(Delta, " Per Newly-Insured Effect")),
                             expression(paste(Delta, " Covariance of effects")))),
                   limits = rev(c("diff",  "B", "A", "C"))) +
  geom_hline(yintercept=0) +
  labs(x = "",
       y = "Percentage Change in Per Capita Effect \n from Pre-ACA to Post-ACA ") +
  theme(text=element_text(size = 18)) +
  coord_flip() +
  geom_text(data = decomp_data %>% mutate(textlocal = -0.01) %>% filter(chunk == "A"),
            aes(x = chunk, y=textlocal,  label = south), 
            position=position_dodge2(width=1), hjust=1, size = 5 ) +
  geom_text(data = decomp_data %>% mutate(textlocal = 0.01) %>% filter(chunk != "A"),
            aes(x = chunk, y=textlocal,  label = south), 
            position=position_dodge2(width=1), hjust=0, size = 5 ) +
  annotation_custom(b1)+
  annotation_custom(b2)+
  annotation_custom(b3)+
  annotation_custom(b4) +
  geom_text(data = decomp_data %>%
              left_join(tibble(diff = val1, B = val2, A = val3, C =val4) %>% 
                          gather(chunk, val) %>%
                          bind_cols(tibble(loc = c(-.39, -.385, 0.085, -0.01)))) %>%
              mutate(val2 = case_when(chunk == "A" & south == "All Others" ~ val,
                                       chunk != "A" & south == "South" ~ val)),
            aes(x = chunk, label = val2, y = loc, group=south), 
            position=position_dodge2(width=1), hjust=1, size = 5 ) +
  geom_vline(xintercept = 3.5, size=1)
ggsave("graphs/decomp_graph.pdf", width = 8, height=8, unit = "in")


### Appendix Figure Tau vs Beta
plot_data = czone_estimates %>% left_join(czone_iv_estimates %>% filter(variable == "q_coll_12") %>%
                                            select(czone, beta)) %>%
  filter(variable == "q_coll_12") %>% left_join(pop_data)  %>%
  filter(!is.na(beta) & !is.na(tau))
m = summary(lm(data = plot_data , 
               formula = beta ~ tau, weight= pop), robust = TRUE)

a = signif(m$coefficients[1,1], digits = 3)
b = signif(m$coefficients[2,1], digits = 3)
se = signif(m$coefficients[2,2], digits = 3)
r2 = signif(m$r.squared, digits = 3)
bin_plot = binsreg(x = -plot_data$tau, y = -plot_data$beta, nbins = 20, weights = plot_data$pop)
bin_plot$bins_plot +
  geom_point(data = plot_data , aes(y = beta, x = tau), color = "gray", size = 1, alpha = 0.3) +
  geom_abline(slope=b, intercept = a, linetype = "dashed") +
  theme_classic() +
  theme(text = element_text(size=22)) +
  labs(x = "Estimated Effect on Per Capita Debt Collections", 
       y ="") +
  annotate("text", x = -Inf, y = ypos, hjust=0, vjust=vpos, size = 6,
           label = paste0("Slope: ", b, "\n", "SE:", se, "\n", "R-squared:", r2)) +
  ylim(-1000,2000)  + xlim(-100,200)
ggsave(paste0("graphs/insights_tau_on_beta_appendix_cz.pdf"),  width = 8.5, height = 6)


### Appendix Table 1

varlist1 = c("q_coll_12", "avg_riskscore", "new_bpt", "balance_ttl_delinq",
             "balance_mort_delinq", "balance_cc_delinq", "new_fc", "balance_mort_delinq_scale",
             "balance_cc_delinq_scale")
varlist2 = c("has_insurance", "employed", "inctot")
varlabel1 = c("Debt in collections", "Credit score", "Bankruptcy (pp)", "Total debt past due",
              "Mortgage debt past due", "Credit card debt past due", "Foreclosure", 
              "Share of mortgage debt past due", "Share of cc debt past due")
varlabel2 = c("Share with any coverage", "Share employed", "Income")

varlist3 = c("has_insurance", "q_coll_12", "avg_riskscore", "new_bpt", "employed", "inctot", "balance_ttl_delinq",
             "balance_mort_delinq", "balance_cc_delinq", "new_fc", "balance_mort_delinq_scale",
             "balance_cc_delinq_scale")
ccp_estimates_naive = ccp_data$ccp_data_collapsed_county %>% ungroup() %>%
  nest(data = c(age, census_code, state, State_FIPS, county_fips, outcome, pop)) %>%
  filter(variable %in% varlist1) %>%
  mutate(estimate = future_map(data, estimate_model_naive)) %>% 
  select(-data) %>%
  mutate(tau_linear_robust      = map_dbl(estimate, "tau_linear_robust"),
         se_linear_robust       = map_dbl(estimate, "se_linear_robust"),
         cilower_linear_robust  = map_dbl(estimate, "cilower_linear_robust"),
         ciupper_linear_robust  = map_dbl(estimate, "ciupper_linear_robust"),
         tau_quad_robust      = map_dbl(estimate, "tau_quad_robust"),
         se_quad_robust       = map_dbl(estimate, "se_quad_robust"),
         cilower_quad_robust  = map_dbl(estimate, "cilower_quad_robust"),
         ciupper_quad_robust  = map_dbl(estimate, "ciupper_quad_robust"),
         tau_cubic_robust      = map_dbl(estimate, "tau_cubic_robust"),
         se_cubic_robust       = map_dbl(estimate, "se_cubic_robust"),
         cilower_cubic_robust  = map_dbl(estimate, "cilower_cubic_robust"),
         ciupper_cubic_robust  = map_dbl(estimate, "ciupper_cubic_robust"),
         tau_linear_cluster      = map_dbl(estimate, "tau_linear_cluster"),
         se_linear_cluster       = map_dbl(estimate, "se_linear_cluster"),
         cilower_linear_cluster  = map_dbl(estimate, "cilower_linear_cluster"),
         ciupper_linear_cluster  = map_dbl(estimate, "ciupper_linear_cluster"),
         tau_quad_cluster      = map_dbl(estimate, "tau_quad_cluster"),
         se_quad_cluster       = map_dbl(estimate, "se_quad_cluster"),
         cilower_quad_cluster  = map_dbl(estimate, "cilower_quad_cluster"),
         ciupper_quad_cluster  = map_dbl(estimate, "ciupper_quad_cluster"),
         tau_cubic_cluster      = map_dbl(estimate, "tau_cubic_cluster"),
         se_cubic_cluster       = map_dbl(estimate, "se_cubic_cluster"),
         cilower_cubic_cluster  = map_dbl(estimate, "cilower_cubic_cluster"),
         ciupper_cubic_cluster  = map_dbl(estimate, "ciupper_cubic_cluster"),
         tau_linear_rdrobust    = map_dbl(estimate, "tau_linear_rdrobust"),
         se_linear_rdrobust     = map_dbl(estimate, "se_linear_rdrobust"),
         cilower_linear_rdrobust      = map_dbl(estimate, "cilower_linear_rdrobust"),
         ciupper_linear_rdrobust      = map_dbl(estimate, "ciupper_linear_rdrobust")
  )    %>% 
  left_join(tibble(variable = varlist1, var_label = varlabel1)) %>%
  select(-estimate) %>%
  left_join(national_estimates %>% 
              select(tau_main_main = tau, se_main_main = se, 
                     cilower_main_main = ci_lower, ciupper_main_main = ci_upper, variable)) %>%
  gather(est_name, estimate, -variable, -var_label) %>%
  separate(est_name, into = c("estimator", "model", "model2")) %>% 
  unite(model_name, model, model2) %>%
  spread(estimator, estimate ) %>%
  rename(ci_lower = cilower, ci_upper = ciupper) %>%
  arrange(factor(variable, levels = varlist1))

acs_estimates_naive = bind_rows(czone_data_list$czone_acs_data, 
                                czone_data_list$czone_acs_data_cov) %>% ungroup() %>%
  nest(data = c(age, czone, outcome, pop)) %>%
  filter(variable %in% varlist2) %>%
  mutate(estimate = future_map(data, estimate_model_naive)) %>% 
  select(-data) %>%
  mutate(tau_linear_robust      = map_dbl(estimate, "tau_linear_robust"),
         se_linear_robust       = map_dbl(estimate, "se_linear_robust"),
         cilower_linear_robust  = map_dbl(estimate, "cilower_linear_robust"),
         ciupper_linear_robust  = map_dbl(estimate, "ciupper_linear_robust"),
         tau_quad_robust      = map_dbl(estimate, "tau_quad_robust"),
         se_quad_robust       = map_dbl(estimate, "se_quad_robust"),
         cilower_quad_robust  = map_dbl(estimate, "cilower_quad_robust"),
         ciupper_quad_robust  = map_dbl(estimate, "ciupper_quad_robust"),
         tau_cubic_robust      = map_dbl(estimate, "tau_cubic_robust"),
         se_cubic_robust       = map_dbl(estimate, "se_cubic_robust"),
         cilower_cubic_robust  = map_dbl(estimate, "cilower_cubic_robust"),
         ciupper_cubic_robust  = map_dbl(estimate, "ciupper_cubic_robust"),
         tau_linear_cluster      = map_dbl(estimate, "tau_linear_cluster"),
         se_linear_cluster       = map_dbl(estimate, "se_linear_cluster"),
         cilower_linear_cluster  = map_dbl(estimate, "cilower_linear_cluster"),
         ciupper_linear_cluster  = map_dbl(estimate, "ciupper_linear_cluster"),
         tau_quad_cluster      = map_dbl(estimate, "tau_quad_cluster"),
         se_quad_cluster       = map_dbl(estimate, "se_quad_cluster"),
         cilower_quad_cluster  = map_dbl(estimate, "cilower_quad_cluster"),
         ciupper_quad_cluster  = map_dbl(estimate, "ciupper_quad_cluster"),
         tau_cubic_cluster      = map_dbl(estimate, "tau_cubic_cluster"),
         se_cubic_cluster       = map_dbl(estimate, "se_cubic_cluster"),
         cilower_cubic_cluster  = map_dbl(estimate, "cilower_cubic_cluster"),
         ciupper_cubic_cluster  = map_dbl(estimate, "ciupper_cubic_cluster"),
         tau_linear_rdrobust    = map_dbl(estimate, "tau_linear_rdrobust"),
         se_linear_rdrobust     = map_dbl(estimate, "se_linear_rdrobust"),
         cilower_linear_rdrobust      = map_dbl(estimate, "cilower_linear_rdrobust"),
         ciupper_linear_rdrobust      = map_dbl(estimate, "ciupper_linear_rdrobust")
  )    %>% 
  left_join(tibble(variable = varlist2, var_label = varlabel2)) %>%
  select(-estimate) %>%
  left_join(bind_rows(national_acs_estimates, national_acs_estimates_cov) %>% 
              select(tau_main_main = tau, se_main_main = se, 
                     cilower_main_main = ci_lower, ciupper_main_main = ci_upper, variable)) %>%
  gather(est_name, estimate, -variable, -var_label) %>%
  separate(est_name, into = c("estimator", "model", "model2")) %>% 
  unite(model_name, model, model2) %>%
  spread(estimator, estimate ) %>%
  rename(ci_lower = cilower, ci_upper = ciupper) %>%
  arrange(factor(variable, levels = varlist2))

estimates_naive = ccp_estimates_naive %>% bind_rows(acs_estimates_naive) %>%
  arrange(factor(variable, levels = varlist3))



table_naive = combine_table_cols(estimates_naive)
knitr::kable(table_naive,"latex", booktabs = T, linesep = "",
             col.names = c("Outcome", "Main Estimate", 
                           "linear_robust", "quad_robust", "cubic_robust", "linear_cluster", "quad_cluster",  "cubic_cluster", "linear_rdrobust" )) %>%
  cat(sep = "\n", file = "tables/tables_naive.tex")

### Appendix Table 2
tau_full_hat %>% select(czone, tau_ch, rmse) %>% 
  left_join(cz_names) %>%
  left_join(beta_full_hat %>% select(czone, beta_ch, beta_rmse = rmse)) %>%
  left_join(pop_data) %>%
  arrange(-pop) %>%
  filter(row_number()<= 50) %>%
  arrange(statename, czname) %>%
  select(statename, czname, tau_ch, rmse, beta_ch, beta_rmse) %>%
  kable("latex", digits = 0, booktabs = TRUE,linesep = "" ) %>%
  cat(sep = "\n", file = "tables/appendix_table2_topczs.tex")

source("code/R_code/presentation_figs.R")



