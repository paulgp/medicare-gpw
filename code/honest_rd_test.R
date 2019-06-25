

install.packages("devtools") ## if devtools package not installed
devtools::install_github("kolesarm/RDHonest")
library(RDHonest)
  
library(haven)
final_zip_level5080_clean <- read_dta("~/Dropbox/GPW/data/final_zip_level5080_clean.dta")
final_county_level <- read_dta("~/Dropbox/GPW/data/final_county_level.dta")
county_temp_pctui <- read_dta("~/Dropbox/GPW/data/county_collapse_pctui_presentation.dta")
county_temp <- read_dta("~/Dropbox/GPW/data/county_collapse_presentation.dta")



final_zip_level5080_clean2 <- final_zip_level5080_clean %>% 
  select(q_coll_12, n_coll_12, avg_riskscore, balance_cc_delinq, 
         balance_mort_delinq, q_coll_12,  age, population) %>%
  mutate(q_coll_12 = q_coll_12* sqrt(population))

final_county_level2 <- final_county_level %>% mutate(age = age - 1) %>%
  filter(age > 60 & age < 70) %>%
  group_by(state, census_code, age) %>%
  summarize(q_coll_12 = weighted.mean(q_coll_12,population),
            n_coll_12 = weighted.mean(n_coll_12,population),
            avg_riskscore = weighted.mean(avg_riskscore,population),
            balance_cc_delinq = weighted.mean(balance_cc_delinq,population),
            balance_mort_delinq = weighted.mean(balance_mort_delinq,population),
            population = sum(population)) 

rescale <- final_county_level2 %>% group_by(age) %>% summarize(totalpop = sum(population))


RDHonest(n_coll_12 ~ age, cutoff=65,
         data=county_temp  %>% filter(age != 65),
         kern="uniform", M=0.04, opt.criterion="FLCI", sclass="H", se.method="EHW")
RDHonest(new_bpt ~ age, cutoff=65,
         data=final_zip_level5080_clean %>% filter(age != 65), 
         kern="uniform", M=0.04, opt.criterion="FLCI", sclass="H")
RDHonest(avg_riskscore ~ age, cutoff=65,
         data=final_zip_level5080_clean %>% filter(age != 65), 
         kern="triangular", M=0.04, opt.criterion="FLCI", sclass="H")
RDHonest(balance_cc_delinq ~ age, cutoff=65,
         data=final_zip_level5080_clean %>% filter(age != 65), 
         kern="triangular", M=0.04, opt.criterion="FLCI", sclass="H")
RDHonest(balance_ttl_delinq ~ age, cutoff=65,
         data=final_zip_level5080_clean %>% filter(age != 65), kern="triangular", M=0.04, opt.criterion="FLCI", sclass="H")
RDHonest(balance_mort_delinq ~ age, cutoff=65,
         data=final_zip_level5080_clean %>% filter(age != 65), kern="triangular", M=0.04, opt.criterion="FLCI", sclass="H")
