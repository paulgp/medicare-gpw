
library(tidyverse)
library(tidycensus)
library(stringr)
library(ggthemes)
library(readxl)
library(viridis)
## key = Your Census API key. Obtain one at http://api.census.gov/data/key_signup.html
key <- "6dc01cb16b173a97a1215ea441de09bc0d2da25d"


acs_varnames2012 <- load_variables(2012, "acs5", cache = TRUE)

pull_vars_varnames <- acs_varnames2012 %>%
  filter(concept == "HEALTH INSURANCE COVERAGE STATUS BY SEX BY AGE") %>%
  rename(variable = name)

insurance_data_county2012 <- get_acs(geography = "county",
                                     variables = pull_vars_varnames$variable,
                                     key       = key,
                                     cache     = TRUE,
                                     year = 2012
)

insurance_data_county2012 <- insurance_data_county2012 %>%
  left_join(by="variable", pull_vars_varnames) %>%
  separate(label, into = c("Estimate", "Num", "Sex", "Age", "InsuranceType"), sep ="!!")


ages <- c("Under 6 years",    "6 to 17 years"  , "18 to 24 years"  ,
          "25 to 34 years"  ,"35 to 44 years" ,"45 to 54 years" ,
          "55 to 64 years" ,  "65 to 74 years" ,"75 years and over")

insurance_data2$Age <- factor(insurance_data2$Age, levels =ages)

insurance_data2 <- insurance_data_county2012 %>% 
  filter(InsuranceType != "With health insurance coverage" |
         is.na(InsuranceType)) %>%
  select(-Estimate, -moe, -Num, -concept, -variable) %>%
  mutate(InsuranceType = case_when(InsuranceType ==  "No health insurance coverage" ~ "Uninsured",
                                   TRUE ~ "Total")) %>%
  group_by(Age, GEOID, InsuranceType) %>%
  summarize(estimate = sum(estimate)) %>%
  spread(InsuranceType, estimate) %>% filter(is.na(Age) == FALSE) %>%
  mutate(ui_rate = Uninsured / Total) %>%
  filter(Age == "55 to 64 years" | Age == "65 to 74 years") %>%
  select(-Uninsured, -Total) %>%
  spread(Age, ui_rate, sep="_") %>%
  mutate(fips = as.numeric(GEOID)) %>%
  ungroup() %>%
  select(-GEOID)





acs_varnames  <- load_variables(2016, "acs5", cache = TRUE)

insurance_var <- acs_varnames %>% filter(str_detect(name, "^B2700"))

pull_vars_varnames <- acs_varnames %>%
  filter(concept == "HEALTH INSURANCE COVERAGE STATUS BY SEX BY AGE") %>%
  rename(variable = name)



insurance_data_us <- get_acs(geography = "us",
                           variables = pull_vars_varnames$variable,
                           key       = key,
                           cache     = TRUE
)

insurance_data_us <- insurance_data_us %>%
  left_join(by="variable", pull_vars_varnames) %>%
  separate(label, into = c("Estimate", "Num", "Sex", "Age", "InsuranceType"), sep ="!!")


insurance_data2 <- insurance_data_us %>% filter(InsuranceType != "With health insurance coverage" |
                                               is.na(InsuranceType)) %>%
  group_by(Sex, Age, GEOID) %>%
  select(-Estimate, -moe, -Num, -concept, -variable) %>%
  mutate(InsuranceType = case_when(InsuranceType ==  "No health insurance coverage" ~ "Uninsured",
                                   TRUE ~ "Total")) %>%
  spread(InsuranceType, estimate) %>% filter(is.na(Age) == FALSE) %>%
  mutate(ui_rate = Uninsured / Total)

ages <- c("Under 6 years",    "6 to 17 years"  , "18 to 24 years"  ,
          "25 to 34 years"  ,"35 to 44 years" ,"45 to 54 years" ,
          "55 to 64 years" ,  "65 to 74 years" ,"75 years and over")

insurance_data2$Age <- factor(insurance_data2$Age, levels =ages)

ggplot(data = insurance_data2 %>% mutate(ui_rate = Uninsured / Total)) + 
  geom_col(aes(y = ui_rate, x = Age, fill = Sex), position = "dodge2")  + 
  coord_flip() +
  labs(x = "", y= "Uninsurance Rate", 
       title = "National Uninsurance Rates, 2016") +
  theme_minimal() +
  theme(text = element_text(size=18, 
                            family="News Cycle")) 

### Pull Race Data by County for 2016
pull_vars_varnames <- acs_varnames %>%
  filter(concept == "RACE") %>%
  rename(variable = name)

race_data_county <- get_acs(geography = "county",
                                   variables = pull_vars_varnames$variable,
                                   key       = key,
                                   cache     = TRUE
)

race_data_county <- race_data_county %>%
  left_join(by="variable", pull_vars_varnames) %>%
  separate(label, into = c("Estimate", "Num", "Race", "RaceSub"), sep ="!!")

race_data_county  %>% select(-NAME, -moe, -Num, -RaceSub, -concept, -Estimate) %>%
  filter(variable != "B02001_009" & variable != "B02001_010") %>%
  mutate(race2 = case_when(Race == "White alone" ~ "White",
                           Race == "Black or African American alone" ~ "Black",
                           is.na(Race) ~ "Total",
                           TRUE ~ "NonWhite")) %>%
  group_by(GEOID, race2) %>%
  summarize(estimate = sum(estimate)) %>% 
  ungroup() %>%
  spread( race2, estimate) %>% write_csv("~/Dropbox/GPW/data/race_by_county2016.csv")

### Pull Race Data by Zip for 2012
pull_vars_varnames <- acs_varnames2012 %>%
  filter(concept == "RACE") %>%
  rename(variable = name)

race_data_zip <- get_acs(geography = "zip code tabulation area",
                            variables = pull_vars_varnames$variable,
                            key       = key,
                            cache     = TRUE,
                            year      = 2012
)

race_data_zip <- race_data_zip %>%
  left_join(by="variable", pull_vars_varnames) %>%
  separate(label, into = c("Estimate", "Num", "Race", "RaceSub"), sep ="!!")

race_data_zip  %>% select(-NAME, -moe, -Num, -RaceSub, -concept, -Estimate) %>%
  filter(variable != "B02001_009" & variable != "B02001_010") %>%
  mutate(race2 = case_when(Race == "White alone" ~ "White",
                           Race == "Black or African American alone" ~ "Black",
                           is.na(Race) ~ "Total",
                           TRUE ~ "NonWhite")) %>%
  group_by(GEOID, race2) %>%
  summarize(estimate = sum(estimate)) %>% 
  ungroup() %>%
  spread( race2, estimate) %>% write_csv("~/Dropbox/GPW/data/race_by_zip2012.csv")


### Pull College Education by County

pull_vars_varnames <- acs_varnames %>%
  filter(concept == str_to_upper("sex by age by educational attainment for the population 18 years and over")) %>%
  rename(variable = name)

education_data_county <- get_acs(geography = "county",
                            variables = pull_vars_varnames$variable,
                            key       = key,
                            cache     = TRUE
)

education_data_county <- education_data_county %>%
  left_join(by="variable", pull_vars_varnames) %>%
  separate(label, into = c("Estimate", "Num", "Sex", "Age",  "Education"), sep ="!!")


education_data_county %>% filter(Age == "65 years and over" | Age == "45 to 64 years") %>%
  mutate(bachelor_plus = case_when(Education == "Bachelor's degree" ~ "BachelorPlus",
                                  Education == "Graduate or professional degree" ~ "BachelorPlus",
                                  is.na(Education) == TRUE ~ "Total",
                                  TRUE ~ "Other")) %>%
  filter(bachelor_plus != "Other") %>%
  group_by(GEOID, bachelor_plus, Age) %>%
  summarize(total = sum(estimate)) %>%
  spread(bachelor_plus, total) %>%
  mutate(bachelor_plus_share = BachelorPlus / Total) %>% 
  select(-Total, -BachelorPlus) %>%
  mutate(age_group = case_when(Age == "45 to 64 years" ~ "CollegeShare45_64",
                               TRUE ~ "CollegeShare65_Plus")) %>%
  select(-Age) %>%
  spread(age_group, bachelor_plus_share) %>% write_csv("~/Dropbox/GPW/data/educ_by_county2016.csv")

### Zip Code PCTUI 
insurance_data_zip<- get_acs(geography = "zip code tabulation area",
                                variables = pull_vars_varnames$variable,
                                key       = key,
                             year = 2012,
                                cache     = TRUE
)

insurance_data_zip <- insurance_data_zip %>%
  left_join(by="variable", pull_vars_varnames) %>%
  separate(label, into = c("Estimate", "Num", "Sex", "Age", "InsuranceType"), sep ="!!")


insurance_data2 <- insurance_data_zip%>% 
  filter(InsuranceType != "With health insurance coverage" |
           is.na(InsuranceType)) %>%
  select(-Estimate, -moe, -Num, -concept, -variable) %>%
  mutate(InsuranceType = case_when(InsuranceType ==  "No health insurance coverage" ~ "Uninsured",
                                   TRUE ~ "Total")) %>%
  group_by(Age, GEOID, InsuranceType) %>%
  summarize(estimate = sum(estimate)) %>%
  spread(InsuranceType, estimate) %>% filter(is.na(Age) == FALSE) %>%
  mutate(ui_rate = Uninsured / Total) %>%
  filter(Age == "55 to 64 years" | Age == "65 to 74 years") %>%
  select(-Uninsured, -Total) %>%
  spread(Age, ui_rate, sep="_") %>%
  mutate(fips = as.numeric(GEOID)) %>%
  ungroup() %>%
  select(-GEOID)

write_csv(insurance_data2, "~/Dropbox/GPW/data/zip_sahie.csv")
