## Load packages
library(tidyverse)
library(tidycensus)
library(stringr)
library(ggthemes)
library(readxl)
library(viridis)

## key = Your Census API key. Obtain one at http://api.census.gov/data/key_signup.html
key <- "6dc01cb16b173a97a1215ea441de09bc0d2da25d"

### Pull "good neighborhood" variables from ACS: see http://housinginsights.org/data/census.html (for now just poverty share)

acs_varnames2012 <- load_variables(2012, "acs5", cache = TRUE)
pull_vars_varnames <- acs_varnames2012 %>%
  filter(concept == "SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER") %>%
  rename(variable = name)


# As a test grab the data for Oregon
employment_state <- get_acs( geography = "state",
                    variables = pull_vars_varnames$variable,
                    key       = key,
                    cache     = TRUE,
                    year      = 2012
                    )

employment_state <- employment_state %>%
  left_join(by="variable", pull_vars_varnames) %>%
  separate(label, into = c("Estimate", "Num", "Sex", "Age", "LaborForce", "Military", "Working"), sep ="!!")

employment_state_clean <- employment_state  %>%
  filter((Working == "Employed" | Working == "Unemployed" | (is.na(LaborForce) & is.na(Age) == FALSE)) |
          ((Age == "65 to 69 years" | Age == "70 to 74 years" | Age == "75 years and over") & (Military == "Employed" | Military == "Unemployed" ))) %>%
  mutate(employed = case_when(Working == "Employed" ~ "Employed",
                              Working == "Unemployed" ~ "Unemployed",
                              Military == "Employed" ~ "Employed",
                              Military == "Unemployed" ~ "Unemployed",
                              TRUE ~ "All")) %>%
  group_by(GEOID, NAME, Age, employed) %>%
  summarize(num_workers = sum(estimate)) %>% 
  spread(employed, num_workers) %>%
  mutate(work_perc = Employed / (Employed + Unemployed),
         work_all_perc   = Employed / All) %>%
  mutate(age2 = case_when(Age == "16 to 19 years" ~ 19,
                          Age == "20 and 21 years"~ 21,
                          Age == "22 to 24 years" ~ 24,
                          Age == "25 to 29 years" ~ 29,
                          Age == "30 to 34 years" ~ 34,
                          Age == "35 to 44 years" ~ 44,
                          Age == "45 to 54 years" ~ 54,
                          Age == "55 to 59 years" ~ 59,
                          Age == "60 and 61 years"~ 61,
                          Age == "62 to 64 years" ~ 64,
                          Age == "65 to 69 years" ~ 69,
                          Age == "70 to 74 years" ~ 74,
                          TRUE ~ 80))
                          

ggplot(data=employment_state_clean) + geom_point(aes(y = work_all_perc, x = age2))

ggplot(data = state_collapse %>% filter(!is.na(state_name))) +
  geom_col(aes(y = q_coll_12, x = reorder(state_name, -q_coll_12))) +
  coord_flip() + 
  theme_minimal() +
  theme(text = element_text(size=16)) +
  labs(x="", y="Beta", title="Non-linear market loadings", fill="")    
write_csv(employment_state_clean, "/Users/psg24/Dropbox/GPW/data/employemnt_state_clean.csv")


