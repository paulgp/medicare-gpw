library(readr)
library(tidyverse)
library(Hmisc)
library(rdrobust)

age_collapse <- read_csv("~/Dropbox/Research/GPW/data/ACS_microdata_collapsed_by_age.csv")
#age_county_collapse <- read_csv("~/Dropbox/Research/GPW/data/ACS_microdata_collapsed_by_age_county.csv")

# Set age as numeric, center, and restrict to 50-80
for_rd <- age_collapse %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(age_centered = age-65) %>% 
  filter(!is.na(age_centered) & age >= 50 & age <= 80)

# Loop over columns
loop.vars     = c("own_dwelling", "married", "employed", "inctot", "fam_size_1", "fam_size_2", "fam_size_more", "female")
var.labels    = c(age = "Age in years", own_dwelling = "Homeowner", married = "Married", employed = "Employed", inctot = "Income", fam_size_1 = "Household of 1", fam_size_2 = "Household of 2", fam_size_more = "Household of 3 or more", female = "Share female")
label(for_rd) = as.list(var.labels[match(names(for_rd), names(var.labels))])

for (var in loop.vars) {

    print(var)
    y <- pull(for_rd, var)
    x <- pull(for_rd, age)
  
  pdf(paste("~/Dropbox/Research/GPW/graphs/smoothness_", var, ".pdf", sep=""))
  rdplot(y, x, c = 65, x.label = "Age in years", y.label = var.labels[var], title = "") + theme_grey(base_size = 22)
  dev.off()
  
}


