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

# Name the tables we need
neighborhood_vars <- c("B17001_001", "B17001_002", "B17001_014",  "B17001_028", "B17001_043",  "B17001_057")

# As a test grab the data for Oregon
neighborhood_data_zip <- get_acs( geography = "zip code tabulation area",
                    variables = neighborhood_vars,
                    key       = key,
                    cache     = TRUE,
                    output    = "wide",
                    year      = 2012
                    )

neighborhood_data_zip <- neighborhood_data_zip %>%
  mutate(PovShare = B17001_002E/B17001_001E, PovShareNearElderly = (B17001_014E + B17001_028E) / ((B17001_014E + B17001_028E) + (B17001_043E + B17001_057E))) %>%
  select(zcta = GEOID, PovShare, PovShareNearElderly)

write_csv(neighborhood_data_zip, "/Users/psg24/Dropbox/GPW/data/zip_povertyshare.csv")


