


local path "/Users/PSG24/Dropbox/GPW"

/** Black Share in 2012 **/
insheet using "`path'/data/race_by_zip2012.csv", comma clear names
tempfile tmp
gen black_share = black / total
gen nonwhite_share = nonwhite / total
drop black nonwhite total
rename geoid zip
save `tmp'


insheet using "`path'/data/zip_povertyshare.csv", comma clear names
tempfile tmp2
rename zcta zip
save `tmp2'


/** Pct UI in 2012 **/
insheet using "`path'/data/zip_sahie.csv", comma clear names
tempfile tmp3
rename age_55to64years pctui_young
rename age_65to74years pctui_old
rename fips zip
destring pctui_*, replace force
save `tmp3'


local credit_vars n_coll_12 q_coll_12_gt0 q_coll_12 new_bpt new_fc balance_ttl_delinq avg_riskscore balance_cc balance_mort balance_heloc balance_ttl balance_cf balance_cf_delinq balance_auto_delinq  balance_mort_delinq balance_heloc_delinq balance_cc_delinq

local credit_vars q_coll_12 n_coll_12 avg_riskscore balance_cc_delinq balance_mort_delinq
use "`path'/data/final_zip_level5080.dta", clear
rename balance_consumer_finance  balance_cf
rename balance_consumer_finance_delinq  balance_cf_delinq
keep if inrange(age, 55, 75)
rename zipcode zip
merge m:1 zip using `tmp'
keep if _merge == 1 | _merge == 3
drop _merge


merge m:1 zip using `tmp2'
keep if _merge == 1 | _merge == 3
drop _merge

merge m:1 zip using `tmp3'
keep if _merge == 1 | _merge == 3
drop _merge



gen year = floor(quarter / 100)
*egen state_zip_year = group(state zip year)

/** make Bankruptcy in percentage points **/
replace new_bpt = new_bpt * 100
collapse `credit_vars' pctui_young   (rawsum) population [aweight=population ] if inrange(age, 55,64), by(state)
statastates, abbrev(state) nogen

save "`path'/data/state_collapse.dta",  replace
outsheet using "`path'/data/state_collapse.csv", comma replace


use "`path'/data/final_zip_level5080.dta", clear
rename balance_consumer_finance  balance_cf
rename balance_consumer_finance_delinq  balance_cf_delinq
keep if inrange(age, 62, 64)
replace new_bpt = new_bpt * 100
collapse `credit_vars'    (rawsum) population [aweight=population ] , by(state age)
statastates, abbrev(state) nogen

save "`path'/data/state_collapse_63_64.dta",  replace
outsheet using "`path'/data/state_collapse_63_64.csv", comma replace


/** R Code

library(readr)
library(tidyverse)
library(ggrepel)
state_collapse <- read_csv("~/Dropbox/GPW/data/state_collapse.csv")
ggplot(data = state_collapse %>% filter(!is.na(state_name)) %>% mutate(state_name = str_to_title(state_name))) +
   geom_col(aes(y = q_coll_12, x = reorder(state_name, -q_coll_12)), fill =  "#007CB2") +
   coord_flip() + 
   theme_minimal() +
   theme(text = element_text(size=12)) +
   scale_y_continuous(expand = c(0, 0), limits = c(0, 250)) +
   labs(x="", y="", title="Per-capita annual collections debt across states", fill="")
ggsave("~/Dropbox/GPW/graphs/q_coll_12_states_intro.pdf", width = 12, height = 6)


ggplot(data = state_collapse %>% filter(!is.na(state_name)) %>% mutate(state_name = str_to_title(state_name))) +
   geom_col(aes(y = pctui_young, x = reorder(state_name, -pctui_young)), fill =  "#007CB2") +
   coord_flip() + 
   theme_minimal() +
   theme(text = element_text(size=12)) +
   labs(x="", y="", title="Near-elderly uninsurance rates", fill="")    
ggsave("~/Dropbox/GPW/graphs/pctui_states_intro.pdf", width = 12, height = 7)


ggplot(data = state_collapse %>% filter(!is.na(state_name)) %>% mutate(state_name = str_to_title(state_name))) +
   geom_text(aes(y = q_coll_12, x = pctui_young, label = state), color =  "#007CB2") +
   theme_minimal() +
   theme(text = element_text(size=12)) +
   labs(x="Near-elderly uninsurance rates", y="", title="Correlation of collections debt and uninsurance rates", fill="")    
ggsave("~/Dropbox/GPW/graphs/q_coll_12_vs_pctui_states_intro.pdf", width = 12, height = 7)
