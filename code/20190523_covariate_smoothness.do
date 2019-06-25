* This do-file uses the ACS micro-data to test for covariate smoothness
set more off

local path "/Users/jww48/Dropbox/Research/GPW"
local covariates own_dwelling fam_size_1 fam_size_2 fam_size_more married educ_less_than_12 educ_high_school educ_some_college educ_full_college educ_gtr_college employed female inctot 

use "`path'/data/ACS_microdata 2.dta", clear

**************************************************************
* Construct covariates and collapse to county-year-age level *
**************************************************************

* Collapse to county-year-age level
* drop if year < 2005 /* could drop pre-2005 if using countyfip since it is missing */

* Homeownership
gen own_dwelling      = ownershp == 1

* Family size
gen fam_size_1        = famsize == 1
gen fam_size_2        = famsize == 2
gen fam_size_more     = famsize != 1 & famsize != 2

* Marital status
gen married           = marst == 1 | marst == 2

* Education variables
gen educ_less_than_12 = educ == 0 | educ == 1 | educ == 2 | educ == 3 | educ == 4 | educ == 5
gen educ_high_school  = educ == 6
gen educ_some_college = educ == 7 | educ == 8
gen educ_full_college = educ == 10
gen educ_gtr_college  = educ == 11

* Employment status
gen employed = empstat == 1

* Sex
gen female = sex == 2

preserve



* Collapsed by age - state
collapse `covariates' (rawsum) population = perwt [aw=perwt], by(age statefip)
save "`path'/data/ACS_microdata_collapsed_by_age_state.dta", replace
export delimited "`path'/data/ACS_microdata_collapsed_by_age_state.csv", replace nolabel

restore
preserve

* Collapsed by age - county
collapse `covariates' (rawsum) population = perwt [aw=perwt], by(age countyfip statefip)
save "`path'/data/ACS_microdata_collapsed_by_age_state.dta", replace
export delimited "`path'/data/ACS_microdata_collapsed_by_age_county.csv", replace nolabel

restore
preserve

* Collapsed by age
collapse `covariates' (rawsum) population = perwt [aw=perwt=], by(age)
save "`path'/data/ACS_microdata_collapsed_by_age.dta", replace
export delimited "`path'/data/ACS_microdata_collapsed_by_age.csv", replace nolabel
	
