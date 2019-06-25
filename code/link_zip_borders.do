

import excel using ../data/ZIP_COUNTY_032010.xlsx, clear firstrow
drop *_RATIO
gen state_fips = substr(COUNTY, 1,2)
drop COUNTY
duplicates drop
destring state_fips, replace
statastates, fips(state_fips)
destring ZIP, replace
keep if _merge == 3
drop _merge
tempfile tmp
save `tmp'
use ../data/sf12010zcta5distance25miles, clear
destring zip1 zip2, replace

rename zip2 ZIP
merge m:1 ZIP using `tmp'
keep if _merge == 1 | _merge == 3
drop _merge
rename state_abbrev bs_1
tempfile tmp2
save `tmp2'

insheet using ../data/ZipDistances_2010_FieldKey/ZipDistances_2010.csv, comma clear names
rename input_zip zip1
joinby zip1 bs_1 using `tmp2', unmatched(master)

sort zip1 input_state bs_1 mi_to_zcta5

bys zip1 input_state (mi_to_zcta5): keep if _n == 1
keep if _merge == 3
keep if bs_1 != ""


save ../data/matched_zips.dta, replace


