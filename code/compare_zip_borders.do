
local path "/Users/PSG24/Dropbox/GPW"

use `path'/data/matched_zips, clear

keep zip1 input_state bs_1 d_bs_1 ZIP mi_to_zcta5 state_fips state_name

rename ZIP zip_dest
rename zip1 zip_orig
rename (input_state bs_1) (state_orig state_dest)
drop state_fips state_name

tempfile tmp
save `tmp'


local credit_vars n_coll_12 q_coll_12_gt0 q_coll_12 new_bpt new_fc balance_ttl_delinq avg_riskscore balance_cc balance_mort balance_heloc balance_ttl balance_cf balance_cf_delinq balance_auto_delinq  balance_mort_delinq balance_heloc_delinq balance_cc_delinq

use "`path'/data/final_zip_level5080.dta", clear
keep if inrange(age, 64, 66)

gen near_elderly = inrange(age , 60,64)
gen young_elderly = inrange(age , 66,70)

rename balance_consumer_finance  balance_cf
rename balance_consumer_finance_delinq  balance_cf_delinq
rename quarter year

local outcome avg_riskscore

collapse `credit_vars'   (rawsum) population  [aw=population ] if young_elderly ==1 | near_elderly == 1, by(young_elderly zipcode )

tempfile tmp_credit
save `tmp_credit'

use `tmp', clear

rename zip_orig zipcode
merge 1:m zipcode using `tmp_credit', keep(match)

drop _merge
rename zipcode zip_orig

foreach var of varlist `credit_vars' population {
	rename `var' `var'_orig
	}


rename zip_dest zipcode
joinby zipcode young_elderly using `tmp_credit', 

*drop _merge
rename zipcode zip_dest

foreach var of varlist `credit_vars' population {
	rename `var' `var'_dest
	}



egen zip_0 = rowmin(zip_orig zip_dest)
egen zip_1 = rowmax(zip_orig zip_dest)

egen zip_pair_id = group(zip_0 zip_1)

bys zip_pair_id young_elderly: keep if _n == 1

keep if population_orig + population_dest > 25

preserve

collapse n_coll_12_orig-balance_cc_delinq_orig (rawsum) population_orig [aweight=population_orig], by(state_orig young_elderly)
tempfile zip_avg
save `zip_avg'
restore


gen state_0 = state_orig if zip_0 == zip_orig
replace state_0 = state_dest if state_0 == ""
gen state_1 = state_orig if zip_1 == zip_orig
replace state_1 = state_dest if state_1 == ""

foreach var in `credit_vars' population {
	gen `var'_0 = `var'_orig if zip_0 == zip_orig
	replace `var'_0 = `var'_dest if `var'_0 == .
	gen `var'_1 = `var'_orig if zip_1 == zip_orig
	replace `var'_1 = `var'_dest if `var'_1 == .
	}

drop *_orig *_dest

reshape long `credit_vars' zip population, i(zip_pair_id young_elderly ) j(pair_id "_0" "_1") string


/** 1. fix a state-pair **/
/** 2. Estimate regression with zips in that pair, with zip-pair fe, and a dummy for one of the two states **/
/** this captures the relative effect within that pair. **/
/** 3. Collect these ~N^2 effects **/
/** 4. Regress these effects on design matrix from Appendix 1 of Chetty Hendren **/

statastates, abbreviation(state_0) nogenerate
drop state_name
rename state_fips state_fips_0
statastates, abbreviation(state_1) nogenerate
drop state_name
rename state_fips state_fips_1

egen state_min = rowmin(state_fips_0 state_fips_1)
egen state_max = rowmax(state_fips_0 state_fips_1)

gen state_min_str = state_0 if state_fips_0 == state_min
replace state_min_str = state_1 if state_min_str == ""
gen state_max_str = state_0 if state_fips_0 == state_max
replace state_max_str = state_1 if state_max_str == ""

gen state_pair_str = state_min_str + "_" + state_max_str
encode state_pair_str, gen(state_pair) label(state_pair)

levelsof state_pair, local(state_pairs)

gen alter = pair_id == "_1"
encode state_1, gen(alter_state)


capture drop test
foreach state_pair in `state_pairs' {
	gen test = state_pair == `state_pair'
	gsort -test
	local state_pair_str_`state_pair' = state_pair_str[1]
	local alter_state_`state_pair' = state_1[1]
	qui reg `outcome' alter [aweight=population] if state_pair == `state_pair' & young_elderly == 0, absorb(zip_pair_id) 
	local b_`state_pair'  = _b[alter]
	local se_`state_pair' = _se[alter]
	qui reg `outcome' alter [aweight=population] if state_pair == `state_pair' & young_elderly == 1, absorb(zip_pair_id) 
	local b_`state_pair'_med  = _b[alter]
	local se_`state_pair'_med = _se[alter]
	drop test
}



clear

set obs `=wordcount("`state_pairs'") * 2'
gen b = .
gen se = .
gen alter = ""
gen pair = ""
gen medicare = .

local i = 1
foreach state_pair in `state_pairs' {
	qui replace pair = "`state_pair_str_`state_pair''" if _n == `i'
	qui replace alter = "`alter_state_`state_pair''" if _n == `i'
	qui replace b = `b_`state_pair'' if _n == `i'
	qui replace se= `se_`state_pair'' if _n == `i'
	qui replace medicare= 0 if _n == `i'
	local i = `i' + 1
}

foreach state_pair in `state_pairs' {
	qui replace pair = "`state_pair_str_`state_pair''" if _n == `i'
	qui replace alter = "`alter_state_`state_pair''" if _n == `i'
	qui replace b = `b_`state_pair'_med' if _n == `i'
	qui replace se= `se_`state_pair'_med' if _n == `i'
	qui replace medicare= 1 if _n == `i'
	local i = `i' + 1
}


split pair, parse("_")

drop if pair1 == "ME" | pair2 == "ME"
levelsof pair1, local(pair1)
levelsof pair2, local(pair2)

foreach state in `pair1' `pair2' {
	capture gen state_`state' = 0
	}


foreach state in `pair1' `pair2' {
	replace state_`state' = -1 if alter != "`state'" & (pair1 == "`state'" | pair2 == "`state'" )
	replace state_`state' = 1 if alter == "`state'" & (pair1 == "`state'" | pair2 == "`state'" )
	}

gen weight = 1/(se^2)
reg b state_AL-state_WV [aweight=weight] if medicare == 0, nocons
mat b = e(b)
mat b = b'
mat V = e(V)
mat V = vecdiag(V)
mat V = V'


reg b state_AL-state_WV [aweight=weight] if medicare == 1, nocons
mat b_med = e(b)
mat b_med = b_med'
mat V_med = e(V)
mat V_med = vecdiag(V_med)
mat V_med = V_med'



clear
svmat2 b, rnames(state)
svmat V
tempfile tmp
save `tmp'
clear
svmat2 b_med, rnames(state)
rename b_med1 b1
svmat V_med
rename V_med1 V1
gen medicare = 1
append using `tmp'
replace medicare = 0 if medicare == .

split state, parse("_")
drop state1 state
rename state2 state_orig

rename medicare young_elderly
merge 1:1 state  young_elderly using `zip_avg'

egen mean_b = mean(b1), by(young_elderly)
replace b1 = b1 - mean_b

*keep if young_elderly == 0
twoway (scatter `outcome'_orig b1 if young_elderly == 0,  mlab(state_orig) mlabpos(0) m(i)) ///
  (scatter `outcome'_orig b1 if young_elderly == 1,  mlab(state_orig) mlabpos(0) m(i)), ///
  legend(off) name(a, replace)

reg b1 `outcome'_orig
reg  `outcome'_orig b1 if young_elderly == 0

/*
keep b1 state_orig young_elderly
reshape wide b1 , i(state_orig) j(young_elderly)
twoway (scatter b11 b10 ,  mlab(state_orig) mlabpos(0) m(i)), name( b, replace)
