
set matsize 11000

local path "/Users/PSG24/Dropbox/GPW"

/* use ../data/sf12000countydistancemiles, clear */
/* destring county1, replace */
/* destring county2, replace */
/* gen state_match = floor(county2/1000) */
/* save ../data/county_distances_all, replace */
insheet using ../data/county_pairs_dist.asc, clear comma

rename v1 bordindx
label var bordindx "Border Index"
rename v2 st1st2
label var st1st2  "Name of the border"
rename v3 st1
label var st1 "State 1 of the border pair"
rename v4 st2
label var st2  "State 2 of the border pair"
rename v5 state
rename v6 county
rename v7 mindist
label var mindist "Minimum distance of county from border."
rename v8 milemark
label var milemark "Point along border where minimum is obtained."

rename st1 st_orig
rename st2 st_dest
rename state state_orig
rename county county_orig

drop v9
/*gen st1_st2_state = st1st2+string(state_orig)*/
/*tabstat mindist , by(st1_st2_state ) stat(min p25) */
/* egen min_dist_all = min(mindist), by(st1_st2_state)*/
/* sum min_dist_all */
gen county = state_orig * 1000 + county_orig
/*** KEEP BORDERING COUNTIES USING THIS FILE ***/
merge m:1 county using ../data/CountyPairs, keep(match) nogen keepusing(county)
rename county county1
gen state_match = st_orig if state_orig != st_orig
replace state_match = st_dest if state_orig == st_orig
joinby county1 state_match using ../data/county_distances_all

bys st1st2 county1 (mi_to_county): keep if _n == 1

egen county_min = rowmin(county1 county2)
egen county_max = rowmax(county1 county2)
egen county_pair_id = group(county_min county_max)
*keep if mindist < 16

preserve
keep state_orig county_orig mindist
duplicates drop
gen county = state_orig * 1000 + county_orig
sort county mindist
by county: keep if _n == 1
maptile mindist, geo(county2000) twopt(name(map_dist, replace))
graph export ../graphs/border_map.pdf, replace
restore

destring state_orig, replace
gen alter = state_orig == st_dest

egen pair_alter = total(alter), by(bordindx)
egen pair_ego = count(alter), by(bordindx)
replace pair_ego = pair_ego -pair_alter
drop if pair_ego == 0 | pair_alter == 0
drop pair_ego pair_alter alter

preserve
keep county1
duplicates drop
rename county1 county
tempfile county_list
save `county_list'
restore

tempfile tmp
save `tmp'


*
*local credit_vars q_coll_12 
*local credit_vars q_coll_12 n_coll_12 avg_riskscore balance_cc_delinq balance_mort_delinq
*local credit_vars n_coll_12 q_coll_12_gt0 q_coll_12 new_bpt new_fc balance_ttl_delinq avg_riskscore balance_cc balance_mort balance_heloc balance_ttl balance_cf balance_cf_delinq balance_auto_delinq  balance_mort_delinq balance_heloc_delinq balance_cc_delinq
local credit_vars q_coll_12 n_coll_12 avg_riskscore balance_cc_delinq balance_mort_delinq

use "`path'/data/final_county_level.dta", clear
replace age = age - 1
keep if inrange(age, 62, 68)
rename bpt new_bpt
rename fc new_fc

gen near_elderly = inrange(age , 60,64)
gen young_elderly = inrange(age , 66,70)

rename balance_consumer_finance  balance_cf
rename balance_consumer_finance_delinq  balance_cf_delinq
rename quarter year

local outcome q_coll_12
statastates, abbrev(state) nogen
gen t = age - 65
gen county = state_fips*1000 + census_code
drop if county == .


merge m:1 county using `county_list', keep(match)

levelsof county, local(county_list)

foreach x of varlist `credit_vars' {
	gen `x'_lower = .
	gen `x'_upper = .	
	foreach county in `county_list' {
		qui gen medicare = (age-65) >=0		
		qui reg `x' c.t##i.medicare [aweight=population] if inrange(age, 62,68) & age != 65 & county == `county',
		qui replace medicare = 0
		qui predict `x'_lower_temp if age == 65
		qui replace `x'_lower = `x'_lower_temp if age == 65 & county == `county'
		qui drop `x'_lower_temp
		qui replace medicare = 1
		qui predict `x'_upper_temp if age == 65
		qui replace `x'_upper = `x'_upper_temp if age == 65 & county == `county'
		qui drop `x'_upper_temp
		qui drop medicare
		}
	egen `x'_64 = max(`x'_lower), by(county year)
	egen `x'_66 = max(`x'_upper), by(county year)
	replace `x' = `x'_64 if age == 64
	replace `x' = `x'_66	if age == 66
	}


keep if inrange(age, 64, 66)

collapse `credit_vars'   (rawsum) population  [aw=population ] if young_elderly ==1 | near_elderly == 1, by(young_elderly state census_code )
statastates, abbrev(state) nogen

save credit_65_estimates, replace

use `tmp', clear

rename state_orig state_fips
rename county_orig census_code
*merge 1:m state_fips census_code using `tmp_credit', keep(match)
joinby state_fips census_code using ../code/credit_65_estimates

rename state_fips statefips
rename census_code countyfips
merge m:1 statefips countyfips using ../data/pctui_county_2010, keep(match)

rename  statefips state_orig
rename  countyfips county_orig


*drop _merge

*keep if population_orig + population_dest > 25

preserve
collapse `credit_vars' pctui (rawsum) population [aweight=population], by(state_orig young_elderly)
tempfile zip_avg
save `zip_avg'
restore




/** 1. fix a state-pair **/
/** 2. Estimate regression with zips in that pair, with zip-pair fe, and a dummy for one of the two states **/
/** this captures the relative effect within that pair. **/
/** 3. Collect these ~N^2 effects **/
/** 4. Regress these effects on design matrix from Appendix 1 of Chetty Hendren **/

replace st1st2 = subinstr(st1st2, "-", "_", .)
levelsof st1st2, local(state_pairs)

gen alter = st_dest == state_orig
encode st1st2, gen(st1st2_id)


foreach outcome of varlist `credit_vars' pctui {

	preserve
	reg `outcome' i.state_orig [aweight=population] if young_elderly == 0, absorb(st1st2_id) /*absorb(county_pair_id) */
	mat b_joint = e(b)
	mat b_joint = b_joint'
	reg `outcome' i.state_orig [aweight=population] if young_elderly == 1, absorb(st1st2_id) /*absorb(county_pair_id) */
	mat b_joint_med = e(b)
	mat b_joint_med = b_joint_med'

	reg `outcome' i.state_orig [aweight=population] if young_elderly == 0,  absorb(county_pair_id) 
	mat b_joint_cont = e(b)
	mat b_joint_cont = b_joint_cont'
	reg `outcome' i.state_orig [aweight=population] if young_elderly == 1,  absorb(county_pair_id) 
	mat b_joint_cont_med = e(b)
	mat b_joint_cont_med = b_joint_cont_med'



	capture drop test
	foreach state_pair in `state_pairs' {
		gen test = st1st2 == "`state_pair'"
		gsort -test
		local state_pair_str_`state_pair' = st1st2[1]
		local alter_state_`state_pair' = st_dest[1]
		qui reg `outcome' alter [aweight=population] if st1st2 == "`state_pair'" & young_elderly == 0, absorb(county_pair_id) 
		local b_`state_pair'  = _b[alter]
		local se_`state_pair' = _se[alter]
		qui reg `outcome' alter [aweight=population] if st1st2 == "`state_pair'" & young_elderly == 1, absorb(county_pair_id) 
		local b_`state_pair'_med  = _b[alter]
		local se_`state_pair'_med = _se[alter]
		drop test
		}





	distinct st1st2
	local num = r(ndistinct)
	clear
	set obs  `=`num' * 2'
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

*drop if pair1 == "ME" | pair2 == "ME"
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
	reg b state_* [aweight=weight] if medicare == 0, nocons
	mat b = e(b)
	mat b = b'
	mat V = e(V)
	mat V = vecdiag(V)
	mat V = V'


	reg b state_* [aweight=weight] if medicare == 1, nocons
	mat b_med = e(b)
	mat b_med = b_med'
	mat V_med = e(V)
	mat V_med = vecdiag(V_med)
	mat V_med = V_med'




	clear
	svmat2 b_joint, rnames(state)
	split state, parse(".")
	replace state1 = "1" if state1 == "_cons"
	drop if state1 == "1b"
	destring state1, replace
	drop state state2
	rename state1 state_orig
	sort state_orig
	replace b_joint1 = b_joint1 + b_joint1[1] if _n != 1
	gen young_elderly = 0
	tempfile tmp_joint
	save `tmp_joint'

	clear
	svmat2 b_joint_med, rnames(state)
	split state, parse(".")
	replace state1 = "1" if state1 == "_cons"
	drop if state1 == "1b"
	destring state1, replace
	drop state state2
	rename state1 state_orig
	sort state_orig
	replace b_joint_med = b_joint_med + b_joint_med[1] if _n != 1
	rename b_joint_med b_joint1
	gen young_elderly = 1

	append using `tmp_joint'
	destring state_orig, replace force
	drop if state_orig == .
	tempfile tmp_joint2
	save `tmp_joint2'

	clear
	svmat2 b_joint_cont, rnames(state)
	split state, parse(".")
	replace state1 = "1" if state1 == "_cons"
	drop if state1 == "1b"
	destring state1, replace
	drop state state2
	rename state1 state_orig
	sort state_orig
	replace b_joint_cont1 = b_joint_cont1 + b_joint_cont1[1] if _n != 1
	gen young_elderly = 0
	tempfile tmp_joint
	save `tmp_joint'

	clear
	svmat2 b_joint_cont_med, rnames(state)
	split state, parse(".")
	replace state1 = "1" if state1 == "_cons"
	drop if state1 == "1b"
	destring state1, replace
	drop state state2
	rename state1 state_orig
	sort state_orig
	replace b_joint_cont_med1 = b_joint_cont_med1 + b_joint_cont_med1[1] if _n != 1
	rename b_joint_cont_med b_joint_cont1
	gen young_elderly = 1

	append using `tmp_joint'
	destring state_orig, replace force
	drop if state_orig == .
	tempfile tmp_joint_cont
	save `tmp_joint_cont'


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

	statastates, abbrev(state_orig) nogen
	/*rename state_orig state
	rename state_fips state_orig*/
	rename medicare young_elderly

	rename state_orig state_abbrev
	rename state_fips state_orig
	merge 1:1 state_orig  young_elderly using `zip_avg'
	drop _merge
	merge m:1 state_orig  young_elderly using `tmp_joint2'
	drop _merge
	merge m:1 state_orig  young_elderly using `tmp_joint_cont'
	drop _merge

	egen mean_b = mean(b1), by(young_elderly)
	replace b1 = b1 - mean_b
	drop mean_b
	egen mean_b = mean(b_joint1), by(young_elderly)
*	replace b_joint1 = b_joint1 - mean_b
	drop mean_b
	egen mean_b = mean(b_joint_cont1), by(young_elderly)
*	replace b_joint_cont1 = b_joint_cont1 - mean_b


*keep if young_elderly == 0
	twoway (scatter `outcome' b1 if young_elderly == 0,  mlab(state_abbrev) mlabpos(0) m(i)) ///
	  (scatter `outcome' b1 if young_elderly == 1,  mlab(state_abbrev) mlabpos(0) m(i)), ///
	  legend(off) name(a, replace)
	twoway (scatter  b_joint_cont1 `outcome' if young_elderly == 0,  mlab(state_abbrev) mlabpos(0) m(i)) ///
	  (lfit  b_joint_cont1 `outcome' if young_elderly == 0) ///
	  (scatter  b_joint_cont1 `outcome' if young_elderly == 1,  mlab(state_abbrev) mlabpos(0) m(i)) ///
	  (lfit  b_joint_cont1 `outcome' if young_elderly == 1), ///  
	  legend(off) name(a, replace)

	gen stderr  = sqrt(V1)
	gen weight = (1/(stderr)^2)
	reg b_joint1 c.`outcome'##i.young_elderly [aweight=weight]


	tabstat b1, by(young_elderly) stat(mean sd)
	tabstat b_joint1, by(young_elderly) stat(mean sd)
	tabstat b_joint_cont1, by(young_elderly) stat(mean sd)

	sum b_joint1 if young_elderly == 0 [aweight=weight]
	local sd0 = r(sd)
	sum b_joint1 if young_elderly == 1 [aweight=weight]
	local sd1 = r(sd)
	local medicare_ratio = string(`sd1'/`sd0', "%9.3f")

	gen state = state_abbrev
	save ../data/`outcome'_state_rd.dta, replace
	
	merge m:1  state using  ../data/theta1_state_data

	sum q_coll_12_lower if young_elderly == 0
	local sd2 = r(sd)
	local medicare_ratio3 = string(`sd1'/`sd2', "%9.3f")

	qui sum q_coll_12_lower
	local x_location = r(max)*0.9
	qui sum b_joint1
	local y_location = r(min)
	sum q_coll_12_lower if young_elderly == 0
	gen q_coll_12_lower2 = q_coll_12_lower - r(mean)
	twoway (scatter b_joint1 q_coll_12_lower2 if young_elderly ==0 , mlab(state) mlabpos(0) m(i)) ///
	  (function y = x if young_elderly == 0, range(q_coll_12_lower2)), ///
	  name( b, replace)  legend(off) xtitle("State Effects without Medicare") ytitle("State Effects with Medicare") ///
	  ylabel(``outcome'_range') xlabel(``outcome'_range') text(`y_location' `x_location'  "{&theta} = `medicare_ratio3'" )
	graph save ../graphs/`outcome'_compare_nocontrols_theta3.gph, replace
	graph export ../graphs/`outcome'_compare_nocontrols_theta3.pdf, replace
/*	
	keep b_joint1 b_joint_cont1   state_abbrev young_elderly
	reshape wide b_joint1 b_joint_cont , i(state_abbrev) j(young_elderly)
	rename state_abbrev state

	qui reg b_joint11 b_joint10, robust
	local b = string(_b[b_joint10], "%9.3f")
	qui sum b_joint10
	local x_location = r(max)*0.9
	qui sum b_joint11
	local y_location = r(min)
	local q_coll_12_range "-100(25)125"
	twoway (scatter b_joint11 b_joint10 , mlab(state) mlabpos(0) m(i)) ///
	  (lfit b_joint11 b_joint10), ///
	  name( b, replace)  legend(off) xtitle("State Effects without Medicare") ytitle("State Effects with Medicare") ///
	  ylabel(``outcome'_range') xlabel(``outcome'_range') text(`y_location' `x_location' "{&beta} = `b'" "{&theta} = `medicare_ratio'" )
	graph save ../graphs/`outcome'_compare_nocontrols.gph, replace
	graph export ../graphs/`outcome'_compare_nocontrols.pdf, replace

	twoway (scatter b_joint_cont11 b_joint_cont10 , mlab(state) mlabpos(0) m(i)) ///
	  (lfit b_joint_cont11 b_joint_cont10), ///
	  name( b, replace)  legend(off) xtitle("State Effects without Medicare") ytitle("State Effects with Medicare") ///
	  ylabel(``outcome'_range') xlabel(``outcome'_range')
	graph export ../graphs/`outcome'_compare_controls.pdf, replace


	maptile b_joint10, geo(state) twopt(name(state_pre,replace))
	mat breaks = r(breaks)
	local breaks  "`=breaks[1,1]'  `=breaks[2,1]' `=breaks[3,1]' `=breaks[4,1]' `=breaks[5,1]'"
	graph export "../graphs/`outcome'_map_pre_nocontrols.pdf", replace
	maptile b_joint11, geo(state) twopt(name(state_post,replace)) cutvalues(`breaks' )
	graph export "../graphs/`outcome'_map_post_nocontrols.pdf", replace

	graph hbar b_joint1*, over(state, sort(1) label(labsize(vsmall)) ) legend(off)
	graph export "../graphs/`outcome'_bar_nocontrols.pdf", replace
	*/
	restore
	}


