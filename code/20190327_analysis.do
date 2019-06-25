
capture program drop split_figs
program define split_figs
	syntax varlist, bin(name) [ binnum(real 4) bandwidth(real 3) rdpoint(real 65)]
	preserve
	collapse `varlist'   (rawsum) population  [aw=population ], by(age `bin')

	gen t = age - `rdpoint'

	foreach x of varlist `varlist'  {
		gen medicare = (age-`rdpoint') >=0
		reg `x' c.t##i.medicare##i.`bin' [aweight=population] if inrange(age, `=`rdpoint'-`bandwidth'',`=`rdpoint'+`bandwidth'') & age != `rdpoint'
		estimates store `x'_reg
		replace medicare = 0
		predict `x'_lower if age == `rdpoint'
		replace medicare = 1
		predict `x'_upper if age == `rdpoint'
		drop medicare
		}

	drop if `bin' == .
	drop population
	reshape wide `varlist' *_lower *_upper, i(age) j(`bin')

	foreach x in `varlist' {
		local full_list ""
		local full_list_lower ""
		local full_list_upper ""						
		forvalues i = 1/`binnum' {
			local full_list "`full_list' `x'`i' "
			local full_list_lower "`full_list_lower' `x'_lower`i' "
			local full_list_upper "`full_list_upper' `x'_upper`i' "						
			}
		twoway (scatter `full_list' age if age != 65 & age < 65, mcolor(dblue dblue dblue dblue dblue ) msymbol(O D T S)) ///
		  (scatter `full_list'  age if age != 65 & age > 65, mcolor(dred dred dred dred dred) msymbol(O D T S )) ///	  
		  (scatter `full_list_lower'  age, mcolor(dblue dblue dblue dblue dblue) mfcolor(none none none none none) msymbol(O D T S )) ///
		  (scatter `full_list_upper'  age , mcolor(dred dred dred dred dred) mfcolor(none none none none none) msymbol(O D T S )) if inrange(age, 55,75), ///
		  legend(off) 	  name(`x'_age, replace)
		  graph export "~/Dropbox/GPW/graphs/`x'_age_`bin'.pdf", replace
		}

	foreach x in `varlist' {
		twoway (scatter `x'1  `x'`binnum' age if age != 65 & age < 65, mcolor(dblue dblue dblue dblue dblue ) msymbol(O  S)) ///
		  (scatter `x'1  `x'`binnum' age if age != 65 & age > 65, mcolor(dred dred dred dred dred) msymbol(O  S)) ///	  
		  (scatter `x'_lower1  `x'_lower`binnum' age, mcolor(dblue dblue dblue dblue dblue) mfcolor(none none none none none) msymbol(O  S)) ///
		  (scatter `x'_upper1  `x'_upper`binnum' age , mcolor(dred dred dred dred dred) mfcolor(none none none none none) msymbol(O  S)) if inrange(age, 55,75), ///
		  legend(off) 	  name(`x'_age2, replace)
		  graph export "~/Dropbox/GPW/graphs/`x'_age_`bin'2.pdf", replace
		}

	restore

end
	


capture program drop split_fig_iterate
program define split_fig_iterate
	syntax varlist, bin(name) [ binnum(real 4) bandwidth(real 3) rdpointstart(real 60) rdpointend(real 70)]

	preserve
	collapse `varlist'   (rawsum) population  [aw=population ], by(age `bin')

	forvalues rdpoint = `rdpointstart'/`rdpointend' {
		gen t = age - `rdpoint'	
		foreach x of varlist `varlist'  {
			gen medicare = (age-`rdpoint') >=0
			reg `x' c.t##i.medicare##i.pctui_bin [aweight=population] if inrange(age, `=`rdpoint'-`bandwidth'',`=`rdpoint'+`bandwidth'') & age != `rdpoint'
			estimates store `x'_reg
			replace medicare = 0
			predict `x'_lower if age == `rdpoint'
			replace medicare = 1
			predict `x'_upper if age == `rdpoint'
			drop medicare
			gen diff =  `x'_upper  - `x'_lower
			forvalues bin_count = 1/`binnum' {
				sum diff [aweight=population] if `bin' == `bin_count' 
				mat est_`x' = (nullmat(est_`x'), (r(mean) \ `rdpoint' \ `bin_count'))
				}
			drop diff
			drop `x'_lower `x'_upper
			}
		drop t
		}
	restore


end



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

/*
use `credit_vars' zipcode state quarter population age using  "`path'/data/final_zip_level.dta", clear

keep if inrange(age, 31, 91)
replace age = age - 1

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
egen state_zip_year = group(state zip year)

/** make Bankruptcy in percentage points **/
replace new_bpt = new_bpt * 100



fastxtile pctui_bin = pctui_young [aweight=population], n(4)

split_fig_iterate `credit_vars', bin(pctui_bin) rdpointstart(35) rdpointend(85)

preserve
foreach x of varlist `credit_vars' {
	clear
*	mat est_`x' = est_`x''
	svmat est_`x'
	reshape wide est_`x'1 , i(est_`x'2 ) j(est_`x'3)
	rename est_`x'2 age
	forvalues i = 1/4 {
		sum est_`x'1`i'  if age == 65
		local mean_`i' = r(mean)
		twoway hist est_`x'1`i' if age != 65, bin(10) fcolor(dblue) lcolor(white) percent name(hist_`x'_`i', replace)  xline(`mean_`i'', lcolor(dred)) xlabel(#7 `mean_`i'' `"" ""True" "Value""' )  plotregion(margin(b=0)) xtitle("") ytitle("")
		graph export "~/Dropbox/GPW/graphs/hist_`x'_`i'.pdf", replace
		}
	}
restore

*/

*use "`path'/data/final_zip_level.dta", clear
*keep if inrange(age, 50, 80)
*save "`path'/data/final_zip_level5080.dta", replace

use "`path'/data/final_zip_level5080.dta", clear
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
egen state_zip_year = group(state zip year)

/** make Bankruptcy in percentage points **/
replace new_bpt = new_bpt * 100



preserve
save `credit_vars' "`path'/data/final_zip_level5080_clean.dta", replace
restore


/*** Main RD Figures for

1. Debt COllection
2. Total Debt in Delinquency
3. Bankruptcy
4. Risk Score

****/

local credit_vars q_coll_12 new_bpt new_fc balance_ttl_delinq avg_riskscore  balance_mort_delinq balance_cc_delinq



/*
preserve
collapse `credit_vars'   (rawsum) population  [aw=population ], by(age)


gen t = age - 65

foreach x of varlist `credit_vars'  {
	gen medicare = (age-65) >=0
	reg `x' c.t##i.medicare [aweight=population] if inrange(age, 62,68) & age != 65
	estimates store `x'_reg
	replace medicare = 0
	predict `x'_lower if age == 65 
	replace medicare = 1
	predict `x'_upper if age == 65 
	twoway (scatter `x' age if age != 65 & age < 65, mcolor(dblue)) ///
	  (scatter `x' age if age != 65 & age > 65, mcolor(dred)) ///	  
	  (scatter `x'_lower age , mcolor(dblue) mfcolor(none)) ///
	  (scatter `x'_upper age , mcolor(dred) mfcolor(none)) if inrange(age, 55,75), ///
	  legend(label(1 "Data") ///
	  label(3 "Imputed Without Medicare") ///
	  label(4 "Imputed With Medicare") ///
	  order(1 3 4)) ///
	  xlabel("") ///
	  name(`x'_age, replace)
	drop medicare `x'_lower `x'_upper
	graph export "~/Dropbox/GPW/graphs/`x'_age.pdf", replace
	}

restore
*/
/*** Split main figures by uninsurance quartiles ***/


fastxtile pctui_bin = pctui_young [aweight=population], n(4)

split_figs `credit_vars', bin(pctui_bin) rdpoint(65)

/*** Same exercise but with race ***/

fastxtile race_bin = black_share [aweight=population], n(4)

split_figs `credit_vars', bin(race_bin) rdpoint(65) 


/*** Same exercise but with poverty_share for near elderly ***/

destring povshare, replace force
destring povsharenearelderly, replace force
fastxtile pov_bin = povsharenearelderly [aweight=population], n(4)

split_figs `credit_vars', bin(pov_bin) rdpoint(65)



/*** Overall Poverty rate **/
fastxtile povb_bin = povshare [aweight=population], n(4)

split_figs `credit_vars', bin(povb_bin) rdpoint(65) 


/*
keep if inrange(age, 60,70)
tsset state_zip_year age
tsfill


foreach x of varlist q_coll_12 new_bpt balance_ttl_delinq  {
	bys state_zip_year (age): gen delta_`x' = (`x' - `x'[_n-1])/ (age - age[_n-1])
	bys state_zip_year (age): gen `x'_adj = `x'[_n-1] + delta_`x'[_n-1]
	bys state_zip_year (age): gen `x'_adj2 = `x'_adj[_n+1]	
	bys state_zip_year (age): gen `x'_forward_adj = `x'[_n+1] - delta_`x'[_n+2]
	bys state_zip_year (age): gen `x'_forward_adj2 = `x'_forward_adj[_n-1]
}

gen population2 = population
bys state_zip_year (age): replace population2 = population[_n-1] if population == .

preserve
collapse q_coll_12 new_bpt balance_ttl_delinq   *_adj [aw=population2 ], by(age)
foreach x in q_coll_12 new_bpt balance_ttl_delinq  {
	twoway (scatter `x' age, mcolor(dblue)) ///
	  (scatter `x'_adj age if age == 65, mcolor(dred)) ///
	  (scatter `x'_forward_adj age if age == 65, mcolor(dred)) if inrange(age, 60, 70), ///
	  legend(label(1 "`x'") label(2 "`x' adjusted for age trend") order(1 2))
	graph export "~/Dropbox/GPW/graphs/`x'_age_adj.pdf", replace
	}
restore



/*** This just doesn't work, because the inferrin within zip code is too noisy **/
gen q_coll_12_adj_65 = q_coll_12_adj2 if age == 64
replace q_coll_12_adj_65 = q_coll_12_forward_adj2 if age == 66

/*** This is within bin and works well! ***/
fastxtile pctui_bin = pctui_young if age == 63 | age == 64 | age == 66 | age == 67 [aweight = population], n(35)
egen pctui_bin2 = max(pctui_bin), by(zip)
replace pctui_bin = pctui_bin2

foreach x of varlist q_coll_12 new_bpt balance_ttl_delinq {
	gen `x'_lin_adj = `x'
	gen `x'_lin_adj2 = `x'
	forvalues i = 1/35 {
		reg `x' age [aweight=population] if (age == 63 | age == 64 ) & pctui_bin == `i'
		bys state_zip_year (age): replace `x'_lin_adj = `x'_lin_adj[_n-1] + _b[age]*(65 -age[_n-1] ) if pctui_bin == `i' & age == 65
		reg `x' age [aweight=population] if (age == 66 | age == 67 ) & pctui_bin == `i'
		bys state_zip_year (age): replace `x'_lin_adj2 = `x'_lin_adj2[_n+1] + _b[age]*(65 -age[_n+1] ) if pctui_bin == `i' & age == 65
		}
	}

/** Imputed datapoint **/
binscatter q_coll_12 q_coll_12_lin_adj* age [aweight=population ], discrete linetype(none)


/** Now heterogeneity **/
preserve
keep if age == 65
keep *_lin_adj* population pctui_young state_zip_year
rename (*_lin_adj) (*_lin_adj1)
reshape long q_coll_12_lin_adj new_bpt_lin_adj balance_ttl_delinq_lin_adj, i(state_zip_year pctui population) j(side)
binsreg q_coll_12_lin_adj pctui_young [aweight=population], by(side) nbins(35)
restore


/*** within race bin ***/
fastxtile race_bin = black_share if age == 63 | age == 64 | age == 66 | age == 67 [aweight = population], n(35)
egen race_bin2 = max(race_bin), by(zip)
replace race_bin = race_bin2

foreach x of varlist q_coll_12 new_bpt balance_ttl_delinq {
	gen `x'_race_lin_adj = `x'
	gen `x'_race_lin_adj2 = `x'
	forvalues i = 1/35 {
		qui reg `x' age [aweight=population] if (age == 63 | age == 64 ) & race_bin == `i'
		bys state_zip_year (age): replace `x'_race_lin_adj = `x'_race_lin_adj[_n-1] + _b[age]*(65 -age[_n-1] ) if race_bin == `i' & age == 65
		qui reg `x' age [aweight=population] if (age == 66 | age == 67 ) & race_bin == `i'
		bys state_zip_year (age): replace `x'_race_lin_adj2 = `x'_race_lin_adj2[_n+1] + _b[age]*(65 -age[_n+1] ) if race_bin == `i' & age == 65
		}
	}

/** Imputed datapoint **/
binscatter q_coll_12 q_coll_12_lin_adj* q_coll_12_race_lin_adj age [aweight=population ], discrete linetype(none)


/** Now heterogeneity **/
preserve
keep if age == 65
keep *_lin_adj* *_race_lin_adj* population black_share state_zip_year
rename (*_lin_adj) (*_lin_adj1)
reshape long q_coll_12_race_lin_adj new_bpt_race_lin_adj balance_ttl_delinq_race_lin_adj q_coll_12_lin_adj new_bpt_lin_adj balance_ttl_delinq_lin_adj, i(state_zip_year) j(side)
binsreg q_coll_12_lin_adj pctui_young [aweight=population], by(side) nbins(35)

restore

drop race_bin2
fastxtile race_bin2 = black_share [aweight=population], n(5)
drop pctui_bin2
fastxtile pctui_bin2 = pctui_young [aweight=population], n(5)

binscatter q_coll_12 q_coll_12_lin_adj* q_coll_12_race_lin_adj age [aweight=population ], discrete linetype(none) by(race_bin2)


/*
egen pctui_2006b = max(pctui) if year == 2006, by(countyfips statefips)
egen pctui_2006 = max(pctui_2006b) , by(countyfips statefips)

egen pctui_2015b = max(pctui) if year == 2015, by(countyfips statefips)
egen pctui_2015 = max(pctui_2015b) , by(countyfips statefips)

replace pctui = pctui_2006 if year < 2006
replace pctui = pctui_2015 if year > 2015
*/


/*** TODO:
1. make age RD figs
2. Make binscatter by PCTUI
3. make binscatter by race
4. Get povery rate
5. make binscatter by poverty

/*

fastxtile pctui_quintile = pctui_young [aw=population], n(5)

gen medicare = age > 65

preserve
collapse avg_riskscore balance_ttl - new_fc (rawsum) pop=population  [aw=population ], by(age pctui_quintile  )


drop if pctui_quintile == .
reshape wide  pop avg_riskscore -new_fc , i(age) j(pctui_quintile)

foreach x in q_coll_12  new_bpt {
	twoway (scatter `x'1 `x'2 `x'3 `x'4 `x'5  age , mcolor(dblue dblue dblue dblue dblue) msymbol(O D T S +)) ///
	  (scatter `x'1 `x'2 `x'3 `x'4 `x'5 age if age >= 66 , mcolor(dred dred dred dred dred) msymbol(O D T S +)), ///
	  legend(off) name(`x', replace)
}

foreach x of varlist avg_riskscore -new_fc {	
	twoway (scatter `x'  age if pctui_quintile <= 1, mcolor(dblue)) ///
	  (scatter `x'  age if age >= 66 & pctui_quintile <= 1, mcolor(dred)) ///
	  (scatter `x'  age if pctui_quintile >=5, mcolor(dblue) msymbol(diamond)) ///
	  (scatter `x'  age if age >= 66 & pctui_quintile >= 5, mcolor(dred) msymbol(diamond)), xline(66) name(`x', replace)
}




preserve
collapse `credit_vars'   (rawsum) population  [aw=population ], by(age pctui_bin)


gen t = age - 65

foreach x of varlist `credit_vars'  {
	gen medicare = (age-65) >=0
	reg `x' c.t##i.medicare##i.pctui_bin [aweight=population] if inrange(age, 62,68) & age != 65
	estimates store `x'_reg
	replace medicare = 0
	predict `x'_lower if age == 65 
	replace medicare = 1
	predict `x'_upper if age == 65
	drop medicare
	}

drop if pctui_bin == .
drop population
reshape wide `credit_vars' *_lower *_upper, i(age) j(pctui_bin)

foreach x in `credit_vars' {
	twoway (scatter `x'1 `x'2 `x'3 `x'4 age if age != 65 & age < 65, mcolor(dblue dblue dblue dblue dblue ) msymbol(O D T S)) ///
	  (scatter `x'1 `x'2 `x'3 `x'4  age if age != 65 & age > 65, mcolor(dred dred dred dred dred) msymbol(O D T S )) ///	  
	  (scatter `x'_lower1 `x'_lower2 `x'_lower3 `x'_lower4  age, mcolor(dblue dblue dblue dblue dblue) mfcolor(none none none none none) msymbol(O D T S )) ///
	  (scatter `x'_upper1 `x'_upper2 `x'_upper3 `x'_upper4  age , mcolor(dred dred dred dred dred) mfcolor(none none none none none) msymbol(O D T S )) if inrange(age, 55,75), ///
	  legend(off) 	  name(`x'_age_pctui, replace)
	/* legend(label(1 "Data") /// */
	/*   label(3 "Imputed Without Medicare") /// */
	/*   label(4 "Imputed With Medicare") /// */
	/*   order(1 3 4)) /// */
	/*   xlabel("") /// */
	graph export "~/Dropbox/GPW/graphs/`x'_age_pctui.pdf", replace
	}

foreach x in `credit_vars' {
	twoway (scatter `x'1  `x'4 age if age != 65 & age < 65, mcolor(dblue dblue dblue dblue dblue ) msymbol(O  S)) ///
	  (scatter `x'1  `x'4 age if age != 65 & age > 65, mcolor(dred dred dred dred dred) msymbol(O  S)) ///	  
	  (scatter `x'_lower1  `x'_lower4 age, mcolor(dblue dblue dblue dblue dblue) mfcolor(none none none none none) msymbol(O  S)) ///
	  (scatter `x'_upper1  `x'_upper4 age , mcolor(dred dred dred dred dred) mfcolor(none none none none none) msymbol(O  S)) if inrange(age, 55,75), ///
	  legend(off) 	  name(`x'_age_pctui, replace)

	/* legend(label(1 "Data") /// */
	/*   label(3 "Imputed Without Medicare") /// */
	/*   label(4 "Imputed With Medicare") /// */
	/*   order(1 3 4)) /// */
	/*   xlabel("") /// */
	graph export "~/Dropbox/GPW/graphs/`x'_age_pctui2.pdf", replace
	}

restore


preserve
collapse `credit_vars'   (rawsum) population  [aw=population ], by(age race_bin)


gen t = age - 65

foreach x of varlist `credit_vars'  {
	gen medicare = (age-65) >=0
	reg `x' c.t##i.medicare##i.race_bin [aweight=population] if inrange(age, 62,68) & age != 65
	estimates store `x'_reg
	replace medicare = 0
	predict `x'_lower if age == 65 
	replace medicare = 1
	predict `x'_upper if age == 65
	drop medicare
	}

drop if race_bin == .
drop population
reshape wide `credit_vars' *_lower *_upper, i(age) j(race_bin)

foreach x in `credit_vars' {
	twoway (scatter `x'1 `x'2 `x'3 `x'4 age if age != 65 & age < 65, mcolor(dblue dblue dblue dblue dblue ) msymbol(O D T S)) ///
	  (scatter `x'1 `x'2 `x'3 `x'4  age if age != 65 & age > 65, mcolor(dred dred dred dred dred) msymbol(O D T S )) ///	  
	  (scatter `x'_lower1 `x'_lower2 `x'_lower3 `x'_lower4  age, mcolor(dblue dblue dblue dblue dblue) mfcolor(none none none none none) msymbol(O D T S )) ///
	  (scatter `x'_upper1 `x'_upper2 `x'_upper3 `x'_upper4  age , mcolor(dred dred dred dred dred) mfcolor(none none none none none) msymbol(O D T S)) if inrange(age, 55,75), ///
	  legend(off) 	  name(`x'_age_race, replace)
	/* legend(label(1 "Data") /// */
	/*   label(3 "Imputed Without Medicare") /// */
	/*   label(4 "Imputed With Medicare") /// */
	/*   order(1 3 4)) /// */
	/*   xlabel("") /// */
	graph export "~/Dropbox/GPW/graphs/`x'_age_race.pdf", replace
	}

foreach x in `credit_vars' {
	twoway (scatter `x'1  `x'4 age if age != 65 & age < 65, mcolor(dblue dblue dblue dblue dblue ) msymbol(O  S)) ///
	  (scatter `x'1  `x'4 age if age != 65 & age > 65, mcolor(dred dred dred dred dred) msymbol(O  S)) ///	  
	  (scatter `x'_lower1  `x'_lower4 age, mcolor(dblue dblue dblue dblue dblue) mfcolor(none none none none none) msymbol(O  S)) ///
	  (scatter `x'_upper1  `x'_upper4 age , mcolor(dred dred dred dred dred) mfcolor(none none none none none) msymbol(O  S)) if inrange(age, 55,75), ///
	  legend(off) 	  name(`x'_age_race, replace)

	/* legend(label(1 "Data") /// */
	/*   label(3 "Imputed Without Medicare") /// */
	/*   label(4 "Imputed With Medicare") /// */
	/*   order(1 3 4)) /// */
	/*   xlabel("") /// */
	graph export "~/Dropbox/GPW/graphs/`x'_age_race2.pdf", replace
	}

restore




preserve
collapse `credit_vars'   (rawsum) population  [aw=population ], by(age pov_bin)


gen t = age - 65

foreach x of varlist `credit_vars'  {
	gen medicare = (age-65) >=0
	reg `x' c.t##i.medicare##i.pov_bin [aweight=population] if inrange(age, 62,68) & age != 65
	estimates store `x'_reg
	replace medicare = 0
	predict `x'_lower if age == 65 
	replace medicare = 1
	predict `x'_upper if age == 65
	drop medicare
	}

drop if pov_bin == .
drop population
reshape wide `credit_vars' *_lower *_upper, i(age) j(pov_bin)

foreach x in `credit_vars'  {
	twoway (scatter `x'1 `x'2 `x'3 `x'4 age if age != 65 & age < 65, mcolor(dblue dblue dblue dblue dblue ) msymbol(O D T S)) ///
	  (scatter `x'1 `x'2 `x'3 `x'4  age if age != 65 & age > 65, mcolor(dred dred dred dred dred) msymbol(O D T S )) ///	  
	  (scatter `x'_lower1 `x'_lower2 `x'_lower3 `x'_lower4  age, mcolor(dblue dblue dblue dblue dblue) mfcolor(none none none none none) msymbol(O D T S )) ///
	  (scatter `x'_upper1 `x'_upper2 `x'_upper3 `x'_upper4  age , mcolor(dred dred dred dred dred) mfcolor(none none none none none) msymbol(O D T S )) if inrange(age, 55,75), ///
	  legend(off) 	  name(`x'_age_pov, replace)
	/* legend(label(1 "Data") /// */
	/*   label(3 "Imputed Without Medicare") /// */
	/*   label(4 "Imputed With Medicare") /// */
	/*   order(1 3 4)) /// */
	/*   xlabel("") /// */
	graph export "~/Dropbox/GPW/graphs/`x'_age_pov.pdf", replace
	}

foreach x in `credit_vars'  {
	twoway (scatter `x'1  `x'4 age if age != 65 & age < 65, mcolor(dblue dblue dblue dblue dblue ) msymbol(O  S)) ///
	  (scatter `x'1  `x'4 age if age != 65 & age > 65, mcolor(dred dred dred dred dred) msymbol(O  S)) ///	  
	  (scatter `x'_lower1  `x'_lower4 age, mcolor(dblue dblue dblue dblue dblue) mfcolor(none none none none none) msymbol(O  S)) ///
	  (scatter `x'_upper1  `x'_upper4 age , mcolor(dred dred dred dred dred) mfcolor(none none none none none) msymbol(O  S)) if inrange(age, 55,75), ///
	  legend(off) 	  name(`x'_age_pov, replace)

	/* legend(label(1 "Data") /// */
	/*   label(3 "Imputed Without Medicare") /// */
	/*   label(4 "Imputed With Medicare") /// */
	/*   order(1 3 4)) /// */
	/*   xlabel("") /// */
	graph export "~/Dropbox/GPW/graphs/`x'_age_pov2.pdf", replace
	}

restore


preserve
collapse `credit_vars'    (rawsum) population  [aw=population ], by(age pov2_bin)


gen t = age - 65

foreach x of varlist `credit_vars'   {
	gen medicare = (age-65) >=0
	reg `x' c.t##i.medicare##i.pov2_bin [aweight=population] if inrange(age, 60,70) & age != 65
	estimates store `x'_reg
	replace medicare = 0
	predict `x'_lower if age == 65 
	replace medicare = 1
	predict `x'_upper if age == 65
	drop medicare
	}

drop if pov2_bin == .
drop population
reshape wide `credit_vars'  *_lower *_upper, i(age) j(pov2_bin)

foreach x in `credit_vars'  {
	twoway (scatter `x'1 `x'2 `x'3 `x'4 age if age != 65 & age < 65, mcolor(dblue dblue dblue dblue dblue ) msymbol(O D T S)) ///
	  (scatter `x'1 `x'2 `x'3 `x'4  age if age != 65 & age > 65, mcolor(dred dred dred dred dred) msymbol(O D T S )) ///	  
	  (scatter `x'_lower1 `x'_lower2 `x'_lower3 `x'_lower4  age, mcolor(dblue dblue dblue dblue dblue) mfcolor(none none none none none) msymbol(O D T S )) ///
	  (scatter `x'_upper1 `x'_upper2 `x'_upper3 `x'_upper4  age , mcolor(dred dred dred dred dred) mfcolor(none none none none none) msymbol(O D T S )) if inrange(age, 55,75), ///
	  legend(off) 	  name(`x'_age_povb, replace)
	/* legend(label(1 "Data") /// */
	/*   label(3 "Imputed Without Medicare") /// */
	/*   label(4 "Imputed With Medicare") /// */
	/*   order(1 3 4)) /// */
	/*   xlabel("") /// */
	graph export "~/Dropbox/GPW/graphs/`x'_age_povb.pdf", replace
	}

foreach x in `credit_vars'  {
	twoway (scatter `x'1  `x'4 age if age != 65 & age < 65, mcolor(dblue dblue dblue dblue dblue ) msymbol(O  S)) ///
	  (scatter `x'1  `x'4 age if age != 65 & age > 65, mcolor(dred dred dred dred dred) msymbol(O  S)) ///	  
	  (scatter `x'_lower1  `x'_lower4 age, mcolor(dblue dblue dblue dblue dblue) mfcolor(none none none none none) msymbol(O  S)) ///
	  (scatter `x'_upper1  `x'_upper4 age , mcolor(dred dred dred dred dred) mfcolor(none none none none none) msymbol(O  S)) if inrange(age, 55,75), ///
	  legend(off) 	  name(`x'_age_povb, replace)

	/* legend(label(1 "Data") /// */
	/*   label(3 "Imputed Without Medicare") /// */
	/*   label(4 "Imputed With Medicare") /// */
	/*   order(1 3 4)) /// */
	/*   xlabel("") /// */
	graph export "~/Dropbox/GPW/graphs/`x'_age_povb_2.pdf", replace
	}

restore
