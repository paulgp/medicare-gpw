
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

use "`path'/data/final_county_level.dta", clear
keep if inrange(age, 55, 75)
replace age = age - 1


/*** Main RD Figures for

1. Debt COllection
2. Total Debt in Delinquency
3. Bankruptcy
4. Risk Score

****/

local credit_vars q_coll_12 n_coll_12 avg_riskscore balance_cc_delinq balance_mort_delinq


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
	  legend(off) ///
	  xtitle("") ///
	  name(`x'_age, replace) ysize(9) xsize(16)
	sum `x'_lower, d	
	drop medicare `x'_lower `x'_upper
	graph export "~/Dropbox/GPW/graphs/county_`x'_age.pdf", replace 
	}
save ../data/county_collapse_presentation.dta, replace
restore

statastates, abbrev(state) nogen
rename census_code countyfips
rename state_fips statefips
merge m:1 statefips countyfips using ../data/pctui_county_2010.dta

fastxtile pctui_bin = pctui [aweight=population], n(4)

preserve
collapse `credit_vars'   (rawsum) population  [aw=population ], by(age pctui_bin)
drop if pctui_bin == .
gen t = age - 65

foreach x of varlist `credit_vars'  {
	gen medicare = (age-65) >=0
	reg `x' c.t##i.medicare##i.pctui_bin [aweight=population] if inrange(age, 62,68) & age != 65
	estimates store `x'_reg
	replace medicare = 0
	predict `x'_lower if age == 65 
	replace medicare = 1
	predict `x'_upper if age == 65 
	twoway ///
	  (scatter `x' age if age != 65 & age < 65 & pctui_bin == 1, mcolor(dblue) msymbol(O)) ///
	  (scatter `x' age if age != 65 & age > 65  & pctui_bin == 1, mcolor(dred) msymbol(O)) /// 
	  (scatter `x'_lower age if  pctui_bin == 1, mcolor(dblue) mfcolor(none) msymbol(Oh)) ///
	  (scatter `x'_upper age if pctui_bin == 1, mcolor(dred) mfcolor(none) msymbol(Oh))  ///
	  (scatter `x' age if age != 65 & age < 65 & pctui_bin == 4, mcolor(dblue) msymbol(D)) ///
	  (scatter `x' age if age != 65 & age > 65 & pctui_bin == 4, mcolor(dred) msymbol(D)) ///	  
	  (scatter `x'_lower age if pctui_bin == 4, mcolor(dblue)  msymbol(Dh)) ///
	  (scatter `x'_upper age if pctui_bin == 4, mcolor(dred)  msymbol(Dh)) if inrange(age, 55,75), ///	  
	  legend(off) ///
	  xtitle("") ///
	  name(`x'_age, replace) ysize(9) xsize(16)
	sum `x'_lower, d
	drop medicare `x'_lower `x'_upper
	graph export "~/Dropbox/GPW/graphs/county_`x'_age_pctui_bin.pdf", replace 
	}
save ../data/county_collapse_pctui_presentation.dta, replace
restore
