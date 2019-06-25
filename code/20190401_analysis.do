
clear all
set matsize 2000


capture program drop split_figs
program define split_figs
	syntax varlist, bin(name) [ binnum(real 4) bandwidth(real 3) rdpoint(real 65)]
	preserve
	collapse `varlist'   (rawsum) population  [aw=population ], by(age `bin')

	gen t = age - `rdpoint'

	foreach x of varlist `varlist'  {
		gen medicare = (age-`rdpoint') >=0
		reg `x' c.t##i.medicare##i.`bin' [aweight=population] if inrange(age, `=`rdpoint'-`bandwidth'',`=`rdpoint'+`bandwidth'') & age != `rdpoint'
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
	syntax varlist, [bin(name)  binnum(real 4) bandwidth(real 3) rdpointstart(real 60) rdpointend(real 70)  regtype(name)]
	mat drop _all
	preserve
	collapse `varlist'   (rawsum) population  [aw=population ], by(age `bin')

	forvalues rdpoint = `rdpointstart'/`rdpointend' {
		gen t = age - `rdpoint'
		gen t_sq = t^2
		foreach x of varlist `varlist'  {
			gen medicare = (age-`rdpoint') >=0
			if "`bin'" != "" {
				forvalues i = 1/`binnum' {
					if "`regtype'" != "" {
						reg `x' c.(t t_sq)##i.medicare##ib`i'.`bin' [aweight=population] if inrange(age, `=`rdpoint'-`bandwidth'',`=`rdpoint'+`bandwidth'') & age != `rdpoint', robust
						}
					else {
						reg `x' c.(t)##i.medicare##ib`i'.`bin' [aweight=population] if inrange(age, `=`rdpoint'-`bandwidth'',`=`rdpoint'+`bandwidth'') & age != `rdpoint', robust
						}
					local se_`i' = _se[1.medicare]
					local b_`i' = _b[1.medicare]
					}
				}
			else {
				if "`regtype'" != "" {
					reg `x' c.(t t_sq)##i.medicare [aweight=population] if inrange(age, `=`rdpoint'-`bandwidth'',`=`rdpoint'+`bandwidth'') & age != `rdpoint', robust
					}
				else {
					reg `x' c.(t)##i.medicare [aweight=population] if inrange(age, `=`rdpoint'-`bandwidth'',`=`rdpoint'+`bandwidth'') & age != `rdpoint', robust
					}
				local se_1 = _se[1.medicare]
				local b_1 = _b[1.medicare]				
				}
			estimates store `x'_reg
			replace medicare = 0
			predict `x'_lower if age == `rdpoint'
			replace medicare = 1
			predict `x'_upper if age == `rdpoint'
			drop medicare
			gen diff =  `x'_upper  - `x'_lower
			if "`bin'" != "" {
				forvalues bin_count = 1/`binnum' {
					sum diff [aweight=population] if `bin' == `bin_count' 
					mat est_`x' = (nullmat(est_`x'), (r(mean) \ `b_`bin_count'' \ `se_`bin_count'' \ `rdpoint' \ `bin_count'))
					}
				}
			else {
				sum diff [aweight=population]
				local bin_count = 1
				mat est_`x' = (nullmat(est_`x'), (r(mean) \ `b_`bin_count'' \ `se_`bin_count'' \ `rdpoint' \ `bin_count'))
				}
			drop diff
			drop `x'_lower `x'_upper
			}
		drop t t_sq
		}
	foreach x in `varlist' {
		mat est_`x' = est_`x''
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


/*** Main Results, Bandwith = 3, Linear **/

foreach bandwidth in 3 5 7 {
	foreach model in "li" "qu" {
		if "`model'" == "li" {
			split_fig_iterate `credit_vars', rdpointstart(35) rdpointend(85) bandwidth(`bandwidth')
			}
		else {
			split_fig_iterate `credit_vars', rdpointstart(35) rdpointend(85) bandwidth(`bandwidth') regtype("`model'")
			}
		preserve
		foreach x in  `credit_vars' {
			clear
			svmat est_`x'
			rename est_`x'4 age
			rename est_`x'5 bin
			rename est_`x'2 b_`x'
			rename est_`x'3 se_`x'
			sort bin b_`x'
			by bin: gen cdf_share = _n/_N
			forvalues i = 1/1 {
				sum b_`x'  if age == 65 & bin == `i'
				local mean_`i' = r(mean)
				twoway hist b_`x' if age != 65 & bin == `i', bin(10) fcolor(dblue) lcolor(white) percent name(hist_`x'_`i', replace)  xline(`mean_`i'', lcolor(dred)) xlabel(#7 `mean_`i'' `"" ""True" "Value""' )  plotregion(margin(b=0)) xtitle("") ytitle("")
				graph export "~/Dropbox/GPW/graphs/hist_`x'_0_bd`bandwidth'_`model'.pdf", replace
				twoway line cdf_share b_`x' if bin == `i', color(dblue) name(cdf_`x'_`i', replace)  yline(0.025 0.975, lcolor(black ) lpattern(dash)) xline(`mean_`i'', lcolor(dred)) xlabel(#7 `mean_`i'' `"" ""True" "Value""' )  plotregion(margin(b=0)) xtitle("") ytitle("")
				graph export "~/Dropbox/GPW/graphs/cdf_`x'_0_bd`bandwidth'_`model'.pdf", replace
				
				}
			sum b_`x' if age == 65 & bin == 1
			local b_`x'_0_`bandwidth'_`model' = string(r(mean), "%9.3f")
			sum se_`x' if age == 65 & bin == 1
			local se_`x'_0_`bandwidth'_`model'=  string(r(mean), "%9.3f")
			sum cdf_share if age == 65 & bin == 1
			local p_`x'_0_`bandwidth'_`model' = string(r(mean), "%9.3f")	
			}
		restore
		}
	}

capture file close fh
file open fh using "~/Dropbox/GPW/tables/main_results.tex", replace write

foreach bandwidth in 3 5 7 {
	foreach model in "li" "qu" {
		local line1 ""
		local line2 ""
		local line3 ""
		local line4 ""
		foreach x in `credit_vars' {
			local line1 "`line1' & `x' "
			local line2 "`line2' & `b_`x'_0_`bandwidth'_`model''"
			local line3 "`line3' & (`se_`x'_0_`bandwidth'_`model'')"
			local line4 "`line4' & [`p_`x'_0_`bandwidth'_`model''] "
			}
		file write fh "\multicolumn{8}{c}{`model' bandwidth: `bandwidth'}\\" _n
		file write fh "`line1'\\" _n
		file write fh "`line2'\\" _n
		file write fh "`line3'\\" _n
		file write fh "`line4'\\" _n
		}
	}
file close fh

fastxtile pctui_bin = pctui_young [aweight=population], n(4)
fastxtile race_bin = black_share [aweight=population], n(4)
destring povshare, replace force
fastxtile pov_bin = povshare [aweight=population], n(4)


/** Percent UI **/
foreach bandwidth in 3 {
	foreach model in "li"  {
		if "`model'" == "li" {
			split_fig_iterate `credit_vars', rdpointstart(35) rdpointend(85) bandwidth(`bandwidth') bin(pctui_bin)
			}
		else {
			split_fig_iterate `credit_vars', rdpointstart(35) rdpointend(85) bandwidth(`bandwidth') regtype("`model'") bin(pctui_bin)
			}
		preserve
		foreach x in  `credit_vars' {
			clear
			svmat est_`x'
			rename est_`x'4 age
			rename est_`x'5 bin
			rename est_`x'2 b_`x'
			rename est_`x'3 se_`x'
			sort bin b_`x'
			by bin: gen cdf_share = _n/_N
			forvalues i = 1/4 {
				sum b_`x'  if age == 65 & bin == `i'
				local mean_`i' = r(mean)
				twoway hist b_`x' if age != 65 & bin == `i', bin(10) fcolor(dblue) lcolor(white) percent name(hist_`x'_`i', replace)  xline(`mean_`i'', lcolor(dred)) xlabel(#7 `mean_`i'' `"" ""True" "Value""' )  plotregion(margin(b=0)) xtitle("") ytitle("")
				graph export "~/Dropbox/GPW/graphs/hist_`x'_0_bd`bandwidth'_`model'.pdf", replace
				twoway line cdf_share b_`x' if bin == `i', color(dblue) name(cdf_`x'_`i', replace)  yline(0.025 0.975, lcolor(black ) lpattern(dash)) xline(`mean_`i'', lcolor(dred)) xlabel(#7 `mean_`i'' `"" ""True" "Value""' )  plotregion(margin(b=0)) xtitle("") ytitle("")
				graph export "~/Dropbox/GPW/graphs/cdf_`x'_0_bd`bandwidth'_`model'.pdf", replace
				sum b_`x' if age == 65 & bin == `i'
				local b_`x'_`i'_`bandwidth'_`model' = string(r(mean), "%9.3f")
				sum se_`x' if age == 65 & bin == `i'
				local se_`x'_`i'_`bandwidth'_`model'=  string(r(mean), "%9.3f")
				sum cdf_share if age == 65 & bin == `i'
				local p_`x'_`i'_`bandwidth'_`model' = string(r(mean), "%9.3f")	
				}
			}
		restore
		}
	}

capture file close fh
file open fh using "~/Dropbox/GPW/tables/main_results_pctui.tex", replace write

foreach bandwidth in 3 {
	foreach model in "li" {
		forvalues i = 1/4 {
			local line1 ""
			local line2 ""
			local line3 ""
			local line4 ""
			foreach x in `credit_vars' {
				local line1 "`line1' & `x' "
				local line2 "`line2' & `b_`x'_`i'_`bandwidth'_`model''"
				local line3 "`line3' & (`se_`x'_`i'_`bandwidth'_`model'')"
				local line4 "`line4' & [`p_`x'_`i'_`bandwidth'_`model''] "
				}
			file write fh "\multicolumn{8}{c}{`model' bandwidth: `bandwidth'}\\" _n
			file write fh "\multicolumn{8}{c}{PCTUI bin = `i'}\\" _n		
			file write fh "`line1'\\" _n
			file write fh "`line2'\\" _n
			file write fh "`line3'\\" _n
			file write fh "`line4'\\" _n
			}
		}
	}
file close fh


/** Race **/
foreach bandwidth in 3 {
	foreach model in "li"  {
		if "`model'" == "li" {
			split_fig_iterate `credit_vars', rdpointstart(35) rdpointend(85) bandwidth(`bandwidth') bin(race_bin)
			}
		else {
			split_fig_iterate `credit_vars', rdpointstart(35) rdpointend(85) bandwidth(`bandwidth') regtype("`model'") bin(race_bin)
			}
		preserve
		foreach x in  `credit_vars' {
			clear
			svmat est_`x'
			rename est_`x'4 age
			rename est_`x'5 bin
			rename est_`x'2 b_`x'
			rename est_`x'3 se_`x'
			sort bin b_`x'
			by bin: gen cdf_share = _n/_N
			forvalues i = 1/4 {
				sum b_`x'  if age == 65 & bin == `i'
				local mean_`i' = r(mean)
				twoway hist b_`x' if age != 65 & bin == `i', bin(10) fcolor(dblue) lcolor(white) percent name(hist_`x'_`i', replace)  xline(`mean_`i'', lcolor(dred)) xlabel(#7 `mean_`i'' `"" ""True" "Value""' )  plotregion(margin(b=0)) xtitle("") ytitle("")
				graph export "~/Dropbox/GPW/graphs/hist_`x'_0_bd`bandwidth'_`model'.pdf", replace
				twoway line cdf_share b_`x' if bin == `i', color(dblue) name(cdf_`x'_`i', replace)  yline(0.025 0.975, lcolor(black ) lpattern(dash)) xline(`mean_`i'', lcolor(dred)) xlabel(#7 `mean_`i'' `"" ""True" "Value""' )  plotregion(margin(b=0)) xtitle("") ytitle("")
				graph export "~/Dropbox/GPW/graphs/cdf_`x'_0_bd`bandwidth'_`model'.pdf", replace
				sum b_`x' if age == 65 & bin == `i'
				local b_`x'_`i'_`bandwidth'_`model' = string(r(mean), "%9.3f")
				sum se_`x' if age == 65 & bin == `i'
				local se_`x'_`i'_`bandwidth'_`model'=  string(r(mean), "%9.3f")
				sum cdf_share if age == 65 & bin == `i'
				local p_`x'_`i'_`bandwidth'_`model' = string(r(mean), "%9.3f")	
				}
			}
		restore
		}
	}

capture file close fh
file open fh using "~/Dropbox/GPW/tables/main_results_race.tex", replace write

foreach bandwidth in 3 {
	foreach model in "li" {
		forvalues i = 1/4 {
			local line1 ""
			local line2 ""
			local line3 ""
			local line4 ""
			foreach x in `credit_vars' {
				local line1 "`line1' & `x' "
				local line2 "`line2' & `b_`x'_`i'_`bandwidth'_`model''"
				local line3 "`line3' & (`se_`x'_`i'_`bandwidth'_`model'')"
				local line4 "`line4' & [`p_`x'_`i'_`bandwidth'_`model''] "
				}
			file write fh "\multicolumn{8}{c}{`model' bandwidth: `bandwidth'}\\" _n
			file write fh "\multicolumn{8}{c}{RACE bin = `i'}\\" _n		
			file write fh "`line1'\\" _n
			file write fh "`line2'\\" _n
			file write fh "`line3'\\" _n
			file write fh "`line4'\\" _n
			}
		}
	}
file close fh


/** Poverty **/
foreach bandwidth in 3 {
	foreach model in "li"  {
		if "`model'" == "li" {
			split_fig_iterate `credit_vars', rdpointstart(35) rdpointend(85) bandwidth(`bandwidth') bin(pov_bin)
			}
		else {
			split_fig_iterate `credit_vars', rdpointstart(35) rdpointend(85) bandwidth(`bandwidth') regtype("`model'") bin(pov_bin)
			}
		preserve
		foreach x in  `credit_vars' {
			clear
			svmat est_`x'
			rename est_`x'4 age
			rename est_`x'5 bin
			rename est_`x'2 b_`x'
			rename est_`x'3 se_`x'
			sort bin b_`x'
			by bin: gen cdf_share = _n/_N
			forvalues i = 1/4 {
				sum b_`x'  if age == 65 & bin == `i'
				local mean_`i' = r(mean)
				twoway hist b_`x' if age != 65 & bin == `i', bin(10) fcolor(dblue) lcolor(white) percent name(hist_`x'_`i', replace)  xline(`mean_`i'', lcolor(dred)) xlabel(#7 `mean_`i'' `"" ""True" "Value""' )  plotregion(margin(b=0)) xtitle("") ytitle("")
				graph export "~/Dropbox/GPW/graphs/hist_`x'_0_bd`bandwidth'_`model'.pdf", replace
				twoway line cdf_share b_`x' if bin == `i', color(dblue) name(cdf_`x'_`i', replace)  yline(0.025 0.975, lcolor(black ) lpattern(dash)) xline(`mean_`i'', lcolor(dred)) xlabel(#7 `mean_`i'' `"" ""True" "Value""' )  plotregion(margin(b=0)) xtitle("") ytitle("")
				graph export "~/Dropbox/GPW/graphs/cdf_`x'_0_bd`bandwidth'_`model'.pdf", replace
				sum b_`x' if age == 65 & bin == `i'
				local b_`x'_`i'_`bandwidth'_`model' = string(r(mean), "%9.3f")
				sum se_`x' if age == 65 & bin == `i'
				local se_`x'_`i'_`bandwidth'_`model'=  string(r(mean), "%9.3f")
				sum cdf_share if age == 65 & bin == `i'
				local p_`x'_`i'_`bandwidth'_`model' = string(r(mean), "%9.3f")	
				}
			}
		restore
		}
	}

capture file close fh
file open fh using "~/Dropbox/GPW/tables/main_results_pov.tex", replace write

foreach bandwidth in 3 {
	foreach model in "li" {
		forvalues i = 1/4 {
			local line1 ""
			local line2 ""
			local line3 ""
			local line4 ""
			foreach x in `credit_vars' {
				local line1 "`line1' & `x' "
				local line2 "`line2' & `b_`x'_`i'_`bandwidth'_`model''"
				local line3 "`line3' & (`se_`x'_`i'_`bandwidth'_`model'')"
				local line4 "`line4' & [`p_`x'_`i'_`bandwidth'_`model''] "
				}
			file write fh "\multicolumn{8}{c}{`model' bandwidth: `bandwidth'}\\" _n
			file write fh "\multicolumn{8}{c}{POV bin = `i'}\\" _n		
			file write fh "`line1'\\" _n
			file write fh "`line2'\\" _n
			file write fh "`line3'\\" _n
			file write fh "`line4'\\" _n
			}
		}
	}
file close fh




*use "`path'/data/final_zip_level.dta", clear
*keep if inrange(age, 50, 80)
*save "`path'/data/final_zip_level5080.dta", replace

*/
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
egen state_zip_year = group(state zip year)

/** make Bankruptcy in percentage points **/
replace new_bpt = new_bpt * 100

/*
/*
preserve
save `credit_vars' using "`path'/data/final_zip_level5080_clean.dta", replace
restore






/*** Main RD Figures for

1. Debt COllection
2. Total Debt in Delinquency
3. Bankruptcy
4. Risk Score

****/


preserve
collapse `credit_vars'   (rawsum) population  [aw=population ], by(age)


gen t = age - 65

foreach x of varlist `credit_vars'  {
	gen medicare = (age-65) >=0
	reg `x' c.t##i.medicare [aweight=population] if inrange(age, 62,68) & age != 65
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

/*** Split main figures by uninsurance quartiles ***/


fastxtile pctui_bin = pctui_young [aweight=population], n(4)

split_figs `credit_vars', bin(pctui_bin) rdpoint(65)

/*** Same exercise but with race ***/

fastxtile race_bin = black_share [aweight=population], n(4)

split_figs `credit_vars', bin(race_bin) rdpoint(65) 


/*** Same exercise but with poverty_share for near elderly ***/
*/
destring povshare, replace force
destring povsharenearelderly, replace force
fastxtile pov_bin = povsharenearelderly [aweight=population], n(4)

/*
split_figs `credit_vars', bin(pov_bin) rdpoint(65)



/*** Overall Poverty rate **/
fastxtile povb_bin = povshare [aweight=population], n(4)

split_figs `credit_vars', bin(povb_bin) rdpoint(65) 

/** By ACA years ***/


gen aca_bin = year > 2013
replace aca_bin = aca_bin + 1
split_figs `credit_vars', bin(aca_bin) rdpoint(65)  binnum(2)

*/



local demo_var povshare
fastxtile pov_bin2 = `demo_var' [aweight=population], n(100)

gen t = age - 65

foreach x of varlist `credit_vars' {
	gen medicare = (age-65) >=0	
	qui reg `x' c.t##i.medicare##i.pov_bin2 [aweight=population] if inrange(age, 62,68) & age != 68, robust
	replace medicare = 0
	predict `x'_lower if age == 65
	replace medicare = 1
	predict `x'_upper if age ==  65
	drop medicare	
	}
drop t


preserve 
collapse *_upper *_lower `demo_var' [aweight=population] if age == 65, by(pov_bin2)

foreach x in `credit_vars' {
	gen diff_`x' = `x'_lower - `x'_upper
	}

foreach x in `credit_vars' {
	reg diff_`x' `demo_var', robust
	local beta = string(_b[`demo_var'], "%9.3f")
	local se = string(_se[`demo_var'], "%9.3f")
	sum `demo_var', d
	local x_max = r(max)
	sum diff_`x', d
	local y_max = r(max)
	twoway (scatter diff_`x' `demo_var') (lfit diff_`x' `demo_var') , name(`x'_diff, replace) text(`y_max' `x_max' "{&beta}: `beta'" "s.e.: `se'")
	graph export "~/Dropbox/GPW/graphs/`x'_pov_bins_diff.pdf"	, replace
	twoway scatter `x'_lower `x'_upper `demo_var' , name(`x'_bins, replace)
	graph export "~/Dropbox/GPW/graphs/`x'_pov_bins_points.pdf"	, replace
	twoway (rcap `x'_lower `x'_upper `demo_var' if diff_`x' > 0, color(dblue)) (rcap `x'_lower `x'_upper `demo_var' if diff_`x' <= 0, color(dred)), legend(label(1 "Positive Drop") label(2 "Negative Drop"))
	graph export "~/Dropbox/GPW/graphs/`x'_pov_bins_rcap.pdf", replace
	}


restore

drop *_lower *_upper

local demo_var black_share
fastxtile race_bin2 = `demo_var' [aweight=population], n(100)

gen t = age - 65

foreach x of varlist `credit_vars' {
	gen medicare = (age-65) >=0	
	qui reg `x' c.t##i.medicare##i.race_bin2 [aweight=population] if inrange(age, 62,68) & age != 68, robust
	replace medicare = 0
	predict `x'_lower if age == 65
	replace medicare = 1
	predict `x'_upper if age ==  65
	drop medicare	
	}

drop t
preserve

collapse *_upper *_lower `demo_var' [aweight=population] if age == 65, by(race_bin2)

foreach x in `credit_vars' {
	gen diff_`x' = `x'_lower - `x'_upper
	}

foreach x in `credit_vars' {
	reg diff_`x' `demo_var', robust
	local beta = string(_b[`demo_var'], "%9.3f")
	local se = string(_se[`demo_var'], "%9.3f")
	sum `demo_var', d
	local x_max = r(max)
	sum diff_`x', d
	local y_max = r(max)
	twoway (scatter diff_`x' `demo_var') (lfit diff_`x' `demo_var') , name(`x'_diff, replace) text(`y_max' `x_max' "{&beta}: `beta'" "s.e.: `se'")
	graph export "~/Dropbox/GPW/graphs/`x'_race_bins_diff.pdf"	, replace
	twoway scatter `x'_lower `x'_upper `demo_var' , name(`x'_bins, replace)
	graph export "~/Dropbox/GPW/graphs/`x'_race_bins_points.pdf"	, replace
	twoway (rcap `x'_lower `x'_upper `demo_var' if diff_`x' > 0, color(dblue)) (rcap `x'_lower `x'_upper `demo_var' if diff_`x' <= 0, color(dred)), legend(label(1 "Positive Drop") label(2 "Negative Drop"))
	graph export "~/Dropbox/GPW/graphs/`x'_race_bins_rcap.pdf", replace
	}


restore

drop *_lower *_upper
fastxtile pctui_bin2 = pctui_young [aweight=population], n(100)

gen t = age - 65

foreach x of varlist `credit_vars' {
	gen medicare = (age-65) >=0	
	qui reg `x' c.t##i.medicare##i.pctui_bin2 [aweight=population] if inrange(age, 62,68) & age != 68, robust
	replace medicare = 0
	predict `x'_lower if age == 65
	replace medicare = 1
	predict `x'_upper if age ==  65
	drop medicare	
	}

drop t
preserve 
collapse *_upper *_lower pctui_young [aweight=population] if age == 65, by(pctui_bin2)

foreach x in `credit_vars' {
	gen diff_`x' = `x'_lower - `x'_upper
	}

foreach x in `credit_vars' {
	reg diff_`x' pctui_young, robust
	local beta = string(_b[pctui_young], "%9.3f")
	local se = string(_se[pctui_young], "%9.3f")
	sum pctui_young, d
	local x_max = r(max)
	sum diff_`x', d
	local y_max = r(max)
	twoway (scatter diff_`x' pctui_young) (lfit diff_`x' pctui_young) , name(`x'_diff, replace) text(`y_max' `x_max' "{&beta}: `beta'" "s.e.: `se'")
	graph export "~/Dropbox/GPW/graphs/`x'_pctui_bins_diff.pdf"	, replace
	twoway scatter `x'_lower `x'_upper pctui_young , name(`x'_bins, replace)
	graph export "~/Dropbox/GPW/graphs/`x'_pctui_bins_points.pdf"	, replace
	twoway (rcap `x'_lower `x'_upper pctui_young if diff_`x' > 0, color(dblue)) (rcap `x'_lower `x'_upper pctui_young if diff_`x' <= 0, color(dred)), legend(label(1 "Positive Drop") label(2 "Negative Drop"))
	graph export "~/Dropbox/GPW/graphs/`x'_pctui_bins_rcap.pdf", replace
	}


restore

