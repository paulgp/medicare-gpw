

local path "/Users/PSG24/Dropbox/GPW"

/** Black Share in 2012 **/
insheet using "`path'/data/race_by_zip2012.csv", comma clear names
tempfile tmp
gen black_share = black / total
gen nonwhite_share = nonwhite / total
drop black nonwhite total
rename geoid zip
save `tmp'

/*
insheet using "`path'/data/educ_by_zip2016.csv", comma clear names
tempfile tmp2
rename geoid fips
save `tmp2'
*/

/** Pct UI in 2012 **/
insheet using "`path'/data/zip_sahie.csv", comma clear names
tempfile tmp3
rename age_55to64years pctui_young
rename age_65to74years pctui_old
rename fips zip
destring pctui_*, replace force
save `tmp3'



use "`path'/data/final_zip_level5080.dta", clear
replace age = age - 1
keep if inrange(age, 55, 75)
*keep if inrange(age, 50, 80)


rename zipcode zip
merge m:1 zip using `tmp'
keep if _merge == 1 | _merge == 3
drop _merge



merge m:1 zip using `tmp3'
keep if _merge == 1 | _merge == 3
drop _merge


gen year = floor(quarter / 100)

egen state_zip_year = group(state zip year)
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
