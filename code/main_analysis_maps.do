
ssc install maptile
ssc install spmap
maptile_install using "http://files.michaelstepner.com/geo_cz1990.zip"
maptile_install using "http://files.michaelstepner.com/geo_state.zip"
/** Maps **/
/** Figure 2 **/
insheet using ~/Dropbox/GPW/data/czone_estimates_fig2.csv, clear
rename czone cz
drop if cz == "NA"

destring cz, replace


maptile lower_estimate, geo(cz1990) nquantiles(7)    savegraph("~/Dropbox/GPW/graphs/fig2_q_coll_12_65_minus_shrink.png") resolution(0.75)  replace stateoutline(thin)  legdecimals(0) twopt(legend(title("Annual" "Per-Capita" "Dollars of" "Collections Debt", size(medium)) label(2 "Less than 27") label(8 "Greater than 155")) )
mat breaks = r(breaks)
mat breaks = breaks'
graph save "~/Dropbox/GPW/graphs/fig2_q_coll_12_65_minus_shrink.gph", replace
graph export "~/Dropbox/GPW/graphs/fig2_q_coll_12_65_minus_shrink.pdf", replace

local e "`=breaks[1,1]'  `=breaks[1,2]' `=breaks[1,3]' `=breaks[1,4]' `=breaks[1,5]' `=breaks[1,6]'"
maptile upper_estimate, geo(cz1990) cutvalues(`e') savegraph("~/Dropbox/GPW/graphs/fig2_q_coll_12_65_plus_shrink.png") resolution(0.75) replace  stateoutline(thin)  legdecimals(0) twopt(legend(title("Annual" "Per-Capita" "Dollars of" "Collections Debt", size(medium)) label(2 "Less than 27") label(8 "Greater than 155")) text(0.95 1 "test") ) 
graph save "~/Dropbox/GPW/graphs/fig2_q_coll_12_65_plus_shrink.gph", replace
graph export "~/Dropbox/GPW/graphs/fig2_q_coll_12_65_plus_shrink.pdf", replace


keep cz
tempfile czlist
save `czlist'
/** Figure 4 **/
insheet using "~/Dropbox/GPW/data/czone_lasso_predicted_full.csv", comma clear names

rename czone cz
merge 1:1 cz using `czlist'
replace tau_ch = -1*tau_ch
replace tau_shrink = -1*tau_shrink
replace tau_mean = -1*tau_mean
replace tau_ch = tau_shrink if tau_ch == .
egen tau_mean2 = max(tau_mean)
replace tau_ch = tau_mean2 if tau_ch == .

maptile tau_ch , geo(cz1990) rangecolor(white dblue) nquantiles(7) savegraph("~/Dropbox/GPW/graphs/fig4_q_coll_12_predicted_tau_shrink_full.png") resolution(0.5) replace stateoutline(thin) legdecimals(0) twopt(legend(title("Predicted Decrease in"  "Annual Per-Capita" "Collections Debt", size(small)) label(2 "Less than 6") label(8 "Greater than 52")))
mat breaks = r(breaks)
mat breaks = breaks'
local e  "`=breaks[1,1]'  `=breaks[1,2]' `=breaks[1,3]' `=breaks[1,4]' `=breaks[1,5]' `=breaks[1,6]'"
graph export "~/Dropbox/GPW/graphs/fig4_q_coll_12_predicted_tau_shrink_full.pdf", replace
tempfile tmp
save `tmp'


insheet using "~/Dropbox/GPW/data/czone_lasso_predicted_post.csv", comma clear names
rename czone cz
merge 1:1 cz using `czlist'
replace tau_ch = -1*tau_ch
replace tau_shrink = -1*tau_shrink
replace tau_mean = -1*tau_mean
replace tau_ch = tau_shrink if tau_ch == .
egen tau_mean2 = max(tau_mean)
replace tau_ch = tau_mean2 if tau_ch == .

maptile tau_ch if aca == "Post-ACA", geo(cz1990) rangecolor(white dblue) cutvalues(`e') savegraph("~/Dropbox/GPW/graphs/fig4_q_coll_12_predicted_tau_shrink_post.png") resolution(0.5) replace stateoutline(thin) legdecimals(0) twopt(legend(title("Predicted Decrease in"  "Annual Per-Capita" "Collections Debt", size(small)) label(2 "Less than 6") label(8 "Greater than 52")))
graph export "~/Dropbox/GPW/graphs/fig4_q_coll_12_predicted_tau_shrink_post.pdf", replace


insheet using "~/Dropbox/GPW/data/czone_lasso_predicted_pre.csv", comma clear names

rename czone cz
merge 1:1 cz using `czlist'

replace tau_ch = -1*tau_ch
replace tau_shrink = -1*tau_shrink
replace tau_mean = -1*tau_mean
replace tau_ch = tau_shrink if tau_ch == .
egen tau_mean2 = max(tau_mean)
replace tau_ch = tau_mean2 if tau_ch == .

maptile tau_ch if aca == "Pre-ACA", geo(cz1990) rangecolor(white dblue) cutvalues(`e') savegraph("~/Dropbox/GPW/graphs/fig4_q_coll_12_predicted_tau_shrink_pre.png") resolution(0.5) replace stateoutline(thin) legdecimals(0) twopt(legend(title("Predicted Decrease in"  "Annual Per-Capita" "Collections Debt", size(small)) label(2 "Less than 6") label(8 "Greater than 52")))
graph export "~/Dropbox/GPW/graphs/fig4_q_coll_12_predicted_tau_shrink_pre.pdf", replace

/** Counterfactual Health insurance rates by commuting zone w/o Medicare, pre- & post-ACA **/

insheet using "~/Dropbox/GPW/data/czone_acs_lasso_predicted_pre.csv", comma clear names
rename czone cz

maptile lower_estimate if aca == "Pre-ACA", geo(cz1990) rangecolor(white dred) nquantiles(8) savegraph("~/Dropbox/GPW/graphs/has_insurance_65_minus_shrink_pre.png") resolution(0.5) replace stateoutline(thin) legdecimals(2) twopt(legend(title( "Insurance Rate" "at age 65 w/o Medicare", size(small)) label(2 "Less than 0.84") label(9 "Greater than 0.99")))
mat breaks = r(breaks)
mat breaks = breaks'

local e "`=breaks[1,1]'  `=breaks[1,2]' `=breaks[1,3]' `=breaks[1,4]' `=breaks[1,5]' `=breaks[1,6]' `=breaks[1,7]' "

insheet using "~/Dropbox/GPW/data/czone_acs_lasso_predicted_post.csv", comma clear names
rename czone cz
maptile lower_estimate if aca == "Post-ACA", geo(cz1990) rangecolor(white dred) cutvalues(`e') savegraph("~/Dropbox/GPW/graphs/has_insurance_65_minus_shrink_post.png") resolution(0.5) replace stateoutline(thin) legdecimals(2) twopt(legend(title("Insurance Rate" "at age 65 w/o Medicare", size(small)) label(2 "Less than 0.84") label(9 "Greater than 0.99")))


insheet using "~/Dropbox/GPW/data/czone_lasso_predicted_full_beta.csv", comma clear names
rename czone cz
merge 1:1 cz using `czlist'


replace beta_ch = -1*beta_ch
replace beta_shrink = -1*beta_shrink
replace beta_mean = -1*beta_mean
replace beta_ch = beta_shrink if beta_ch == .
egen beta_mean2 = max(beta_mean)
replace beta_ch = beta_mean2 if beta_ch == .

maptile beta_ch, geo(cz1990) rangecolor(white dblue) nquantiles(7) savegraph("~/Dropbox/GPW/graphs/fig4_q_coll_12_predicted_beta_shrink_full.png") resolution(0.5) replace stateoutline(thin) legdecimals(0) twopt(legend(title("Predicted Decrease in"  "Annual Per-Newly-Insured" "Collections Debt", size(small)) label(2 "Less than 77") label(8 "Greater than 582")))
graph export "~/Dropbox/GPW/graphs/fig4_q_coll_12_predicted_beta_shrink_full.pdf", replace



insheet using "~/Dropbox/GPW/data/czone_lasso_predicted_pre_beta.csv", comma clear names
rename czone cz
merge 1:1 cz using `czlist'


replace beta_ch = -1*beta_ch
replace beta_mean = -1*beta_mean
egen beta_mean2 = max(beta_mean)
replace beta_ch = beta_mean2 if beta_ch == .

maptile beta_ch, geo(cz1990) rangecolor(white dblue) nquantiles(7) savegraph("~/Dropbox/GPW/graphs/fig4_q_coll_12_predicted_beta_shrink_pre.png") resolution(0.5) replace stateoutline(thin) legdecimals(0) twopt(legend(title("Predicted Decrease in"  "Annual Per-Newly-Insured" "Collections Debt", size(small)) label(2 "Less than 36") label(8 "Greater than 483")))
mat breaks = r(breaks)
mat breaks = breaks'

local e "`=breaks[1,1]'  `=breaks[1,2]' `=breaks[1,3]' `=breaks[1,4]' `=breaks[1,5]' `=breaks[1,6]' "

graph export "~/Dropbox/GPW/graphs/fig4_q_coll_12_predicted_beta_shrink_pre.pdf", replace


insheet using "~/Dropbox/GPW/data/czone_lasso_predicted_post_beta.csv", comma clear names
rename czone cz
merge 1:1 cz using `czlist'


replace beta_ch = -1*beta_ch
replace beta_mean = -1*beta_mean
egen beta_mean2 = max(beta_mean)
replace beta_ch = beta_mean2 if beta_ch == .

maptile beta_ch, geo(cz1990) rangecolor(white dblue) cutvalues(`e')  savegraph("~/Dropbox/GPW/graphs/fig4_q_coll_12_predicted_beta_shrink_post.png") resolution(0.5) replace stateoutline(thin) legdecimals(0) twopt(legend(title("Predicted Decrease in"  "Annual Per-Newly-Insured" "Collections Debt", size(small)) label(2 "Less than 36") label(8 "Greater than 483")))

graph export "~/Dropbox/GPW/graphs/fig4_q_coll_12_predicted_beta_shrink_post.pdf", replace


insheet using "~/Dropbox/GPW/data/czone_acs_estimates_prepost_dind_shrink.csv", comma clear names

replace tau = -1*tau
rename czone cz
maptile tau, geo(cz1990) rangecolor(white dblue)   savegraph("~/Dropbox/GPW/graphs/has_insurance_diff_in_disc_shrink.png") resolution(0.5) replace stateoutline(thin) legdecimals(2) twopt(legend(title("Predicted Increase in"  "Health Insurance" "Due to ACA", size(small)) label(2 "Less than 0.01") label(7 "Greater than 0.07")))
