



local path "/Users/PSG24/Dropbox/GPW"
local credit_vars q_coll_12 n_coll_12 avg_riskscore balance_cc_delinq balance_mort_delinq

use `path'/data/theta1_state_data, clear

twoway (scatter diff_q_coll_12 pctui_young,   mlab(state ) mlabpos(0) m(i) ) ///
  (lfit diff_q_coll_12 pctui_young), ///
  xsize(16) ysize(9) xtitle("Percent Uninsured 55-64") ytitle("") legend(off)
graph export "`path'/graphs/diff_q_coll_12_pctui.pdf", replace
reg diff_q_coll_12 pctui_young
twoway (scatter diff_q_coll_12 pctui_young if state != "MS",   mlab(state ) mlabpos(0) m(i) ) ///
  (lfit diff_q_coll_12 pctui_young if state != "MS"), ///
  xsize(16) ysize(9) xtitle("Percent Uninsured 55-64") ytitle("") legend(off)
graph export "`path'/graphs/diff_q_coll_12_pctui_noms.pdf", replace

drop if state_fips == .

foreach x in `credit_vars' {
	rename (`x'_lower `x'_upper) (`x'0  `x'1)
	rename (diff_`x') (`x'_diff)
	}

keep *0 *1 state state_name state_fips *_diff
reshape long `credit_vars', i(state state_name state_fips) j(young_elderly)

foreach x in `credit_vars' {
	merge 1:1 state young_elderly using `path'/data/`x'_state_rd
	drop _merge
	rename b_joint1 `x'_border
	rename b_joint_cont1 `x'_border_cont
	rename weight `x'_weight
	drop b1 V1 
	drop if stderr == 0	
}

save "`path'/data/state_collapse_rd.dta",  replace
outsheet using "`path'/data/state_collapse_rd.csv", comma replace


foreach x of varlist `credit_vars' {
	qui sum `x'    if young_elderly == 0 & state_abbrev != "AK" & state_abbrev != "HI", d
	local r1 = r(sd)
	qui sum `x'    if young_elderly == 1 & state_abbrev != "AK" & state_abbrev != "HI", d	
	local r2 = r(sd)
	disp "Theta 1 `x' : `=`r2' / `r1''"
	qui sum `x'_border  [aweight=`x'_weight]  if young_elderly == 0 & state_abbrev != "AK" & state_abbrev != "HI", d
	local r1 = r(sd)
	qui sum `x'_border  [aweight=`x'_weight]  if young_elderly == 1 & state_abbrev != "AK" & state_abbrev != "HI", d	
	local r2 = r(sd)
	disp "Theta 2 `x' : `=`r2' / `r1''"
	qui sum `x'    if young_elderly == 0 & state_abbrev != "AK" & state_abbrev != "HI", d
	local r1 = r(sd)
	qui sum `x'_border  [aweight=`x'_weight]  if young_elderly == 1 & state_abbrev != "AK" & state_abbrev != "HI", d	
	local r2 = r(sd)
	disp "Theta 3 `x' : `=`r2' / `r1''"
	}
/** Labels: 

65- all: q_coll_12, young_elderly == 0
65+ all: q_coll_12, young_elderly == 1
65- border: q_coll_12_border, young_elderly == 0
65+ border: q_coll_12_border, young_elderly == 1

*/

/** R Code

library(readr)
library(tidyverse)
library(ggrepel)
state_collapse <- read_csv("~/Dropbox/GPW/data/state_collapse.csv")
state_collapse_rd <- read_csv("~/Dropbox/GPW/data/state_collapse_rd.csv")
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
