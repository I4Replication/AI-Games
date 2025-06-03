*cd "~/Dropbox/I4R/AI paper"
use "data/AI games.dta", clear

*** Generate branch_team_n just once
bysort branch: gen branch_team_n = _n

*** Preserve the dataset state
preserve

local varlist time2_reproduction time2_first_minor time2_first_major

foreach var of local varlist {
    *** Restore the original dataset for this iteration
    restore
    preserve

    *** Set missing values to 999
    replace `var' = 999 if `var'==.

    *** Keep only relevant variables
    keep branch_team_n branch `var'
    rename `var' `var'_br

    *** Reshape wide for each branch
    reshape wide `var'_br, i(branch_team_n) j(branch)

    *** Generate cumulative variables
    forvalues num = 1/3 {
        cumul `var'_br`num', gen(`var'_cum_br`num')
    }

    *** Stack back into long format
    stack `var'_br1 `var'_cum_br1 ///
          `var'_br2 `var'_cum_br2 ///
          `var'_br3 `var'_cum_br3, ///
          into(`var'_br `var'_cum_br) wide clear

    *** Extend the CDF to 420 minutes
    sort _stack `var'_br `var'_cum_br

    bysort _stack: gen not_repro_n   = _n if `var'_br==999
    bysort _stack: egen not_repro_min = min(not_repro_n)
    gen not_repro_first = (not_repro_n==not_repro_min)
    gen repro_last = (not_repro_first[_n + 1])==1
    expand 2 if repro_last==1, gen(xpd)
    replace `var'_br = 420 if xpd==1

    *** Drop missing and sort
    drop if `var'_br==.
    sort _stack `var'_br `var'_cum_br

    *** Set axis title based on variable
    local xtitle ""
    if "`var'" == "time2_reproduction" {
        local xtitle "Minutes to reproduction"
    }
    else if "`var'" == "time2_first_minor" {
        local xtitle "Minutes to first minor error"
    }
    else if "`var'" == "time2_first_major" {
        local xtitle "Minutes to first major error"
    }

    *** Plot the graph
    twoway 	(area `var'_cum_br1 `var'_br if `var'_br!=999, color("100 36 0 30")  fcolor(%50)) ///
			(area `var'_cum_br2 `var'_br if `var'_br!=999, color("0 35 100 10")  fcolor(%50)) ///
			(area `var'_cum_br3 `var'_br if `var'_br!=999, color("100 0 30 38")  fcolor(%50)) ///
    , xtitle("`xtitle'") ///
      graphregion(color(white)) ///
      ytitle("Cumulative density") ///
      note("") ///
      title("") ///
      legend(label(1 "Human-Only" ) label(2 "AI-Assisted") label(3 "AI-Led") rows(1) pos(6) region(lstyle(none))) ///
      ylab(, nogrid)

    *** Export graph
    gr export "output/figures/`var'.pdf", replace
}
