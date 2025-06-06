*==============================================================*
*  Poisson–Logit  – Two-panel table
*==============================================================*
* cd "~/Dropbox/I4R/AI paper"
use "data/AI games.dta", clear

* panel identifier for xtlogit FE
egen game_software = group(game software)

***************************************************************
* 1. Panel A – Study I
***************************************************************
eststo clear
local i = 0
foreach var of varlist                                    ///
        reproduction minor_errors major_errors            ///
        two_good_robustness ran_one_robustness            ///
        ran_two_robustness {

    local ++i

    /* ---------- POISSON specs ---------- */
    if inlist("`var'","minor_errors","major_errors") {
        eststo: ppmlhdfe `var' i.branch number_teammates  	///
               if game != 9,                             	///
               a(i.game##i.software max_skill min_skill  	///
               attendance) vce(r)
		estadd ysumm
        estadd local models   "Poisson":  est`i'
    }
    /* ---------- LOGIT + marginal effects ---------- */
    else {
        quietly logit `var' i.branch number_teammates   ///
                i.max_skill i.min_skill i.attendance i.game##i.software      ///
                if game != 9, vce(r)
		estadd ysumm
        margins, dydx(2.branch 3.branch)
        eststo: margins
        estadd local models   "Logit":    est`i'
    }

    * common extras
    test (2.branch - 3.branch = 0)
    estadd scalar pval = r(p): est`i'
    estadd local  controls "\checkmark": est`i'
}

* --- write the top half -----------------------------------------------------*
estout using "output/tables/logit poisson.tex", replace style(tex)  ///
    prehead("\def\sym#1{\ifmmode^{#1}\else$begin:math:text$^{#1}$end:math:text$\fi}"    ///
            "\begin{tabular}{l*{6}{c}}" "\hline\hline"                 					///
            "\multicolumn{7}{l}{\textbf{Panel A: Study I}}\\"    	   					///
            "& (1) & (2) & (3) & (4) & (5) & (6)\\")                   					///
    posthead("\hline")                                                 					///
    prefoot("\hline")                                                  					///
    cells(b(fmt(%15.3f) star label(""))                                					///
          se(par fmt(%15.3f))                                          					///
          ci(par("[" "; " "]") fmt(%15.3f)))                           					///
    keep(2.branch 3.branch)                                            					///
    collabels(none) label                                              					///
    starlevels(* 0.1 ** 0.05 *** 0.01)                                 					///
    mlabels("Reproduction" "\shortstack[c]{Minor\\errors}"             					///
            "\shortstack[c]{Major\\errors}"                            					///
            "\shortstack[c]{Two good\\robustness}"                     					///
            "\shortstack[c]{Ran one\\robustness}"                      					///
            "\shortstack[c]{Ran two\\robustness}")                     					///
    stats(models controls ymean pval N,                                					///
          fmt(%s %s %9.3f %9.3f %9.0f)                                 					///
          labels("Model" "Controls" "Mean of dep. var"                 					///
                 "p-val (AI-Assisted vs. AI-Led)" "Obs."))

***************************************************************
* 2. Panel B – Study II
***************************************************************
eststo clear
local i = 0
foreach var of varlist                                    ///
        reproduction minor_errors major_errors            ///
        two_good_robustness ran_one_robustness            ///
        ran_two_robustness {

    local ++i

    if inlist("`var'","minor_errors","major_errors") {
        eststo: ppmlhdfe `var' i.branch number_teammates  ///
               , a(i.game##i.software max_skill min_skill ///
                 attendance) vce(r)
		estadd ysumm
        estadd local models   "Poisson":  est`i'
    }
    else {
        quietly logit `var' i.branch number_teammates   	///
                i.max_skill i.min_skill i.attendance i.game##i.software,     		///
                vce(r)
		estadd ysumm
        margins, dydx(2.branch 3.branch)
        eststo: margins
        estadd local models   "Logit":    est`i'
    }

    test (2.branch - 3.branch = 0)
    estadd scalar pval = r(p): est`i'
    estadd local  controls "\checkmark": est`i'
}

* --- append the bottom half -------------------------------------------------*
estout using "output/tables/logit poisson.tex", append style(tex)  	///
    prehead("\hline\\"                                                    	///
            "\multicolumn{7}{l}{\textbf{Panel B: Studies I and II combined}}\\"      ///
            "& (1) & (2) & (3) & (4) & (5) & (6)\\")                     	///
    posthead("\hline")                                                   	///
    prefoot("\hline")                                                   	///
    postfoot("\hline\hline"                                              	///
             "\multicolumn{7}{p{0.8\textwidth}}{\it{Note:} "            	///
             "Standard errors in parentheses, confidence intervals "     	///
             "in brackets; human-only group omitted. "						///
			 "The model for One good robustness is not included "			///
			 "due to unsuficient observations, preventing it from "			///
			 "converging. Marginal effects reported for Logit models.}\\"   ///
               "\multicolumn{7}{l}{Controls include number of teammates; game, software, and game $\times$ software fixed effects; maximum and minimum position skill fixed effects; attendance fixed effects.}\"        ///
             "\multicolumn{7}{l}{\sym{*} \(p<0.1\), \sym{**} \(p<0.05\)," 	///
             " \sym{***} \(p<0.01\)}\\"                                  	///
             "\end{tabular}")                                            	///
    cells(b(fmt(%15.3f) star label(""))                                  	///
          se(par fmt(%15.3f))                                            	///
          ci(par("[" "; " "]") fmt(%15.3f)))                             	///
    keep(2.branch 3.branch) collabels(none)                              	///
    label starlevels(* 0.1 ** 0.05 *** 0.01)                             	///
    mlabels("Reproduction" "\shortstack[c]{Minor\\errors}"               	///
            "\shortstack[c]{Major\\errors}"                              	///
            "\shortstack[c]{Two good\\robustness}"                       	///
            "\shortstack[c]{Ran one\\robustness}"                        	///
            "\shortstack[c]{Ran two\\robustness}")                       	///
    stats(models controls ymean pval N,                                  	///
          fmt(%s %s %9.3f %9.3f %9.0f)                                   	///
          labels("Model" "Controls" "Mean of dep. var"                   	///
                 "p-val (AI-Assisted vs. AI-Led)" "Obs."))
				 
				 
				 
				 