* ==============================================================
*   Full controls – Two-tables (Panel A: Study I)
* ==============================================================
* cd "~/Dropbox/I4R/AI paper"
use "data/AI games.dta", clear

gen study=(game==9)
label define study_lbl 0 "Study I" 1 "Study II"
label values study study_lbl

***************************************************************
* 1. Panel A – Study I
***************************************************************
eststo clear
local i = 0
foreach var of varlist reproduction minor_errors major_errors one_good_robustness two_good_robustness ran_one_robustness ran_two_robustness {
	local i = `i' + 1
        * Regression with full controls for Study I (game != 9)
	eststo: reghdfe `var' i.branch##i.study number_teammates, a(i.game##i.software i.max_skill i.min_skill i.attendance) vce(r)
	estadd ysumm
	test (2.branch - 3.branch = 0)
	local try = r(p)
	estadd scalar pval=`try': est`i'
}

* --- Export estout table to LaTeX ---
estout using "output/tables/study 2.tex", ///
    cells(b(fmt(%9.3f) star label("")) se(par fmt(%9.3f)) ci(par("[" "; " "]") fmt(%9.3f))) ///
    drop(1.branch* *0.* 1.study number_teammates _cons) ///
    collabels(none) label starlevels(* 0.1 ** 0.05 *** 0.01) ///
    mlabels("Reproduction" "\shortstack[c]{Minor\\errors}" "\shortstack[c]{Major\\errors}" "\shortstack[c]{One good\\robustness}" "\shortstack[c]{Two good\\robustness}" "\shortstack[c]{Ran one\\robustness}" "\shortstack[c]{Ran two\\robustness}") ///
    stats(ymean pval N, fmt(%9.3f %9.3f %9.0f) labels("Mean of dep. var" "p-val (AI-Assisted vs. AI-Led)" "Observations")) ///
    style(tex) replace ///
    prehead("\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" ///
    "\begin{tabular}{l*{7}{c}}" "\hline\hline" "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\" ) ///
    posthead("\hline") ///
    prefoot("\hline") ///
    postfoot("\hline\hline" ///
    "\multicolumn{8}{l}{\it{Note:} Standard errors in parentheses, confidence intervals in brackets; human-only group omitted.}\\" ///
    "\multicolumn{8}{l}{\sym{*} \(p<0.1\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\" ///
    "\end{tabular}")
