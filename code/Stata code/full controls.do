*==============================================================*
*   Full controls – Two-tables
*==============================================================*
*cd "~/Dropbox/I4R/AI paper"
use "data/AI games.dta", clear

foreach var of varlist branch game software max_skill min_skill attendance{
	
	local vlabel : variable label `var'
	levelsof `var', local(bvals)
	
	foreach val of local bvals {
		
		local oldlbl : label (`var') `val'
		label define `var'_label `val' "`vlabel': `oldlbl'" , modify

}
}

***************************************************************
* 1. Panel A – Study 1
***************************************************************
eststo clear
local i =0
foreach var of varlist reproduction minor_errors major_errors one_good_robustness two_good_robustness ran_one_robustness ran_two_robustness{
	
	local i = `i'+1

	eststo: reg `var' i.branch number_teammates i.game##i.software i.max_skill i.min_skill i.attendance if game!=9, r
	estadd ysumm
	test (2.branch - 3.branch = 0)
	local try = r(p)
	estadd scalar pval=`try': est`i'
	
}

* --- write first table -----------------------------------------------------*	
estout using "output/tables/full controls (s1).tex", ///
    cells(b(fmt(%9.3f) star label("")) se(par fmt(%9.3f)) ci(par("[" "; " "]") fmt(%9.3f))) ///
    drop(1.branch 1.game* 0.* *#0.software 1.*skill _cons) collabels(none) label starlevels(* 0.1 ** 0.05 *** 0.01) ///
    mlabels("Reproduction" "\shortstack[c]{Minor\\errors}" "\shortstack[c]{Major\\errors}" "\shortstack[c]{One good\\robustness}" "\shortstack[c]{Two good\\robustness}" "\shortstack[c]{Ran one\\robustness}" "\shortstack[c]{Ran two\\robustness}") ///
    stats(ymean pval N, fmt(%9.3f %9.3f %9.0f) labels("Mean of dep. var" "p-val (AI-Assisted vs. AI-Led)" "Observations")) ///
    style(tex) replace ///
    prehead("\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" ///
    "\begin{tabular}{l*{7}{c}}" "\hline\hline" "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\" ) ///
    posthead("\hline") ///
    prefoot("\hline") ///
    postfoot("\hline\hline" ///
    "\multicolumn{8}{l}{\it{Note:} Standard errors in parentheses, confidence intervals in brackets; human-only branch omitted.}\\" ///
    "\multicolumn{8}{l}{\sym{*} \(p<0.1\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\" ///
    "\end{tabular}")

	

	
***************************************************************
* 2. Panel B – Study 2
***************************************************************	
	
eststo clear
local i =0
foreach var of varlist reproduction minor_errors major_errors one_good_robustness two_good_robustness ran_one_robustness ran_two_robustness{
	
	local i = `i'+1

	eststo: reg `var' i.branch number_teammates i.game##i.software i.max_skill i.min_skill i.attendance, r
	estadd ysumm
	test (2.branch - 3.branch = 0)
	local try = r(p)
	estadd scalar pval=`try': est`i'
	
}

* --- write second table -----------------------------------------------------*	
estout using "output/tables/full controls (s2).tex", ///
    cells(b(fmt(%9.3f) star label("")) se(par fmt(%9.3f)) ci(par("[" "; " "]") fmt(%9.3f))) ///
    drop(1.branch 1.game* 0.* *#0.software 1.*skill _cons) collabels(none) label starlevels(* 0.1 ** 0.05 *** 0.01) ///
    mlabels("Reproduction" "\shortstack[c]{Minor\\errors}" "\shortstack[c]{Major\\errors}" "\shortstack[c]{One good\\robustness}" "\shortstack[c]{Two good\\robustness}" "\shortstack[c]{Ran one\\robustness}" "\shortstack[c]{Ran two\\robustness}") ///
    stats(ymean pval N, fmt(%9.3f %9.3f %9.0f) labels("Mean of dep. var" "p-val (AI-Assisted vs. AI-Led)" "Observations")) ///
    style(tex) replace ///
    prehead("\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" ///
    "\begin{tabular}{l*{7}{c}}" "\hline\hline" "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\" ) ///
    posthead("\hline") ///
    prefoot("\hline") ///
    postfoot("\hline\hline" ///
    "\multicolumn{8}{l}{\it{Note:} Standard errors in parentheses, confidence intervals in brackets; human-only branch omitted.}\\" ///
    "\multicolumn{8}{l}{\sym{*} \(p<0.1\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\" ///
    "\end{tabular}")
