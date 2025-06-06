*==============================================================*
*  Main table  – Two-panel table
*==============================================================*
* cd "~/Dropbox/I4R/AI paper"
use "data/AI games.dta", clear


bysort paper: egen max_minor = max(minor_errors)
bysort paper: egen max_major = max(major_errors)

generate minor_errors_norm = minor_errors/max_minor
generate major_errors_norm = major_errors/max_major


******************************************************************************
* 1. Panel A – Study I
******************************************************************************
eststo clear
local i = 0
foreach var of varlist minor_errors_norm major_errors_norm {

    local ++i
    eststo: reghdfe `var' i.branch number_teammates if game!=9,      										///
                     a(i.game##i.software max_skill min_skill attendance) vce(r)
    estadd ysumm
    test (2.branch - 3.branch = 0)
	local try = r(p)
	estadd scalar pval=`try': est`i'
	estadd local controls= "\checkmark": est`i'
}

* -- write the top half of the table ----------------------------------------
estout using "output/tables/error_shares.tex", 															///
      replace style(tex)                                        			 								///
      prehead("\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"													///
              "\begin{tabular}{l*{7}{c}}" "\hline\hline"    												///
              "\multicolumn{8}{l}{\textbf{Panel A: Study I}}\\" 											///
              "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\") 												///
      posthead("\hline")                                        											///
      prefoot("\hline")																						///
      cells(b(fmt(%9.3f) star label("")) se(par fmt(%9.3f)) ci(par("[" "; " "]") fmt(%9.3f)))   	 		///
      drop(1.branch number_teammates _cons) collabels(none)     											///
      label starlevels(* 0.1 ** 0.05 *** 0.01)                 												///
      mlabels("Reproduction" "\shortstack[c]{Minor\\errors}"    											///
              "\shortstack[c]{Major\\errors}"                   											///
              "\shortstack[c]{One good\\robustness}"           												///
              "\shortstack[c]{Two good\\robustness}"           												///
              "\shortstack[c]{Ran one\\robustness}"            												///
              "\shortstack[c]{Ran two\\robustness}")           												///
      stats(controls ymean pval N,                             												///
            fmt(%s %9.3f %9.3f %9.0f)                         												///
            labels("Controls" "Mean of dep. var"               												///
                   "p-val (AI-Assisted vs. AI-Led)" "Obs."))

******************************************************************************
* 2. Panel B – Study II
******************************************************************************

eststo clear
local i = 0
foreach var of varlist minor_errors_norm major_errors_norm {

    local ++i
    eststo: reghdfe `var' i.branch number_teammates,              											///
                     a(i.game##i.software max_skill min_skill attendance) vce(r)
    estadd ysumm
    test (2.branch - 3.branch = 0)
	local try = r(p)
	estadd scalar pval=`try': est`i'
	estadd local controls= "\checkmark": est`i'
}

* -- append the second half --------------------------------------------------
estout using "output/tables/error_shares.tex", 																///
      append style(tex)                                      												///
      prehead("\hline\\"																					///
			  "\multicolumn{8}{l}{\textbf{Panel B: Studies I and II combined}}\\" 									///
              "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\") 												///
      posthead("\hline")                                     												///
      postfoot("\hline\hline"                                												///
               "\multicolumn{8}{l}{\it{Note:} Standard errors in " 											///
               "parentheses, confidence intervals in brackets; " 											///
               "human-only group omitted.}\\"               												///
               "\multicolumn{8}{l}{Controls include number of "    											///
               "teammates; game–software, skill, and attendance "  											///
               "fixed effects.}\\"                           												///
               "\multicolumn{8}{l}{\sym{*} \(p<0.1\), \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)}\\" 		///
               "\end{tabular}")                             												///
	  prefoot("\hline")																						///
      cells(b(fmt(%9.3f) star label("")) se(par fmt(%9.3f)) ci(par("[" "; " "]") fmt(%9.3f))) 				///
      drop(1.branch number_teammates _cons) collabels(none)  												///
      label starlevels(* 0.1 ** 0.05 *** 0.01)                												///
      mlabels("Reproduction" "\shortstack[c]{Minor\\errors}"  												///
              "\shortstack[c]{Major\\errors}"                 												///
              "\shortstack[c]{One good\\robustness}"          												///
              "\shortstack[c]{Two good\\robustness}"          												///
              "\shortstack[c]{Ran one\\robustness}"           												///
              "\shortstack[c]{Ran two\\robustness}")          												///
      stats(controls ymean pval N,                            												///
            fmt(%s %9.3f %9.3f %9.0f)                         												///
            labels("Controls" "Mean of dep. var"              												///
                   "p-val (AI-Assisted vs. AI-Led)" "Obs."))
				   
				   
				   				   