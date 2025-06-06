/*********************************************************************
*  Density plots – "Stata-style kernel estimates" (AI-Assisted only)
*********************************************************************/

* -------------------------------------------------------------------
* 1. Load data and keep only the AI-Assisted group
use "data/AI games.dta", clear
keep if branch == 2

* -------------------------------------------------------------------
* 2. Variables to plot
local vars prompts files images words

* -------------------------------------------------------------------
* 3. Generate Gaussian kernel density for each variable
*    - graph name: dens_<var>
*    - minimalist look: scheme plotplain
foreach v of local vars {
    twoway kdensity `v', ///
        name(dens_`v', replace) ///
        xtitle("`v'") ///
        ytitle("Density") ///
        legend(off) ///
        scheme(plotplain)          // use your favourite scheme
}

* -------------------------------------------------------------------
* 4. Combine the four graphs in a 2×2 grid and export to PDF
graph combine dens_prompts dens_files dens_images dens_words, ///
    cols(2) imargin(0 0 0 0) iscale(*.9) ///
    name(dens_grid, replace)

capture mkdir "output/figures"
graph export "output/figures/prompt_distribution.pdf", as(pdf) replace
