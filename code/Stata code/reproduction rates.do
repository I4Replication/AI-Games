* cd "~/Dropbox/I4R/AI paper"
use "data/AI games.dta", clear

collapse (mean) reproduction minor_errors major_errors , by(game2 branch)
reshape wide reproduction minor_errors major_errors, i(game2) j(branch)


gen rep_machine = reproduction1-reproduction3
gen rep_cyorg = reproduction1-reproduction2

gen minor_machine = minor_errors1-minor_errors3
gen minor_cyorg = minor_errors1-minor_errors2

gen major_machine = major_errors1-major_errors3
gen major_cyorg = major_errors1-major_errors2


twoway	(line rep_machine game2, lcolor("85 70 0 43") lwidth(thick)) ///
		(line rep_cyorg game2, lcolor("0 100 100 7") lwidth(thick)), ///
		xlabel(1 `" "Toronto" "Feb" "' 2 `" "Ottawa" "May" "' 3 `" "Sheffield" "Jun" "' ////
		4 `" "Cornell" "Aug" "' 5 `" "Bogota" "Oct" "' 6 `" "Tilburg" "Oct" "' 7 `" "Virtual" "Nov" "' 8 `" "Virtual" "2025" "', valuelabel grid) ///
		ylabel(-0.5 "-50%" 0 "0%" 0.5 "50%" 1 "100%", grid) ///
		graphregion(color(white)) ///
		plotregion(lstyle(box)) ///
		ytitle("Difference in reproduction rate") ///
		xtitle( " " "AI game") ///
		note("") ///
		title("") ///
		legend( label(1 "Human-Only Vs AI-Led") label(2 "Human-Only Vs AI-Assisted") rows(1)  region(lstyle(none))) ylab(, nogrid)
gr export "output/figures/reproduction rates.pdf", replace



twoway	(line minor_machine game2, lcolor("85 70 0 43") lwidth(thick)) ///
		(line minor_cyorg game2, lcolor("0 100 100 7") lwidth(thick) lpattern(dash)), ///
		xlabel(1 `" "Toronto" "Feb" "' 2 `" "Ottawa" "May" "' 3 `" "Sheffield" "Jun" "' ////
		4 `" "Cornell" "Aug" "' 5 `" "Bogota" "Oct" "' 6 `" "Tilburg" "Oct" "' 7 `" "Virtual" "Nov" "' 8 `" "Virtual" "2025" "', valuelabel grid) ///
		ylabel(, grid) ///
		graphregion(color(white)) ///
		plotregion(lstyle(box)) ///
		ytitle("Differences in the number of minor errors detected") ///
		xtitle( " " "AI game") ///
		note("") ///
		title("") ///
		legend( label(1 "Human-Only Vs AI-Led") label(2 "Human-Only Vs AI-Assisted") rows(1)  region(lstyle(none))) ylab(, nogrid)
gr export "output/figures/minor errors.pdf", replace



twoway	(line major_machine game2, lcolor("85 70 0 43") lwidth(thick)) ///
		(line major_cyorg game2, lcolor("0 100 100 7") lwidth(thick)), ///
		xlabel(1 `" "Toronto" "Feb" "' 2 `" "Ottawa" "May" "' 3 `" "Sheffield" "Jun" "' ////
		4 `" "Cornell" "Aug" "' 5 `" "Bogota" "Oct" "' 6 `" "Tilburg" "Oct" "' 7 `" "Virtual" "Nov" "' 8 `" "Virtual" "2025" "', valuelabel grid) ///
		ylabel(, grid) ///
		graphregion(color(white)) ///
		plotregion(lstyle(box)) ///
		ytitle("Differences in the number of major errors detected") ///
		xtitle( " " "AI game") ///
		note("") ///
		title("") ///
		legend( label(1 "Human-Only Vs AI-Led") label(2 "Human-Only Vs AI-Assisted") rows(1)  region(lstyle(none))) ylab(, nogrid)
gr export "output/figures/major errors.pdf", replace
