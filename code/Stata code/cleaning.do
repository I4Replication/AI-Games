*cd "~/Dropbox/I4R/AI paper"

clear all
frame rename default main
import excel "data/AI games.xlsx", sheet("Sheet1") firstrow clear

*this team didnt have R, so they didnt do anything
drop if game=="cornell" & team=="4r"

foreach var of varlist game branch paper software attendance max_skill min_skill min_gpt max_gpt min_coding max_coding{
	
	replace `var'=proper(`var')
	
}

rename paper paper_game
gen paper = substr(paper_game, 9, .)
replace paper_game = substr(paper_game, 1, 7)

replace branch = "Human-Only" if branch=="Human"
replace branch = "AI-Assisted" if branch=="Cyborg"
replace branch = "AI-Led" if branch=="Machine"

gen game2=game
replace game2="Virtual" if game2=="Virtual Europe"
replace game2="Virtual" if game2=="Virtual North America"

label define game_label 1 "Toronto" 2 "Ottawa" 3 "Sheffield" 4 "Cornell" 5 "Bogota" 6 "Tilburg" 7 "Virtual Europe" 8 "Virtual North America" 9 "Virtual 2025"
label define game2_label 1 "Toronto" 2 "Ottawa" 3 "Sheffield" 4 "Cornell" 5 "Bogota" 6 "Tilburg" 7 "Virtual" 8 "Virtual 2025"
label define min_coding_label 1 "Novice" 2 "Advanced Beginner" 3 "Competent" 4 "Proficient" 5 "Expert"
label define max_coding_label 1 "Novice" 2 "Advanced Beginner" 3 "Competent" 4 "Proficient" 5 "Expert"
label define min_skill_label 1 "Student" 2 "Researcher" 3 "Postdoc" 4 "Professor"
label define max_skill_label 1 "Student" 2 "Researcher" 3 "Postdoc" 4 "Professor"
label define min_gpt_label 1 "Never" 2 "Beginner" 3 "Intermediate" 4 "Advanced"
label define max_gpt_label 1 "Never" 2 "Beginner" 3 "Intermediate" 4 "Advanced"
label define branch_label 1 "Human-Only" 2 "AI-Assisted" 3 "AI-Led"
label define software_label 0 "Stata" 1 "R"
label define reproduction_label 0 "no" 1 "yes"
label define attendance_label 0 "Virtual" 1 "In-Person"

foreach var of varlist game game2 branch software reproduction min_coding max_coding min_skill max_skill min_gpt max_gpt attendance{
	
	encode `var' , generate(`var'_id) label(`var'_label)
	drop `var'
	rename `var'_id `var'
		
}

*Sheffield & Bogota started 15 min late
foreach var of varlist time_reproduction time_first_minor time_first_major{
	
	replace  `var' = mod(`var', 24*60*60*1000) - hms(0, 15, 0) if game==4 | game==1
	
}

generate time2_reproduction = (mod(time_reproduction, 24*60*60*1000) - hms(9, 0, 0)) / (60*1000)
generate time2_first_minor = (mod(time_first_minor, 24*60*60*1000) - hms(9, 0, 0)) / (60*1000)
generate time2_first_major = (mod(time_first_major, 24*60*60*1000) - hms(9, 0, 0)) / (60*1000)

recode minor_errors major_errors good_robustness (.=0)

gen one_good_robustness =(good_robustness>=1)
gen two_good_robustness =(good_robustness==2)

gen ran_one_robustness =(ran_robustness>=1 & one_good_robustness==1)
gen ran_two_robustness =(ran_robustness==2 & two_good_robustness==1)

gen combined_follow=combined_coding/followup_responces

order game branch team software paper_game paper number_teammates attendance max_skill min_skill reproduction time_reproduction time2_reproduction minor_errors time_first_minor time2_first_minor major_errors time_first_major time2_first_major good_robustness one_good_robustness two_good_robustness ran_robustness ran_one_robustness ran_two_robustness


frame create follow
frame change follow

import excel "data/AI Games - Prompts Information.xlsx", sheet("Sheet1") firstrow
drop N-Y

recode numberofprompts numberoffiles numberofimages numberofwords (.=0)
replace game=proper(game)
*this team didnt have R, so they didnt do anything
drop if game=="cornell" & team=="4r"


collapse (sum) prompts=numberofprompts files=numberoffiles images=numberofimages words=numberofwords, by(game team)

label define game_label 1 "Toronto" 2 "Ottawa" 3 "Sheffield" 4 "Cornell" 5 "Bogota" 6 "tTilburg" 7 "Virtual" 8 "Virtual 2025"
encode game , generate(game2) label(game_label)
drop game
frame change main
frlink 1:1 game2 team, frame(follow)
frget *, from(follow)
drop follow

recode prompts files images words (.=0)

label variable game "Game"
label variable branch "Branch"
label variable team "Team"
label variable paper "Paper"
label variable paper_game "Paper in game"
label variable software "Software"
label variable number_teammates "Number of teammates"
label variable attendance "Attendance"
label variable max_skill "Maximum academic level"
label variable min_skill "Minimum academic level"
label variable reproduction "Reproduction"
label variable time_reproduction "Time of reproduction"
label variable minor_errors "Number of minor errors"
label variable major_errors "Number of major errors"
label variable good_robustness "Number of appropiate robustness checks"
label variable time_first_minor "Minutes of first minor error"
label variable time_first_major "Minutes of first major error"
label variable ran_robustness "Number of robustness run"
label variable min_gpt "Minimum ChatGPT level"
label variable max_gpt "Maximum ChatGPT level"
label variable combined_coding "Combined years of coding experience"
label variable min_coding "Minimum coding level"
label variable max_coding "Maximum ChatGPT level"
label variable followup_responces "Number of follow up survey responces"

label variable time2_reproduction "Minutes to reproduction"
label variable time2_first_minor "Minutes to first minor error"
label variable time2_first_major "Minutes to first major error"
label variable one_good_robustness "At least one appropiate robustness check"
label variable two_good_robustness "At least two appropiate robustness checks"
label variable ran_one_robustness "Ran at leat one appropiate robustness check"
label variable ran_two_robustness "Ran at leat two appropiate robustness checks"
label variable combined_follow "Average years of coding experience"
label variable prompts "Number of prompts for ChatGPT"
label variable files "Number of files for ChatGPT"
label variable images "Number of images for ChatGPT"
label variable words "Number of words for ChatGPT"

compress
save "data/AI games.dta", replace
