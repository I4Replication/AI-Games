clear all
*********************
* Necessary packages**
*********************

local pkgs "reghdfe ftools estout rsource ppmlhdfe"

foreach p of local pkgs {
    capture which `p'               // does the command exist?
    if _rc {                        // _rc != 0  →  not installed
        di as txt "Installing `p'..."
        ssc install `p', replace
    }
    else di as txt "`p' already installed."
}


*********************
********Paths********
*********************

global rpath "/usr/local/bin/R"
global path "~/Dropbox/I4R/AI paper"
cd "$path"

log using "output/master_log_stata.log", replace text


*********************
**Cleaning raw data**
*********************

do "code/Stata code/cleaning.do"

*********************
*****Proccesing******
*********************

******Tables
do "code/Stata code/main.do"

do "code/Stata code/logit poisson.do"

do "code/Stata code/full controls.do"

do "code/Stata code/softwares.do"

do "code/Stata code/error shares.do"

do "code/Stata code/study 2.do"


* Power
rsource, terminator(END_OF_R) rpath("$rpath") ro(--vanilla)
setwd("~/Dropbox/I4R/AI paper/R code")
library(haven);
df <- data.frame(readRDS("data/AI games.rds"));
source("code/R code/power.R");
q();
END_OF_R


**Group differences
rsource, terminator(END_OF_R) rpath("$rpath") ro(--vanilla)
setwd("~/Dropbox/I4R/AI paper/R code")
library(haven);
df <- data.frame(readRDS("data/AI games.rds"));
source("code/R code/branches.R");
q();
END_OF_R


**Balance tables
rsource, terminator(END_OF_R) rpath("$rpath") ro(--vanilla)
library(haven);
df <- data.frame(readRDS("data/AI games.rds"));
source("code/R code/balance.R");
q();
END_OF_R


**ChatGPT tables
rsource, terminator(END_OF_R) rpath("$rpath") ro(--vanilla)
library(haven);
df <- data.frame(readRDS("data/AI games.rds"));
source("code/R code/gpt skill.R");
q();
END_OF_R


**Prompts usage tables
rsource, terminator(END_OF_R) rpath("$rpath") ro(--vanilla)
library(haven);
df <- data.frame(readRDS("data/AI games.rds"));
source("code/R code/prompts.R");
q();
END_OF_R

**RMST tables
rsource, terminator(END_OF_R) rpath("$rpath") ro(--vanilla)
library(haven);
df <- data.frame(readRDS("data/AI games.rds"));
source("code/R code/rmst.R");
q();
END_OF_R



******Figures
do "code/Stata code/time to first.do"

do "code/Stata code/reproduction rates.do"

do "code/Stata code/prompt distribution.do"

log close
