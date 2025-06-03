# Reproducibility, Coding‑Error Detection & Robustness  
### Comparing Human‑Only, AI‑Assisted, and AI‑Led Teams on Assessing Research Reproducibility in Quantitative Social Science

> **Short‑version** This repo contains **17 R scripts** and **10 Stata scripts** that jointly reproduce every table and figure in Brodeur *et al.* (2025).  
> Stata’s `master.do` installs its own dependencies and calls the “R‑only” modules via `{rsource}`, so you can stay inside Stata while still getting the full analysis.

[![MIT Licence](https://img.shields.io/badge/Code-MIT-blue.svg)](LICENSE) 
[![CC‑BY 4.0](https://img.shields.io/badge/Data-CC--BY_4.0-lightgrey.svg)](LICENSE-data) 
![Last Updated](https://img.shields.io/badge/updated-2025‑06‑03-success)

---

## 1  |  Project layout

```
├── data/                # Raw + processed datasets
│   ├── AI games.xlsx
│   ├── AI Games - Prompts Information.xlsx
│   ├── AI games.dta
│   └── AI games.rds
├── code/
│   ├── R code/          # 17 scripts (analysis & figures)
│   └── Stata code/      # 10 scripts + master.do
└── output/
    ├── tables/
    ├── figures/
    └── logs/
```

---

## 2  |  Quick start

### 2.1 R workflow

```r
# from the repo root
source("code/R code/master.R")
```

### 2.2 Stata workflow

You have two options:

| Option | Command | When to use |
|--------|---------|-------------|
| **A. Launch from repo root** |  
```stata
cd "/path/to/AI paper"
global path "`c(pwd)'"           // project root
do "code/Stata code/master.do"
``` | Clone the repo and run directly. |
| **B. Keep original globals** | Edit the `global path` line in `master.do` to your local clone location. | If you prefer hard‑coded paths. |

The Stata pipeline:

1. Checks & installs required packages (`reghdfe`, `ftools`, …).  
2. Runs core Stata analysis (`cleaning.do`, `main.do`, etc.).  
3. Calls the remaining R‑only scripts with  
   ```stata
   rsource … , rpath("/usr/local/bin/R")
   ```  
   so results stay consistent across languages.

---

## 3  |  Dependencies

### R ≥ 4.4.0
Auto‑installed via `pacman::p_load()`:
```
haven, rmarkdown, readxl,
dplyr, stringr, tidyr, forcats, janitor, lubridate,
fixest, purrr, broom, tibble, car, margins,
sandwich, lmtest, multcomp, kableExtra,
ggplot2, patchwork, modelsummary,
ggsurvfit, survRM2, xtable
```

### Stata ≥ 17/MP
`reghdfe`, `ftools`, `estout`, `ppmlhdfe`, `rsource`

---

## 4  |  Script map (Stata ↔︎ R)

| Block | Stata script | R script (if any) | Purpose |
|-------|--------------|-------------------|---------|
| Cleaning | `cleaning.do` | `cleaning.R` | Raw → tidy |
| Main OLS | `main.do` | `main.R` | Core regressions |
| Logit/Poisson | `logit poisson.do` | `logit poisson.R` | Alt link functions |
| Full controls | `full controls.do` | `full controls.R` | Max covariate set |
| Software heterogeneity | `softwares.do` | `softwares.R` | Split by software |
| Error shares | `error shares.do` | `error shares.R` | Outcome decomposition |
| Study‑2 interaction | `study 2.do` | `study 2.R` | Wave‑specific effects |
| Power analysis | — | `power.R` | Ex‑post power *(R‑only)* |
| Branch differences | — | `branches.R` | Human vs AI |
| Balance tables | — | `balance.R` | Covariate balance |
| GPT skill | — | `gpt skill.R` | Skill heterogeneity |
| Prompt usage | — | `prompts.R` | Prompt heterogeneity |
| RMST | — | `rmst.R` | Restricted‑mean survival |
| Time‑to‑event figs | `time to first.do` | `time to first.R` | Kaplan‑Meier curves |
| KM figs | `reproduction rates.do` | `reproduction rates.R` | Rates across events |
| Prompt distribution | `prompt distribution.do` | `prompt distribution.R` | Usage distribution |

---

## 5  |  Citation

If you build on this code or data, please cite:

> **Abel Brodeur**, David Valenta, Alexandru Marcoci, Juan P. Aparicio, Derek Mikola, Bruno Barbarioli, Rohan Alexander, Lachlan Deer, Tom Stafford, Lars Vilhuber, Gunther Bensch *et al.* (2025).  
> “Comparing Human‑Only, AI‑Assisted, and AI‑Led Teams on Assessing Research Reproducibility in Quantitative Social Science.”  
> *Working paper, under revision at Nature*.

A machine‑readable **CITATION.cff** is included for convenience.

---

## 6  |  Licence

* **Code** – © Abel Brodeur, 2025 • MIT Licence  
* **Data & generated figures** – CC‑BY 4.0

See the `LICENSE` and `LICENSE-data` files for full terms.

---

*Last updated: 03 Jun 2025*
