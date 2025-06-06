###############################################################
#  FULL CONTROLS • Replica estout (Panel A)                   #
#  ---------------------------------------------------------  #
#  Estudio 1 vs. Estudio 2 (“Virtual 2025”)                   #
#  Genera:  output/tables/study_2.tex                         #
###############################################################

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, fixest, car, broom, glue, stringr)

# ---- 1. Leer y preparar datos -------------------------------------------
df <- readRDS("data/AI games.rds") |>
  mutate(
    study_f = factor(game == "Virtual 2025",
                     levels = c(FALSE, TRUE),
                     labels = c("Study 1", "Study 2")),
    branch  = relevel(factor(branch), "Human-Only"),
    game_software = interaction(game, software, drop = TRUE),
    across(c(max_skill, min_skill, attendance), as.factor)
  )

dep_vars <- c(
  "reproduction","minor_errors","major_errors",
  "one_good_robustness","two_good_robustness",
  "ran_one_robustness","ran_two_robustness"
)

# ---- 2. Auxiliares -------------------------------------------------------
fmt      <- \(x) sprintf("% .3f", x)
fmt_ci   <- \(lo, hi) sprintf("[% .3f; % .3f]", lo, hi)
star     <- \(p) ifelse(is.na(p),"",
                        ifelse(p<.01,"***",ifelse(p<.05,"**",
                                                  ifelse(p<.1,"*",""))))

wald_p <- function(m){
  w <- linearHypothesis(m,"branchAI-Assisted = branchAI-Led",
                        vcov.=vcov(m,type="HC1"))
  as.numeric(w[2,"Pr(>Chisq)"])
}

pick <- function(td, pattern, col){
  v <- td %>% filter(str_detect(term, pattern)) %>% pull({{col}})
  if (length(v)==0) NA_real_ else v[1]
}

summ <- function(m,dv){
  td <- broom::tidy(m, conf.int = TRUE)
  
  tibble(
    # Principales (Study 1)
    b_aa  = pick(td,"branchAI.*Assisted$",estimate),
    se_aa = pick(td,"branchAI.*Assisted$",std.error),
    lo_aa = pick(td,"branchAI.*Assisted$",conf.low),
    hi_aa = pick(td,"branchAI.*Assisted$",conf.high),
    p_aa  = pick(td,"branchAI.*Assisted$",p.value),
    
    b_al  = pick(td,"branchAI.*Led$",estimate),
    se_al = pick(td,"branchAI.*Led$",std.error),
    lo_al = pick(td,"branchAI.*Led$",conf.low),
    hi_al = pick(td,"branchAI.*Led$",conf.high),
    p_al  = pick(td,"branchAI.*Led$",p.value),
    
    # Interacciones con Study 2
    b_iaa  = pick(td,"branchAI.*Assisted:study_fStudy 2$",estimate),
    se_iaa = pick(td,"branchAI.*Assisted:study_fStudy 2$",std.error),
    lo_iaa = pick(td,"branchAI.*Assisted:study_fStudy 2$",conf.low),
    hi_iaa = pick(td,"branchAI.*Assisted:study_fStudy 2$",conf.high),
    p_iaa  = pick(td,"branchAI.*Assisted:study_fStudy 2$",p.value),
    
    b_ial  = pick(td,"branchAI.*Led:study_fStudy 2",estimate),
    se_ial = pick(td,"branchAI.*Led:study_fStudy 2",std.error),
    lo_ial = pick(td,"branchAI.*Led:study_fStudy 2",conf.low),
    hi_ial = pick(td,"branchAI.*Led:study_fStudy 2",conf.high),
    p_ial  = pick(td,"branchAI.*Led:study_fStudy 2",p.value),
    
    ymean = mean(df[[dv]], na.rm = TRUE),
    N     = nobs(m),
    waldp = wald_p(m)
  )
}

# ---- 3. Estimar modelos ---------------------------------------------------
models <- lapply(dep_vars, \(dv)
                 feols(
                   as.formula(glue(
                     "{dv} ~ branch + branch:study_f + number_teammates | ",
                     "software + max_skill + min_skill + attendance")),
                   df, se = "hetero")
)
res <- Map(summ, models, dep_vars)

# ---- 4. Construir tabla LaTeX --------------------------------------------
col_labels <- c(
  "Reproduction",
  "\\shortstack[c]{Minor\\\\errors}",
  "\\shortstack[c]{Major\\\\errors}",
  "\\shortstack[c]{One good\\\\robustness}",
  "\\shortstack[c]{Two good\\\\robustness}",
  "\\shortstack[c]{Ran one\\\\robustness}",
  "\\shortstack[c]{Ran two\\\\robustness}"
)

vec <- function(field, stars=FALSE){
  sapply(res, \(x){
    v <- x[[field]]
    if (is.na(v)) "" else {
      out <- fmt(v)
      if (stars) out <- paste0(out, star(x[[sub("^b_","p_",field)]]))
      out
    }
  })
}
ci_vec <- \(lo, hi) sapply(res, \(x)
                           if (is.na(x[[lo]])) "" else fmt_ci(x[[lo]], x[[hi]]))
row <- \(lab,cells) glue("{lab} & {paste(cells, collapse=' & ')} \\\\")

body <- c(
  row("AI-Assisted",                  vec("b_aa",TRUE)),
  row("",                             paste0("(",vec("se_aa"),")")),
  row("",                             ci_vec("lo_aa","hi_aa")),
  row("AI-Led",                       vec("b_al",TRUE)),
  row("",                             paste0("(",vec("se_al"),")")),
  row("",                             ci_vec("lo_al","hi_al")),
  row("AI-Assisted $\\times$ Study II",vec("b_iaa",TRUE)),
  row("",                             paste0("(",vec("se_iaa"),")")),
  row("",                             ci_vec("lo_iaa","hi_iaa")),
  row("AI-Led $\\times$ Study II",     vec("b_ial",TRUE)),
  row("",                             paste0("(",vec("se_ial"),")")),
  row("",                             ci_vec("lo_ial","hi_ial")),
  "\\hline",
  row("Mean of dep. var",             vec("ymean")),
  row("p-val (AI-Assisted vs. AI-Led)", sapply(res,\(x) fmt(x$waldp))),
  row("Observations",                 sapply(res,\(x) x$N))
)

# ---- 5. Guardar archivo .tex ---------------------------------------------
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
outfile <- "output/tables/study 2.tex"

writeLines(c(
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{l*{7}{c}}",
  "\\hline\\hline",
  "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\\\",
  "                    &", paste(col_labels, collapse = "   &"), "   \\\\",
  "\\hline",
  body,
  "\\hline\\hline",
  "\\multicolumn{8}{l}{\\it{Note:} Standard errors in parentheses, confidence intervals in brackets; human-only branch omitted.}\\\\",
  "\\multicolumn{8}{l}{\\sym{*} \\(p<0.1\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\",
  "\\end{tabular}"
), outfile)

