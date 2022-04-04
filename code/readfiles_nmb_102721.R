## (updated 2021/10/27 16:07) preliminaries/functions ---------------------
library(kableExtra)
library(janitor)
library(countrycode)
library(Cairo)
library(tidyverse)
library(naniar)
library(formattable)
suppressMessages(library(meta))
settings.meta(CIbracket = "(", CIseparator = ", ")

# tally 2 variables
table_tally_2 <- function(data, var_1, var_2){
  data %>%
    arrange({{var_1}}, {{var_2}}) %>%
    select({{var_1}}, {{var_2}},) %>%
    group_by({{var_1}}, {{var_2}},) %>%
    tally() %>%
    ungroup() %>%
    mutate(end = cumsum(n),
           start = end - n + 1
    ) %>%
    select({{var_1}}, {{var_2}}, start, end)
}

# list of tabyls
tab_lst <- function(data, vars){
  data %>%
    select({{vars}}) %>%
    map(~ tabyl(.))
}

# count of unique for variable
qinu <- function(df, var){
  df %>%
    select({{var}}) %>%
    distinct() %>%
    count() %>%
    pull(n)
}

proc_freq <- function(df, a, b){
  df %>%
    group_by({{a}}, {{b}}) %>%
    tally() %>%
    spread({{a}}, n) %>%
    gt::gt(.)
}

`%notin%` <- Negate(`%in%`)

options(todor_patterns = c("FIXME", "TODO", "CHANGED", "TEXT", "NOTE", "REVIEW", "QUESTION", "RESUME"))

color_1 <- "#76d7c4"
color_2 <- "#7fb3d5"
color_3 <- "#c39bd3"
color_4 <- "#f1948a"

# color_bar2
bg <- function(start, end, color, ...) {
  paste(
    "linear-gradient(270deg, transparent ", percent(start), ",",
    color, percent(start), ",", color, percent(end),
    ", transparent", percent(end), ")"
  )
}

color_bar2 <- function(color = "lightgray", fun = "proportion", ...) {
  fun <- match.fun(fun)
  formatter("span", style = function(x) {
    style(
      display = "inline-block",
      `unicode-bidi` = "plaintext",
      "background" = bg(1 - fun(as.numeric(x), ...), 1, color), "width" = "100%"
    )
  })
}

# function to add color_bar2 w or w/o number
bar_wn = function(var_nper_or_per, percent, color, div_by){
  dichot_format <- function(x) (percent/div_by)
  with_bar <-  color_bar2(color, fun = dichot_format)(var_nper_or_per)
  with_bar <-  str_replace(with_bar, ">.*<", paste0(">", var_nper_or_per, "<"))
  with_bar <- ifelse(is.na(var_nper_or_per), NA, with_bar)
  return(with_bar)
}

bar_wn1d = function(var_nper_or_per, percent, color, div_by){
  dichot_format <- function(x) (percent/div_by)
  with_bar <-  color_bar2(color, fun = dichot_format)(var_nper_or_per)
  with_bar <-  str_replace(with_bar, ">.*<", paste0(">", formatC(var_nper_or_per, digits = 1, format = "f"), "<"))
  with_bar <- ifelse(is.na(var_nper_or_per), NA, with_bar)
  return(with_bar)
}

# spacing
study_width <- "10em"
bar_width <- "7em"
bar_ref <- "<span style='display: inline-block; direction: auto; unicode-bidi: plaintext; border-radius: 0px; padding-right: 0px; background-color: #e0e0de ; width: 100.00%'>   0%  →  100%</span>"
or_width <- "8em"
note_width <- "12em"

# convenience
# ci using t dist
ci_t = function(m, sd, n, low_up){
  low_up <- ifelse(low_up == "low", -1, 1)
  m + low_up * qt(0.975, n) * sd
}

# kable defaults
opt_font <-  c("Source Sans Pro")
opt_boot <- c("striped", "hover", "condensed")

knitr::opts_chunk$set(
  echo = FALSE,
  options(knitr.kable.NA = "", dev = "svg"),
  knitr.graphics.error = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.align = "left",
  comment = NA
)

# functions for all
round_0    <- function(x) {formatC( round(x, 0), format = 'f', digits = 0)}
round_1    <- function(x) {formatC( round(x, 1), format = 'f', digits = 1)}
round_1d   <- function(x) {format(x, digits = 1)}
round_0d   <- function(x) {format(x, digits = 0)}
round_2d   <- function(x) {formatC(x, digits = 2, zero.print = TRUE)}
round_2    <- function(x) {formatC( round(x, 2), format = 'f', digits = 2)}
sd_conf_t  <- function(low, up, n){(abs(up - low)/2)/abs(qt(0.025, n))}
sd_confint <- function(low, up){(abs(up - low)/2)/1.96}
sd_conf_cont_t <- function(low, up, n){sqrt(n)*(abs(up - low)/2)/abs(qt(0.025, n))}
sd_confint_cont <- function(low, up, n){sqrt(n)*(abs(up - low)/2)/1.96}

conf_tl    <- function(samp_mean, samp_sd, n, low_up) {
  error <- qt(0.975, df = n - 1) * samp_sd / (sqrt(n))
  (ci_low <- samp_mean - error)
}

conf_tu <- function(samp_mean, samp_sd, n, low_up) {
  error <- qt(0.975, df = n - 1) * samp_sd / (sqrt(n))
  (ci_up <- samp_mean + error)
}

conf_q <- function(samp_mean, samp_sd, n) {
  error <- qnorm(0.975) * samp_sd / (sqrt(n))
  ci_up <- samp_mean + error
  ci_low <- samp_mean - error
  c(ci_low, ci_up)
}

# variable coded as TF with NAs will give "n (XX.X)" percent, can specify digits

n_per_tfna <- function(var_name, n_dig = 1){
  paste0(sum(var_name == TRUE, na.rm = TRUE), " (", format(round(100*(mean(var_name, na.rm = TRUE)), n_dig), nsmall = 1), ")")
}

# variable coded as TF will give "n (XX.X)" percent, can specify digits
n_per_tf <- function(var_name, n_dig = 1){
  paste0(sum(var_name == TRUE), " (", format(round(100*(mean(var_name)), n_dig), nsmall = 1), ")")
}

combine_contin <- function(n_1, n_2, x_1, x_2, sd_1, sd_2){
  n_comb <- n_1 + n_2
  x_comb <-  (n_1 * x_1 + n_2 * x_2)/(n_comb)
  sd_comb <- sqrt(((n_1 - 1) * sd_1^2 + (n_2 - 1) * sd_2^2 + (n_1 * n_2)/(n_comb) * (x_1 - x_2)^2) / (n_comb - 1))
  return(c(n_comb, x_comb, sd_comb))}

n_percent <- function(a, b){
  str_c(a," (", round_0(b), ")")
}

n_percent1 <- function(a, b){
  str_c(a," (", round_1(b), ")")
}

n_percent_dig <- function(a, b, n){
  str_c(a," (", formatC(b, digits = n, format = "f"), ")")
}

bwgrp_pval <- function(m1, m2, n1, n2, pVal) {
  sd2 <- abs((m2 - m1) / qt(pVal / 2, n1 + n2 - 2)) * sqrt(n2 * n1 / (n2 + n1))
  sd1 <- sd2
  print(c(n1, m1, sd1, n2, m2, sd2))
}

# return single value
sd_bwgrp <- function(m1, m2, n1, n2, pVal) {
  abs((m2 - m1) / qt(pVal / 2, n1 + n2 - 2)) * sqrt(n2 * n1 / (n2 + n1))
}

# top level kable
pack_top <- function(kable_input, name, a, b, indent_tf = TRUE) {
  pack_rows(kable_input, name,
            start_row = a, end_row = b,
            indent = indent_tf,
            label_row_css = "border-top: 1px solid;",
            color = "black",
            background = "#EBEBEB",
  )
}

# top level kable no indent
pack_topnoind <- function(kable_input, name, a, b) {
  pack_rows(kable_input, name,
            start_row = a, end_row = b,
            label_row_css = "border-top: 1px solid;", color = "black", background = "#EBEBEB",
            indent = FALSE
  )
}

# subgroup title kable
pack_sub <- function(kable_input, name, a, b) {
  pack_rows(kable_input, name,
    start_row = a, end_row = b,
    label_row_css = "border-bottom: 0px solid;", color = "black", background = "white", bold = FALSE,  indent = FALSE
  )
}

pack_sub_bold <- function(kable_input, name, a, b) {
  pack_rows(kable_input, name,
            start_row = a, end_row = b,
            label_row_css = "border-bottom: 0px solid;", color = "#646971", background = "white", bold = TRUE,  indent = FALSE, italic = FALSE
  )
}

# add results in row for individual study
result_pack <- function(kable_input, result, row, padding = 62, bold = FALSE, color = "gray", italic = TRUE){
  padding <- paste0("padding-left:" , padding, "%; ", "border-bottom: 0px solid;")
  pack_rows(kable_input, result, row, row, label_row_css = padding, underline = FALSE, bold = bold, color = color, italic = italic, indent = FALSE)
}

result_pack_line <- function(kable_input, result, row, padding = 62, bold = FALSE, color = "gray", italic = TRUE){
  padding <- paste0("padding-left:" , padding, "%; ", "border-bottom: 0.5px solid gray;")
  pack_rows(kable_input, result, row, row, label_row_css = padding, underline = FALSE, bold = bold, color = color, italic = italic, indent = FALSE)
}

# capitalize 1st letter
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# convenience function to view record(s) for refid from dataset
view_rec <- function(data_set, refid_select) {
  data_set %>%
    filter(refid == refid_select) %>%
    janitor::remove_empty(which = "cols") %>%
    t() %>%
    View()
}

# odds ratio
or_join <- function(a_tbl, outcome) {
  left_join(a_tbl %>% filter(row_number() == 1), a_tbl %>% filter(row_number() == 2), by = "refid") %>%
    select(-study.y) %>%
    rename(
      c_tx = arm_tx.x, tx = arm_tx.y, study = study.x,
      n_c = arm_n.x, event_c = str_c(outcome, "_n.x"),
      n_e = arm_n.y, event_e = str_c(outcome, "_n.y"))
}

# calculate odds ratio, ci, and format no refid
odds_ratio_y <- function(event1, n1, event2, n2, digits = 2) {
  a <- meta::metabin(event1, n1, event2, n2, sm = "OR")
  with(a, paste0(
    sprintf(paste0("%.", digits, "f"), round(exp(TE), digits)), " (",
    sprintf(paste0("%.", digits, "f"), round(exp(lower), digits)), "-",
    sprintf(paste0("%.", digits, "f"), round(exp(upper), digits)), ")"))
  # rename(or_ci = a)
}

# calculate relative risk, ci, and format no refid
risk_ratio_y <- function(event1, n1, event2, n2, digits = 2) {
  a <- meta::metabin(event1, n1, event2, n2, sm = "RR")
  with(a, paste0(
    sprintf(paste0("%.", digits, "f"), round(exp(TE), digits)), " (",
    sprintf(paste0("%.", digits, "f"), round(exp(lower), digits)), "-",
    sprintf(paste0("%.", digits, "f"), round(exp(upper), digits)), ")"))
  # rename(or_ci = a)
}



# calculate odds ratio, ci, and format
odds_ratio <- function(event1, n1, event2, n2, refid, digits = 2) {
  a <- meta::metabin(event1, n1, event2, n2, sm = "OR")
  a <- with(a, paste0(
    sprintf(paste0("%.", digits, "f"), round(exp(TE), digits)), " (",
    sprintf(paste0("%.", digits, "f"), round(exp(lower), digits)), "-",
    sprintf(paste0("%.", digits, "f"), round(exp(upper), digits)), ")"))
  tibble(refid, a) %>%
    rename(or_ci = a)
}

# add a footnote to a variable and value eg, add_foot(study, "Tudor-Drobjewski 2018", "c")
add_foot <- function(.data, use_var, study_to_footnote, value) {
  foot_letter <- paste0("^", value, "^")
  use_var <- enquo(use_var)
  .data %>%
    mutate(!!use_var := if_else(!!use_var == study_to_footnote, paste0(study_to_footnote, foot_letter), !!use_var))
}

path_csv <- function(name_csv){
  sheet <- name_csv
  path <- str_c("data/", sheet)
  return(path)
}

# function to calculate means, sd, and create analytical data set (w/log transform)
calc_mn_sd <- function(n_e, m_e, sd_e, md_e, q1_e, q3_e, min_e, max_e, study, tx, subgroup = NULL, refid, data = NULL, log_trans = FALSE) {
  temp.dat <- data %>%
    select(all_of(c(n_e, m_e, sd_e, md_e, q1_e, q3_e, min_e, max_e, study, tx, subgroup, refid))) %>%
    # placeholder control arm values to obtain standard deviations if missing
    mutate(n_c = 20, m_c = 2, sd_c = 1, md_c = 3, q1_c = 2, q3_c = 3, min_c = 1, max_c = 5)
  names(temp.dat)[1:11] <- c("n_e", "m_e", "sd_e", "md_e", "q1_e", "q3_e", "min_e", "max_e", "study", "tx", "subgroup")

  temp <- metacont(
    n.e = n_e,
    n.c = n_c,
    fixed = TRUE,
    mean.e = m_e,
    sd.e = sd_e,
    median.e = md_e,
    q1.e = q1_e,
    q3.e = q3_e,
    min.e = min_e,
    max.e = max_e,
    mean.c = m_c,
    sd.c = sd_c,
    median.c = md_c,
    q1.c = q1_c,
    q3.c = q3_c,
    min.c = min_c,
    max.c = max_c,
    sm = "SMD",
    studlab = study,
    data = temp.dat
  )

  temp <- as_tibble(temp[c("studlab", "n.e", "mean.e", "sd.e")])

  mean_raw_log <- function(raw_mean, raw_sd) {
    (log_mean <- log(raw_mean) - 0.5 * log(raw_sd^2 / raw_mean^2 + 1))
  }
  sd_raw_log <- function(raw_mean, raw_sd) {
    (log_sd <- sqrt(log(raw_sd^2 / raw_mean^2 + 1)))
  }

  if (log_trans == TRUE) {
    temp$mean_log.e <- mean_raw_log(temp$mean.e, temp$sd.e)
    temp$sd_log.e <- sd_raw_log(temp$mean.e, temp$sd.e)
  }
  temp <- cbind(temp.dat$tx, temp)
  names(temp) <- c("arm_tx", "study", "n", "mean", "sd", "mean_log", "sd_log")
  temp[, c(2, 1, 3:7)]
}

tab_inc <- function() {
  table_n <- table_n + 1
  table_n}
fig_inc <- function() {
  figure_n <- figure_n + 1
  figure_n}

# function recode design to formatted names
design_fac_recode <- function(des_var) {
  des_var <- fct_recode(des_var,
    "RCT"                   = "rct",
    "Crossover"             = "crossover",
    "Cluster"               = "cluster",
    "Fully paired"          = "fully_paired",
    "Quasi-experimental"    = "quasiexp",
    "NRSI"                  = "nrsi",
    "Prospective cohort"    = "prospect_coh",
    "Retrospective cohort"  = "retrospect_coh",
    "Case control"          = "casecontrol",
    "Case series"           = "case_series",
    "Other"                 = "other"
  )
}

# function factor study design
# design_fac <- function(des_var) {
#   des_var <- factor(des_var,
#     levels = c(
#       "rct",
#       "crossover",
#       "cluster",
#       "fully_paired",
#       "quasiexp",
#       "nrsi",
#       "prospect_coh",
#       "retrospect_coh",
#       "casecontrol",
#       "case_series",
#       "other"
#     )
#   )
# }

## data files ####
data_files <- as_tibble(list.files("data/"))

study_char_file <- data_files %>%
  filter(str_detect(value, "studyChar")) %>%
  arrange(desc(value)) %>%
  slice(1)

age_file <- data_files %>%
  filter(str_detect(value, "age")) %>%
  arrange(desc(value)) %>%
  slice(1)

study_arm_file <- data_files %>%
  filter(str_detect(value, "studyArm")) %>%
  arrange(desc(value)) %>%
  slice(1)

cont_out_file <- data_files %>%
  filter(str_detect(value, "contOutcomes")) %>%
  arrange(desc(value)) %>%
  slice(1)

dichot_out_file <- data_files %>%
  filter(str_detect(value, "dichotOutcomes")) %>%
  arrange(desc(value)) %>%
  slice(1)

likert_out_file <- data_files %>%
  filter(str_detect(value, "likertOutcomes")) %>%
  arrange(desc(value)) %>%
  slice(1)

rob_file <- data_files %>%
  filter(str_detect(value, "rob_")) %>%
  arrange(desc(value)) %>%
  slice(1)

study_refs_file <- data_files %>%
  filter(str_detect(value, "studyRefs")) %>%
  arrange(desc(value)) %>%
  slice(1)

# display file characteristics
a <- as.character(file.mtime(paste0("data/", study_arm_file)))
b <- as.character(file.mtime(paste0("data/", study_char_file)))
c <- as.character(file.mtime(paste0("data/", cont_out_file)))
d <- as.character(file.mtime(paste0("data/", dichot_out_file)))
e <- as.character(file.mtime(paste0("data/", likert_out_file)))
f <- as.character(file.mtime(paste0("data/", rob_file)))
g <- as.character(file.mtime(paste0("data/", study_refs_file)))
z <- matrix(c(
  paste(b, study_char_file),
  paste(a, study_arm_file),
  paste(c, cont_out_file),
  paste(d, dichot_out_file),
  paste(e, likert_out_file),
  paste(f, rob_file),
  paste(g, study_refs_file)))
z

# save list of files in current analysis
write_delim(data.frame(z), "used_files_dates.txt", delim = "--", col_names = FALSE)
rm(a, b, c, d, e, f, g, z)

## study characteristics ####
path <- path_csv(study_char_file)

study_char.dat <- read_csv(path) %>%
  janitor::clean_names() %>%
  select(-ris_code, -level, -study_char_k) %>%
  rename(author_dist = author, author = author_added, age = patient_pop) %>% # author distiller, author entered
  select(refid, year, age, author:comment, linked_references, labels) %>%
  mutate(age = case_when(
    age == "adult" ~ "Adult",
    age == "adult_peds" ~ "All ages",
    age == "peds" ~ "Pediatric",
    TRUE ~ "MISSING/FIX"),
  age = if ("MISSING/FIX" %in% age) stop() else age
  ) %>%
  group_by(refid) %>%
  slice(1) %>%  # use to temporarily remove duplicates from run-in
  ungroup()


# number of studies
(study_char_n <- study_char.dat %>%
    distinct(refid) %>%
    count())

study_char.dat <- study_char.dat %>%
  mutate(
    design = factor(design,
      levels = c(
        "rct",
        "crossover",
        "cluster",
        "fully_paired",
        "quasiexp",
        "nrsi",
        "prospect_coh",
        "retrospect_coh",
        "casecontrol",
        "case_series",
        "other"
      )
    ),
    design_f = fct_recode(design,
      "RCT" = "rct",
      "Crossover" = "crossover",
      "Cluster Randomized" = "cluster",
      "Fully Paired" = "fully_paired",
      "Before-After/Time Series" = "quasiexp",
      "Nonrandomized Trial" = "nrsi",
      "Prospective Cohort" = "prospect_coh",
      "Retrospective Cohort" = "retrospect_coh",
      "Case-Control" = "casecontrol",
      "Case Series" = "case_series",
      "Other" = "other"
    ),
    # shorten select study names
    author = case_when(
      refid == 3521 ~ "S Machado",
      refid == 1064 ~ "K-Nielsen",
      TRUE ~ author
    ),
    study = paste(author, year),
    study_l = paste0("[", study, "]", "(", "evidence_tables.html#", refid, ")")
  ) %>%
  relocate(design_f, .after = design) %>%
  select(refid, study, study_l, everything())

study_char.dat %>%
  select(design) %>%
  tabyl(design)

# study_identifiers
study_ident <- study_char.dat %>%
  select(refid, study, study_l, age, design, design_f)

# * (end)

## create new author field and append a, b, c etc for same year ####
# list of multiple author-year publications
# temp_1 <- study_char.dat %>%
#   mutate(study = paste(stringr::str_extract(author_dist, "^([^,])+"), year)) %>%
#   arrange(author, refid) %>%
#   group_by(study) %>%
#   mutate(
#     n = row_number(),
#     let_add = ifelse(n > 0, letters[n], "")) %>%
#   ungroup() %>%
#   mutate(study_r = str_c(study, let_add))
#
# # with multiple per year letters added to year n = XX
# temp_2 <- temp_1 %>%
#   group_by(study) %>%
#   filter(n() > 1) %>%
#   ungroup() %>%
#   mutate(study = study_r) %>%
#   select(-study_r)
#
# # one publication per year n = 13 12/5/20
# temp_1 <- temp_1 %>%
#   group_by(study) %>%
#   filter(n() == 1) %>%
#   ungroup() %>%
#   select(-study_r)
#
# study_char.dat <- bind_rows(temp_1, temp_2) %>%
#   # select(!c(author, author_dist, refid_ver, year_added, pub_type, n, let_add)) %>%
#   select(!c(author, author_dist, user, reviewer, refid_ver, year_added, n, let_add)) %>%
#   select(refid, year, study, design, everything())
#
# rm(temp_1, temp_2)

## study references ----------------------------
study_refs.dat <- read_csv(paste0("data/", study_refs_file)) %>%
  clean_names() %>%
  remove_empty(which = "cols") %>%
  select(-c(user, level, linked_references))

## study arm ####
path <- path_csv(study_arm_file)
study_arm.dat <- read_csv(path)

# number of studies date n =
(study_arm_n <- study_arm.dat %>%
    distinct(Refid) %>%
    count())

# delete <- length(names(study_arm.dat))
study_arm.dat <- study_arm.dat %>%
  janitor::clean_names() %>%
  select(-c(user, labels, ris_code, level, design)) %>%
  group_by(refid) %>%
  mutate(arm_id = row_number()) %>%
  ungroup()

# use updated study names for duplicate author year, appended w/letter
# use study_char design
study_names <- study_char.dat %>% select(refid, study, study_l, age, design)

study_arm.dat <- left_join(study_arm.dat, study_names, by = "refid") %>%
  mutate(
    mon_cat = str_replace(mon_cat, "moncat_", ""),
    mon_cat_f = factor(mon_cat),
    mon_cat_f = fct_collapse(mon_cat_f, "Quant" = "quan", "Qual" = "qual", "Clin" = "clin", "None/NS" = c("none", "not_described")),
    mon_cat_f = fct_relevel(mon_cat_f, c("Quant", "Qual", "Clin", "None/NS"))
  ) %>%
  select(refid, arm_id, study, study_l, year, design, age, everything()) %>%
  mutate(across(starts_with("depth_rev"), ~ str_replace(.x, "depth_rev_", ""))) %>%
  unite(., col = "depth_rev_f", depth_rev_deep:depth_rev_clincrit, sep = ":", remove = FALSE, na.rm = TRUE) %>%
  mutate(depth_rev_f = factor(depth_rev_f, levels = c("deep", "deep:mod", "deep:mod:shall", "mod", "mod:shall", "mod:shall:min", "min", "shall", "clincrit", "endsurg", "oth", "ns"))) %>%
  relocate(depth_rev_f, .before = depth_rev_deep) %>%
  relocate(linked_references, .after = last_col()) %>%
  relocate(mon_cat_f, .after = mon_cat)

# check n same as study_arm_n
length(unique(study_arm.dat$refid)) == study_arm_n

# * (end)

## continuous outcome data ####
path <- path_csv(cont_out_file)

contin.dat <- read_csv(path) %>%
  janitor::clean_names() %>%
  select(-c(user, labels, ris_code, level, design)) %>%
  group_by(refid) %>%
  mutate(arm_id = row_number()) %>%
  ungroup()

(contin_n <- contin.dat %>%
    distinct(refid) %>%
    count())

# use updated study names for duplicate author year, appended w/letter
# use study_char design
contin.dat <- left_join(contin.dat, study_names, by = "refid") %>%
  select(refid, arm_id, study, study_l, year, design, age, everything()) %>%
  mutate(across(starts_with("depth_rev"), ~ str_replace(.x, "depth_rev_", ""))) %>%
  unite(., col = "depth_rev_f", depth_rev_deep:depth_rev_clincrit, sep = ":", remove = FALSE, na.rm = TRUE) %>%
  mutate(depth_rev_f = factor(depth_rev_f, levels = c("deep", "deep:mod", "deep:mod:shall", "mod", "mod:shall", "mod:shall:min", "min", "shall", "clincrit", "endsurg", "oth", "ns"))) %>%
  relocate(depth_rev_f, .before = depth_rev_deep) %>%
  relocate(linked_references, .after = last_col())

# check n same as study_arm_n
length(unique(contin.dat$refid)) == contin_n

# * (end)

## dichotomous outcome data ####
path <- path_csv(dichot_out_file)
dichot.dat <- read_csv(path) %>%
  janitor::clean_names() %>%
  select(-c(user, labels, ris_code, level, design))

(dichot_n <- dichot.dat %>%
    distinct(refid) %>%
    count())

# use updated study names for duplicate author year, appended w/letter
# use study_char design
dichot.dat <- left_join(dichot.dat, study_names, by = "refid") %>%
  select(refid, study, study_l, year, design, age, everything()) %>%
  group_by(refid) %>%
  mutate(arm_id = row_number()) %>%
  ungroup() %>%
  mutate(across(starts_with("depth_rev"), ~ str_replace(.x, "depth_rev_", ""))) %>%
  unite(., col = "depth_rev_f", depth_rev_deep:depth_rev_clincrit, sep = ":", remove = FALSE, na.rm = TRUE) %>%
  mutate(depth_rev_f = factor(depth_rev_f, levels = c("deep", "deep:mod", "deep:mod:shall", "mod", "mod:shall", "mod:shall:min", "min", "shall", "clincrit", "endsurg", "oth", "ns"))) %>%
  relocate(depth_rev_f, .before = depth_rev_deep) %>%
  relocate(linked_references, .after = last_col()) %>%
  relocate(arm_id, .after = refid)

# check n same as study_arm_n
length(unique(dichot.dat$refid)) == dichot_n

# * (end)

## likert outcome data ####
path <- path_csv(likert_out_file)
likert.dat <- read_csv(path) %>%
  janitor::clean_names() %>%
  select(-c(user, labels, ris_code, level, design)) %>%
  group_by(refid) %>%
  mutate(arm_id = row_number()) %>%
  ungroup()

(likert_n <- likert.dat %>%
    distinct(refid) %>%
    count())

# use updated study names for duplicate author year, appended w/letter
# use study_char design
likert.dat <- left_join(likert.dat, study_names, by = "refid") %>%
  select(refid, arm_id, study, study_l, year, design, age, everything()) %>%
  # mutate(across(starts_with("depth_rev"), ~ str_replace(.x, "depth_rev_", ""))) %>%
  # unite(., col = "depth_rev_f", depth_rev_deep:depth_rev_clincrit, sep = ":", remove = FALSE, na.rm = TRUE) %>%
  # mutate(depth_rev = factor(depth_rev_f, levels = c("deep", "deep:mod", "deep:mod:shall", "mod", "mod:shall", "mod:shall:min", "min", "shall", "clincrit", "endsurg", "oth", "ns"))) %>%
  # relocate(depth_rev_f, .before = depth_rev_deep) %>%
  relocate(linked_references, .after = last_col())

# check n same as study_arm_n
length(unique(likert.dat$refid)) == dichot_n

# * (end)

## (updated 2021/11/05 17:09) cleanup -------------------------------------
rm(list = ls(pattern = "*.file"))
rm(list = ls(pattern = "*_n"))
rm(path)

table_n <- 1
figure_n <- 1

## (updated 2021/11/05 13:09) rob data ------------------------------------
## (updated 2021/11/05 13:10) quadas --------------------------------------
## (updated 2021/11/05 13:10) robins-i ------------------------------------

