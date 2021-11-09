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
round_2    <- function(x) {formatC( round(x, 2), format = 'f', digits = 2)}
sd_conf_t  <- function(low, up, n){(abs(up - low)/2)/abs(qt(0.025, n))}
sd_confint <- function(low, up){(abs(up - low)/2)/1.96}

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
pack_top_noind <- function(kable_input, name, a, b) {
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
    label_row_css = "border-bottom: 0px solid;", color = "black", background = "white", bold = FALSE, indent = FALSE
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

# display file characteristics
a <- as.character(file.mtime(paste0("data/", study_arm_file)))
b <- as.character(file.mtime(paste0("data/", study_char_file)))
c <- as.character(file.mtime(paste0("data/", cont_out_file)))
d <- as.character(file.mtime(paste0("data/", dichot_out_file)))
e <- as.character(file.mtime(paste0("data/", likert_out_file)))
f <- as.character(file.mtime(paste0("data/", rob_file)))
z <- matrix(c(
  paste(b, study_char_file),
  paste(a, study_arm_file),
  paste(c, cont_out_file),
  paste(d, dichot_out_file),
  paste(e, likert_out_file),
  paste(f, rob_file)))
z

# save list of files in current analysis
write_delim(data.frame(z), "used_files_dates.txt", delim = "--", col_names = FALSE)
rm(a, b, c, d, e, f, z)

## study characteristics ####
path <- path_csv(study_char_file)

study_char.dat <- read_csv(path) %>%
  janitor::clean_names() %>%
  select(-ris_code, -level, -study_char_k) %>%
  rename(author_dist = author, author = author_added) %>% # author distiller, author entered
  select(refid, year, author_dist:results, linked_references, labels) %>%
  group_by(refid) %>%
  slice(1) %>%  # use to temporarily remove duplicates from run-in
  ungroup()

# number of studies
(study_char_n <- study_char.dat %>%
    distinct(refid) %>%
    count())

## add age from full-text screening level, study design categories ####
age.dat <- read_csv(paste0("data/", age_file), col_types = c("ncccnccncccccccccc")) %>%
  janitor::clean_names() %>%
  select(refid, age) %>%
  filter(refid %in% unique(study_char.dat$refid)) %>%
  distinct() # duplicate records at full-text screening

study_char.dat <- study_char.dat %>%
  left_join(age.dat, study_char.dat, by = "refid") %>%
  select(refid, year, age, everything())

study_char.dat <- study_char.dat %>%
  mutate(
    design = ifelse(design == "quasiexp", "nrsi", design),
    design = factor(design,
      levels = c(
        "rct",
        "crossover",
        "cluster",
        "nrsi",
        "prospect_coh",
        "retrospect_coh",
        "casecontrol",
        "case_series",
        "other"
      )
    )
  ) %>%
    mutate(study = paste(stringr::str_extract(author_dist, "^([^,])+"), year)) %>%
    select(refid, study, everything()) %>%
    relocate(doi, .after = last_col()) %>%
    relocate(title, .after = last_col())

study_char.dat %>%
  select(design) %>%
  tabyl(design)

# * (end)

## create new author field and append a, b, c etc for same year ####
# list of multiple author-year publications
temp_1 <- study_char.dat %>%
  mutate(study = paste(stringr::str_extract(author_dist, "^([^,])+"), year)) %>%
  arrange(author, refid) %>%
  group_by(study) %>%
  mutate(
    n = row_number(),
    let_add = ifelse(n > 0, letters[n], "")) %>%
  ungroup() %>%
  mutate(study_r = str_c(study, let_add))

# with multiple per year letters added to year n = XX
temp_2 <- temp_1 %>%
  group_by(study) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  mutate(study = study_r) %>%
  select(-study_r)

# one publication per year n = 13 12/5/20
temp_1 <- temp_1 %>%
  group_by(study) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  select(-study_r)

study_char.dat <- bind_rows(temp_1, temp_2) %>%
  # select(!c(author, author_dist, refid_ver, year_added, pub_type, n, let_add)) %>%
  select(!c(author, author_dist, user, reviewer, refid_ver, year_added, n, let_add)) %>%
  select(refid, year, study, design, everything())

rm(temp_1, temp_2)

## study arm ####
path <- path_csv(study_arm_file)
columns <- c("ncccnccnccccccccncccccccccccccccccccccnnnnnnnnncccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccnnccccncnnncncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnc")
study_arm.dat <- read_csv(path, col_types = columns)

# number of studies date n =
(study_arm_n <- study_arm.dat %>%
    distinct(Refid) %>%
    count())

# delete <- length(names(study_arm.dat))
study_arm.dat <- study_arm.dat %>%
  janitor::clean_names() %>%
  # temp fix duplicates [DELETE WHEN FIXED]
  filter(!(refid %in% c(60, 664, 868, 887) & user == "Anne_Marbella")) %>%
  filter(!(refid == 1472 & user == "Madhulika_Agarkar")) %>%
  # --------------------------------------------------------------------- *
  select(-c(user, labels, ris_code, level, design))

# use updated study names for duplicate author year, appended w/letter
# use study_char design
study_names <- study_char.dat %>% select(refid, study, age, design)

study_arm.dat <- left_join(study_arm.dat, study_names, by = "refid") %>%
  select(refid, study, year, design, age, everything()) %>%
  relocate(linked_references, .after = last_col())

# check n same as study_arm_n
length(unique(study_arm.dat$refid)) == study_arm_n

# * (end)

## continuous outcome data ####
path <- path_csv(cont_out_file)

contin.dat <- read_csv(path) %>%
  janitor::clean_names() %>%
  # temp fix duplicates [DELETE WHEN FIXED]
  filter(!(refid %in% c(60, 664, 868, 887) & user == "Anne_Marbella")) %>%
  filter(!(refid == 1472 & user == "Madhulika_Agarkar")) %>%
  # --------------------------------------------------------------------- *
  select(-c(user, labels, ris_code, level, design))

(contin_n <- contin.dat %>%
    distinct(refid) %>%
    count())

# use updated study names for duplicate author year, appended w/letter
# use study_char design
contin.dat <- left_join(contin.dat, study_names, by = "refid") %>%
  select(refid, study, year, design, age, everything()) %>%
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
  select(refid, study, year, design, age, everything()) %>%
  relocate(linked_references, .after = last_col())

# check n same as study_arm_n
length(unique(dichot.dat$refid)) == dichot_n

# * (end)

## likert outcome data ####
path <- path_csv(likert_out_file)
likert.dat <- read_csv(path) %>%
  janitor::clean_names() %>%
  select(-c(user, labels, ris_code, level, design))

(likert_n <- likert.dat %>%
    distinct(refid) %>%
    count())

# use updated study names for duplicate author year, appended w/letter
# use study_char design
likert.dat <- left_join(likert.dat, study_names, by = "refid") %>%
  select(refid, study, year, design, age, everything()) %>%
  relocate(linked_references, .after = last_col())

# check n same as study_arm_n
length(unique(likert.dat$refid)) == dichot_n

# * (end)

## ------------------------------------------------------------------------
## (updated 2021/11/05 17:09) cleanup -------------------------------------
rm(list = ls(pattern = "*.file"))
rm(list = ls(pattern = "*_n"))
rm(age.dat)

## ------------------------------------------------------------------------

## (updated 2021/11/05 13:09) lme4: Mixed-effects modeling
## (updated 2021/11/05 13:09) rob data ------------------------------------
## (updated 2021/11/05 13:10) quadas --------------------------------------
## (updated 2021/11/05 13:10) robins-i ------------------------------------

