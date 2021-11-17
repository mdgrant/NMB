sdConfint <- function(low, up){(abs(up-low)/2)/1.96}
sdConfintT <- function(low, up, n){(abs(up-low)/2)/abs(qt(0.025, n))}

mean_diff <- function(m1, sd1, n1, m2, sd2, n2, digits = 2) {
  a <- meta::metacont(n1, m1, sd1, n2, m2, sd2, method.ci = "t")
  a <- with(a, paste0(
    sprintf(paste0("%.", digits, "f"), round(TE, digits)), " (",
    sprintf(paste0("%.", digits, "f"), round(lower, digits)), ", ",
    sprintf(paste0("%.", digits, "f"), round(upper, digits)), ")"))
  (clipr::write_clip(a))
  a
}

mean_diff(0.94, 0.06, 23, 0.92, 0.07, 23)

odds_ratio_calc <- function(event1, n1, event2, n2, digits = 2) {
  a <- meta::metabin(event1, n1, event2, n2, sm = "OR")
  a <- with(a, paste0(
    sprintf(paste0("%.", digits, "f"), round(exp(TE), digits)), " (",
    sprintf(paste0("%.", digits, "f"), round(exp(lower), digits)), ", ",
    sprintf(paste0("%.", digits, "f"), round(exp(upper), digits)), ")"))
  (clipr::write_clip(a))
  a
}

odds_ratio_calc(10, 20, 15, 20, 2)

m_table <- function(data, x){
  group_by(data, arm_tx) %>%
    summarise(tx_count = n())
}

dichot_hunger <- dichot_cho.dat %>%
  filter(!is.na(hunger_np)) %>%
  arrange(age, desc(surg_nosurg), design, year, study) %>%
  select(refid:time_outcome, hunger_np, hunger_per, hunger_n)

dichot_hunger <- left_join(dichot_hunger, or_hunger, by = "refid") %>%
  group_by(study) %>%
  mutate(
    study = ifelse(row_number() == 1, study, NA),
    or_ci = ifelse(row_number() == 1, or_ci, NA),
    time_outcome = ifelse(row_number() == 1, time_outcome, NA)) %>%
  ungroup() %>%
  mutate(hunger_per = round(hunger_per, 0)) %>%
  select(refid, arm, design, age, study, arm_n, arm_tx, starts_with("hr"), time_outcome, hunger_np, hunger_per, or_ci)

cho_row <- dichot_hunger %>%
  mutate(line = row_number()) %>%
  filter(grepl("CHO", arm_tx)) %>%
  pull(line)

dichot_hunger$hunger_bar <- 0
dichot_hunger$hunger_bar <- ifelse(grepl("CHO", dichot_hunger$arm_tx),
                                   color_bar("tomato", fun = dichot_format)(dichot_hunger$hunger_per),
                                   color_bar("lightgray", fun = dichot_format)(dichot_hunger$hunger_per))

dichot_hunger  <- dichot_hunger %>%
  mutate(hunger_bar = str_replace(hunger_bar, ">[0-9][0-9]", ">&nbsp;"),
         study = ifelse(study == "Taniguchi 2011", "Taniguchi 2011^c^", study),
         study = ifelse(study == "Taniguchi 2011^c^", cell_spec(study, color = "black"), study),
         study = ifelse(study == "Koeppe 2013", "Koeppe 2013^b^", study),
         study = ifelse(study == "Panebianco 2020", "Panebianco 2020^d^", study),
         study = ifelse(study == "Tudor-Drobjewski 2018", "Tudor-Drobjewski 2018^e^", study))

add_foot <- function(.data, study_to_footnote, value){
  foot_letter <- paste0("^", value, "^")
  .data %>% mutate(study = ifelse(study == study_to_footnote, paste0(study_to_footnote, foot_letter), study))
}

add_foot <- function(.data, use_var, study_to_footnote, value){
  foot_letter <- paste0("^", value, "^")
  use_var <- enquo(use_var)
  .data %>%
    mutate(!!use_var := if_else(!!use_var == study_to_footnote, paste0(study_to_footnote, foot_letter), !!use_var) )
}

temp <- dichot_hunger %>%
  add_foot(study, "Tudor-Drobjewski 2018", "c")

summarise_data_categorical <- function(var1, t_var, dt){

  var1 <- enquo(var1)
  t_var <- enquo(t_var)
  v1 <- quo_name(var1)
  v2 <- quo_name(t_var)

  dt %>%
    select(one_of(v1, v2)) %>%
    group_by(!!t_var, !!var1) %>%
    summarise(count = n())

}



study_foot <- function(data, var_foot, study_select, letter){
  data %>% mutate( ((var_foot)) = ifelse( ((var_foot)) == study_select, paste_0( ((var_foot)), "^", letter, "^"), ((var_foot))) )
}


study_foot <- function(.data, study, study_select, letter){
  .data %>% mutate( var_stud = ifelse(var_stud == study_select, str_c(var_stud, "^", letter, "^"), var_stud))
}

study_foot <- function(.data, study_to_footnote, value){
  foot_letter <- paste0("^", value, "^")
  .data %>% mutate(study = ifelse(study == study_to_footnote, paste0(study_to_footnote, foot_letter), study))
}

temp <- dichot_hunger %>%
  study_foot("Tudor-Drobjewski 2018", "c")


ftemp <- dichot_hunger %>%
  study_foot("Taniguchi 2011", "c")


smd.bwgrpS <- function(m1, m2, n1, n2, pVal){
  sd2 <- abs((m2-m1)/qt(pVal/2, n1+n2-2))*sqrt(n2*n1/(n2+n1))
  sd1 <- sd2
  print(c(n1, m1, sd1, n2, m2, sd2))
}

# Yilmaz
smd.bwgrpS(21.85, 19.15, 20, 20, .77)
# Yildiz
smd.bwgrpS(18.1, 9.3, 30, 30, .05)

# cochrane function to combine arms
combine_contin <- function(n_1, n_2, x_1, x_2, sd_1, sd_2){
  n_comb <- n_1 + n_2
  x_comb <-  (n_1 * x_1 + n_2 * x_2)/(n_comb)
  sd_comb <- sqrt(((n_1 - 1) * sd_1^2 + (n_2 - 1) * sd_2^2 + (n_1 * n_2)/(n_comb) * (x_1 - x_2)^2) / (n_comb - 1))
  return(n_comb, x_comb, sd_comb)
}

# use metacont to obtain means and std
