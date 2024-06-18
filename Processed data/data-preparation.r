## ---------------------------
## Script name: data_preparation.r
##
## Purpose of script:
##  Creates a long data.frame with ratings.
##
## Author: Carsten Sander
##
## Date Created: 2024-04-26
##
## Copyright (c) Carsten Sander, 2024
## Email: carsten.sander@uni-hamburg.de
## ---------------------------
## Notes:
##  Before running this script, make sure your working directory
##  is set to a folder containing the /Processed data/ folder from
##  the github repository
##
##  R Version -- 4.3.1
## ---------------------------

# Load packages
library(tidyverse) # tidyverse_2.0.0
library(magrittr, include.only = "%T>%") # magrittr_2.0.3

# Download raw data from Github
raw <- read.csv("Processed data/raw-data.csv") [-c(1, 2), ]

# ---------------------------

# Process qualtrics data
dat <- raw %>%
  pivot_longer(
    cols = matches("_like_|_stereo|_inf_|_typicality_"), # nolint
    names_to = c("condx", ".value"),
    names_pattern = "(X\\d+)_(.*)") %>%
  filter(COUNTERPROB_inf_1 != "") %>%
  # Separate condition variable into three
  separate(condition, remove = FALSE,
    into = c("target_category_label", "typicality", "issue"), sep = "_") %>%
  # Create category type and target category
  mutate(
    category_type = ifelse(
      target_category_label %in% c("REP", "DEM"),
      "PARTY", "IDEOLOGICAL"),
    target_category = ifelse(
      target_category_label %in% c("REP", "CON"),
      "RIGHT", "LEFT"
    )) %>%
  # Recode items
  mutate(.keep = "unused",
    age = as.numeric(str_extract(age, "\\d*")),
    gender = ifelse(gender == "specified", gender_3_TEXT, gender),
    partisan_identity = ifelse(political_identity == "Other",
      str_to_lower(political_identity_3_TEXT), political_identity),
    time_taken = ymd_hms(EndDate) - ymd_hms(StartDate)) %>%
  # Recode open responses for gender
  # mutate(
  #   gender = case_match(gender,
  #     c("Non Binary", "non-binary") ~ "non-binary",
  #     c("") ~ "n/a", c("male", "man") ~ "man",
  #     c("woman", "Female") ~ "woman")) %>%
  # Recode open responses for partisan identity
  # mutate(
  #   partisan_identity = case_match(partisan_identity,
  #     .default = partisan_identity,
  #     c("i'm registered republican but consider myself more independent",
  #       "independent", "independent lean democrat",
  #       "independent. lean liberal sometimes and lean conservative other times. ", # nolint
  #       "independant", "independent ",
  #       "independent leaning left", "") ~ "Independent",
  #     c("centrist", "neutral", "true moderate", "communist", "i don't vote",
  #       "leftist", "neither", "socialist", "socialist  ",
  #       "libertarian") ~ "Other",
  #     c("") ~ "n/a", )) %>%
  # Rename inference items
  rename(
    political_ideology = political_ideology_1,
    typicality_nontarget = typicality_nontarget_1,
    typicality_target = typicality_target_1) %>%
  rename_with(~ str_replace_all(.x,
    c("like_1" = "like_nontarget",
      "like_35" = "like_target"))) %>%
  rename_with(~ str_replace(.x, "_1$", "_t_target")) %>%
  rename_with(~ str_replace(.x, "_4$", "_t_other_1")) %>%
  rename_with(~ str_replace(.x, "_5$", "_t_other_2")) %>%
  rename_with(~ str_replace(.x, "_8$", "_f_abortion")) %>%
  rename_with(~ str_replace(.x, "_9$", "_f_immigration")) %>%
  rename_with(~ str_replace(.x, "_10$", "_f_income")) %>%
  mutate(across(matches("like_|inf_|stereo_|typicality_"),
    ~ as.numeric(.x))) %>%
  # Get time spent on data security, storage, and analysis info
  mutate(
    response_clicked = str_split(response_clicked, "; "),
    time_spent = str_split(time_spent, "; ") %>%
      map(~ as.duration(as.numeric(.x) / 1000))
  ) %>%
  mutate(.keep = "unused",
    time_on_data_security = as.duration(map2_vec(response_clicked, time_spent,
      ~sum(.y[.x == "Data Security, Storage, and Analysis"])))) %>%
  mutate(
    # Recode inference items from 0 = Target, 100 = Non-Target to
    # 0 = incorrect inference and 100 = correct inference
    across(matches("counterprob_inf_f_|stereoprob_inf_t_"),
      ~ (. - 50) * -1 + 50),
    # Recode stereotype items from 0 = disagree to 100 = agree to
    # 0 = stereotype-incongruent and 100 = stereotype-congruent response
    across(matches("stereo_t"), ~ (.x - 50) * -1 + 50)) %>%
  # Count the number of "wrong" inferences in the pre inference measure
  rowwise() %>%
  mutate(n_wrong_pre = sum(c_across(matches("PRE_inf_")) < 50)) %>%
  ungroup() %T>% {
  # Exclude participants
    assign(x = "last_n", value = nrow(.), envir = .GlobalEnv)
    cat("Exclusion according to the pre-registered exclusion criteria\n\n")
    cat("Participants before exclusion:", nrow(.), "\n")
  } %>%
  filter(informed_consent == "yes") %T>% {
    cat("a. participants who withdrew their consent to\n",
        "  data analysis after full debriefing:", last_n - nrow(.), "\n")
    assign(x = "last_n", value = nrow(.), envir = .GlobalEnv)
  } %>%
  filter(data_quality == "yes") %T>% {
    cat("b. participants who rate their own data to be\n",
        "  unfit for analyses:", last_n - nrow(.), "\n")
    assign(x = "last_n", value = nrow(.), envir = .GlobalEnv)
  } %>%
  filter(n_wrong_pre < 5) %T>% {
    cat("c. participants who inferred the wrong category\n",
        "  in five or more non-target inference items:", last_n - nrow(.), "\n")
    cat("Participants after exclusion:", nrow(.), "\n")
  } %>%
  # Pivot inference scores by diagnosticity component
  pivot_longer(
    cols = matches("_inf_"),
    names_to = c("diagnosticity_component", ".value"),
    names_pattern = "([A-Z]*)_(.*)"
  ) %>%
  # Pivot inference and stereotype scores by item
  pivot_longer(
    cols = matches("inf_|stereo_"),
    names_to = c(".value", "item"),
    names_pattern = "([a-z]*)_(.*)"
  ) %>%
  # Get item issue and item type
  mutate(
    item_issue = case_when(
      item == "t_target" ~ str_to_lower(issue),
      item == "t_other_1" & issue == "GUN" ~ "affirm",
      item == "t_other_1" & issue == "AFFIRM" ~ "environment",
      item == "t_other_1" & issue == "ENVIRONMENT" ~ "gun",
      item == "t_other_2" & issue == "GUN" ~ "environment",
      item == "t_other_2" & issue == "AFFIRM" ~ "gun",
      item == "t_other_2" & issue == "ENVIRONMENT" ~ "affirm",
      item == "f_abortion" ~ "abortion",
      item == "f_immigration" ~ "immigration",
      item == "f_income" ~ "income"),
    item_type = case_when(
      item == "t_target" ~ "target",
      item %in% c("t_other_1", "t_other_2") ~ "test",
      item %in% c("f_abortion", "f_immigration", "f_income") ~ "filler")) %>%
  # Select variables
  select(subject_id = ResponseId,
    condition, target_category_label, target_category, category_type,
    typicality, issue, diagnosticity_component, item_issue, item_type,
    inf, stereo, n_wrong_pre, time_on_data_security, time_taken, gender,
    age, partisan_identity, political_ideology,
    matches("_like_"), matches("typicality_"),
    stand_out, deception, further_comments = debriefing) %>%
  # Define factors
  mutate(across(c(subject_id, condition, target_category_label,
    target_category, category_type, typicality, issue,
    diagnosticity_component, item_issue, item_type, gender, partisan_identity),
    ~ as.factor(.x))) %>%
  mutate(typicality = factor(typicality, levels = c("DIS", "CON"))) %>%
  mutate(political_ideology = as.numeric(political_ideology))

# Export data
saveRDS(dat, file = "Processed data/processed-data.rds")

create_codebook <- function(dat) {
  ## ---------------------------
  ## This function takes a prepared dataframe and creates
  ## a codebook (csv) with the variable name, type, and
  ## factor levels/range
  ##
  ## @dat -- data.frame
  ##
  ## @codebook data.frame that can be exported as csv
  ## ---------------------------
  out <- data.frame(
    variable = names(dat),
    description = rep("", ncol(dat)),
    type = unname(unlist(lapply(dat, typeof))),
    unit_of_measurement = rep("", ncol(dat)),
    coding = rep("", ncol(dat)),
    derivation = rep("", ncol(dat))
  ) %>%
  mutate(type = ifelse(
    unname(unlist(lapply(dat, is.factor))),
    "factor", type
  )) %>%
  mutate(type = ifelse(
    unname(unlist(lapply(dat, is.numeric))),
    "numeric", type
  )) %>%
  mutate(type = ifelse(
    unname(unlist(lapply(dat, is.difftime))) |
      unname(unlist(lapply(dat, is.duration))),
    "duration", type
  )) %>%
  mutate(coding = ifelse(type == "factor",
    unname(unlist(lapply(dat,
      function(x) paste(levels(x), collapse = "\n")))), ""))
  return(out)
}
# write_csv2(create_codebook(dat), "Processed data/new-codebook.csv")