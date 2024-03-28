library(tidyverse)

yes_no_label = c("Yes", "No")

barriers <- read_csv("./data/raw/barriers.2022-04-24.csv")[-c(1, 2),18:100] %>%
  mutate(consent_agree = factor(consent_agree, labels = yes_no_label)) %>%
  mutate(student = factor(student, levels = c("1", "2"), labels = yes_no_label)) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(sex = factor(sex, labels = c("Female", "Male", "Prefer not to say"))) %>%
  mutate(gender = factor(gender, labels = c("Female", "Male", "Trans male", "Trans female", "Non-binary", "Prefer not to say"))) %>%
  mutate(race = factor(race, labels = c("American Indian or Alaskan Native", "Asian", "Black/African American", "Hispanic/Latin(x)", "Native Hawaiian or Pacific Islander", "Middle Eastern or Arab", "White"))) %>%
  mutate(international = factor(international, labels = yes_no_label)) %>%
  mutate(religion = factor(religion, labels = c("Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important"))) %>%
  mutate(present_ses = factor(present_ses, labels = c("Never stressful", "Rarely stressful", "Sometimes stressful", "Often stressful", "Always stressful"))) %>%
  mutate(past_ses = factor(past_ses, labels = c("Never stressful", "Rarely stressful", "Sometimes stressful", "Often stressful", "Always stressful"))) %>%
  mutate(work = as.numeric(work)) %>%
  mutate(extracurricular = factor(extracurricular, labels = yes_no_label)) %>%
  mutate(transfer = factor(transfer, labels = c("No", "Yes, I transferred from a community or junior college", "Yes, I transferred from a 4-year college or university"))) %>%
  mutate(year = factor(year, levels = c("1", "2", "3", "4", "5"))) %>%
  mutate(enrollment = factor(enrollment, labels = c("Full-time student", "Part-time student"))) %>%
  mutate(academic_time = as.numeric(academic_time)) %>%
  mutate(housing = factor(housing, labels = c("On-campus housing", "Fraternity or sorority housing", "Off-campus university housing", "Off-campus non-university housing"))) %>%
  mutate(disability = factor(disability, labels = c("No", "Yes"))) %>%
  mutate(academic_performance = factor(academic_performance, labels = c("None", "1-2 days", "3-5 days", "6 or more days"))) %>%
  mutate(across(matches("phq_\\d+"), ~ factor(., labels = c("Not at all", "Several days", "More than half the days", "Nearly every day")))) %>%
  mutate(phq_problem = factor(phq_problem, labels = c("Not difficult at all", "Somewhat difficult", "Very difficult", "Extremely difficult"))) %>%
  mutate(across(matches("gad_\\d+"), ~ factor(., labels = c("Not at all", "Several days", "Over half the days", "Nearly every day")))) %>%
  mutate(gad_problem = factor(gad_problem, labels = c("Not difficult at all", "Somewhat difficult", "Very difficult", "Extremely difficult"))) %>%
  mutate(across(matches("k6_\\d+"), ~ factor(., labels = c("None of the time", "A little of the time", "Some of the time", "Most of the time", "All of the time")))) %>%
  mutate(across(matches("d-d_\\d+"), ~ factor(., labels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Somewhat agree", "Agree", "Strongly agree")))) %>%
  mutate(across(matches("d-d_personal_\\d+"), ~ factor(., levels = c("0", "1", "2", "3", "4", "5"), labels = c("Strongly disagree", "Disagree", "Somewhat disagree", "Somewhat agree", "Agree", "Strongly agree")))) %>%
  mutate(knowledge_of_service = factor(knowledge_of_service, labels = c("Strongly agree", "Agree", "Somewhat agree", "Somewhat disagree", "Disagree", "Strongly disagree"))) %>%
  mutate(perceived_need = factor(perceived_need, labels = c("Strongly agree", "Agree", "Somewhat agree", "Somewhat disagree", "Disagree", "Strongly disagree"))) %>%
  mutate(past_therapy_use = factor(past_therapy_use, labels = c("No", "Yes, prior to starting college", "Yes, since starting college"))) %>%
  mutate(current_therapy_use = factor(current_therapy_use, labels = yes_no_label)) %>%
  mutate(raffle = factor(raffle, labels = yes_no_label)) %>%
  filter(consent_agree == "Yes") %>%
  filter(student == "Yes") %>%
  filter(age >= 18 & age < 90) %>%
  mutate(first_year = year == 1) %>%
  mutate(barriers_count = str_split(barriers, ",") %>% sapply(function(x) x %>% discard(~ . %in% c("1", "13")) %>% length)) %>%
  mutate(have_barriers = ifelse(barriers_count > 0, 1, 0)) %>%
  mutate(have_barriers = factor(have_barriers, labels = c("No", "Yes"))) %>%
  mutate(insurance_count = str_split(insurance, ",") %>% sapply(function(x) x %>% discard(~ . %in% c("1", "9")) %>% length)) %>%
  mutate(have_insurance = ifelse(insurance_count > 0, 1, 0)) %>%
  mutate(have_insurance = factor(have_insurance, labels = c("No", "Yes"))) %>%
  mutate(phq_total = rowSums(sapply(select(., matches("phq_\\d+")), as.numeric))) %>%
  mutate(gad_total = rowSums(sapply(select(., matches("gad_\\d+")), as.numeric))) %>%
  mutate(k6_total = rowSums(sapply(select(., matches("k6_\\d+")), as.numeric))) %>%
  mutate(`d-d_total` = rowSums(sapply(select(., matches("d-d_\\d+")), as.numeric))) %>%
  mutate(`d-d_personal_total` = rowSums(sapply(select(., matches("d-d_personal_\\d+")), as.numeric)))

write_rds(barriers, "./data/processed/barriers.rds")
