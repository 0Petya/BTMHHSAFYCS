library(tidyverse)

barriers <- read_rds("./data/processed/barriers.rds")

simple_logisitic_model <- glm(have_barriers ~ first_year, family = "binomial", data = barriers)
summary(simple_logisitic_model)
exp(coef(simple_logisitic_model))

logistic_model <- glm(have_barriers ~ first_year + age + sex + gender + race + international + religion + present_ses + past_ses + work + extracurricular + transfer + enrollment + academic_time + housing + disability + have_insurance, family = "binomial", data = barriers)
summary(logistic_model)
exp(coef(logistic_model))

simple_poisson_model <- glm(barriers_count ~ first_year, family = "poisson", data = barriers)
summary(simple_poisson_model)
exp(coef(simple_poisson_model))

exp(cbind(RR = coef(simple_poisson_model), confint(simple_poisson_model))) %>%
  as.data.frame() %>%
  mutate(sig = ifelse(1 < `2.5 %` | 1 > `97.5 %`, "*", "")) %>%
  write.csv("./results/crude_poisson_model_results.csv")

poisson_model <- glm(barriers_count ~ first_year + age + sex + gender + race + international + religion + present_ses + past_ses + work + extracurricular + transfer + enrollment + academic_time + housing + disability + have_insurance, family = "poisson", data = barriers)
summary(poisson_model)
exp(coef(poisson_model))

exp(cbind(RR = coef(poisson_model), confint(poisson_model))) %>%
  as.data.frame() %>%
  mutate(sig = ifelse(1 < `2.5 %` | 1 > `97.5 %`, "*", "")) %>%
  write.csv("./results/poisson_model_results.csv")
