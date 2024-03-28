library(arsenal)
library(tidyverse)

barriers <- read_rds("./data/processed/barriers.rds")

descriptive_table <- tableby(first_year ~ age + sex + gender + race + international + religion + present_ses + past_ses + work + extracurricular + transfer + year + enrollment + academic_time + housing + disability + have_insurance, data = barriers)
capture.output(summary(descriptive_table), file = "./results/descriptive_statistics.md")

measures_table <- tableby(first_year ~ phq_total + gad_total + k6_total + `d-d_total` + `d-d_personal_total`, data = barriers)
capture.output(summary(measures_table), file = "./results/measures_table.md")

utilization_table <- tableby(first_year ~ past_therapy_use + current_therapy_use + barriers_count, data = barriers)
capture.output(summary(utilization_table), file = "./results/utilization_table.md")
