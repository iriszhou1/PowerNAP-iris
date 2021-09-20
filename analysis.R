library(tidyverse)
library(dplyr)
library(knitr)
library(patchwork)

report_card_grad_df <- read.csv("report-card-grad-data.csv", stringsAsFactors = FALSE)

student_group_grad_rate_df <- report_card_grad_df %>% 
  filter(SchoolName == "State Total") %>% 
  select(StudentGroup, Cohort, Graduate, FinalCohort, GraduationRate) %>% 
  group_by(StudentGroup) %>% 
  summarize(grad_rate = sum(Graduate)/sum(FinalCohort))

all_students_grad_rate <- student_group_grad_rate_df %>% 
  filter(StudentGroup == "All Students") %>% 
  select(grad_rate) %>% 
  pull() %>% 
  as.double()

################################################################################
##### Homeless/non-homeless and low-income/non-low income graduation rates #####

student_groups_vec <- c("Homeless", "Low-Income",
                        "Non-Homeless", "Non-Low Income")

grad_rate_df <- student_group_grad_rate_df %>% 
  filter(StudentGroup %in% student_groups_vec)

grad_rate_table <- grad_rate_df %>% 
  rename("Student Group" = "StudentGroup",
         "Graduation Rate" = "grad_rate") %>% 
  kable()

grad_rate_plot <- grad_rate_df %>% 
  ggplot() + 
  geom_col(mapping = aes(x = StudentGroup, y = grad_rate, fill = StudentGroup)) +
  geom_text(aes(label = scales::percent(grad_rate), x = StudentGroup, y = grad_rate),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5, color = 'gray30') +
  geom_line(mapping = aes(x = StudentGroup, y = all_students_grad_rate, 
                          group = 1), color = "black", size = 1, linetype = 2) +
  geom_text(aes(label = paste(sprintf("%.2f", all_students_grad_rate*100), "% (All Students)"), 
                x = 1.7, y = all_students_grad_rate),
                position = position_dodge(width = 0.5), vjust = -0.5, size = 4) +
  ylim(0, 1) +
  labs(
    title = "Graduation Rates by Student Groups",
    x = "Student Group",
    y = "Graduation Rate"
  ) +
  scale_fill_manual(
    name = "Student Group",
    values = c("indianred3", "dodgerblue3", "lightcoral", "skyblue")
  ) +
  theme_light() +
  theme(plot.title = element_text(face = "bold"))

################################################################################
##### Student Group Results #####

student_group_outcomes_df <- report_card_grad_df %>% 
  filter(SchoolName == "State Total", StudentGroupType %in% c("Homeless", "LowIncome")) %>% 
  select(StudentGroup, FinalCohort, Graduate, Continuing, Dropout) %>% 
  group_by(StudentGroup) %>% 
  summarize(Graduate = sum(Graduate),
            Continuing = sum(Continuing),
            Dropout = sum(Dropout)) %>% 
  pivot_longer(cols = !StudentGroup, names_to = "Outcome", values_to = "Count") %>% 
  group_by(StudentGroup) %>% 
  mutate(pct = Count/sum(Count))

student_group_outcomes_plot <- student_group_outcomes_df %>% 
  ggplot(aes(x = StudentGroup, y = Count, group = Outcome, fill = Outcome)) +
  geom_col(position = "fill") +
  geom_text(aes(label = paste0(sprintf("%.1f", pct*100), "%")), 
            position = position_fill(vjust = 0.5), size = 3.5, color = 'white') +
  geom_text(aes(label = paste0(sprintf("%.1f", pct*100), "%")), 
            position = position_fill(vjust = 0.5), size = 3.57, color = 'white') +
  geom_text(aes(label = paste0(sprintf("%.1f", pct*100), "%")), 
            position = position_fill(vjust = 0.5), size = 3.6, color = 'white') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("gold2", "indianred2", "turquoise3")) +
  labs(
    title = "Outcomes of Student Groups",
    x = "Student Group",
    y = "Percentage of Group"
  ) +
  theme_light() +
  theme(plot.title = element_text(face = "bold"))

################################################################################
##### Dropout rate over 7 years ######

student_group_dropouts_df <- report_card_grad_df %>% 
  filter(SchoolName == "State Total", StudentGroupType %in% c("Homeless", "LowIncome")) %>% 
  select(StudentGroup, 18:24, Dropout, FinalCohort) %>% 
  replace(is.na(.), 0) %>% 
  rename("Year 1" = "Year1Dropout",
         "Year 2" = "Year2Dropout",
         "Year 3" = "Year3Dropout",
         "Year 4" = "Year4Dropout",
         "Year 5" = "Year5Dropout",
         "Year 6" = "Year6Dropout",
         "Year 7" = "Year7Dropout") 

dropout_rate_over_cohort_total_df <- student_group_dropouts_df %>% 
  group_by(StudentGroup) %>% 
  summarize(across(c(starts_with("Year")), sum)/sum(FinalCohort))

cohort_dropout_rate_plot <- dropout_rate_over_cohort_total_df %>% 
  pivot_longer(cols = !StudentGroup, names_to = "Year", values_to = "DropoutRate") %>% 
  ggplot(mapping = aes(x = Year, y = DropoutRate, color = StudentGroup, group = StudentGroup)) +
  geom_point(size = 1.5) +
  geom_line(size = .8) + 
  scale_color_manual(
    name = "Student Group",
    values = c("indianred3", "dodgerblue3", "lightcoral", "skyblue")
  ) +
  labs(
    title = "Dropout rates for student groups over seven years",
    x = "Dropout Year (after 9th grade)",
    y = "Proportion of dropouts to total students"
  ) +
  theme_light() +
  theme(plot.title = element_text(face = "bold"))


