# Load required packages
library(tidyverse)
library(ggplot2)
library(knitr)
library(scales)

costly_injuries <- tibble(
  cause = c("Overexertion involving outside sources", "Falls on same level", 
            "Falls to lower level", "Struck by object or equipment",
            "Other exertions or bodily reactions", "Roadway incidents",
            "Slip or trip without fall", "Caught in equipment",
            "Struck against object", "Repetitive motions"),
  percentage = c(21.51, 17.21, 9.78, 9.56, 6.34, 4.76, 4.02, 3.54, 3.17, 2.65)
) %>%
  mutate(
    office_related = case_when(
      cause %in% c("Falls on same level", "Slip or trip without fall", 
                   "Repetitive motions") ~ "Office",
      cause %in% c("Falls to lower level", "Struck by object or equipment",
                   "Roadway incidents", "Caught in equipment",
                   "Struck against object") ~ "Non-Office",
      TRUE ~ "Other"  
    )
  )

# filter out "Other" category for comparisons
classified_injuries <- costly_injuries %>%
  filter(office_related != "Other")

# costly Injuries Visualization
ggplot(classified_injuries, 
       aes(x = reorder(cause, percentage), y = percentage, 
           fill = office_related)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) + # Office blue, Non-Office orange
  labs(
    title = "Most Costly Workplace Injuries by Cause (2024)",
    subtitle = "Only clearly classifiable injuries shown (Office vs Non-Office)",
    x = "",
    y = "Percentage of Total Costs",
    fill = "Work Environment"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Fall-Specific Analysis
fall_data <- classified_injuries %>%
  filter(str_detect(cause, "Falls|Slip")) %>%
  group_by(office_related) %>%
  summarize(
    total_percentage = sum(percentage),
    n_types = n()
  ) %>%
  mutate(
    prop_of_falls = total_percentage / sum(total_percentage)
  )

#  Office vs Non-Office Comparison 
comparison_stats <- classified_injuries %>%
  group_by(office_related) %>%
  summarize(
    total_percentage = sum(percentage),
    avg_percentage = mean(percentage),
    n_causes = n()
  ) %>%
  mutate(
    prop_of_total = total_percentage / sum(total_percentage)
  )


# Classified Injuries Only
classified_injuries %>%
  arrange(desc(percentage)) %>%
  select(cause, percentage, office_related) %>%
  kable(
    caption = "Costly Workplace Injuries (Classified as Office or Non-Office Only)",
    col.names = c("Injury Cause", "% of Costs", "Environment"),
    digits = 2
  )

#  Fall Injury Comparison
fall_data %>%
  select(-n_types) %>%
  kable(
    caption = "Fall-Related Injury Costs by Environment",
    col.names = c("Environment", "Total % of Costs", "Proportion of Falls"),
    digits = 2
  )

# Table 3: Overall Comparison
comparison_stats %>%
  select(-n_causes) %>%
  kable(
    caption = "Injury Cost Distribution by Work Environment",
    col.names = c("Environment", "Total %", "Average % per Cause", "Proportion of Total"),
    digits = 2
  )


