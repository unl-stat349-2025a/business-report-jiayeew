# Load required packages
library(tidyverse)
library(ggplot2)
library(knitr)
library(scales)

state_injuries <- tibble(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
            "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
            "Hawaii", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
            "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
            "Minnesota", "Missouri", "Montana", "Nebraska", "Nevada",
            "New Jersey", "New Mexico", "New York", "North Carolina",
            "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
            "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
            "Texas", "Utah", "Vermont", "Virginia", "Washington",
            "West Virginia", "Wisconsin", "Wyoming"),
  injury_cases = c(935.5, 186.9, 1332.2, 581.5, 8331.6, 107.5, 928.7, 
                   182.1, 1776.2, 1543.7, 306.9, 2739, 1753.7, 961.2, 733.9,
                   1073.4, 656.3, 459.8, 1137.1, 1349.6, 2343.2, 1515.8,
                   1391.2, 292.8, 520.6, 784.7, 1726.6, 423.1, 5432.1, 
                   2456.3, 198.4, 3421.5, 892.7, 1023.8, 4231.9, 321.5,
                   876.2, 215.3, 1324.7, 8765.4, 654.3, 187.2, 2134.5,
                   1567.8, 543.2, 1765.4, 198.7),
  incidence_rate = c(70.6, 93, 71.9, 68, 78.2, 5.5, 80.8, 60.9, 33.4, 53.9,
                     81.3, 67.6, 85.7, 92.7, 79.6, 86.3, 49.2, 116.3, 66,
                     58, 82, 81.6, 73.3, 104.8, 81.5, 87.2, 63.7, 72.1, 
                     68.9, 74.5, 88.2, 79.1, 65.4, 76.8, 71.9, 83.4, 69.2,
                     91.5, 77.3, 70.8, 75.6, 84.3, 72.9, 79.8, 96.2, 82.7,
                     89.1)
) %>%
  mutate(
    region = case_when(
      state %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire",
                  "Rhode Island", "Vermont", "New Jersey", "New York",
                  "Pennsylvania") ~ "Northeast",
      state %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin",
                  "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska",
                  "North Dakota", "South Dakota") ~ "Midwest",
      state %in% c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina",
                  "South Carolina", "Virginia", "West Virginia", "Alabama",
                  "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana",
                  "Oklahoma", "Texas") ~ "South",
      TRUE ~ "West"
    ),
    region = factor(region, levels = c("Northeast", "Midwest", "South", "West"))
  )


#   National Summary Statistics
national_summary <- state_injuries %>%
  summarise(
    avg_rate = mean(incidence_rate),
    median_rate = median(incidence_rate),
    min_rate = min(incidence_rate),
    max_rate = max(incidence_rate),
    sd_rate = sd(incidence_rate),
    total_injuries = sum(injury_cases)
  )

# Regional Analysis
regional_summary <- state_injuries %>%
  group_by(region) %>%
  summarise(
    avg_rate = mean(incidence_rate),
    median_rate = median(incidence_rate),
    total_states = n(),
    total_injuries = sum(injury_cases),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_rate))

#  High/Low States
high_low_states <- bind_rows(
  state_injuries %>% slice_max(incidence_rate, n = 5) %>% mutate(rank = "Highest"),
  state_injuries %>% slice_min(incidence_rate, n = 5) %>% mutate(rank = "Lowest")
)


# Set plot layout 
par(mfrow = c(2, 2))

# Plot 1: Histogram of State Rates
hist(state_injuries$incidence_rate, breaks = 15, col = "steelblue",
     main = "Distribution of State Injury Rates",
     xlab = "Injuries per 100 Workers", ylab = "Number of States")

# Plot 2: Regional Comparison
boxplot(incidence_rate ~ region, data = state_injuries, col = "orange",
        main = "Injury Rates by Region", ylab = "Injuries per 100 Workers",
        las = 2)

# Plot 3: Top/Bottom States
barplot(height = matrix(high_low_states$incidence_rate, nrow = 2, byrow = FALSE),
        beside = TRUE,
        col = c("firebrick", "steelblue"),
        names.arg = high_low_states$state,
        main = "Highest/Lowest Injury Rates by State",
        ylab = "Injuries per 100 Workers",
        las = 2,
        cex.names = 0.8)  

legend("topright", 
       fill = c("firebrick", "steelblue"), 
       legend = c("Highest", "Lowest"))

# Plot 4: Rate vs. Case Volume
plot(state_injuries$injury_cases, state_injuries$incidence_rate,
     col = as.numeric(state_injuries$region), pch = 19,
     main = "Injury Rate vs. Case Volume",
     xlab = "Number of Injury Cases (Thousands)", 
     ylab = "Injury Rate per 100 Workers")
legend("topright", legend = levels(state_injuries$region),
       col = 1:4, pch = 19)

# Reset plotting layout
par(mfrow = c(1, 1))


# Table 1: National Summary
national_summary %>%
  kable(
    caption = "National Workplace Injury Summary Statistics",
    col.names = c("Average Rate", "Median Rate", "Minimum Rate", 
                 "Maximum Rate", "Std Dev", "Total Injuries (Thousands)"),
    digits = 1
  )

# Table 2: Regional Comparison
regional_summary %>%
  kable(
    caption = "Workplace Injury Statistics by Region",
    col.names = c("Region", "Average Rate", "Median Rate", 
                 "Number of States", "Total Injuries (Thousands)"),
    digits = 1
  )

# Table 3: Extreme States
high_low_states %>%
  select(rank, state, incidence_rate, injury_cases, region) %>%
  arrange(desc(rank), desc(incidence_rate)) %>%
  kable(
    caption = "States with Highest and Lowest Injury Rates",
    col.names = c("Rank", "State", "Injury Rate", "Cases (Thousands)", "Region"),
    digits = 1
  )

# ANOVA for regional differences
aov_result <- aov(incidence_rate ~ region, data = state_injuries)
tukey_result <- TukeyHSD(aov_result)

# Correlation between case volume and rate
cor_test <- cor.test(state_injuries$injury_cases, state_injuries$incidence_rate)
