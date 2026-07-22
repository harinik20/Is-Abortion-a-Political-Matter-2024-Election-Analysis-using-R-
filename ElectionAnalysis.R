library(readr)
library(tidyverse)
df <- read_csv("C:/Users/vdeiv/OneDrive/Desktop/data/2024_US_County_Level_Presidential_Results.csv")
View(df)

# Segregate States by Abortion Legality (laws in place from the last election, so 2024)
illegal_states <- c('Alabama', 'Arkansas', 'Idaho', 'Indiana', 'Kentucky', 'Louisiana', 
                    'Mississippi', 'Missouri', 'North Dakota', 'Oklahoma', 'South Dakota', 
                    'Tennessee', 'Texas', 'West Virginia')

legal_states <- c('California', 'Colorado', 'Connecticut', 'Delaware', 'Hawaii', 'Illinois', 
                  'Maine', 'Maryland', 'Massachusetts', 'Minnesota', 'Nevada', 'New Jersey', 
                  'New Mexico', 'New York', 'Oregon', 'Vermont', 'Washington')

# Label Counties in Dataset & Filter
analysis_df <- df %>%
  mutate(abortion_status = case_when(
    state_name %in% legal_states ~ "Legal",
    state_name %in% illegal_states ~ "Illegal",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(abortion_status))

# First Hypothesis Test: Democratic Vote Share(using per_dem column in dataset) 
cat("\n========================================\n")
cat("  TEST 1: DEMOCRATIC VOTE SHARE (per_dem)\n")
cat("========================================\n")

t_test_dem <- t.test(per_dem ~ abortion_status, data = analysis_df)
print(t_test_dem)

# Second Hypothesis TesT 2: Republication Vote Share (using per_gop column in dataset)
cat("\n========================================\n")
cat("  TEST 2: REPUBLICAN VOTE SHARE (per_gop)\n")
cat("========================================\n")

t_test_gop <- t.test(per_gop ~ abortion_status, data = analysis_df)
print(t_test_gop)

# Summary Table Of Means ---
cat("\n========================================\n")
cat("             GROUP MEANS SUMMARY         \n")
cat("========================================\n")

summary_table <- analysis_df %>%
  group_by(abortion_status) %>%
  summarize(
    Counties = n(),
    `Mean Dem Vote %` = round(mean(per_dem, na.rm = TRUE) * 100, 2),
    `Mean GOP Vote %` = round(mean(per_gop, na.rm = TRUE) * 100, 2)
  )

print(summary_table)
# Calculate Group Means for Plotting
mean_data <- analysis_df %>%
  group_by(abortion_status) %>%
  summarize(
    mean_dem = mean(per_dem, na.rm = TRUE) * 100,
    mean_gop = mean(per_gop, na.rm = TRUE) * 100
  )


# GRAPH 1: DEMOCRATIC VOTE SHARE BY ABORTION LEGALITY
plot_dem <- ggplot(mean_data, aes(x = abortion_status, y = mean_dem, fill = abortion_status)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", mean_dem)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Illegal" = "#708090", "Legal" = "#2E5B88")) +
  scale_y_continuous(limits = c(0, 60), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Mean Democratic Vote Share by State Abortion Status (2024)",
    subtitle = "Welch Two Sample t-test: p < 0.05 (Reject H0)",
    x = "Abortion Status",
    y = "Average Democratic Vote %"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank()
  )

print(plot_dem)

# GRAPH 2: REPUBLICAN VOTE SHARE BY ABORTION LEGALITY
plot_gop <- ggplot(mean_data, aes(x = abortion_status, y = mean_gop, fill = abortion_status)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", mean_gop)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Illegal" = "#D9534F", "Legal" = "#708090")) +
  scale_y_continuous(limits = c(0, 90), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Mean Republican Vote Share by State Abortion Status (2024)",
    subtitle = "Welch Two Sample t-test: p < 0.05 (Reject H0)",
    x = "Abortion Status",
    y = "Average Republican Vote %"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank()
  )

print(plot_gop)

