# ALERT: This script does NOT require importing a data table.
# All data is entered manually within the script.

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Parameters
num_mice <- 8  # Total number of mice (4 in each group)
num_timepoints <- 5  # Number of time points for each mouse

# Define time points (e.g., days)
time_points <- 1:num_timepoints

# Enter tumor volume data for each mouse at each time point
# Replace the example data below with actual measurements

# Tumor volumes for the Treatment group (4 mice)
treatment_volumes <- data.frame(
  Mouse = factor(rep(1:4, each = num_timepoints)),
  Group = "Treatment",
  Time = rep(time_points, times = 4),
  TumorVolume = c(
    # Mouse 1 data
    95, 80, 65, 50, 40,
    # Mouse 2 data
    100, 90, 70, 55, 45,
    # Mouse 3 data
    110, 85, 60, 50, 35,
    # Mouse 4 data
    105, 85, 75, 60, 50
  )
)

# Tumor volumes for the Control group (4 mice)
control_volumes <- data.frame(
  Mouse = factor(rep(5:8, each = num_timepoints)),
  Group = "Control",
  Time = rep(time_points, times = 4),
  TumorVolume = c(
    # Mouse 5 data
    100, 110, 120, 125, 130,
    # Mouse 6 data
    95, 105, 115, 120, 125,
    # Mouse 7 data
    90, 100, 110, 115, 120,
    # Mouse 8 data
    85, 95, 105, 110, 115
  )
)

# Combine treatment and control data into a single dataframe
mice_data <- rbind(treatment_volumes, control_volumes)

# Calculate mean tumor volume for each group at each time point
mean_data <- mice_data %>%
  group_by(Group, Time) %>%
  summarise(MeanTumorVolume = mean(TumorVolume), .groups = 'drop')

# Plot the data with average lines for each group
ggplot(mean_data, aes(x = Time, y = MeanTumorVolume, color = Group, group = Group)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(
    title = "Average Tumor Volume Over Time by Group",
    x = "Time (Days)",
    y = "Average Tumor Volume (mm³)",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12)
  ) +
  scale_color_manual(values = c("Treatment" = "blue", "Control" = "red"))
