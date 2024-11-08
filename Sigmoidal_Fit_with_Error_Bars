# ALERT: This script requires importing a data table.

# Required Packages
library(readxl)
library(ggplot2)
library(minpack.lm)

# Import the file
path <- "C:/Users/muril/Desktop/R/colonias.xlsx"
data <- read_excel(path, sheet = 1)

# Rename columns
colnames(data) <- c("Dose", "Colonies")

# Transform "Colonies" data to a logarithmic scale (natural log)
data$log_Colonies <- log(data$Colonies)

# Fit a sigmoidal model with "log_Colonies" as the dependent variable
sigmoidal_model <- nlsLM(log_Colonies ~ A / (1 + exp(-k * (Dose - x0))),
                          start = list(A = max(data$log_Colonies), k = 1, x0 = median(data$Dose)),
                          data = data)

# Correct spacing
data$Dose <- factor(data$Dose, levels = c(0, 1, 1.5, 2, 2.67, 5.2, 10))

# Generate the plot
ggplot(data, aes(x = Dose, y = log_Colonies, group = 1)) +
  # Median as a point
  stat_summary(fun = median, geom = "point", size = 6, color = "pink", shape = 16) +
  
  # Error bar for interquartile range
  stat_summary(fun.data = function(y) {
    return(data.frame(y = median(y), ymin = quantile(y, 0.25), ymax = quantile(y, 0.75)))
  }, geom = "errorbar", width = 0.4, color = "blue", size = 0.5) +
  
  # Error bar for minimum and maximum values
  stat_summary(fun.data = function(y) {
    return(data.frame(y = median(y), ymin = min(y), ymax = max(y)))
  }, geom = "errorbar", width = 0.15, color = "blue", size = 0.8) +
  
  # Add the sigmoidal line to the plot based on the log values
  stat_function(fun = function(x) {
    coef(sigmoidal_model)["A"] /
    (1 + exp(-coef(sigmoidal_model)["k"] * (as.numeric(as.character(x))
    - coef(sigmoidal_model)["x0"])))
  }, color = "purple", size = 0.5) +  
  
  # Titles and labels
  labs(
    x = "Radiation Dose (Gy)",
    y = "Log(Number of Colonies)",
    title = "MDA-MB-231"
  ) +
  
  # Custom theme for better visibility
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_blank()
  )
