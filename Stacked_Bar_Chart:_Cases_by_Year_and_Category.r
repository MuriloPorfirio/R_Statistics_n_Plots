library(readxl)
library(ggplot2)
library(reshape2)

# Reading the file
Input <- read_xlsx(file.choose())

# Calculate the "Others" category by subtracting other categories from the total
Input$Others <- Input$Total - (Input$Cure + Input$TBDeath + Input$Death)

# Transforming data to long format
Input_long <- melt(Input, id.vars = "Year", 
                   measure.vars = c("Cure", "TBDeath", "Death", "Others"),
                   variable.name = "Category", value.name = "Quantity")

# Creating the stacked bar chart with customized colors
ggplot(Input_long, aes(x = as.factor(Year), y = Quantity, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Cure" = "blue", "TBDeath" = "red", "Death" = "darkgrey", "Others" = "lightgrey")) +
  labs(x = "Year", y = "Quantity", fill = "Category") +
  ggtitle("Stacked Bar Chart: Cases by Year and Category") +
  theme_minimal()
