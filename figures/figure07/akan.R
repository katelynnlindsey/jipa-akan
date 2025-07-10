# Load packages
install.packages("ggplot2")
install.packages("readr")
install.packages("dplyr")

library(ggplot2)
library(readr)
library(dplyr)
install.packages("readODS")
library(readODS)

#I put a bunch of stuff in this file: scatterplot, boxplot with outliers, and gender plot

file_path <- "GitHub/jipa-akan/figures/figure07/Akan-final-2values.ods"
data <- read_ods(file_path)

# Checking if there's 0 values
print(subset(data, CPP == 0 | H1H2c == 0))

# Makes sure all 0s are numerical 
data[] <- lapply(data, function(x) if (is.character(x)) trimws(x) else x)

# Convert to numeric if needed
data$CPP <- as.numeric(data$CPP)
data$H1H2c <- as.numeric(data$H1H2c)

# Post-cleaning check
print(subset(data, CPP == 0 | H1H2c == 0))

#didn't end up needing this filter but it would get rid 0 values
# Filter out rows where CPP or H1H2c are 0
filtered_data <- data %>%
  filter(CPP != 0, H1H2c != 0)

# Create scatterplot plot using filtered data for all 3 variables 
plot <- ggplot(filtered_data, aes(x = CPP, y = H1H2c, color = ATR)) +
  geom_point() +
  labs(title = "CPP vs H1H2c by ATR",
       x = "CPP",
       y = "H1H2c") +
  scale_color_manual(values = c("+ATR" = "blue", "-ATR" = "red")) +  # Optional: Customize colors
  theme_minimal()

print(plot)

#This section is for the boxplot outliers 
# Calculate IQR and identify outliers
data_with_outliers <- filtered_data %>%
  group_by(ATR) %>%
  mutate(
    Q1 = quantile(H1H2c, 0.25),
    Q3 = quantile(H1H2c, 0.75),
    IQR = Q3 - Q1,
    LowerBound = Q1 - 1.5 * IQR,
    UpperBound = Q3 + 1.5 * IQR,
    Outlier = H1H2c < LowerBound | H1H2c > UpperBound
  ) %>%
  ungroup()

# Filter out only the outliers
outliers <- data_with_outliers %>%
  filter(Outlier)

# Boxplot of H1H2c values by ATR with outliers labeled with gender
plot2 <- ggplot(data_with_outliers, aes(x = ATR, y = H1H2c, fill = ATR)) +
  geom_boxplot() +
  geom_text(data = outliers, aes(label = Gender), vjust = -0.5, hjust = 1.5) +
  labs(title = "Boxplot of H1H2c values by ATR with Outliers Labeled by Gender",
       x = "ATR",
       y = "H1H2c") +
  scale_fill_manual(values = c("+ATR" = "blue", "-ATR" = "red")) +
  theme_minimal()

print(plot2)

# Convert ATR to numeric values for analysis
data$ATR_numeric <- ifelse(data$ATR == "+ATR", 1, 0)

# Convert Gender column to factors with full labels
data$Gender <- factor(data$Gender, levels = c("M", "F"), labels = c("Male", "Female"))

# Boxplot for Gender
plot3 <- ggplot(filtered_data, aes(x = ATR_numeric, y = H1H2c, fill = Gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) +
  labs(title = "Boxplot of H1H2c Values by ATR and Gender",
       x = "ATR",
       y = "H1H2c") +
  scale_x_continuous(breaks = c(0, 1), labels = c("-ATR", "+ATR")) +
  theme_minimal()

print(plot3)