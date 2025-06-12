# Histogram of H1H2c values by ATR
ggplot(filtered_data, aes(x = H1H2c, fill = ATR)) +
  geom_histogram(binwidth = 0.1, position = "dodge") +
  labs(title = "Distribution of H1H2c Values by ATR",
       x = "H1H2c",
       y = "Count") +
  scale_fill_manual(values = c("+ATR" = "blue", "-ATR" = "red")) +
  theme_minimal()


file_path <- "/Users/chloeguttmann/Downloads/Akan-final-2values.ods"
data <- read_ods(file_path)



# Boxplot of CPP values by ATR
ggplot(filtered_data, aes(x = ATR, y = CPP, fill = ATR)) +
  geom_boxplot() +
  labs(title = "Boxplot of CPP values by ATR",
       x = "ATR",
       y = "CPP") +
  scale_fill_manual(values = c("+ATR" = "blue", "-ATR" = "red")) +
  theme_minimal()

ggplot(filtered_data, aes(x = H1H2c, y = ATR, color = ATR)) +
  geom_point() +
  labs(title = "H1H2c vs ATR",
       x = "H1H2c",
       y = "ATR") +
  scale_color_manual(values = c("+ATR" = "blue", "-ATR" = "red")) +
  theme_minimal()

# Scatter plot to visualize the relationship
ggplot(filtered_data, aes(x = CPP, y = ATR, color = ATR)) +
  geom_point() +
  labs(title = "CPP vs ATR",
       x = "CPP",
       y = "ATR") +
  scale_color_manual(values = c("+ATR" = "blue", "-ATR" = "red")) +
  theme_minimal()



# Correlation between CPP and ATR
cor.test(filtered_data$CPP, as.numeric(filtered_data$ATR))

# Correlation between H1H2c and ATR
cor.test(filtered_data$H1H2c, as.numeric(filtered_data$ATR))


# Load necessary libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readODS")

library(ggplot2)
library(dplyr)
library(readODS)
install.packages("GGally")
library(GGally)

# File path
file_path <- "/Users/chloeguttmann/Downloads/Akan-Final-2values.ods"

# Read the ODS file
data <- read_ods(file_path)

# Check column names and data types
print(colnames(data))
str(data)

# Check unique values in A
unique(data$ATR)

# Convert ATR to numeric values: +ATR = 1, -ATR = 0
data$ATR_numeric <- ifelse(data$ATR == "+ATR", 1, 0)

# Check the conversion
summary(data$ATR_numeric)

# Remove rows with NA values in CPP or ATR_numeric
filtered_data_clean <- data %>%
  filter(!is.na(CPP) & !is.na(ATR_numeric))

# Check the cleaned data
summary(filtered_data_clean$CPP)
summary(filtered_data_clean$ATR_numeric)

# Perform correlation test between CPP and Numeric ATR
cor_test_result <- cor.test(filtered_data_clean$CPP, filtered_data_clean$ATR_numeric)

# Print correlation test results
print(cor_test_result)

# Visualize the data
ggplot(filtered_data_clean, aes(x = CPP, y = ATR_numeric)) +
  geom_point() +
  labs(title = "CPP vs Numeric ATR",
       x = "CPP",
       y = "Numeric ATR") +
  theme_minimal()

# Assuming your data is in a dataframe called filtered_data_clean
correlation_result <- cor.test(filtered_data_clean$ATR_numeric, filtered_data_clean$H1H2c)
print(correlation_result)


# Perform Pearson correlation test between H1H2c and ATR_numeric
correlation_result_H1H2c <- cor.test(filtered_data_clean$H1H2c, filtered_data_clean$ATR_numeric)

# Print the result
print(correlation_result_H1H2c)


wilcox.test(H1H2c ~ ATR, data = filtered_data_clean)

# Calculate range and standard deviation for binwidth suggestion
range_CPP <- range(filtered_data_clean$CPP, na.rm = TRUE)
range_H1H2c <- range(filtered_data_clean$H1H2c, na.rm = TRUE)

# You can use a fraction of the range or standard deviation as a guideline for binwidth
binwidth_CPP <- (range_CPP[2] - range_CPP[1]) / 30  # Example for 30 bins
binwidth_H1H2c <- (range_H1H2c[2] - range_H1H2c[1]) / 30  # Example for 30 bins


ggplot(filtered_data_clean, aes(x = ATR_numeric, y = CPP)) +
  geom_point(aes(color = ATR)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = ATR)) +
  labs(title = "CPP vs ATR with Regression Line",
       x = "ATR",
       y = "CPP") +

  
  
  # Plot CPP vs ATR with regression line
  ggplot(filtered_data_clean, aes(x = ATR_numeric, y = CPP)) +
  geom_point(aes(color = ATR)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "CPP vs ATR with Regression Line",
       x = "ATR",
       y = "CPP") +
  scale_x_continuous(breaks = c(0, 1), labels = c("-ATR", "+ATR")) +
  theme_minimal()

# Plot H1H2c vs ATR with regression line
ggplot(filtered_data_clean, aes(x = ATR_numeric, y = H1H2c)) +
  geom_point(aes(color = ATR)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "H1H2c vs ATR with Regression Line",
       x = "ATR",
       y = "H1H2c") +
  scale_x_continuous(breaks = c(0, 1), labels = c("-ATR", "+ATR")) +
  theme_minimal()


t.test(H1H2c ~ ATR, data = filtered_data_clean)

install.packages("corrplot")
library("corrplot")

ggpairs(filtered_data_clean, columns = c("CPP", "H1H2c", "ATR_numeric"),
        mapping = aes(color = factor(ATR_numeric)),
        upper = list(continuous = wrap("cor", size = 5))) +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("+ATR", "-ATR"))


ggplot(filtered_data_clean, aes(x = CPP, y = H1H2c, color = factor(ATR_numeric))) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ ATR_numeric, labeller = as_labeller(c("0" = "-ATR", "1" = "+ATR"))) +
  labs(title = "CPP vs H1H2c by ATR Category",
       x = "CPP",
       y = "H1H2c",
       color = "ATR") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue"))


data_male <- filtered_data_clean %>% filter(Gender == "M")
data_female <- filtered_data_clean %>% filter(Gender == "F")

cor_male <- cor.test(data_male$H1H2c, data_male$ATR_numeric)
print(cor_male)

cor_female <- cor.test(data_female$H1H2c, data_female$ATR_numeric)
print(cor_female)


ggplot(data, aes(x = ATR, y = H1H2c, fill = Gender)) +
  geom_boxplot() +
  facet_wrap(~ Gender) +
  labs(title = "Boxplot of H1H2c Values by ATR and Gender",
       x = "ATR",
       y = "H1H2c") +
  scale_fill_manual(values = c("M" = "blue", "F" = "pink")) +
  theme_minimal()

ggplot(data, aes(x = ATR, y = H1H2c, fill = Gender)) +
  geom_violin(alpha = 0.6) +
  labs(title = "Violin Plot of H1H2c Values by ATR and Gender",
       x = "ATR",
       y = "H1H2c") +
  scale_fill_manual(values = c("M" = "blue", "F" = "pink")) +
  theme_minimal()


library(ggplot2)

# Make sure Gender is a factor with correct levels
data$Gender <- factor(data$Gender, levels = c("M", "F"))

library(ggplot2)

# Create a boxplot with labels for each gender
ggplot(data, aes(x = ATR, y = H1H2c, fill = Gender)) +
  geom_boxplot() +
  facet_wrap(~ Gender) +
  geom_text(
    data = data %>%
      group_by(ATR, Gender) %>%
      summarise(mean_H1H2c = mean(H1H2c, na.rm = TRUE), .groups = 'drop'),
    aes(x = ATR, y = mean_H1H2c, label = Gender),
    position = position_nudge(y = 0.2), # Adjust the position of the labels
    size = 4
  ) +
  labs(title = "Boxplot of H1H2c Values by ATR and Gender",
       x = "ATR",
       y = "H1H2c") +
  theme_minimal()

# Install necessary packages if not already installed
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readODS")

# Load the required libraries
library(ggplot2)
library(dplyr)
library(readODS)

# Define the path to your ODS file
file_path <- "/Users/chloeguttmann/Downloads/Akan-Final-2values.ods"

# Read the data from the ODS file
data <- read_ods(file_path)

# Display column names and data structure
print(colnames(data))
str(data)

# Convert ATR to numeric values for analysis
data$ATR_numeric <- ifelse(data$ATR == "+ATR", 1, 0)

# Convert Gender column to factors with appropriate labels
data$Gender <- factor(data$Gender, levels = c("M", "F"), labels = c("Male", "Female"))

# Verify conversion
print(levels(data$Gender))  # Should show "Male" and "Female"
print(table(data$Gender))   # Counts for Male and Female

# Remove rows with missing values in relevant columns
filtered_data <- data %>%
  filter(!is.na(H1H2c) & !is.na(ATR_numeric) & !is.na(Gender))

# Check cleaned data
print(summary(filtered_data))

# Create a boxplot of H1H2c values by ATR and Gender
ggplot(filtered_data, aes(x = ATR_numeric, y = H1H2c, fill = Gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  labs(title = "Boxplot of H1H2c Values by ATR and Gender",
       x = "ATR",
       y = "H1H2c") +
  scale_x_continuous(breaks = c(0, 1), labels = c("-ATR", "+ATR")) +
  theme_minimal()

# Install necessary packages if not already installed
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readODS")

# Load the required libraries
library(ggplot2)
library(dplyr)
library(readODS)

# Define the path to your ODS file
file_path <- "/Users/chloeguttmann/Downloads/Akan-Final-2values.ods"

# Read the data from the ODS file
data <- read_ods(file_path)

# Display column names and data structure
print(colnames(data))
str(data)

# Convert ATR to numeric values for analysis
data$ATR_numeric <- ifelse(data$ATR == "+ATR", 1, 0)

# Convert Gender column to factors with appropriate labels
data$Gender <- factor(data$Gender, levels = c("M", "F"), labels = c("Male", "Female"))

# Verify conversion
print(levels(data$Gender))  # Should show "Male" and "Female"
print(table(data$Gender))   # Counts for Male and Female

# Remove rows with missing values in relevant columns
filtered_data <- data %>%
  filter(!is.na(H1H2c) & !is.na(ATR_numeric) & !is.na(Gender))

# Check cleaned data
print(summary(filtered_data))

# Create a boxplot of H1H2c values by ATR and Gender
ggplot(filtered_data, aes(x = ATR_numeric, y = H1H2c, fill = Gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  labs(title = "Boxplot of H1H2c Values by ATR and Gender",
       x = "ATR",
       y = "H1H2c") +
  scale_x_continuous(breaks = c(0, 1), labels = c("-ATR", "+ATR")) +
  theme_minimal()


