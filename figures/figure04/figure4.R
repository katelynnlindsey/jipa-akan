# Load required packages
library(readxl)    # For reading .xls files
library(ggplot2)   # For plotting
library(dplyr)     # For data manipulation
library(ggpubr)    # For combining plots
library(gridExtra) # For arranging tables and plots

# Read the .xls file
# Replace 'path/to/your/file.xls' with the actual file path
Akan_fricatives_KLL <- read_excel("G:/.shortcut-targets-by-id/1tlhzQiC7N74zS81hrTqKLNquHEwXCqZX/Research/Projects/JIPA Akan/figures/Akan_fricatives_KLL.xlsx", 
                                  col_types = c("text", "text", "text", 
                                                "text", "text", "text", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric"))

data <- Akan_fricatives_KLL

# Clean and prepare the data
# Ensure column names match your spreadsheet
colnames(data) <- c("Sound_file", "Orthographic", "Phonemic", "Phonetic", 
                    "Fricative_phoneme", "Fricative_phonetic", 
                    "start", "duration", "intensity", "cog", "sdev", "skew", "kurt")

data$Fricative_phonetic[is.na(data$Fricative_phonetic)] <- data$Fricative_phoneme[is.na(data$Fricative_phonetic)]

# Convert fricative columns to factors for plotting
data$Fricative_phoneme <- as.factor(data$Fricative_phoneme)
data$Fricative_phonetic <- as.factor(data$Fricative_phonetic)


# Function to remove outliers using IQR method
remove_outliers <- function(df, column, group_by) {
  df %>%
    group_by({{group_by}}) %>%
    mutate(
      Q1 = quantile({{column}}, 0.25, na.rm = TRUE),
      Q3 = quantile({{column}}, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      Lower = Q1 - 1.5 * IQR,
      Upper = Q3 + 1.5 * IQR,
      is_outlier = {{column}} < Lower | {{column}} > Upper
    ) %>%
    filter(!is_outlier) %>%
    select(-Q1, -Q3, -IQR, -Lower, -Upper, -is_outlier)
}

# Remove outliers for cog, sdev, skew, kurt, duration, intensity
data_clean <- data

#for (col in c("cog", "sdev", "skew","kurt", "duration", "intensity")) {
#  data_clean <- remove_outliers(data_clean, !!sym(col), "Fricative_phonetic")
#}

# Report number of removed outliers
cat("Original data rows:", nrow(data), "\n")
cat("Cleaned data rows:", nrow(data_clean), "\n")

# Define a white background theme
white_theme <- theme(
  panel.background = element_rect(fill = "white"),
  plot.background = element_rect(fill = "white"),
  panel.grid.major = element_line(color = "grey90"),
  panel.grid.minor = element_line(color = "grey95"),
  legend.background = element_rect(fill = "white"),
  axis.line = element_line(color = "black")
)


# Filter data to include only the specified allophones in Fricative_phonetic
affricate_data <- data_clean %>% 
  filter(Fricative_phonetic %in% c("t͡ɕ", "t͡ɕᶣ", "d͡ʑ", "d͡ʑᶣ"))

# Boxplot for Center of Gravity (CoG)
p1 <- ggplot(affricate_data, aes(x = Fricative_phonetic, y = cog, fill = Fricative_phonetic)) +
  geom_boxplot() +
  labs(title = "CoG by Affricate Allophone",
       x = "Affricate Allophone", y = "CoG (Hz)") +
  white_theme +
  theme(legend.position = "none")

p1

