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

# 1. Create Boxplots for CoG, sdev, skew, and kurt by Fricative_phonetic
# Boxplot for Center of Gravity (CoG)
p1 <- ggplot(data_clean, aes(x = Fricative_phonetic, y = cog, fill = Fricative_phonetic)) +
  geom_boxplot() +
  labs(title = "Center of Gravity by Fricative Allophone",
       x = "Fricative Allophone", y = "CoG (Hz)") +
  white_theme +
  theme(legend.position = "none")

# Boxplot for Standard Deviation (sdev)
p2 <- ggplot(data_clean, aes(x = Fricative_phonetic, y = sdev, fill = Fricative_phonetic)) +
  geom_boxplot() +
  labs(title = "Standard Deviation by Fricative Allophone",
       x = "Fricative Allophone", y = "Sdev (Hz)") +
  white_theme +
  theme(legend.position = "none")

# Boxplot for Skewness
p3 <- ggplot(data_clean, aes(x = Fricative_phonetic, y = skew, fill = Fricative_phonetic)) +
  geom_boxplot() +
  labs(title = "Skewness by Fricative Allophone",
       x = "Fricative Allophone", y = "Skewness") +
  white_theme +
  theme(legend.position = "none")

# Boxplot for Kurtosis
p4 <- ggplot(data_clean, aes(x = Fricative_phonetic, y = kurt, fill = Fricative_phonetic)) +
  geom_boxplot() +
  labs(title = "Kurtosis by Fricative Allophone",
       x = "Fricative Allophone", y = "Kurtosis") +
  white_theme +
  theme(legend.position = "none")

# Combine boxplots into a single figure
boxplot_figure <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Save boxplot figure
ggsave("fricative_boxplots.png", boxplot_figure, width = 10, height = 8, dpi = 300)

# 2. Create Scatterplot of CoG vs. Sdev, colored by Fricative_phonetic
scatter_plot <- ggplot(data_clean, aes(x = cog, y = sdev, color = Fricative_phonetic)) +
  geom_point(size = 3) +
  labs(title = "CoG vs. Sdev by Fricative (Phonetic)",
       x = "Center of Gravity (Hz)", y = "Standard Deviation (Hz)") +
  white_theme +
  theme(legend.title = element_text(size = 10),
        legend.position = "right")

# Save scatterplot
ggsave("fricative_scatterplot.png", scatter_plot, width = 8, height = 6, dpi = 300)

# 3. Create Summary Table of Mean Values
summary_table <- data_clean %>%
  group_by(Fricative_phoneme, Fricative_phonetic) %>%
  summarise(
    Mean_CoG = mean(cog, na.rm = TRUE),
    Mean_Sdev = mean(sdev, na.rm = TRUE),
    Mean_Skew = mean(skew, na.rm = TRUE),
    Mean_Kurt = mean(kurt, na.rm = TRUE),
    Mean_Duration = mean(duration, na.rm = TRUE) * 1000, # Convert to ms
    Mean_Intensity = mean(intensity, na.rm = TRUE),
    N = n(),
    .groups = "drop"
  )

# Round numeric columns for readability
summary_table <- summary_table %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# Save summary table as a CSV for reference
write.csv(summary_table, "fricative_summary_table.csv", row.names = FALSE)

# Create a table plot for the manuscript
table_plot <- tableGrob(summary_table, rows = NULL)
grid.arrange(table_plot)

# Save table as an image
png("fricative_summary_table.png", width = 800, height = 600)
grid.arrange(table_plot)
dev.off()

# Optional: Print summary table to console for inspection
print(summary_table)

# Define a function to create boxplots for a given Fricative_phoneme
create_boxplots <- function(data, phoneme, output_file) {
  # Filter data for the specific Fricative_phoneme
  data_subset <- data %>% filter(Fricative_phoneme == phoneme)
  
  # Boxplot for Center of Gravity (CoG)
  p1 <- ggplot(data_subset, aes(x = Fricative_phonetic, y = cog, fill = Fricative_phonetic)) +
    geom_boxplot() +
    labs(title = paste("CoG by Fricative Allophone (", phoneme, ")"),
         x = "Fricative Allophone", y = "CoG (Hz)") +
    white_theme +
    theme(legend.position = "none")
  
  # Boxplot for Standard Deviation (sdev)
  p2 <- ggplot(data_subset, aes(x = Fricative_phonetic, y = sdev, fill = Fricative_phonetic)) +
    geom_boxplot() +
    labs(title = paste("Sdev by Fricative Allophone (", phoneme, ")"),
         x = "Fricative Allophone", y = "Sdev (Hz)") +
    white_theme +
    theme(legend.position = "none")
  
  # Boxplot for Skewness
  p3 <- ggplot(data_subset, aes(x = Fricative_phonetic, y = skew, fill = Fricative_phonetic)) +
    geom_boxplot() +
    labs(title = paste("Skewness by Fricative Allophone (", phoneme, ")"),
         x = "Fricative Allophone", y = "Skewness") +
    white_theme +
    theme(legend.position = "none")
  
  # Boxplot for Kurtosis
  p4 <- ggplot(data_subset, aes(x = Fricative_phonetic, y = kurt, fill = Fricative_phonetic)) +
    geom_boxplot() +
    labs(title = paste("Kurtosis by Fricative Allophone (", phoneme, ")"),
         x = "Fricative Allophone", y = "Kurtosis") +
    white_theme +
    theme(legend.position = "none")
  
  # Combine boxplots into a single figure
  boxplot_figure <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
  
  # Save the figure
  ggsave(output_file, boxplot_figure, width = 10, height = 8, dpi = 300)
}

# List of Fricative_phoneme values
phonemes <- c("f", "s", "h", "ɕᶣ")

# Create and save boxplots for each Fricative_phoneme
for (phoneme in phonemes) {
  output_file <- paste0("fricative_boxplots_", phoneme, ".png")
  create_boxplots(data_clean, phoneme, output_file)
  cat("Saved boxplots for", phoneme, "to", output_file, "\n")
}

# Function to create a CoG boxplot for a given Fricative_phoneme
create_cog_boxplot <- function(data, phoneme) {
  # Filter data for the specific Fricative_phoneme
  data_subset <- data %>% filter(Fricative_phoneme == phoneme)
  
  # Boxplot for Center of Gravity (CoG)
  ggplot(data_subset, aes(x = Fricative_phonetic, y = cog, fill = Fricative_phonetic)) +
    geom_boxplot() +
    labs(title = paste("Center of Gravity of /",phoneme,"/"),
         x = "Fricative Allophones", y = "Center of Gravity (Hz)") +
    white_theme +
    theme(legend.position = "none")
}

# List of Fricative_phoneme values
phonemes <- c("f", "s", "h", "ɕᶣ")

# Create CoG boxplots for each Fricative_phoneme
cog_plots <- lapply(phonemes, function(phoneme) {
  create_cog_boxplot(data_clean, phoneme)
})

# Combine CoG boxplots into a single 2x2 figure
combined_cog_figure <- ggarrange(plotlist = cog_plots, ncol = 2, nrow = 2)

# Save the combined figure
ggsave("combined_cog_boxplots.png", combined_cog_figure, width = 10, height = 8, dpi = 300)

# Optional: Print a message to confirm
cat("Saved combined CoG boxplots to combined_cog_boxplots.png\n")

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

# Boxplot for Duration (convert to milliseconds for better readability)
p2 <- ggplot(affricate_data, aes(x = Fricative_phonetic, y = duration * 1000, fill = Fricative_phonetic)) +
  geom_boxplot() +
  labs(title = "Duration by Affricate Allophone",
       x = "Affricate Allophone", y = "Duration (ms)") +
  white_theme +
  theme(legend.position = "none")

# Boxplot for Intensity
p3 <- ggplot(affricate_data, aes(x = Fricative_phonetic, y = intensity, fill = Fricative_phonetic)) +
  geom_boxplot() +
  labs(title = "Intensity by Affricate Allophone",
       x = "Affricate Allophone", y = "Intensity (dB)") +
  white_theme +
  theme(legend.position = "none")

# Combine the boxplots into a single figure (1 row, 3 columns)
affricate_comparison_figure <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)

# Save the combined figure
ggsave("affricate_comparison_boxplots.png", affricate_comparison_figure, width = 12, height = 4, dpi = 300)

# Optional: Print a message to confirm
cat("Saved affricate comparison boxplots to affricate_comparison_boxplots.png\n")

