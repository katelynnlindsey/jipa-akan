# Load required packages
library(readxl)    # For reading .xls files
library(ggplot2)   # For plotting
library(dplyr)     # For data manipulation
library(ggpubr)    # For combining plots
library(gridExtra) # For arranging tables and plots
library(viridis)
library(extrafont)
font_import(pattern = "CharisSIL", prompt = FALSE)
loadfonts()

data <- read_excel("GitHub/jipa-akan/figures/figure07/Akan_fricatives_KLL.xlsx")


# Clean and prepare the data
# Ensure column names match your spreadsheet
colnames(data) <- c("Sound_file", "Orthographic", "Phonemic", "Phonetic", 
                    "Fricative_phoneme", "Fricative_phonetic", 
                    "start", "duration", "intensity", "cog", "sdev", "skew", "kurt")

data$Fricative_phonetic[is.na(data$Fricative_phonetic)] <- data$Fricative_phoneme[is.na(data$Fricative_phonetic)]

# Convert fricative columns to factors for plotting
data$Fricative_phoneme <- as.factor(data$Fricative_phoneme)
data$Fricative_phonetic <- as.factor(data$Fricative_phonetic)

data$Fricative_phoneme <- gsub("ɕᶣ", "çʷ", data$Fricative_phoneme)
data$Fricative_phonetic <- gsub("ɕᶣ", "çʷ", data$Fricative_phonetic)



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
  axis.line = element_line(color = "black"),
  text=element_text(family="Charis SIL")
)

# List of Fricative_phoneme values
phonemes <- c("f", "s", "h", "ɕᶣ")

# Function to create a CoG boxplot for a given Fricative_phoneme
create_cog_boxplot <- function(data, phoneme) {
  data_subset <- data %>% filter(Fricative_phoneme == phoneme)
  
  ggplot(data_subset, aes(x = Fricative_phonetic, y = cog, fill = Fricative_phonetic)) +
    geom_boxplot() +
    labs(title = paste("Center of Gravity of /",phoneme,"/"),
         x = "Fricative Allophones", y = "Center of Gravity (Hz)") +
    white_theme +
    scale_color_viridis()+
    theme(legend.position = "none",
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
}

# List of Fricative_phoneme values
phonemes <- c("f", "s", "h", "çʷ")

# Create CoG boxplots for each Fricative_phoneme
cog_plots <- lapply(phonemes, function(phoneme) {
  create_cog_boxplot(data_clean, phoneme)
})

# Combine CoG boxplots into a single 2x2 figure
combined_cog_figure <- ggarrange(plotlist = cog_plots, ncol = 2, nrow = 2)

combined_cog_figure


# Save the combined figure
ggsave(combined_cog_figure,
       file = "~/GitHub/jipa-akan/figures/figure07/figure7.png",
       height = 8, width = 10, dpi = 300)
