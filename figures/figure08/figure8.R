# Load package and dataframe
library(tidyverse)

# Define a white background theme
white_theme <- theme(
  panel.background = element_rect(fill = "white"),
  plot.background = element_rect(fill = "white"),
  panel.grid.major = element_line(color = "grey90"),
  panel.grid.minor = element_line(color = "grey95"),
  legend.background = element_rect(fill = "white"),
  axis.line = element_line(color = "black")
)

dfvowels <- read.csv("~/GitHub/jipa-akan/figures/figure06/vowel_formants.csv")
summary(dfvowels)

# Recode vowel
# key: 1 =  /open O/, 2 = /epsilon/, 3 = /I/, and 4 = /horse shoe/ 

dfvowels$vowel_id <- recode(dfvowels$vowel_id, "1" = "\u0254", "2" = "\u025B", 
                            "3" = "\u026A", "4" = "\u028A", "a" = "a","e" = "e", 
                            "i" = "i", "o"  = "o", "ae" = "\u00E6", "u" = "u")

# covert duration from secs to milisecs
dfvowels$durationMS <- dfvowels$duration*1000

# calculate mean per vowel quality
voweldur <- dfvowels%>%
  group_by(vowel_id)%>%
  summarise(mean_dur = mean(durationMS))

# remove outliers
remove_outliers <- function(df, column, group_var) {
  col <- enquo(column)
  grp <- enquo(group_var)
  
  df %>%
    group_by(!!grp) %>%
    mutate(
      Q1 = quantile(!!col, 0.25, na.rm = TRUE),
      Q3 = quantile(!!col, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      Lower = Q1 - 1.5 * IQR,
      Upper = Q3 + 1.5 * IQR,
      is_outlier = !!col < Lower | !!col > Upper
    ) %>%
    filter(!is_outlier) %>%
    ungroup() %>%
    select(-Q1, -Q3, -IQR, -Lower, -Upper, -is_outlier)
}

# Remove outliers per vowel
dfvowels_clean <- remove_outliers(dfvowels, durationMS, vowel_id)

# categorize high vowels
dfvowels_clean <- dfvowels_clean %>%
  mutate(
    vowel_type = ifelse(vowel_id %in% c("i", "u", "\u026A","\u028A"), "high", "other")
  )

# Plot short vowel duration
ggplot(dfvowels_clean, aes(x=vowel_id, y=durationMS, fill=vowel_type))+
  geom_boxplot(notch = F)+
  white_theme +
  theme(legend.position = "none")+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  xlab("Vowel")+
  ylab("Duration (ms)")+
  scale_x_discrete(limits=c("i", "u", "\u026A","\u028A","e", "o", "\u025B", "\u0254", "\u00E6","a"))+
  scale_fill_manual(values = c("high" = "tomato", "other" = "skyblue")) +
  ylim(100,250)
