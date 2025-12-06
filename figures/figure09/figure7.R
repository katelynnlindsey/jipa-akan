# Load package and dataframe
library(tidyverse)
library(phonR)
library(ggplot2)
library(viridis)
library(extrafont)
font_import(pattern = "CharisSIL", prompt = FALSE)
loadfonts()

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

dfvowels <- read.csv("~/GitHub/jipa-akan/figures/figure07/vowel_formants.csv")

# Recode vowel
# key: 1 =  /open O/, 2 = /epsilon/, 3 = /I/, and 4 = /horse shoe/ 

dfvowels$vowel_id <- recode(dfvowels$vowel_id, "1" = "\u0254", "2" = "\u025B", 
                            "3" = "\u026A", "4" = "\u028A", "a" = "a","e" = "e", 
                            "i" = "i", "o"  = "o", "ae" = "\u00E6", "u" = "u")

# Exclude Elizabeth's data -- a lot of clipping in her recording
dfvowels <- dfvowels%>%
  filter(speaker_id!="Elizabeth-010_mono")%>%
  droplevels()

# Remove non-phonemic ash vowel
dfvowels <- dfvowels%>%
  filter(vowel_id!="\u00E6")%>%
  droplevels()

df_male <- subset(dfvowels, sex == "male")
df_female <- subset(dfvowels, sex == "female")

means <- dfvowels %>%
  group_by(vowel_id) %>%
  summarise(F1 = mean(F1),
            F2 = mean(F2))

v1 <- ggplot(dfvowels, aes(x = F2, y = F1, color = vowel_id, label = vowel_id)) + 
  geom_point(alpha = 0.2) + 
  stat_ellipse(level = 0.67) + 
  geom_label(data = means) + 
  scale_x_reverse() + 
  scale_y_reverse() + 
  white_theme +
  scale_fill_viridis()+
  facet_wrap(~sex) + 
  guides(color = FALSE)

v1

ggsave(v1,
       file = "~/GitHub/jipa-akan/figures/figure07/figure7.png",
       height = 4, width = 5, dpi = 300)
