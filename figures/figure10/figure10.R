# Load package and dataframe
library(tidyverse)
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

# Load dataframe
voweldur <- read.csv("~/GitHub/jipa-akan/figures/figure10/vowel_duration_contrast.csv")


# Recode vowel
# key: 1 =  /open O/, 11 = /long open o/
# 2 = /epsilon/, 22 = /long epsilon/
# 3 = /I/, 33 = /long I/ 
# 4 = /horse shoe/, 44 = /long horse shoe/ 

voweldur$vowel_id <- recode(voweldur$vowel_id, "1" = "\u0254", "11" = "\u0254\u02D0", 
                            "2" = "\u025B", "22" = "\u025B\u02D0",
                            "3" = "\u026A", "33" = "\u026A\u02D0",
                            "4" = "\u028A", "44" = "\u028A\u02D0",
                            "a" = "a", "a:" = "a\u02D0", 
                            "e" = "e", "e:" = "e\u02D0",
                            "i" = "i", "i:" = "i\u02D0",
                            "o" = "o", "o:" = "o\u02D0", 
                            "u" = "u", "u:" = "u\u02D0")


voweldur$vowel_id <- factor(voweldur$vowel_id,
                            levels = c("i", "iː", "u", "uː", "ɪ", "ɪː",
                                       "ʊ", "ʊː", "e", "eː", "o", "oː",
                                       "ɛ", "ɛː", "ɔ", "ɔː", "a", "aː"))

# check vowel_id levels to make sure the length diacritic appeared as expected
levels(voweldur$vowel_id)

# covert duration from secs to milisecs
voweldur$durationMS <- voweldur$duration*1000

# calculate mean per vowel quality
voweldur%>%
  group_by(vowel_id)%>%
  summarise(mean_dur = mean(durationMS))

# Plot length contrast
figure10 <- ggplot(voweldur, aes(x=vowel_id, y=durationMS, fill=vowel_id))+
  geom_boxplot(notch = F)+
  white_theme +
  theme(legend.position = "none") +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  xlab("")+
  ylab("Duration (ms)")+
  scale_color_viridis()+
  scale_x_discrete(limits=c("i", "i\u02D0",  
                            "u","u\u02D0",
                            "\u026A","\u026A\u02D0",
                            "\u028A","\u028A\u02D0",
                            "e","e\u02D0",
                            "o","o\u02D0",
                            "\u025B","\u025B\u02D0",
                            "\u0254", "\u0254\u02D0",
                            "a", "a\u02D0"))

figure10

ggsave(figure10,
       file = "~/GitHub/jipa-akan/figures/figure10/figure10.png",
       height = 4, width = 5, dpi = 300)
