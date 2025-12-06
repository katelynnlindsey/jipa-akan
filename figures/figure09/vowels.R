# Load package and dataframe
library(tidyverse)

dfvowels <- read.csv("~/GitHub/jipa-akan/figures/figure06/vowel_formants.csv")
summary(dfvowels)

# Recode vowel
# key: 1 =  /open O/, 2 = /epsilon/, 3 = /I/, and 4 = /horse shoe/ 

dfvowels$vowel_id <- recode(dfvowels$vowel_id, "1" = "\u0254", "2" = "\u025B", 
                            "3" = "\u026A", "4" = "\u028A", "a" = "a","e" = "e", 
                            "i" = "i", "o"  = "o", "ae" = "\u00E6", "u" = "u")

# Exclude Elizabeth's data -- a lot of clipping in her recording
dfvowels <- dfvowels%>%
  filter(speaker_id!="Elizabeth-010_mono")%>%
  droplevels()

# Normalize vowels: z-score/Lobanov normalization

#In this method, each subject's formants are put on a #scale of standard deviations.
#This can be done using either the normLobanov() #function from phonR or the scale() function in base R.


library(phonR)

# Normalize vowels

dfvowelslobanov = dfvowels %>% 
  group_by(speaker_id) %>% 
  mutate(f1scale = scale(F1), f2scale = scale(F2), f1lobanov = normLobanov(F1), f2lobanov = normLobanov(F2))

#Reorder columns
dfvowelslobanov2 <- dfvowelslobanov[, c(1, 3, 2, 12, 13, 4, 5, 6, 7, 8, 9, 10, 11)]

# Create means for every single vowel

my_vowels <- dfvowels %>%
  group_by(vowel_id) %>%
  summarise(mean_F1 = mean(F1),
            mean_F2 = mean(F2))%>%
  print(n=10)

# Make vowel plot

with(dfvowelslobanov, plotVowels(f1scale, f2scale, vowel_id, plot.tokens = FALSE, plot.means = TRUE, 
                           pch.means = vowel_id, cex.means = 2, var.col.by = vowel_id, 
                           ellipse.fill = FALSE, pretty = TRUE))


my_vowels%>%
  ggplot(aes(mean_F2, mean_F1, color=vowel_id, label=vowel_id))+
  geom_text(size=7)+
  scale_x_reverse() +
  scale_y_reverse() +
  theme_minimal()+
  theme(legend.position = "none")+
  ylab("F1")+
  xlab("F3")


# Make vowel plot with tokens and means
view(dfvowelslobanov)

par(mfrow = c(2, 2))
with(dfvowelslobanov, plotVowels(f1scale, f2scale, var.sty.by = vowel_id, var.col.by = vowel_id, pretty = TRUE))
with(dfvowelslobanov, plotVowels(f1scale, f2scale, var.sty.by = vowel_id, var.col.by = sex, pretty = TRUE))
with(dfvowelslobanov, plotVowels(f1scale, f2scale, var.sty.by = sex, var.col.by = speaker_id, pretty = TRUE))


par(mfrow=)
with(dfvowelslobanov, plotVowels(f1scale, f2scale, vowel_id, plot.tokens = TRUE, pch.tokens = vowel_id, cex.tokens = 1.2, 
                      alpha.tokens = 0.2, plot.means = TRUE, pch.means = vowel_id, cex.means = 2, var.col.by = vowel_id, 
                      ellipse.line = TRUE, pretty = TRUE))


# Plot vowels by gender

with(dfvowelslobanov, plotVowels(f1scale, f2scale, vowel_id, group = sex, plot.tokens = FALSE, plot.means = TRUE, 
                      pch.means = vowel_id, cex.means = 2, var.col.by = sex, ellipse.line = TRUE, pretty = TRUE,legend.kwd="bottomright"))

## Short vowel duration
# Load package
library(hrbrthemes)
library(viridis)

# covert duration from secs to milisecs
dfvowels$durationMS <- dfvowels$duration*1000

# calculate mean per vowel quality
voweldur <- dfvowels%>%
  group_by(vowel_id)%>%
  summarise(mean_dur = mean(durationMS))

# Plot short vowel duration
ggplot(dfvowels, aes(x=durationMS, y=vowel_id, fill=vowel_id))+
  geom_boxplot(notch = T)+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  xlab("duration (ms)")+
  ylab("vowel")+
  scale_y_discrete(limits=c("a", "\u025B", "?", "e", "\u026A", "\u0254", 
                            "o", "\u028A", "u", "i"))+
  xlim(100,350)
  
# Eliminate vowels in 'closed' syllables and plot vowels in 'open' syllables only
opensyl <- dfvowels%>%
  filter(syllable_type=="open")%>%
  droplevels()

# Plot short vowels in open syllables
ggplot(opensyl, aes(x=durationMS, y=vowel_id, fill=vowel_id))+
  geom_boxplot(notch = T)+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  xlab("duration (ms)")+
  ylab("vowel")+
  scale_y_discrete(limits=c("a", "\u025B", "?", "e", "\u026A", "\u0254", 
                            "o", "\u028A", "u", "i"))+
  xlim(100,500)


# Plot vowel qualities of filtered vowels

overlapo <- dfvowelslobanov%>%
  filter(vowel_id=="o"|vowel_id=="\u028A")%>%
  droplevels()

overlape <- dfvowelslobanov%>%
  filter(vowel_id=="e"|vowel_id=="\u026A")%>%
  droplevels()

overlapep <- dfvowelslobanov%>%
  filter(vowel_id=="\u025B"|vowel_id=="\xe6")%>%
  droplevels()


par(mfrow = c(2, 3))
with(overlapo, plotVowels(f1scale, f2scale, vowel_id, plot.tokens = TRUE, pch.tokens = vowel_id, cex.tokens = 1.2, 
                                 alpha.tokens = 0.2, plot.means = TRUE, pch.means = vowel_id, cex.means = 2, var.col.by = vowel_id, 
                                 ellipse.line = TRUE, pretty = TRUE))
with(overlape, plotVowels(f1scale, f2scale, vowel_id, plot.tokens = TRUE, pch.tokens = vowel_id, cex.tokens = 1.2, 
                          alpha.tokens = 0.2, plot.means = TRUE, pch.means = vowel_id, cex.means = 2, var.col.by = vowel_id, 
                          ellipse.line = TRUE, pretty = TRUE))
with(overlapep, plotVowels(f1scale, f2scale, vowel_id, plot.tokens = TRUE, pch.tokens = vowel_id, cex.tokens = 1.2, 
                          alpha.tokens = 0.2, plot.means = TRUE, pch.means = vowel_id, cex.means = 2, var.col.by = vowel_id, 
                          ellipse.line = TRUE, pretty = TRUE))


ggplot(overlapo, aes(x=duration, y=vowel_id, fill=vowel_id))+
  geom_boxplot(notch = T)+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  xlab("duration (ms)")+
  ylab("vowel")+
  scale_y_discrete(limits=c("u","\u026A","i", "\u028A", "\u0254", "o", 
                            "\u025B", "a", "e"))
  #xlim(100,300)


ggplot(overlape, aes(x=duration, y=vowel_id, fill=vowel_id))+
  geom_boxplot(notch = T)+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  xlab("duration (ms)")+
  ylab("vowel")+
  scale_y_discrete(limits=c("u","\u026A","i", "\u028A", "\u0254", "o", 
                            "\u025B", "a", "e"))+
  xlim(.1,.3)

ggplot(overlapep, aes(x=duration, y=vowel_id, fill=vowel_id))+
  geom_boxplot(notch = T)+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  xlab("duration (ms)")+
  ylab("vowel")+
  xlim(.1,.3)

## Length contrast

# Load dataframe
voweldur <- read.csv(file.choose())

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
# check vowel_id levels to make sure the length diacritic appeared as expected
levels(voweldur$vowel_id)

# covert duration from secs to milisecs
voweldur$durationMS <- voweldur$duration*1000

# calculate mean per vowel quality
voweldur%>%
  group_by(vowel_id)%>%
  summarise(mean_dur = mean(durationMS))

# Plot length contrast
ggplot(voweldur, aes(x=durationMS, y=vowel_id, fill=vowel_id))+
  geom_boxplot(notch = T)+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  xlab("duration (ms)")+
  ylab("vowel")+
  scale_y_discrete(limits=c("a\u02D0", "a", "\u0254\u02D0", "\u0254", 
                            "\u025B\u02D0", "\u025B",
                            "o\u02D0", "o",
                            "\u028A\u02D0", "\u028A",
                            "u\u02D0", "u",
                            "\u026A\u02D0", "\u026A",
                            "i\u02D0", "i"))

# Figure 4 for JIPA Akan Resubmission
dfvowels <- read.csv(file.choose())
vowel_formants -> dfvowels
summary(dfvowels)
view(dfvowels)

# Recode vowel
# key: 1 =  /open O/, 2 = /epsilon/, 3 = /I/, and 4 = /horse shoe/ 

dfvowels$vowel_id <- recode(dfvowels$vowel_id, "1" = "\u0254", "2" = "\u025B", 
                            "3" = "\u026A", "4" = "\u028A", "a" = "a","e" = "e", 
                            "i" = "i", "o"  = "o", "æ" = "\xe6", "u" = "u")
view(dfvowels)

# Exclude Elizabeth's data -- a lot of clipping in her recording
dfvowels <- dfvowels%>%
  filter(speaker_id!="Elizabeth-010_mono")%>%
  droplevels()

view(dfvowels)

library(phonR)

# Create means for every single vowel

my_vowels <- dfvowels %>%
  group_by(vowel_id) %>%
  summarise(mean_F1 = mean(F1),
            mean_F2 = mean(F2))%>%
  print(n=10)

# Remove non-phonemic ash vowel
dfvowels <- dfvowels%>%
  filter(vowel_id!="ae")%>%
  droplevels()

# graph by sex
with(dfvowels, plotVowels(F1, F2, vowel_id, group = sex, plot.tokens = TRUE, pch.tokens = vowel_id, cex.tokens = 1.2, alpha.tokens = 0.2, 
                                plot.means = TRUE, pch.means = vowel_id, cex.means = 2, var.col.by = sex, 
                                ellipse.line = TRUE, pretty = TRUE, legend.kwd="bottomright"))
