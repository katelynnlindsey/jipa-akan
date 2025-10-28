# Load package and dataframe
library(tidyverse)
library(phonR)


dfvowels <- read.csv("~/GitHub/jipa-akan/figures/figure06/vowel_formants.csv")

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

par(mfrow = c(1, 2))

# First plot
with(df_male, plotVowels(F1, F2, vowel_id,
                         plot.tokens = TRUE,
                         pch.tokens = vowel_id,
                         cex.tokens = 1.2,
                         alpha.tokens = 0.2,
                         plot.means = TRUE,
                         pch.means = vowel_id,
                         cex.means = 2,
                         var.col.by = vowel_id,
                         ellipse.line = TRUE,
                         pretty = TRUE,
                         xlim = c(3000, 400),
                         ylim = c(1200, 100),
                         xlab = "F2 (Hz)",
                         ylab = "F1 (Hz)",
                         main = "Male speakers"))

# Second plot
with(df_female, plotVowels(F1, F2, vowel_id,
                           plot.tokens = TRUE,
                           pch.tokens = vowel_id,
                           cex.tokens = 1.2,
                           alpha.tokens = 0.2,
                           plot.means = TRUE,
                           pch.means = vowel_id,
                           cex.means = 2,
                           var.col.by = vowel_id,
                           ellipse.line = TRUE,
                           pretty = TRUE,
                           xlim = c(3000, 400),
                           ylim = c(1200, 100),
                           xlab = "F2 (Hz)",
                           ylab = "F1 (Hz)",
                           main = "Female speakers"))
