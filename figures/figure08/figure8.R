library(dplyr)
library(ggplot2)
library(readr)
library(zoo)   # for rollmean
library(scales)
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



# Distinguishable linetypes for B&W printing
lt <- c(
  "çʷ" = "solid",      # ———
  "f"   = "22",         # ——  ——  (long dash)
  "h"   = "42",         # — · — ·  (dash + dot)
  "s"   = "F2"          # —— · —— (long dash + dot)
)


spectra <- read.csv("~/GitHub/jipa-akan/figures/figure08/sound_spectra.csv")

spectra[spectra == "??"] <- "çʷ"

spectra <- rename(spectra, Fricative = Name)

#Normalize intensity within groups

spectra_norm <- spectra %>%
  group_by(Fricative) %>%
  mutate(
    Intensity_norm = Intensity - max(Intensity, na.rm = TRUE)
  ) %>%
  ungroup()

#Smooth intensity within groups

spectra_norm <- spectra_norm %>%
  group_by(Fricative) %>%
  arrange(Frequency) %>%
  mutate(
    Intensity_smooth = zoo::rollmean(Intensity_norm, k = 40, fill = "extend", na.pad = TRUE)
  ) %>%
  ungroup()

# Pick a labeling point for each line (right edge of plot)
label_points <- spectra_norm %>%
  filter(Frequency <= 10000) %>%
  group_by(Fricative) %>%
  slice_max(Frequency, n = 1)

px_spectra <-
  ggplot(spectra_norm,
         aes(x = Frequency, y = Intensity_smooth,
             color = Fricative, linetype = Fricative)) +
  geom_line(size = 0.9, alpha = 0.95) +
  scale_fill_viridis()+
  scale_linetype_manual(values = lt) +
  
  # Add symbols next to lines
  geom_point(
    data = label_points,
    aes(x = Frequency, y = Intensity_smooth, shape = Fricative),
    size = 3, fill = "white", stroke = 1.2
  ) +
  scale_shape_manual(values = c(
    "çʷ" = 21,
    "f"   = 22,
    "h"   = 23,
    "s"   = 24
  )) +
  
  coord_cartesian(xlim = c(0, 10000), ylim = c(-65, -5)) +
  labs(
    x = "Frequency (Hz)",
    y = "Normalized amplitude (dB)",
    #shape = "Fricative",
    #color = "Fricative",
    #linetype = "Fricative"
    ) +
  white_theme

px_spectra

ggsave(px_spectra,
       file = "~/GitHub/jipa-akan/figures/figure08/figure8.png",
       height = 4, width = 5, dpi = 300)

