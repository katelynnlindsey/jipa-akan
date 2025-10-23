library(readxl)
VOT_data <- read_excel("~/GitHub/jipa-akan/figures/figure03/VOT_data.xlsx")

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

VOT_data_filtered <- VOT_data %>%
  mutate(
    VOT = VOT...6 * 1000   # convert seconds → milliseconds
  ) %>%
  filter(!phoneme %in% c("tw", "dw")) %>%
  mutate(
    phoneme = factor(phoneme, levels = c("p", "t", "k", "b", "d", "g"))
  )

# Build base plot (establish discrete x scale)
p <- ggplot(VOT_data_filtered, aes(x = phoneme, y = VOT, fill = phoneme)) +
  geom_blank() +   # establishes discrete x scale early
  
  # shaded regions using numeric x indices (1 to 6 categories)
  annotate("rect", xmin = 0.5, xmax = 6.5, ymin = -139, ymax = -60,
           fill = "#F8766D", alpha = 0.1) +
  annotate("rect", xmin = 0.5, xmax = 6.5, ymin = 1.4, ymax = 41,
           fill = "#7CAE00", alpha = 0.1) +
  annotate("rect", xmin = 0.5, xmax = 6.5, ymin = 57, ymax = 97,
           fill = "#00BFC4", alpha = 0.1) +
  
  # labels for shaded regions
  annotate("text", x = 3.5, y = -100, label = "long-lead", size = 4, color = "#F8766D") +
  annotate("text", x = 3.5, y = 20, label = "short-lag", size = 4, color = "#7CAE00") +
  annotate("text", x = 3.5, y = 77, label = "long-lag", size = 4, color = "#00BFC4") +
  
  # data layers
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.15, size = 1, alpha = 0.5) +
  
  # colors and styling
  theme_classic(base_size = 14) +
  labs(
    x = "Plosives",
    y = "VOT Duration (ms)",
    title = "Voice Onset Time for Plosives"
  ) +
  white_theme +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

# Print the plot
p
