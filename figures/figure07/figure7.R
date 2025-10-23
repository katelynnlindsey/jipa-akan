install.packages("readODS")
library(readODS)
library(ggplot2)

# Define a white background theme
white_theme <- theme(
  panel.background = element_rect(fill = "white"),
  plot.background = element_rect(fill = "white"),
  panel.grid.major = element_line(color = "grey90"),
  panel.grid.minor = element_line(color = "grey95"),
  legend.background = element_rect(fill = "white"),
  axis.line = element_line(color = "black")
)

Akan.final.2values <- read.csv("~/GitHub/jipa-akan/figures/figure07/Akan-final-2values.csv")

data <- Akan.final.2values


# Makes sure all 0s are numerical 
data[] <- lapply(data, function(x) if (is.character(x)) trimws(x) else x)

# Convert to numeric if needed
data$CPP <- as.numeric(data$CPP)
data$H1H2c <- as.numeric(data$H1H2c)

filtered_data <- data %>%
  filter(CPP != 0, H1H2c != 0)

# Boxplot of CPP values by ATR
ggplot(filtered_data, aes(x = ATR, y = CPP, fill = ATR)) +
  geom_boxplot() +
  labs(title = "Relationship between CPP and ATR values",
       x = "",
       y = "CPP (dB)") +
  white_theme +
  theme(legend.position = "none")
