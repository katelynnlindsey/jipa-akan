install.packages("readODS")
library(readODS)
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

data <- read.csv("~/GitHub/jipa-akan/figures/figure08/Akan-final-2values.csv")


# Makes sure all 0s are numerical 
data[] <- lapply(data, function(x) if (is.character(x)) trimws(x) else x)

# Convert to numeric if needed
data$CPP <- as.numeric(data$CPP)
data$H1H2c <- as.numeric(data$H1H2c)

filtered_data <- data %>%
  filter(CPP != 0, H1H2c != 0)

# Boxplot of CPP values by ATR
figure8 <- ggplot(filtered_data, aes(x = ATR, y = CPP, fill = ATR)) +
  geom_boxplot() +
  labs(title = "Relationship between CPP and ATR values",
       x = "",
       y = "CPP (dB)") +
  white_theme +
  scale_color_viridis()+
  theme(legend.position = "none")

figure8

ggsave(figure8,
       file = "~/GitHub/jipa-akan/figures/figure08/figure8.png",
       height = 4, width = 5, dpi = 300)
