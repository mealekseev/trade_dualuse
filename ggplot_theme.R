### ggplot2 theme for project graphs

library(ggplot2)
library(ggthemes)
library(colorspace)
library(extrafont)


# colors
magenta <- rgb(135 / 255, 0 / 255, 57 / 255, 0.8, names = NULL, maxColorValue = 1)
forest <- rgb(0 / 255, 60 / 255, 28 / 255, 0.8, names = NULL, maxColorValue = 1)
gold <- rgb(175 / 255, 142 / 255, 44 / 255, 0.8, names = NULL, maxColorValue = 1)
royalblue <- rgb(51 / 255, 71 / 255, 106 / 255, 1.0, names = NULL, maxColorValue = 1)
royalred <- rgb(120 / 255, 0 / 255, 15 / 255, 1.0, names = NULL, maxColorValue = 1)
lightblue <- rgb(200 / 255, 210 / 255, 230 / 255, 1.0, names = NULL, maxColorValue = 1)
graphite <- rgb(94 / 255, 94 / 255, 94 / 255, 1.0, names = NULL, maxColorValue = 1)

# graph theme
custom_theme <- theme_minimal() + theme(
    text = element_text(size = 16, family = "Erewhon"),
    legend.position = 'bottom',
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 16)
)

