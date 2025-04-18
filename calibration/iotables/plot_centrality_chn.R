### Plot centrality

rm(list=ls())
gc()

library(data.table)
library(ggrepel)
library(fixest)
library(xtable)
library(stringr)
library(latex2exp)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


plot_industries <- function() {
    df <- fread(file.path(OUTPUT_PATH, "calibration", "iotables", "centrality_naics_chn.csv"))
    df <- df[X != 0]
    df <- df[tradable == 1]
    df <- df[!(naics12_two_digit %in% c("92", "99"))]
 
    # normalize centrality
    df[, cent_C_norm := cent_C / sum(cent_C) * 100]
    df[, cent_M_norm := cent_M / sum(cent_M) * 100]
    df[, sector_name := str_replace_all(sector_name, "Manufacturing", "Mfg")]
    df[, sector_name := str_replace_all(sector_name, "Farming", "Frm")]

    # curate labels
    df[, cent_M_rank := rank(-cent_M_norm)]
    df[, cent_C_rank := rank(-cent_C_norm)]
    df[, plot_desc := ""]
    df[cent_M_rank <= 40, plot_desc := sector_name]
    df[cent_C_rank <= 40, plot_desc := sector_name]
    df[, sales_distance := cent_M_norm / (cent_M_norm + cent_C_norm)]

    # plot
    ggplot(data = df, aes(x = cent_C_norm, y = cent_M_norm, color = sales_distance)) +
        geom_point(size = 4, shape = 1, stroke = 1) +
        scale_color_gradient2(name = "HHI change", low = royalblue, mid = rgb(94 / 255, 94 / 255, 94 / 255, 0.2, names = NULL, maxColorValue = 1),
            high = royalred, midpoint = 0.5, n.breaks = 5) +
        geom_text_repel(aes(label = plot_desc), color = "black", box.padding = 0.7, size = 4.5, family = "Erewhon") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = graphite) +
        xlab("% in household basket (network-adjusted)") +
        ylab("% in military basket (network-adjusted)") +
        guides(color = FALSE) +
        custom_theme
    ggsave(file.path(STATS_PATH, "calibration", "iotables", "cent_naics_chn.jpeg"), width = 10, height = 10)

    ggplot(data = df, aes(x = cent_C_norm, y = cent_M_norm, color = sales_distance)) +
        geom_point(size = 4, shape = 1, stroke = 1) +
        scale_color_gradient2(name = "HHI change", low = royalblue, mid = rgb(94 / 255, 94 / 255, 94 / 255, 0.2, names = NULL, maxColorValue = 1),
            high = royalred, midpoint = 0.5, n.breaks = 5) +
        geom_text_repel(aes(label = plot_desc), color = "black", box.padding = 0.7, size = 3.9, family = "Erewhon") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = graphite) +
        xlab("% in household basket (network-adjusted)") +
        ylab("% in military basket (network-adjusted)") +
        guides(color = FALSE) +
        custom_theme
    ggsave(file.path(STATS_PATH, "calibration", "iotables", "cent_naics_chn_slides.jpeg"), width = 6, height = 6)
}


plot_industries()

