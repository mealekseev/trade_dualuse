### Summarize trade over time

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(stringr)
library(scales)
library(fixest)
library(cowplot)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


plot_counties <- function() {
    coef_list <- list()
    ix <- 1
    for (year in 1995:2022) {
        print(year)
        coef_table <- fread(file.path(OUTPUT_PATH, "trade_patterns", "hs", paste0(year, ".csv")), keepLeadingZeros = TRUE)
        coef_list[[ix]] <- coef_table
        ix <- ix + 1
    }
    coeftable_hs <- rbindlist(coef_list)
    coeftable_hs[, se := sqrt(se)]
    coeftable_hs[, coef_lower := coef - 1.96 * se]
    coeftable_hs[, coef_upper := coef + 1.96 * se]

    coef_list <- list()
    ix <- 1
    for (year in 1962:2021) {
        print(year)
        coef_table <- fread(file.path(OUTPUT_PATH, "trade_patterns", "sitc", paste0(year, ".csv")), keepLeadingZeros = TRUE)
        coef_list[[ix]] <- coef_table
        ix <- ix + 1
    }
    coeftable_sitc <- rbindlist(coef_list)
    coeftable_sitc[, se := sqrt(se)]
    coeftable_sitc[, coef_lower := coef - 1.96 * se]
    coeftable_sitc[, coef_upper := coef + 1.96 * se]

    p <- ggplot(coeftable_sitc[importer_id == "IRQ" & type == "i" & cent == "pct"], aes(x = year, y = coef)) +
        geom_line(linewidth = 1.0, color = royalblue) +
        geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
        geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
        geom_vline(xintercept = 1975, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 1990, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 2001, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 2014, linetype = "dashed", color = "black") +
        geom_text(x = 1975.5, hjust = 0, y = 0.37, label = "Iran-Iraq War", family = "Erewhon", size = 5) + 
        geom_text(x = 1990.5, hjust = 0, y = 0.57, label = "Gulf War", family = "Erewhon", size = 5) + 
        geom_text(x = 2001.5, hjust = 0, y = 0.57, label = "War on Terror", family = "Erewhon", size = 5) + 
        geom_text(x = 2014.5, hjust = 0, y = 0.57, label = "ISIS Threat", family = "Erewhon", size = 5) + 
        scale_y_continuous(labels = scales::percent) +
        labs(x = "", y = "Avg. mil. use pct") +
        custom_theme_slides
    ggsave(file.path(STATS_PATH, "trade_patterns", "iraq.jpeg"), p, width = 10, height = 4, dpi = 300)
    ggsave(file.path(STATS_PATH, "trade_patterns", "iraq_slides.jpeg"), width = 10, height = 5.5, dpi = 300)

    p1 <- ggplot(coeftable_sitc[exporter_id == "TWN" & type == "e" & cent == "pct"], aes(x = year, y = coef)) +
        geom_line(linewidth = 1.0, color = royalblue) +
        geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
        geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
        labs(x = "", y = "Taiwan, exports") +
        scale_y_continuous(labels = scales::percent) +
        custom_theme_slides
    p2 <- ggplot(coeftable_hs[exporter_iso3 == "DEU" & importer_iso3 == "RUS" & type == "p" & cent == "pct"], aes(x = year, y = coef)) +
        geom_line(linewidth = 1.0, color = royalblue) +
        geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
        geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
        labs(x = "", y = "Germany-Russia") +
        scale_y_continuous(labels = scales::percent) +
        custom_theme_slides
    p3 <- ggplot(coeftable_sitc[exporter_id == "FRA" & importer_id == "MAR" & type == "p" & cent == "pct"], aes(x = year, y = coef)) +
        geom_line(linewidth = 1.0, color = royalblue) +
        geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
        geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
        labs(x = "", y = "France-Morocco") +
        scale_y_continuous(labels = scales::percent) +
        custom_theme_slides
    p4 <- ggplot(coeftable_sitc[exporter_id == "USA" & importer_id == "SAU" & type == "p" & cent == "pct"], aes(x = year, y = coef)) +
        geom_line(linewidth = 1.0, color = royalblue) +
        geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
        geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
        labs(x = "", y = "U.S.-Saudi Arabia") +
        scale_y_continuous(labels = scales::percent) +
        custom_theme_slides
    p5 <- ggplot(coeftable_sitc[importer_id == "LBY" & type == "i" & cent == "pct"], aes(x = year, y = coef)) +
        geom_line(linewidth = 1.0, color = royalblue) +
        geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
        geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
        labs(x = "", y = "Libya, imports") +
        scale_y_continuous(labels = scales::percent) +
        custom_theme_slides
    p6 <- ggplot(coeftable_sitc[importer_id == "YUG" & type == "i" & cent == "pct"], aes(x = year, y = coef)) +
        geom_line(linewidth = 1.0, color = royalblue) +
        geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
        geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
        labs(x = "", y = "Yugoslavia, imports") +
        scale_y_continuous(labels = scales::percent) +
        custom_theme_slides
    p7 <- ggplot(coeftable_sitc[importer_id == "ISR" & type == "i" & cent == "pct"], aes(x = year, y = coef)) +
        geom_line(linewidth = 1.0, color = royalblue) +
        geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
        geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
        labs(x = "", y = "Israel, imports") +
        scale_y_continuous(labels = scales::percent) +
        custom_theme_slides

    plot_grid(p1, p2, p3, p4, ncol = 1, rel_heights = c(0.25, 0.25, 0.25, 0.25))
    ggsave(file.path(STATS_PATH, "trade_patterns", "trade_patterns1.jpeg"), width = 10, height = 12, dpi = 300)
    plot_grid(p5, p6, p7, ncol = 1, rel_heights = c(0.33, 0.33, 0.33))
    ggsave(file.path(STATS_PATH, "trade_patterns", "trade_patterns2.jpeg"), width = 10, height = 12, dpi = 300)

    ggsave(file.path(STATS_PATH, "trade_patterns", "trade_patterns1_slides.jpeg"), p1, width = 10, height = 5.5, dpi = 300)
    ggsave(file.path(STATS_PATH, "trade_patterns", "trade_patterns2_slides.jpeg"), p2, width = 10, height = 5.5, dpi = 300)
    ggsave(file.path(STATS_PATH, "trade_patterns", "trade_patterns3_slides.jpeg"), p3, width = 10, height = 5.5, dpi = 300)
    ggsave(file.path(STATS_PATH, "trade_patterns", "trade_patterns4_slides.jpeg"), p4, width = 10, height = 5.5, dpi = 300)
    ggsave(file.path(STATS_PATH, "trade_patterns", "trade_patterns5_slides.jpeg"), p5, width = 10, height = 5.5, dpi = 300)
    ggsave(file.path(STATS_PATH, "trade_patterns", "trade_patterns6_slides.jpeg"), p6, width = 10, height = 5.5, dpi = 300)
    ggsave(file.path(STATS_PATH, "trade_patterns", "trade_patterns7_slides.jpeg"), p7, width = 10, height = 5.5, dpi = 300)
}


plot_counties()

