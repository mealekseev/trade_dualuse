### Plot EU dual-use centrality over time

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(stringr)
library(fixest)
library(latex2exp)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


plot_eu_dualuse <- function() {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)
    cols <- copy(colnames(cent)[startsWith(colnames(cent), "dualuse_2")])

    i <- 1
    coef_list <- list()
    for (col in cols) {
        reg <- feols(as.formula(paste0("rank_C_M_sigma ~ 0 + ", col)), data = cent, se = "hetero")
        se <- sqrt(diag(vcov(reg, se = "hetero")))
        coeftable <- data.table(
            name = col,
            beta = reg$coefficients,
            se = se
        )
        coeftable[, beta_upper := beta + 1.96 * se]
        coeftable[, beta_lower := beta - 1.96 * se]
        coef_list[[i]] <- coeftable
        i <- i + 1
    }
    coef_list <- rbindlist(coef_list)
    coef_list[, year := as.integer(substr(name, 9, 13))]

    ggplot(coef_list[year < 2024], aes(x = year, y = beta)) +
        geom_point(size = 4, shape = 1, stroke = 1, color = royalred) +
        geom_line(linewidth = 1.0, color = royalred) +
        geom_ribbon(aes(ymin = beta_lower, ymax = beta_upper), alpha = 0.2, fill = royalred) +
        scale_y_continuous(labels = scales::percent) +
        geom_vline(xintercept = 2014, linewidth = 1.0, linetype = "dashed") +
        geom_vline(xintercept = 2021, linewidth = 1.0, linetype = "dashed") +
        labs(x = "", y = TeX("Avg percentile $C^M/\\sigma$")) +
        custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "eu", "centrality.jpeg"), width = 10, height = 5.5, dpi = 300)
}


plot_eu_dualuse()

