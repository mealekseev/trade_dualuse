### Plot EU critical lists

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


plot_eu_critical <- function() {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)

    cols <- colnames(cent)[grepl("critical", colnames(cent)) | grepl("economic", colnames(cent))]
    df_list <- list()
    coef_list <- list() 
    i <- 1
    col <- cols[1]
    for (col in cols) {
        print(col)
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
    df <- rbindlist(coef_list)
    df <- df[name %in% c("critical_2024feb_1", "critical_2024feb_2", "critical_2024feb_3", "critical_2024feb_4", "economic_2023oct")]
    df[, place := seq(1, 5)]
    label_list <- list(
        "5" = "Tier 1. Semiconductors (N = 4)",
        "4" = "Tier 2. Radioelectronics (N = 5)",
        "3" = "Tier 3. Navigation & optics (N = 25)",
        "2" = "Tier 4. Manufacturing equipment (N = 16)",
        "1" = "Economically critical (N = 73)"
    )

    ggplot(data = df, aes(x = as.character(6 - place), y = beta)) +
        geom_point(size = 4, shape = 1, stroke = 1, color = royalred) +
        geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), linewidth = 1.0, width = 0.05, color = royalred) +
        scale_x_discrete(labels = label_list) + scale_y_continuous(labels = scales::percent) +
        geom_hline(yintercept = 0.5, linewidth = 1.0, linetype = "dashed") +
        labs(y = TeX("Percentile $C^M/\\sigma$"), x = "") + coord_flip() +
        custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "eu", "critical.jpeg"), width = 10, height = 5.5, dpi = 300)
}


plot_eu_critical()

