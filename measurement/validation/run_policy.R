### Validate outcomes against centrality

rm(list=ls())
gc()

library(data.table)
library(ggrepel)
library(fixest)
library(xtable)
library(stringr)
library(latex2exp)
library(cowplot)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


get_rank_validation <- function() {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)
    cent[, wgt := 1]

    cols <- c("S_M", "C_M", "C_M_sigma")
    for (v in cols) {
        reg <- feols(
            as.formula(paste0("dualuse_outcome ~ 1 + rank_", v, " + rank_", v, "^2 + rank_", v, "^3")),
            data = cent,
            weights = cent$wgt
        )
        cent[wgt > 0, (paste0("resid_", v)) := predict(reg)]
        cent[, (paste0("yvar_", v)) := weighted.mean(dualuse_outcome, w = wgt), by = c(paste0("pbin50_", v))]   
        cent[, (paste0("pbin50_", v)) := get(paste0("pbin50_", v)) / 50 - 0.5 / 50]
        reg_fit <- feols(
            as.formula(paste0("yvar_", v, " ~ 0 + resid_", v)),
            data = cent,
            weights = cent$wgt
        )
        cent[, (paste0("r2_", v)) := r2(reg_fit)[2]]
        cent[, (paste0("r2_str_", v)) := paste0("R$^2$ = ", format(round(get(paste0("r2_", v)), 2), nsmall = 2))]

        reg <- feols(
            as.formula(paste0("gta_export_usa_post22 ~ 1 + rank_", v, " + rank_", v, "^2 + rank_", v, "^3")),
            data = cent,
            weights = cent$wgt
        )
        cent[wgt > 0, (paste0("resid_gta_", v)) := predict(reg)]
        cent[, (paste0("yvar_gta_", v)) := weighted.mean(gta_export_usa_post22, w = wgt), by = c(paste0("pbin50_", v))]   
        reg_fit <- feols(
            as.formula(paste0("yvar_gta_", v, " ~ 0 + resid_gta_", v)),
            data = cent,
            weights = cent$wgt
        )
        cent[, (paste0("r2_gta_", v)) := r2(reg_fit)[2]]
        cent[, (paste0("r2_gta_str_", v)) := paste0("R$^2$ = ", format(round(get(paste0("r2_gta_", v)), 2), nsmall = 2))]
    }

    ymax <- 0.64
    p1_empty <- ggplot(data = cent, aes(x = rank_S_M, y = resid_S_M, color = "C_M_sigma")) +
        geom_line(linewidth = 1.0, alpha = 0.0) + ylim(-0.01, ymax) +
        scale_y_continuous(labels = scales::percent, limits = c(-0.01, ymax)) + scale_x_continuous(labels = scales::percent) +
        xlab(TeX("Percentile $S^M$")) + ylab("") + labs(title = "On dual-use list, %") +
        custom_theme + theme(legend.position = "none", plot.title.position = "plot")
    p2_empty <- ggplot(data = cent, aes(x = rank_S_M, y = resid_gta_S_M, color = "C_M_sigma")) +
        geom_line(linewidth = 1.0, alpha = 0.0) + ylim(-0.01, ymax) +
        geom_point(aes(x = pbin50_C_M_sigma, y = yvar_gta_C_M_sigma), size = 2, shape = 1, stroke = 1, alpha = 0.0,
            color = rgb(51 / 255, 71 / 255, 106 / 255, alpha = 0.00, maxColorValue = 1)) +
        scale_color_manual(name = "Sorting", labels = c(TeX("Percentile $C^M/\\sigma$")), values = c(royalblue)) +
        xlab(TeX("Percentile $S^M$")) + ylab("") + labs(title = "US export NTMs after 2022, %") +
        scale_y_continuous(labels = scales::percent, limits = c(-0.01, ymax)) + scale_x_continuous(labels = scales::percent) +
        custom_theme + theme(legend.position = "none", plot.title.position = "plot")
    p_together_empty <- plot_grid(p1_empty, p2_empty, ncol = 2)
    p_together_empty
    ggsave(file.path(STATS_PATH, "measurement", "validation", "rank_empty.jpeg"), width = 10, height = 5.5, dpi = 300)

    p1_shares <- p1_empty +
        geom_line(linewidth = 1.0) + ylim(-0.01, ymax) +
        geom_point(aes(x = pbin50_S_M, y = yvar_S_M), size = 2, shape = 1, stroke = 1,
            color = rgb(51 / 255, 71 / 255, 106 / 255, alpha = 0.0025, maxColorValue = 1)) +
        scale_y_continuous(labels = scales::percent, limits = c(-0.01, ymax)) +
        scale_color_manual(name = "Sorting", labels = c(TeX("$S^M$")), values = c(royalred)) +
        geom_text(x = 0, y = ymax, label = TeX(cent[1, "r2_str_S_M"][[1]]), hjust = 0, color = royalred, size = 6, family = "Erewhon") +
        custom_theme + theme(legend.position = "none",
            plot.title.position = "plot",
        )
    p2_shares <- p2_empty +
        geom_line(linewidth = 1.0) + ylim(-0.01, ymax) +
        geom_point(aes(x = pbin50_S_M, y = yvar_gta_S_M), size = 2, shape = 1, stroke = 1,
            color = rgb(51 / 255, 71 / 255, 106 / 255, alpha = 0.0025, maxColorValue = 1)) +
        scale_y_continuous(labels = scales::percent, limits = c(-0.01, ymax)) +
        scale_color_manual(name = "Sorting", labels = c(TeX("$S^M$")), values = c(royalred)) +
        geom_text(x = 0, y = ymax, label = TeX(cent[1, "r2_gta_str_S_M"][[1]]), hjust = 0, color = royalred, size = 6, family = "Erewhon") +
        custom_theme + theme(legend.position = "none",
            plot.title.position = "plot",
        )
    p_together_shares <- plot_grid(p1_shares, p2_shares, ncol = 2)
    p_together_shares
    ggsave(file.path(STATS_PATH, "measurement", "validation", "rank_shares.jpeg"), width = 10, height = 5.5, dpi = 300)

    p1_cent <- ggplot(data = cent, aes(x = rank_C_M, y = resid_C_M, color = "C_M")) +
        geom_line(linewidth = 1.0) + ylim(-0.01, ymax) +
        geom_point(aes(x = pbin50_C_M, y = yvar_C_M), size = 2, shape = 1, stroke = 1,
            color = rgb(51 / 255, 71 / 255, 106 / 255, alpha = 0.0025, maxColorValue = 1)) +
        scale_color_manual(name = "Sorting", labels = c(TeX("$C^M$")), values = c(royalblue)) +
        geom_line(aes(x = rank_S_M, y = resid_S_M, color = "new1"), linewidth = 1.0) +
        geom_line(aes(x = rank_C_M, y = resid_C_M, color = "new2"), linewidth = 1.0) +
        scale_color_manual(name = "Sorting", labels = c("new2" = TeX("$C^M$"), "new1" = TeX("$S^M$")),
            values = c("new2" = royalblue, "new1" =  rgb(120 / 255, 0 / 255, 15 / 255, 0.3, names = NULL, maxColorValue = 1))) +
        xlab(TeX("Percentile $C^M$")) + ylab("") + labs(title = "On dual-use list, %") +
        scale_y_continuous(labels = scales::percent, limits = c(-0.01, ymax)) + scale_x_continuous(labels = scales::percent) +
        custom_theme + theme(legend.position = "none",
            plot.title.position = "plot",
        ) + geom_text(x = 0, y = ymax, label = TeX(cent[1, "r2_str_C_M"][[1]]), hjust = 0, color = royalblue, size = 6, family = "Erewhon")    
    p2_cent <- ggplot(data = cent, aes(x = rank_C_M, y = resid_gta_C_M, color = "C_M")) +
        geom_line(linewidth = 1.0) + ylim(-0.01, ymax) +
        geom_point(aes(x = pbin50_C_M, y = yvar_gta_C_M), size = 2, shape = 1, stroke = 1,
            color = rgb(51 / 255, 71 / 255, 106 / 255, alpha = 0.0025, maxColorValue = 1)) +
        scale_color_manual(name = "Sorting", labels = c(TeX("$C^M$")), values = c(royalblue)) +
        geom_line(aes(x = rank_S_M, y = resid_gta_S_M, color = "new1"), linewidth = 1.0) +
        geom_line(aes(x = rank_C_M, y = resid_gta_C_M, color = "new2"), linewidth = 1.0) +
        scale_color_manual(name = "Sorting", labels = c("new2" = TeX("$C^M$"), "new1" = TeX("$S^M$")),
            values = c("new2" = royalblue, "new1" =  rgb(120 / 255, 0 / 255, 15 / 255, 0.3, names = NULL, maxColorValue = 1))) +
        xlab(TeX("Percentile $C^M$")) + ylab("") + labs(title = "US export NTMs after 2022, %") +
        scale_y_continuous(labels = scales::percent, limits = c(-0.01, ymax)) + scale_x_continuous(labels = scales::percent) +
        custom_theme + theme(legend.position = "none",
            plot.title.position = "plot",
        ) + geom_text(x = 0, y = ymax, label = TeX(cent[1, "r2_gta_str_C_M"][[1]]), hjust = 0, color = royalblue, size = 6, family = "Erewhon")
    p_together_cent <- plot_grid(p1_cent, p2_cent, ncol = 2)
    ggsave(file.path(STATS_PATH, "measurement", "validation", "rank_cent.jpeg"), width = 10, height = 5.5, dpi = 300)


    p1 <- ggplot(data = cent, aes(x = rank_C_M_sigma, y = resid_C_M_sigma, color = "C_M_sigma")) +
        geom_line(linewidth = 1.0) + ylim(-0.01, ymax) +
        geom_point(aes(x = pbin50_C_M_sigma, y = yvar_C_M_sigma), size = 2, shape = 1, stroke = 1,
            color = rgb(51 / 255, 71 / 255, 106 / 255, alpha = 0.0025, maxColorValue = 1)) +
        scale_color_manual(name = "Sorting", labels = c(TeX("$C^M/\\sigma$")), values = c(royalblue)) +
        xlab(TeX("Percentile $C^M/\\sigma$")) + ylab("") + labs(title = "On dual-use list, %") +
        geom_line(aes(x = rank_S_M, y = resid_S_M, color = "new1"), linewidth = 1.0) +
        geom_line(aes(x = rank_C_M, y = resid_C_M, color = "new2"), linewidth = 1.0) +
        geom_line(aes(x = rank_C_M_sigma, y = resid_C_M_sigma), linewidth = 1.0, color = royalblue) +
        scale_color_manual(name = "Sorting", labels = c("C_M_sigma" = TeX("$C^M/\\sigma$"), "new2" = TeX("$C^M$"), "new1" = TeX("$S^M$")),
            values = c("C_M_sigma" = royalblue, "new2" = lightblue, "new1" =  rgb(120 / 255, 0 / 255, 15 / 255, 0.3, names = NULL, maxColorValue = 1))) +
        scale_y_continuous(labels = scales::percent, limits = c(-0.01, ymax)) + scale_x_continuous(labels = scales::percent) +
        custom_theme + theme(legend.position = "none",
            plot.title.position = "plot",
        ) + geom_text(x = 0, y = ymax, label = TeX(cent[1, "r2_str_C_M_sigma"][[1]]), hjust = 0, color = royalblue, size = 6, family = "Erewhon")
    p2 <- ggplot(data = cent, aes(x = rank_C_M_sigma, y = resid_gta_C_M_sigma, color = "C_M_sigma")) +
        geom_line(linewidth = 1.0) + ylim(-0.01, ymax) +
        geom_point(aes(x = pbin50_C_M_sigma, y = yvar_gta_C_M_sigma), size = 2, shape = 1, stroke = 1,
            color = rgb(51 / 255, 71 / 255, 106 / 255, alpha = 0.0025, maxColorValue = 1)) +
        scale_color_manual(name = "Sorting", labels = c(TeX("Percentile $C^M/\\sigma$")), values = c(royalblue)) +
        xlab(TeX("Percentile $C^M/\\sigma$")) + ylab("") + labs(title = "US export NTMs after 2022, %") +
        scale_y_continuous(labels = scales::percent, limits = c(-0.01, ymax)) + scale_x_continuous(labels = scales::percent) +
        geom_line(aes(x = rank_S_M, y = resid_gta_S_M, color = "new1"), linewidth = 1.0) +
        geom_line(aes(x = rank_C_M, y = resid_gta_C_M, color = "new2"), linewidth = 1.0) +
        geom_line(aes(x = rank_C_M_sigma, y = resid_gta_C_M_sigma), linewidth = 1.0, color = royalblue) +
        scale_color_manual(name = "Sorting", labels = c("C_M_sigma" = TeX("$C^M/\\sigma$"), "new2" = TeX("$C^M$"), "new1" = TeX("$S^M$")),
            values = c("C_M_sigma" = royalblue, "new2" = lightblue, "new1" =  rgb(120 / 255, 0 / 255, 15 / 255, 0.3, names = NULL, maxColorValue = 1))) +
        custom_theme + theme(legend.position = "none",
            plot.title.position = "plot",
        ) + geom_text(x = 0, y = ymax, label = TeX(cent[1, "r2_gta_str_C_M_sigma"][[1]]), hjust = 0, color = royalblue, size = 6, family = "Erewhon")
    p_together <- plot_grid(p1, p2, ncol = 2)
    p_together    
    ggsave(file.path(STATS_PATH, "measurement", "validation", "rank_use.jpeg"), width = 10, height = 5.5, dpi = 300)
}


get_rank_validation()

