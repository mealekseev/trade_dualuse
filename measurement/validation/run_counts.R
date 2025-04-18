### Validate counts

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


get_count_validation <- function() {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)
    for (col in c("gta_export_2018", "gta_export_2020", "gta_export_2022", "gta_security_2018", "gta_security_2020", "gta_security_2022")) {
        cent[, (paste0("du_", col)) := get(col) * dualuse_outcome] 
    }

    cent <- cent[, lapply(.SD, mean),
        .SDcols = c("gta_security_2018", "gta_security_2020", "gta_security_2022", "gta_export_2018", "gta_export_2020", "gta_export_2022",
            "du_gta_security_2018", "du_gta_security_2020", "du_gta_security_2022", "du_gta_export_2018", "du_gta_export_2020", "du_gta_export_2022"),
        by = c("pbin50_C_M_sigma")
    ]
    cent <- cent[order(pbin50_C_M_sigma)]
    cent[, pbin50_C_M_sigma := (pbin50_C_M_sigma - 0.5) / 50]
    cent_copy <- copy(cent)
    cent <- copy(cent_copy)

    for (col in c("gta_security_2018", "gta_security_2020", "gta_security_2022", "gta_export_2018", "gta_export_2020", "gta_export_2022",
            "du_gta_security_2018", "du_gta_security_2020", "du_gta_security_2022", "du_gta_export_2018", "du_gta_export_2020", "du_gta_export_2022")) {
        cent[, (col) := log(get(col))]
        cent <- cent[get(col) != -Inf]
        reg <- feols(as.formula(paste0(col, " ~ pbin50_C_M_sigma")), data = cent, se = "hetero")
        cent[, (paste0("resid_", col)) := predict(reg) - reg$coefficient[["(Intercept)"]]]
        cent[, (col) := get(col) - reg$coefficient[["(Intercept)"]]]
    }

    cent_exp <- melt(cent[, c("pbin50_C_M_sigma", "gta_export_2018", "gta_export_2020", "gta_export_2022")], id.vars = "pbin50_C_M_sigma")
    cent_exp1 <- melt(cent[, c("pbin50_C_M_sigma", "resid_gta_export_2018", "resid_gta_export_2020", "resid_gta_export_2022")], id.vars = "pbin50_C_M_sigma")
    cent_exp[, resid := cent_exp1$value]

    ggplot(data = cent_exp[variable == "gta_export_2018"], aes(x = pbin50_C_M_sigma, y = value, color = variable, fill = variable)) +
        geom_point(aes(y = value), size = 2, shape = 1, stroke = 1, alpha = 0.0) +
        scale_color_manual(name = "period", labels = c("2018-2019"), values = c(graphite)) +
        scale_fill_manual(name = "period", labels = c("2018-2019"), values = c(graphite)) +
        ylim(-1, 1) +
        scale_x_continuous(labels = scales::percent) +
        xlab(TeX("Percentile $C^M/\\sigma$")) + ylab("log(announcements)") +
        custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "validation", "count_anim0.jpeg"), width = 6, height = 6, dpi = 300)

    ggplot(data = cent_exp[variable == "gta_export_2018"], aes(x = pbin50_C_M_sigma, y = value, color = variable, fill = variable)) +
        geom_point(aes(y = value), size = 2, shape = 1, stroke = 1) +
        geom_smooth(method = "lm", formula = y ~ 0 + x, se = TRUE) +
        scale_color_manual(name = "period", labels = c("2018-2019"), values = c(graphite)) +
        scale_fill_manual(name = "period", labels = c("2018-2019"), values = c(graphite)) +
        ylim(-1, 1) +
        scale_x_continuous(labels = scales::percent) +
        xlab(TeX("Percentile $C^M/\\sigma$")) + ylab("log(announcements)") +
        custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "validation", "count_anim1.jpeg"), width = 6, height = 6, dpi = 300)

    ggplot(data = cent_exp[variable %in% c("gta_export_2018", "gta_export_2020")],
            aes(x = pbin50_C_M_sigma, y = value, color = variable, fill = variable)) +
        geom_point(aes(y = value), size = 2, shape = 1, stroke = 1) +
        geom_smooth(method = "lm", formula = y ~ 0 + x, se = TRUE) +
        scale_color_manual(name = "period", labels = c("2018-2019", "2020-2021"), values = c(lightblue, royalblue)) +
        scale_fill_manual(name = "period", labels = c("2018-2019", "2020-2021"), values = c(lightblue, royalblue)) +
        ylim(-1, 1) +
        scale_x_continuous(labels = scales::percent) +
        xlab(TeX("Percentile $C^M/\\sigma$")) + ylab("log(announcements)") +
        custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "validation", "count_anim2.jpeg"), width = 6, height = 6, dpi = 300)

    ggplot(data = cent_exp, aes(x = pbin50_C_M_sigma, y = value, color = variable, fill = variable)) +
        geom_point(aes(y = value), size = 2, shape = 1, stroke = 1) +
        geom_smooth(method = "lm", formula = y ~ 0 + x, se = TRUE) +
        scale_color_manual(name = "period", labels = c("2018-2019", "2020-2021", "2022-2023"), values = c(lightblue, royalblue, royalred)) +
        scale_fill_manual(name = "period", labels = c("2018-2019", "2020-2021", "2022-2023"), values = c(lightblue, royalblue, royalred)) +
        ylim(-1, 1) +
        scale_x_continuous(labels = scales::percent) +
        xlab(TeX("Percentile $C^M/\\sigma$")) + ylab("log(announcements)") +
        custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "validation", "count.jpeg"), width = 6, height = 6, dpi = 300)

    ggplot(data = cent_exp, aes(x = pbin50_C_M_sigma, y = value, color = variable, fill = variable)) +
        geom_point(aes(y = value), size = 4, shape = 1, stroke = 1) +
        geom_smooth(method = "lm", formula = y ~ 0 + x, se = TRUE) +
        scale_color_manual(name = "period", labels = c("2018-2019", "2020-2021", "2022-2023"), values = c(lightblue, royalblue, royalred)) +
        scale_fill_manual(name = "period", labels = c("2018-2019", "2020-2021", "2022-2023"), values = c(lightblue, royalblue, royalred)) +
        ylim(-1, 1) +
        scale_x_continuous(labels = scales::percent) +
        xlab(TeX("Percentile $C^M/\\sigma$")) + ylab("log(announcements)") +
        custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "validation", "count_large.jpeg"), width = 10, height = 10, dpi = 300)
}


get_count_validation()

