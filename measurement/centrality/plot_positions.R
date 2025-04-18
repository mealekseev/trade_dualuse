### Output upstream analysis

rm(list=ls())
gc()

library(data.table)
library(fixest)
library(xtable)
library(cowplot)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


plot_positions <- function() {
    coeflist <- list()

    df <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"))
    df <- df[X != 0]
    df[, cent_C_norm := cent_C / sum(cent_C) * 100]
    df[, cent_M_norm := cent_M / sum(cent_M) * 100]
    df[, upstream := (X - E) / X]
    df[, upstream_bin := cut(upstream, breaks = seq(0, 1, 0.2), include.lowest = TRUE, include.high = TRUE)]
    df[, heuristic := 1 - abs(cent_M_norm - cent_C_norm) / (cent_M_norm + cent_C_norm)]
    df[, heuristic_bin := cut(heuristic, breaks = seq(0, 1, 0.2), include.lowest = TRUE, include.high = TRUE)]
    df[, military := cent_M_norm / (cent_M_norm + cent_C_norm)]
    df[, military_bin := cut(military, breaks = seq(0, 1, 0.2), include.lowest = TRUE, include.high = TRUE)]

    df[, wgt_no := 1]
    df[, wgt_yes := perc_value]
    ix <- 1
    for (wgt  in c("wgt_no", "wgt_yes")) {
        # upstream heuristic
        reg <- feols(heuristic ~ 0 + factor(upstream_bin), data = df, weights = df[[wgt]])
        coef <- reg$coefficients
        se <- sqrt(diag(vcov(reg, type = "hetero")))
        coeft <- data.table(
            spec = "upstream_heuristic",
            wgtvar = wgt,
            line = "upstream",
            point = seq(0.1, 1.0, 0.2),
            coef = coef,
            se = se
        )
        coeft[, beta_upper := coef + 1.96 * se]
        coeft[, beta_lower := coef - 1.96 * se]
        coeflist[[ix]] <- copy(coeft)
        ix <- ix + 1

        reg <- feols(heuristic ~ 0 + factor(military_bin), data = df, weights = df[[wgt]])
        coef <- reg$coefficients
        se <- sqrt(diag(vcov(reg, type = "hetero")))
        coeft <- data.table(
            spec = "upstream_heuristic",
            wgtvar = wgt,
            line = "military",
            point = seq(0.1, 1.0, 0.2),
            coef = coef,
            se = se
        )
        coeft[, beta_upper := coef + 1.96 * se]
        coeft[, beta_lower := coef - 1.96 * se]
        coeflist[[ix]] <- copy(coeft)
        ix <- ix + 1

        # specialization-dualuse
        reg <- feols(dualuse_2018 ~ 0 + factor(military_bin), data = df, weights = df[[wgt]])
        coef <- reg$coefficients
        se <- sqrt(diag(vcov(reg, type = "hetero")))
        coeft <- data.table(
            spec = "military_dualuse",
            wgtvar = wgt,
            line = "military",
            point = seq(0.1, 1.0, 0.2),
            coef = coef,
            se = se
        )
        coeft[, beta_upper := coef + 1.96 * se]
        coeft[, beta_lower := coef - 1.96 * se]
        coeflist[[ix]] <- copy(coeft)
        ix <- ix + 1

        reg <- feols(dualuse_2018 ~ 0 + factor(heuristic_bin), data = df, weights = df[[wgt]])
        coef <- reg$coefficients
        se <- sqrt(diag(vcov(reg, type = "hetero")))
        coeft <- data.table(
            spec = "military_dualuse",
            wgtvar = wgt,
            line = "heuristic",
            point = seq(0.1, 1.0, 0.2),
            coef = coef,
            se = se
        )
        coeft[, beta_upper := coef + 1.96 * se]
        coeft[, beta_lower := coef - 1.96 * se]
        coeflist[[ix]] <- copy(coeft)
        ix <- ix + 1

        # upstream-dualuse
        reg <- feols(dualuse_2018 ~ 0 + factor(upstream_bin), data = df, weights = df[[wgt]])
        coef <- reg$coefficients
        se <- sqrt(diag(vcov(reg, type = "hetero")))
        coeft <- data.table(
            spec = "upstream_dualuse",
            wgtvar = wgt,
            line = "military",
            point = seq(0.1, 1.0, 0.2),
            coef = coef,
            se = se
        )
        coeft[, beta_upper := coef + 1.96 * se]
        coeft[, beta_lower := coef - 1.96 * se]
        coeflist[[ix]] <- copy(coeft) 
        ix <- ix + 1
    }
    coeft <- rbindlist(coeflist)

    wgt <- "wgt_yes"
    for (wgt in c("wgt_no", "wgt_yes")) { 
        # one line
        p1 <- ggplot(coeft[spec == "upstream_heuristic" & line == "upstream" & wgtvar == wgt], aes(x = point, y = coef)) +
            geom_point(size = 4, shape = 1, stroke = 1, color = royalblue) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), width = 0.01, color = royalblue, linewidth = 1.0) +
            xlim(-1.0, 1.1) +
            scale_x_continuous(breaks = seq(-0.0, 1.01, 0.2), labels = scales::percent(seq(0, 1, 0.2))) +
            xlab("Intermediate sales") + ylab("") + labs(title = "Military-HH sales symmetry") +
            custom_theme + theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 16),
                plot.title.position = "plot",
            )
        p1

        p2 <- ggplot(coeft[spec == "military_dualuse" & line == "military" & wgtvar == wgt], aes(x = point, y = coef)) +
            geom_point(size = 4, shape = 1, stroke = 1, color = royalred) +
            geom_line(linewidth = 1.0, color = royalred) +
            geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), width = 0.01, color = royalred, linewidth = 1.0) +
            xlim(-1.0, 1.1) + scale_y_continuous(labels = scales::percent) +
            scale_x_continuous(breaks = seq(-0.0, 1.01, 0.2), labels = scales::percent(seq(0, 1, 0.2))) +
            xlab("Military sales") + ylab("") + labs(title = "On dual-use list") +
            custom_theme + theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 16),
                plot.title.position = "plot",
            )
        p2

        p3 <- ggplot(coeft[spec == "upstream_dualuse" & line == "military" & wgtvar == wgt], aes(x = point, y = coef)) +
            geom_point(size = 4, shape = 1, stroke = 1, color = graphite) +
            geom_line(linewidth = 1.0, color = graphite) +
            geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), width = 0.01, color = graphite, linewidth = 1.0) +
            xlim(-1.0, 1.1) + scale_y_continuous(labels = scales::percent) +
            scale_x_continuous(breaks = seq(-0.0, 1.01, 0.2), labels = scales::percent(seq(0, 1, 0.2))) +
            xlab("Intermediate sales") + ylab("") + labs(title = "On dual-use list") +
            custom_theme + theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 16),
                plot.title.position = "plot",
            )
        p3
        p_together <- plot_grid(p1, p2, p3, ncol = 3)
        p_together    
        ggsave(file.path(STATS_PATH, "motivation", "dualuse", paste0("position_", wgt, ".jpeg")), width = 10, height = 5.5, dpi = 300)

        # no weights -- two line
        p1
        p1 <- p1 + custom_theme + geom_line(data = coeft[spec == "upstream_heuristic" & line == "military" & wgtvar == wgt], linewidth = 1.0, color = lightblue) +
            annotate("text", x = 0.8, y = 0.32, label = "Military sales",
                family = "Erewhon", hjust = 0.5, vjust = 0.5, size = 5, color = graphite) +
            xlab("Intermediate sales (Military sales)")

        p2
        p2 <- p2 + geom_line(data = coeft[spec == "military_dualuse" & line == "heuristic" & wgtvar == wgt], linewidth = 1.0, color = lightblue) +
            annotate("text", x = 0.8, y = 0.22, label = "Military-HH sales symmetry",
                family = "Erewhon", hjust = 0.5, vjust = 0.5, size = 5, color = graphite) +
            xlab("Military sales (Military-HH sales symmetry)")

        p3
        p_together <- plot_grid(p1, p2, p3, ncol = 3)
        p_together    
        ggsave(file.path(STATS_PATH, "measurement", "centrality", paste0("position_alt_", wgt, ".jpeg")), width = 10, height = 5.5, dpi = 300)


        # SLIDE VERSION
        # one line
        p1 <- ggplot(coeft[spec == "upstream_heuristic" & line == "upstream" & wgtvar == wgt], aes(x = point, y = coef)) +
            geom_point(size = 4, shape = 1, stroke = 1, color = royalblue) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), width = 0.01, color = royalblue, linewidth = 1.0) +
            xlim(-1.0, 1.1) +
            scale_x_continuous(breaks = seq(-0.0, 1.01, 0.2), labels = scales::percent(seq(0, 1, 0.2))) +
            xlab("Intermediate sales") + ylab("") + labs(title = "Military-HH sales symmetry") +
            custom_theme + theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 18),
                plot.title.position = "plot",
            )
        p1
        p1_empty <- ggplot(coeft[spec == "upstream_heuristic" & line == "upstream" & wgtvar == wgt], aes(x = point, y = coef)) +
            geom_point(size = 4, shape = 1, stroke = 1, color = royalblue, alpha = 0.0) +
            geom_line(linewidth = 1.0, color = royalblue, alpha = 0.0) +
            geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), width = 0.01, color = royalblue, linewidth = 1.0, alpha = 0.0) +
            xlim(-1.0, 1.1) +
            scale_x_continuous(breaks = seq(-0.0, 1.01, 0.2), labels = scales::percent(seq(0, 1, 0.2))) +
            xlab("Intermediate sales") + ylab("") + labs(title = "Military-HH sales symmetry") +
            custom_theme + theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 18),
                plot.title.position = "plot",
            )

        p2 <- ggplot(coeft[spec == "military_dualuse" & line == "military" & wgtvar == wgt], aes(x = point, y = coef)) +
            geom_point(size = 4, shape = 1, stroke = 1, color = royalred) +
            geom_line(linewidth = 1.0, color = royalred) +
            geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), width = 0.01, color = royalred, linewidth = 1.0) +
            xlim(-1.0, 1.1) + scale_y_continuous(labels = scales::percent) +
            scale_x_continuous(breaks = seq(-0.0, 1.01, 0.2), labels = scales::percent(seq(0, 1, 0.2))) +
            xlab("Military sales") + ylab("") + labs(title = "On dual-use list") +
            custom_theme + theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 18),
                plot.title.position = "plot",
            )
        p2
        p2_empty <- ggplot(coeft[spec == "military_dualuse" & line == "military" & wgtvar == wgt], aes(x = point, y = coef)) +
            geom_point(size = 4, shape = 1, stroke = 1, color = royalred, alpha = 0.0) +
            geom_line(linewidth = 1.0, color = royalred, alpha = 0.0) +
            geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), width = 0.01, color = royalred, linewidth = 1.0, alpha = 0.0) +
            xlim(-1.0, 1.1) + scale_y_continuous(labels = scales::percent) +
            scale_x_continuous(breaks = seq(-0.0, 1.01, 0.2), labels = scales::percent(seq(0, 1, 0.2))) +
            xlab("Military sales") + ylab("") + labs(title = "On dual-use list") +
            custom_theme + theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 18),
                plot.title.position = "plot",
            )

        p3 <- ggplot(coeft[spec == "upstream_dualuse" & line == "military" & wgtvar == wgt], aes(x = point, y = coef)) +
            geom_point(size = 4, shape = 1, stroke = 1, color = graphite) +
            geom_line(linewidth = 1.0, color = graphite) +
            geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), width = 0.01, color = graphite, linewidth = 1.0) +
            xlim(-1.0, 1.1) + scale_y_continuous(labels = scales::percent) +
            scale_x_continuous(breaks = seq(-0.0, 1.01, 0.2), labels = scales::percent(seq(0, 1, 0.2))) +
            xlab("Intermediate sales") + ylab("") + labs(title = "On dual-use list") +
            custom_theme + theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 18),
                plot.title.position = "plot",
            )
        p3
        p3_empty <- ggplot(coeft[spec == "upstream_dualuse" & line == "military" & wgtvar == wgt], aes(x = point, y = coef)) +
            geom_point(size = 4, shape = 1, stroke = 1, color = graphite, alpha = 0.0) +
            geom_line(linewidth = 1.0, color = graphite, alpha = 0.0) +
            geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), width = 0.01, color = graphite, linewidth = 1.0, alpha = 0.0) +
            xlim(-1.0, 1.1) + scale_y_continuous(labels = scales::percent) +
            scale_x_continuous(breaks = seq(-0.0, 1.01, 0.2), labels = scales::percent(seq(0, 1, 0.2))) +
            xlab("Intermediate sales") + ylab("") + labs(title = "On dual-use list") +
            custom_theme + theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 18),
                plot.title.position = "plot",
            )
        
        p_together <- plot_grid(p1, p2, p3, ncol = 3)
        p_together    
        ggsave(file.path(STATS_PATH, "measurement", "centrality", paste0("position_", wgt, ".jpeg")), width = 10, height = 5.5, dpi = 300)

        p_together <- plot_grid(p1_empty, p2_empty, p3_empty, ncol = 3)
        ggsave(file.path(STATS_PATH, "measurement", "centrality", paste0("position_", wgt, "_anim1.jpeg")), width = 10, height = 5.5, dpi = 300)

        p_together <- plot_grid(p1, p2_empty, p3_empty, ncol = 3)
        ggsave(file.path(STATS_PATH, "measurement", "centrality", paste0("position_", wgt, "_anim2.jpeg")), width = 10, height = 5.5, dpi = 300)

        p_together <- plot_grid(p1, p2, p3_empty, ncol = 3)
        ggsave(file.path(STATS_PATH, "measurement", "centrality", paste0("position_", wgt, "_anim3.jpeg")), width = 10, height = 5.5, dpi = 300)
    }
}


plot_positions()

