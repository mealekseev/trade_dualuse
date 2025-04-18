### Run policy robustness

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


get_rank_plot <- function(
        subcontracts = FALSE, four_digit = FALSE, nosmall = FALSE, gta = FALSE, weights = FALSE, elasticity = ""
    ) {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)

    # four_digit <- TRUE
    suffix <- ""
    if (four_digit) {
        cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12_4digit.csv"), keepLeadingZeros = TRUE)
        suffix <- "_4digit"
    }

    # nosmall <- TRUE
    cent[, weights_rank := rank(cent_M + cent_C) / nrow(cent)]
    if (nosmall) {
        cent <- cent[weights_rank > 0.05]
        suffix <- paste0(suffix, "_nosmall")
    }

    df_list <- list()
    i <- 1
    if (subcontracts) {
        cols <- colnames(cent)[(grepl("S_M", colnames(cent)) | grepl("C_M", colnames(cent))) & !grepl("subctr", colnames(cent))]
        for (v in cols) {
            cent[, (v) := NULL]
            cent[, (v) := get(paste0(v, "_subctr"))]
        }
        suffix <- paste0(suffix, "_subctr")
    }

    if (elasticity != "") {
        cols <- colnames(cent)[(grepl("sigma$", colnames(cent)))]
        for (v in cols) {
            cent[, (v) := NULL]
            cent[, (v) := get(paste0(v, "_", elasticity))]
        }
        suffix <- paste0(suffix, "_", elasticity)
    }

    ylab <- "On dual-use list, %"
    if (gta) {
        ylab <- "US export NTMs after 2022, %"
        cent[, dualuse_outcome := gta_export_usa_post22]
        suffix <- paste0(suffix, "_gta")
    }

    cent[, wgt := 1]
    ymax <- 0.64
    if (weights) {
        cent[, wgt := perc_value]
        suffix <- paste0(suffix, "_wgt")
        ymax <- 1.01
    }

    p_list <- list()
    j <- 1
    cols <- c("S_M", "C_M", "C_M_sigma")
    for (j in seq_along(cols)) {
        v <- cols[j]
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
    }

    p1 <- ggplot(data = cent, aes(x = rank_S_M, y = resid_S_M, color = "S_M")) +
        geom_line(linewidth = 1.0) + ylim(-0.01, ymax) +
        geom_point(aes(x = pbin50_S_M, y = yvar_S_M), size = 4, shape = 1, stroke = 1) +
        xlab(TeX("$S^M$")) + ylab("") + labs(title = ylab) +
        scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent) +
        scale_color_manual(name = "Sorting", labels = c(TeX("$S^M$")), values = c(rgb(120 / 255, 0 / 255, 15 / 255, 0.6, names = NULL, maxColorValue = 1))) +
        custom_theme + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 16),
            plot.title.position = "plot",
        ) + geom_text(x = 0, y = max(cent$yvar_S_M, na.rm = TRUE), label = TeX(cent[1, "r2_str_S_M"][[1]]), hjust = 0, color = royalred, size = 6, family = "Erewhon")
    p1
    
    p2 <- ggplot(data = cent, aes(x = rank_C_M, y = resid_C_M, color = "C_M")) +
        geom_line(linewidth = 1.0) + ylim(-0.01, ymax) +
        geom_point(aes(x = pbin50_C_M, y = yvar_C_M), size = 4, shape = 1, stroke = 1) +
        xlab(TeX("$C^M$")) + ylab("") + labs(title = ylab) +
        scale_color_manual(name = "Sorting", labels = c(TeX("$C^M$")), values = c(graphite)) +
        scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent) +
        custom_theme + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 16),
            plot.title.position = "plot",
        ) + geom_text(x = 0, y = max(cent$yvar_C_M, na.rm = TRUE), label = TeX(cent[1, "r2_str_C_M"][[1]]), hjust = 0, color = graphite, size = 6, family = "Erewhon")
    p2

    p3 <- ggplot(data = cent, aes(x = rank_C_M_sigma, y = resid_C_M_sigma, color = "C_M_sigma")) +
        geom_line(linewidth = 1.0) + ylim(-0.01, ymax) +
        geom_point(aes(x = pbin50_C_M_sigma, y = yvar_C_M_sigma), size = 4, shape = 1, stroke = 1) +
        scale_color_manual(name = "Sorting", labels = c(TeX("$C^M/\\sigma$")), values = c(royalblue)) +
        xlab(TeX("$C^M/\\sigma$")) + ylab("") + labs(title = ylab) +
        scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent) +
        custom_theme + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 16),
            plot.title.position = "plot",
        ) + geom_text(x = 0, y = max(cent$yvar_C_M_sigma, na.rm = TRUE), label = TeX(cent[1, "r2_str_C_M_sigma"][[1]]), hjust = 0, color = royalblue, size = 6, family = "Erewhon")
    p3

    p_together <- plot_grid(p1, p2, p3, ncol = 3)
    p_together    
    ggsave(file.path(STATS_PATH, "measurement", "validation", "robustness", paste0("rank", suffix, ".jpeg")), p_together, width = 10, height = 5.5, dpi = 300)
}


get_rank_plot_iterations <- function() {
    for (subcontracts in c(FALSE, TRUE)) {
        for (four_digit in c(FALSE, TRUE)) {
            for (nosmall in c(FALSE, TRUE)) {
                for (gta in c(FALSE, TRUE)) {
                    for (weights in c(FALSE, TRUE)) {
                        for (elasticity in c("", "sdb", "sbw", "bw", "cepii")) {
                            get_rank_plot(
                                subcontracts = subcontracts,
                                four_digit = four_digit,
                                nosmall = nosmall,
                                gta = gta,
                                weights = weights,
                                elasticity = elasticity
                            )
                        }
                    }
                }
            }
        }
    }
}


# get_rank_plot_iterations()


get_trade_zeros <- function(four_digit = FALSE) {
    trade <- fread(file.path(OUTPUT_PATH, "motivation", "trade", "trade_12_22.csv"), keepLeadingZeros = TRUE)
    trade <- trade[period >= 2015 & period < 2020]
    trade_share <- fread(file.path(OUTPUT_PATH, "motivation", "trade", "trade_shares_15_19.csv"), keepLeadingZeros = TRUE)

    suffix <- ""
    if (four_digit) {
        trade[, hscode := substr(hscode, 1, 4)]
        suffix <- "_4digit"    
        trade_share[, hscode := substr(hscode, 1, 4)]
    }
    trade <- trade[, lapply(.SD, sum), .SDcols = c("value_baci"), by = c("exporter_iso3", "hscode")]
    trade_zero <- trade[value_baci > 0]
    trade_zero <- trade_zero[, .N, by = "hscode"]
    trade_zero[, zeros := max(trade_zero$N) + 1 - N]
    trade_zero[, log_zeros := log(zeros)]

    trade[, share_value := value_baci / sum(value_baci), by = "hscode"]
    trade[, share_hhi := (share_value * 100)^2]
    trade_hhi <- trade[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("share_hhi"), by = "hscode"]
    colnames(trade_hhi) <- c("hscode", "hhi")
    trade_hhi[, log_hhi := log(hhi)]

    trade_share <- trade_share[, lapply(.SD, sum), .SDcols = c("perc_value"), by = "hscode"]
    trade_share[, perc_value_pbin10 := floor((rank(perc_value) - 1) / nrow(trade_share) * 10) + 1]
    trade_share[, perc_value_pbin5 := floor((rank(perc_value) - 1) / nrow(trade_share) * 5) + 1]

    trade <- merge_df(trade_zero, trade_hhi, by = "hscode", how = "outer")
    trade <- merge_df(trade, trade_share, by = "hscode", how = "outer")
    trade[, merge_ := NULL]

    fwrite(trade, file.path(OUTPUT_PATH, "measurement", "validation", paste0("trade_zeros", suffix, ".csv")))
}


get_trade_zeros()
get_trade_zeros(four_digit = TRUE)


run_regressions <- function(subcontracts = FALSE, four_digit = FALSE, nosmall = FALSE, gta = FALSE, weights = FALSE, elasticity = "") {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)
    trade_zeros <- fread(file.path(OUTPUT_PATH, "measurement", "validation", "trade_zeros.csv"), keepLeadingZeros = TRUE)
    trade_zeros[, perc_value := NULL]
    cent <- merge_df(cent, trade_zeros, by.x = "hs12", by.y = "hscode", how = "left")

    # four_digit <- TRUE
    suffix <- ""
    if (four_digit) {
        cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12_4digit.csv"), keepLeadingZeros = TRUE)
        trade_zeros <- fread(file.path(OUTPUT_PATH, "measurement", "validation", "trade_zeros_4digit.csv"), keepLeadingZeros = TRUE)
        trade_zeros[, perc_value := NULL]
        cent <- merge_df(cent, trade_zeros, by.x = "hs4", by.y = "hscode", how = "left")
        suffix <- "_4digit"
    }

    # nosmall <- TRUE
    cent[, weights_rank := rank(cent_M + cent_C) / nrow(cent)]
    if (nosmall) {
        cent <- cent[weights_rank > 0.05]
        suffix <- paste0(suffix, "_nosmall")
    }

    df_list <- list()
    i <- 1
    if (subcontracts) {
        cols <- colnames(cent)[(grepl("S_M", colnames(cent)) | grepl("C_M", colnames(cent))) & !grepl("subctr", colnames(cent))]
        for (v in cols) {
            cent[, (v) := NULL]
            cent[, (v) := get(paste0(v, "_subctr"))]
        }
        suffix <- paste0(suffix, "_subctr")
    }

    if (elasticity != "") {
        cols <- colnames(cent)[(grepl("sigma$", colnames(cent)))]
        for (v in cols) {
            cent[, (v) := NULL]
            cent[, (v) := get(paste0(v, "_", elasticity))]
        }
        suffix <- paste0(suffix, "_", elasticity)
    }

    ylab <- "On dual-use list: Yes"
    if (gta) {
        ylab <- "Had a US export NTM after 2022"
        cent[, dualuse_outcome := gta_export_usa_post22]
        suffix <- paste0(suffix, "_gta")
    }

    cent[, wgt := 1]
    ymax <- 0.64
    if (weights) {
        cent[, wgt := perc_value]
        suffix <- paste0(suffix, "_wgt")
        ymax <- 1.01
    }

    var_rename <- c(
        "dualuse_outcome" = ylab,
        "any_export" = "US Export NTMs after 2022: Yes",
        "S_M" = "$S_{\\mbox{\\scriptsize US}}^M$",
        "C_M" = "$\\mathcal{C}_{\\mbox{\\scriptsize US}}^M$",
        "C_M_sigma" = "$\\mathcal{C}_{\\mbox{\\scriptsize US}}^M / \\sigma$",
        "hs2" = "HS 2-digit",
        "hs4" = "HS 4-digit",
        "id" = "Polynomial $S_{\\mbox{\\scriptsize US}}^M$",
        "log_zeros" = "log(Export zeros)",
        "factor(pbin5_S_M_alt)" = "Piecewise $S_{\\mbox{\\scriptsize US}}^M$",
        "perc_value_pbin5" = "Goods controls (trade, sales, ...)"
    )
    cent[, S_M_alt := S_M]
    cent[, pbin5_S_M_alt := pbin5_S_M]
    cent[, id := 1]

    cent[is.na(log_hhi), log_hhi := min(log_hhi)]
    cent[is.na(log_zeros), log_zeros := min(log_zeros)]
    cent[, log_X := log(X)]
    
    reg1 <- feols(as.formula("dualuse_outcome ~ 1 + S_M"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg1)

    reg2 <- feols(as.formula("dualuse_outcome ~ 1 + C_M_sigma"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg2)

    reg3 <- feols(as.formula("dualuse_outcome ~ 1 + S_M + C_M_sigma"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg3)

    reg4 <- feols(
        as.formula("dualuse_outcome ~ 0 + S_M_alt + S_M_alt^2 + S_M_alt^3 + S_M_alt^4 + C_M_sigma | id"),
        data = cent,
        se = "hetero",
        weights = cent$wgt
    )
    summary(reg4)

    reg5 <- feols(
        as.formula("dualuse_outcome ~ C_M_sigma + factor(pbin5_S_M_alt):S_M_alt | factor(pbin5_S_M_alt)"),
        data = cent,
        se = "hetero",
        weights = cent$wgt
    )
    summary(reg5)

    reg6 <- feols(
        as.formula(paste0("dualuse_outcome ~ C_M_sigma ",
            "+ log_zeros + log_hhi + log_X ",
            "+ factor(perc_value_pbin5) * perc_value ",
            "+ factor(pbin5_S_M_alt):S_M_alt ",
            " | perc_value_pbin5 + factor(pbin5_S_M_alt)")),
        data = cent,
        se = "hetero",
        weights = cent$wgt
    )
    summary(reg6)

    reg7 <- feols(
       as.formula(paste0("dualuse_outcome ~ C_M_sigma ",
            "+ log_zeros + log_hhi + log_X ",
            "+ factor(perc_value_pbin5) * perc_value ",
            "+ factor(pbin5_S_M_alt):S_M_alt ",
            " | perc_value_pbin5 + factor(pbin5_S_M_alt) + hs2")),
        data = cent,
        se = "hetero",
        weights = cent$wgt
    )
    summary(reg7)

    etable(reg1, reg2, reg3, reg4, reg5, reg6, reg7,
        file = file.path(STATS_PATH, "measurement", "validation", "robustness", paste0('baseline', suffix, '.tex')),
        keep = c("US"), replace = T, dict = var_rename,
        signif.code = c("*" = 0.05, "**" = 0.01, "***" = 0.001))


    # logit
    reg1 <- feglm(as.formula("dualuse_outcome ~ 1 + S_M"),
        data = cent, family = binomial(link = "logit"), se = "hetero", weights = cent$wgt)
    summary(reg1)

    reg2 <- feglm(as.formula("dualuse_outcome ~ 1 + C_M_sigma"),
        data = cent, family = binomial(link = "logit"), se = "hetero", weights = cent$wgt)
    summary(reg2)

    reg3 <- feglm(as.formula("dualuse_outcome ~ 1 + S_M + C_M_sigma"),
        data = cent, family = binomial(link = "logit"), se = "hetero", weights = cent$wgt)
    summary(reg3)

    reg4 <- feglm(
        as.formula("dualuse_outcome ~ 0 + S_M_alt + S_M_alt^2 + S_M_alt^3 + S_M_alt^4 + C_M_sigma | id"),
        data = cent,
        family = binomial(link = "logit"),
        se = "hetero",
        weights = cent$wgt
    )
    summary(reg4)

    reg5 <- feglm(
        as.formula("dualuse_outcome ~ 1 + C_M_sigma + factor(pbin5_S_M_alt):S_M_alt | factor(pbin5_S_M_alt)"),
        data = cent,
        family = binomial(link = "logit"),
        se = "hetero",
        weights = cent$wgt
    )
    summary(reg5)

    reg6 <- feglm(
        as.formula(paste0("dualuse_outcome ~ C_M_sigma ",
            "+ log_zeros + log_hhi + log_X ",
            "+ factor(perc_value_pbin5) * perc_value ",
            "+ factor(pbin5_S_M_alt):S_M_alt ",
            " | perc_value_pbin5 + factor(pbin5_S_M_alt)")),
        data = cent,
        family = binomial(link = "logit"),
        se = "hetero",
        weights = cent$wgt
    )
    summary(reg6)

    reg7 <- feglm(
        as.formula(paste0("dualuse_outcome ~ C_M_sigma ",
            "+ log_zeros + log_hhi + log_X ",
            "+ factor(perc_value_pbin5) * perc_value ",
            "+ factor(pbin5_S_M_alt):S_M_alt ",
            " | perc_value_pbin5 + factor(pbin5_S_M_alt) + hs2")),
        data = cent,
        family = binomial(link = "logit"),
        se = "hetero",
        weights = cent$wgt
    )
    summary(reg7)

    etable(reg1, reg2, reg3, reg4, reg5, reg6, reg7,
        file = file.path(STATS_PATH, "measurement", "validation", "robustness", paste0('logit', suffix, '.tex')),
        keep = c("US"), replace = T, dict = var_rename,
        signif.code = c("*" = 0.05, "**" = 0.01, "***" = 0.001))
}


get_regression_iterations <- function() {
    for (subcontracts in c(TRUE, FALSE)) {
        for (four_digit in c(TRUE, FALSE)) {
            for (nosmall in c(TRUE, FALSE)) {
                for (gta in c(TRUE, FALSE)) {
                    for (weights in c(TRUE, FALSE)) {
                        for (elasticity in c("", "sdb", "sbw", "bw", "cepii")) {
                            print(paste0(subcontracts, four_digit, nosmall, gta, weights, elasticity))
                            run_regressions(
                                subcontracts = subcontracts,
                                four_digit = four_digit,
                                nosmall = nosmall,
                                gta = gta,
                                weights = weights,
                                elasticity = elasticity
                            )
                        }
                    }
                }
            }
        }
    }
}


get_regression_iterations()


run_regression_extras <- function(subcontracts = FALSE, four_digit = FALSE, nosmall = FALSE, gta = FALSE, weights = FALSE, elasticity = "") {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)
    trade_zeros <- fread(file.path(OUTPUT_PATH, "measurement", "validation", "trade_zeros.csv"), keepLeadingZeros = TRUE)
    trade_zeros[, perc_value := NULL]
    cent <- merge_df(cent, trade_zeros, by.x = "hs12", by.y = "hscode", how = "left")

    # four_digit <- TRUE
    suffix <- ""
    if (four_digit) {
        cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12_4digit.csv"), keepLeadingZeros = TRUE)
        trade_zeros <- fread(file.path(OUTPUT_PATH, "measurement", "validation", "trade_zeros_4digit.csv"), keepLeadingZeros = TRUE)
        trade_zeros[, perc_value := NULL]
        cent <- merge_df(cent, trade_zeros, by.x = "hs4", by.y = "hscode", how = "left")
        suffix <- "_4digit"
    }

    # nosmall <- TRUE
    cent[, weights_rank := rank(cent_M + cent_C) / nrow(cent)]
    if (nosmall) {
        cent <- cent[weights_rank > 0.05]
        suffix <- paste0(suffix, "_nosmall")
    }

    df_list <- list()
    i <- 1
    if (subcontracts) {
        cols <- colnames(cent)[(grepl("S_M", colnames(cent)) | grepl("C_M", colnames(cent))) & !grepl("subctr", colnames(cent))]
        for (v in cols) {
            cent[, (v) := NULL]
            cent[, (v) := get(paste0(v, "_subctr"))]
        }
        suffix <- paste0(suffix, "_subctr")
    }

    if (elasticity != "") {
        cols <- colnames(cent)[(grepl("sigma$", colnames(cent)))]
        for (v in cols) {
            cent[, (v) := NULL]
            cent[, (v) := get(paste0(v, "_", elasticity))]
        }
        suffix <- paste0(suffix, "_", elasticity)
    }

    ylab <- "On dual-use list: Yes"
    if (gta) {
        ylab <- "Had a US export NTM after 2022"
        cent[, dualuse_outcome := gta_export_usa_post22]
        suffix <- paste0(suffix, "_gta")
    }

    cent[, wgt := 1]
    ymax <- 0.64
    if (weights) {
        cent[, wgt := perc_value]
        suffix <- paste0(suffix, "_wgt")
        ymax <- 1.01
    }

    var_rename <- c(
        "dualuse_outcome" = ylab,
        "any_export" = "US Export NTMs after 2022: Yes",
        "S_M" = "$S_{\\mbox{\\scriptsize US}}^M$",
        "C_M" = "$\\mathcal{C}_{\\mbox{\\scriptsize US}}^M$",
        "C_M_sigma" = "$\\mathcal{C}_{\\mbox{\\scriptsize US}}^M / \\sigma$",
        "hs2" = "HS 2-digit",
        "hs4" = "HS 4-digit",
        "id" = "Polynomial $S_{\\mbox{\\scriptsize US}}^M$",
        "id_sM" = "Polynomial $s_{\\mbox{\\scriptsize US}}^M$",
        "id_centM" = "Polynomial $\\Psi' s_{\\mbox{\\scriptsize US}}^M$",
        "log_zeros" = "log(Export zeros)",
        "factor(pbin5_S_M_alt)" = "Piecewise $S_{\\mbox{\\scriptsize US}}^M$",
        "perc_value_pbin5" = "Goods controls (trade, sales, ...)",
        "rank_S_M" = "rank $S_{\\mbox{\\scriptsize US}}^M$",
        "rank_C_M" = "rank $\\mathcal{C}_{\\mbox{\\scriptsize US}}^M$",
        "rank_C_M_sigma" = "rank $\\mathcal{C}_{\\mbox{\\scriptsize US}}^M / \\sigma$",
        "rank_id" = "Polynomial rank $S_{\\mbox{\\scriptsize US}}^M$",
        "rank_sM" = "Polynomial rank $s_{\\mbox{\\scriptsize US}}^M$",
        "rank_centM" = "Polynomial rank $\\Psi' s_{\\mbox{\\scriptsize US}}^M$",
        "log_zeros" = "log(Export zeros)",
        "factor(pbin5_rank_S_M_alt)" = "Piecewise rank $S_{\\mbox{\\scriptsize US}}^M$",
        "perc_value_pbin5" = "Goods controls (trade, sales, ...)"
    )
    cent[, rank_S_M_alt := rank_S_M]
    cent[, pbin5_rank_S_M_alt := floor(rank_S_M * 5)]
    cent[, id := 1]
    cent[, rank_id := 1]
    cent[, id_sM := 1]
    cent[, id_centM := 1]
    cent[, rank_sM := 1]
    cent[, rank_centM := 1]

    cent[is.na(log_hhi), log_hhi := min(log_hhi)]
    cent[is.na(log_zeros), log_zeros := min(log_zeros)]
    cent[, log_X := log(X)]
    
    reg1 <- feols(as.formula("dualuse_outcome ~ 1 + rank_S_M"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg1)

    reg2 <- feols(as.formula("dualuse_outcome ~ 1 + rank_C_M_sigma"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg2)

    reg3 <- feols(as.formula("dualuse_outcome ~ 1 + rank_S_M + rank_C_M_sigma"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg3)

    reg4 <- feols(
        as.formula("dualuse_outcome ~ 0 + rank_S_M_alt + rank_S_M_alt^2 + rank_S_M_alt^3 + rank_S_M_alt^4 + rank_C_M_sigma | rank_id"),
        data = cent,
        se = "hetero",
        weights = cent$wgt
    )
    summary(reg4)

    reg5 <- feols(
        as.formula("dualuse_outcome ~ rank_C_M_sigma + factor(pbin5_rank_S_M_alt):rank_S_M_alt | factor(pbin5_rank_S_M_alt)"),
        data = cent,
        se = "hetero",
        weights = cent$wgt
    )
    summary(reg5)

    reg6 <- feols(
        as.formula(paste0("dualuse_outcome ~ rank_C_M_sigma ",
            "+ log_zeros + log_hhi + log_X ",
            "+ factor(perc_value_pbin5) * perc_value ",
            "+ factor(pbin5_rank_S_M_alt):rank_S_M_alt ",
            " | perc_value_pbin5 + factor(pbin5_rank_S_M_alt)")),
        data = cent,
        se = "hetero",
        weights = cent$wgt
    )
    summary(reg6)

    reg7 <- feols(
       as.formula(paste0("dualuse_outcome ~ rank_C_M_sigma ",
            "+ log_zeros + log_hhi + log_X ",
            "+ factor(perc_value_pbin5) * perc_value ",
            "+ factor(pbin5_rank_S_M_alt):rank_S_M_alt ",
            " | perc_value_pbin5 + factor(pbin5_rank_S_M_alt) + hs2")),
        data = cent,
        se = "hetero",
        weights = cent$wgt
    )
    summary(reg7)

    etable(reg1, reg2, reg3, reg4, reg5, reg6, reg7,
        file = file.path(STATS_PATH, "measurement", "validation", "robustness", paste0('rank_extra_', suffix, '.tex')),
        keep = c("US"), replace = T, dict = var_rename,
        signif.code = c("*" = 0.05, "**" = 0.01, "***" = 0.001))
    

    cent[, rank_s_M := rank(s_M) / sum(!is.na(s_M))]
    cent[, rank_cent_M := rank(cent_M) / sum(!is.na(cent_M))]
    
    reg1 <- feols(as.formula("dualuse_outcome ~ 1 + C_M_sigma + s_M + s_M^2 + s_M^3 + s_M^4 | id_sM"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg1)

    reg2 <- feols(as.formula("dualuse_outcome ~ 1 + C_M_sigma + cent_M + cent_M^2 + cent_M^3 + cent_M^4 | id_centM"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg2)

    reg3 <- feols(as.formula("dualuse_outcome ~ 1 + C_M_sigma + rank_s_M + rank_s_M^2 + rank_s_M^3 + rank_s_M^4 | rank_sM"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg3)

    reg4 <- feols(as.formula("dualuse_outcome ~ 1 + C_M_sigma + rank_cent_M + rank_cent_M^2 + rank_cent_M^3 + rank_cent_M^4 | rank_centM"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg4)

    reg5 <- feols(as.formula("dualuse_outcome ~ 1 + rank_C_M_sigma + s_M + s_M^2 + s_M^3 + s_M^4 | id_sM"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg5)

    reg6 <- feols(as.formula("dualuse_outcome ~ 1 + rank_C_M_sigma + cent_M + cent_M^2 + cent_M^3 + cent_M^4 | id_centM"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg6)

    reg7 <- feols(as.formula("dualuse_outcome ~ 1 + rank_C_M_sigma + rank_s_M + rank_s_M^2 + rank_s_M^3 + rank_s_M^4 | rank_sM"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg7)

    reg8 <- feols(as.formula("dualuse_outcome ~ 1 + rank_C_M_sigma + rank_cent_M + rank_cent_M^2 + rank_cent_M^3 + rank_cent_M^4 | rank_centM"), data = cent, se = "hetero", weights = cent$wgt)
    summary(reg8)

    etable(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8,
        file = file.path(STATS_PATH, 'measurement', "validation", "robustness", paste0('horseraces', suffix, '.tex')),
        keep = c("US"), replace = T, dict = var_rename,
        signif.code = c("*" = 0.05, "**" = 0.01, "***" = 0.001))
}


get_regression_extras_iterations <- function() {
    for (subcontracts in c(FALSE, TRUE)) {
        for (four_digit in c(FALSE, TRUE)) {
            for (nosmall in c(FALSE, TRUE)) {
                for (gta in c(FALSE, TRUE)) {
                    for (weights in c(FALSE, TRUE)) {
                        for (elasticity in c("", "sdb", "sbw", "bw", "cepii")) {
                            print(paste0(subcontracts, four_digit, nosmall, gta, weights, elasticity))
                            run_regression_extras(
                                subcontracts = subcontracts,
                                four_digit = four_digit,
                                nosmall = nosmall,
                                gta = gta,
                                weights = weights,
                                elasticity = elasticity
                            )
                        }
                    }
                }
            }
        }
    }
}


get_regression_extras_iterations()

