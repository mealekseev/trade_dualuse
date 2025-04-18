### Estimate Tullock parameter

rm(list=ls())
gc()

library(data.table)
library(fixest)
library(readxl)
library(stringr)
library(stats)
library(xtable)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


prepare_data <- function() {
    df <- fread(file.path(OUTPUT_PATH, "calibration", "tullock", "milex.csv"))
    df <- df[classif == 1 | classif == 2]
    df <- dcast(df, year ~ classif, value.var = "milex")
    df <- df[year >= 1950 & year <= 2021]
    colnames(df) <- c("year", "M_USA", "M_CHN")
    df[, year_sq := year * year]

    # add wars
    df[, war := ""]
    df[year >= 1950 & year <= 1953, war := "Korean war"]
    df[year >= 1964 & year <= 1975, war := "Vietnam war"]
    df[year >= 1990 & year <= 1991, war := "Desert storm"]
    df[year >= 2001 & year <= 2002, war := "Afghanistan"]
    df[year >= 2003 & year <= 2011, war := "Iraq war"]
    df[year >= 2022, war := "Ukraine war"]
    df[, war_year := as.integer(seq_len(.N) == 1), by = "war"]
    for (i in 1:4) {
        df[shift(war_year) == i & war_year == 0, war_year := i + 1]
    }
    df[, war_start := as.integer(seq_len(.N) == 1), by = "war"]

    # add cold war
    df[, cold_war := 1]
    df[year >= 1989, cold_war := 0]

    # add periods
    df[, period := ""]
    df[year < 1989, period := "cold_war"]
    df[year >= 1989 & year < 2001, period := "end_of_history"]
    df[year >= 2001 & year < 2014, period := "war_on_terror"]
    df[year >= 2014 & year <= 2021, period := "new_cold_war"]
    df[year >= 2022, period := "ukraine"]
    return(df)
}


estimate_fit <- function(gamma, tdf) {
    tdf[, sol := gamma * log(M_USA) + gamma * log(M_CHN) - 2 * log(M_USA^gamma + M_CHN^gamma)]
    tdf[, depvar := log(M_USA) - sol]
    tdf[, lag_depvar := shift(depvar, 1, type = "lag")]
    tdf[is.na(lag_depvar), lag_depvar := depvar]

    reg <- lm(depvar ~ year, data = tdf)
    r2_year <- summary(reg)$r.squared

    reg <- lm(depvar ~ year + lag_depvar, data = tdf)
    r2_lag <- summary(reg)$r.squared

    reg <- lm(depvar ~ year + lag_depvar + factor(period), data = tdf)
    r2_period <- summary(reg)$r.squared

    reg <- lm(depvar ~ year + lag_depvar + factor(period) + factor(war), data = tdf)
    r2_war <- summary(reg)$r.squared

    reg <- lm(depvar ~ year + lag_depvar + factor(period) + factor(war) + factor(war_start), data = tdf)
    r2_wardyn <- summary(reg)$r.squared

    res <- list(
        r2_year = r2_year, r2_lag = r2_lag, r2_period = r2_period, r2_war = r2_war, r2_wardyn = r2_wardyn,
        data = copy(tdf)
    )
    return(res)
}
    

get_elasticity <- function() {
    df <- prepare_data()
    gamma_list <- seq(0.001, 2.000, 0.001)
    r2_year <- c()
    r2_lag <- c()
    r2_period <- c()
    r2_war <- c()
    r2_wardyn <- c()
    for (gamma in gamma_list) {
        print(gamma)
        res <- estimate_fit(gamma, copy(df))
        r2_year <- c(r2_year, res$r2_year)
        r2_lag <- c(r2_lag, res$r2_lag)
        r2_period <- c(r2_period, res$r2_period)
        r2_war <- c(r2_war, res$r2_war)
        r2_wardyn <- c(r2_wardyn, res$r2_wardyn)
    }
    df_plot <- data.table(
        gamma = gamma_list,
        r2_year = r2_year,
        r2_period = r2_period,
        r2_lag = r2_lag,
        r2_war = r2_war,
        r2_wardyn = r2_wardyn
    )

    gamma_year <- gamma_list[which.max(df_plot$r2_year)]
    r2_year <- df_plot[which.max(df_plot$r2_year), r2_year]
    print(paste0("gamma_year: ", gamma_year))

    gamma_lag <- gamma_list[which.max(df_plot$r2_lag)]
    r2_lag <- df_plot[which.max(df_plot$r2_lag), r2_lag]
    print(paste0("gamma_lag: ", gamma_lag))

    gamma_period <- gamma_list[which.max(df_plot$r2_period)]
    r2_period <- df_plot[which.max(df_plot$r2_period), r2_period]
    print(paste0("gamma_period: ", gamma_period))
    
    gamma_war <- gamma_list[which.max(df_plot$r2_war)]
    r2_war <- df_plot[which.max(df_plot$r2_year), r2_war]
    print(paste0("gamma_war: ", gamma_war))

    gamma_wardyn <- gamma_list[which.max(df_plot$r2_wardyn)]
    r2_wardyn <- df_plot[which.max(df_plot$r2_year), r2_wardyn]
    print(paste0("gamma_wardyn: ", gamma_wardyn))

    ggplot(data = df_plot[gamma < 1.0], aes(x = gamma, y = r2_year)) +
        geom_line(aes(color = "(1) trend"), linewidth = 1.0) +
        geom_line(aes(x = gamma, y = r2_lag, color = "(2) trend + AR(1)"), linewidth = 1.0) +
        geom_line(aes(x = gamma, y = r2_period, color = "(3) + period dummies"), linewidth = 1.0) +
        geom_line(aes(x = gamma, y = r2_war, color = "(4) + war dummies"), linewidth = 1.0) +
        geom_line(aes(x = gamma, y = r2_wardyn, color = "(5) + war start shocks"), linewidth = 1.0) +
        scale_color_manual(name = "Specification", values = c(royalred, royalblue, lightblue, graphite, gold)) +
        geom_vline(xintercept = gamma_year, linetype = "dashed", color = royalred, linewidth = 1.0) +
        geom_vline(xintercept = gamma_lag, linetype = "dashed", color = royalblue, linewidth = 1.0) +
        geom_vline(xintercept = gamma_period, linetype = "dashed", color = lightblue, linewidth = 1.0) +
        geom_vline(xintercept = gamma_war, linetype = "dashed", color = graphite, linewidth = 1.0) +
        geom_vline(xintercept = gamma_wardyn, linetype = "dashed", color = gold, linewidth = 1.0) +
        geom_hline(yintercept = r2_year, linetype = "dashed", color = royalred, linewidth = 1.0) +
        geom_hline(yintercept = r2_lag, linetype = "dashed", color = royalblue, linewidth = 1.0) +
        geom_hline(yintercept = r2_period, linetype = "dashed", color = lightblue, linewidth = 1.0) +
        geom_hline(yintercept = r2_war, linetype = "dashed", color = graphite, linewidth = 1.0) +
        geom_hline(yintercept = r2_wardyn, linetype = "dashed", color = gold, linewidth = 1.0) +
        custom_theme + theme(legend.position = "right")
    ggsave(file.path(STATS_PATH, "calibration", "tullock", "gamma.jpeg"), width = 10, height = 5.5, dpi = 300)

    res <- estimate_fit(gamma_war, copy(df))
    tdf <- res$data
    reg <- lm(depvar ~ year + factor(period) + lag_depvar + factor(war), data = tdf)
    tdf[, log_M_USA_predict := reg$fitted.values + sol]
    tdf[, M_USA_predict := exp(log_M_USA_predict)]
    tdf_long <- melt(tdf, id.vars = c("year"), measure.vars = c("M_USA", "M_CHN", "M_USA_predict"))
    ggplot(data = tdf_long, aes(x = year, y = value, color = variable, linetype = variable)) +
        geom_line(linewidth = 1.0) +
        scale_color_manual(values = c(royalblue, royalred, royalblue), name = "", labels = c("Western bloc", "Eastern bloc", "Western bloc (predicted)")) +
        scale_linetype_manual(values = c("solid", "solid", "dashed"), name = "", labels = c("Western bloc", "Eastern bloc", "Western bloc (predicted)")) +
        ylab("Military spending (trl $)") + xlab("") +
        custom_theme
    ggsave(file.path(STATS_PATH, "calibration", "tullock", "milex_fit_slides.jpeg"), width = 10, height = 5.5, dpi = 300)

    ggplot(data = tdf_long, aes(x = year, y = value / 1e6, color = variable, linetype = variable)) +
        geom_line(linewidth = 1.0) +
        scale_color_manual(values = c(royalblue, royalred, royalblue), name = "", labels = c("Western bloc", "Eastern bloc", "Western bloc (predicted)")) +
        scale_linetype_manual(values = c("solid", "solid", "dashed"), name = "", labels = c("Western bloc", "Eastern bloc", "Western bloc (predicted)")) +
        ylab("Military spending (trln $)") + xlab("") +
        custom_theme
    ggsave(file.path(STATS_PATH, "calibration", "tullock", "milex_fit.jpeg"), width = 10, height = 4, dpi = 300)

    res <- data.table(
        names = c("$\\hat \\gamma$", "$R^2$"),
        gamma_year = c(gamma_year, r2_year),
        gamma_lag = c(gamma_lag, r2_lag),
        gamma_period = c(gamma_period, r2_period),
        gamma_war = c(gamma_war, r2_war),
        gamma_wardyn = c(gamma_wardyn, r2_wardyn)
    )
    colnames(res) <- c("", "trend", "trend + AR(1)", "+ period dummies", "+ war dummies", "+ war start shocks")
    latex_code <- xtable(res, include.rownames = FALSE, digits = 3)
	latex_code <- print(latex_code, type = "latex", digits = 3, include.rownames = FALSE, sanitize.text.function = identity)
	latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
	latex_code <- sub("\\{lrrrrr\\}", "{l|ccccc}", latex_code)
	write(latex_code, file.path(STATS_PATH, "calibration", "tullock", "gamma_specifications.tex"))
}


get_elasticity()

