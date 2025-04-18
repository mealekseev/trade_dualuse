### Validate trade

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


prepare_event_flows <- function() {
    cpi_u <- fread(file.path(DATA_PATH, 'measurement', 'accounting', 'CPIAUCSL.csv'))
    colnames(cpi_u) <- c('fiscal_year', 'index')
    cpi_u[, fiscal_year := as.integer(substr(fiscal_year, 1, 4))]
    cpi_u[, index := as.numeric(index)]    
    cpi_u[, index := cpi_u[fiscal_year == 2018, index] / index]

    df_list <- list()
    yr <- 2012
    i <- 1
    for (yr in seq(2012, 2022, 1)) {
        df <- fread(file.path(DATA_PATH, "motivation", "trade", "BACI_HS12_V202401b", paste0("BACI_HS12_Y", yr, "_V202401b.csv")), keepLeadingZeros = TRUE)
        df <- df[i != j]
        df <- df[j == 156 | j == 643 | j == 804]
        colnames(df) <- c("year", "exporter", "importer", "hscode", "value", "quantity")
        df[, hscode := str_pad(hscode, 6, pad = "0")]
        df[, value := value * 1000 * cpi_u[fiscal_year == yr, index]]
        df <- df[, lapply(.SD, sum), .SDcols = c("value"), by = c("year", "exporter", "importer", "hscode")]
        df_list[[i]] <- df
        i <- i + 1
    }
    df <- rbindlist(df_list)
    
    df_importer <- df[, lapply(.SD, sum), .SDcols = c("value"), by = c("year", "importer", "hscode")]
    country <- fread(file.path(DATA_PATH, "motivation", "trade", "BACI_HS12_V202401b", "country_codes_V202401b.csv"))
    country[is.na(country$country_iso3), country_iso3 := as.character(country$country_code)]
    country <- country[, c("country_code", "country_iso3")]
    setnames(country, c("importer", "importer_code"))
    df_importer <- merge_df(df_importer, country, by = "importer", how = "left", indicator = FALSE)
    df_importer <- df_importer[, c("year", "importer_code", "hscode", "value")]

    panel <- CJ(seq(2012, 2022), unique(df_importer$importer_code), unique(df_importer$hscode))
    colnames(panel) <- c("year", "importer_code", "hscode")
    panel <- merge_df(panel, df_importer, by = c("year", "importer_code", "hscode"), how = "left")
    panel[is.na(value), value := 0]
    panel[, merge_ := NULL]
    fwrite(panel, file.path(OUTPUT_PATH, "measurement", "validation", "event_import.csv"))

    country <- fread(file.path(DATA_PATH, "motivation", "trade", "BACI_HS12_V202401b", "country_codes_V202401b.csv"))
    country[is.na(country$country_iso3), country_iso3 := as.character(country$country_code)]
    country <- country[, c("country_code", "country_iso3")]
    setnames(country, c("exporter", "exporter_code"))
    df <- merge_df(df, country, by = "exporter", how = "left", indicator = FALSE)
    setnames(country, c("importer", "importer_code"))
    df <- merge_df(df, country, by = "importer", how = "left", indicator = FALSE)
    df <- df[, c("year", "exporter_code", "importer_code", "hscode", "value")]

    panel <- CJ(seq(2012, 2022), unique(df$exporter_code), unique(df$importer_code), unique(df$hscode))
    colnames(panel) <- c("year", "exporter_code", "importer_code", "hscode")
    panel <- merge_df(panel, df, by = c("year", "exporter_code", "importer_code", "hscode"), how = "left", indicator = FALSE)
    panel[is.na(value), value := 0]
    fwrite(panel, file.path(OUTPUT_PATH, "measurement", "validation", "event_flows.csv"))
}


# prepare_event_flows()


plot_event_study <- function() {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)
    panel_import <- fread(file.path(OUTPUT_PATH, "measurement", "validation", "event_import.csv"), keepLeadingZeros = TRUE)
    cent <- merge_df(panel_import, cent, by.x = c("hscode"), by.y = c("hs12"), how = "inner")

    df_list <- list()
    cent[, wgt := 1]

    cty <- "UKR"
    for (cty in c("CHN", "UKR", "RUS")) {
        df_cty <- cent[importer_code == cty]
        if (cty == "CHN") {
            country <- "China"
            base_year <- 2015
        } else if (cty == "UKR") {
            country <- "Ukraine"
            base_year <- 2021
        } else if (cty == "RUS") {
            country <- "Russia"
            base_year <- 2021
        }
        for (yr in seq(2012, 2022, 1)) {
            if (yr != base_year) {
                df_cty[, (paste0("yr", yr)) := 0]
                df_cty[year == yr, (paste0("yr", yr)) := 1]
            }
        }
        yr_vars <- colnames(df_cty)[grepl("^yr", colnames(df_cty))]
        fml <- paste0(
            "value ~ ",
            paste(paste0("C_M_sigma:", yr_vars, sep = ""), collapse = " + "),
            " | hscode + year"
        )
        reg1 <- fepois(as.formula(fml), data = df_cty, weights = df_cty$wgt)
        vcov1_clustered <- vcov(reg1, cluster = ~ hscode)
        se_clustered <- sqrt(diag(vcov1_clustered))
        year_list <- as.integer(str_replace(names(reg1$coefficients), "C_M_sigma:yr", ""))
        coeftable <- data.table(
            year = year_list,
            beta = reg1$coefficients,
            se = se_clustered,
            N = reg1$nobs,
            spec = "C"
        )
        new_row <- data.table(year = base_year, beta = 0, se = 0, N = 0, spec = "C")
        coeftable <- rbind(coeftable, new_row)
        coeftable[, beta_upper := beta + 1.96 * se]
        coeftable[, beta_lower := beta - 1.96 * se]

        ggplot(data = coeftable, aes(x = year, y = beta)) +
            geom_point(size = 4, shape = 1, stroke = 1) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_ribbon(aes(ymin = beta_lower, ymax = beta_upper), fill = royalblue, alpha = 0.2) +
            scale_x_continuous(breaks = seq(2012, 2022, 1)) +
            geom_vline(xintercept = base_year, linetype = "dashed", color = "black", linewidth = 1.0) +
            xlab("") + ylab(TeX(paste0("Imp. response $\\beta_t$"))) +
            custom_theme
        ggsave(
            file.path(STATS_PATH, "measurement", "validation", paste0("event_", cty, ".jpeg")), width = 10, height = 3, dpi = 300
        )

        ggplot(data = coeftable, aes(x = year, y = beta)) +
            geom_ribbon(aes(ymin = beta_lower, ymax = beta_upper), fill = royalblue, alpha = 0.0) +
            scale_x_continuous(breaks = seq(2012, 2022, 1)) +
            geom_vline(xintercept = base_year, linetype = "dashed", color = "black", linewidth = 1.0) +
            xlab("") + ylab(TeX(paste0("Imp. response $\\beta_t$"))) +
            custom_theme
        ggsave(
            file.path(STATS_PATH, "measurement", "validation", paste0("event_", cty, "_empty.jpeg")), width = 10, height = 3, dpi = 300
        )
    }
}


plot_source_decomposition <- function() {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)
    cent <- cent[, c("hs12", "C_M_sigma")]
    panel_import <- fread(file.path(OUTPUT_PATH, "measurement", "validation", "event_flows.csv"), keepLeadingZeros = TRUE)
    cent <- merge_df(panel_import, cent, by.x = c("hscode"), by.y = c("hs12"), how = "inner")
    cty <- "UKR"
    for (cty in c("CHN", "UKR", "RUS")) {
        df_cty <- cent[importer_code == cty]
        if (cty == "CHN") {
            country <- "China"
            yr_start <- 2015
            yr_end <- 2022
        } else if (cty == "UKR") {
            country <- "Ukraine"
            yr_start <- 2021
            yr_end <- 2022
        } else if (cty == "RUS") {
            country <- "Russia"
            yr_start <- 2021
            yr_end <- 2022
        }

        trade <- copy(df_cty)
        trade_sum <- trade[, sum(C_M_sigma * value), by = c("year")]
        colnames(trade_sum) <- c("year", "value")

        trade_cty <- trade[, sum(C_M_sigma * value), by = c("year", "exporter_code")]
        trade_cty <- merge_df(trade_cty, trade_sum, by = "year", how = "inner", indicator = FALSE)
        colnames(trade_cty) <- c("year", "exporter_code", "value", "value_sum")
        trade_cty[, value_perc := value / value_sum]

        trade_year1 <- trade_cty[year == yr_start]
        trade_year2 <- trade_cty[year == yr_end]
        trade_chg <- merge_df(trade_year1, trade_year2, by = c("exporter_code"), how = "outer")
        trade_chg[is.na(value_perc.x), value_perc.x := 0]
        trade_chg[is.na(value_perc.y), value_perc.y := 0]
        trade_chg[, delta_C_mean := value_perc.y - value_perc.x]
        trade_chg <- trade_chg[order(-delta_C_mean)]
        trade_chg1 <- trade_chg[1:10]
        trade_chg1[, type := "top"]
        trade_chg2 <- trade_chg[(nrow(trade_chg) - 9):(nrow(trade_chg))]
        trade_chg2[, type := "bottom"]
        plot_chg <- rbind(trade_chg1, trade_chg2)
        plot_chg[exporter_code == "S19", exporter_code := "TWN"]

        ggplot(data = plot_chg, aes(x = reorder(exporter_code, -delta_C_mean), y = delta_C_mean, fill = type)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_text(data = plot_chg[type == "top"], aes(label = exporter_code, y = 0), vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
            geom_text(data = plot_chg[type == "bottom"], aes(label = exporter_code, y = 0), vjust = -0.7, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
            scale_fill_manual(values = c(royalred, royalblue)) +
            scale_y_continuous(labels = scales::percent) +
            xlab("") + ylab(TeX(paste0("Avg. mil. contrib., %"))) +
            custom_theme + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
        ggsave(file.path(STATS_PATH, "measurement", "validation", paste0("bar_", cty, ".jpeg")), width = 10, height = 3, dpi = 300)

        ggplot(data = plot_chg, aes(x = reorder(exporter_code, -delta_C_mean), y = delta_C_mean, fill = type)) +
            geom_bar(stat = "identity", position = "dodge", alpha = 0.0) +
            scale_fill_manual(values = c(royalred, royalblue)) +
            scale_y_continuous(labels = scales::percent) +
            xlab("") + ylab(TeX(paste0("Avg. mil. contrib., %"))) +
            custom_theme + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
        ggsave(file.path(STATS_PATH, "measurement", "validation", paste0("bar_", cty, "_empty.jpeg")), width = 10, height = 3, dpi = 300)
    }
}


plot_event_study()
plot_source_decomposition()

