### Build decompositions

rm(list=ls())
gc()

library(data.table)
library(stringr)
library(fixest)
library(latex2exp)
library(xtable)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


build_hscode_decompositions <- function() {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)
    panel <- fread(file.path(OUTPUT_PATH, "measurement", "validation", "event_import.csv"), keepLeadingZeros = TRUE)

    cent[, wgt := 1]
    cent <- cent[, c("hs12", "hscode_name", "C_M_sigma")]
    cent <- merge_df(panel, cent, by.x = c("hscode"), by.y = c("hs12"), how = "inner")
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
        trade_mean <- trade[, lapply(.SD, weighted.mean, w = value), .SDcols = "C_M_sigma", by = c("hscode", "hscode_name")]
        trade_sum <- trade[, lapply(.SD, sum), by = c("year", "hscode", "hscode_name"), .SDcols = "value"]        
        trade_ttl <- merge_df(trade_mean, trade_sum, by = c("hscode", "hscode_name"), how = "inner")
        trade_ttl[, C_M_sigma_mean := weighted.mean(C_M_sigma, w = value), by = "year"]
        trade_ttl[, perc_value := value / sum(value), by = "year"]

        trade_year1 <- trade_ttl[year == yr_start]
        trade_year2 <- trade_ttl[year == yr_end]
        trade_chg <- merge_df(trade_year1, trade_year2, by = c("hscode", "hscode_name", "C_M_sigma"), how = "outer")
        trade_chg[, delta_C_mean := C_M_sigma_mean.y - C_M_sigma_mean.x]
        trade_chg[, delta_C := C_M_sigma * (perc_value.y - perc_value.x)]
        trade_chg[, contrib := delta_C / delta_C_mean]
        trade_chg_copy <- copy(trade_chg)

        trade_chg <- trade_chg[order(-contrib)]
        trade_chg <- trade_chg[1:10]
        trade_chg[, trade_chg := perc_value.y - perc_value.x]

        trade_chg <- trade_chg[, c("hscode", "hscode_name", "C_M_sigma", "trade_chg", "contrib"), with = TRUE]
        trade_chg[, trade_chg := 100 * trade_chg]
        trade_chg[, contrib := 100 * contrib]

        colnames(trade_chg) <- c(
            "HS code", "Description", "$\\mathcal{C}^M_{US, k} / \\sigma_k$",
            "trade chg (%)", "contribution (%)"
        )
	    latex_code <- xtable(trade_chg, include.rownames = FALSE)
	    latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
	    latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
	    latex_code <- sub("\\{llrrr\\}", "{lp{0.75\\\\textwidth}ccc}", latex_code)
        latex_code <- str_replace_all(latex_code, "\\%", "\\\\%")
	    write(latex_code, file.path(STATS_PATH, "measurement", "validation", "decompositions", paste0("decomposition_", cty, ".tex")))

        trade_chg <- copy(trade_chg_copy)
        trade_chg <- trade_chg[order(contrib)]
        trade_chg <- trade_chg[1:10]
        trade_chg[, trade_chg := perc_value.y - perc_value.x]

        trade_chg <- trade_chg[, c("hscode", "hscode_name", "C_M_sigma", "trade_chg", "contrib"), with = TRUE]
        trade_chg[, trade_chg := 100 * trade_chg]
        trade_chg[, contrib := 100 * contrib]

        colnames(trade_chg) <- c(
            "HS code", "Description", "$\\mathcal{C}^M_{US, k} / \\sigma_k$",
            "trade chg (%)", "contribution (%)"
        )
	    latex_code <- xtable(trade_chg, include.rownames = FALSE)
	    latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
	    latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
	    latex_code <- sub("\\{llrrr\\}", "{lp{0.75\\\\textwidth}ccc}", latex_code)
        latex_code <- str_replace_all(latex_code, "\\%", "\\\\%")
	    write(latex_code, file.path(STATS_PATH, "measurement", "validation", "decompositions", paste0("decomposition_rev_", cty, ".tex")))
    }
}


build_hscode_cty_event <- function() {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)
    panel <- fread(file.path(OUTPUT_PATH, "measurement", "validation", "event_flows.csv"), keepLeadingZeros = TRUE)

    cent <- cent[, c("hs12", "hscode_name", "C_M_sigma")]
    cent <- merge_df(panel, cent, by.x = c("hscode"), by.y = c("hs12"), how = "inner")
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

        trade_cty <- trade[, sum(C_M_sigma * value), by = c("year", "exporter_code", "hscode", "hscode_name")]
        trade_cty <- merge_df(trade_cty, trade_sum, by = "year", how = "inner", indicator = FALSE)
        colnames(trade_cty) <- c("year", "exporter_code", "hscode", "hscode_name", "value", "value_sum")
        trade_cty[, value_perc := value / value_sum]

        trade_year1 <- trade_cty[year == yr_start]
        trade_year2 <- trade_cty[year == yr_end]
        trade_chg <- merge_df(trade_year1, trade_year2, by = c("exporter_code", "hscode", "hscode_name"), how = "outer")
        trade_chg[is.na(value_perc.x), value_perc.x := 0]
        trade_chg[is.na(value_perc.y), value_perc.y := 0]
        trade_chg[, delta_C_mean := value_perc.y - value_perc.x]
        trade_chg <- trade_chg[order(-delta_C_mean)]
        if (cty == "CHN") {
            trade_chg <- trade_chg[order(delta_C_mean)]
        }
        fwrite(trade_chg, file.path(STATS_PATH, "measurement", "validation", "decompositions", paste0("trade_chg_", cty, ".csv")))

        trade_chg_copy <- copy(trade_chg)
        trade_chg <- trade_chg[, c("hscode", "hscode_name", "exporter_code", "delta_C_mean"), with = TRUE]
        trade_chg[, delta_C_mean := 100 * delta_C_mean]
        trade_chg <- trade_chg[1:20]
        trade_chg[exporter_code == "S19", exporter_code := "TWN"]
        
        colnames(trade_chg) <- c(
            "HS code", "Description", "ISO", "chg (%)"
        )
	    latex_code <- xtable(trade_chg, include.rownames = FALSE)
	    latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
	    latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
	    latex_code <- sub("\\{lllr\\}", "{lp{0.75\\\\textwidth}cc}", latex_code)
        latex_code <- str_replace_all(latex_code, "\\%", "\\\\%")
	    write(latex_code, file.path(STATS_PATH, "measurement", "validation", "decompositions", paste0("hscodecty_", cty, ".tex")))

        trade_chg <- copy(trade_chg_copy)
        trade_chg <- trade_chg[order(delta_C_mean)]
        if (cty == "CHN") {
            trade_chg <- trade_chg[order(-delta_C_mean)]
        }
        trade_chg <- trade_chg[, c("hscode", "hscode_name", "exporter_code", "delta_C_mean"), with = TRUE]
        trade_chg[, delta_C_mean := 100 * delta_C_mean]
        trade_chg <- trade_chg[1:20]
        trade_chg[exporter_code == "S19", exporter_code := "TWN"]

        colnames(trade_chg) <- c(
            "HS code", "Description", "ISO", "chg (%)"
        )
	    latex_code <- xtable(trade_chg, include.rownames = FALSE)
	    latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
	    latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
	    latex_code <- sub("\\{lllr\\}", "{lp{0.75\\\\textwidth}cc}", latex_code)
        latex_code <- str_replace_all(latex_code, "\\%", "\\\\%")
	    write(latex_code, file.path(STATS_PATH, "measurement", "validation", "decompositions", paste0("hscodecty_rev_", cty, ".tex")))
    }
}
 

build_hscode_decompositions()
build_hscode_cty_event()

