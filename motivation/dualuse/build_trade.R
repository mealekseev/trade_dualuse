### Construct trade data

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(stringr)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


get_trade_12_22 <- function() {
    year_list <- seq(2012, 2022)
    trade_dict <- fread(file.path(DATA_PATH, "motivation", "trade", "BACI_HS12_V202401b", "country_codes_V202401b.csv"), keepLeadingZeros = TRUE)
    trade_dict_xpt <- copy(trade_dict[, c("country_code", "country_iso3"), with = FALSE])
    colnames(trade_dict_xpt) <- c("exporter_code", "exporter_iso3")
    trade_dict_mpt <- copy(trade_dict[, c("country_code", "country_iso3"), with = FALSE])
    colnames(trade_dict_mpt) <- c("importer_code", "importer_iso3")
    df_trade <- list()
    i <- 1
    for (i in seq_along(year_list)) {
        yr <- year_list[i]
        print(paste0("Processing year ", yr, "..."))
        df <- fread(file.path(DATA_PATH, "motivation", "trade", "BACI_HS12_V202401b", paste0("BACI_HS12_Y", yr, "_V202401b.csv")), keepLeadingZeros = TRUE)
        df[, v := as.numeric(v)]
        df <- df[, c("t", "i", "j", "k", "v"), with = FALSE]
        df <- df[i != j]
        colnames(df) <- c("period", "exporter_code", "importer_code", "hscode", "value")
        df <- merge_df(df, trade_dict_xpt, by = "exporter_code", how = "left")
        df <- merge_df(df, trade_dict_mpt, by = "importer_code", how = "left")
        df[is.na(exporter_iso3), exporter_iso3 := exporter_code]
        df[is.na(importer_iso3), importer_iso3 := importer_code]
        df <- df[, c("period", "exporter_iso3", "importer_iso3", "hscode", "value"), with = FALSE]
        df_trade[[i]] <- df
    }
    df_trade <- rbindlist(df_trade)
    setnames(df_trade, "value", "value_baci")
    fwrite(df_trade, file.path(OUTPUT_PATH, "motivation", "trade", "trade_12_22.csv"))
}


get_trade_shares_15_19 <- function() {
    trade <- fread(file.path(OUTPUT_PATH, "motivation", "trade", "trade_12_22.csv"), keepLeadingZeros = TRUE)
    trade <- trade[period >= 2015 & period <= 2019]
    trade <- trade[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("value_baci"), by = c("hscode")]
    trade[, value_sum := sum(value_baci, na.rm = TRUE)]
    trade[, perc_value := value_baci / value_sum]
    fwrite(trade, file.path(OUTPUT_PATH, "motivation", "trade", "trade_shares_15_19.csv"))
}


get_trade_12_22()
get_trade_shares_15_19()

