### Plot GTA

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
source("./motivation/policy/policy_names.R")


output_gta <- function() {
    load(file.path(DATA_PATH, "motivation", "policy", "GTA+Database+2008-latest.Rdata"))
    df <- data.table(master)
    colnames(df) <- c(
        "id_act", "id_policy", "act_title", "date_announced", "gta_eval", "is_active",
        "date_started", "date_ended", "country", "gov_level", "firms_elibible", "type_policy",
        "chapter", "sectors", "products", "country_against"
    )
    df <- df[!is.na(products)]
    df[, id := seq(1, nrow(df))]
    df[, year := year(date_announced)]
    df[, gta_export := as.integer(type_policy %in% export_policies)]
    df[, gta_security := as.integer(type_policy %in% security_policies)]

    # export
    df_export <- df[year >= 2022 & gta_export == 1 & country == "United States of America"]
    df_export_unlist <- df_export[, list(hscode = unlist(strsplit(products, split = ",\\s*"))), by = id]
    df_export <- merge_df(df_export, df_export_unlist, by = "id", how = "left")
    df_export[startsWith(hscode, "2710"), hscode := "271000"]
    df_export <- df_export[, .SD[1], .SDcols = c("year", "type_policy"), by = c("id_policy", "hscode")]

    # security
    df_security <- df[year >= 2022 & gta_security == 1 & country == "United States of America"]
    df_security_unlist <- df_security[, list(hscode = unlist(strsplit(products, split = ",\\s*"))), by = id]
    df_security <- merge_df(df_security, df_security_unlist, by = "id", how = "left")
    df_security[startsWith(hscode, "2710"), hscode := "271000"]
    df_security <- df_security[, .SD[1], .SDcols = c("year", "type_policy"), by = c("id_policy", "hscode")]

    # counts by period
    df_unlist <- df[, list(hscode = unlist(strsplit(products, split = ",\\s*"))), by = id]
    df <- merge_df(df, df_unlist, by = "id", how = "left")
    df <- df[, .SD[1], .SDcols = c("year", "gta_export", "gta_security"), by = c("id_policy", "hscode")]
    df[startsWith(hscode, "2710"), hscode := "271000"]
    df <- df[, lapply(.SD, sum), .SDcols = c("gta_export", "gta_security"), by = c("year", "hscode")]
    df[, period := ""]
    df[year >= 2022 & year <= 2023, period := "2022"]
    df[year >= 2020 & year <= 2021, period := "2020"]
    df[year >= 2018 & year <= 2019, period := "2018"]
    df <- df[period != ""]
    df <- df[, lapply(.SD, sum), .SDcols = c("gta_export", "gta_security"), by = c("hscode", "period")]
    df <- dcast(df, hscode ~ period, value.var = c("gta_export", "gta_security"), fill = 0)

    # 6-digit
    hscode <- fread(file.path(OUTPUT_PATH, "crosswalks", "H4_codes.csv"), keepLeadingZeros = TRUE)
    hscode <- hscode[, c("sector")]
    hscode <- hscode[nchar(sector) == 6]
    hscode[startsWith(sector, "2710"), sector := "271000"]
    hscode <- unique(hscode)
    hscode[sector %in% df_export$hscode, gta_export_usa_post22 := 1]
    hscode[sector %in% df_security$hscode, gta_security_usa_post22 := 1]
    hscode <- merge_df(hscode, df, by.x = "sector", by.y = "hscode", how = "left", indicator = FALSE)
    hscode[is.na(hscode)] <- 0
    fwrite(hscode, file.path(OUTPUT_PATH, "motivation", "policy", "gta_policies_hs4.csv"))

    # 4-digit
    hscode <- fread(file.path(OUTPUT_PATH, "crosswalks", "H4_codes.csv"), keepLeadingZeros = TRUE)
    hscode <- hscode[, c("sector")]
    hscode <- hscode[nchar(sector) == 4]
    hscode[sector %in% substr(df_export$hscode, 1, 4), gta_export_usa_post22 := 1]
    hscode[sector %in% substr(df_security$hscode, 1, 4), gta_security_usa_post22 := 1]
    df[, hscode := substr(hscode, 1, 4)]
    df <- df[, lapply(.SD, sum), .SDcols = c("gta_export_2018", "gta_export_2020", "gta_export_2022", "gta_security_2018", "gta_security_2020", "gta_security_2022"), by = c("hscode")] 
    hscode <- merge_df(hscode, df, by.x = "sector", by.y = "hscode")
    hscode[is.na(hscode)] <- 0
    fwrite(hscode, file.path(OUTPUT_PATH, "motivation", "policy", "gta_policies_hs4_4digit.csv"))
}


output_gta()

