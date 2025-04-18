### Construct HS code data

rm(list=ls())
gc()

library(data.table)
library(stringr)
library(haven)
library(fixest)
library(zoo)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


construct_trade <- function() {
    hscode_stats <- fread(file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs0.csv"), keepLeadingZeros = TRUE)
    hscode_stats[, dualuse := pmax(dualuse_2018, military)]
    hscode_stats <- hscode_stats[, c("hs92", "dualuse")]
    colnames(hscode_stats) <- c("hscode", "dualuse")
    
    df_cty <- fread(file.path(DATA_PATH, "motivation", "trade", "BACI_HS92_V202401b", "country_codes_V202401b_manual.csv"))
    df_cty <- df_cty[, c("country_code", "country_iso3", "modern_bloc", "gokmen_bloc", "cold_war_bloc", "manual_bloc")]
    for (col in c("modern_bloc", "gokmen_bloc", "cold_war_bloc", "manual_bloc")) {
        df_cty[get(col) == "", (col) := 0]
        df_cty[get(col) == "Western", (col) := 1]
        df_cty[get(col) == "Eastern", (col) := 2]
        df_cty[, (col) := as.integer(get(col))]
    }
    df_cty_exp <- copy(df_cty)
    colnames(df_cty_exp) <- c("exporter_code", "exporter_iso3", "modern_bloc_exp", "gokmen_bloc_exp", "cold_war_bloc_exp", "manual_bloc_exp")
    df_cty_imp <- copy(df_cty)
    colnames(df_cty_imp) <- c("importer_code", "importer_iso3", "modern_bloc_imp", "gokmen_bloc_imp", "cold_war_bloc_imp", "manual_bloc_imp")
    yr_list <- seq(1995, 2022, 1)

    hscode_list <- unique(hscode_stats$hscode)
    hscode_2digit_list <- unique(substr(hscode_stats$hscode, 1, 2))
    hscode_4digit_list <- unique(substr(hscode_stats$hscode, 1, 4))
    j <- 1
    for (j in seq_along(hscode_2digit_list)) {
        hscode_2digit <- hscode_2digit_list[j]
        hscode_subset <- hscode_list[substr(hscode_list, 1, 2) == hscode_2digit]
        i <- 1

        panel_list <- list()
        for (i in seq_along(yr_list)) {
            yr <- yr_list[i]
            print(paste0("Processing ", yr, "..."))
            df <- fread(file.path(DATA_PATH, "motivation", "trade", "BACI_HS92_V202401b", paste0("BACI_HS92_Y", yr, "_V202401b.csv")), keepLeadingZeros = TRUE)
            exporter_list <- unique(df$i)
            importer_list <- unique(df$j)
            colnames(df) <- c("year", "exporter_code", "importer_code", "hscode", "value", "quantity")
            df[, quantity := NULL]
            
            panel <- CJ(c(yr), exporter_list, importer_list, hscode_subset)
            colnames(panel) <- c("year", "exporter_code", "importer_code", "hscode")
            panel <- merge_df(panel, df_cty_exp, by = "exporter_code", how = "left", allow.cartesian = TRUE, indicator = FALSE)            
            panel <- merge_df(panel, df_cty_imp, by = "importer_code", how = "left", allow.cartesian = TRUE, indicator = FALSE)
            panel <- merge_df(panel, hscode_stats, by = "hscode", how = "left", allow.cartesian = TRUE, indicator = FALSE)    
            panel <- merge_df(panel, df, by = c("year", "exporter_code", "importer_code", "hscode"),
                how = "left", allow.cartesian = TRUE, indicator = FALSE)        
            panel[is.na(value), value := 0]
            panel[, c("exporter_code", "importer_code") := NULL]
            panel_list[[i]] <- panel
        }
        panel <- rbindlist(panel_list)
        panel <- panel[exporter_iso3 != importer_iso3]

        for (h in hscode_subset) {
            fwrite(panel[hscode == h], file.path(EXTERNAL_PATH, "output", "motivation", "gravity", "hs0", paste0("gravity_hs0_", h, ".csv")))
        }
    }
}


check_all_files <- function() {
    file_list <- list.files(file.path(EXTERNAL_PATH, "output", "motivation", "gravity", "hs0"))
    file_list <- substr(file_list, 14, 17)
    yr_list <- seq(1995, 2022, 1)

    i <- 1
    hscode_list <- c()
    for (i in seq_along(yr_list)) {
        yr <- yr_list[i]
        print(paste0("Processing ", yr, "..."))
        df <- fread(file.path(DATA_PATH, "motivation", "trade", "BACI_HS92_V202401b", paste0("BACI_HS92_Y", yr, "_V202401b.csv")), keepLeadingZeros = TRUE)
        hscode_list <- unique(c(hscode_list, as.character(df$k)))
    }
    print(setdiff(hscode_list, file_list))
    print(setdiff(file_list, hscode_list))
}


construct_trade()
# check_all_files()


