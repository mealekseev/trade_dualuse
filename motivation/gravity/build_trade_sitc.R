### Construct SITC code data

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


# run a sitc code loop
construct_trade <- function() {
    sitc_stats <- fread(file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_sitc2.csv"), keepLeadingZeros = TRUE)
    sitc_stats[, dualuse := pmax(dualuse_2018, military)]
    sitc_stats <- sitc_stats[, c("sitc75", "dualuse")]
    colnames(sitc_stats) <- c("sitc", "dualuse")

    df_cty <- fread(file.path(DATA_PATH, "motivation", "trade", "trade_atlas", "location_manual.csv"))
    df_cty <- df_cty[, c("location_code", "level", "modern_bloc", "gokmen_bloc", "cold_war_bloc", "manual_bloc")]
    for (col in c("modern_bloc", "gokmen_bloc", "cold_war_bloc", "manual_bloc")) {
        df_cty[get(col) == "", (col) := 0]
        df_cty[get(col) == "Western", (col) := 1]
        df_cty[get(col) == "Eastern", (col) := 2]
        df_cty[, (col) := as.integer(get(col))]
    }
    df_cty_exp <- copy(df_cty)
    colnames(df_cty_exp) <- paste0(colnames(df_cty_exp), "_exp")
    df_cty_imp <- copy(df_cty)
    colnames(df_cty_imp) <- paste0(colnames(df_cty_imp), "_imp")
    yr_list <- seq(1962, 2021, 1)

    sitc_list <- unique(sitc_stats$sitc)
    sitc_1digit_list <- unique(substr(sitc_stats$sitc, 1, 1))
    sitc_2digit_list <- unique(substr(sitc_stats$sitc, 1, 2))
    sitc_4digit_list <- unique(substr(sitc_stats$sitc, 1, 4))
    j <- 1
    for (j in seq_along(sitc_4digit_list)) {
        sitc_4digit <- sitc_4digit_list[j]
        sitc_subset <- sitc_list[substr(sitc_list, 1, 4) %in% sitc_4digit_list]

        panel_list <- list()
        i <- 1
        for (i in seq_along(yr_list)) {
            yr <- yr_list[i]
            print(paste0("Processing ", yr, "..."))
            
            df <- data.table(
                read_dta(file.path(DATA_PATH, "motivation", "trade", "trade_atlas",
                    paste0("country_partner_sitcproduct4digit_year_", yr, ".dta")))
            )
            exporter_list <- unique(df$location_code)
            importer_list <- unique(df$partner_code)
            df <- df[sitc_product_code %in% sitc_subset]
            df_export <- df[, c("location_code", "partner_code", "sitc_product_code", "export_value")]
            colnames(df_export) <- c("exporter_code", "importer_code", "sitc", "value")
            
            df_import <- df[, c("partner_code", "location_code", "sitc_product_code", "import_value")]
            colnames(df_import) <- c("exporter_code", "importer_code", "sitc", "value")
            df <- rbind(df_export, df_import)
            df <- df[, mean(value), by = c("exporter_code", "importer_code", "sitc")]
            colnames(df) <- c("exporter_code", "importer_code", "sitc", "value")
            
            panel <- CJ(c(yr), exporter_list, importer_list, sitc_subset)
            colnames(panel) <- c("year", "exporter_code", "importer_code", "sitc")
            panel <- merge_df(panel, df_cty_exp, by.x = "exporter_code", by.y = "location_code_exp", how = "left", allow.cartesian = TRUE, indicator = FALSE)            
            panel <- merge_df(panel, df_cty_imp, by.x = "importer_code", by.y = "location_code_imp", how = "left", allow.cartesian = TRUE, indicator = FALSE)
            panel <- merge_df(panel, sitc_stats, by = "sitc", how = "left", allow.cartesian = TRUE, indicator = FALSE)    
            panel <- merge_df(panel, df, by = c("exporter_code", "importer_code", "sitc"),
                how = "left", allow.cartesian = TRUE, indicator = FALSE)        
            panel[is.na(value), value := 0]
            panel <- panel[level_exp == "country" & level_imp == "country"]
            panel[, c("level_exp", "level_imp") := NULL]
            panel <- panel[exporter_code != importer_code]
            panel_list[[i]] <- panel
        }
        panel <- rbindlist(panel_list)

        for (s in sitc_subset) {
            fwrite(panel[sitc == s], file.path(EXTERNAL_PATH, "output", "motivation", "gravity", "sitc2", paste0("gravity_sitc2_", s, ".csv")))
        }
    }
}


check_all_files <- function() {
    file_list <- list.files(file.path(EXTERNAL_PATH, "output", "motivation", "gravity", "sitc2"))
    file_list <- substr(file_list, 14, 17)
    yr_list <- seq(1962, 2021)
    i <- 1
    sitc_list <- c()
    for (i in seq_along(yr_list)) {
        yr <- yr_list[i]
        print(paste0("Processing ", yr, "..."))
        df <- data.table(
            read_dta(file.path(DATA_PATH, "motivation", "trade", "trade_atlas",
                paste0("country_partner_sitcproduct4digit_year_", yr, ".dta")))
        )
        sitc_list <- unique(c(sitc_list, as.character(df$sitc_product_code)))
    }
    print(setdiff(sitc_list, file_list))
    print(setdiff(file_list, sitc_list))
}


construct_trade()
# check_all_files()

