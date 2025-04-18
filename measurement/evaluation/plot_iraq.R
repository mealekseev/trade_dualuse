### Summarize trade over time

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(stringr)
library(scales)
library(fixest)
library(haven)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


collapse_iraq_imports <- function(year) {
    # read and process the data
    df <- data.table(
        read_dta(file.path(DATA_PATH, 'motivation', 'trade', 'trade_atlas',
            paste0('country_partner_sitcproduct4digit_year_', year, '.dta')))
    )
    df_export <- df[, c("location_code", "partner_code", "sitc_product_code", "export_value")]
    colnames(df_export) <- c("exporter_code", "importer_code", "sitc", "value")
    df_import <- df[, c("partner_code", "location_code", "sitc_product_code", "import_value")]
    colnames(df_import) <- c("exporter_code", "importer_code", "sitc", "value")
    df <- rbind(df_export, df_import)
    df <- df[, mean(value), by = c("exporter_code", "importer_code", "sitc")]
    colnames(df) <- c("exporter_code", "importer_code", "sitc", "value")
    df[, value := value / 1000]
    df <- df[sitc != "ZZZZ"]
    df <- df[exporter_code != importer_code]
    df <- df[value != 0]
    df <- df[importer_code == "IRQ"]

    # crosswalk into hs12
    cw <- fread(file.path(OUTPUT_PATH, "crosswalks", "sitc75_hs12.csv"), keepLeadingZeros = TRUE)
    df <- merge_df(df, cw, by.x = "sitc", by.y = "sitc75", how = "left", allow.cartesian = TRUE)
    df[, value := value * wgt]
    df <- df[, sum(value), by = c("exporter_code", "importer_code", "hs12")]
    setnames(df, "V1", "value")

    # merge in centrality
    cent <- fread(file.path(OUTPUT_PATH, 'measurement', 'centrality', 'centrality_hs12.csv'), keepLeadingZeros = TRUE)
    cent <- cent[, c("hs12", "C_M_sigma", "rank_C_M_sigma")]
    df <- merge_df(df, cent, by = "hs12", how = "inner")
    
    # run regresion
    reg <- feols(rank_C_M_sigma ~ 1, data = df, weight = df[, value])
    coef <- reg$coefficients[1]
    se <- sqrt(vcov(reg, se = "hetero")[1, 1])
    coeftable <- data.table(
        coef = coef,
        se = se
    )
    coeftable[, coef_lower := coef - 1.96 * se]
    coeftable[, coef_upper := coef + 1.96 * se]
    coeftable[, year := year]
    return(coeftable)
}


save_iraq_imports <- function() {
    coef_list <- list()
    ix <- 1
    for (year in 1962:2021) {
        print(year)
        coeftable <- collapse_iraq_imports(year)
        coef_list[[ix]] <- coeftable
        ix <- ix + 1
    }
    coeftable <- rbindlist(coef_list)
    fwrite(coeftable, file.path(OUTPUT_PATH, "measurement", "evaluation", "iraq_imports.csv"))
}


plot_iraq_imports <- function() {
    coefplot <- fread(file.path(OUTPUT_PATH, "measurement", "evaluation", "iraq_imports.csv"))
    ggplot(coefplot, aes(x = year, y = coef)) +
        geom_line(linewidth = 1.0, color = royalblue) +
        geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
        geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
        geom_vline(xintercept = 1975, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 1990, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 2001, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 2014, linetype = "dashed", color = "black") +
        geom_text(x = 1975.5, hjust = 0, y = 0.37, label = "Iran-Iraq War", family = "Erewhon", size = 5) + 
        geom_text(x = 1990.5, hjust = 0, y = 0.57, label = "Gulf War", family = "Erewhon", size = 5) + 
        geom_text(x = 2001.5, hjust = 0, y = 0.57, label = "War on Terror", family = "Erewhon", size = 5) + 
        geom_text(x = 2014.5, hjust = 0, y = 0.57, label = "ISIS Threat", family = "Erewhon", size = 5) + 
        scale_y_continuous(labels = scales::percent) +
        labs(x = "", y = "Avg. mil. use pct") +
        custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "flows", "iraq_imports.jpeg"), width = 10, height = 4, dpi = 300)

    ggplot(coefplot, aes(x = year, y = coef)) +
        geom_line(linewidth = 1.0, color = royalblue, alpha = 0.0) +
        geom_point(size = 3, shape = 1, stroke = 1, color = royalblue, alpha = 0.0) +
        geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.0) +
        geom_vline(xintercept = 1975, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 1990, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 2001, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 2014, linetype = "dashed", color = "black") +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "", y = "Avg. mil. use pct") +
        custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "flows", "iraq_imports_empty.jpeg"), width = 10, height = 4, dpi = 300)
}


# save_iraq_imports()
plot_iraq_imports()

