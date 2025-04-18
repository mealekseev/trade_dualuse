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


EXTERNAL_PATH <- "/Volumes/Seagate/trade_defense"
year <- 2005
collapse_year <- function(year) {
    df <- data.table(
        read_dta(file.path(DATA_PATH, "trade_atlas_double", "dataverse_files",
            paste0("country_partner_sitcproduct4digit_year_", year, ".dta")))
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
    
    cw <- fread(file.path(OUTPUT_PATH, "crosswalks", "sitc_hs12.csv"), keepLeadingZeros = TRUE)
    df <- merge_df(df, cw, by = "sitc", how = "left", allow.cartesian = TRUE)
    df[, value := value * wgt]
    df <- df[, sum(value), by = c("exporter_code", "importer_code", "hs12")]
    setnames(df, "V1", "value")
    df <- df[exporter_code != importer_code]
    df <- df[value != 0]

    cent <- fread(file.path(OUTPUT_PATH, "network_stats", 'centrality_hs12.csv'), keepLeadingZeros = TRUE)
    cent <- cent[, c("hs12", "C_M_sigma", "rank_C_M_sigma")]

    df <- merge_df(df, cent, by = "hs12", how = "inner")
    df[, pair_id := paste0(exporter_code, "-", importer_code)]
    exporter_id <- unique(df$exporter_code)
    importer_id <- unique(df$importer_code)
    pair_id <- unique(df$pair_id)

    coef_list <- list()
    ix <- 1
    for (exp_id in exporter_id) {
        print(paste0("Exporters: ", exp_id))
        if (min(df[exporter_code == exp_id, C_M_sigma]) == max(df[exporter_code == exp_id, C_M_sigma])) {
            coef_table <- data.table(
                exporter_id = exp_id,
                importer_id = exp_id,
                type = "e",
                cent = "raw",
                coef = df[exporter_code == exp_id, C_M_sigma],
                se = 0
            )
        } else {
            reg <- feols(C_M_sigma ~ 1, data = df[exporter_code == exp_id], weight = df[exporter_code == exp_id, value])
            coef <- reg$coefficients[1]
            se <- vcov(reg, se = "hetero")[1, 1]
            coef_table <- data.table(
                exporter_id = exp_id,
                importer_id = exp_id,
                type = "e",
                cent = "raw",
                coef = coef,
                se = se
            )
        }
        coef_list[[ix]] <- coef_table
        ix <- ix + 1

        if (min(df[exporter_code == exp_id, rank_C_M_sigma]) == max(df[exporter_code == exp_id, rank_C_M_sigma])) {
            coef_table <- data.table(
                exporter_id = exp_id,
                importer_id = exp_id,
                type = "e",
                cent = "pct",
                coef = df[exporter_code == exp_id, rank_C_M_sigma],
                se = 0
            )
        } else {
            reg <- feols(rank_C_M_sigma ~ 1, data = df[exporter_code == exp_id], weight = df[exporter_code == exp_id, value])
            coef <- reg$coefficients[1]
            se <- vcov(reg, se = "hetero")[1, 1]
            coef_table <- data.table(
                exporter_id = exp_id,
                importer_id = exp_id,
                type = "e",
                cent = "pct",
                coef = coef,
                se = se
            )
        }
        coef_list[[ix]] <- coef_table
        ix <- ix + 1
    }

    for (imp_id in importer_id) {
        print(paste0("Importers: ", imp_id))
        if (min(df[importer_code == imp_id, C_M_sigma]) == max(df[importer_code == imp_id, C_M_sigma])) {
            coef_table <- data.table(
                exporter_id = imp_id,
                importer_id = imp_id,
                type = "i",
                cent = "raw",
                coef = df[importer_code == imp_id, C_M_sigma],
                se = 0
            )
        } else {
            reg <- feols(C_M_sigma ~ 1, data = df[importer_code == imp_id], weight = df[importer_code == imp_id, value])
            coef <- reg$coefficients[1]
            se <- vcov(reg, se = "hetero")[1, 1]
            coef_table <- data.table(
                exporter_id = imp_id,
                importer_id = imp_id,
                type = "i",
                cent = "raw",
                coef = coef,
                se = se
            )
        }
        coef_list[[ix]] <- coef_table
        ix <- ix + 1

        if (min(df[importer_code == imp_id, C_M_sigma]) == max(df[importer_code == imp_id, C_M_sigma])) {
            coef_table <- data.table(
                exporter_id = imp_id,
                importer_id = imp_id,
                type = "i",
                cent = "pct",
                coef = df[importer_code == imp_id, rank_C_M_sigma],
                se = 0
            )
        } else {
            reg <- feols(rank_C_M_sigma ~ 1, data = df[importer_code == imp_id], weight = df[importer_code == imp_id, value])
            coef <- reg$coefficients[1]
            se <- vcov(reg, se = "hetero")[1, 1]
            coef_table <- data.table(
                exporter_id = imp_id,
                importer_id = imp_id,
                type = "i",
                cent = "pct",
                coef = coef,
                se = se
            )
        }
        coef_list[[ix]] <- coef_table
        ix <- ix + 1
    }

    for (p_id in pair_id) {
        print(paste0("Pair: ", p_id))
        exp_id <- str_split(p_id, "-")[[1]][1]
        imp_id <- str_split(p_id, "-")[[1]][2]
        
        if (min(df[pair_id == p_id, C_M_sigma]) == max(df[pair_id == p_id, C_M_sigma])) {
            coef_table <- data.table(
                exporter_id = exp_id,
                importer_id = imp_id,
                type = "p",
                cent = "raw",
                coef = df[pair_id == p_id, C_M_sigma],
                se = 0
            )
        } else {
            reg <- feols(C_M_sigma ~ 1, data = df[pair_id == p_id], weight = df[pair_id == p_id, value])
            coef <- reg$coefficients[1]
            se <- vcov(reg, se = "hetero")[1, 1]
            coef_table <- data.table(
                exporter_id = exp_id,
                importer_id = imp_id,
                type = "p",
                cent = "raw",
                coef = coef,
                se = se
            )
        }
        coef_list[[ix]] <- coef_table
        ix <- ix + 1

        if (min(df[pair_id == p_id, rank_C_M_sigma]) == max(df[pair_id == p_id, rank_C_M_sigma])) {
            coef_table <- data.table(
                exporter_id = exp_id,
                importer_id = imp_id,
                type = "p",
                cent = "pct",
                coef = df[pair_id == p_id, rank_C_M_sigma],
                se = 0
            )
        } else {
            reg <- feols(rank_C_M_sigma ~ 1, data = df[pair_id == p_id], weight = df[pair_id == p_id, value])
            coef <- reg$coefficients[1]
            se <- vcov(reg, se = "hetero")[1, 1]
            coef_table <- data.table(
                exporter_id = exp_id,
                importer_id = imp_id,
                type = "p",
                cent = "pct",
                coef = coef,
                se = se
            )
        }
        coef_list[[ix]] <- coef_table
        ix <- ix + 1
    }
    coeftable <- rbindlist(coef_list)
    coeftable[, year := year]
    return(coeftable)
}


save_years <- function() {
    for (year in 2003:2021) {
        print(year)
        coeftable <- collapse_year(year)
        fwrite(coeftable, file.path(OUTPUT_PATH, "trade_patterns", "sitc", paste0(year, ".csv")))
    }
}


# save_years()


plot_counties <- function() {
    coef_list <- list()
    ix <- 1
    for (year in 1962:2021) {
        print(year)
        coef_table <- fread(file.path(OUTPUT_PATH, "trade_patterns", "sitc", paste0(year, ".csv")), keepLeadingZeros = TRUE)
        coef_list[[ix]] <- coef_table
        ix <- ix + 1
    }
    coeftable <- rbindlist(coef_list)

    importer_id <- unique(coeftable$importer_id)
    exporter_id <- unique(coeftable$exporter_id)
    coeftable[, pair_id := paste0(exporter_id, "-", importer_id)]
    coeftable[, se := sqrt(se)]
    coeftable[, coef_lower := coef - 1.96 * se]
    coeftable[, coef_upper := coef + 1.96 * se]
    pair_id <- unique(coeftable$pair_id)
    
    exp_id <- exporter_id[1]
    for (exp_id in exporter_id) {
        print(paste0("Exporter: ", exp_id))
        coefplot <- coeftable[exporter_id == exp_id & type == "e" & cent == "raw"]
        ggplot(coefplot, aes(x = year, y = coef)) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
            geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
            labs(x = "", y = paste0("Avg. exp. mil. use, ", coefplot$exporter_id[1])) +
            custom_theme_slides
        ggsave(file.path(EXTERNAL_PATH, "trade_patterns", "sitc", paste0("exp_", coefplot$exporter_id[1], "_raw.jpeg")), width = 10, height = 4, dpi = 300)
        
        coefplot <- coeftable[exporter_id == exp_id & type == "e" & cent == "pct"]
        ggplot(coefplot, aes(x = year, y = coef)) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
            geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
            labs(x = "", y = paste0("Avg. exp. mil. use, ", coefplot$exporter_id[1])) +
            custom_theme_slides
        ggsave(file.path(EXTERNAL_PATH, "trade_patterns", "sitc", paste0("exp_", coefplot$exporter_id[1], "_pct.jpeg")), width = 10, height = 4, dpi = 300)
    }

    for (imp_id in importer_id) {
        print(paste0("Importer: ", imp_id))
        coefplot <- coeftable[importer_id == imp_id & type == "i" & cent == "raw"]
        ggplot(coefplot, aes(x = year, y = coef)) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
            geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
            labs(x = "", y = paste0("Avg. imp. mil. use, ", coefplot$exporter_id[1])) +
            custom_theme_slides
        ggsave(file.path(EXTERNAL_PATH, "trade_patterns", "sitc", paste0("imp_", coefplot$exporter_id[1], "_raw.jpeg")), width = 10, height = 4, dpi = 300)
        
        coefplot <- coeftable[importer_id == imp_id & type == "i" & cent == "pct"]
        ggplot(coefplot, aes(x = year, y = coef)) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
            geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
            labs(x = "", y = paste0("Avg. imp. mil. use, ", coefplot$importer_id[1])) +
            custom_theme_slides
        ggsave(file.path(EXTERNAL_PATH, "trade_patterns", "sitc", paste0("imp_", coefplot$importer_id[1], "_pct.jpeg")), width = 10, height = 4, dpi = 300)
    }

    for (p_id in pair_id) {
        print(paste0("Pair: ", p_id))
        coefplot <- coeftable[pair_id == p_id & type == "p" & cent == "raw"]
        ggplot(coefplot, aes(x = year, y = coef)) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
            geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
            labs(x = "", y = paste0("Avg. flow mil. use, ", coefplot$exporter_id[1], "-", coefplot$importer_id[1])) +
            custom_theme_slides
        ggsave(file.path(EXTERNAL_PATH, "trade_patterns", "sitc", paste0("pr_", coefplot$exporter_id[1], "_", coefplot$importer_id[1], "_raw.jpeg")), width = 10, height = 4, dpi = 300)
        
        coefplot <- coeftable[pair_id == p_id & type == "p" & cent == "pct"]
        ggplot(coefplot, aes(x = year, y = coef)) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_point(size = 3, shape = 1, stroke = 1, color = royalblue) +
            geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
            labs(x = "", y = paste0("Avg. flow mil. use, ", coefplot$exporter_id[1], "-", coefplot$importer_id[1])) +
            custom_theme_slides
        ggsave(file.path(EXTERNAL_PATH, "trade_patterns", "sitc", paste0("pr_", coefplot$exporter_id[1], "_", coefplot$importer_id[1], "_pct.jpeg")), width = 10, height = 4, dpi = 300)
    }
}


plot_counties()

