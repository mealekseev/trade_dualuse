### Summarize trade over time

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(stringr)
library(scales)
library(fixest)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


collapse_year <- function(year) {
    df <- fread(
        file.path(DATA_PATH, "cepii", "BACI_HS92_V202401b", paste0("BACI_HS92_Y", year, "_V202401b.csv")),
        keepLeadingZeros = TRUE    
    )
    df[, (c("t", "q")) := NULL]

    cw <- fread(file.path(OUTPUT_PATH, "crosswalks", "hs92_hs12.csv"), keepLeadingZeros = TRUE)
    df <- merge_df(df, cw, by.x = "k", by.y = "hs92", how = "left", allow.cartesian = TRUE)
    df[, v := v * wgt]
    df <- df[, sum(v), by = c("i", "j", "hs12")]
    setnames(df, "V1", "v")
    df <- df[i != j]

    cent <- fread(file.path(OUTPUT_PATH, "network_stats", 'centrality_hs12.csv'), keepLeadingZeros = TRUE)
    cent <- cent[, c("hs12", "C_M_sigma", "rank_C_M_sigma")]

    df <- merge_df(df, cent, by = "hs12", how = "inner")
    df[, pair_id := paste0(i, "-", j)]
    exporter_id <- unique(df$i)
    importer_id <- unique(df$j)
    pair_id <- unique(df$pair_id)
    # exporter_id <- exporter_id[1:3]
    # importer_id <- importer_id[1:3]
    # pair_id <- pair_id[1:3]

    coef_list <- list()
    ix <- 1
    for (exp_id in exporter_id) {
        print(paste0("Exporters: ", exp_id))
        reg <- feols(C_M_sigma ~ 1, data = df[i == exp_id], weight = df[i == exp_id, v])
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
        coef_list[[ix]] <- coef_table
        ix <- ix + 1

        reg <- feols(rank_C_M_sigma ~ 1, data = df[i == exp_id], weight = df[i == exp_id, v])
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
        coef_list[[ix]] <- coef_table
        ix <- ix + 1
    }

    for (imp_id in importer_id) {
        print(paste0("Importers: ", imp_id))
        reg <- feols(C_M_sigma ~ 1, data = df[j == imp_id], weight = df[j == imp_id, v])
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
        coef_list[[ix]] <- coef_table
        ix <- ix + 1

        reg <- feols(rank_C_M_sigma ~ 1, data = df[j == imp_id], weight = df[j == imp_id, v])
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
            reg <- feols(C_M_sigma ~ 1, data = df[pair_id == p_id], weight = df[pair_id == p_id, v])
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
            reg <- feols(rank_C_M_sigma ~ 1, data = df[pair_id == p_id], weight = df[pair_id == p_id, v])
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

    country_list <- fread(
        file.path(DATA_PATH, "cepii", "BACI_HS92_V202401b", "country_codes_V202401b.csv"),
        keepLeadingZeros = TRUE    
    )
    export_list <- copy(country_list)
    colnames(export_list) <- str_replace(colnames(export_list), "country_", "exporter_")
    export_list[, exporter_code := as.character(exporter_code)]
    import_list <- copy(country_list)
    colnames(import_list) <- str_replace(colnames(import_list), "country_", "importer_")
    import_list[, importer_code := as.character(importer_code)]
    
    coeftable <- merge_df(coeftable, import_list, by.x = "importer_id", by.y = "importer_code", how = "left", allow.cartesian = TRUE, indicator = TRUE)
    if (any(coeftable$merge_ == "left_only")) {
        raise("Missing importer code")
    } else {
        coeftable[, merge_ := NULL]
    }
    coeftable <- merge_df(coeftable, export_list, by.x = "exporter_id", by.y = "exporter_code", how = "left", allow.cartesian = TRUE, indicator = FALSE)
    if (any(coeftable$merge_ == "left_only")) {
        raise("Missing exporter code")
    } else {
        coeftable[, merge_ := NULL]
    }
    coeftable <- coeftable[, c("year", "exporter_id", "exporter_iso2", "exporter_iso3", "importer_id", "importer_iso2", "importer_iso3", "type", "cent", "coef", "se")]
    return(coeftable)
}


save_years <- function() {
    for (year in 1995:2011) {
        print(year)
        coeftable <- collapse_year(year)
        fwrite(coeftable, file.path(OUTPUT_PATH, "trade_patterns", "hs", paste0(year, ".csv")))
    }
}


save_years()


plot_counties <- function() {
    coef_list <- list()
    ix <- 1
    for (year in 1995:2022) {
        print(year)
        coef_table <- fread(file.path(OUTPUT_PATH, "trade_patterns", "hs", paste0(year, ".csv")), keepLeadingZeros = TRUE)
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
            geom_point(size = 4, shape = 1, stroke = 1, color = royalblue) +
            geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
            labs(x = "", y = paste0("Avg. exp. mil. use, ", coefplot$exporter_iso3[1])) +
            custom_theme_slides
        ggsave(file.path(STATS_PATH, "trade_patterns", "hs", paste0("exp_", coefplot$exporter_iso3[1], "_raw.jpeg")), width = 10, height = 4, dpi = 300)
        
        coefplot <- coeftable[exporter_id == exp_id & type == "e" & cent == "pct"]
        ggplot(coefplot, aes(x = year, y = coef)) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_point(size = 4, shape = 1, stroke = 1, color = royalblue) +
            geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
            labs(x = "", y = paste0("Avg. exp. mil. use, ", coefplot$exporter_iso3[1])) +
            custom_theme_slides
        ggsave(file.path(STATS_PATH, "trade_patterns", "hs", paste0("exp_", coefplot$exporter_iso3[1], "_pct.jpeg")), width = 10, height = 4, dpi = 300)
    }

    for (imp_id in importer_id) {
        print(paste0("Importer: ", imp_id))
        coefplot <- coeftable[importer_id == imp_id & type == "i" & cent == "raw"]
        ggplot(coefplot, aes(x = year, y = coef)) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_point(size = 4, shape = 1, stroke = 1, color = royalblue) +
            geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
            labs(x = "", y = paste0("Avg. imp. mil. use, ", coefplot$exporter_iso3[1])) +
            custom_theme_slides
        ggsave(file.path(STATS_PATH, "trade_patterns", "hs", paste0("imp_", coefplot$exporter_iso3[1], "_raw.jpeg")), width = 10, height = 4, dpi = 300)
        
        coefplot <- coeftable[importer_id == imp_id & type == "i" & cent == "pct"]
        ggplot(coefplot, aes(x = year, y = coef)) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_point(size = 4, shape = 1, stroke = 1, color = royalblue) +
            geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
            labs(x = "", y = paste0("Avg. imp. mil. use, ", coefplot$importer_iso3[1])) +
            custom_theme_slides
        ggsave(file.path(STATS_PATH, "trade_patterns", "hs", paste0("imp_", coefplot$importer_iso3[1], "_pct.jpeg")), width = 10, height = 4, dpi = 300)
    }

    for (p_id in pair_id) {
        print(paste0("Pair: ", p_id))
        coefplot <- coeftable[pair_id == p_id & type == "p" & cent == "raw"]
        ggplot(coefplot, aes(x = year, y = coef)) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_point(size = 4, shape = 1, stroke = 1, color = royalblue) +
            geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
            labs(x = "", y = paste0("Avg. flow mil. use, ", coefplot$exporter_iso3[1], "-", coefplot$importer_iso3[1])) +
            custom_theme_slides
        ggsave(file.path(STATS_PATH, "trade_patterns", "hs", paste0("pr_", coefplot$exporter_iso3[1], "_", coefplot$importer_iso3[1], "_raw.jpeg")), width = 10, height = 4, dpi = 300)
        
        coefplot <- coeftable[pair_id == p_id & type == "p" & cent == "pct"]
        ggplot(coefplot, aes(x = year, y = coef)) +
            geom_line(linewidth = 1.0, color = royalblue) +
            geom_point(size = 4, shape = 1, stroke = 1, color = royalblue) +
            geom_ribbon(aes(ymin = coef_lower, ymax = coef_upper), fill = royalblue, alpha = 0.2) +
            labs(x = "", y = paste0("Avg. flow mil. use, ", coefplot$exporter_iso3[1], "-", coefplot$importer_iso3[1])) +
            custom_theme_slides
        ggsave(file.path(STATS_PATH, "trade_patterns", "hs", paste0("pr_", coefplot$exporter_iso3[1], "_", coefplot$importer_iso3[1], "_pct.jpeg")), width = 10, height = 4, dpi = 300)
    }
}


# plot_counties()

