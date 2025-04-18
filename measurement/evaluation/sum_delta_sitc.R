### Summarize trade over time

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(stringr)
library(scales)
library(fixest)
library(haven)
library(cowplot)

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
    
    df[, share_v := value / sum(value)]
    df[, share_C_M_sigma := share_v * C_M_sigma]
    df[, share_rank_C_M_sigma := share_v * rank_C_M_sigma]
    df[, sum_C_M_sigma := sum(share_C_M_sigma)]
    df[, sum_rank_C_M_sigma := sum(share_rank_C_M_sigma)]
    df[, contrib := share_C_M_sigma / sum_C_M_sigma]
    df[, contrib_rank := share_rank_C_M_sigma / sum_rank_C_M_sigma]
    
    df_pr <- df[, lapply(.SD, sum), .SDcols = c("contrib", "contrib_rank"), by = c("exporter_code", "importer_code")]
    df_pr[, type := 'p']

    df_exp <- df[, lapply(.SD, sum), .SDcols = c("contrib", "contrib_rank"), by = c("exporter_code")]
    df_exp[, importer_code := exporter_code]
    df_exp[, type := 'e']

    df_imp <- df[, lapply(.SD, sum), .SDcols = c("contrib", "contrib_rank"), by = c("importer_code")]
    df_imp[, exporter_code := importer_code]
    df_imp[, type := 'i']

    df_common <- rbind(df_pr, df_exp, df_imp)
    df_common[, year := year]
    return(df_common)
}


save_years <- function() {
    for (year in 1962:2021) {
        print(year)
        coeftable <- collapse_year(year)
        fwrite(coeftable, file.path(OUTPUT_PATH, "trade_patterns", "sitc_composition", paste0(year, ".csv")))
    }
}


# save_years()


norank <- FALSE
plot_deltas <- function(norank=FALSE) {
    coef_list <- list()
    ix <- 1
    for (year in 1962:2021) {
        print(year)
        contrib <- fread(file.path(OUTPUT_PATH, "trade_patterns", "sitc_composition", paste0(year, ".csv")), keepLeadingZeros = TRUE)
        coef_list[[ix]] <- contrib
        ix <- ix + 1
    }
    contrib <- rbindlist(coef_list)
    if (norank) {
        contrib[, contrib_rank := contrib]
    }
    contrib <- contrib[exporter_code != "ANS" & importer_code != "ANS"]
    contrib_end <- contrib[year >= 1995 & year <= 1999]
    contrib_end <- contrib_end[, lapply(.SD, mean), .SDcols = c("contrib", "contrib_rank"), by = c("exporter_code", "importer_code", "type")]
    contrib_end[, pair := paste0(exporter_code, "-", importer_code)]
    contrib_start <- contrib[year >= 1965 & year <= 1969]
    contrib_start <- contrib_start[, lapply(.SD, mean), .SDcols = c("contrib", "contrib_rank"), by = c("exporter_code", "importer_code", "type")]
    contrib_change <- merge_df(contrib_end, contrib_start, by = c("exporter_code", "importer_code", "type"), how = "outer")
    contrib_change[is.na(contrib_rank.x), contrib_rank.x := 0]
    contrib_change[is.na(contrib_rank.y), contrib_rank.y := 0]
    contrib_change[, delta_contrib := contrib_rank.x - contrib_rank.y]
    contrib_change[, pair := paste0(exporter_code, "-", importer_code)]

    contrib_exp <- contrib_change[type == "e"][order(-delta_contrib)]
    contrib_exp1 <- contrib_exp[1:10]
    contrib_exp1[, type := "top"]
    contrib_exp2 <- contrib_exp[(nrow(contrib_exp) - 9):(nrow(contrib_exp))]
    contrib_exp2[, type := "bottom"]
    contrib_exp <- rbind(contrib_exp1, contrib_exp2)
    p1 <- ggplot(data = contrib_exp, aes(x = reorder(exporter_code, -delta_contrib), y = delta_contrib, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(data = contrib_exp[type == "top"], aes(label = exporter_code, y = 0), vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        geom_text(data = contrib_exp[type == "bottom"], aes(label = exporter_code, y = 0), vjust = -0.7, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        scale_fill_manual(values = c(royalred, royalblue)) +
        scale_y_continuous(labels = scales::percent) +
        xlab("") +
        ylab(("Exports")) +
        custom_theme_slides + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")

    contrib_imp <- contrib_change[type == "i"][order(-delta_contrib)]
    contrib_imp1 <- contrib_imp[1:10]
    contrib_imp1[, type := "top"]
    contrib_imp2 <- contrib_imp[(nrow(contrib_imp) - 9):(nrow(contrib_imp))]
    contrib_imp2[, type := "bottom"]
    contrib_imp <- rbind(contrib_imp1, contrib_imp2)
    p2 <- ggplot(data = contrib_imp, aes(x = reorder(importer_code, -delta_contrib), y = delta_contrib, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(data = contrib_imp[type == "top"], aes(label = importer_code, y = 0), vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        geom_text(data = contrib_imp[type == "bottom"], aes(label = importer_code, y = 0), vjust = -0.7, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        scale_fill_manual(values = c(royalred, royalblue)) +
        scale_y_continuous(labels = scales::percent) +
        xlab("") +
        ylab(("Imports")) +
        custom_theme_slides + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")

    contrib_pr <- contrib_change[type == "p"][order(-delta_contrib)]
    contrib_pr1 <- contrib_pr[1:10]
    contrib_pr1[, type := "top"]
    contrib_pr1[, id := 1:.N]
    contrib_pr2 <- contrib_pr[(nrow(contrib_pr) - 9):(nrow(contrib_pr))]
    contrib_pr2[, type := "bottom"]
    contrib_pr2[, id := 1:.N]
    contrib_pr <- rbind(contrib_pr1, contrib_pr2)
    p3 <- ggplot(data = contrib_pr, aes(x = reorder(pair, -delta_contrib), y = delta_contrib, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(data = contrib_pr[type == "top" & id != 1], aes(label = pair, y = 0), hjust = 1, vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 30) +
        geom_text(data = contrib_pr[type == "top" & id == 1], aes(label = pair, y = 0.001), hjust = 0, vjust = 0.5, color = "white", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 90, fontface = "bold") +
        geom_text(data = contrib_pr[type == "bottom" & id != 10], aes(label = pair, y = 0), hjust = 0, vjust = -0.7, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 30) +
        geom_text(data = contrib_pr[type == "bottom" & id == 10], aes(label = pair, y = -0.0005), hjust = 1, vjust = 0.5, color = "white", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 90, fontface = "bold") +
        # scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
        scale_fill_manual(values = c(royalred, royalblue)) +
        scale_y_continuous(labels = scales::percent) +
        xlab("") +
        ylab(("Pair")) +
        custom_theme_slides + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")

    p <- plot_grid(p1, p2, p3, ncol = 1, align = "v", rel_heights = c(0.33, 0.33, 0.33))
    name <- "barplots.jpeg"
    if (norank) {
        name <- "barplots_norank.jpeg"
    }
    ggsave(file.path(STATS_PATH, "trade_patterns", "delta_sitc_composition", name), p, width = 10, height = 10)
}


plot_deltas()
plot_deltas(norank=TRUE)

