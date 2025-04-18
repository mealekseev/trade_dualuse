### Summarize trade over time

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(stringr)
library(scales)
library(fixest)
library(cowplot)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


year <- 2005
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
    df[, share_v := v / sum(v)]
    df[, share_C_M_sigma := share_v * C_M_sigma]
    df[, share_rank_C_M_sigma := share_v * rank_C_M_sigma]
    df[, sum_C_M_sigma := sum(share_C_M_sigma)]
    df[, sum_rank_C_M_sigma := sum(share_rank_C_M_sigma)]
    df[, contrib := share_C_M_sigma / sum_C_M_sigma]
    df[, contrib_rank := share_rank_C_M_sigma / sum_rank_C_M_sigma]
    
    df_pr <- df[, lapply(.SD, sum), .SDcols = c("contrib", "contrib_rank"), by = c("i", "j")]
    df_pr[, type := 'p']

    df_exp <- df[, lapply(.SD, sum), .SDcols = c("contrib", "contrib_rank"), by = c("i")]
    df_exp[, j := i]
    df_exp[, type := 'e']

    df_imp <- df[, lapply(.SD, sum), .SDcols = c("contrib", "contrib_rank"), by = c("j")]
    df_imp[, i := j]
    df_imp[, type := 'i']
    # df[, min_i := pmin(i, j)]
    # df[, max_j := pmax(i, j)]
    # df_up <- df[, lapply(.SD, sum), .SDcols = c("contrib", "contrib_rank"), by = c("max_i", "max_j")]
    # df_up[, type := 'u']
    # df_up[, i := max_i]
    # df_up[, j := max_j]
    # df_up[, max_i := NULL]
    # df_up[, max_j := NULL]

    df_common <- rbind(df_pr, df_exp, df_imp)
    df_common[, i := as.character(i)]
    df_common[, j := as.character(j)]
    df_common[, year := year]

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
    
    df_common <- merge_df(df_common, import_list, by.x = "j", by.y = "importer_code", how = "left", allow.cartesian = TRUE, indicator = TRUE)
    if (any(df_common$merge_ == "left_only")) {
        raise("Missing importer code")
    } else {
        df_common[, merge_ := NULL]
    }
    df_common <- merge_df(df_common, export_list, by.x = "i", by.y = "exporter_code", how = "left", allow.cartesian = TRUE, indicator = TRUE)
    if (any(df_common$merge_ == "left_only")) {
        raise("Missing exporter code")
    } else {
        df_common[, merge_ := NULL]
    }
    df_common <- df_common[, c("year", "i", "exporter_iso2", "exporter_iso3", "j", "importer_iso2", "importer_iso3", "type", "contrib", "contrib_rank")]
    return(df_common)
}


save_years <- function() {
    for (year in 2012:2022) {
        print(year)
        coeftable <- collapse_year(year)
        fwrite(coeftable, file.path(OUTPUT_PATH, "trade_patterns", "hs_composition", paste0(year, ".csv")))
    }
}


# save_years()


plot_countries <- function(norank=FALSE) {
    coef_list <- list()
    ix <- 1
    for (year in 2012:2022) {
        print(year)
        contrib <- fread(file.path(OUTPUT_PATH, "trade_patterns", "hs_composition", paste0(year, ".csv")), keepLeadingZeros = TRUE)
        coef_list[[ix]] <- contrib
        ix <- ix + 1
    }
    contrib <- rbindlist(coef_list)
    contrib <- contrib[year >= 2015 & year <= 2019]
    contrib <- contrib[, lapply(.SD, mean), .SDcols = c("contrib", "contrib_rank"), by = c("exporter_iso2", "exporter_iso3", "importer_iso2", "importer_iso3", "type")]
    contrib[exporter_iso3 == "S19", exporter_iso3 := "TWN"]
    contrib[importer_iso3 == "S19", importer_iso3 := "TWN"]
    contrib[exporter_iso3 == "TWN", exporter_iso2 := "TW"]
    contrib[importer_iso3 == "TWN", importer_iso2 := "TW"]
    contrib[, pair := paste0(exporter_iso2, "-", importer_iso2)]
    if (norank) {
        contrib[, contrib_rank := contrib]
    }

    contrib_exp <- contrib[type == "e"][order(-contrib_rank)]
    p1 <- ggplot(data = contrib_exp[1:20], aes(x = reorder(exporter_iso3, -contrib_rank), y = contrib_rank)) +
        geom_bar(stat = "identity", position = "dodge", fill = royalred) +
        geom_text(aes(label = exporter_iso3, y = 0), vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        xlab("") + ylab("Exports") +
        scale_y_continuous(labels = scales::percent, limits = c(-max(contrib_exp[1:20, contrib_rank]) / 20, max(contrib_exp[1:20, contrib_rank]))) +
        # custom_theme_slides + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
        custom_theme_slides + theme(axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank(), legend.position = "none")

    contrib_imp <- contrib[type == "i"][order(-contrib_rank)]
    p2 <- ggplot(data = contrib_imp[1:20], aes(x = reorder(importer_iso3, -contrib_rank), y = contrib_rank)) +
        geom_bar(stat = "identity", position = "dodge", fill = graphite) +
        geom_text(aes(label = importer_iso3, y = 0), vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        xlab("") + ylab("Imports") +
        scale_y_continuous(labels = scales::percent, limits = c(-max(contrib_imp[1:20, contrib_rank]) / 20, max(contrib_imp[1:20, contrib_rank]))) +
        custom_theme_slides + theme(axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank(), legend.position = "none")

    contrib_pr <- contrib[type == "p"][order(-contrib_rank)]
    p3 <- ggplot(data = contrib_pr[1:20], aes(x = reorder(pair, -contrib_rank), y = contrib_rank)) +
        geom_bar(stat = "identity", position = "dodge", fill = royalblue) +
        geom_text(aes(label = pair, y = -0.001), hjust = 0.8, vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 45) +
        xlab("") + ylab("Pairs") +
        scale_y_continuous(labels = scales::percent, limits = c(-max(contrib_pr[1:20, contrib_rank]) / 4, max(contrib_pr[1:20, contrib_rank]))) +
        custom_theme_slides + theme(axis.text.x = element_text(color = "white", size = 20), axis.ticks.x = element_blank(), legend.position = "none")

    p <- plot_grid(p1, p2, p3, ncol = 1, align = "v", rel_heights = c(0.33, 0.33, 0.33))
    name <- "barplots.jpeg"
    if (norank) {
        name <- "barplots_norank.jpeg"
    }
    ggsave(file.path(STATS_PATH, "trade_patterns", "hs_composition", name), p, width = 10, height = 10)
}


# plot_countries()
# plot_countries(norank=TRUE)


norank <- FALSE
plot_deltas <- function(norank=FALSE) {
    coef_list <- list()
    ix <- 1
    for (year in 2012:2022) {
        print(year)
        contrib <- fread(file.path(OUTPUT_PATH, "trade_patterns", "hs_composition", paste0(year, ".csv")), keepLeadingZeros = TRUE)
        coef_list[[ix]] <- contrib
        ix <- ix + 1
    }
    contrib <- rbindlist(coef_list)
    contrib[exporter_iso3 == "S19", exporter_iso3 := "TWN"]
    contrib[importer_iso3 == "S19", importer_iso3 := "TWN"]
    contrib[exporter_iso3 == "TWN", exporter_iso2 := "TW"]
    contrib[importer_iso3 == "TWN", importer_iso2 := "TW"]
    if (norank) {
        contrib[, contrib_rank := contrib]
    }
    contrib_end <- contrib[year >= 2015 & year <= 2019]
    contrib_end <- contrib_end[, lapply(.SD, mean), .SDcols = c("contrib", "contrib_rank"), by = c("exporter_iso2", "exporter_iso3", "importer_iso2", "importer_iso3", "type")]
    contrib_end[, pair := paste0(exporter_iso2, "-", importer_iso2)]
    contrib_start <- contrib[year >= 1995 & year <= 1999]
    contrib_start <- contrib_start[, lapply(.SD, mean), .SDcols = c("contrib", "contrib_rank"), by = c("exporter_iso2", "exporter_iso3", "importer_iso2", "importer_iso3", "type")]
    contrib_start[, pair := paste0(exporter_iso2, "-", importer_iso2)]
    contrib_change <- merge_df(contrib_end, contrib_start, by = c("exporter_iso2", "exporter_iso3", "importer_iso2", "importer_iso3", "type"), how = "inner")
    contrib_change[, delta_contrib := contrib_rank.x - contrib_rank.y]


    contrib_exp <- contrib[type == "e"][order(-contrib_rank)]
    p1 <- ggplot(data = contrib_exp[1:20], aes(x = reorder(exporter_iso3, -contrib_rank), y = contrib_rank)) +
        geom_bar(stat = "identity", position = "dodge", fill = royalred) +
        geom_text(aes(label = exporter_iso3, y = 0), vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        xlab("") + ylab("Exports") +
        scale_y_continuous(labels = scales::percent, limits = c(-max(contrib_exp[1:20, contrib_rank]) / 20, max(contrib_exp[1:20, contrib_rank]))) +
        # custom_theme_slides + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
        custom_theme_slides + theme(axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank(), legend.position = "none")

    contrib_imp <- contrib[type == "i"][order(-contrib_rank)]
    p2 <- ggplot(data = contrib_imp[1:20], aes(x = reorder(importer_iso3, -contrib_rank), y = contrib_rank)) +
        geom_bar(stat = "identity", position = "dodge", fill = graphite) +
        geom_text(aes(label = importer_iso3, y = 0), vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        xlab("") + ylab("Imports") +
        scale_y_continuous(labels = scales::percent, limits = c(-max(contrib_imp[1:20, contrib_rank]) / 20, max(contrib_imp[1:20, contrib_rank]))) +
        custom_theme_slides + theme(axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank(), legend.position = "none")

    contrib_pr <- contrib[type == "p"][order(-contrib_rank)]
    p3 <- ggplot(data = contrib_pr[1:20], aes(x = reorder(pair, -contrib_rank), y = contrib_rank)) +
        geom_bar(stat = "identity", position = "dodge", fill = royalblue) +
        geom_text(aes(label = pair, y = -0.001), hjust = 0.8, vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 45) +
        xlab("") + ylab("Pairs") +
        scale_y_continuous(labels = scales::percent, limits = c(-max(contrib_pr[1:20, contrib_rank]) / 4, max(contrib_pr[1:20, contrib_rank]))) +
        custom_theme_slides + theme(axis.text.x = element_text(color = "white", size = 20), axis.ticks.x = element_blank(), legend.position = "none")

    p <- plot_grid(p1, p2, p3, ncol = 1, align = "v", rel_heights = c(0.33, 0.33, 0.33))
    name <- "barplots.jpeg"
    if (norank) {
        name <- "barplots_norank.jpeg"
    }
    ggsave(file.path(STATS_PATH, "trade_patterns", "delta_hs_composition", name), p, width = 10, height = 10)
}


plot_countries()
plot_countries(norank=TRUE)

