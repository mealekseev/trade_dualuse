### Summarize delta over time

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
    for (year in 1995:2022) {
        print(year)
        coeftable <- collapse_year(year)
        fwrite(coeftable, file.path(OUTPUT_PATH, "trade_patterns", "hs_composition", paste0(year, ".csv")))
    }
}


# save_years()


norank <- FALSE
plot_deltas <- function(norank=FALSE) {
    coef_list <- list()
    ix <- 1
    for (year in 1995:2022) {
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
    contrib_change <- merge_df(contrib_end, contrib_start, by = c("exporter_iso2", "exporter_iso3", "importer_iso2", "importer_iso3", "type"), how = "outer")
    contrib_change[is.na(contrib_rank.x), contrib_rank.x := 0]
    contrib_change[is.na(contrib_rank.y), contrib_rank.y := 0]
    contrib_change[, delta_contrib := contrib_rank.x - contrib_rank.y]
    contrib_change[, pair := paste0(exporter_iso3, "-", importer_iso3)]

    contrib_exp <- contrib_change[type == "e"][order(-delta_contrib)]
    contrib_exp1 <- contrib_exp[1:10]
    contrib_exp1[, type := "top"]
    contrib_exp2 <- contrib_exp[(nrow(contrib_exp) - 9):(nrow(contrib_exp))]
    contrib_exp2[, type := "bottom"]
    contrib_exp <- rbind(contrib_exp1, contrib_exp2)
    p1 <- ggplot(data = contrib_exp, aes(x = reorder(exporter_iso3, -delta_contrib), y = delta_contrib, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(data = contrib_exp[type == "top"], aes(label = exporter_iso3, y = 0), vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        geom_text(data = contrib_exp[type == "bottom"], aes(label = exporter_iso3, y = 0), vjust = -0.7, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
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
    p2 <- ggplot(data = contrib_imp, aes(x = reorder(importer_iso3, -delta_contrib), y = delta_contrib, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(data = contrib_imp[type == "top"], aes(label = importer_iso3, y = 0), vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        geom_text(data = contrib_imp[type == "bottom"], aes(label = importer_iso3, y = 0), vjust = -0.7, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
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
        geom_text(data = contrib_pr[type == "top" & id == 1], aes(label = pair, y = 0.0015), hjust = 0, vjust = 0.5, color = "white", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 90, fontface = "bold") +
        geom_text(data = contrib_pr[type == "bottom" & id != 10], aes(label = pair, y = 0), hjust = 0, vjust = -0.7, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 30) +
        geom_text(data = contrib_pr[type == "bottom" & id == 10], aes(label = pair, y = -0.0015), hjust = 1, vjust = 0.5, color = "white", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 90, fontface = "bold") +
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
    ggsave(file.path(STATS_PATH, "trade_patterns", "delta_hs_composition", name), p, width = 10, height = 10)
}


plot_deltas()
plot_deltas(norank=TRUE)

