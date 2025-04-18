### Summarize delta over time

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


get_deltas_hs <- function(norank=FALSE) {
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
    contrib_change <- merge_df(contrib_end, contrib_start, by = c("exporter_iso2", "exporter_iso3", "importer_iso2", "importer_iso3", "type"), how = "inner")
    contrib_change[, delta_contrib := contrib_rank.x - contrib_rank.y]
    contrib_change[, pair := paste0(exporter_iso3, "-", importer_iso3)]
    contrib_change <- contrib_change[, c("exporter_iso3", "importer_iso3", "pair", "type", "delta_contrib")]
    contrib_change[, exporter_code := exporter_iso3]
    contrib_change[, importer_code := importer_iso3]
    contrib_change[, exporter_iso3 := NULL]
    contrib_change[, importer_iso3 := NULL]
    return(contrib_change)
}


norank <- FALSE
get_deltas_sitc <- function(norank=FALSE) {
    coef_list <- list()
    ix <- 1
    for (year in c(as.numeric(1965:1969), as.numeric(1995:1999))) {
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
    contrib_change <- merge_df(contrib_end, contrib_start, by = c("exporter_code", "importer_code", "type"), how = "inner")
    contrib_change[, delta_contrib := contrib_rank.x - contrib_rank.y]
    contrib_change[, pair := paste0(exporter_code, "-", importer_code)]
    return(contrib_change)
}


norank <- FALSE
plot_final <- function(norank=FALSE) {
    contrib_sitc <- get_deltas_sitc(norank=norank)
    contrib_sitc <- contrib_sitc[, c("exporter_code", "importer_code", "pair", "type", "delta_contrib")]
    contrib_hs <- get_deltas_hs(norank=norank)

    contrib_exp <- contrib_sitc[type == "e"][order(-delta_contrib)]
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
        ylab(("1965-1995")) +
        custom_theme_slides + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")

    contrib_exp <- contrib_hs[type == "e"][order(-delta_contrib)]
    contrib_exp1 <- contrib_exp[1:10]
    contrib_exp1[, type := "top"]
    contrib_exp2 <- contrib_exp[(nrow(contrib_exp) - 9):(nrow(contrib_exp))]
    contrib_exp2[, type := "bottom"]
    contrib_exp <- rbind(contrib_exp1, contrib_exp2)
    p2 <- ggplot(data = contrib_exp, aes(x = reorder(exporter_code, -delta_contrib), y = delta_contrib, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(data = contrib_exp[type == "top"], aes(label = exporter_code, y = 0), vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        geom_text(data = contrib_exp[type == "bottom"], aes(label = exporter_code, y = 0), vjust = -0.7, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        scale_fill_manual(values = c(royalred, royalblue)) +
        scale_y_continuous(labels = scales::percent) +
        xlab("") +
        ylab(("1995-2015")) +
        custom_theme_slides + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
    p <- plot_grid(p2, p1, ncol = 1, align = "v", rel_heights = c(0.5, 0.5))
    name <- "exports_combined.jpeg"
    if (norank) {
        name <- "exports_combined_norank.jpeg"
    }
    ggsave(file.path(STATS_PATH, "trade_patterns", name), p, width = 10, height = 5.5)

    contrib_imp <- contrib_sitc[type == "i"][order(-delta_contrib)]
    contrib_imp1 <- contrib_imp[1:10]
    contrib_imp1[, type := "top"]
    contrib_imp2 <- contrib_imp[(nrow(contrib_imp) - 9):(nrow(contrib_imp))]
    contrib_imp2[, type := "bottom"]
    contrib_imp <- rbind(contrib_imp1, contrib_imp2)
    p1 <- ggplot(data = contrib_imp, aes(x = reorder(exporter_code, -delta_contrib), y = delta_contrib, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(data = contrib_imp[type == "top"], aes(label = exporter_code, y = 0), vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        geom_text(data = contrib_imp[type == "bottom"], aes(label = exporter_code, y = 0), vjust = -0.7, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        scale_fill_manual(values = c(royalred, royalblue)) +
        scale_y_continuous(labels = scales::percent) +
        xlab("") +
        ylab(("1965-1995")) +
        custom_theme_slides + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")

    contrib_imp <- contrib_hs[type == "i"][order(-delta_contrib)]
    contrib_imp1 <- contrib_imp[1:10]
    contrib_imp1[, type := "top"]
    contrib_imp2 <- contrib_imp[(nrow(contrib_imp) - 9):(nrow(contrib_imp))]
    contrib_imp2[, type := "bottom"]
    contrib_imp <- rbind(contrib_imp1, contrib_imp2)
    p2 <- ggplot(data = contrib_imp[1:20], aes(x = reorder(importer_code, -delta_contrib), y = delta_contrib, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(data = contrib_imp[type == "top"], aes(label = importer_code, y = 0), vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        geom_text(data = contrib_imp[type == "bottom"], aes(label = importer_code, y = 0), vjust = -0.7, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon") +
        scale_fill_manual(values = c(royalred, royalblue)) +
        scale_y_continuous(labels = scales::percent) +
        xlab("") +
        ylab(("1995-2015")) +
        custom_theme_slides + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
    p <- plot_grid(p2, p1, ncol = 1, align = "v", rel_heights = c(0.5, 0.5))
    name <- "imports_combined.jpeg"
    if (norank) {
        name <- "imports_combined_norank.jpeg"
    }
    ggsave(file.path(STATS_PATH, "trade_patterns", name), p, width = 10, height = 5.5)

    contrib_pr <- contrib_sitc[type == "p"][order(-delta_contrib)]
    contrib_pr1 <- contrib_pr[1:10]
    contrib_pr1[, type := "top"]
    contrib_pr1[, id := 1:.N]
    contrib_pr2 <- contrib_pr[(nrow(contrib_pr) - 9):(nrow(contrib_pr))]
    contrib_pr2[, type := "bottom"]
    contrib_pr2[, id := 1:.N]
    contrib_pr <- rbind(contrib_pr1, contrib_pr2)
    y_pair <- -0.0002
    if (norank) {
        y_pair <- -0.001
    }
    p1 <- ggplot(data = contrib_pr, aes(x = reorder(pair, -delta_contrib), y = delta_contrib, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(data = contrib_pr[type == "top" & id != 1], aes(label = pair, y = 0), hjust = 1, vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 30) +
        geom_text(data = contrib_pr[type == "top" & id == 1], aes(label = pair, y = 0.0005), hjust = 0, vjust = 0.5, color = "white", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 90, fontface = "bold") +
        geom_text(data = contrib_pr[type == "bottom" & id != 10], aes(label = pair, y = 0), hjust = 0, vjust = -0.7, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 30) +
        geom_text(data = contrib_pr[type == "bottom" & id == 10], aes(label = pair, y = y_pair), hjust = 1, vjust = 0.5, color = "white", position = position_dodge(width = 0.9), size = 4.5, family = "Erewhon", angle = 90, fontface = "bold") +
        scale_fill_manual(values = c(royalred, royalblue)) +
        scale_y_continuous(labels = scales::percent) +
        xlab("") + ylab(("1965-1995")) +
        custom_theme_slides + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")

    contrib_pr <- contrib_hs[type == "p"][order(-delta_contrib)]
    contrib_pr1 <- contrib_pr[1:10]
    contrib_pr1[, type := "top"]
    contrib_pr1[, id := 1:.N]
    contrib_pr2 <- contrib_pr[(nrow(contrib_pr) - 9):(nrow(contrib_pr))]
    contrib_pr2[, type := "bottom"]
    contrib_pr2[, id := 1:.N]
    contrib_pr <- rbind(contrib_pr1, contrib_pr2)
    p2 <- ggplot(data = contrib_pr[1:20], aes(x = reorder(pair, -delta_contrib), y = delta_contrib, fill = type)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(data = contrib_pr[type == "top" & id != 1], aes(label = pair, y = 0), hjust = 1, vjust = 1.5, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 30) +
        geom_text(data = contrib_pr[type == "top" & id == 1], aes(label = pair, y = 0.001), hjust = 0, vjust = 0.5, color = "white", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 90, fontface = "bold") +
        geom_text(data = contrib_pr[type == "bottom" & id != 10], aes(label = pair, y = 0), hjust = 0, vjust = -0.7, color = "black", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 30) +
        geom_text(data = contrib_pr[type == "bottom" & id == 10], aes(label = pair, y = -0.001), hjust = 1, vjust = 0.5, color = "white", position = position_dodge(width = 0.9), size = 5, family = "Erewhon", angle = 90, fontface = "bold") +
        scale_fill_manual(values = c(royalred, royalblue)) +
        scale_y_continuous(labels = scales::percent) +
        xlab("") +
        ylab(("1995-2015")) +
        custom_theme_slides + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
    p <- plot_grid(p2, p1, ncol = 1, align = "v", rel_heights = c(0.5, 0.5))
    name <- "pair_combined.jpeg"
    if (norank) {
        name <- "pair_combined_norank.jpeg"
    }
    ggsave(file.path(STATS_PATH, "trade_patterns", name), p, width = 10, height = 5.5)
}


plot_final()
plot_final(norank=TRUE)

