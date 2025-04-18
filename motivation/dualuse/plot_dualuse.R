### Plot dualuse trends

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(stringr)
library(xtable)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


get_military_trends <- function() {
    ### graph on the % of trade and global military spending 
    mil_wld <- data.table(read_excel(
        file.path(DATA_PATH, "motivation", "military", "SIPRI-Milex-data-1949-2022.xlsx"), sheet = "Regional totals", skip = 13
    ))
    mil_wld <- mil_wld[Region == "World"]
    mil_wld <- mil_wld[, paste0(2007:2022, ".0"), with = FALSE]
    mil <- data.table(year = 2007:2022, global = as.numeric(mil_wld[1, ]))

    mil_cty <- data.table(read_excel(
        file.path(DATA_PATH, "motivation", "military", "SIPRI-Milex-data-1949-2022.xlsx"), sheet = "Constant (2021) US$", skip = 5
    ))
    mil_cty <- mil_cty[Country %in% c("China", "Russia")]
    mil_cty <- mil_cty[, paste0(2007:2022, ".0"), with = FALSE]
    mil_cty <- data.table(year = 2007:2022, china = as.numeric(mil_cty[1, ]), russia = as.numeric(mil_cty[2, ]))
    mil_cty[, china := china / 1000]
    mil_cty[, russia := russia / 1000]
    mil_cty[, chn_rus := china + russia]

    mil <- merge_df(mil, mil_cty, by = "year")
    mil[, share_rus := russia / global]
    mil[, share_chn_rus := chn_rus / global]
    
    # used estimated projections
    mil_est <- data.table(year = 2023, global = 2181.921 * 1.05, china = c(297.9990 * 1.072), russia = c(71.98111 * 2.5))
    mil_est[, chn_rus := china + russia]
    mil_est[, share_rus := russia / global]
    mil_est[, share_chn_rus := chn_rus / global]
    mil_est[, merge_ := "both"]
    mil <- rbindlist(list(mil, mil_est), use.names = TRUE)
    return(mil)
}


plot_shares <- function() {
    ### graph on the % of trade
    trade <- fread(file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs4.csv"), keepLeadingZeros = TRUE)
    du_cols <- colnames(trade)[startsWith(colnames(trade), "dualuse_2")]
    df_list <- list()
    i <- 1
    for (col in du_cols) {
        yr <- as.integer(str_extract(col, "[0-9]+"))
        du_perc <- sum(trade[get(col) == 1, perc_value])
        df_list[[i]] <- data.table(year = yr, du_perc = du_perc)
        i <- i + 1
    }
    df <- rbindlist(df_list)

    ### main graph
    p <- ggplot(df[!(year %in% c(2009, 2010, 2011, 2024))], aes(x = year, y = du_perc)) +
        geom_line(color = royalblue, linewidth = 1.0) +
        geom_point(color = royalblue, size = 4, shape = 1, stroke = 1) +
        scale_x_continuous(limits = c(2007, 2023), breaks = seq(2007, 2023, by = 2)) +
        scale_y_continuous(limits = c(min(df$du_perc), max(df$du_perc)), labels = scales::percent) +
        annotate("text", x = 2014.25, y = 0.407, label = "Crimea-2014\nEC review of export controls",
            family = "Erewhon", hjust = 0.0, vjust = 0.5, size = 6, color = "black") +
        annotate("text", x = 2021.25, y = 0.34, label = "Ukraine war",
            family = "Erewhon", hjust = 0.0, vjust = 0.5, size = 6, color = "black") +
        geom_vline(xintercept = 2014, linetype = "dashed", linewidth = 1.0) +
        geom_vline(xintercept = 2021, linetype = "dashed", linewidth = 1.0) +
        xlab("European Commission dual-use goods list vintage") +
        ylab("Dual-use HS6 codes\n(2015-2019 global trade share)") +
        custom_theme
    p
    ggsave(file.path(STATS_PATH, "motivation", "dualuse", "dualuse_perc.jpeg"), width = 10, height = 5.5, dpi = 300)

    ### slide animations
    ggplot(df[!(year %in% c(2009, 2010, 2011, 2024)) & year <= 2014], aes(x = year, y = du_perc)) +
        scale_x_continuous(limits = c(2007, 2023), breaks = seq(2007, 2023, by = 2)) +
        scale_y_continuous(limits = c(min(df$du_perc), max(df$du_perc)), labels = scales::percent) +
        xlab("European Commission dual-use goods list vintage") +
        ylab("Dual-use HS6 codes\n(2015-2019 global trade share)") +
        custom_theme
    ggsave(file.path(STATS_PATH, "motivation", "dualuse", "dualuse_perc_anim0.jpeg"), width = 10, height = 5.5, dpi = 300)   

    ggplot(df[!(year %in% c(2009, 2010, 2011, 2024))], aes(x = year, y = du_perc)) +
        geom_line(color = royalblue, linewidth = 1.0) +
        geom_point(color = royalblue, size = 4, shape = 1, stroke = 1) +
        scale_x_continuous(limits = c(2007, 2023), breaks = seq(2007, 2023, by = 2)) +
        scale_y_continuous(labels = scales::percent) +
        annotate("text", x = 2016.25, y = 0.3, label = "Dual-use goods,\n % global trade",
            family = "Erewhon", hjust = 0.5, vjust = 0.5, size = 6, color = royalblue) +
        xlab("European Commission dual-use goods list vintage") +
        ylab("Dual-use HS6 codes\n(2015-2019 global trade share)") +
        custom_theme
    ggsave(file.path(STATS_PATH, "motivation", "dualuse", "dualuse_perc_anim1.jpeg"), width = 10, height = 5.5, dpi = 300)   

    ggplot(df[!(year %in% c(2009, 2010, 2011, 2024))], aes(x = year, y = du_perc)) +
        geom_line(color = royalblue, linewidth = 1.0) +
        geom_point(color = royalblue, size = 4, shape = 1, stroke = 1) +
        scale_x_continuous(limits = c(2007, 2023), breaks = seq(2007, 2023, by = 2)) +
        scale_y_continuous(limits = c(min(df$du_perc), max(df$du_perc)), labels = scales::percent) +
        annotate("text", x = 2014.25, y = 0.407, label = "Crimea-2014\nEC review of export controls",
            family = "Erewhon", hjust = 0.0, vjust = 0.5, size = 6, color = "black") +
        annotate("text", x = 2021.25, y = 0.34, label = "Ukraine war",
            family = "Erewhon", hjust = 0.0, vjust = 0.5, size = 6, color = "black") +
        geom_vline(xintercept = 2014, linetype = "dashed", linewidth = 1.0) +
        geom_vline(xintercept = 2021, linetype = "dashed", linewidth = 1.0) +
        xlab("European Commission dual-use goods list vintage") +
        ylab("Dual-use HS6 codes\n(2015-2019 global trade share)") +
        custom_theme
    ggsave(file.path(STATS_PATH, "motivation", "dualuse", "dualuse_perc_anim2.jpeg"), width = 10, height = 5.5, dpi = 300)

    ### add military
    mil <- get_military_trends()
    df <- merge_df(mil, df, by = "year", how = "outer")
    df <- df[year >= 2007 & year <= 2023]
    p + scale_y_continuous(
            "% 2015-2019 global trade",
            labels = scales::percent,
            sec.axis = sec_axis(
                ~. - 0.15, name = "% global military spending",
                labels = scales::percent
            )
        ) +
        geom_line(data = df, aes(x = year, y = 0.2 + share_chn_rus), color = royalred, linewidth = 1.0) +
        geom_point(data = df, aes(x = year, y = 0.2 + share_chn_rus), color = royalred, size = 4, shape = 1, stroke = 1) +
        annotate("text", x = 2010.5, y = 0.355, label = "China + Russia,\n % global military spending",
            family = "Erewhon", hjust = 0.5, vjust = 0.5, size = 6, color = royalred) +
        annotate("text", x = 2016.25, y = 0.3, label = "Dual-use goods,\n % global trade",
            family = "Erewhon", hjust = 0.5, vjust = 0.5, size = 6, color = royalblue)
    ggsave(file.path(STATS_PATH, "motivation", "dualuse", "dualuse_perc_mil.jpeg"), width = 10, height = 5.5, dpi = 300)

    p + scale_y_continuous(
            "% 2015-2019 global trade",
            labels = scales::percent,
            sec.axis = sec_axis(
                ~. - 0.15, name = "% global military spending",
                labels = scales::percent
            )
        ) +
        geom_line(data = df, aes(x = year, y = 0.2 + share_chn_rus), color = royalred, linewidth = 1.0) +
        geom_point(data = df, aes(x = year, y = 0.2 + share_chn_rus), color = royalred, size = 4, shape = 1, stroke = 1) +
        annotate("text", x = 2010.5, y = 0.355, label = "China + Russia,\n % global military spending",
            family = "Erewhon", hjust = 0.5, vjust = 0.5, size = 6, color = royalred)
    ggsave(file.path(STATS_PATH, "motivation", "dualuse", "dualuse_perc_anim3.jpeg"), width = 10, height = 5.5, dpi = 300)
}


plot_counts <- function() {
    ### plot counts
    trade <- fread(file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs4.csv"), keepLeadingZeros = TRUE)
    du_cols <- colnames(trade)[startsWith(colnames(trade), "dualuse_2")]
    df_list <- list()
    i <- 1
    for (col in du_cols) {
        yr <- as.integer(str_extract(col, "[0-9]+"))
        du_perc <- nrow(trade[get(col) == 1])
        df_list[[i]] <- data.table(year = yr, du_perc = du_perc)
        i <- i + 1
    }
    df <- rbindlist(df_list)

    df_cn <- fread(file.path(OUTPUT_PATH, "motivation", "eu_commission", "dualuse_hs6_raw.csv"))
    df_cn <- df_cn[, .N, by = c("year")]
    df <- merge_df(df, df_cn, by = "year", indicator = FALSE)
    df <- melt(df, id.vars = "year")

    ### plot graphs
    ggplot(df[!(year %in% c(2009, 2010, 2011, 2024))], aes(x = year, y = value, color = variable)) +
        geom_line(linewidth = 1.0) +
        geom_point(size = 4, shape = 1, stroke = 1) +
        scale_x_continuous(breaks = seq(min(df$year) - 1, max(df$year), by = 2)) +
        scale_color_manual(values = c(royalred, royalblue), labels = c("2012 Revision", "Raw counts")) +
        annotate("text", x = 2016, y = 925, label = "HS Rev. 4 (2012)",
            family = "Erewhon", hjust = 0.5, vjust = 0.5, size = 5, color = royalred) +
        annotate("text", x = 2019, y = 780, label = "Raw counts",
            family = "Erewhon", hjust = 0.5, vjust = 0.5, size = 5, color = royalblue) +
        xlab("European Commission dual-use goods list vintage") +
        ylab("Dual-use HS6 codes count") +
        custom_theme + theme(legend.position = "none")
    ggsave(file.path(STATS_PATH, "motivation", "dualuse", "dualuse_count.jpeg"), width = 10, height = 5.5, dpi = 300)
}


plot_levels <- function() {
    trade <- fread(file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs4.csv"), keepLeadingZeros = TRUE)
    trade[, hscode := sector]
    trade[, hs2 := substr(hscode, 1, 2)]
    trade[, perc_value_du := perc_value * get(paste0("dualuse_", 2018))]
    trade <- trade[, lapply(.SD, sum), .SDcols = c("perc_value", "perc_value_du"), by = c("hs2")]
    trade <- trade[order(-perc_value_du)]
    trade[, id := seq(1, .N)]
    trade[id == 10, perc_value := sum(trade[id >= 10, perc_value])]
    trade[id == 10, perc_value_du := sum(trade[id >= 10, perc_value_du])]
    trade <- trade[1:10]
    trade[id == 10, hs2 := "Other"]

    hscode <- fread(file.path(OUTPUT_PATH, 'crosswalks', 'H4_codes.csv'))
    trade <- merge_df(trade, hscode, by.x = "hs2", by.y = "sector", how = "left")
    trade[, short_name := c(
        "27 - Mineral fuels",
        "29 - Organic chemicals",
        "38 - Chemicals",
        "39 - Plastics",
        "71 - Precious metals",
        "84 - Mechanical appliances",
        "85 - Electrical machinery",
        "88 - Aerospace",
        "90 - Opticals",
        "Other"
    )]
    trade <- trade[order(id)]

    trade[, bar_end := cumsum(perc_value_du)]
    trade[, bar_start := shift(bar_end, fill = 0)]
    trade[, bar_end2 := bar_start + perc_value]
    
    label_function <- function(y) {
        labels <- trade[, short_name]
        return(labels[-y])
    }

    trade[, plot_dummy := c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)]
    ggplot(trade, aes(
        ymin = -id - 0.4, ymax = -id + 0.4,
        xmin = bar_start, xmax = bar_end)) +
        geom_rect(aes(xmin = bar_start, xmax = bar_end2), color = "black", linetype = "dashed", fill = NA) +
        geom_rect(fill = royalblue, alpha = 0.9, color = "black") +
        scale_y_continuous(breaks = seq(-1, -10, -1), labels = label_function) +
        scale_x_continuous(labels = scales::percent) +
        custom_theme +
        geom_ribbon(aes(x = bar_start, y_lower = -id, y_higher = -id, fill = as.factor(plot_dummy)), alpha = 0) + 
        scale_fill_manual(
            labels = c("dual-use", "other"),
            values = c(royalblue, "white"),
            name = "HS 6-digit categories") +
        guides(fill = guide_legend(override.aes = list(alpha = 1, color = "black", linetype = "dashed"))) +
        geom_vline(xintercept = sum(trade$perc_value_du), linewidth = 0.5) +
        xlab("% global trade, 2015-2019") +
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
        ) + custom_theme
    ggsave(file.path(STATS_PATH, "motivation", "dualuse", "breakdown.jpeg"), width = 10, height = 5.5, dpi = 300)

    trade_anim <- copy(trade)
    trade_anim[, c("bar_end", "bar_start", "bar_end2") := NA]
    ggplot(trade_anim, aes(
        ymin = -id - 0.4, ymax = -id + 0.4,
        xmin = bar_start, xmax = bar_end)) +
        geom_rect(aes(xmin = bar_start, xmax = bar_end2), color = "black", linetype = "dashed", fill = NA) +
        geom_rect(fill = royalblue, alpha = 0.9, color = "black") +
        scale_y_continuous(breaks = seq(-1, -10, -1), labels = label_function) +
        scale_x_continuous(limits = c(0, 0.778), labels = scales::percent) +
        custom_theme +
        geom_ribbon(aes(x = bar_start, y_lower = -id, y_higher = -id, fill = as.factor(plot_dummy)), alpha = 0) + 
        scale_fill_manual(
            labels = c("dual-use", "other"),
            values = c(royalblue, "white"),
            name = "HS 6-digit categories") +
        guides(fill = guide_legend(override.aes = list(alpha = 1, color = "black", linetype = "dashed"))) +
        geom_vline(xintercept = sum(trade$perc_value_du), linewidth = 0.5) +
        xlab("% global trade, 2015-2019") +
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
        ) + custom_theme
    ggsave(file.path(STATS_PATH, "motivation", "dualuse", "breakdown_anim0.jpeg"),
        width = 10, height = 5.5, dpi = 300)

    trade_anim <- copy(trade)
    trade_anim[hs2 != c(85, 84), c("bar_end", "bar_start", "bar_end2") := NA]
    ggplot(trade_anim, aes(
        ymin = -id - 0.4, ymax = -id + 0.4,
        xmin = bar_start, xmax = bar_end)) +
        geom_rect(aes(xmin = bar_start, xmax = bar_end2), color = "black", linetype = "dashed", fill = NA) +
        geom_rect(fill = royalblue, alpha = 0.9, color = "black") +
        scale_y_continuous(breaks = seq(-1, -10, -1), labels = label_function) +
        scale_x_continuous(limits = c(0, 0.778), labels = scales::percent) +
        custom_theme +
        geom_ribbon(aes(x = bar_start, y_lower = -id, y_higher = -id, fill = as.factor(plot_dummy)), alpha = 0) + 
        scale_fill_manual(
            labels = c("dual-use", "other"),
            values = c(royalblue, "white"),
            name = "HS 6-digit categories") +
        guides(fill = guide_legend(override.aes = list(alpha = 1, color = "black", linetype = "dashed"))) +
        geom_vline(xintercept = sum(trade$perc_value_du), linewidth = 0.5) +
        xlab("% global trade, 2015-2019") +
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
        ) + custom_theme
    ggsave(file.path(STATS_PATH, "motivation", "dualuse", "breakdown_anim1.jpeg"),
        width = 10, height = 5.5, dpi = 300)

    trade_anim <- copy(trade)
    trade_anim[hs2 != c(85, 84, 27, 90, 88), c("bar_end", "bar_start", "bar_end2") := NA]
    ggplot(trade_anim, aes(
        ymin = -id - 0.4, ymax = -id + 0.4,
        xmin = bar_start, xmax = bar_end)) +
        geom_rect(aes(xmin = bar_start, xmax = bar_end2), color = "black", linetype = "dashed", fill = NA) +
        geom_rect(fill = royalblue, alpha = 0.9, color = "black") +
        scale_y_continuous(breaks = seq(-1, -10, -1), labels = label_function) +
        scale_x_continuous(limits = c(0, 0.778), labels = scales::percent) +
        custom_theme +
        geom_ribbon(aes(x = bar_start, y_lower = -id, y_higher = -id, fill = as.factor(plot_dummy)), alpha = 0) + 
        scale_fill_manual(
            labels = c("dual-use", "other"),
            values = c(royalblue, "white"),
            name = "HS 6-digit categories") +
        guides(fill = guide_legend(override.aes = list(alpha = 1, color = "black", linetype = "dashed"))) +
        geom_vline(xintercept = sum(trade$perc_value_du), linewidth = 0.5) +
        xlab("% global trade, 2015-2019") +
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
        ) + custom_theme
    ggsave(file.path(STATS_PATH, "motivation", "dualuse", "breakdown_anim2.jpeg"),
        width = 10, height = 5.5, dpi = 300)
}


decompose_levels <- function(level = "4digit") {
    trade <- fread(file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs4.csv"), keepLeadingZeros = TRUE)
    hscode <- fread(file.path(OUTPUT_PATH, 'crosswalks', 'H4_codes.csv'))
    trade <- merge_df(trade, hscode, by.x = "hscode", by.y = "sector", how = "left")
    trade[hscode == "271000", hscode_name := "Oils; petroleum oils and oils obtained from bituminous minerals, not crude; preparations n.e.s., containing by weight 70% or more of petroleum oils or oils obtained from bituminous minerals"]
    trade[, perc_value := perc_value * 100]
    trade[, hs2 := substr(hscode, 1, 2)]

    i <- 5
    for (i in seq(5, length(colnames(trade)))) {
        yr_now <- colnames(trade)[i]

        # based on trade share
        trade_add <- trade[get(yr_now) == 1, .(hscode, hscode_name, perc_value, hs2)]
        if (level == "4digit") {
            trade_add[, hscode := substr(hscode, 1, 4)]
            trade_add <- trade_add[, lapply(.SD, sum), .SDcols = "perc_value", by = c("hscode", "hs2")]
            trade_add <- merge_df(trade_add, hscode, by.x = "hscode", by.y = "sector", how = "left")
        }
        trade_add[, count := .N, by = "hs2"]
        trade_add <- trade_add[order(-count, -perc_value)][, .(hscode, hscode_name, count, perc_value)]
        
        trade_add_latex <- copy(trade_add)
        colnames(trade_add_latex) <- c("HS code", "Description", "HS2 \\#", "Trade (\\%)")
        if (nrow(trade_add_latex) > 10) {
            trade_add_latex <- trade_add_latex[1:10]
        }
        if (level == "4digit") {
            colnames(trade_add_latex) <- c("HS code", "Description", "HS2 \\#", "Trade (\\%)")
        }
        
        latex_code <- xtable(trade_add_latex, include.rownames = FALSE, digits = 3)
        latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
        latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
        latex_code <- sub("\\{llrr\\}", "{lp{13cm}rr}", latex_code)
        write(latex_code, file.path(STATS_PATH, "motivation", "dualuse", paste0(yr_now, level, "_lvl_share.tex")))

        # based on HS2 category
        trade_add <- trade_add[order(-count, -perc_value)]
        colnames(trade_add) <- c("HS code", "Description", "HS2 \\#", "Trade (\\%)")
        if (nrow(trade_add) > 10) {
            trade_add <- trade_add[1:10]
        }
        if (level == "4digit") {
            colnames(trade_add) <- c("HS code", "Description", "HS2 \\#", "Trade (\\%)")
        }

        latex_code <- xtable(trade_add, include.rownames = FALSE, digits = 3)
        latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
        latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
        latex_code <- sub("\\{llrr\\}", "{lp{13cm}rr}", latex_code)
        write(latex_code, file.path(STATS_PATH, "motivation", "dualuse", "dualuse_inclusions", paste0(yr_now, level, "_lvl_categ.tex")))
    }
}


decompose_shifts <- function(level = "4digit") {
    trade <- fread(file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs4.csv"), keepLeadingZeros = TRUE)
    hscode <- fread(file.path(OUTPUT_PATH, 'crosswalks', 'H4_codes.csv'))
    trade <- merge_df(trade, hscode, by.x = "hscode", by.y = "sector", how = "left")
    trade[hscode == "271000", hscode_name := "Oils; petroleum oils and oils obtained from bituminous minerals, not crude; preparations n.e.s., containing by weight 70% or more of petroleum oils or oils obtained from bituminous minerals"]
    trade[, perc_value := perc_value * 100]
    trade[, hs2 := substr(hscode, 1, 2)]

    i <- 6
    for (i in seq(6, length(colnames(trade)))) {
        yr_past <- colnames(trade)[i - 1]
        yr_now <- colnames(trade)[i]

        # based on trade share
        trade_add <- trade[get(yr_now) == 1 & get(yr_past) == 0, .(hscode, hscode_name, perc_value, hs2)]
        trade_del <- trade[get(yr_now) == 0 & get(yr_past) == 1, .(hscode, hscode_name, perc_value, hs2)]
        if (level == "4digit") {
            trade_add[, hscode := substr(hscode, 1, 4)]
            trade_del[, hscode := substr(hscode, 1, 4)]
            
            trade_add <- trade_add[, lapply(.SD, sum), .SDcols = "perc_value", by = c("hscode", "hs2")]
            trade_del <- trade_del[, lapply(.SD, sum), .SDcols = "perc_value", by = c("hscode", "hs2")]

            trade_add <- merge_df(trade_add, hscode, by.x = "hscode", by.y = "sector", how = "left")
            trade_del <- merge_df(trade_del, hscode, by.x = "hscode", by.y = "sector", how = "left")
        }
        trade_add[, count := .N, by = "hs2"]
        trade_del[, count := .N, by = "hs2"]
        trade_add <- trade_add[order(-count, -perc_value)][, .(hscode, hscode_name, count, perc_value)]
        trade_del <- trade_del[order(-count, -perc_value)][, .(hscode, hscode_name, count, perc_value)]
        
        trade_add_latex <- copy(trade_add)
        colnames(trade_add_latex) <- c("HS code", "Description", "HS2 \\#", "Trade (\\%)")
        if (nrow(trade_add_latex) > 10) {
            trade_add_latex <- trade_add_latex[1:10]
        }
        trade_del_latex <- copy(trade_del)
        if (nrow(trade_del_latex) > 10) {
            trade_del_latex <- trade_del_latex[1:10]
        }
        colnames(trade_del_latex) <- c("HS code", "Description", "HS2 del-ns", "Trade (\\%)")
        if (level == "4digit") {
            colnames(trade_add_latex) <- c("HS code", "Description", "HS2 \\#", "Trade (\\%)")
            colnames(trade_del_latex) <- c("HS code", "Description", "HS2 \\#", "Trade (\\%)")
        }
        
        latex_code <- xtable(trade_add_latex, include.rownames = FALSE, digits = 3)
        latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
        latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
        latex_code <- sub("\\{llrr\\}", "{lp{13cm}rr}", latex_code)
        write(latex_code, file.path(STATS_PATH, "motivation", "dualuse", paste0(yr_now, level, "_add_share.tex")))

        latex_code <- xtable(trade_del_latex, include.rownames = FALSE, digits = 3)
        latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
        latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
        latex_code <- sub("\\{llrr\\}", "{lp{13cm}rr}", latex_code)
        write(latex_code, file.path(STATS_PATH, "motivation", "dualuse", paste0(yr_now, level, "_del_share.tex")))

        # based on HS2 category
        trade_add <- trade_add[order(-count, -perc_value)]
        colnames(trade_add) <- c("HS code", "Description", "HS2 \\#", "Trade (\\%)")
        trade_del <- trade_del[order(-count, -perc_value)]
        colnames(trade_del) <- c("HS code", "Description", "HS2 del-ns", "Trade (\\%)")
        if (level == "4digit") {
            colnames(trade_add) <- c("HS code", "Description", "HS2 \\#", "Trade (\\%)")
            colnames(trade_del) <- c("HS code", "Description", "HS2 \\#", "Trade (\\%)")
        }

        if (nrow(trade_add) > 10) {
            trade_add <- trade_add[1:10]
        }
        if (nrow(trade_del) > 10) {
            trade_del <- trade_del[1:10]
        }

        latex_code <- xtable(trade_add, include.rownames = FALSE, digits = 3)
        latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
        latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
        latex_code <- sub("\\{llrr\\}", "{lp{13cm}rr}", latex_code)
        write(latex_code, file.path(STATS_PATH, "motivation", "dualuse", "dualuse_inclusions", paste0(yr_now, level, "_add_categ.tex")))

        latex_code <- xtable(trade_del, include.rownames = FALSE, digits = 3)
        latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
        latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
        latex_code <- sub("\\{llrr\\}", "{lp{13cm}rr}", latex_code)
        write(latex_code, file.path(STATS_PATH, "motivation", "dualuse", "dualuse_inclusions", paste0(yr_now, level, "_del_categ.tex")))
    }
}


plot_shares()
plot_counts()
plot_levels()
# decompose_levels(level = "")
# decompose_levels(level = "hs4")
# decompose_shifts(level = "")
# decompose_shifts(level = "hs4")

