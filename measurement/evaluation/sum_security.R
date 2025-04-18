### Summarize security categories

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(stringr)
library(scales)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


plot_shares <- function() {
    df <- fread(file.path(OUTPUT_PATH, "eu_commission", "dualuse.csv"))
    df <- df[date == '2018-01-01']
    df_count <- df[, .N, by = c("dualuse_categ", "dualuse_subct")]
    for (i in 0:9) {
        for (j in c("A", "B", "C", "D", "E")) {
            if (nrow(df_count[dualuse_categ == i & dualuse_subct == j]) == 0) {
                df_count <- rbind(df_count, data.table(dualuse_categ = i, dualuse_subct = j, N = 0))
            }
        }
    }

    df_count[, dualuse_categ := as.character(dualuse_categ)]
    df_count[dualuse_categ == "0", dualuse_categ := "0. Nuclear"]
    df_count[dualuse_categ == "1", dualuse_categ := "1. Chemicals"]
    df_count[dualuse_categ == "2", dualuse_categ := "2. Processing"]
    df_count[dualuse_categ == "3", dualuse_categ := "3. Electronics"]
    df_count[dualuse_categ == "4", dualuse_categ := "4. Computers"]
    df_count[dualuse_categ == "5", dualuse_categ := "5. Telecom & IT"]
    df_count[dualuse_categ == "6", dualuse_categ := "6. Sensors/lasers"]
    df_count[dualuse_categ == "7", dualuse_categ := "7. Navigation"]
    df_count[dualuse_categ == "8", dualuse_categ := "8. Marine"]
    df_count[dualuse_categ == "9", dualuse_categ := "9. Aerospace"]

    df_count[dualuse_subct == "A", dualuse_subct := "A. End-use"]
    df_count[dualuse_subct == "B", dualuse_subct := "B. Production"]
    df_count[dualuse_subct == "C", dualuse_subct := "C. Materials"]
    df_count[dualuse_subct == "D", dualuse_subct := "D. Software"]
    df_count[dualuse_subct == "E", dualuse_subct := "E. Technology"]
    df_count[, dualuse_subct := factor(dualuse_subct, levels = sort(unique(dualuse_subct), decreasing = TRUE))]

    ggplot(df_count, aes(x = dualuse_categ, y = dualuse_subct, fill = log(N))) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = royalblue) +
        geom_text(aes(label = N), color = lightblue, size = 6, family = "Erewhon") +
        geom_text(data = df_count[N < 20], aes(label = N), color = "black", size = 6, family = "Erewhon") +
        labs(x = "", y = "", fill = "Count") +
        custom_theme_slides + theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
        # scale_x_discrete(position = "top") +
        # scale_y_discrete(position = "right")
    ggsave(file.path(STATS_PATH, "dualuse_plots", "dualuse_security_categ.jpeg"), width = 10, height = 5.5, dpi = 300)
}


plot_shares()


plot_trade_shares <- function() {
    df <- fread(file.path(OUTPUT_PATH, "eu_commission", "dualuse.csv"))
    df <- df[date == '2018-01-01']
    trade <- fread(file.path(OUTPUT_PATH, "cepii", "trade_shares.csv"), keepLeadingZeros = TRUE)
    df[, hscode12 := as.character(hscode12)]
    df <- merge_df(df, trade, how = "left", by.x = "hscode12", by.y = "hscode", indicator = FALSE)
    df[is.na(perc_value), perc_value := 0]

    df_count <- df[, sum(perc_value), by = c("dualuse_categ", "dualuse_subct")]
    for (i in 0:9) {
        for (j in c("A", "B", "C", "D", "E")) {
            if (nrow(df_count[dualuse_categ == i & dualuse_subct == j]) == 0) {
                df_count <- rbind(df_count, data.table(dualuse_categ = i, dualuse_subct = j, V1 = 0))
            }
        }
    }

    df_count[, dualuse_categ := as.character(dualuse_categ)]
    df_count[dualuse_categ == "0", dualuse_categ := "0. Nuclear"]
    df_count[dualuse_categ == "1", dualuse_categ := "1. Chemicals"]
    df_count[dualuse_categ == "2", dualuse_categ := "2. Processing"]
    df_count[dualuse_categ == "3", dualuse_categ := "3. Electronics"]
    df_count[dualuse_categ == "4", dualuse_categ := "4. Computers"]
    df_count[dualuse_categ == "5", dualuse_categ := "5. Telecom & IT"]
    df_count[dualuse_categ == "6", dualuse_categ := "6. Sensors/lasers"]
    df_count[dualuse_categ == "7", dualuse_categ := "7. Navigation"]
    df_count[dualuse_categ == "8", dualuse_categ := "8. Marine"]
    df_count[dualuse_categ == "9", dualuse_categ := "9. Aerospace"]

    df_count[dualuse_subct == "A", dualuse_subct := "A. End-use"]
    df_count[dualuse_subct == "B", dualuse_subct := "B. Production"]
    df_count[dualuse_subct == "C", dualuse_subct := "C. Materials"]
    df_count[dualuse_subct == "D", dualuse_subct := "D. Software"]
    df_count[dualuse_subct == "E", dualuse_subct := "E. Technology"]
    df_count[, dualuse_subct := factor(dualuse_subct, levels = sort(unique(dualuse_subct), decreasing = TRUE))]

    ggplot(df_count, aes(x = dualuse_categ, y = dualuse_subct, fill = log(V1))) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = royalblue) +
        geom_text(aes(label = percent(V1, accuracy = 0.01)), color = lightblue, size = 6, family = "Erewhon") +
        geom_text(data = df_count[V1 < 0.005], aes(label = percent(V1, accuracy = 0.01)), color = "black", size = 6, family = "Erewhon") +
        labs(x = "", y = "", fill = "Count") +
        custom_theme_slides + theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
        # scale_x_discrete(position = "top") +
        # scale_y_discrete(position = "right")
    ggsave(file.path(STATS_PATH, "dualuse_plots", "dualuse_security_trade_categ.jpeg"), width = 10, height = 5.5, dpi = 300)
}


plot_trade_shares()

