### CSMAR data cleaning

rm(list=ls())
gc()

library(data.table)
library(ggrepel)
library(fixest)
library(xtable)
library(readxl)
library(stringr)
library(latex2exp)
library(zoo)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


eval_sanctions <- function() {
    df <- fread(file.path(OUTPUT_PATH, "open_sanctions", "centrality_sanctions.csv"), keepLeadingZeros = TRUE)
    df[, sum(wgt, na.rm = TRUE), by = c("title")]
    df_count <- fread(file.path(OUTPUT_PATH, "open_sanctions", "statements_filtered_rus.csv"), keepLeadingZeros = TRUE)
    df_count <- df_count[, length(unique(canonical_id)), by = "title"]
    colnames(df_count) <- c("title", "N")
    df <- merge_df(df, df_count, by.x = "title", by.y = "title", how = "left")
    df <- df[
        !startsWith(title, "Wikidata Entities of Interest") &
        !startsWith(title, "State Register of legal entities in the Republic of Moldova") &
        !startsWith(title, "EU Financial Instruments Reference Data System") &
        !startsWith(title, "ICIJ Offshore") &
        !startsWith(title, "Syrian Observatory of Political and Economic Networks") &
        !startsWith(title, "PermID Open Data") &
        !startsWith(title, "Legal Entity Identifier") &
        !startsWith(title, "Business Identifier Code") & 
        !startsWith(title, "OpenSanctions Default") &
        !startsWith(title, "Graph") &
        !startsWith(title, "OpenCorporates") &
        !startsWith(title, "Cyprus Companies and Corporate Officers") &
        !startsWith(title, "OpenSanctions Research Data")
    ]
    df[, title := paste0(title, " (N = ", N, ")")]

    reg <- feols(rank_C_M ~ 0 + factor(title), weights = df$wgt, data = df)
    coef <- reg$coefficients
    se <- sqrt(diag(vcov(reg, type = "hetero")))                                
    coeft <- data.table(
        names = names(coef),
        beta = coef,
        se = se
    )
    coeft[, beta_upper := beta + 1.96 * se]
    coeft[, beta_lower := beta - 1.96 * se]
    coeft[, names := str_replace(names, "factor\\(title\\)", "")]
    coeft <- coeft[order(-beta)]

    ggplot(data = coeft, aes(x = reorder(names, beta), y = beta)) +
        geom_point(size = 4, shape = 1, stroke = 1, color = royalblue) +
        geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper),
            linewidth = 1.0, width = 0.05, color = royalblue) +
        scale_y_continuous(labels = scales::percent) +
        geom_hline(yintercept = 0.5, linewidth = 1.0, linetype = "dashed") +
        labs(y = TeX("Percentile $C^M$"), x = "") +
        coord_flip() +
        custom_theme_slides
    ggsave(file.path(STATS_PATH, "open_sanctions", "centrality_sanctions.jpeg"), width = 10, height = 12, dpi = 300)

    coeft[, names := gsub("\\s*\\(N = \\d+\\)$", "", names)]
    coeft <- merge_df(coeft, df_count, by.x = "names", by.y = "title", how = "left")
    ggplot(data = coeft, aes(x = log(N), y = beta)) +
        geom_point(size = 4, shape = 1, stroke = 1, color = royalblue) +
        scale_y_continuous(labels = scales::percent) +
        geom_text_repel(aes(label = names), family = "Erewhon", size = 3.5) +
        xlab("log(entity count)") + ylab(TeX("Avg percentile $C^M$")) +
        custom_theme_slides + theme(legend.position = "none")
    ggsave(file.path(STATS_PATH, "open_sanctions", "scatter_sanctions.jpeg"), width = 10, height = 10, dpi = 300)

    ggplot(data = coeft, aes(x = log(N), y = beta)) +
        geom_point(size = 4, shape = 1, stroke = 1, color = royalblue) +
        geom_text_repel(aes(label = names), family = "Erewhon", size = 4.0) +
        scale_y_continuous(labels = scales::percent) +
        xlab("log(entity count)") + ylab(TeX("Avg percentile $C^M$")) +
        custom_theme_slides + theme(legend.position = "none")
    ggsave(file.path(STATS_PATH, "open_sanctions", "scatter_sanctions_slides.jpeg"), width = 7, height = 7, dpi = 300)
}


eval_sanctions()

