### Summarize dual-use lists

rm(list=ls())
gc()

library(data.table)
library(ggrepel)
library(fixest)
library(xtable)
library(stringr)
library(latex2exp)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


summarize_words <- function(num_gap = 39, min_num_gap = 1, cent_gap = 0.02, min_cent_gap = 0.00, filename = "top_words.tex") {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12_4digit.csv"), keepLeadingZeros = TRUE)
    cent[, id := 1:.N]
    group_num <- 1
    id_num <- 1
    C_num <- cent[id == 1, C_M_sigma]
    for (i in 1:nrow(cent)) {
        if (i + num_gap > nrow(cent)) {
            cent[is.na(group), group := group_num]
            break
        }
        if (i > id_num + num_gap & cent[id == i, C_M_sigma] < C_num - min_cent_gap) {
            id_num <- i
            C_num <- cent[id == i, C_M_sigma]
            group_num <- group_num + 1
        } else if ((cent[id == i, C_M_sigma] < C_num - cent_gap) & (i > id_num + min_num_gap)) {
            id_num <- i
            C_num <- cent[id == i, C_M_sigma]
            group_num <- group_num + 1
        }
        cent[id == i, group := group_num]
    }
    cent[, bin_C_M_sigma := -group]
    cent_count <- cent[, .N, by = c("bin_C_M_sigma")]
    cent_interv <- cent[, paste0("\n[", sprintf("%.2f", min(C_M_sigma * 100)), ", ", sprintf("%.2f", max(C_M_sigma * 100)), "]"), by = c("bin_C_M_sigma")]
    cent_rank_interv <- cent[, paste0("\n[", sprintf("%.1f", min(rank_C_M_sigma * 100)), ", ", sprintf("%.1f", max(rank_C_M_sigma * 100)), "]"), by = c("bin_C_M_sigma")]
    
    cent[, hscode_name := tolower(hscode_name)]
    cent[, hscode_name := gsub("[[:punct:]]", " ", hscode_name)]
    cent[, hscode_name := gsub("[0-9]", "", hscode_name)]

    words_dt <- cent[, .(word = unlist(strsplit(hscode_name, "\\s+"))), by = c("bin_C_M_sigma")]
    result <- words_dt[, .N, by = .(bin_C_M_sigma, word)][order(-N)]
    stopwords <- c(
        "and", "or", "the", "of", "in", "to", "for", "a", "an", "up", "such", "kg", "kn", "kw",
        "with", "by", "n", "e", "c", "s", "g", "m", "containing", "under", "articles", "very", "like",
        "otherwise", "which", "is", "chapter", "made", "base", "fitted", "main", "function", "any", "products", "based",
        "women", "girls", "persons",
        "but", "parts", "exceeding", "derivatives", "no", "mm", "non",
        "more", "less", "item", "those", "similar", "width", "weight", "uses",
        "on", "at", "from", "into", "other", "than", "etc", "not", "as",
        "its", "their", "thereof", "therein", "thereto", "therefrom",
        "including", "included", "except", "excepted", "excluding", "excluded", "heading",
        "whether", "used", "being", "having"
    )
    result <- result[!(word %in% stopwords)]
    result <- result[word != ""]

    top_words <- result[order(-N), .SD[1:10], by = bin_C_M_sigma]
    top_words[, word_list := paste(word, collapse = ", "), by = bin_C_M_sigma]
    top_words_summary <- unique(top_words[,.(bin_C_M_sigma, word_list)])

    top_words_summary <- merge_df(cent_count, top_words_summary, by = "bin_C_M_sigma", indicator = FALSE)
    top_words_summary <- merge_df(cent_interv, top_words_summary, by = "bin_C_M_sigma", indicator = FALSE)    
    top_words_summary <- merge_df(cent_rank_interv, top_words_summary, by = "bin_C_M_sigma", indicator = FALSE)
    top_words_summary[, word_list := gsub(", NA", "", word_list)]
    setorder(top_words_summary, -bin_C_M_sigma)
    top_words_summary[, bin_C_M_sigma := NULL]
    colnames(top_words_summary) <- c("Pct $\\mathcal{C}^M_k/\\sigma$", "$\\mathcal{C}^M_k/\\sigma$ (\\%)", "N", "Key words in HS code descriptions")

    latex_code <- xtable(top_words_summary, include.rownames = FALSE)
	latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
	latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
    latex_code <- gsub("\\{llrl\\}\n  \\\\hline", "{cccl}\n \\\\toprule \\\\midrule", latex_code)
    latex_code <- gsub("\\\\hline", "\\\\midrule ", latex_code)
    latex_code <- gsub("\n\\\\end\\{", "\\\\bottomrule \n \\\\end{", latex_code)
	write(latex_code, file.path(STATS_PATH, "measurement", "centrality", filename))
}


summarize_words(filename = "top_words.tex")
summarize_words(num_gap = 74, min_num_gap = 4, cent_gap = 0.02, min_cent_gap = 0.00, filename = "top_words_slides.tex")


output_reg <- function() {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)
    cent[, rank_sigma := rank(-sigma) / sum(!is.na(sigma))]
    vars <- c(
        "C_M_sigma", "rank_C_M_sigma",
        "C_M", "rank_C_M",
        "S_M", "rank_S_M",  
        "s_M", "rank_s_M",
        "cent_M", "rank_cent_M",
        "s_C", "rank_s_C",
        "cent_C", "rank_cent_C"
    )
    cent <- cent[, ..vars]
    new_vars <- c(
        "$\\mathcal{C}^M/\\sigma$", "pct $\\mathcal{C}^M/\\sigma$",
        "$\\mathcal{C}^M$", "pct $\\mathcal{C}^M$",
        "$\\mathcal{S}^M$", "pct $\\mathcal{S}^M$", 
        "$s^M$", "pct $s^M$",
        "$\\Psi' s^M$", "pct $\\Psi' s^M$",
        "$s^C$", "pct $s^C$",
        "$\\Psi' s^C$", "pct $\\Psi' s^C$"
    )
    colnames(cent) <- new_vars
    cor_matrix <- cor(cent, use = "pairwise.complete.obs")
    
    # Convert to data.table for formatting
    df <- as.data.table(cor_matrix)
    df[, Variable := new_vars]
    setcolorder(df, c("Variable", new_vars))
    
    # Format numbers and create LaTeX table
    df[, id := 1:.N]
    for (k in seq_along(new_vars)) {
        col <- new_vars[k]
        df[, (col) := sprintf("%.2f", get(col))]
        df[id > k, (col) := ""]
    }
    df[, id := NULL]

    latex_code <- xtable(df, include.rownames = FALSE)
    latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
    latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
    latex_code <- gsub("\\{lllllllllllllll\\}", "{r|cccccccccccccc} \\\\toprule ", latex_code)
    latex_code <- gsub("\\\\hline", "\\\\midrule", latex_code)
    latex_code <- gsub("\n\\\\end\\{", "\\\\bottomrule \n \\\\end{", latex_code)
    write(latex_code, file.path(STATS_PATH, "measurement", "centrality", "corrtable.tex"))
}

output_reg()


plot_distribution <- function() {
    cent <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)
    vars <- c(
        "C_M_sigma", "rank_C_M_sigma",
        "C_M", "rank_C_M",
        "S_M", "rank_S_M",  
        "s_M", "rank_s_M",
        "cent_M", "rank_cent_M",
        "s_C", "rank_s_C",
        "cent_C", "rank_cent_C"
    )
    cent <- cent[, ..vars]
    ggplot(data = cent) +
        geom_line(aes(x = rank_C_M_sigma, y = C_M_sigma, color = "C_M_sigma", linetype = "C_M_sigma"), linewidth = 1.0) +
        geom_line(aes(x = rank_C_M, y = C_M, color = "C_M", linetype = "C_M"), linewidth = 1.0) +
        geom_line(aes(x = rank_S_M, y = S_M, color = "S_M", linetype = "S_M"), linewidth = 1.0) +
        geom_line(aes(x = rank_s_M, y = s_M, color = "s_M", linetype = "s_M"), linewidth = 1.0) +
        geom_line(aes(x = rank_cent_M, y = cent_M, color = "cent_M", linetype = "cent_M"), linewidth = 1.0) +
        geom_line(aes(x = rank_s_C, y = s_C, color = "s_C", linetype = "s_C"), linewidth = 1.0) +
        geom_line(aes(x = rank_cent_C, y = cent_C, color = "cent_C", linetype = "cent_C"), linewidth = 1.0) +
        scale_color_manual(name = "Variable",
                          values = c("C_M_sigma" = royalred,
                                   "C_M" = royalblue, 
                                   "S_M" = lightblue,
                                   "s_M" = "black",
                                   "cent_M" = "black",
                                   "s_C" = gold,
                                   "cent_C" = gold),
                          labels = c("C_M_sigma" = TeX("$C^M/\\sigma$"),
                                   "C_M" = TeX("$C^M$"),
                                   "S_M" = TeX("$S^M$"),
                                   "s_M" = TeX("$s^M$"),
                                   "cent_M" = TeX("$\\Psi' s^M$"),
                                   "s_C" = TeX("$s^C$"), 
                                   "cent_C" = TeX("$\\Psi' s^C$"))) +
        scale_linetype_manual(name = "Variable",
                            values = c("C_M_sigma" = "solid",
                                     "C_M" = "solid",
                                     "S_M" = "solid", 
                                     "s_M" = "dashed",
                                     "cent_M" = "dotted",
                                     "s_C" = "dashed",
                                     "cent_C" = "dotted"),
                            labels = c("C_M_sigma" = TeX("$C^M/\\sigma$"),
                                     "C_M" = TeX("$C^M$"),
                                     "S_M" = TeX("$S^M$"),
                                     "s_M" = TeX("$s^M$"),
                                     "cent_M" = TeX("$\\Psi' s^M$"),
                                     "s_C" = TeX("$s^C$"),
                                     "cent_C" = TeX("$\\Psi' s^C$"))) +
        scale_x_continuous(labels = scales::percent) +
        labs(x = "Rank", y = "Value") +
        custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "centrality", "cdf_keyvars.jpeg"), width = 10, height = 5.5, dpi = 300)
}


plot_distribution()


output_plot <- function() {
    df_naics <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)
    df_naics[, naics12 := as.character(naics12)]
    df1 <- df_naics[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c("sigma"), by = c("naics12")]
    df2 <- df_naics[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("perc_value"), by = c("naics12")]
    df3 <- df_naics[, lapply(.SD, max, na.rm = TRUE), .SDcols = c("dualuse_outcome"), by = c("naics12")]
    df_naics <- merge_df(df1, df2, by = "naics12", how = "outer", indicator = FALSE)
    df_naics <- merge_df(df_naics, df3, by = "naics12", how = "outer", indicator = FALSE)

    df <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_naics12.csv"), keepLeadingZeros = TRUE)
    df <- merge_df(df, df_naics, by.x = "sector", by.y = "naics12", how = "inner", indicator = FALSE)
    df[, perc_value := perc_value * 10000]
    pv_q <- quantile(df$perc_value, 0.95, na.rm = TRUE)
    df[perc_value > pv_q, perc_value := pv_q]
    df[grepl("Farming", sector_name), sector_name := gsub("Farming", "Frm", sector_name)]
    df[grepl("Manufacturing", sector_name), sector_name := gsub("Manufacturing", "Mfg", sector_name)]
    df[, sector_name_text := ""]
    df[, rank_X := rank(-X)]
    df[, rank_trade := rank(-perc_value)]
    df[, rank_C_M := rank(-C_M)]
    df[, rank_sigma := rank(-sigma)]
    df[rank_X < 10 | rank_C_M < 10 | rank_trade < 5 | rank_sigma < 10, sector_name_text := sector_name]

    ggplot(data = df[dualuse_outcome == 0], aes(x = C_M, y = sigma, size = X, fill = perc_value)) +
        geom_point(shape = 21, stroke = 1) +
        scale_size_continuous(range = c(1, 10), name = "U.S. output, bln $") +
        scale_fill_gradient(name = "Global trade, bps", high = royalred, low = "white") +
        geom_point(data = df[dualuse_outcome == 1], shape = 23, stroke = 1) +
        geom_text_repel(data = df[sector_name_text != ""], aes(label = sector_name_text), box.padding = 0.2, size = 3.7, family = "Erewhon") +
        xlab(TeX("$C^M$")) + ylab(TeX("$\\sigma$")) +
        custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "centrality", "naics_sigma.jpeg"), width = 10, height = 12, dpi = 300)

    ggplot(data = df[dualuse_outcome == 0], aes(x = C_M, y = sigma, size = X, fill = perc_value)) +
        geom_point(shape = 21, stroke = 1) +
        scale_size_continuous(range = c(1, 10), name = "U.S. output, bln $") +
        scale_fill_gradient(name = "Global trade, bps", high = royalred, low = "white") +
        geom_point(data = df[dualuse_outcome == 1], shape = 23, stroke = 1) +
        geom_text_repel(data = df[sector_name_text != ""], aes(label = sector_name_text), box.padding = 0.2, size = 3.7, family = "Erewhon") +
        xlab(TeX("$C^M$")) + ylab(TeX("$\\sigma$")) +
        custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "centrality", "naics_sigma_slides.jpeg"), width = 10, height = 5.5, dpi = 300)
}

output_plot()

