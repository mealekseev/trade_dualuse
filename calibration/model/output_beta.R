### Outputs beta estimates

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


# convert beta
get_beta <- function() {
    df_res <- fread(file.path(STATS_PATH, "calibration", "model", "beta_estimates.csv"), keepLeadingZeros = TRUE)
    for (col in colnames(df_res)) {
        df_res[, (col) := 100 * get(col)]
    }
    df_res[, name := c("Military value", "Partial equilibrium", "General equilibrium")]
    df_res[, pct_empty := ""]
    df_res[, pct_stock := ""]
    df_res[, pct_allies := ""]
    df_res[, pct_total := ""]
    df_res <- df_res[,
        c(
            "name", "pct_empty_CHN", "pct_empty_USA", 
            "pct_stock", "pct_stock_CHN", "pct_stock_USA",
            "pct_allies", "pct_allies_CHN", "pct_allies_USA",
            "pct_total", "pct_total_CHN", "pct_total_USA"
        ), with = FALSE
    ]

    latex_code <- xtable(df_res, include.rownames = FALSE, digits = 2)
    latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, digits = 3, sanitize.text.function = identity)
    latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
    latex_string <- paste0(" & \\\\multicolumn{2}{c}{yearly budget} & & \\\\multicolumn{2}{c}{+ stock} & & \\\\multicolumn{2}{c}{+ allies' budgets} & & \\\\multicolumn{2}{c}{+ allies' stock} \\\\\\\\ \n", "\\\\cline{2-3} \\\\cline{5-6} \\\\cline{8-9} \\\\cline{11-12} \n", " & CHN & USA & & CHN & USA & & CHN & USA & & CHN & USA \\\\")
    latex_code <- gsub("lrrlrrlrrlrr", "lccccccccccc", latex_code)
    latex_code <- gsub("name.*?pct_total_USA \\\\", latex_string, latex_code)
    latex_code <- gsub("\\\\hline", "", latex_code)
    latex_code <- gsub("cc\\}\n  \n \\& \\\\mult", "cc}\n \\\\toprule \n & \\\\mult", latex_code)
    latex_code <- gsub(" Partial equilibrium", "\\\\hline Partial equilibrium", latex_code)
    latex_code <- gsub("\n\\\\end\\{", "\\\\bottomrule \n \\\\end{", latex_code)
    print(latex_code)
    write(latex_code, file.path(STATS_PATH, "calibration", "model", "beta_estimates.tex"))

    df_res <- fread(file.path(STATS_PATH, "calibration", "model", "changes.csv"), keepLeadingZeros = TRUE)
    df_res[, name := c("CHN", "USA", "ROW")]
    df_res[, change_CHN := ""]
    df_res[, change_USA := ""]
    df_res[, change_ROW := ""]
    df_res <- df_res[,
        c("name", "change_CHN", "change_CHN_w", "change_CHN_C", "change_CHN_M", "change_USA",
        "change_USA_w", "change_USA_C", "change_USA_M", "change_ROW",
        "change_ROW_w", "change_ROW_C", "change_ROW_M"), with = FALSE]
    latex_code <- xtable(df_res, include.rownames = FALSE, digits = 4)
    latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, digits = 4, sanitize.text.function = identity)
    latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
    latex_string <- paste0(" & & \\\\multicolumn{3}{c}{CHN} & & \\\\multicolumn{3}{c}{USA} & & \\\\multicolumn{3}{c}{ROW} \\\\\\\\ \n", " & & $w$ & $P^C$ & $P^M$ & & $w$ & $P^C$ & $P^M$ & & $w$ & $P^C$ & $P^M$ \\\\\\\\", "\n \\\\cline{3-5} \\\\cline{7-9} \\\\cline{11-13} \n")
    latex_code <- gsub("llrrrlrrrlrrr", "l|cccccccccccc", latex_code)
    latex_code <- gsub("name.*?change_ROW_M \\\\", latex_string, latex_code)
    latex_code <- gsub("\\\\hline", "", latex_code)
    latex_code <- gsub("cc\\}", "cc}\n \\\\toprule ", latex_code)
    latex_code <- gsub("\n\\\\end\\{", "\\\\bottomrule \n \\\\end{", latex_code)
    print(latex_code)
    write(latex_code, file.path(STATS_PATH, "calibration", "model", "changes.tex"))

    df_res <- fread(file.path(STATS_PATH, "calibration", "model", "shares.csv"), keepLeadingZeros = TRUE)
    df_res[, name := c("CHN", "USA", "ROW")]
    df_res[, share_CHN := ""]
    df_res[, share_USA := ""]
    df_res[, share_ROW := ""]
    df_res <- df_res[,
        c("name", "share_CHN", "share_CHN_C", "share_CHN_M",
        "share_USA", "share_USA_C", "share_USA_M",
        "share_ROW", "share_ROW_C", "share_ROW_M"), with = FALSE]
    latex_code <- xtable(df_res, include.rownames = FALSE, digits = 3)
    latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, digits = 3, sanitize.text.function = identity)
    latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
    latex_string <- paste0(" & & \\\\multicolumn{2}{c}{CHN} & & \\\\multicolumn{2}{c}{USA} & & \\\\multicolumn{2}{c}{ROW} \\\\\\\\ \n", " & & $C$ & $M$ & & $C$ & $M$ & & $C$ & $M$ \\\\\\\\", "\n \\\\cline{3-4} \\\\cline{6-7} \\\\cline{9-10} \n")
    latex_code <- gsub("llrrlrrlrr", "l|ccccccccc", latex_code)
    latex_code <- gsub("name.*?share_ROW_M \\\\", latex_string, latex_code)
    latex_code <- gsub("\\\\hline", "", latex_code)
    latex_code <- gsub("cc\\}", "cc}\n \\\\toprule ", latex_code)
    latex_code <- gsub("\n\\\\end\\{", "\\\\bottomrule \n \\\\end{", latex_code)
    print(latex_code)
    write(latex_code, file.path(STATS_PATH, "calibration", "model", "shares.tex"))
}


get_beta()

