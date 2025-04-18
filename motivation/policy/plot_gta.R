### Explore GTA

rm(list=ls())
gc()

library(data.table)
library(fixest)
library(stringr)
library(ggplot2)
library(ggrepel)
library(xtable)
library(latex2exp)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")
source("./motivation/policy/policy_names.R")


### GTA
prepare_data <- function() {
    load(file.path(DATA_PATH, "motivation", "policy", "GTA+Database+2008-latest.Rdata"))
    df <- data.table(master)
    colnames(df) <- c(
        "id_act", "id_policy", "act_title", "date_announced", "gta_eval", "is_active",
        "date_started", "date_ended", "country", "gov_level", "firms_elibible", "type_policy",
        "chapter", "sectors", "products", "country_against"
    )
    df <- df[!is.na(products)]
    df[, id := seq(1, nrow(df))]

    df_unlist <- df[, list(hscode = unlist(strsplit(products, split = ",\\s*"))), by = id]
    df <- merge_df(df, df_unlist, by = "id", how = "left")
    df <- df[, .SD[1], .SDcols = c("type_policy"), by = c("id_policy", "hscode")]
    df[startsWith(hscode, "2710"), hscode := "271000"]

    dualuse <- fread(file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs4.csv"), keepLeadingZeros = TRUE)
    dualuse <- dualuse[, c("sector", "dualuse_2018", "military"), with = FALSE]
    df <- merge_df(df, dualuse, by.y = "sector", by.x = "hscode", how = "inner", indicator = FALSE)
    
    df[type_policy == "Import tariff", type_policy := "0. Import tariff"]
    reg <- feols(dualuse_2018 ~ factor(type_policy), data = df, se = "hetero")
    se <- sqrt(diag(vcov(reg, se = "hetero")))

    coeftable <- data.table(
        names = names(reg$coefficients),
        coef = reg$coefficients,
        se = se
    )
    coeftable[, names := str_replace(names, "factor\\(type_policy\\)", "")]
    coeftable[, names := str_replace(names, "abour", "abor")]
    value <- coeftable[names == "(Intercept)", coef]
    coeftable[names == "(Intercept)", coef := 0]
    coeftable[names == "(Intercept)", se := 0]
    coeftable[names == "(Intercept)", special_names := "Import tariff"]
    coeftable[names == "(Intercept)", names := NA]
    coeftable[, coef := coef + value]
    coeftable[names != "Import tariff", coef_upper := coef + 1.96 * se]
    coeftable[names != "Import tariff", coef_lower := coef - 1.96 * se]
    coeftable <- coeftable[order(-coef)]
    fwrite(coeftable, file.path(STATS_PATH, "motivation", "policy", "coeftable.csv"))
    coeftable[, id := seq(1, nrow(coeftable))]
    coeftable[, greater := coef > value]
    coeftable[id == 32, names_graph := names]

    coeftable[id <= 5, names_graph := names]
    coeftable[
        (grepl("procurement", tolower(names)) & (id < 32))
        | (grepl("local", tolower(names)) & (id < 32))
        | (grepl("export", tolower(names)) & (id < 32))
        | (grepl("intellectual", tolower(names)) & (id < 32))
        | (grepl("monitor", tolower(names)) & (id < 32))
        | (grepl("import", tolower(names)) & (id >= 32))
        | (grepl("interest", tolower(names)) & (id >= 32)),
        names_graph := names
    ]

    ggplot(data = coeftable, aes(x = id, y = coef, fill = greater)) +
        geom_bar(stat = "identity", color = "white", alpha = 0.9) + 
        geom_bar(data = coeftable[special_names == "Import tariff"], stat = "identity", fill = royalred, alpha = 0.5, color = "white") +
        geom_errorbar(aes(ymin = coef_lower, ymax = coef_upper)) +
        scale_x_discrete(labels = coeftable$names) +
        scale_fill_manual(values = c(lightblue, royalblue)) +
        xlab("Trade and industrial policies by type") +
        ylab("Dual-use share, announcements") +
        geom_label_repel(aes(x = id, y = coef, label = names_graph), fill = "white",
            direction = "y", hjust = -0.1, nudge_y = 0.01, family = "Erewhon") + 
        geom_label_repel(aes(x = id, y = coef, label = special_names), fill = "white", color = royalred, fontface = "bold",
            direction = "y", label.size = 0.7, hjust = -0.1, nudge_y = 0.01, segment.size = 0, family = "Erewhon") +
        scale_y_continuous(labels = scales::percent) +
        custom_theme +
        theme(legend.position = "none", axis.text.x = element_blank(),  panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
    ggsave(file.path(STATS_PATH, "motivation", "policy", "policy_top.jpeg"), width = 10, height = 5.5, dpi = 300)

    baseline <- coeftable[!is.na(special_names), coef]
    du_list <- str_replace(c(coeftable[coef > baseline, names], "", ""), "\\&", "\\\\&")
    nondu_list <- c("Import tariff", coeftable[coef < baseline, names])
    du_list[!(du_list %in% security_policies)] <- paste0("\\textcolor{magenta}{", du_list[!(du_list %in% security_policies)], "}")
    nondu_list[(nondu_list %in% security_policies)] <- paste0("\\textcolor{magenta}{", nondu_list[(nondu_list %in% security_policies)], "}")
    policy_table <- data.table(
        "Security policies" = du_list,
        "Other policies" = nondu_list
    )
    latex_code <- xtable(policy_table, include.rownames = FALSE)
	latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
	latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
	latex_code <- sub("\\{ll\\}", "{rl}", latex_code)
	write(latex_code, file.path(STATS_PATH, "motivation", "policy", "policy_list.tex"))
}

# prepare_data()


run_regression_over_time <- function() {
    load(file.path(DATA_PATH, "motivation", "policy", "GTA+Database+2008-latest.Rdata"))
    df <- data.table(master)
    colnames(df) <- c(
        "id_act", "id_policy", "act_title", "date_announced", "gta_eval", "is_active",
        "date_started", "date_ended", "country", "gov_level", "firms_elibible", "type_policy",
        "chapter", "sectors", "products", "country_against"
    )
    df <- df[!is.na(products)]
    df[, id := seq(1, nrow(df))]
    df[, year := year(date_announced)]

    df_unlist <- df[, list(hscode = unlist(strsplit(products, split = ",\\s*"))), by = id]
    df <- merge_df(df, df_unlist, by = "id", how = "left")
    df <- df[, .SD[1], .SDcols = c("year", "type_policy"), by = c("id_policy", "hscode")]
    df[startsWith(hscode, "2710"), hscode := "271000"]

    coeftable <- fread(file.path(STATS_PATH, "motivation", "policy", "coeftable.csv"))
    coef_import <- coeftable[special_names != "", coef]
    df[, ntm := as.integer(type_policy %in% security_policies)]

    dualuse <- fread(file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs4.csv"), keepLeadingZeros = TRUE)
    dualuse <- dualuse[, c("sector", "dualuse_2018"), with = FALSE]
    df <- merge_df(df, dualuse, by.y = "sector", by.x = "hscode", how = "inner", indicator = FALSE)

    df[dualuse_2018 == 1 & ntm == 1, factor_ntm_dualuse := "Dual-use + security"]
    df[dualuse_2018 == 1 & ntm == 0, factor_ntm_dualuse := "Only dual-use"]
    df[dualuse_2018 == 0 & ntm == 1, factor_ntm_dualuse := "Only security"]
    df[dualuse_2018 == 0 & ntm == 0, factor_ntm_dualuse := "Other policies"]

    df_count <- df[, .N, by = c("year", "factor_ntm_dualuse")]
    df_count <- df_count[order(year, factor_ntm_dualuse)]
    df_count[year <= 2019 & year >= 2008, base_count := mean(log(N)), by = c("factor_ntm_dualuse")]
    df_count[, base_count := max(base_count, na.rm = TRUE), by = c("factor_ntm_dualuse")]
    df_count[, log_count := log(N)]    
    df_count[, diff_count := log_count - base_count]
    df_count[factor_ntm_dualuse == "Other policies", diff_count_base := diff_count]
    df_count[, diff_count_base := max(diff_count_base, na.rm = TRUE), by = c("year")]
    df_count[, double_diff := diff_count - diff_count_base]
    df_count[, double_diff_cum := cumsum(double_diff), by = c("factor_ntm_dualuse")]

    ggplot(data = df_count, aes(x = year, y = double_diff, color = factor_ntm_dualuse)) +
        geom_point(size = 4, shape = 1, stroke = 1) +
        scale_color_manual(
            values = c(royalblue, lightblue, royalred, "black")) +
        xlab("") + ylab("log(announcements)") +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Dual-use + security" & year <= 2019],
            aes(x = year, y = double_diff), color = royalblue, size = 1.5, alpha = 0.1, fill = royalblue,
            method = 'lm', se = TRUE, formula = y ~ x, level = 0.95) +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Dual-use + security" & year >= 2019],
            aes(x = year, y = double_diff), color = royalblue, size = 1.5, alpha = 0.1, fill = royalblue,
            method = 'lm', se = TRUE, formula = y ~ x, level = 0.95) +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Only dual-use" & year <= 2019],
            aes(x = year, y = double_diff), color = lightblue, size = 1.5, alpha = 0.1, fill = lightblue,
            method = 'lm', se = TRUE, formula = y ~ x, level = 0.95) +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Only dual-use" & year >= 2019],
            aes(x = year, y = double_diff), color = lightblue, size = 1.5, alpha = 0.1, fill = lightblue,
            method = 'lm', se = TRUE, formula = y ~ x, level = 0.95) +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Only security" & year <= 2019],
            aes(x = year, y = double_diff), color = royalred, size = 1.5, alpha = 0.1, fill = royalred,
            method = 'lm', se = TRUE, formula = y ~ x, level = 0.95) +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Only security" & year >= 2019],
            aes(x = year, y = double_diff), color = royalred, size = 1.5, alpha = 0.1, fill = royalred,
            method = 'lm', se = TRUE, formula = y ~ x, level = 0.95) +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Other policies"],
            aes(x = year, y = double_diff), color = "black", size = 1.5,
            method = 'lm', se = TRUE, formula = y ~ x, level = 0.95) +
        geom_vline(xintercept = 2019, size = 1.0, color = "black", linetype = "dashed") +
        scale_x_continuous(breaks = seq(2008, 2023, 2)) +
        labs(color = "Policy type") + 
        custom_theme
    ggsave(file.path(STATS_PATH, "motivation", "policy", "double_diff.jpeg"), width = 10, height = 5.5, dpi = 300)

    security_names <- coeftable[coef > coef_import, names]
    df[, ntm := as.integer(type_policy %in% security_names)]

    df[dualuse_2018 == 1 & ntm == 1, factor_ntm_dualuse := "Dual-use + security"]
    df[dualuse_2018 == 1 & ntm == 0, factor_ntm_dualuse := "Only dual-use"]
    df[dualuse_2018 == 0 & ntm == 1, factor_ntm_dualuse := "Only security"]
    df[dualuse_2018 == 0 & ntm == 0, factor_ntm_dualuse := "Other policies"]

    df_count <- df[, .N, by = c("year", "factor_ntm_dualuse")]
    df_count <- df_count[order(year, factor_ntm_dualuse)]
    df_count[year <= 2019 & year >= 2008, base_count := mean(log(N)), by = c("factor_ntm_dualuse")]
    df_count[, base_count := max(base_count, na.rm = TRUE), by = c("factor_ntm_dualuse")]
    df_count[, log_count := log(N)]    
    df_count[, diff_count := log_count - base_count]
    df_count[factor_ntm_dualuse == "Other policies", diff_count_base := diff_count]
    df_count[, diff_count_base := max(diff_count_base, na.rm = TRUE), by = c("year")]
    df_count[, double_diff := diff_count - diff_count_base]
    df_count[, double_diff_cum := cumsum(double_diff), by = c("factor_ntm_dualuse")]

    ggplot(data = df_count, aes(x = year, y = double_diff, color = factor_ntm_dualuse)) +
        geom_point(size = 4, shape = 1, stroke = 1) +
        scale_color_manual(values = c(royalblue, lightblue, royalred, "black")) +
        xlab("") + ylab("log(announcements)") +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Dual-use + security" & year <= 2019],
            aes(x = year, y = double_diff), color = royalblue, size = 1.5, alpha = 0.1, fill = royalblue,
            method = 'lm', se = TRUE, formula = y ~ x, level = 0.95) +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Dual-use + security" & year >= 2019],
            aes(x = year, y = double_diff), color = royalblue, size = 1.5, alpha = 0.1, fill = royalblue,
            method = 'lm', se = TRUE, formula = y ~ x, level = 0.95) +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Only dual-use" & year <= 2019],
            aes(x = year, y = double_diff), color = lightblue, size = 1.5, alpha = 0.1, fill = lightblue,
            method = 'lm', se = TRUE, formula = y ~ x, level = 0.95) +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Only dual-use" & year >= 2019],
            aes(x = year, y = double_diff), color = lightblue, size = 1.5, alpha = 0.1, fill = lightblue,
            method = 'lm', se = TRUE, formula = y ~ x, level = 0.95) +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Only security" & year <= 2019],
            aes(x = year, y = double_diff), color = royalred, size = 1.5, alpha = 0.1, fill = royalred,
            method = 'lm', se = TRUE, formula = y ~ x, level = 0.95) +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Only security" & year >= 2019],
            aes(x = year, y = double_diff), color = royalred, size = 1.5, alpha = 0.1, fill = royalred,
            method = 'lm', se = TRUE, formula = y ~ x, level = 0.95) +
        geom_smooth(
            data = df_count[factor_ntm_dualuse == "Other policies"],
            aes(x = year, y = double_diff), color = "black", size = 1.5,
            method = 'lm', formula = y ~ x, se = TRUE, level = 0.95) +
        geom_vline(xintercept = 2019, size = 1.0, color = "black", linetype = "dashed") +
        scale_x_continuous(breaks = seq(2008, 2023, 2)) +
        labs(color = "Policy type") + 
        custom_theme
    ggsave(file.path(STATS_PATH, "motivation", "policy", "double_diff_nomanual.jpeg"), width = 10, height = 5.5, dpi = 300)
}


run_regression_over_time()

