### Plot final plots

rm(list=ls())
gc()

library(data.table)
library(stringr)
library(haven)
library(fixest)
library(zoo)
library(xtable)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


plot_war_trade <- function() {
    file_list <- list.files(file.path(OUTPUT_PATH, "motivation", "events", "flow"))
    file_list <- file_list[startsWith(file_list, "vol_")]
    ols_list <- list()

    f <- file_list[1]
    i <- 1
    for (i in seq_along(file_list)) {
        f <- file_list[i]
        print(f)
        df <- fread(file.path(OUTPUT_PATH, "motivation", "events", "flow", f), keepLeadingZeros = TRUE)
        df <- df[spec == 3]
        ols_list[[i]] <- df
    }
    df <- rbindlist(ols_list)
    df[, period := year - year_start]
    df[, dualuse_cont := dualuse]
    df[, dualuse := dualuse_dummy]
    df[, sitcwgt_no := 1 / .N, by = c("war_num", "importer_code")]
    df[, sitcwgt_yes := value / sum(value), by = c("war_num", "importer_code")]
    df[, war_importer := paste0(war_num, "_", importer_code)]

    # throw out Portugal for Portugal-Mozambique and GBR for Falklands, as well as Iraq/Afghanistan for duplicates
    df <- df[!(war_num == 202 & importer_code == "GBR")]
    df <- df[!(war_num == 471 & importer_code == "PRT")]
    df <- df[!(war_num == 482)]
    df <- df[!(war_num == 481)]
    df <- df[!(war_num == 477 & importer_code == "IRQ")]
    df <- df[!(war_num == 193 & importer_code == "VDR")]

    # pooled
    wgtcol <- "wgt_no"
    sitcwgtcol <- "sitcwgt_no"
    df_ols <- df[wgt == wgtcol]
    reg <- feols(as.formula("coef ~ 0 + factor(period):factor(var)"), data = df_ols, weights = df_ols[[sitcwgtcol]])
    coef <- reg$coefficients
    se <- sqrt(diag(vcov(reg, type = "hetero")))
    coeft <- data.table(
        wgt_col = wgtcol,
        sitcwgt_col = sitcwgtcol,
        spec = "1",
        names = names(coef),
        coef = coef,
        se = se
    )
    coeft[, beta_upper := coef + 1.96 * se]
    coeft[, beta_lower := coef - 1.96 * se]

    coeft[, c("period", "var") := tstrsplit(names, ":")]                    
    coeft[, period := as.integer(str_replace(period, "factor\\(period\\)", ""))]
    coeft[, var := str_replace(var, "factor\\(var\\)", "")]
    coeft_base <- data.table(
        wgt_col = c(wgtcol, wgtcol), sitcwgt_col = c(sitcwgtcol, sitcwgtcol),
        spec = c("1", "1"), names = c("", ""),
        period = c(-1, -1), var = c("allies_exp", "enemies_exp"), coef = c(0, 0), se = c(0, 0), beta_lower = c(0, 0), beta_upper = c(0, 0))
    coeft <- rbind(coeft_base, coeft)
    coeft <- coeft[var != "importer_dummy"]

    ggplot(coeft[period <= 10 & period >= -9], aes(x = period, y = coef, color = factor(var), fill = factor(var))) +
        geom_line(linewidth = 1.0) +
        geom_point(size = 4, shape = 1, stroke = 1) +
        geom_vline(xintercept = -1, linetype = "dashed", linewidth = 1.0) +
        geom_ribbon(aes(ymin = beta_lower, ymax = beta_upper), alpha = 0.1, color = NA) +
        scale_color_manual(label = c("allies", "enemies"), values = c(royalblue, royalred), name = "Relationship") +
        scale_fill_manual(label = c("allies", "enemies"), values = c(royalblue, royalred), name = "Relationship") +
        xlab("period") + ylab("Relationship") +
        custom_theme
    ggsave(file.path(STATS_PATH, "stats", "motivation", "events", paste0("alliances_", substr(sitcwgtcol, 9, 9), substr(wgtcol, 5, 5), ".jpeg")), width = 10, height = 5.5, dpi = 300)

    reg <- feols(as.formula("coef ~ 0 + factor(period):factor(var):dualuse | period^var^importer_code"), data = df_ols, weights = df_ols[[sitcwgtcol]])
    summary(reg)
    coef <- reg$coefficients
    se <- sqrt(diag(vcov(reg, type = "hetero")))                
    coeft <- data.table(
        wgt_col = wgtcol,
        sitcwgt_col = sitcwgtcol,
        spec = "2",
        names = names(coef),
        coef = coef,
        se = se
    )
    coeft[, beta_upper := coef + 1.96 * se]
    coeft[, beta_lower := coef - 1.96 * se]
                                
    coeft[, c("period", "var", "dualuse") := tstrsplit(names, ":")]
    coeft <- coeft[!is.na(dualuse)]
    coeft[, dualuse := NULL]
    coeft[, period := as.integer(str_replace(period, "factor\\(period\\)", ""))]
    coeft[, var := str_replace(var, "factor\\(var\\)", "")]
    coeft_base <- data.table(
        wgt_col = c(wgtcol, wgtcol), sitcwgt_col = c(sitcwgtcol, sitcwgtcol),
        spec = c("2", "2"), names = c("", ""),
        period = c(-1, -1), var = c("allies_exp", "enemies_exp"), coef = c(0, 0), se = c(0, 0), beta_lower = c(0, 0), beta_upper = c(0, 0))
    coeft <- rbind(coeft_base, coeft)
    coeft <- coeft[var != "importer_dummy"]

    ggplot(coeft[period <= 10 & period >= -9], aes(x = period, y = coef, color = factor(var), fill = factor(var))) +
        geom_line(linewidth = 1.0) +
        geom_point(size = 4, shape = 1, stroke = 1) +
        geom_vline(xintercept = -1, linetype = "dashed", linewidth = 1.0) +
        geom_ribbon(aes(ymin = beta_lower, ymax = beta_upper), color = NA, alpha = 0.1) +
        scale_color_manual(label = c("allies", "enemies"), values = c(royalblue, royalred), name = "Relationship") +
        scale_fill_manual(label = c("allies", "enemies"), values = c(royalblue, royalred), name = "Relationship") +
        xlab("period") + ylab("Dual-use x Relationship") +
        custom_theme
    ggsave(file.path(STATS_PATH, "motivation", "events", paste0("dualuse_", substr(sitcwgtcol, 9, 9), substr(wgtcol, 5, 5), ".jpeg")), width = 10, height = 5.5, dpi = 300)           
}


plot_war_trade()


plot_war_vol <- function() {
    file_list <- list.files(file.path(OUTPUT_PATH, "gravity", "event_sitc"))
    file_list <- file_list[startsWith(file_list, "vol_")]
    ols_list <- list()

    f <- file_list[1]
    i <- 1
    for (i in seq_along(file_list)) {
        f <- file_list[i]
        print(f)
        df <- fread(file.path(OUTPUT_PATH, "gravity", "event_sitc", f), keepLeadingZeros = TRUE)
        df <- df[spec == 1]
        ols_list[[i]] <- df
    }
    df <- rbindlist(ols_list)
    df[, period := year - year_start]
    df[, dualuse_cont := dualuse]
    df[, dualuse := dualuse_dummy]
    df[, sitcwgt_no := 1 / .N, by = c("war_num", "importer_code")]
    df[, sitcwgt_no := sitcwgt_no / sum(sitcwgt_no), by = c("war_num")]
    df[, sitcwgt_yes := value / sum(value), by = c("war_num", "importer_code")]
    df[, sitcwgt_yes := sitcwgt_yes / sum(sitcwgt_yes), by = c("war_num")]
    df[, war_importer := paste0(war_num, "_", importer_code)]

    # throw out Portugal for Portugal-Mozambique and GBR for Falklands, as well as Iraq/Afghanistan for duplicates
    df <- df[!(war_num == 202 & importer_code == "GBR")]
    df <- df[!(war_num == 471 & importer_code == "PRT")]
    df <- df[!(war_num == 482)]
    df <- df[!(war_num == 481)]
    df <- df[!(war_num == 477 & importer_code == "IRQ")]
    df <- df[!(war_num == 193 & importer_code == "VDR")]
    df <- df[!(war_num == 216)]

    # merge in conflict
    df_conflict <- fread(file.path(OUTPUT_PATH, "cow", "conflict_panel.csv"))
    df_conflict <- df_conflict[, c("year", "importer_code", "conflict")]
    df <- merge_df(df, df_conflict, by = c("year", "importer_code"), how = "left")
    df <- df[!(conflict == 1 & period < 0)]

    # pooled
    wgtcol <- "wgt_no"
    df_ols <- df[wgt == wgtcol]
    sitcwgtcol <- "sitcwgt_no"
    coef_filter_apply <- coef_filter[wgt_col == wgtcol & sitcwgt_col == sitcwgtcol]
    df_ols <- merge_df(df_ols, coef_filter_apply, by = c("war_num", "importer_code"), how = "inner")
    reg <- feols(as.formula("coef ~ 0 + factor(period)"), data = df_ols, weights = df_ols[[sitcwgtcol]])
    coef <- reg$coefficients
    se <- sqrt(diag(vcov(reg, type = "hetero")))
    coeft <- data.table(
        wgt_col = wgtcol,
        sitcwgt_col = sitcwgtcol,
        spec = "1",
        names = names(coef),
        coef = coef,
        se = se
    )
    coeft[, beta_upper := coef + 1.96 * se]
    coeft[, beta_lower := coef - 1.96 * se]
                                    
    coeft[, period := as.integer(str_replace(names, "factor\\(period\\)", ""))]
    coeft_base <- data.table(wgt_col = wgtcol, sitcwgt_col = sitcwgtcol, spec = c("1"), names = c(""),
        period = c(-1), coef = c(0), se = c(0), beta_lower = c(0), beta_upper = c(0))
    coeft <- rbind(coeft_base, coeft)

    ggplot(coeft[period <= 10 & period >= -9], aes(x = period, y = coef)) +
        geom_line(linewidth = 1.0) +
        geom_point(size = 4, shape = 1, stroke = 1) +
        geom_vline(xintercept = -1, linetype = "dashed", linewidth = 1.0) +
        geom_ribbon(aes(ymin = beta_lower, ymax = beta_upper), color = NA, fill = royalblue, alpha = 0.1) +
        custom_theme
    ggsave(file.path(STATS_PATH, "event_studies", paste0("vol_raw_", substr(sitcwgtcol, 9, 9), substr(wgtcol, 5, 5), ".jpeg")), width = 10, height = 5.5)

    reg <- feols(as.formula("coef ~ factor(period):dualuse | period^war_importer"), data = df_ols, weights = df_ols[[sitcwgtcol]])
    summary(reg)
    coef <- reg$coefficients
    se <- sqrt(diag(vcov(reg, type = "hetero")))                                
    coeft <- data.table(
        wgt_col = wgtcol,
        sitcwgt_col = sitcwgtcol,
        spec = "2",
        names = names(coef),
        coef = coef,
        se = se
    )
    coeft[, beta_upper := coef + 1.96 * se]
    coeft[, beta_lower := coef - 1.96 * se]
                                
    coeft[, c("period", "dualuse") := tstrsplit(names, ":")]
    coeft <- coeft[!is.na(dualuse)]
    coeft[, dualuse := NULL]
    coeft[, period := as.integer(str_replace(period, "factor\\(period\\)", ""))]
    coeft_base <- data.table(wgt_col = wgtcol, sitcwgt_col = sitcwgtcol, spec = c("2"),
        period = c(-1), coef = c(0), se = c(0), beta_lower = c(0), beta_upper = c(0), names = c(""))
    coeft <- rbind(coeft_base, coeft)

    ggplot(coeft[period <= 10 & period >= -9], aes(x = period, y = coef)) +
        geom_line(linewidth = 1.0, color = royalblue) +
        geom_point(size = 4, shape = 1, stroke = 1, color = royalblue) +
        geom_vline(xintercept = -1, linetype = "dashed", linewidth = 1.0) +
        geom_ribbon(aes(ymin = beta_lower, ymax = beta_upper), color = NA, fill = royalblue, alpha = 0.1) +
        xlab("period") + ylab("Dual-use x Relationship") +
        custom_theme
    ggsave(file.path(STATS_PATH, "motivation", "events", paste0("vol_dff_", substr(sitcwgtcol, 9, 9), substr(wgtcol, 5, 5), ".jpeg")), width = 10, height = 5.5)           
}


# plot_war_vol()


list_wars <- function() {
    file_list <- list.files(file.path(OUTPUT_PATH, "gravity", "event_sitc"))
    file_list <- file_list[startsWith(file_list, "vol_")]
    ols_list <- list()

    f <- file_list[1]
    i <- 1
    for (i in seq_along(file_list)) {
        f <- file_list[i]
        print(f)
        df <- fread(file.path(OUTPUT_PATH, "gravity", "event_sitc", f), keepLeadingZeros = TRUE)
        df <- df[spec == 3]
        ols_list[[i]] <- df
    }
    df <- rbindlist(ols_list)
    
    # throw out Portugal for Portugal-Mozambique and GBR for Falklands, as well as Iraq/Afghanistan for duplicates
    df <- df[!(war_num == 202 & importer_code == "GBR")]
    df <- df[!(war_num == 471 & importer_code == "PRT")]
    df <- df[!(war_num == 482)]
    df <- df[!(war_num == 481)]
    df <- df[!(war_num == 477 & importer_code == "IRQ")]
    df <- df[!(war_num == 193 & importer_code == "VDR")]
    df_cty <- unique(df[, c("war_num", "war_name", "year_start", "year_end", "importer_code")])

    df_war <- fread(file.path(OUTPUT_PATH, "cow", "war_list.csv"))
    df_extra <- fread(file.path(DATA_PATH, "cow_double", "extra_manual.csv"))
    df_inter <- fread(file.path(DATA_PATH, "cow_double", "inter_manual.csv"))
    df_alliances <- rbind(df_extra, df_inter)
    df_alliances[allies == 0 & enemies == 0, allies := -99]
    df_alliances <- df_alliances[exporter_name != "Soviet Union"]
    df_alliances[, ally_exporter := ""]
    df_alliances[, enemy_exporter := ""]
    df_alliances[allies == 0, ally_exporter := exporter_name]
    df_alliances[allies == 1, enemy_exporter := exporter_name]

    df_alliances[, enemy_names := paste(unique(ally_exporter[ally_exporter != ""]), collapse = ", "), by = c("war_num", "importer_code")]
    df_alliances[, ally_names := paste(unique(enemy_exporter[enemy_exporter != ""]), collapse = ", "), by = c("war_num", "importer_code")]
    df_alliances <- df_alliances[, .SD[1], by = c("war_num", "importer_name")]
    df_cty <- merge_df(df_cty, df_alliances, by = c("war_num", "importer_code"), how = "left")
    df_cty <- df_cty[, c("war_name.x", "year_start", "year_end", "importer_name", "ally_names", "enemy_names")]
    df_cty[, war_name.x := paste0(war_name.x, " (", year_start, "--", year_end, ")")]
    df_cty <- df_cty[, c("war_name.x", "importer_name", "ally_names", "enemy_names")]
    df_cty[, obs := seq_len(.N), by = c("war_name.x")]
    df_cty[obs > 1, war_name.x := ""]
    df_cty[, obs := NULL]
    colnames(df_cty) <- c("War name", "Participant", "Allies", "Enemies")

    latex_code <- xtable(df_cty, include.rownames = FALSE)
	latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
	latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
    latex_code <- gsub("\\\\end\\{tabular\\}", "\\\\caption{List of conflicts, participants, and alliances} \\\\label{table:cow_alliances} \n \\\\end\\{tabular\\}", latex_code)
    
    latex_code <- gsub("tabular", "longtable", latex_code)
	latex_code <- sub("\\{llll\\}", "{p{0.14\\\\textwidth}p{0.1\\\\textwidth}p{0.35\\\\textwidth}p{0.35\\\\textwidth}}", latex_code)
	write(latex_code, file.path(STATS_PATH, "event_studies", "cow_list.tex"))
}


# list_wars()

