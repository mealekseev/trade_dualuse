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
library(r2country)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


output_sanction_graphs <- function(coeft, var, label_text, size_var, name) {
    reg <- feols(as.formula(paste0("beta ~ ", var)), data = coeft, weights = coeft$entity_count, se = "hetero")
    print(summary(reg))
    coef <- reg$coefficients[[var]] * 100
    t_stat <- summary(reg)$coeftable[var, "t value"]
    se <- sqrt(diag(vcov(reg, type = "hetero")))[1] * 10
    stars <- ifelse(abs(t_stat) > 1.96, ifelse(abs(t_stat) > 2.58, ifelse(abs(t_stat) > 3.29, "***", "**"), "*"), "")
    coef_str <- paste0("$\\beta$ = ", format(round(coef, 2), nsmall = 2), "$^", stars, "$ (se = ", format(round(se, 2), nsmall = 2), ", t-stat = ", format(round(t_stat, 2), nsmall = 2), ")")   

    x_coord <- max(coeft[!is.na(beta), get(var)]) * 1/2 + min(coeft[!is.na(beta), get(var)]) * 1/2
    y_coord <- 0.21
    if (var == "log_N") {
        x_coord <- max(coeft[!is.na(beta), get(var)]) * 0.17 + min(coeft[!is.na(beta), get(var)]) * 0.83
        y_coord <- min(coeft[!is.na(beta), beta]) - 0.02
    } else if (var == "log_N_per_capita") {
        x_coord <- max(coeft[!is.na(beta), get(var)]) * 0.17 + min(coeft[!is.na(beta), get(var)]) * 0.83
        y_coord <- min(coeft[!is.na(beta), beta]) - 0.02
    } else if (var == "log_N_per_entity") {
        x_coord <- max(coeft[!is.na(beta), get(var)]) * 0.17 + min(coeft[!is.na(beta), get(var)]) * 0.83
        y_coord <- min(coeft[!is.na(beta), beta]) - 0.02
    }

    ggplot(data = coeft, aes(x = get(var), y = beta, color = location, size = get(size_var))) +
        geom_point(shape = 1, stroke = 1) +
        geom_text_repel(aes(label = names), family = "Erewhon", size = 5) +
        geom_smooth(method = "lm", linetype = "dashed", se = TRUE, size = 1, alpha = 0.05, color = "black", fill = "black", mapping = aes(weight = entity_count)) +
        scale_color_manual(values = c(
            "Afghanistan + Iraq" = forest,
            "Central Asia" = "lightpink",
            "Israel + Lebanon" = royalblue,
            "LatAm" = "palegreen4",
            "Nordic + Baltic" = "lightblue2",
            "Asia Minor" = "black",
            "China + Iran + Russia" = royalred,
            "South Asia" = "sienna3",
            "South China Sea" = "darkgoldenrod4",
            "South Sudan" = "gray55",
            "UAE + Oman" = "darkmagenta",
            "Poland + Bulgaria + Serbia" = "firebrick",
            "Western" = "lightgray"
        )) + xlab(label_text) + ylab(TeX("Avg percentile $C^M$")) +
        geom_text(x = x_coord, y = y_coord, label = TeX(coef_str), color = "black", size = 6, family = "Erewhon", fontface = "bold") +
        scale_x_continuous(limits = c(min(coeft[!is.na(N), get(var)]), max(coeft[!is.na(N), get(var)]))) +
        scale_y_continuous(labels = scales::percent, limits = c(min(coeft[!is.na(beta), beta]), max(coeft[!is.na(beta), beta]))) +
        custom_theme + theme(legend.position = "none")
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "bis", paste0(name, ".jpeg")), width = 10, height = 10, dpi = 300)

    ggplot(data = coeft, aes(x = get(var), y = beta, color = location, size = get(size_var))) +
        geom_point(shape = 1, stroke = 1) +
        geom_text_repel(aes(label = location_code), family = "Erewhon", size = 5) +
        scale_color_manual(values = c(
            "Afghanistan + Iraq" = forest,
            "Central Asia" = "lightpink",
            "Israel + Lebanon" = royalblue,
            "LatAm" = "palegreen4",
            "Nordic + Baltic" = "lightblue2",
            "Asia Minor" = "black",
            "China + Iran + Russia" = royalred,
            "South Asia" = "sienna3",
            "South China Sea" = "darkgoldenrod4",
            "South Sudan" = "gray55",
            "UAE + Oman" = "darkmagenta",
            "Poland + Bulgaria + Serbia" = "firebrick",
            "Western" = "lightgray"
        )) + xlab(label_text) + ylab(TeX("Avg percentile $C^M$")) +
        scale_x_continuous(limits = c(min(coeft[!is.na(N), get(var)]), max(coeft[!is.na(N), get(var)]))) +
        scale_y_continuous(labels = scales::percent, limits = c(min(coeft[!is.na(beta), beta]), max(coeft[!is.na(beta), beta]))) +
        custom_theme + theme(legend.position = "none")
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "bis", paste0(name, "_slides.jpeg")), width = 7, height = 7, dpi = 300)

    ggplot(data = coeft, aes(x = get(var), y = beta, color = location, size = get(size_var))) +
        geom_point(shape = 1, stroke = 1, alpha = 0.0) +
        geom_text_repel(aes(label = names), family = "Erewhon", size = 5, alpha = 0.0) +
        scale_color_manual(values = c(
            "Afghanistan + Iraq" = forest,
            "Central Asia" = "lightpink",
            "Israel + Lebanon" = royalblue,
            "LatAm" = "palegreen4",
            "Nordic + Baltic" = "lightblue2",
            "Asia Minor" = "black",
            "China + Iran + Russia" = royalred,
            "South Asia" = "sienna3",
            "South China Sea" = "darkgoldenrod4",
            "South Sudan" = "gray55",
            "UAE + Oman" = "darkmagenta",
            "Poland + Bulgaria + Serbia" = "firebrick",
            "Western" = "lightgray"
        )) + xlab(label_text) + ylab(TeX("Avg percentile $C^M$")) +
        scale_x_continuous(limits = c(min(coeft[!is.na(N), get(var)]), max(coeft[!is.na(N), get(var)]))) +
        scale_y_continuous(labels = scales::percent, limits = c(min(coeft[!is.na(beta), beta]), max(coeft[!is.na(beta), beta]))) +
        custom_theme + theme(legend.position = "none")
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "bis", paste0(name, "_empty.jpeg")), width = 7, height = 7, dpi = 300)

    coeft_copy <- copy(coeft)
    coeft_copy[beta < 0.75, beta := NA]
    ggplot(data = coeft_copy, aes(x = get(var), y = beta, color = location, size = get(size_var))) +
        geom_point(shape = 1, stroke = 1) +
        geom_text_repel(aes(label = location_code), family = "Erewhon", size = 5) +
        scale_color_manual(values = c(
            "Afghanistan + Iraq" = forest,
            "Central Asia" = "lightpink",
            "Israel + Lebanon" = royalblue,
            "LatAm" = "palegreen4",
            "Nordic + Baltic" = "lightblue2",
            "Asia Minor" = "black",
            "China + Iran + Russia" = royalred,
            "South Asia" = "sienna3",
            "South China Sea" = "darkgoldenrod4",
            "South Sudan" = "gray55",
            "UAE + Oman" = "darkmagenta",
            "Poland + Bulgaria + Serbia" = "firebrick",
            "Western" = "lightgray"
        )) + xlab(label_text) + ylab(TeX("Avg percentile $C^M$")) +
        scale_x_continuous(limits = c(min(coeft[!is.na(N), get(var)]), max(coeft[!is.na(N), get(var)]))) +
        scale_y_continuous(labels = scales::percent, limits = c(min(coeft[!is.na(beta), beta]), max(coeft[!is.na(beta), beta]))) +
        custom_theme + theme(legend.position = "none")
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "bis", paste0(name, "_anim1.jpeg")), width = 7, height = 7, dpi = 300)

    coeft_copy <- copy(coeft)
    coeft_copy[beta > 0.75 | beta < 0.5, beta := NA]
    ggplot(data = coeft_copy, aes(x = get(var), y = beta, color = location, size = get(size_var))) +
        geom_point(shape = 1, stroke = 1) +
        geom_text_repel(aes(label = location_code), family = "Erewhon", size = 5) +
        scale_color_manual(values = c(
            "Afghanistan + Iraq" = forest,
            "Central Asia" = "lightpink",
            "Israel + Lebanon" = royalblue,
            "LatAm" = "palegreen4",
            "Nordic + Baltic" = "lightblue2",
            "Asia Minor" = "black",
            "China + Iran + Russia" = royalred,
            "South Asia" = "sienna3",
            "South China Sea" = "darkgoldenrod4",
            "South Sudan" = "gray55",
            "UAE + Oman" = "darkmagenta",
            "Poland + Bulgaria + Serbia" = "firebrick",
            "Western" = "lightgray"
        )) + xlab(label_text) + ylab(TeX("Avg percentile $C^M$")) +
        scale_x_continuous(limits = c(min(coeft[!is.na(N), get(var)]), max(coeft[!is.na(N), get(var)]))) +
        scale_y_continuous(labels = scales::percent, limits = c(min(coeft[!is.na(beta), beta]), max(coeft[!is.na(beta), beta]))) +
        custom_theme + theme(legend.position = "none")
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "bis", paste0(name, "_anim2.jpeg")), width = 7, height = 7, dpi = 300)

    coeft_copy <- copy(coeft)
    coeft_copy[beta > 0.5, beta := NA]
    ggplot(data = coeft_copy, aes(x = get(var), y = beta, color = location, size = get(size_var))) +
        geom_point(shape = 1, stroke = 1) +
        geom_text_repel(aes(label = location_code), family = "Erewhon", size = 5) +
        scale_color_manual(values = c(
            "Afghanistan + Iraq" = forest,
            "Central Asia" = "lightpink",
            "Israel + Lebanon" = royalblue,
            "LatAm" = "palegreen4",
            "Nordic + Baltic" = "lightblue2",
            "Asia Minor" = "black",
            "China + Iran + Russia" = royalred,
            "South Asia" = "sienna3",
            "South China Sea" = "darkgoldenrod4",
            "South Sudan" = "gray55",
            "UAE + Oman" = "darkmagenta",
            "Poland + Bulgaria + Serbia" = "firebrick",
            "Western" = "lightgray"
        )) + xlab(label_text) + ylab(TeX("Avg percentile $C^M$")) +
        scale_x_continuous(limits = c(min(coeft[!is.na(N), get(var)]), max(coeft[!is.na(N), get(var)]))) +
        scale_y_continuous(labels = scales::percent, limits = c(min(coeft[!is.na(beta), beta]), max(coeft[!is.na(beta), beta]))) +
        custom_theme + theme(legend.position = "none")
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "bis", paste0(name, "_anim3.jpeg")), width = 7, height = 7, dpi = 300)    

    ggplot(data = coeft, aes(x = get(var), y = beta, size = get(size_var))) +
        geom_point(shape = 1, stroke = 1) +
        geom_text(
            x = max(coeft[!is.na(beta), get(var)]) * 1/2 + min(coeft[!is.na(beta), get(var)]) * 1/2,
            y = 0.22, label = TeX(coef_str), color = royalblue, size = 6, family = "Erewhon") +
        geom_smooth(method = "lm", se = TRUE, color = royalblue, fill = royalblue, size = 1, mapping = aes(weight = entity_count)) +
        xlab(label_text) + ylab(TeX("Avg percentile $C^M$")) +
        scale_x_continuous(limits = c(min(coeft[!is.na(N), get(var)]), max(coeft[!is.na(N), get(var)]))) +
        scale_y_continuous(labels = scales::percent, limits = c(min(coeft[!is.na(beta), beta]), max(coeft[!is.na(beta), beta]))) +
        custom_theme + theme(legend.position = "none")
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "bis", paste0(name, "_anim4.jpeg")), width = 7, height = 7, dpi = 300)
}


eval_sanctions <- function() {
    df <- fread(file.path(OUTPUT_PATH, 'measurement', 'evaluation', 'bis_res.csv'), keepLeadingZeros = TRUE)
    reg <- feols(rank_C_M ~ 0 + factor(entity_type), weights = df$wgt, data = df)
    coef <- reg$coefficients
    se <- sqrt(diag(vcov(reg, type = "hetero")))                                
    coeft <- data.table(
        names = names(coef),
        beta = coef,
        se = se
    )
    coeft[, beta_upper := beta + 1.96 * se]
    coeft[, beta_lower := beta - 1.96 * se]
    coeft[, names := str_replace(names, "factor\\(entity_type\\)", "")]
    coeft <- coeft[order(beta)]
    coeft[, id := as.character(1:.N)]

    df_sanctions <- fread(file.path(OUTPUT_PATH, 'measurement', 'evaluation', 'bis_list.csv'), keepLeadingZeros = TRUE)
    df_sanctions <- df_sanctions[, length(unique(id)), by = c("entity_type")]
    colnames(df_sanctions) <- c("entity_type", "N")
    label_list <- list(
        "4" = paste0("Military end-use list (N = ", df_sanctions[entity_type == "meu", N], ")"),
        "3" = paste0("Unverified list (N = ", df_sanctions[entity_type == "unverified", N], ")"),
        "2" = paste0("Entity list (N = ", df_sanctions[entity_type == "entity", N], ")"),
        "1" = paste0("Debarred persons list (N = " , df_sanctions[entity_type == "dpl", N], ")")
    )
    ggplot(data = coeft, aes(x = id, y = beta)) +
        geom_point(size = 4, shape = 1, stroke = 1, color = royalblue) +
        geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), linewidth = 1.0, width = 0.05, color = royalblue) +
        scale_x_discrete(labels = label_list) + scale_y_continuous(labels = scales::percent) +
        geom_hline(yintercept = 0.5, linewidth = 1.0, linetype = "dashed") +
        labs(y = TeX("Avg percentile $C^M$"), x = "") +
        coord_flip() + custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "bis", "us_lists.jpeg"), width = 10, height = 4, dpi = 300)
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "bis", "us_lists_slides.jpeg"), width = 10, height = 5.5, dpi = 300)

    ggplot(data = coeft, aes(x = id, y = beta)) +
        geom_point(size = 4, shape = 1, stroke = 1, color = royalblue, alpha = 0.0) +
        geom_errorbar(aes(ymin = beta_lower, ymax = beta_upper), linewidth = 1.0, width = 0.05, color = royalblue, alpha = 0.0) +
        scale_x_discrete(labels = label_list) + scale_y_continuous(labels = scales::percent) +
        geom_hline(yintercept = 0.5, linewidth = 1.0, linetype = "dashed") +
        labs(y = TeX("Avg percentile $C^M$"), x = "") +
        coord_flip() + custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "evaluation", "bis", "us_lists_empty.jpeg"), width = 10, height = 5.5, dpi = 300)

    cty_list <- fread(file.path(DATA_PATH, "measurement", "policy_lists", "location.csv"), keepLeadingZeros = TRUE)
    cty_list[location_name_short_en == "Iran", location_name_short_en := "Islamic Republic of Iran"]
    cty_list[location_name_short_en == "South Korea", location_name_short_en := "Republic of Korea"]
    cty_list[location_name_short_en == "Taiwan", location_name_short_en := "Taiwan, China"]
    cty_list[location_name_short_en == "Hong Kong", location_name_short_en := "Hong Kong SAR, China"]
    cty_list[location_name_short_en == "Myanmar", location_name_short_en := "Myanmar/Burma"]
    cty_list[location_name_short_en == "Laos", location_name_short_en := "Lao People's Democratic Republic"]
    cty_iso3 <- cty_list[, c("location_code", "location_name_short_en"), with = FALSE]
    cty_iso3 <- rbind(cty_iso3, data.table(location_code = "LIE", location_name_short_en = "Liechtenstein"))
    df <- merge_df(df, cty_list, by.x = "country", by.y = "location_name_short_en", how = "left")

    reg <- feols(rank_C_M ~ 0 + factor(country), weights = df$wgt, data = df)
    coef <- reg$coefficients
    se <- sqrt(diag(vcov(reg, type = "hetero")))                                
    coeft <- data.table(
        names = names(coef),
        beta = coef,
        se = se
    )
    coeft[, beta_upper := beta + 1.96 * se]
    coeft[, beta_lower := beta - 1.96 * se]
    coeft[, names := str_replace(names, "factor\\(country\\)", "")]
    df_sanctions <- fread(file.path(OUTPUT_PATH, 'measurement', 'evaluation', 'bis_list.csv'), keepLeadingZeros = TRUE)
    df_sanctions <- df_sanctions[, length(unique(id)), by = c("country")]
    colnames(df_sanctions) <- c("country", "N")
    
    data(country_population)
    data(country_calling_code)
    df_country <- merge_df(data.table(country_names), data.table(country_population), by = "ID", how = "inner", indicator = FALSE)
    df_country[name == "Taiwan", name := "Taiwan, China"]
    df_country[name == "Hong Kong", name := "Hong Kong SAR, China"]
    df_country[name == "Iran", name := "Islamic Republic of Iran"]
    df_country[name == "Russia", name := "Russian Federation"]
    df_country[name == "Syria", name := "Syrian Arab Republic"]
    df_country[name == "United States", name := "United States of America"]
    df_country[name == "South Korea", name := "Republic of Korea"]
    df_country[name == "Myanmar", name := "Myanmar/Burma"]
    df_country[name == "Laos", name := "Lao People's Democratic Republic"]
    df_sanctions <- merge_df(df_sanctions, df_country, by.x = "country", by.y = "name", how = "inner", indicator = TRUE) 
    
    cty_entity <- fread(file.path(OUTPUT_PATH, 'measurement', 'evaluation', 'country_count.csv'))
    colnames(cty_entity) <- c("country", "entity_count")
    df_sanctions <- merge_df(df_sanctions, cty_entity, by = "country", how = "left")

    coeft <- merge_df(coeft, df_sanctions, by.x = "names", by.y = "country", how = "left", indicator = FALSE)
    coeft[, inv_se := 1 / se]
    coeft <- merge_df(coeft, cty_list, by.x = "names", by.y = "location_name_short_en", how = "left")
    coeft[, log_pop := log(population2023)]
    coeft[, log_entity_count := log(entity_count)]
    coeft[, log_N := log(N)]
    coeft[, log_N_per_capita := log_N - log_pop]
    coeft[, log_N_per_entity := log_N - log_entity_count]
    coeft[, id := 4]

    output_sanction_graphs(coeft, "log_N", "log(targeted entities)", "id", "sanctions_country")
    output_sanction_graphs(coeft, "log_N_per_capita", "log(targeted entities/capita)", "population2023", "sanctions_country_capita")
    output_sanction_graphs(coeft, "log_N_per_entity", "log(targeted entities/total entities)", "entity_count", "sanctions_country_entity")
}


eval_sanctions()

