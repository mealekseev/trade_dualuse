### Output gravity

rm(list=ls())
gc()

library(data.table)
library(stringr)
library(haven)
library(fixest)
library(zoo)
library(cowplot)
library(xtable)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


plot_twolines_combined <- function() {
    file_list <- list.files(file.path(OUTPUT_PATH, "motivation", "gravity", "ols_hs0"))
    ols_list <- list()

    f <- file_list[1]
    for (i in seq_along(file_list)) {
        f <- file_list[i]
        print(f)
        df <- fread(file.path(OUTPUT_PATH, "motivation", "gravity", "ols_hs0", f), keepLeadingZeros = TRUE)
        ols_list[[i]] <- df
    }
    df <- rbindlist(ols_list)
    df[, hswgt_no := 1]
    df[, hswgt_yes := value]

    s <- 2
    wgtcol <- "wgt_no"
    b <- "modern_bloc"
    df_ols <- df[bloc == b & spec == s & wgt == wgtcol]
    hswgtcol <- "hswgt_no"
    print(paste0(b, " ", s, " ", wgtcol, " ", hswgtcol))

    df_ols <- df[bloc == b & spec == s & wgt == wgtcol]
    reg <- feols(as.formula("coef ~ 0 + factor(variable):factor(year):factor(dualuse)"), data = df_ols, weights = df_ols[[hswgtcol]])
    summary(reg)

    coef <- reg$coefficients
    var_naive <- as.matrix(vcov(reg, type = "hetero"))    
    Z <- model.matrix(~ 0 + factor(variable):factor(year):factor(dualuse), data = df_ols)
    W <- as.vector(df_ols[[hswgtcol]])
    ZtZ <- solve(sweep(t(Z), 2, W, "*") %*% Z)
    B <- as.vector(df_ols$se * df_ols$se)
    murphy_topel <- sweep(ZtZ %*% t(Z), 2, W * B * W, "*") %*% Z %*% ZtZ
    se <- sqrt(diag(var_naive + murphy_topel))
    coeft <- data.table(
        names = names(coef),
        coef = coef,
        se = se
    )
    coeft[, beta_upper := coef + 1.96 * se]
    coeft[, beta_lower := coef - 1.96 * se]
                    
    coeft[, c("variable", "year", "dualuse") := tstrsplit(names, ":")]
    coeft[, variable := str_replace(variable, "factor\\(variable\\)", "")]
    coeft[, year := as.integer(str_replace(year, "factor\\(year\\)", ""))]
    coeft[, dualuse := str_replace(dualuse, "factor\\(dualuse\\)", "")]
    coeft[variable == "between_bloc", variable := "between blocs"]
    coeft[variable == "same_bloc", variable := "same bloc"]
    coeft[variable == "nonaligned", variable := "non-aligned"]
    coeft_base <- CJ(year = c(1995), variable = unique(coeft$variable), dualuse = c("0", "1"), coef = c(0), se = c(0), beta_lower = c(0), beta_upper = c(0), names = c(""))
    coeft <- rbind(coeft_base, coeft)

    p1 <- ggplot(coeft, aes(x = year, y = coef, color = variable, fill = variable, linetype = dualuse)) +
        geom_line(linewidth = 1.0) +
        geom_ribbon(data = coeft[dualuse == "1"], aes(ymin = beta_lower, ymax = beta_upper), color = NA, alpha = 0.2) +
        scale_color_manual(values = c(graphite, royalblue)) +
        scale_fill_manual(values = c(graphite, royalblue)) +
        labs(color = "Relationship", fill = "Relationship", linetype = "Dual-use") +
        xlab("") + ylab("Dual-use x Relationship") +
        custom_theme
    legend <- get_legend(p1)
    p1 <- p1 + theme(
        text = element_text(size = 16, family = "Erewhon"),
        legend.text = element_text(size = 16) 
    )
    legend_slides <- get_legend(p1)
    p1 <- p1 + theme(legend.position = "none")

    ### SITC
    file_list <- list.files(file.path(OUTPUT_PATH, "motivation", "gravity", "ols_sitc2"))
    ols_list <- list()

    f <- file_list[1]
    for (i in seq_along(file_list)) {
        f <- file_list[i]
        print(f)
        df <- fread(file.path(OUTPUT_PATH, "motivation", "gravity", "ols_sitc2", f), keepLeadingZeros = TRUE)
        ols_list[[i]] <- df
    }
    df <- rbindlist(ols_list)
    df[, value := value / 1e9]
    df[, sitcwgt_no := 1]
    df[, sitcwgt_yes := value]
    df[, dualuse_cont := dualuse]
    df[, dualuse := dualuse_dummy]

    s <- 2
    wgtcol <- "wgt_no"
    b <- "cold_war_bloc"
    df_ols <- df[bloc == b & spec == s & wgt == wgtcol]
    sitcwgtcol <- "sitcwgt_no"
    print(paste0(b, " ", s, " ", wgtcol, " ", sitcwgtcol))

    reg <- feols(as.formula("coef ~ 0 + factor(variable):factor(year):factor(dualuse)"), data = df_ols, weights = df_ols[[sitcwgtcol]])
    summary(reg)

    coef <- reg$coefficients
    var_naive <- as.matrix(vcov(reg, type = "hetero"))    
    Z <- model.matrix(~ 0 + factor(variable):factor(year):factor(dualuse), data = df_ols)
    W <- as.vector(df_ols[[sitcwgtcol]])
    ZtZ <- solve(sweep(t(Z), 2, W, "*") %*% Z)
    B <- as.vector(df_ols$se * df_ols$se)
    murphy_topel <- sweep(ZtZ %*% t(Z), 2, W * B * W, "*") %*% Z %*% ZtZ
    se <- sqrt(diag(var_naive + murphy_topel))
    coeft <- data.table(
        names = names(coef),
        coef = coef,
        se = se
    )
    coeft[, beta_upper := coef + 1.96 * se]
    coeft[, beta_lower := coef - 1.96 * se]
                    
    coeft <- data.table(
        names = names(coef),
        coef = coef,
        se = se
    )
    coeft[, beta_upper := coef + 1.96 * se]
    coeft[, beta_lower := coef - 1.96 * se]
                    
    coeft[, c("variable", "year", "dualuse") := tstrsplit(names, ":")]
    coeft[, variable := str_replace(variable, "factor\\(variable\\)", "")]
    coeft[, year := as.integer(str_replace(year, "factor\\(year\\)", ""))]
    coeft[, dualuse := str_replace(dualuse, "factor\\(dualuse\\)", "")]
    coeft[variable == "between_bloc", variable := "between blocs"]
    coeft[variable == "same_bloc", variable := "same bloc"]
    coeft[variable == "nonaligned", variable := "non-aligned"]

    coeft_base <- CJ(year = c(1962), variable = unique(coeft$variable), dualuse = c("0", "1"), coef = c(0), se = c(0), beta_lower = c(0), beta_upper = c(0), names = c(""))
    coeft <- rbind(coeft_base, coeft)

    p2 <- ggplot(coeft, aes(x = year, y = coef, color = variable, fill = variable, linetype = dualuse)) +
        geom_line(linewidth = 1.0) +
        geom_ribbon(data = coeft[dualuse == "1"], aes(ymin = beta_lower, ymax = beta_upper), color = NA, alpha = 0.2) +
        scale_color_manual(values = c(graphite, royalblue)) +
        scale_fill_manual(values = c(graphite, royalblue)) +
        labs(color = "Alliances", fill = "Alliances") +
        xlab("") + ylab("Dual-use x Relationship") +
        custom_theme + theme(legend.position = "none")
    p_together <- plot_grid(p2, p1, ncol = 2)
    p3 <- plot_grid(p_together, legend, ncol = 1, rel_heights = c(1, 0.05))
    p3
    ggsave(file.path(STATS_PATH, "motivation", "gravity", "blocs_lines.jpeg"), p3, width = 10, height = 5.5)
}


plot_twolines_combined()


output_country_lists <- function() {
    df_cty <- fread(file.path(DATA_PATH,  "trade_atlas_double", "dataverse_files", "location_manual.csv"))
    df_cty <- df_cty[, c("location_name_short_en", "cold_war_bloc")]
    cold_war_eastern <- unique(df_cty[cold_war_bloc == "Eastern", location_name_short_en])
    cold_war_western <- unique(df_cty[cold_war_bloc == "Western", location_name_short_en])

    df_cty <- fread(file.path(DATA_PATH,  "cepii", "BACI_HS92_V202401b", "country_codes_V202401b_manual.csv"))
    modern_eastern <- unique(df_cty[modern_bloc == "Eastern", country_name])
    modern_western <- unique(df_cty[modern_bloc == "Western", country_name])

    max_len <- max(c(length(cold_war_eastern), length(cold_war_western), length(modern_eastern), length(modern_western)))
    cold_war_eastern <- c(cold_war_eastern, rep("", (max_len - length(cold_war_eastern))))
    cold_war_western <- c(cold_war_western, rep("", (max_len - length(cold_war_western))))
    modern_eastern <- c(modern_eastern, rep("", (max_len - length(modern_eastern))))
    modern_western <- c(modern_western, rep("", (max_len - length(modern_western))))
    
    df <- data.table(
        "Cold War, Western" = cold_war_western,
        "Cold War, Eastern" = cold_war_eastern,
        "Modern, Western" = modern_western,
        "Modern, Eastern" = modern_eastern
    )
    latex_code <- xtable(df, include.rownames = FALSE)
	latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
	latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
	latex_code <- sub("\\{llll\\}", "{rl|rl}", latex_code)
	write(latex_code, file.path(STATS_PATH, "gravity", "country_list.tex"))
}


# output_country_lists()


