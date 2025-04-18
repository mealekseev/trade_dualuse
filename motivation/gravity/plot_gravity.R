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


plot_combined <- function() {
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

    reg <- feols(
        as.formula("coef ~ 0 + factor(variable):factor(year) + factor(variable):factor(year):dualuse"),
        data = df_ols, weights = df_ols[[hswgtcol]]
    )
    summary(reg)
    
    coef <- reg$coefficients
    var_naive <- as.matrix(vcov(reg, type = "hetero"))    
    Z <- model.matrix(~ 0 + factor(variable):factor(year) + factor(variable):factor(year):dualuse, data = df_ols)
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

    p1 <- ggplot(coeft, aes(x = year, y = coef, color = variable, fill = variable)) +
        geom_line(linewidth = 1.0) +
        geom_point(size = 2.0, shape = 1, stroke = 1) +
        geom_ribbon(aes(ymin = beta_lower, ymax = beta_upper), alpha = 0.2, color = NA) +
        scale_color_manual(values = c(graphite, royalblue)) +
        scale_fill_manual(values = c(graphite, royalblue)) +
        geom_vline(xintercept = 2021, linetype = "dashed", color = "black", linewidth = 0.5) +
        labs(title = "Modern blocs", color = "Relationship", fill = "Relationship") + xlab("") + ylab("") +
        custom_theme + theme(
            plot.title = element_text(hjust = 0.5, size = 16),
            plot.title.position = "plot"
        )
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

    reg <- feols(as.formula("coef ~ 0 + factor(variable):factor(year) + factor(variable):factor(year):dualuse"), data = df_ols, weights = df_ols[[sitcwgtcol]])
    summary(reg)

    coef <- reg$coefficients
    var_naive <- as.matrix(vcov(reg, type = "hetero"))    
    Z <- model.matrix(~ 0 + factor(variable):factor(year) + factor(variable):factor(year):dualuse, data = df_ols)
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
    coeft <- coeft[endsWith(names, "dualuse")]
                    
    coeft[, c("variable", "year", "dualuse") := tstrsplit(names, ":")]
    coeft[, variable := str_replace(variable, "factor\\(variable\\)", "")]
    coeft[, year := as.integer(str_replace(year, "factor\\(year\\)", ""))]
    coeft[, dualuse := str_replace(dualuse, "factor\\(dualuse\\)", "")]
    coeft[variable == "between_bloc", variable := "between blocs"]
    coeft[variable == "same_bloc", variable := "same bloc"]
    coeft[variable == "nonaligned", variable := "non-aligned"]

    coeft_base <- CJ(year = c(1962), variable = unique(coeft$variable), dualuse = c(""), coef = c(0), se = c(0), beta_lower = c(0), beta_upper = c(0), names = c(""))
    coeft <- rbind(coeft_base, coeft)

    p2 <- ggplot(coeft, aes(x = year, y = coef, color = variable, fill = variable)) +
        geom_line(linewidth = 1.0) +
        geom_point(size = 1.0, shape = 1, stroke = 1) +
        geom_ribbon(aes(ymin = beta_lower, ymax = beta_upper), alpha = 0.2, color = NA) +
        scale_color_manual(values = c(graphite, royalblue)) +
        scale_fill_manual(values = c(graphite, royalblue)) +
        geom_vline(xintercept = 1992, linetype = "dashed", color = "black", linewidth = 0.5) +
        labs(title = "Cold war blocs", color = "Relationship", fill = "Relationship") +
        xlab("") + ylab("Dual-use x Relationship") +
        custom_theme + theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 16),
            plot.title.position = "plot",
        )
    p2

    p_together <- plot_grid(p2, p1, ncol = 2)
    p3 <- plot_grid(p_together, legend, ncol = 1, rel_heights = c(1, 0.05))
    p3
    ggsave(file.path(STATS_PATH, "motivation", "gravity", "blocs.jpeg"), p3, width = 10, height = 5.5, dpi = 300)

    p2_empty <- ggplot(coeft, aes(x = year, y = coef, color = variable, fill = variable)) +
        geom_line(linewidth = 1.0, alpha = 0.0) +
        geom_point(size = 1.0, shape = 1, stroke = 1, alpha = 0.0) +
        geom_ribbon(aes(ymin = beta_lower, ymax = beta_upper), alpha = 0.0, color = NA) +
        scale_color_manual(values = c(graphite, royalblue)) +
        scale_fill_manual(values = c(graphite, royalblue)) +
        labs(title = "Cold war blocs", color = "Relationship", fill = "Relationship") +
        xlab("") + ylab("Dual-use x Relationship") +
        custom_theme + theme(
            plot.title = element_text(hjust = 0.5, size = 18),
            plot.title.position = "plot"
        )
    p2_empty <- p2_empty + theme(legend.position = "none")
    p2_empty <- plot_grid(p2_empty, legend_slides, ncol = 1, rel_heights = c(1, 0.05))
    p2_empty
    ggsave(file.path(STATS_PATH, "motivation", "gravity", "blocs_anim0.jpeg"), p2_empty, width = 5.5, height = 5.5, dpi = 300)

    p1 <- p1 + theme(legend.position = "none")
    p2 <- p2 + theme(legend.position = "none")
    p2_empty1 <- plot_grid(p2, legend_slides, ncol = 1, rel_heights = c(1, 0.05))
    p2_empty1
    ggsave(file.path(STATS_PATH, "motivation", "gravity", "blocs_anim1.jpeg"), p2_empty1, width = 5.5, height = 5.5, dpi = 300)
}


plot_combined()

