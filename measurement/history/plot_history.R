### Run trade regressions

rm(list=ls())
gc()

library(data.table)
library(stringr)
library(haven)
library(fixest)
library(zoo)
library(fixest)
library(readxl)
library(latex2exp)
library(cowplot)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


plot_examples <- function() {
    df <- fread(file.path(OUTPUT_PATH, "measurement", "history", "history_masterfile.csv"), keepLeadingZeros = TRUE)
    df_ex1 <- df[sitc75 == "7132"]

    df_ex1_plot <- melt(df_ex1, id.vars = c("sitc75", "year"), measure.vars = c("s_M", "s_C", "s_M_ma", "s_C_ma", "share_value"))
    p1 <- ggplot(df_ex1_plot[variable %in% c("s_M_ma", "s_C_ma", "share_value")], aes(x = year, y = value, linetype = variable, color = variable)) +
        geom_line(size = 1.0) +
        scale_linetype_manual(name = "", label = c(TeX("$s^M$"), TeX("$s^C$"), "trade share"), values = c("solid", "dotted", "solid")) +
        scale_color_manual(name = "", label = c(TeX("$s^M$"), TeX("$s^C$"), "trade share"), values = c(royalblue, royalblue, royalred)) +
        scale_y_continuous(
            "% basket share",
            labels = scales::percent,
            sec.axis = sec_axis(
                trans = ~.,
                labels = scales::percent
            )
        ) +
        xlab("") + labs(title = "7132 - Internal combustion engines") +
        custom_theme + theme(
            plot.title = element_text(hjust = 0.5, size = 13),
            axis.title.y = element_text(size = 13),
            plot.title.position = "plot",
            axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13)
        )
    legend <- get_legend(p1)
    p1 <- p1 + theme(legend.position = "none")

    df_ex2 <- df[sitc75 == "8732"]
    df_ex2[, share_value := 25 * share_value]
    df_ex2_plot <- melt(df_ex2, id.vars = c("sitc75", "year"), measure.vars = c("s_M", "s_C", "s_M_ma", "s_C_ma", "share_value"))
    p2 <- ggplot(df_ex2_plot[variable %in% c("s_M_ma", "s_C_ma", "share_value")], aes(x = year, y = value, linetype = variable, color = variable)) +
        geom_line(size = 1.0) +
        scale_linetype_manual(name = "", label = c(TeX("$s^M$"), TeX("$s^C$"), "trade share"), values = c("solid", "dotted", "solid")) +
        scale_color_manual(name = "", label = c(TeX("$s^M$"), TeX("$s^C$"), "trade share"), values = c(royalblue, royalblue, royalred)) +
        scale_y_continuous(
            "",
            labels = scales::percent,
            sec.axis = sec_axis(
                trans = ~. / 25,
                name = "",
                labels = scales::percent
            )
        ) +
        xlab("") + labs(title = "8732 - Non-electrical prod. equipment") +
        custom_theme + theme(
            plot.title = element_text(hjust = 0.5, size = 13),
            plot.title.position = "plot",
            axis.title.y = element_text(size = 13),
            legend.position = "none",
            axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13)
        )

    df_ex3 <- df[sitc75 == "8741"]
    df_ex3[, share_value := 10 * share_value]
    df_ex3_plot <- melt(df_ex3, id.vars = c("sitc75", "year"), measure.vars = c("s_M", "s_C", "s_M_ma", "s_C_ma", "share_value"))
    p3 <- ggplot(df_ex3_plot[variable %in% c("s_M_ma", "s_C_ma", "share_value")], aes(x = year, y = value, linetype = variable, color = variable)) +
        geom_line(size = 1.0) +
        scale_linetype_manual(name = "", label = c(TeX("$s^M$"), TeX("$s^C$"), "trade share"), values = c("solid", "dotted", "solid")) +
        scale_color_manual(name = "", label = c(TeX("$s^M$"), TeX("$s^C$"), "trade share"), values = c(royalblue, royalblue, royalred)) +
        scale_y_continuous(
            "",
            labels = scales::percent,
            sec.axis = sec_axis(
                trans = ~. / 10,
                name = "% global trade",
                labels = scales::percent
            )
        ) +
        xlab("") + labs(title = "8741 - Non-electronic navigation inst.") +
        custom_theme + theme(
            plot.title = element_text(hjust = 0.5, size = 13),
            axis.title.y = element_text(size = 13),
            plot.title.position = "plot",
            legend.position = "none",
            axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13)
        )

    p_together <- plot_grid(p1, p2, p3, ncol = 3)
    p_final <- plot_grid(p_together, legend, ncol = 1, rel_heights = c(1, 0.05))
    p_final
    ggsave(file.path(STATS_PATH, "measurement", "history", "sitc_examples.jpeg"), p_final, width = 10, height = 5.5, dpi = 300)


    df_app <- df[sitc75 == "7139"]
    df_app_plot <- melt(df_app, id.vars = c("sitc75", "year"), measure.vars = c("s_M", "s_C", "s_M_ma", "s_C_ma", "share_value"))
    p1 <- ggplot(df_app_plot[variable %in% c("s_M_ma", "s_C_ma", "share_value")], aes(x = year, y = value, linetype = variable, color = variable)) +
        geom_line(size = 1.0) +
        scale_linetype_manual(name = "", label = c(TeX("$s^M$"), TeX("$s^C$"), "trade share"), values = c("solid", "dotted", "solid")) +
        scale_color_manual(name = "", label = c(TeX("$s^M$"), TeX("$s^C$"), "trade share"), values = c(royalblue, royalblue, royalred)) +
        scale_y_continuous(
            "% basket share",
            labels = scales::percent,
            sec.axis = sec_axis(
                trans = ~.,
                name = "",
                labels = scales::percent
            )
        ) +
        xlab("") + labs(title = "7139 - Parts of internal combustion piston engines") +
        custom_theme + theme(
            plot.title = element_text(hjust = 0.5, size = 14),
            plot.title.position = "plot",
            axis.title.y = element_text(size = 13),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13)
        )
    legend <- get_legend(p1)
    p1 <- p1 + theme(legend.position = "none")
    
    df_app <- df[sitc75 == "7783"]
    df_app_plot <- melt(df_app, id.vars = c("sitc75", "year"), measure.vars = c("s_M", "s_C", "s_M_ma", "s_C_ma", "share_value"))
    p2 <- ggplot(df_app_plot[variable %in% c("s_M_ma", "s_C_ma", "share_value")], aes(x = year, y = value, linetype = variable, color = variable)) +
        geom_line(size = 1.0) +
        scale_linetype_manual(name = "", label = c(TeX("$s^M$"), TeX("$s^C$"), "trade share"), values = c("solid", "dotted", "solid")) +
        scale_color_manual(name = "", label = c(TeX("$s^M$"), TeX("$s^C$"), "trade share"), values = c(royalblue, royalblue, royalred)) +
        scale_y_continuous(
            "",
            labels = scales::percent,
            sec.axis = sec_axis(
                trans = ~.,
                name = "% global trade",
                labels = scales::percent
            )
        ) +
        xlab("") + labs(title = "7783 - Electrical equipment for int. comb. eng.") +
        custom_theme + theme(
            plot.title = element_text(hjust = 0.5, size = 14),
            plot.title.position = "plot",
            axis.title.y = element_text(size = 13),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13)
        )

    p_together <- plot_grid(p1, p2, ncol = 2)
    p_final <- plot_grid(p_together, legend, ncol = 1, rel_heights = c(1, 0.05))
    p_final
    ggsave(file.path(STATS_PATH, "measurement", "history", "sitc_more.jpeg"), width = 10, height = 5.5, dpi = 300)
}


plot_examples()



