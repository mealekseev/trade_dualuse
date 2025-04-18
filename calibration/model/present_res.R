### Presents model results

rm(list=ls())
gc()

library(data.table)
library(ggrepel)
library(fixest)
library(xtable)
library(stringr)
library(latex2exp)
library(cowplot)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


get_file <- function(pattern) {
    file_list <- list.files(path = file.path(STATS_PATH, "calibration", "model", "scenarios"), pattern = paste0("^", pattern), full.names = TRUE)
    if (length(file_list) == 0) {
        stop("No files found in the directory.")
    }
    df <- fread(file_list[1], keepLeadingZeros = TRUE)
    return(df)
}


plot_welfare_results <- function() {
    file_list <- c(
        "usa_export",
        "usa_stock_export",
        "usa_reship_export",
        "usa_ally_export",
        "usa_import",
        "chn_export",
        "chn_stock_export",
        "chn_reship_export",
        "chn_ally_export",
        "chn_import"
    )
    f <- file_list[1]
    i <- 0
    ix <- 1
    res_list <- list()
    for (f in file_list) {
        for (i in c(0, 10)) {
            print(paste0(f, i))
            df <- get_file(paste0(f, i))
            if (f %in% c("usa_reship_export", "usa_ally_export", "chn_reship_export", "chn_ally_export")) {
                if (f == "usa_reship_export") {
                    df_base <- get_file(paste0("usa_reship_base_export", i))
                } else if (f == "usa_ally_export") {
                    df_base <- get_file(paste0("usa_ally_base_export", i))
                } else if (f == "chn_reship_export") {
                    df_base <- get_file(paste0("chn_export", i))
                } else if (f == "chn_ally_export") {
                    df_base <- get_file(paste0("chn_ally_base_export", i))
                }
                c_ratio <- -(log(df$c_USA[1]) - log(df$c_init_USA[1]) - (log(df$c_CHN[1]) - log(df$c_init_CHN[1])))
                m_ratio <- (log(df$m_USA[1]) - log(df$m_init_USA[1]) - (log(df$m_CHN[1]) - log(df$m_init_CHN[1])))
                c_ratio_base <- -(log(df_base$c_USA[1]) - log(df_base$c_init_USA[1]) - (log(df_base$c_CHN[1]) - log(df_base$c_init_CHN[1])))
                m_ratio_base <- log(df_base$m_USA[1]) - log(df_base$m_init_USA[1]) - (log(df_base$m_CHN[1]) - log(df_base$m_init_CHN[1]))
                delta_c <- c_ratio - c_ratio_base
                delta_m <- m_ratio - m_ratio_base
                res_temp <- rbindlist(res_list)
                if (f %in% c("usa_reship_export", "usa_ally_export")) {
                    delta_c <- delta_c + res_temp[name == "usa_export" & weight == i / 10, c_ratio][1]
                    delta_m <- delta_m + res_temp[name == "usa_export" & weight == i / 10, m_ratio][1]
                } else if (f %in% c("chn_reship_export", "chn_ally_export")) {
                    delta_c <- delta_c + res_temp[name == "chn_export" & weight == i / 10, c_ratio][1]
                    delta_m <- delta_m + res_temp[name == "chn_export" & weight == i / 10, m_ratio][1]
                }
                
                res <- data.table(
                    name = c(f),
                    weight = c(i / 10),
                    c_ratio = delta_c,
                    m_ratio = delta_m
                )
            } else {
                res <- data.table(
                    name = c(f),
                    weight = c(i / 10),
                    c_ratio = c(-(log(df$c_USA[1]) - log(df$c_init_USA[1]) - (log(df$c_CHN[1]) - log(df$c_init_CHN[1])))),
                    m_ratio = c((log(df$m_USA[1]) - log(df$m_init_USA[1]) - (log(df$m_CHN[1]) - log(df$m_init_CHN[1]))))
                )
            }
            res_list[[ix]] <- copy(res)
            ix <- ix + 1
        }
    }
    res <- rbindlist(res_list)
    res[, id := 1:.N]
    res[, name := paste0(min(id), name), by = c("name")]
    res[grepl("chn", name), c_ratio := -c_ratio]
    res[grepl("chn", name), m_ratio := -m_ratio]

    point_limit <- paste0(
        "(", signif(res[name == "7usa_ally_export" & weight == 0, c_ratio] * 100, 2), ", ",
        signif(res[name == "7usa_ally_export" & weight == 0, m_ratio] * 100, 2), ")"
    )
    ggplot(data = res[grepl("chn", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_text(aes(x = -0.03, y = 0.033, label = paste0("endpoint:\n", point_limit)), size = 5, family = "Erewhon", color = graphite) +
        geom_line(linewidth = 1.0) 
    p1 <- ggplot(data = res[grepl("usa", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_text(aes(x = -0.03, y = 0.033, label = paste0("endpoint:\n", point_limit)), size = 5, family = "Erewhon", color = graphite) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = TeX("$m_{HOME}/m_{FRGN}$, change"), title = "United States") +
        scale_shape_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = 16, "3usa_stock_export" = 15, "5usa_reship_export" = 17, "7usa_ally_export" = 18, "9usa_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = royalblue, "3usa_stock_export" = royalred, "5usa_reship_export" = gold, "7usa_ally_export" = graphite, "9usa_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = "solid", "3usa_stock_export" = "dashed",  "5usa_reship_export" = "dotdash", "7usa_ally_export" = "dotted", "9usa_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme
    legend <- get_legend(p1)
    p1 <- p1 + theme(
        text = element_text(size = 16, family = "Erewhon"),
        legend.text = element_text(size = 16) 
    )
    legend_slides <- get_legend(p1)
    p1 <- p1 + theme(legend.position = "none")
    p1

    p2 <- ggplot(data = res[grepl("chn", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = "", title = "China") +
        scale_shape_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = 16, "13chn_stock_export" = 15, "15chn_reship_export" = 17, "17chn_ally_export" = 18, "19chn_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = royalblue, "13chn_stock_export" = royalred, "15chn_reship_export" = gold, "17chn_ally_export" = graphite, "19chn_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = "solid", "13chn_stock_export" = "dashed",  "15chn_reship_export" = "dotdash", "17chn_ally_export" = "dotted", "19chn_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme + theme(legend.position = "none")
    p2

    p_together <- plot_grid(p1, p2, ncol = 2)
    p3 <- plot_grid(p_together, legend, ncol = 1, rel_heights = c(1, 0.05))
    p3
    ggsave(file.path(STATS_PATH, "calibration", "model", "welfare_plot.jpeg"), p3, width = 10, height = 5.5)

    point_limit <- paste0(
        "(", signif(res[name == "7usa_ally_export" & weight == 0, c_ratio] * 100, 2), ", ",
        signif(res[name == "7usa_ally_export" & weight == 0, m_ratio] * 100, 2), ")"
    )
    p1 <- ggplot(data = res[grepl("usa", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_text(aes(x = -0.037, y = 0.033, label = paste0("endpoint:\n", point_limit)), size = 5, family = "Erewhon", color = graphite) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = TeX("$m_{HOME}/m_{FRGN}$, change"), title = "United States") +
        scale_shape_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = 16, "3usa_stock_export" = 15, "5usa_reship_export" = 17, "7usa_ally_export" = 18, "9usa_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = royalblue, "3usa_stock_export" = royalred, "5usa_reship_export" = gold, "7usa_ally_export" = graphite, "9usa_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = "solid", "3usa_stock_export" = "dashed", "5usa_reship_export" = "dotdash", "7usa_ally_export" = "dotted", "9usa_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme
    legend <- get_legend(p1)
    p1 <- p1 + theme(
        text = element_text(size = 16, family = "Erewhon"),
        legend.text = element_text(size = 16) 
    )
    legend_slides <- get_legend(p1)
    p1 <- p1 + theme(legend.position = "none")
    p1

    p2 <- ggplot(data = res[grepl("chn", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = "", title = "China") +
        scale_shape_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = 16, "13chn_stock_export" = 15, "15chn_reship_export" = 17, "17chn_ally_export" = 18, "19chn_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = royalblue, "13chn_stock_export" = royalred, "15chn_reship_export" = gold, "17chn_ally_export" = graphite, "19chn_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = "solid", "13chn_stock_export" = "dashed",  "15chn_reship_export" = "dotdash", "17chn_ally_export" = "dotted", "19chn_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme + theme(legend.position = "none")
    p2

    p_together <- plot_grid(p1, p2, ncol = 2)
    p3 <- plot_grid(p_together, legend, ncol = 1, rel_heights = c(1, 0.05))
    p3
    ggsave(file.path(STATS_PATH, "calibration", "model", "welfare_plot_slides.jpeg"), p3, width = 10, height = 5.5)   

    p1 <- ggplot(data = res[grepl("usa_", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4, alpha = 0.0) +
        geom_line(linewidth = 1.0, alpha = 0.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = TeX("$m_{HOME}/m_{FRGN}$, change"), title = "United States") +
        scale_shape_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = 16, "3usa_stock_export" = 15, "5usa_reship_export" = 17, "7usa_ally_export" = 18, "9usa_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = royalblue, "3usa_stock_export" = royalred, "5usa_reship_export" = gold, "7usa_ally_export" = graphite, "9usa_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = "solid", "3usa_stock_export" = "dashed",  "5usa_reship_export" = "dotdash", "7usa_ally_export" = "dotted", "9usa_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme + theme(legend.position = "none")
    p2 <- ggplot(data = res[grepl("chn_", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4, alpha = 0.0) +
        geom_line(linewidth = 1.0, alpha = 0.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = "", title = "China") +
        scale_shape_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = 16, "13chn_stock_export" = 15, "15chn_reship_export" = 17, "17chn_ally_export" = 18, "19chn_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = royalblue, "13chn_stock_export" = royalred, "15chn_reship_export" = gold, "17chn_ally_export" = graphite, "19chn_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = "solid", "13chn_stock_export" = "dashed",  "15chn_reship_export" = "dotdash", "17chn_ally_export" = "dotted", "19chn_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme + theme(legend.position = "none")
    p_together <- plot_grid(p1, p2, ncol = 2)
    p3 <- plot_grid(p_together, legend, ncol = 1, rel_heights = c(1, 0.05))
    p3
    ggsave(file.path(STATS_PATH, "calibration", "model", "welfare_plot_slides0.jpeg"), p3, width = 10, height = 5.5)  

    p1 <- ggplot(data = res[grepl("usa_export", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = TeX("$m_{HOME}/m_{FRGN}$, change"), title = "United States") +
        scale_shape_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = 16, "3usa_stock_export" = 15, "5usa_reship_export" = 17, "7usa_ally_export" = 18, "9usa_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = royalblue, "3usa_stock_export" = royalred, "5usa_reship_export" = gold, "7usa_ally_export" = graphite, "9usa_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = "solid", "3usa_stock_export" = "dashed",  "5usa_reship_export" = "dotdash", "7usa_ally_export" = "dotted", "9usa_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) + 
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme + theme(legend.position = "none")
    p2 <- ggplot(data = res[grepl("chn_export", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = "", title = "China") +
        scale_shape_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = 16, "13chn_stock_export" = 15, "15chn_reship_export" = 17, "17chn_ally_export" = 18, "19chn_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = royalblue, "13chn_stock_export" = royalred, "15chn_reship_export" = gold, "17chn_ally_export" = graphite, "19chn_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = "solid", "13chn_stock_export" = "dashed",  "15chn_reship_export" = "dotdash", "17chn_ally_export" = "dotted", "19chn_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme + theme(legend.position = "none")
    p_together <- plot_grid(p1, p2, ncol = 2)
    p3 <- plot_grid(p_together, legend, ncol = 1, rel_heights = c(1, 0.05))
    p3
    ggsave(file.path(STATS_PATH, "calibration", "model", "welfare_plot_slides1.jpeg"), p3, width = 10, height = 5.5)

    p1 <- ggplot(data = res[grepl("usa_export", name) | grepl("usa_import", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = TeX("$m_{HOME}/m_{FRGN}$, change"), title = "United States") +
        scale_shape_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = 16, "3usa_stock_export" = 15, "5usa_reship_export" = 17, "7usa_ally_export" = 18, "9usa_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = royalblue, "3usa_stock_export" = royalred, "5usa_reship_export" = gold, "7usa_ally_export" = graphite, "9usa_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = "solid", "3usa_stock_export" = "dashed",  "5usa_reship_export" = "dotdash", "7usa_ally_export" = "dotted", "9usa_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme + theme(legend.position = "none")
    p2 <- ggplot(data = res[grepl("chn_export", name) | grepl("chn_import", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = "", title = "China") +
        scale_shape_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = 16, "13chn_stock_export" = 15, "15chn_reship_export" = 17, "17chn_ally_export" = 18, "19chn_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = royalblue, "13chn_stock_export" = royalred, "15chn_reship_export" = gold, "17chn_ally_export" = graphite, "19chn_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = "solid", "13chn_stock_export" = "dashed",  "15chn_reship_export" = "dotdash", "17chn_ally_export" = "dotted", "19chn_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme + theme(legend.position = "none")
    p_together <- plot_grid(p1, p2, ncol = 2)
    p3 <- plot_grid(p_together, legend, ncol = 1, rel_heights = c(1, 0.05))
    p3
    ggsave(file.path(STATS_PATH, "calibration", "model", "welfare_plot_slides2.jpeg"), p3, width = 10, height = 5.5) 

    p1 <- ggplot(data = res[grepl("usa_export", name) | grepl("usa_reship", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = TeX("$m_{HOME}/m_{FRGN}$, change"), title = "United States") +
        scale_shape_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = 16, "3usa_stock_export" = 15, "5usa_reship_export" = 17, "7usa_ally_export" = 18, "9usa_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = royalblue, "3usa_stock_export" = royalred, "5usa_reship_export" = gold, "7usa_ally_export" = graphite, "9usa_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = "solid", "3usa_stock_export" = "dashed",  "5usa_reship_export" = "dotdash", "7usa_ally_export" = "dotted", "9usa_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme + theme(legend.position = "none")
    p2 <- ggplot(data = res[grepl("chn_export", name) | grepl("chn_reship", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = "", title = "China") +
        scale_shape_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = 16, "13chn_stock_export" = 15, "15chn_reship_export" = 17, "17chn_ally_export" = 18, "19chn_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = royalblue, "13chn_stock_export" = royalred, "15chn_reship_export" = gold, "17chn_ally_export" = graphite, "19chn_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = "solid", "13chn_stock_export" = "dashed",  "15chn_reship_export" = "dotdash", "17chn_ally_export" = "dotted", "19chn_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme + theme(legend.position = "none")
    p_together <- plot_grid(p1, p2, ncol = 2)
    p3 <- plot_grid(p_together, legend, ncol = 1, rel_heights = c(1, 0.05))
    p3
    ggsave(file.path(STATS_PATH, "calibration", "model", "welfare_plot_slides3.jpeg"), p3, width = 10, height = 5.5) 

    p1 <- ggplot(data = res[grepl("usa_export", name) | grepl("usa_stock", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = TeX("$m_{HOME}/m_{FRGN}$, change"), title = "United States") +
        scale_shape_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = 16, "3usa_stock_export" = 15, "5usa_reship_export" = 17, "7usa_ally_export" = 18, "9usa_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = royalblue, "3usa_stock_export" = royalred, "5usa_reship_export" = gold, "7usa_ally_export" = graphite, "9usa_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = "solid", "3usa_stock_export" = "dashed",  "5usa_reship_export" = "dotdash", "7usa_ally_export" = "dotted", "9usa_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme + theme(legend.position = "none")
    p2 <- ggplot(data = res[grepl("chn_export", name) | grepl("chn_stock", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = "", title = "China") +
        scale_shape_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = 16, "13chn_stock_export" = 15, "15chn_reship_export" = 17, "17chn_ally_export" = 18, "19chn_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = royalblue, "13chn_stock_export" = royalred, "15chn_reship_export" = gold, "17chn_ally_export" = graphite, "19chn_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = "solid", "13chn_stock_export" = "dashed",  "15chn_reship_export" = "dotdash", "17chn_ally_export" = "dotted", "19chn_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.055, 0.002), ylim = c(-0.005, 0.035)) +
        custom_theme + theme(legend.position = "none")
    p_together <- plot_grid(p1, p2, ncol = 2)
    p3 <- plot_grid(p_together, legend, ncol = 1, rel_heights = c(1, 0.05))
    p3
    ggsave(file.path(STATS_PATH, "calibration", "model", "welfare_plot_slides4.jpeg"), p3, width = 10, height = 5.5) 

    p1 <- ggplot(data = res[grepl("usa_export", name) | grepl("usa_ally", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = TeX("$m_{HOME}/m_{FRGN}$, change"), title = "United States") +
        scale_shape_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = 16, "3usa_stock_export" = 15, "5usa_reship_export" = 17, "7usa_ally_export" = 18, "9usa_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = royalblue, "3usa_stock_export" = royalred, "5usa_reship_export" = gold, "7usa_ally_export" = graphite, "9usa_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("1usa_export" = "Baseline", "3usa_stock_export" = "No stockpiles", "5usa_reship_export" = "Smuggling", "7usa_ally_export" = "Coalition", "9usa_import" = "Import tax"),
            values = c("1usa_export" = "solid", "3usa_stock_export" = "dashed",  "5usa_reship_export" = "dotdash", "7usa_ally_export" = "dotted", "9usa_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.1, 0.002), ylim = c(-0.005, 0.08)) +
        custom_theme + theme(legend.position = "none")
    p2 <- ggplot(data = res[grepl("chn_export", name) | grepl("chn_ally", name)], aes(x = c_ratio, y = m_ratio, color = name, linetype = name, shape = name)) +
        geom_point(size = 4) +
        geom_line(linewidth = 1.0) +
        labs(x = TeX("$c_{FRGN}/c_{HOME}$, change"), y = "", title = "China") +
        scale_shape_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = 16, "13chn_stock_export" = 15, "15chn_reship_export" = 17, "17chn_ally_export" = 18, "19chn_import" = 1)
        ) +
        scale_color_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = royalblue, "13chn_stock_export" = royalred, "15chn_reship_export" = gold, "17chn_ally_export" = graphite, "19chn_import" = "black")
        ) +
        scale_linetype_manual(
            name = "Export tax",
            label = c("11chn_export" = "Baseline", "13chn_stock_export" = "No stockpiles", "15chn_reship_export" = "Smuggling", "17chn_ally_export" = "Coalition", "19chn_import" = "Import tax"),
            values = c("11chn_export" = "solid", "13chn_stock_export" = "dashed",  "15chn_reship_export" = "dotdash", "17chn_ally_export" = "dotted", "19chn_import" = "longdash")
        ) + 
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(-0.1, 0.002), ylim = c(-0.005, 0.08)) +
        custom_theme + theme(legend.position = "none")
    p_together <- plot_grid(p1, p2, ncol = 2)
    p3 <- plot_grid(p_together, legend, ncol = 1, rel_heights = c(1, 0.05))
    p3
    ggsave(file.path(STATS_PATH, "calibration", "model", "welfare_plot_slides5.jpeg"), p3, width = 10, height = 5.5) 
}


plot_welfare_results()


output_welfare_diff <- function(file_list, file_name) {
    name_list <- c("actmil_asis", "nomil_asis")
    df_list <- list()
    j <- 1
    for (j in seq_along(file_list)) {
        f <- file_list[j]
        n <- name_list[j]
        print(paste0(j, " ", f))
        df <- get_file(f)
        cty_list <- c("CHN", "USA", "ROW")
        for (ix_imp in cty_list) {
            df[, (paste0("tau_avg_init_", ix_imp)) := 1]
            df[, (paste0("c_ratio_", ix_imp)) := c_ratio]
            df[, (paste0("c_ratio_init_", ix_imp)) := c_ratio]
            df[, (paste0("m_ratio_", ix_imp)) := m_ratio]
            df[, (paste0("m_ratio_init_", ix_imp)) := c_ratio]
        }
        vars <- c(
            "U_int", "U_cons", "U_mil",
            "c", "C", "P_C",
            "nu_self", "m", "M", "P_M",
            "wL",
            "R",
            "MP_CHN", "MP_USA", "MP_ROW"
        )
        latex_names <- c(
            "$U^I$", "$U^C$", "$U^M$",
            "$c$", "$C$", "$P^C$",
            "$\\nu$", "$m$", "$M$", "$P^M$",
            "$wL$", "$R / wL$",
            "$mp_{\\mbox{\\scriptsize CHN}, \\cdot}$", "$mp_{\\mbox{\\scriptsize USA}, \\cdot}$", "$mp_{\\mbox{\\scriptsize ROW}, \\cdot}$"
        )
        res <- data.table(name = latex_names)
        res[, (paste0("CHN_", n, "_init")) := 0]
        res[, (paste0("CHN_", n)) := 0]
        res[, (paste0("USA_", n, "_init")) := 0]
        res[, (paste0("USA_", n)) := 0]
        res[, (paste0("ROW_", n, "_init")) := 0]
        res[, (paste0("ROW_", n)) := 0]
        for (i in seq_along(vars)) {
            varname <- vars[i]
            if (varname != "beta") {
                varname_init <- paste0(varname, "_init")
            } else {
                varname_init <- varname
            }
            if (endsWith(varname, "CHN") | endsWith(varname, "USA") | endsWith(varname, "ROW")) {
                varname_init <- paste0(substr(varname, 1, nchar(varname) - 4), "_init", substr(varname, nchar(varname) - 3, nchar(varname)))
            }
            for (cty in c("CHN", "USA", "ROW")) {
                res[name == latex_names[i], (paste0(cty, "_", n)) := df[1, get(paste0(varname, "_", cty))]]
                res[name == latex_names[i], (paste0(cty, "_", n, "_init")) := df[1, get(paste0(varname_init, "_", cty))]]
                if (vars[i] == "beta") {
                    res[name == latex_names[i], (paste0(cty, "_", n)) := NA]
                    res[name == latex_names[i], (paste0(cty, "_", n, "_init")) := df[1, get(paste0(varname_init, "_", cty))] / df[1, get(paste0("wL_init_", cty))]]
                } else if (vars[i] == "R") {
                    res[name == latex_names[i], (paste0(cty, "_", n)) := df[1, get(paste0(varname, "_", cty))] / df[1, get(paste0("wL_", cty))] * 100]
                } else if (vars[i] == "nu_self") {
                    res[name == latex_names[i], (paste0(cty, "_", n)) := (df[1, get(paste0(varname, "_", cty))] - df[1, get(paste0("nu_self_init_", cty))]) * 100]
                } else {
                    res[name == latex_names[i],
                        (paste0(cty, "_", n)) := (df[1, get(paste0(varname, "_", cty))] / df[1, get(paste0(varname_init, "_", cty))] - 1) * 100]
                }
                if ((vars[i] %in% c("U_mil", "m", "M", "P_M", "U_M_CHN", "U_M_USA", "beta", "nu_self", "R")) & (cty == "ROW")) {
                    res[name == latex_names[i], (paste0(cty, "_", n)) := NA]
                    res[name == latex_names[i], (paste0(cty, "_", n, "_init")) := NA]
                }
                if ((vars[i] %in% c("U_mil", "U_M_CHN", "U_M_USA", "beta", "nu_self")) & (startsWith(n, "1nomil"))) {
                    res[name == latex_names[i], (paste0(cty, "_", n)) := NA]
                    res[name == latex_names[i], (paste0(cty, "_", n, "_init")) := NA]
                }
            }
        }
        col_list <- copy(colnames(res))
        for (col in col_list) {
            print(col)
            if ((endsWith(col, "_init")) & (n != "actmil_asis")) {
                res[, (col) := NULL]
            }
        }
        if (j > 1) {
            res[, name := NULL]
        }
        df_list[[j]] <- copy(res)
    }

    df <- cbind(df_list[[1]], df_list[[2]])
    df[, delta_CHN_mult := CHN_actmil_asis - CHN_nomil_asis]
    df[, delta_USA_mult := USA_actmil_asis - USA_nomil_asis]
    df[, delta_ROW_mult := ROW_actmil_asis - ROW_nomil_asis]

    df[, c("init_actmil_asis", "init_nomil_asis", "init_empty") := ""]
    df <- df[, c(
        "name", "init_actmil_asis",
        "CHN_actmil_asis", "USA_actmil_asis", "ROW_actmil_asis", "init_nomil_asis",
        "CHN_nomil_asis", "USA_nomil_asis", "ROW_nomil_asis", "init_empty",
        "CHN_actmil_asis_init", "USA_actmil_asis_init", "ROW_actmil_asis_init"
    ), with = FALSE]

    latex_code <- xtable(df, include.rownames = FALSE, digits = 3)
    latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, digits = 3, sanitize.text.function = identity)
    latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
    latex_string <- paste0(
        " & & \\\\multicolumn{3}{c}{USA (\\\\% change)} & & \\\\multicolumn{3}{c}{CHN (\\\\% change)} & & \\\\multicolumn{3}{c}{baseline} \\\\\\\\ \n",
        "\\\\cline{3-5} \\\\cline{7-9} \\\\cline{11-13} \n",
        " & & CHN & USA & ROW & & CHN & USA & ROW & & CHN & USA & ROW \\\\"
    )
    latex_code <- gsub("llrrrlrrrlrrr", "l|lccclccclccc", latex_code)
    latex_code <- gsub("name.*?ROW_actmil_asis_init \\\\", latex_string, latex_code)
    latex_code <- gsub("\\\\hline", "", latex_code)
    latex_code <- gsub("cc\\}", "cc}\n \\\\toprule ", latex_code)
    latex_code <- gsub("\n\\\\end\\{", "\\\\bottomrule \n \\\\end{", latex_code)
    latex_code <- gsub("\\$U^I\\$", "\\\\hline $U^I$", latex_code)
    latex_code <- gsub("\\$c\\$", "\\\\hline $c$", latex_code)
    latex_code <- gsub("\\$(\\\\nu)\\$", "\\\\hline $\\\\nu$", latex_code)
    latex_code <- gsub("\\$wL\\$", "\\\\hline $wL$", latex_code)
    print(latex_code)
    write(latex_code, file.path(STATS_PATH, "calibration", "model", "scenarios", paste0(file_name, ".tex")))
}


# output_welfare_diff(c("usa_export0", "chn_export0"), "baseline_export")
# output_welfare_diff(c("usa_subsidy10", "chn_subsidy10"), "baseline_subsidy")

