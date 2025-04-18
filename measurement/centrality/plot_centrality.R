### Plot centrality

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


plot_industries <- function() {
    df <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_naics12.csv"))
    df <- df[X != 0]
    df <- df[tradable == 1]
    df <- df[!(naics12_two_digit %in% c("92", "99"))]
    df[, sector_name := str_replace_all(sector_name, "Manufacturing", "Mfg")]
    df[, sector_name := str_replace_all(sector_name, "Farming", "Frm")]
    df[, cent_C_norm := cent_C / sum(cent_C)]
    df[, cent_M_norm := cent_M / sum(cent_M)]

    # curate labels
    df[, cent_M_rank := rank(-cent_M_norm)]
    df[, cent_C_rank := rank(-cent_C_norm)]
    df[, plot_desc := ""]
    df[cent_M_rank <= 15, plot_desc := sector_name]
    df[cent_C_rank <= 15, plot_desc := sector_name]
    df[, sales_distance := cent_M_norm / (cent_M_norm + cent_C_norm)]
    
    ggplot(data = df[sector == 0], aes(x = cent_C_norm, y = cent_M_norm)) +
      geom_point(size = 4, shape = 1, stroke = 1, color = royalred) +
      scale_x_continuous(limits = c(0.0, 0.08), labels = scales::percent) +
      scale_y_continuous(limits = c(0.0, 0.08), labels = scales::percent) +
      geom_text_repel(aes(label = plot_desc), color = "black", box.padding = 3, size = 4.5, family = "Erewhon") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = graphite) +
      xlab("% in household basket (network-adjusted)") +
      ylab("% in military basket (network-adjusted)") +
      guides(color = FALSE) +
      custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "centrality", "cent_naics_usa_anim0.jpeg"), width = 6, height = 6, dpi = 300)


    # animation 1
    ggplot(data = df[sector == 336992], aes(x = cent_C_norm, y = cent_M_norm)) +
      geom_point(size = 4, shape = 1, stroke = 1, color = royalred) +
      scale_x_continuous(limits = c(0.0, 0.08), labels = scales::percent) +
      scale_y_continuous(limits = c(0.0, 0.08), labels = scales::percent) +
      geom_text_repel(aes(label = plot_desc), color = "black", box.padding = 3, size = 4.5, family = "Erewhon") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = graphite) +
      xlab("% in household basket (network-adjusted)") +
      ylab("% in military basket (network-adjusted)") +
      guides(color = FALSE) +
      custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "centrality", "cent_naics_usa_anim1.jpeg"), width = 6, height = 6, dpi = 300)

    # animation 2
    ggplot(data = df[sector %in% c(336992, 336111)],
      aes(x = cent_C_norm, y = cent_M_norm, color = sales_distance)) +
      geom_point(size = 4, shape = 1, stroke = 1) +
      scale_color_gradient2(name = "HHI change", low = royalblue,
        mid = rgb(94 / 255, 94 / 255, 94 / 255, 0.5, names = NULL, maxColorValue = 1),
        high = royalred, 
                midpoint = 0.5, n.breaks = 5) +
      scale_x_continuous(limits = c(0.0, 0.08), labels = scales::percent) +
      scale_y_continuous(limits = c(0.0, 0.08), labels = scales::percent) +
      geom_text_repel(aes(label = plot_desc), color = "black", box.padding = 3, size = 4.5, family = "Erewhon") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = graphite) +
      xlab("% in household basket (network-adjusted)") +
      ylab("% in military basket (network-adjusted)") +
      guides(color = FALSE) +
      custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "centrality", "cent_naics_usa_anim2.jpeg"), width = 6, height = 6, dpi = 300)

    ggplot(data = df[sector %in% c(336992, 336111, 334413)],
      aes(x = cent_C_norm, y = cent_M_norm, color = sales_distance)) +
      geom_point(size = 4, shape = 1, stroke = 1) +
      scale_color_gradient2(name = "HHI change", low = royalblue,
        mid = rgb(94 / 255, 94 / 255, 94 / 255, 0.5, names = NULL, maxColorValue = 1),
        high = royalred, 
                midpoint = 0.5, n.breaks = 5) +
      scale_x_continuous(limits = c(0.0, 0.08), labels = scales::percent) +
      scale_y_continuous(limits = c(0.0, 0.08), labels = scales::percent) +
      geom_text_repel(aes(label = plot_desc), color = "black", box.padding = 3, size = 4.5, family = "Erewhon") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = graphite) +
      xlab("% in household basket (network-adjusted)") +
      ylab("% in military basket (network-adjusted)") +
      guides(color = FALSE) +
      custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "centrality", "cent_naics_usa_anim3.jpeg"), width = 6, height = 6, dpi = 300)

    # plot
    df[, plot_desc := ""]
    df[cent_M_rank <= 9, plot_desc := sector_name]
    df[cent_C_rank <= 20, plot_desc := sector_name]
    df[, plot_desc2 := plot_desc]
    df[startsWith(plot_desc, "Military Armored Vehicle, Tank, "), plot_desc2 := ""]
    ggplot(data = df, aes(x = cent_C_norm, y = cent_M_norm, color = sales_distance)) +
      geom_point(size = 4, shape = 1, stroke = 1) +
      scale_color_gradient2(name = "HHI change", low = royalblue,
      mid = rgb(94 / 255, 94 / 255, 94 / 255, 0.2, names = NULL, maxColorValue = 1),
      high = royalred, 
                midpoint = 0.5, n.breaks = 5) +
	    scale_x_continuous(limits = c(0.0, 0.08), labels = scales::percent) +
      scale_y_continuous(limits = c(0.0, 0.08), labels = scales::percent) +
      geom_text_repel(aes(label = plot_desc2), max.overlaps = 25, color = "black", box.padding = 0.5, size = 3.7, family = "Erewhon") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = graphite) +
      xlab("% in household basket (network-adjusted)") +
      ylab("% in military basket (network-adjusted)") +
      guides(color = FALSE) +
      custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "centrality", "cent_naics_usa_slides.jpeg"), width = 6, height = 6, dpi = 300)

    ggplot(data = df, aes(x = cent_C_norm, y = cent_M_norm, color = sales_distance)) +
      geom_point(size = 4, shape = 1, stroke = 1) +
      scale_color_gradient2(name = "HHI change", low = royalblue,
      mid = rgb(94 / 255, 94 / 255, 94 / 255, 0.2, names = NULL, maxColorValue = 1),
      high = royalred, 
                midpoint = 0.5, n.breaks = 5) +
	    scale_x_continuous(limits = c(0.0, 0.08), labels = scales::percent) +
      scale_y_continuous(limits = c(0.0, 0.08), labels = scales::percent) +
      geom_text_repel(aes(label = plot_desc), max.overlaps = 20, color = "black", box.padding = 0.5, size = 4.5, family = "Erewhon") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = graphite) +
      xlab("% in household basket (network-adjusted)") +
      ylab("% in military basket (network-adjusted)") +
      guides(color = FALSE) +
      custom_theme
    ggsave(file.path(STATS_PATH, "measurement", "centrality", "cent_naics_usa.jpeg"), width = 10, height = 10, dpi = 300)
}


get_top_industries <- function() {
    df <- fread(file.path(OUTPUT_PATH, "measurement", "centrality", "centrality_hs12.csv"), keepLeadingZeros = TRUE)
	df <- df[hs12 != "999999"]
	
    df <- df[order(-C_M_sigma)]
	df[, delta_tax := (1 / (1 - (0 + 1 * C_M) / sigma) - 1 / (1 - 0 / sigma)) * 100]
	df[, dualuse_outcome := ifelse(dualuse_outcome == 1, "\\ding{51}", "\\ding{55}")]

	df <- df[, c("hs12", "hscode_name", "C_M_sigma", "delta_tax", "dualuse_outcome"), with = FALSE]
	df <- df[1:15]
	colnames(df) <- c(
		"HS code", "Description", "$\\mathcal{C}^M_{\\mbox{\\scriptsize US}, k} / \\sigma_k$", "$\\Delta \\tau (\\%)$", "D-U")
	latex_code <- xtable(df, include.rownames = FALSE)
	latex_code <- print(latex_code, type = "latex", include.rownames = FALSE, sanitize.text.function = identity)
	latex_code <- str_extract(latex_code, "(?s)\\\\begin\\{tabular\\}.*?\\\\end\\{tabular\\}")
	latex_code <- sub("\\{llrrl\\}", "{lp{0.75\\\\textwidth}ccc}", latex_code)
    latex_code <- gsub("\\\\hline", "", latex_code)
    latex_code <- gsub("cc\\}", "cc}\n \\\\toprule ", latex_code)
    latex_code <- gsub("\n\\\\end\\{", "\\\\bottomrule \n \\\\end{", latex_code)
    latex_code <- gsub("D\\-U \\\\", "D\\-U \\\\\\\\ \\\\hline \n", latex_code)
	write(latex_code, file.path(STATS_PATH, "measurement", "centrality", "top_industries.tex"))
}


plot_industries()
get_top_industries()

