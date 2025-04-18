### Estimate Tullock elasticity off Cold-War groups

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(stringr)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


get_cow_expenditures <- function() {
    df <- fread(file.path(DATA_PATH, "calibration", "milex", "NMC_Documentation-6.0", "NMC-60-abridged", "NMC-60-abridged.csv"), keepLeadingZeros = TRUE)
    cty <- fread(file.path(DATA_PATH, "calibration", "milex", "COW-country-codes_manual.csv"), keepLeadingZeros = TRUE)
    colnames(cty) <- tolower(colnames(cty))
    df <- merge_df(df, cty, by = "stateabb", how = "left", allow.cartesian = TRUE)
    df <- df[year > 1947]
    df[milex < 0, milex := 0]

    df <- df[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("milex"), by = c("year", "classif")]
    df <- df[order(year, classif)]
    
    cpi <- fread(file.path(DATA_PATH, "calibration", "milex", "cpi_u.csv"), na.strings = ".")
    colnames(cpi) <- c("year", "cpiu")
    cpi[, year := year(year)]
    cpi_base <- cpi[year == 2021, cpiu]
    cpi[, cpiu := cpiu / cpi_base]

    df <- merge_df(df, cpi, by = "year", how = "left", indicator = FALSE)
    df[, milex := milex / cpiu]
    df <- df[, .(year, classif, milex)]
    df <- df[order(year, classif)]
    ggplot(data = df, aes(x = year, y = milex, color = as.character(classif))) +
        geom_line(linewidth = 1.0) +
        scale_color_manual(values = c(royalblue, royalred, graphite)) +
        custom_theme
    return(df)
}


get_sipri_expenditures <- function() {
    mil <- data.table(read_excel(
        file.path(DATA_PATH, "motivation", "military", "SIPRI-Milex-data-1949-2022.xlsx"), sheet = "Constant (2021) US$", skip = 5
    ))
    cty <- data.table(read_excel(file.path(DATA_PATH, "calibration", "milex", "split.xlsx")))
    mil <- merge_df(mil, cty, by = "Country", how = "left", indicator = FALSE)
    setnames(mil, "Country", "country")
    setnames(mil, "Classification", "classification")
    mil <- mil[!is.na(classification)]
    mil[, ("...2") := NULL]
    mil[, ("Notes") := NULL]
    mil <- melt(mil, id.vars = c("country", "classification"))
    mil[value == "xxx", value := NA]
    mil[value == "...", value := NA]
    mil[, value := as.numeric(value)]
    mil[, variable := as.integer(as.character(variable))]
    setnames(mil, "variable", "year")
    mil_sum <- mil[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("value"), by = c("year", "classification")]
    ggplot(data = mil_sum, aes(x = year, y = value, color = as.character(classification))) +
        geom_line(linewidth = 1.0) +
        scale_color_manual(values = c(royalblue, royalred, graphite)) +
        custom_theme
    colnames(mil_sum) <- c("year", "classif", "milex")
    mil_sum <- mil_sum[order(year, classif)]
    return(mil_sum)
}


build_milex <- function() {
    df_cow <- get_cow_expenditures()
    df_cow[, source := "COW"]
    df_cow[, milex := milex / 1000]
    df_sipri <- get_sipri_expenditures()
    df_sipri[, source := "SIPRI"]
    df_plot <- rbind(df_cow, df_sipri)
    df_plot <- df_plot[order(year, classif)]
    ggplot(data = df_plot, aes(x = year, y = milex, color = as.character(classif), linetype = source)) +
        geom_line(linewidth = 1.0) +
        scale_color_manual(values = c(royalblue, royalred, lightblue)) +
        custom_theme

    ratio_1 <- df_cow[year == 2015 & classif == "1", milex] / df_sipri[year == 2015 & classif == "1", milex]
    ratio_2 <- df_cow[year == 2015 & classif == "2", milex] / df_sipri[year == 2015 & classif == "2", milex]
    ratio_3 <- df_cow[year == 2015 & classif == "3", milex] / df_sipri[year == 2015 & classif == "3", milex]
    
    df_cow <- df_cow[year <= 2015]
    df_sipri <- df_sipri[year >= 2016]
    
    df_sipri[year >= 2016 & classif == "1", milex := milex * ratio_1]
    df_sipri[year >= 2016 & classif == "2", milex := milex * ratio_2]
    df_sipri[year >= 2016 & classif == "3", milex := milex * ratio_3]

    df_agg <- rbind(df_cow, df_sipri)
    fwrite(df_agg, file.path(OUTPUT_PATH, "calibration", "tullock", "milex.csv"))
}


get_sipri_allies_2018 <- function() {
    mil <- data.table(read_excel(
        file.path(DATA_PATH, "motivation", "military", "SIPRI-Milex-data-1949-2022.xlsx"),
        sheet = "Current US$", skip = 5
    ))
    cty <- data.table(read_excel(file.path(DATA_PATH, "calibration", "milex", "split.xlsx")))
    mil <- merge_df(mil, cty, by = "Country", how = "left", indicator = FALSE)
    setnames(mil, "Country", "country")
    setnames(mil, "Classification", "classification")
    setnames(mil, "Modern", "modern")
    mil <- mil[!is.na(modern)]
    mil[, ("...2") := NULL]
    mil[, ("Notes") := NULL]
    mil <- melt(mil, id.vars = c("country", "modern"))
    mil[value == "xxx", value := NA]
    mil[value == "...", value := NA]
    mil[, value := as.numeric(value)]
    mil[, variable := as.integer(as.character(variable))]
    setnames(mil, "variable", "year")
    mil <- mil[country != "United States of America"]
    mil <- mil[country != "China"]
    mil <- mil[year == 2018]
    mil_sum <- mil[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("value"), by = c("year", "modern")]
    colnames(mil_sum) <- c("year", "classif", "milex")
    mil_sum <- mil_sum[order(year, classif)]
    mil_sum[, year := NULL]
    df <- data.table(
        CHN = mil_sum[classif == "2", milex],
        USA = mil_sum[classif == "1", milex],
        ROW = 0
    )
    fwrite(df, file.path(OUTPUT_PATH, "calibration", "tullock", "allies_2018.csv"))
}


build_milex()
get_sipri_allies_2018()

