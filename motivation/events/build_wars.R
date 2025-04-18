### Crosswalk COW codes

rm(list=ls())
gc()

library(data.table)
library(stringr)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


build_wars <- function() {
    df_extra <- fread(file.path(DATA_PATH, "motivation", "events", "Extra-StateWarData_v4.0.csv"), keepLeadingZeros = TRUE)
    df_extra <- df_extra[StartYear1 >= 1962]
    df_extra[, EndYear1 := -EndYear1]
    df_extra <- df_extra[, lapply(.SD, min), .SDcols = c("StartYear1", "EndYear1"), by = c("WarNum", "WarName")]
    df_extra[, EndYear1 := -EndYear1]
    df_extra[EndYear1 == -7, EndYear1 := 2007]

    df_inter <- fread(file.path(DATA_PATH, "motivation", "events", "Inter-StateWarData_v4.0.csv"), keepLeadingZeros = TRUE)
    df_inter <- df_inter[StartYear1 >= 1962]
    df_inter[, EndYear1 := -EndYear1]
    df_inter <- df_inter[, lapply(.SD, min), .SDcols = c("StartYear1", "EndYear1"), by = c("WarNum", "WarName")]
    df_inter[, EndYear1 := -EndYear1]

    df_war <- rbind(df_extra, df_inter)
    colnames(df_war) <- c("war_num", "war_name", "year_start", "year_end")
    fwrite(df_war, file.path(OUTPUT_PATH, "motivation", "events", "war_list.csv"))
}


build_wars()

