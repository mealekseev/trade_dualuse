### Build crosswalks for China

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(zoo)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


get_gbt2017_isic4 <- function() {
    gbt_isic <- data.table(read_xlsx(file.path(DATA_PATH, "crosswalks", "china_GB-T_ISIC", "GB-T 4754-2017.xlsx"), col_types = "text"))
    setnames(gbt_isic, old = names(gbt_isic), new = c("gbt17", "gbt17_name", "isic4", "isic4_name"))
    gbt_isic[,':='(gbt17 = na.locf(gbt17, fromLast = F))]
    gbt_isic[1:152, ':=' (gbt17 = paste0("0", gbt17))] # ad-hoc fix for missing leading zeros
    gbt_isic[nchar(isic4) < 4, isic4 := paste0("0", isic4)]
    gbt_isic[isic4 == 2430, isic4 := 2431] # manually  correct a coding mistake
    gbt_isic[,':=' (gbt17 = gsub(" ", "", gbt17), isic4 = gsub(" ", "", isic4))]
    gbt_isic[,':=' (gbt17 = gsub("　", "", gbt17), isic4 = gsub("　", "", isic4))]
    gbt_isic <- gbt_isic[nchar(gbt17) == 4 & !is.na(isic4)]
    gbt_isic[, ':=' (gbt17_2 = substr(gbt17, 1, 2),
                gbt17_3 = substr(gbt17, 1, 3),
                gbt17_4 = substr(gbt17, 1, 4))]
    fwrite(gbt_isic, file.path(OUTPUT_PATH, "crosswalks", "gbt2017_isic4.csv"))
}


get_naics12_isic4 <- function() {
    xwalk <- fread(file.path(DATA_PATH, "crosswalks", "census", "2012_NAICS_to_ISIC_4.csv"), colClasses = "character")
    setnames(xwalk, old = c("2012\nNAICS\nUS  ", "ISIC 4.0"), new = c("naics12","isic4"))
    xwalk[naics12 == 0, naics12 := 999990]
    xwalk = xwalk[, c("naics12", "isic4")]
    xwalk[, ':=' (w_isic = 1/.N), by = "isic4"]
    fwrite(xwalk, file.path(OUTPUT_PATH, "crosswalks", "naics12_isic4.csv"))
}


get_gbt2017_naics12 <- function() {
    gbt_isic <- fread(file.path(OUTPUT_PATH, "crosswalks", "gbt2017_isic4.csv"), colClasses = "character")
    gbt_isic[, gbt17_2 := as.numeric(gbt17_2)]
  
    naics12_isic <- fread(file.path(OUTPUT_PATH, "crosswalks", "naics12_isic4.csv"))  
    gbt_naics12 <- merge(gbt_isic, naics12_isic[, c("naics12", "isic4")], by = "isic4", all.x = T, allow.cartesian = T)
    gbt_naics12[gbt17_2 > 90, ':=' (gbt17_2 = 9999, naics12 = 92)] # ind 9999 represents the public sector
    gbt_naics12 <- unique(gbt_naics12[, c("gbt17_2", "naics12")])
    gbt_naics12[, ':=' (w = 1 / uniqueN(naics12)), by = "gbt17_2"]
    
    gbt_naics12[, gbt17_2 := as.character(gbt17_2)]
    gbt_naics12[nchar(gbt17_2) == 1, gbt17_2 := paste0("0", gbt17_2)]
    fwrite(gbt_naics12, file.path(OUTPUT_PATH, "crosswalks", "gbt2017_naics12.csv"))
}


get_chinaiosector18_gbt2017 <- function() {
    io_gbt <- data.table(read_xlsx(file.path(DATA_PATH, "crosswalks", "china_GB-T_ISIC", "2018_io_GB-T-2017.xlsx"), col_types = "text"))
    io_gbt <- melt(io_gbt, id.vars = c("iosector_code", "iosector_name"), value.name = "gbt17")
    io_gbt[, ':=' (variable = NULL)]
    io_gbt <- io_gbt[!is.na(gbt17)]
    io_gbt[, ':=' (iosector_code = gsub(" ", "", iosector_code),
                gbt17 = gsub(" ", "", gbt17))]
    io_gbt[, ':=' (iosector_code = gsub("　", "", iosector_code),
                gbt17 = gsub("　", "", gbt17))]
    setorder(io_gbt, iosector_code, gbt17)
    fwrite(io_gbt, file.path(OUTPUT_PATH, "crosswalks", "iosector_gbt2017.csv"))
}


get_chinaiosector18_isic4 <- function(){
    io_gbt <- fread(file.path(OUTPUT_PATH, "crosswalks", "iosector_gbt2017.csv"), keepLeadingZeros = TRUE)
    gbt_isic <- fread(file.path(OUTPUT_PATH, "crosswalks", "gbt2017_isic4.csv"), keepLeadingZeros = TRUE)
  
    merge_by_digit <- function(d) {
        dat <- io_gbt[nchar(gbt17) == d]
        cols <- c(paste0("gbt17_", d), "isic4")
        dat2 <- gbt_isic[, ..cols]
        dat <- merge(dat, dat2, by.x = "gbt17", by.y = paste0("gbt17_", d), all.x = T)
        dat[, ':=' (count_isic4 = .N), by = c("iosector_code", "isic4")]
        dat <- unique(dat[, c("iosector_code", "isic4", "count_isic4")])
        return(dat)
    }
  
    dat <- rbind(merge_by_digit(2), merge_by_digit(3), merge_by_digit(4), use.names = T)
    dat[, ':=' (w_iosector = count_isic4 / sum(count_isic4, na.rm=T)), by = "iosector_code"]
    setorder(dat, iosector_code, isic4)
    dat[, isic4 := as.character(isic4)]
    dat[nchar(isic4) == 3, isic4 := paste0("0", isic4)]
    fwrite(dat, file.path(OUTPUT_PATH, "crosswalks", "iosector_isic4.csv"))
}


get_chinaiosector18_naics12 <- function() {
    dat <- fread(file.path(OUTPUT_PATH, "crosswalks", "iosector_isic4.csv"), keepLeadingZeros = TRUE)
    naics12_isic4 <- fread(file.path(OUTPUT_PATH, "crosswalks", "naics12_isic4.csv"), keepLeadingZeros = TRUE)

    dat1 <- merge(dat, naics12_isic4, by = "isic4", all.x = T, allow.cartesian = T)
    dat1[is.na(naics12)]
    dat1[, ':=' (w = w_iosector * w_isic)]
    dat2 = dat1[, .(w = sum(w)), by = c("iosector_code", "naics12")]
    dat2[,':='(checksum = sum(w)), by = "iosector_code"]
    summary(dat2$checksum)
    dat2[, checksum := NULL]
    setorder(dat2, iosector_code, naics12)
    fwrite(dat2, file.path(OUTPUT_PATH, "crosswalks", "chinaiosector18_naics12.csv"))
}


get_crosswalks <- function() {
    get_gbt2017_isic4()
    get_naics12_isic4()
    get_gbt2017_naics12()
    get_chinaiosector18_gbt2017()
    get_chinaiosector18_isic4()
    get_chinaiosector18_naics12()
}


get_crosswalks()

