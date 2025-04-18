### Clean 2018 I-O tables for China

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(zoo)

source("settings.R")
source("utilities.R")


clean_sectors <- function(io, num_sec) {
    iosector_list <- io[1:num_sec,]$iosector
  
    va <- io[iosector == "TVA"]
    cols <- names(va)[!names(va) %in% iosector_list]
    va <- va[, !..cols]
    va <- transpose(va)
    setnames(va, old = "V1", new = "va")
    va <- va[, iosector := iosector_list]
  
    gout <- io[iosector %in% iosector_list, c("iosector_name", "iosector", "GO", "EX", "IM", "THC")]
    setnames(gout, old = c("GO", "EX", "IM", "THC"), new = c("gross_output", "export", "import", "total_hh_consumption"))

    mg <- merge(gout, va, by = "iosector", all = F)
    return(mg)
}


clean_iofromto <- function(io, num_sec) {
    iosector_list <- io[1:num_sec,]$iosector
    mat <- io[iosector %in% iosector_list, ..iosector_list]
    matname1 <- matrix(names(mat), nrow = nrow(mat), ncol = nrow(mat), byrow = F)
    matname2 <- matrix(names(mat), nrow = nrow(mat), ncol = nrow(mat), byrow = T)
    mat <- as.matrix(mat)
    for (i in 1:nrow(mat)) {
        matname1[, i] <- paste0(matname1[, i], '_to_', matname2[, i])
    }
    dat <- data.table(v1 = as.vector(matname1), v = as.vector(mat))
    dat[, ':=' (iofrom = gsub("^([0-9]+[a-z]*)_to_[0-9]+[a-z]*", "\\1", v1),
            ioto = gsub("^[0-9]+[a-z]*_to_([0-9]+[a-z]*)", "\\1", v1))]
    dat[, ':=' (v1 = NULL, v = as.numeric(v))]
    setcolorder(dat, c("iofrom", "ioto", "v"))
    return(dat)
}


save_io_chn <- function() {
    io <- as.data.table(read_xlsx(file.path(DATA_PATH, "calibration", "chinaio", "2018 copy.xlsx"), col_types = "text"))
    num_sec <- 153
    sector <- clean_sectors(io, num_sec)
    iofromto <- clean_iofromto(io, num_sec)
    fwrite(sector, file.path(OUTPUT_PATH, "calibration", "iotables", "sector18_chn.csv"))
    fwrite(iofromto, file.path(OUTPUT_PATH, "calibration", "iotables", "iofromto18_chn.csv"))
}


save_io_chn()

