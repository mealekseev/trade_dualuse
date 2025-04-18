### Collapse CSMAR

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(zoo)

source("settings.R")
source("utilities.R")


clean_mil_firm_list <- function() {
    mil_firm_list <- data.table(read_excel(file.path(EXTERNAL_PATH, "data", "calibration", "csmar", "军工相关上市公司清单2024.1.2.xlsx"), col_types = "text"))
    setnames(mil_firm_list, old = names(mil_firm_list), new = tolower(names(mil_firm_list)))
    mil_firm_list <- mil_firm_list[, !c("ideaname")]
    return(mil_firm_list)
}


clean_firm_ind <- function(mil_firm_list) {
    firm_info_yearly <- data.table(read_excel(file.path(EXTERNAL_PATH, "data", "calibration", "csmar", "STK_LISTEDCOINFOANL", "STK_LISTEDCOINFOANL.xlsx"), col_types = "text"))
    firm_info_yearly <- firm_info_yearly[!c(1:2),]
    setnames(firm_info_yearly, old = names(firm_info_yearly), new = tolower(names(firm_info_yearly)))
    setnames(firm_info_yearly, old = c("industrycodec"), new = c("industrycode2012")) # industrycodec is based on 2012 CSRC code which is based on GB-T 4754-2011
    firm_info_yearly1 <- firm_info_yearly[symbol %in% mil_firm_list$symbol]
    firm_info_yearly2 <- firm_info_yearly[shortname %in% mil_firm_list$shortname & !symbol %in% mil_firm_list$symbol]
    firm_info_yearly2[symbol == "832317", symbol := "688287"] # 观典防务 seemed to have changed the symbol, but it's the same firm judging from the shortname as well as address and main business
    firm_info_yearly1 <- rbind(firm_info_yearly1, firm_info_yearly2[symbol == "688287"], use.names = T) 
    firm_info_yearly1 <- merge(firm_info_yearly1, mil_firm_list, by = "symbol", suffixes = c("",".mil_list"), all.x = T)
    firm_info_yearly1[, ':=' (year = year(enddate))]
    firm_info_yearly1 <- firm_info_yearly1[, c("symbol", "year", "industrycode2012")]
    return(firm_info_yearly1)
}


clean_financial_statements <- function(mil_firm_list) {
    fd <- data.table(read_excel(file.path(EXTERNAL_PATH, "data", "calibration", "csmar", "FS_Comins", "FS_Comins.xlsx"), col_types = c("text")))
    fd <- fd[!c(1:2),]
    setnames(fd, old = c("Stkcd", "ShortName", "Accper", "Typrep", "B001100000"), new = c("symbol", "shortname", "report_date", "type", "tot_rev"))
    fd <- fd[, c("symbol", "shortname", "report_date", "type", "tot_rev")] # type A ＝ 合并报表 (consolidated); type B ＝ 母公司报表 (parent company)
    fd[, ':=' (report_year = year(report_date),
            report_month_date = paste0(month(report_date), "-", mday(report_date)),
            tot_rev = as.numeric(tot_rev))]
    fd1 <- fd[symbol %in% mil_firm_list$symbol]
    fd2 <- fd[shortname %in% mil_firm_list$shortname & !symbol %in% mil_firm_list$symbol]
    fd2[symbol == "832317", symbol := "688287"] # 观典防务 seemed to have changed the symbol, but it's the same firm judging from the shortname as well as address and mainbusiness
    fd1 <- rbind(fd1, fd2[symbol == "688287"], use.names = T)
  
    fd1[report_month_date == "1-1", ':='(report_year = report_year - 1)] # 1-1 report is about the previous year
    fd1[, ':=' (rev_rank = frank(-tot_rev)), by = c("symbol", "type", "report_year")]
    table(fd1[, c("report_month_date", "rev_rank")])
  
    fd1 <- fd1[, .(tot_rev = max(tot_rev, na.rm = T)), by = c("symbol", "type", "report_year")]
    fd1[tot_rev == -Inf, tot_rev := NA]
    fd1[, ':=' (type = paste0("tot_rev.type", type))]
    fd2 <- dcast(fd1, symbol + report_year ~ type, value.var = "tot_rev")
    return(fd2)
}


combine_info <- function(){
    mil_firm_list <- clean_mil_firm_list()
    firm_info_yearly <- clean_firm_ind(mil_firm_list)
    fd <- clean_financial_statements(mil_firm_list)
    mg <- merge(firm_info_yearly, fd, by.x = c("symbol", "year"), by.y = c("symbol", "report_year"), all.y = T)
    setorder(mg, symbol, year)
    mg[, industrycode2012 := na.locf(industrycode2012, na.rm = F, fromLast = F), by = "symbol"]
    mg[, industrycode2012 := na.locf(industrycode2012, na.rm = F, fromLast = T), by = "symbol"]
    mg = mg[, .(tot_rev.A = sum(tot_rev.typeA, na.rm = T), tot_rev.B = sum(tot_rev.typeB, na.rm = T)),
                by = c("year", "industrycode2012")]
    mg[, ':=' (share_rev.A = tot_rev.A / sum(tot_rev.A), share_rev.B = tot_rev.B / sum(tot_rev.B)), by = c("year")]
    mg = mg[year > 1993] # there was less then 10 military firms in 1991-1993
    fwrite(mg, file.path(OUTPUT_PATH, "calibration", "iotables", "share_rev_by_ind_chn.csv"))
    return(mg)
}


crosswalk_industries <- function() {
    mg <- combine_info()
    mg[, ':=' (ind = as.numeric(substr(industrycode2012, 2, 3)))]
  
    # industrycode2012 = GBT 2011
    mg[ind > 78 & ind < 90, ':=' (ind = ind + 1)] # crosswalk GBT-2011 to GBT 2017; post-78 codes are different in these two versions
    mg[ind == 90, ind := 9999] # ind 90 is the public sector, recode it to 9999
  
    # load crosswalks
    gbt_naics12 <- fread(file.path(OUTPUT_PATH, "crosswalks", "gbt2017_naics12.csv"))
    gbt_naics12[, gbt17_2 := as.numeric(gbt17_2)]
    gbt_naics12[, naics12 := as.character(naics12)]

    mg1 <- merge(mg, gbt_naics12, by.x = "ind", by.y = "gbt17_2", all.x = T, allow.cartesian = T)
    mg2 <- mg1[, .(
        tot_rev.A = sum(tot_rev.A * w, na.rm = T), tot_rev.B = sum(tot_rev.B * w, na.rm = T)),
        by = c("year", "naics12")
    ]
    fwrite(mg2, file.path(OUTPUT_PATH, "calibration", "iotables", "share_rev_by_naics_chn.csv"))
}


mg <- combine_info()
crosswalk_industries()

