### Get dual use list

rm(list=ls())
gc()

library(data.table)
library(readxl)
library(stringr)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


get_dualuse <- function() {
    file_dict <- list(
        list(file = "2007 - Dual use correlation table 01 01 2007.xls", date = "2007-01-01", hs = "hs07_hs12_plus.csv"),
        list(file = "2008 - Dual use correlation table 01 may 2008.xls", date = "2008-05-01", hs = "hs07_hs12_plus.csv"),
        list(file = "2012 - Dual use correlation table 09 July 2012.xls", date = "2012-07-09", hs = "hs12_hs12_plus.csv"),
        list(file = "2012 - Dual use correlation table 17 feb 2012.xls", date = "2012-02-17", hs = "hs12_hs12_plus.csv"),
        list(file = "2013 - Dual use correlation table 01 July 2013.xlsx", date = "2013-07-01", hs = "hs12_hs12_plus.csv"),
        list(file = "2014 - Dual use correlation table 01 Apri 2014.xlsx", date = "2014-04-01", hs = "hs12_hs12_plus.csv"),
        list(file = "2015 - Dual use correlation table 01 Jan 2015.xlsx", date = "2015-01-01", hs = "hs12_hs12_plus.csv"),
        list(file = "2015 - Dual use correlation table 01 September 2015.xlsx", date = "2015-09-01", hs = "hs12_hs12_plus.csv"),
        list(file = "2016 - Dual use correlation table 01 Jan 2016.xlsx", date = "2016-01-01", hs = "hs12_hs12_plus.csv"),
        list(file = "2016 - Dual use correlation table 01 March 2016.xlsx", date = "2016-03-01", hs = "hs12_hs12_plus.csv"),
        list(file = "2017 - Dual use correlation table 01 Jan 2017.xlsx", date = "2017-01-01", hs = "hs17_hs12_plus.csv"),
        list(file = "2017 - Dual use correlation table 07 March 2017.xlsx", date = "2017-03-07", hs = "hs17_hs12_plus.csv"),
        list(file = "2018 - Dual use correlation table 01 Jan 2018.xlsx", date = "2018-01-01", hs = "hs17_hs12_plus.csv"),
        list(file = "2019 - Dual use correlation table 01 Jan 2019.xlsx", date = "2019-01-01", hs = "hs17_hs12_plus.csv"),
        list(file = "2020 - Dual use correlation table 01 Jan 2020.xlsx", date = "2020-01-01", hs = "hs17_hs12_plus.csv"),
        list(file = "2020 - Dual use correlation table 15 Dec 2020.xlsx", date = "2020-12-15", hs = "hs17_hs12_plus.csv"),
        list(file = "2021 - Dual use correlation table 13 April 2021.xlsx", date = "2021-04-13", hs = "hs17_hs12_plus.csv"),
        list(file = "2022 - Dual use correlation table January 2022- New HS.xlsx", date = "2022-01-01", hs = "hs22_hs12_plus.csv"),
        list(file = "2023 - Dual use correlation table January 2023.xlsx", date = "2023-01-01", hs = "hs22_hs12_plus.csv"),
        list(file = "2024 - Dual use correlation table January 2024.xlsx", date = "2024-01-01", hs = "hs22_hs12_plus.csv")
    )
    df_list <- list()
    df_list_cn <- list()
    for (i in seq_along(file_dict)) {
        print(paste0("Processing ", i, "..."))
        row <- file_dict[[i]]
        f <- row$file
        d <- row$date
        h <- row$hs
        cw <- fread(file.path(OUTPUT_PATH, "crosswalks", h), keepLeadingZeros = TRUE)

        df <- data.table(read_excel(file.path(DATA_PATH, "motivation", "eu_commission", f), sheet = 1))
        if (d == "2007-01-01") {
            df <- df[, c(1, 2, 3)]
        }
        
        if (i == 2) {
            colnames(df) <- c("cn_code", "taric", "dualuse", "comment1", "comment2")
        } else {
            colnames(df) <- c("cn_code", "taric", "dualuse")
        }
        
        sheet_names <- excel_sheets(file.path(DATA_PATH, "motivation", "eu_commission", f))
        if (length(sheet_names) > 1) {
            check <- data.table(read_excel(file.path(DATA_PATH, "motivation", "eu_commission", f), sheet = 2))
            if (i != 3) {
                colnames(check) <- c("cn_code", "taric", "dualuse")
                for (code in check$cn_code) {
                    if (!code %in% df$cn_code) {
                        print(code)
                        raise("Error")
                    }
                }
            }
        }

        df_cn <- copy(df)
        df_cn[, cn_code := substr(cn_code, 1, 8)]
        df_cn <- df_cn[, c("cn_code", "dualuse"), with = FALSE]
        df_cn <- unique(df_cn[, .(cn_code, dualuse)])[order(cn_code)]
        df_cn[, date := d]
        df_cn[, year := year(d)]
        df_list_cn[[i]] <- df_cn

        df <- df[, hscode_from := substr(cn_code, 1, 6)]
        df <- df[, .SD[1], by = "hscode_from"]
        df <- df[, .(hscode_from, dualuse)]
        df <- merge_df(df, cw, by = "hscode_from", how = "left", indicator = FALSE)
        df <- df[, .SD[1], by = "hscode12"]
        df <- df[, .(hscode12, dualuse)]
        df[, date := d]
        df_list[[i]] <- df
    }
    df <- rbindlist(df_list)
    df <- df[order(date, hscode12)]
    df[, dualuse_categ := substr(dualuse, 1, 1)]
    df[, dualuse_subct := substr(dualuse, 2, 2)]
    fwrite(df, file.path(OUTPUT_PATH, "motivation", "eu_commission", "dualuse.csv"))

    df_cn <- rbindlist(df_list_cn)
    df_cn[, dualuse_categ := substr(dualuse, 1, 1)]
    df_cn[, dualuse_subct := substr(dualuse, 2, 2)]
    fwrite(df_cn, file.path(OUTPUT_PATH, "motivation", "eu_commission", "dualuse_cn8_raw.csv"))
    df_cn[, max_date := max(date), by = "year"]
    df_cn <- df_cn[date == max_date]
    df_cn[, hscode_raw := substr(cn_code, 1, 6)]
    df_cn[startsWith(hscode_raw, "2710"), hscode_raw := "271000"]
    df_cn <- unique(df_cn[, c("year", "hscode_raw")])    
    fwrite(df_cn, file.path(OUTPUT_PATH, "motivation", "eu_commission", "dualuse_hs6_raw.csv"))
}


assemble_dualuse_hs4 <- function() {
    hscode <- fread(file.path(OUTPUT_PATH, "crosswalks", "H4_codes.csv"), keepLeadingZeros = TRUE)
    hscode <- hscode[nchar(sector) == 6]

    df <- fread(file.path(OUTPUT_PATH, "motivation", "eu_commission", "dualuse.csv"), keepLeadingZeros = TRUE)
    df[, dualuse_categ := as.character(dualuse_categ)]    
    df[, year := year(date)]
    df[, max_date := max(date), by = "year"]
    df <- df[max_date == date]
    year_matrix <- model.matrix(~as.factor(year) + 0, data = df)
    colnames(year_matrix) <- str_replace(colnames(year_matrix), "as.factor\\(year\\)", "dualuse_")
    df <- cbind(df, year_matrix)
    df <- df[
        , lapply(.SD, max, na.rm = TRUE),
        .SDcols = c(colnames(df)[startsWith(colnames(df), "dualuse")]),
        by = c("hscode12")
    ]
    df[, hscode12 := as.character(hscode12)]

    hscode <- merge_df(hscode, df, by.x = "sector", by.y = "hscode12", how = "left", indicator = FALSE)
    cols <- colnames(hscode)[grepl("dualuse_2", colnames(hscode))]
    for (col in cols) {
        hscode[is.na(get(col)), (col) := 0]
    }
    hscode[is.na(dualuse), dualuse := ""]
    hscode[is.na(dualuse_categ), dualuse_categ := ""]
    hscode[is.na(dualuse_subct), dualuse_subct := ""]

    # add military items
    hscode[, military := 0]
    hscode[startsWith(sector, "93"), military := 1]
    hscode[startsWith(sector, "8710"), military := 1]
    hscode[startsWith(sector, "890610"), military := 1]

    # add battlefield
    df <- fread(file.path(OUTPUT_PATH, "motivation", "eu_commission", "critical_list_wide.csv"), keepLeadingZeros = TRUE)
    df[, hs12 := as.character(hs12)]
    cols <- colnames(df)[colnames(df) != "hs12"]
    hscode <- merge_df(hscode, df, by.x = "sector", by.y = "hs12", how = "left", indicator = FALSE)
    for (col in cols) {
        hscode[is.na(get(col)), (col) := 0]
    }

    # add chemicals
    df <- fread(file.path(OUTPUT_PATH, "motivation", "eu_commission", "chemicals_wide.csv"), keepLeadingZeros = TRUE)
    df[, hs12 := as.character(hs12)]
    cols <- colnames(df)[colnames(df) != "hs12"]
    hscode <- merge_df(hscode, df, by.x = "sector", by.y = "hs12", how = "left", indicator = FALSE)
    for (col in cols) {
        hscode[is.na(get(col)), (col) := 0]
    }

    # add economic
    df <- fread(file.path(OUTPUT_PATH, "motivation", "eu_commission", "economic_list_wide.csv"), keepLeadingZeros = TRUE)
    df[, hs12 := as.character(hs12)]
    cols <- colnames(df)[colnames(df) != "hs12"]
    hscode <- merge_df(hscode, df, by.x = "sector", by.y = "hs12", how = "left", indicator = FALSE)
    for (col in cols) {
        hscode[is.na(get(col)), (col) := 0]
    }
    
    # fix 271000
    hscode[startsWith(sector, "2710"), sector := "271000"]
    hscode <- hscode[, lapply(.SD, max, na.rm = TRUE), .SDcols = c(colnames(hscode)[colnames(hscode) != "sector"]), by = "sector"]
    hscode[, ("_merge") := NULL]

    # add gta counts
    gta <- fread(file.path(OUTPUT_PATH, "motivation", "policy", "gta_policies_hs4.csv"), keepLeadingZeros = TRUE)
    hscode <- merge_df(hscode, gta, by = "sector", how = "left", indicator = FALSE)

    # add trade stats
    trade <- fread(file.path(OUTPUT_PATH, "motivation", "trade", "trade_shares_15_19.csv"), keepLeadingZeros = TRUE)
    hscode <- merge_df(hscode, trade, by.x = "sector", by.y = "hscode", how = "left", indicator = FALSE)
    hscode[is.na(perc_value), perc_value := 0]
    hscode[is.na(value_baci), value_baci := 0]
    fwrite(hscode, file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs4.csv"))
}


assemble_dualuse_hs4_4digit <- function() {
    hscode <- fread(file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs4.csv"), keepLeadingZeros = TRUE)
    hscode[, dualuse_categ := as.character(dualuse_categ)]
    hscode[is.na(dualuse), dualuse := ""]
    hscode[dualuse_categ == "NA", dualuse_categ := ""]
    hscode[is.na(dualuse_subct), dualuse_subct := ""]
    hscode[, c("gta_export_usa_post22", "gta_security_usa_post22", "gta_export_2018", "gta_export_2020", "gta_export_2022", "gta_security_2018", "gta_security_2020", "gta_security_2022", "value_baci", "value_sum", "perc_value") := NULL]

    hscode <- hscode[, sector := substr(sector, 1, 4)]
    cols <- colnames(hscode)[3:length(colnames(hscode))]
    hscode <- hscode[, lapply(.SD, max, na.rm = TRUE), .SDcols = cols, by = "sector"]

    names <- fread(file.path(OUTPUT_PATH, "crosswalks", "H4_codes.csv"), keepLeadingZeros = TRUE)
    names <- names[nchar(sector) == 4]

    hscode <- merge_df(names, hscode, by.x = "sector", by.y = "sector", how = "left", indicator = FALSE)
    hscode[, merge_ := NULL]

    gta <- fread(file.path(OUTPUT_PATH, "motivation", "policy", "gta_policies_hs4_4digit.csv"), keepLeadingZeros = TRUE)
    hscode <- merge_df(hscode, gta, by = "sector", how = "left", indicator = FALSE)

    trade <- fread(file.path(OUTPUT_PATH, "motivation", "trade", "trade_shares_15_19.csv"), keepLeadingZeros = TRUE)
    trade[, hscode := substr(hscode, 1, 4)]
    trade <- trade[, lapply(.SD, sum), .SDcols = c("value_baci", "value_sum", "perc_value"), by = c("hscode")]
    hscode <- merge_df(hscode, trade, by.x = "sector", by.y = "hscode", how = "left", indicator = FALSE)
    hscode[is.na(perc_value), perc_value := 0]
    hscode[is.na(value_baci), value_baci := 0]
    fwrite(hscode, file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs4_4digit.csv"))
}


assemble_dualuse_hs0 <- function() {
    hscode <- fread(file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs4.csv"), keepLeadingZeros = TRUE)
    hscode[, dualuse_categ := as.character(dualuse_categ)]
    hscode[is.na(dualuse), dualuse := ""]
    hscode[dualuse_categ == "NA", dualuse_categ := ""]
    hscode[is.na(dualuse_subct), dualuse_subct := ""]
    hscode[, c("gta_export_2018", "gta_export_2020", "gta_export_2022", "gta_security_2018", "gta_security_2020", "gta_security_2022", "value_baci", "value_sum", "perc_value") := NULL]

    cw <- fread(file.path(OUTPUT_PATH, "crosswalks", "hs92_hs12.csv"), keepLeadingZeros = TRUE)
    cw[startsWith(hs12, "2710"), hs12 := "271000"]
    hscode <- merge_df(hscode, cw, by.x = "sector", by.y = "hs12", how = "left")
    cols <- colnames(hscode)[3:(length(colnames(hscode)) - 3)]
    hscode <- hscode[, lapply(.SD, max, na.rm = TRUE), .SDcols = cols, by = "hs92"]

    names <- fread(file.path(OUTPUT_PATH, "crosswalks", "H0_codes.csv"))
    hscode <- merge_df(hscode, names, by.x = "hs92", by.y = "sector", how = "left")
    hscode[, merge_ := NULL]
    fwrite(hscode, file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs0.csv"))
}


assemble_dualuse_sitc2 <- function() {
    hscode <- fread(file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_hs4.csv"), keepLeadingZeros = TRUE)
    hscode[, dualuse_categ := as.character(dualuse_categ)]
    hscode[is.na(dualuse), dualuse := ""]
    hscode[dualuse_categ == "NA", dualuse_categ := ""]
    hscode[is.na(dualuse_subct), dualuse_subct := ""]
    hscode[, c("gta_export_2018", "gta_export_2020", "gta_export_2022", "gta_security_2018", "gta_security_2020", "gta_security_2022", "value_baci", "value_sum", "perc_value") := NULL]

    cw <- fread(file.path(OUTPUT_PATH, "crosswalks", "sitc75_hs12.csv"), keepLeadingZeros = TRUE)
    hscode <- merge_df(hscode, cw, by.x = "sector", by.y = "hs12", how = "inner")
    cols <- colnames(hscode)[3:(length(colnames(hscode)) - 3)]
    hscode <- hscode[, lapply(.SD, max, na.rm = TRUE), .SDcols = cols, by = "sitc75"]

    names <- fread(file.path(OUTPUT_PATH, "crosswalks", "S2_codes.csv"))
    hscode <- merge_df(hscode, names, by.x = "sitc75", by.y = "sector", how = "left")
    hscode[merge_ == "left_only", sitc_name := "Petroleum products, refined"]
    hscode[, merge_ := NULL]
    fwrite(hscode, file.path(OUTPUT_PATH, "motivation", "dualuse_masterfile_sitc2.csv"))
}


get_dualuse()
assemble_dualuse_hs4()
assemble_dualuse_hs4_4digit()
assemble_dualuse_hs0()
assemble_dualuse_sitc2()

