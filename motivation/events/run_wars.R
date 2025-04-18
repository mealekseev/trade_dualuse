### Run a gravity regression

rm(list=ls())
gc()

library(data.table)
library(stringr)
library(haven)
library(fixest)
library(zoo)

source("settings.R")
source("utilities.R")
source("ggplot_theme.R")


regress_wars_sitc <- function(f) {
    print(paste0("Processing ", f, "..."))

    df <- fread(file.path(EXTERNAL_PATH, "output", "motivation", "gravity", "sitc2", f), keepLeadingZeros = TRUE)
    df <- df[order(exporter_code, importer_code, year)]
    df[, value := as.numeric(value)]
    df[value == 0, value := 1]
    df[, log_value := log(value)]
    df[, dlog_value := log_value - shift(log_value, n = 1), by = c("exporter_code", "importer_code")]
    df[, d2log_value := dlog_value - shift(dlog_value, n = 1), by = c("exporter_code", "importer_code")]
    df[, abs_d2log_value := abs(d2log_value)]

    sitc <- substr(f, 15, 18)
    dualuse <- df$dualuse[1]
    dualuse_dummy <- df$dualuse_dummy[1]

    df_war <- fread(file.path(OUTPUT_PATH, "motivation", "events", "war_list.csv"))
    df_extra <- fread(file.path(DATA_PATH, "motivation", "events", "extra_manual.csv"))
    df_inter <- fread(file.path(DATA_PATH, "motivation", "events", "inter_manual.csv"))
    df_alliances <- rbind(df_extra, df_inter)
    df_alliances[allies == 0 & enemies == 0, allies := -99]

    coeftable_list <- list()
    ix_cf <- 1
    ix_pl <- 1
    i <- 1
    for (i in 1:nrow(df_war)) {
        row <- df_war[i]
        print(row)
        df_conflict <- merge_df(row, df_alliances, by = c("war_num", "war_name"), how = "left")
        df_all <- df_alliances[war_num == row$war_num]
        importer_list <- unique(df_conflict$importer_code)
        
        # form the dataset
        ys <- row$year_start - 10
        ye <- row$year_end + 10
        df_trade <- copy(df[(year >= ys) & (year <= ye)])
        df_trade <- merge_df(
            df_trade, df_all[, c("allies", "exporter_code", "importer_code")], by = c("exporter_code", "importer_code"),
            how = "outer", allow.cartesian = TRUE
        )
        df_trade[is.na(allies), allies := -99]
        if (length(unique(df_trade[merge_ == "right_only"]$exporter_code)) > 0) {
            print(unique(df_trade[merge_ == "right_only"]$exporter_code))
        }
        df_trade <- df_trade[merge_ != "right_only"]
        df_trade[, exporter_dummy := ""]
        df_trade[merge_ == "both", exporter_dummy := exporter_code]
        yr_list <- unique(df_trade$year)[unique(df_trade$year) != row$year_start - 1]
        for (yr in yr_list) {
            df_trade[, (paste0("yr", yr)) := as.integer(year == yr)]
        }
        yr_vars <- paste0("yr", yr_list)

        iso3 <- importer_list[1]
        for (iso3 in importer_list) {
            if (min(df_trade[importer_code == iso3, year]) > row$year_start - 1) {
                next
            }
            df_trade[, importer_dummy := as.integer(importer_code == iso3)]
            df_trade[, western_exp := as.integer(cold_war_bloc_exp == 1) * importer_dummy]
            df_trade[, eastern_exp := as.integer(cold_war_bloc_exp == 2) * importer_dummy]
            df_trade[, allies_exp := as.integer(allies == 1) * importer_dummy]
            df_trade[, enemies_exp := as.integer(allies == 0) * importer_dummy]

            exporter_dummy_list <- unique(df_trade[exporter_dummy != "", exporter_dummy])
            for (iso_exp in exporter_dummy_list) {
                df_trade[, (paste0("exporter_dummy_", iso_exp)) := as.integer(exporter_code == iso_exp) * importer_dummy]
            }

            df_trade[, wgt_no := 1]
            df_trade[(year <= row$year_start - 1) & (importer_dummy == 1), wgt_yes := value]
            df_trade[, wgt_yes := sum(wgt_yes, na.rm = TRUE), by = c("exporter_code")]
            df_trade[is.na(wgt_yes) | wgt_yes == -Inf | wgt_yes == 0, wgt_yes := 1]

            val <- sum(df_trade[year <= row$year_start - 1 & importer_dummy == 1, value] / 1000)
            cold_war_bloc_imp_val <- df_trade[importer_dummy == 1, cold_war_bloc_imp][1]
        
            wgtcol <- "wgt_no"
            for (wgtcol in c("wgt_no", "wgt_yes")) {
                # run simple imports regression
                print("Run reg 1...")
                max_val <- max(df_trade[importer_dummy == 1][["abs_d2log_value"]], na.rm = TRUE)
                min_val <- min(df_trade[importer_dummy == 1][["abs_d2log_value"]], na.rm = TRUE)
                if (max_val == min_val) {
                    next
                }
                fml1 <- paste0(
                    "abs_d2log_value ~ ",
                    paste(paste0("importer_dummy:", yr_vars, sep = ""), collapse = " + "),
                    " | exporter_code^importer_code + exporter_code^year"
                )
                reg <- feols(as.formula(fml1), data = df_trade, weights = df_trade[[wgtcol]])
                coef <- reg$coefficients
                se <- sqrt(diag(vcov(reg, type = "hetero")))
                coeft <- data.table(
                    war_num = row$war_num,
                    war_name = row$war_name,
                    year_start = row$year_start,
                    year_end = row$year_end,
                    importer_code = iso3,
                    cold_war_bloc_imp = cold_war_bloc_imp_val,
                    spec = "1",
                    wgt = wgtcol,
                    sitc = sitc,
                    value = val,
                    dualuse = dualuse,
                    dualuse_dummy = dualuse_dummy,
                    names = names(coef),
                    coef = coef,
                    se = se
                )
                coeft[, beta_upper := coef + 1.96 * se]
                coeft[, beta_lower := coef - 1.96 * se]
                coeft[, c("var1", "var2") := tstrsplit(names, ":")]
                coeft[startsWith(var1, "yr"), year := str_replace(var1, "yr", "")]
                coeft[startsWith(var2, "yr"), year := str_replace(var2, "yr", "")]
                coeft[!startsWith(var1, "yr"), var := var1]
                coeft[!startsWith(var2, "yr"), var := var2]
                coeft[, c("var1", "var2") := NULL]
                ix_cf <- ix_cf + 1
                coeftable_list[[ix_cf]] <- coeft

                # run bloc regression
                print("Run reg 2...")
                fml2 <- paste0(
                    "log_value ~ ",
                    paste(paste0("importer_dummy:", yr_vars, sep = ""), collapse = " + "), " + ",
                    paste(paste0("western_exp:", yr_vars, sep = ""), collapse = " + "), " + ",
                    paste(paste0("eastern_exp:", yr_vars, sep = ""), collapse = " + "), 
                    " | exporter_code^importer_code + exporter_code^year"
                )
                reg <- feols(as.formula(fml2), data = df_trade, weights = df_trade[[wgtcol]])
                coef <- reg$coefficients
                se <- sqrt(diag(vcov(reg, type = "hetero")))
                coeft <- data.table(
                    war_num = row$war_num,
                    war_name = row$war_name,
                    year_start = row$year_start,
                    year_end = row$year_end,
                    importer_code = iso3,
                    cold_war_bloc_imp = cold_war_bloc_imp_val,
                    spec = "2",
                    wgt = wgtcol,
                    sitc = sitc,
                    value = val,
                    dualuse = dualuse,
                    dualuse_dummy = dualuse_dummy,
                    names = names(coef),
                    coef = coef,
                    se = se
                )
                coeft[, beta_upper := coef + 1.96 * se]
                coeft[, beta_lower := coef - 1.96 * se]
                coeft[, c("var1", "var2") := tstrsplit(names, ":")]
                coeft[startsWith(var1, "yr"), year := str_replace(var1, "yr", "")]
                coeft[startsWith(var2, "yr"), year := str_replace(var2, "yr", "")]
                coeft[!startsWith(var1, "yr"), var := var1]
                coeft[!startsWith(var2, "yr"), var := var2]
                coeft[, c("var1", "var2") := NULL]
                ix_cf <- ix_cf + 1
                coeftable_list[[ix_cf]] <- coeft

                # run alliance regressions
                print("Run reg 3...")
                fml3 <- paste0(
                    "log_value ~ ",
                    paste(paste0("importer_dummy:", yr_vars, sep = ""), collapse = " + "), " + ",
                    paste(paste0("allies_exp:", yr_vars, sep = ""), collapse = " + "), " + ",
                    paste(paste0("enemies_exp:", yr_vars, sep = ""), collapse = " + "), 
                    " | exporter_code^importer_code + exporter_code^year"
                )                
                reg <- feols(as.formula(fml3), data = df_trade, weights = df_trade[[wgtcol]])
                coef <- reg$coefficients
                se <- sqrt(diag(vcov(reg, type = "hetero")))
                coeft <- data.table(
                    war_num = row$war_num,
                    war_name = row$war_name,
                    year_start = row$year_start,
                    year_end = row$year_end,
                    importer_code = iso3,
                    cold_war_bloc_imp = cold_war_bloc_imp_val,
                    spec = "3",
                    wgt = wgtcol,
                    sitc = sitc,
                    value = val,
                    dualuse = dualuse,
                    dualuse_dummy = dualuse_dummy,
                    names = names(coef),
                    coef = coef,
                    se = se
                )
                coeft[, beta_upper := coef + 1.96 * se]
                coeft[, beta_lower := coef - 1.96 * se]
                coeft[, c("var1", "var2") := tstrsplit(names, ":")]
                coeft[startsWith(var1, "yr"), year := str_replace(var1, "yr", "")]
                coeft[startsWith(var2, "yr"), year := str_replace(var2, "yr", "")]
                coeft[!startsWith(var1, "yr"), var := var1]
                coeft[!startsWith(var2, "yr"), var := var2]
                coeft[, c("var1", "var2") := NULL]
                ix_cf <- ix_cf + 1
                coeftable_list[[ix_cf]] <- coeft
            }
        }
    }
    coeftable <- rbindlist(coeftable_list)
    fwrite(coeftable, file.path(OUTPUT_PATH, "motivation", "events", "flow", paste0("vol_", sitc, ".csv")))
}


file_list <- list.files(file.path(EXTERNAL_PATH, "output", "motivation", "gravity", "sitc2"))
for (f in file_list) {
    regress_wars_sitc(f)
}

