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


regress_sitc <- function(f) {
    print(paste0("Processing ", f, "..."))

    df <- fread(file.path(EXTERNAL_PATH, "output", "motivation", "gravity", "sitc2", f), keepLeadingZeros = TRUE)
    df[, value := as.numeric(value)]
    val <- sum(df$value)
    df[value == 0, value := 1]
    df[, log_value := log(value)]
    coeftable_list <- list()

    sitc <- substr(f, 15, 18)
    dualuse <- df$dualuse[1]
    dualuse_dummy <- df$dualuse_dummy[1]
    df[, wgt_no := 1]

    ix_cf <- 1
    col <- "modern_bloc"
    col_list <- c("cold_war_bloc", "gokmen_bloc", "modern_bloc")
    for (col in col_list) {
        col_exp <- paste0(col, "_exp")
        col_imp <- paste0(col, "_imp")

        df[, same_bloc := as.integer((get(col_exp) == get(col_imp)) & (get(col_exp) != 0))]
        df[, between_bloc := as.integer((get(col_exp) != get(col_imp)) & (get(col_exp) != 0) & (get(col_imp) != 0))]
        df[, nonaligned := as.integer((get(col_exp) != get(col_imp)) & ((get(col_exp) == 0) | (get(col_imp) == 0)))]            
        
        wgt_list <- c("wgt_no")
        for (wgtcol in wgt_list) {
            print(paste0(col, " ", wgtcol))
            
            print("Start reg 1...")
            fml1 <- "log_value ~ between_bloc + nonaligned + factor(year):between_bloc + factor(year):nonaligned | exporter_code^importer_code + exporter_code^year + importer_code^year"
            reg <- feols(as.formula(fml1), data = df)
            coef <- reg$coefficients
            se <- sqrt(diag(vcov(reg, type = "hetero")))
            coeft <- data.table(
                spec = "1",
                wgt = wgtcol,
                bloc = col,
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
            coeft[, c("variable", "year") := tstrsplit(names, ":")]
            coeft[, year := str_replace(year, "factor\\(year\\)", "")]
            ix_cf <- ix_cf + 1
            coeftable_list[[ix_cf]] <- coeft

            print("Start reg 2...")
            fml2 <- "log_value ~ same_bloc + nonaligned + factor(year):same_bloc + factor(year):nonaligned | exporter_code^importer_code + exporter_code^year + importer_code^year"
            reg <- feols(as.formula(fml2), data = df)
            coef <- reg$coefficients
            se <- sqrt(diag(vcov(reg, type = "hetero")))
            coeft <- data.table(
                spec = "2",
                wgt = wgtcol,
                bloc = col,
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
            coeft[, c("variable", "year") := tstrsplit(names, ":")]
            coeft[, year := str_replace(year, "factor\\(year\\)", "")]
            ix_cf <- ix_cf + 1
            coeftable_list[[ix_cf]] <- coeft

            print("Start reg 3...")
            fml3 <- "log_value ~ between_bloc + same_bloc + factor(year):between_bloc + factor(year):same_bloc | exporter_code^importer_code + exporter_code^year + importer_code^year"
            reg <- feols(as.formula(fml3), data = df)
            coef <- reg$coefficients
            se <- sqrt(diag(vcov(reg, type = "hetero")))
            coeft <- data.table(
                spec = "3",
                wgt = wgtcol,
                bloc = col,
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
            coeft[, c("variable", "year") := tstrsplit(names, ":")]
            coeft[, year := str_replace(year, "factor\\(year\\)", "")]
            ix_cf <- ix_cf + 1
                
            coeftable_list[[ix_cf]] <- coeft
        }
    }
    coeftable <- rbindlist(coeftable_list)
    fwrite(coeftable, file.path(EXTERNAL_PATH, "output", "motivation", "gravity", "ols_sitc2", paste0("ols_sitc2_", sitc, ".csv")))
}


file_list <- list.files(file.path(EXTERNAL_PATH, "output", "motivation", "gravity", "sitc2"))
for (f in file_list) {
    regress_sitc(f)
}

