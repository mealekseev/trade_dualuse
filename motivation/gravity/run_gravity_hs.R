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


# run a gravity regression loop
regress_hscode <- function(f) {
    print(paste0("Processing ", f, "..."))

    df <- fread(file.path(EXTERNAL_PATH, "output", "motivation", "gravity", "hs0", f), keepLeadingZeros = TRUE)
    val <- sum(df$value)
    df[, value := 1000 * value]
    df[value == 0, value := 1]
    df[, log_value := log(value)]
    coeftable_list <- list()

    hscode <- substr(f, 13, 18)
    dualuse <- df$dualuse[1]

    df[, value_exp := sum(value), by = c("exporter_iso3")]
    df[, value_imp := sum(value), by = c("importer_iso3")]
    df[, share_exp := value_exp / sum(value)]
    df[, share_imp := value_imp / sum(value)]

    df[, wgt_no := 1]
    df[, wgt_yes := share_exp * share_imp]

    ix_cf <- 1
    col <- "modern_bloc"
    col_list <- c("cold_war_bloc", "modern_bloc")
    for (col in col_list) {
        col_exp <- paste0(col, "_exp")
        col_imp <- paste0(col, "_imp")

        df[, same_bloc := as.integer((get(col_exp) == get(col_imp)) & (get(col_exp) != 0))]
        df[, between_bloc := as.integer((get(col_exp) != get(col_imp)) & (get(col_exp) != 0) & (get(col_imp) != 0))]
        df[, nonaligned := as.integer((get(col_exp) != get(col_imp)) & ((get(col_exp) == 0) | (get(col_imp) == 0)))]            
        
        wgtcol <- "wgt_no"
        for (wgtcol in c("wgt_no", "wgt_yes")) {
            fml1 <- "log_value ~ between_bloc + nonaligned + factor(year):between_bloc + factor(year):nonaligned | exporter_iso3^importer_iso3 + exporter_iso3^year + importer_iso3^year"
            reg <- feols(as.formula(fml1), data = df, weights = df[[wgtcol]])
            summary(reg)
            coef <- reg$coefficients
            se <- sqrt(diag(vcov(reg, type = "hetero")))
            coeft <- data.table(
                spec = "1",
                wgt = wgtcol,
                bloc = col,
                hscode = hscode,
                value = val,
                dualuse = dualuse,
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

            fml2 <- "log_value ~ same_bloc + nonaligned + factor(year):same_bloc + factor(year):nonaligned | exporter_iso3^importer_iso3 + exporter_iso3^year + importer_iso3^year"
            reg <- feols(as.formula(fml2), data = df, weights = df[[wgtcol]])
            summary(reg)
            coef <- reg$coefficients
            se <- sqrt(diag(vcov(reg, type = "hetero")))
            coeft <- data.table(
                spec = "2",
                wgt = wgtcol,
                bloc = col,
                hscode = hscode,
                value = val,
                dualuse = dualuse,
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

            fml3 <- "log_value ~ between_bloc + same_bloc + factor(year):between_bloc + factor(year):same_bloc | exporter_iso3^importer_iso3 + exporter_iso3^year + importer_iso3^year"
            reg <- feols(as.formula(fml3), data = df, weights = df[[wgtcol]])
            summary(reg)
            coef <- reg$coefficients
            se <- sqrt(diag(vcov(reg, type = "hetero")))
            coeft <- data.table(
                spec = "3",
                wgt = wgtcol,
                bloc = col,
                hscode = hscode,
                value = val,
                dualuse = dualuse,
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
    fwrite(coeftable, file.path(EXTERNAL_PATH, "output", "motivation", "gravity", "ols_hs0", paste0("ols_hs0_", hscode, ".csv")))
}


file_list <- list.files(file.path(EXTERNAL_PATH, "output", "motivation", "gravity", "hs0"))
for (f in file_list) {
    regress_hscode(f)
}

