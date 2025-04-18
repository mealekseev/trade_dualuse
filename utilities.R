### Utilities


merge_df = function(
    df1, df2, by.x = "", by.y = "", by = "", how = "outer", indicator = TRUE, raise = "",
    allow.cartesian = FALSE, silent = FALSE
    ) {
  df1[, TEMPVAR1 := .I]
  df2[, TEMPVAR2 := .I]
  if (length(by) > 1) {
    by.x = by
    by.y = by
  } else if (by != "") {
    by.x = by
    by.y = by
  }
  df = merge(df1, df2, by.x = by.x, by.y = by.y, all.x = TRUE, all.y = TRUE,
             allow.cartesian = allow.cartesian)
  df[, merge_ := "both"]
  df[is.na(TEMPVAR1), merge_ := "right_only"]
  df[is.na(TEMPVAR2), merge_ := "left_only"]
  res = table(df$merge_)
  if (!silent) {
    print(res)
  }
  if (raise == "left") {
    if ("left_only" %in% names(res)) {
      stop("Bad match")
    }
  }
  if (raise == "right") {
    if ("right_only" %in% names(res)) {
      stop("Bad match")
    }
  }
  if (raise == "unmatched") {
    if (("right_only" %in% names(res)) | ("left_only" %in% names(res))) {
      stop("Bad match")
    }
  }
  if (raise == "matched") {
    if (("right_only" %in% names(res)) | ("left_only" %in% names(res))) {
      stop("Bad match")
    }
  }
  df1[, TEMPVAR1 := NULL]
  df2[, TEMPVAR2 := NULL]
  df[, TEMPVAR1 := NULL]
  df[, TEMPVAR2 := NULL]
  if (how == "inner") {
    df = df[merge_ == "both"]
  }
  if (how == "left") {
    df = df[merge_ %in% c("both", "left_only")]
  }
  if (how == "right") {
    df = df[merge_ %in% c("both", "right_only")]
  }
  if (!indicator){
    df[, merge_ := NULL]
  }
  return(df)
}


