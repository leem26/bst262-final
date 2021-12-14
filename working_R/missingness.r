
#########################
# Make Missing Function #
#########################

makeNA <- function (df, proportionNA) {
  r <- nrow(df)
  c <- ncol(df)
  NAs <- rep(FALSE, r * c)
  NAs[sample.int(r * c, floor(r * c * proportionNA))] <- TRUE
  df[matrix(NAs, nrow = r, ncol = c)] <- NA
  return(df)
}

