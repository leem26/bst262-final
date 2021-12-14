###############################################################################
#
# Methods to implement imputation for completely non-missing data
# (i.e. simulation vignette methods)
#
###############################################################################


#' Set random values in a data frame to missing
#' 
#' @param df A data.frame for which NA values should be generated
#' @param proportionNA The proportion of all cells that should be NA, 
#' across variables and observations
#' 
#' @return A data.frame (\code{df}) that is the same dimensions as the input
#' data frame, but now has values missing (set by \code{proportionNA})
#' 
#' @examples
#' data(nhanes)
#' miss_data <- imputevalR::makeNA(nhanes, proportionNA = 0.2)
#' 
#' @importFrom Rdpack reprompt
#' @import stats
#' 
#' @export
makeNA <- function (df, proportionNA = 0.2) {
  r <- nrow(df)
  c <- ncol(df)

  # length of data rows times # variables
  NAs <- rep(FALSE, r * c)

  # sample from r x c cells to get proportionNA NA's
  NAs[sample.int(r * c, floor(r * c * proportionNA))] <- TRUE
  df[matrix(NAs, nrow = r, ncol = c)] <- NA
  
  # return data with NA values
  return(df)
}

#' Squared error between means for all columns in a dataset
#' 
#' @param df1 The first data.frame for which comparisons should be made
#' @param df2 The second data.frame for which comparisons should be made
#' @return \code{squarediff}, a named vector with length equal to the 
#' the number of variables for the comparison
#' 
#' @import stats
#' 
#' @export
compare_by_column <- function(df1, df2){
  
  # check length of variables
  if (length(names(df1)) != length(names(df2))) {
    stop("df1 and df2 have different numbers of variables!")
  }
  mean1 <- apply(df1, 2, mean, na.rm=TRUE)
  mean2 <- apply(df2, 2, mean, na.rm=TRUE)
  
  squarediff <- (mean1-mean2)**2
  
  return(squarediff)
  
}

#' Squared error between means for all cells in a dataset
#' 
#' @param df1 The first data.frame for which comparisons should be made
#' @param df2 The second data.frame for which comparisons should be made
#' @return \code{summed_diff}, a named vector of length equal to the 
#' number of variables, where each value is the summed squared difference between 
#' the imputed and true value of a cell, for each variable
#' 
#' @import stats
#' 
#' @export
compare_by_cell <- function(df1, df2){

  # check length of variables
  if (length(names(df1)) != length(names(df2))) {
    stop("df1 and df2 have different numbers of variables!")
  }
  compared_df <- (df1 - df2)**2
  summed_diff <- apply(compared_df, 2, sum, na.rm=TRUE)
  
  return(summed_diff)
  
}














