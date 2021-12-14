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
#' data(nhanes, package = "imputevalR")
#' miss_data <- imputevalR::makeNA(nhanes, proportionNA = 0.2)
#' 
#' @importFrom Rdpack reprompt
#' @importFrom stats runif quantile glm lm predict rnorm binomial rbinom
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
#' @importFrom stats runif quantile glm lm predict rnorm binomial rbinom
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
#' @importFrom stats runif quantile glm lm predict rnorm binomial rbinom
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

#' Simulation study to evaluate imputer() vs. MICE using predictive mean matching
#' 
#' Completes a simulation study to compare the differences between \code{mice()} from the mice package
#' and \code{imputer()}, using an example complete case NHANES dataset from 2017-2018. Differences
#' are generated as the absolute difference between (1) \code{imputer} and the original NHANES data and (2)
#' \code{mice} and the original NHANES data, scaled by the difference \code{mice} and the original NHANES data.
#' Results are therefore interpreted as percent difference between \code{imputer} and \code{mice} as a function of
#' the difference between \code{mice} and the true cell values. 
#' 
#' @param proportionNA The proportion of all cells that should be NA, 
#' across variables and observations, to be passed to \code{makeNA()}
#' @param numSims Number of simulations to run
#' @param nchains The number of chains (ie number of imputed datasets) to generate for imputer()
#' @param niter The number of iterations to complete in each chain (default is 100 for convergence) for imputer()
#' 
#' @return A data.frame (\code{differences}) that includes the results of the simulation, with
#' a row for each simulation and a column for each variable
#' 
#' @examples
#' # NOT RUN FOR RUNTIME
#' \dontrun{
#' library(imputevalR)
#' simulate_nhanes_study(numSims = 20, proportionNA = 0.2)
#' }
#' 
#' @importFrom Rdpack reprompt
#' @importFrom stats runif quantile glm lm predict rnorm binomial rbinom 
#' @importFrom mice mice complete
#' @importFrom utils data
#' 
#' @export
simulate_nhanes_study <- function(numSims = 50, proportionNA = 0.2, nchains = 3, niter = 100) {
  
  # complete example nhanes dataset
  df <- get("nhanes")

  # create the data frame of differences: each column is a variable from the original data frame. Each row has the difference between mice and our method
  differences <- data.frame( matrix( nrow = 0, ncol = dim(df)[2])  )
  names(differences) <- names(df)
  
  # go through num simulations
  for (i in 1:numSims){
    
    cat(paste0("SIMULATION NUMBER: ", i, "/", numSims, "\n"))

    # create a copy of the data frame
    df_orig <- df[,]
    
    # create missing values
    df_miss <- makeNA(df_orig, proportionNA)
    
    # impute and pool the imputed data sets to make one data frame with our method
    # 3 chains, 100 iterations each
    imputed_list <- imputer(df_miss, nchains = nchains, niter = niter)
    df_imp <- pool_df(imputed_list)
    
    # *************** now impute with mice
    
    # make gender and white into factors for mice. mice must run after our imputer, because our imputer won't like this step
    df_miss$gender <- as.factor(df_miss$gender)
    df_miss$white <- as.factor(df_miss$white)
    
    init = mice(df_miss, maxit=0, printFlag=FALSE) 
    meth = init$method
    predM = init$predictorMatrix
    
    # set method to impute each variable
    meth[c("age", "poverty", "weight", "height", "bmi", "waist_circum", 
      "hip_circum", "sbp1", "dbp1", "sbp2", "dbp2", "sbp3", "dbp3", 
      "sbp4", "dbp4", "android_pfat", "gynoid_pfat", "selfreported_weight", 
      "selfreported_ht")]="norm" 
    
    meth[c("gender", "white")] = "logreg" 
    
    df_mice = mice(df_miss, method=meth, predictorMatrix=predM, m=5, printFlag = FALSE)
    
    df_mice <- complete(df_mice)
    
    # compare our imputer to the original, then compare mice to the original
    compare_imp <- compare_by_cell(df_orig, df_imp)
    compare_mice <- compare_by_cell(df_orig, df_mice)
    
    # take the absolute difference of the two
    diff <- abs(compare_imp - compare_mice)
    
    # now normalize it to the comparison of mice to the original
    diff <- diff/compare_mice
    
    # add the difference to our data frame of differences
    differences <- rbind(differences, diff)
    
    # remove the variables to start the next cycle
    rm(df_orig, df_miss, df_imp, df_mice)
    
  }
  
  names(differences) <- names(df)
  
  return(differences)
}












