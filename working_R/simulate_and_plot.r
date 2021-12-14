# Set source directory to location of this file
# setwd(dirname(parent.frame(2)$ofile))
setwd("/Volumes/GoogleDrive-101809232694958266345/My Drive/Matt/School/PhD /2021-2022/BST 262/Project/bst262-final/working_R/")

# load mice
library(mice)
library(miceadds)

# source all the functions (from package)
source.all("../imputevalR/R/")
# source("compare_data.r")
# source("imputation.R")
# source("missingness.r")
# source("pool_df.r")

# df = the data frame
# num = the number of simulations
# miss = proportion of missingness
simulate <- function(df, num, miss){
  
  # create the data frame of differences: each column is a variable from the original data frame. Each row has the difference between mice and our method
  differences <- data.frame( matrix( nrow = 0, ncol = dim(df)[2])  )
  names(differences) <- names(df)
  
  # go through num simulations
  for (i in 1:num){
    
    cat(paste0("SIMULATION NUMBER: ", i, "/", num, "\n"))

    # create a copy of the data frame
    df_orig <- df[,]
    
    # create missing values
    df_miss <- makeNA(df_orig, miss)
    
    # impute and pool the imputed data sets to make one data frame with our method
    # 3 chains, 100 iterations each
    imputed_list <- imputer(df_miss, nchains = 3, niter = 100)
    df_imp <- pool_df(imputed_list)
    
    # *************** now impute with mice
    
    # make gender and white into factors for mice. mice must run after our imputer, because our imputer won't like this step
    df_miss$gender <- as.factor(df_miss$gender)
    df_miss$white <- as.factor(df_miss$white)
    
    init = mice(df_miss, maxit=0, print=FALSE) 
    meth = init$method
    predM = init$predictorMatrix
    
    # set method to impute each variable
    meth[c("age", "poverty", "weight", "height", "bmi", "waist_circum", "hip_circum", "sbp1", "dbp1", "sbp2", "dbp2", "sbp3", "dbp3", "sbp4", "dbp4", "android_pfat", "gynoid_pfat", "selfreported_weight", "selfreported_ht")]="norm" 
    
    meth[c("gender", "white")] = "logreg" 
    
    df_mice = mice(df_miss, method=meth, predictorMatrix=predM, m=5, print = FALSE)
    
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


# # read hanes data
# hanes_full <- read.csv("../raw_data/imputed_nhanes.csv")


# # ******** restrict to 50 rows.
# hanes_full <- hanes_full[1:100,]

# run simulation on full dataset
load(file = "../imputevalR/data/nhanes.rda") # from package data folder

# ******** run 100 simulations with 0.2 values missing
sim_diff <- simulate(df = nhanes, num = 20, miss = 0.2)


saveRDS(sim_diff, "sim_diff_100.Rdata")

# ******** make a histogram of the simulation differences for bmi
# hist(sim_diff$bmi)


