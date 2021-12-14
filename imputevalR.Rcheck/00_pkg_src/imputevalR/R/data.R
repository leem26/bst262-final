#' Example NHANES data with no missing values
#'
#' A dataset containing sample variables from the National
#' Health and Nutrition Examination Survey (2017-18) cycle
#'
#' @format A data frame with 9254 rows and 21 variables:
#' \describe{
#'   \item{gender}{gender (male/female)}
#'   \item{age}{age in years}
#'   \item{white}{Non-Hispanic White race/ethnicity}
#'   \item{poverty}{ratio of household income to federal poverty level}
#'   \item{weight}{measured weight in kg }
#'   \item{height}{measured height in cm}
#'   \item{bmi}{BMI in kg/m^2}
#'   \item{waist_circum}{waist circumference in cm}
#'   \item{hip_circum}{hip circumference in cm}
#'   \item{sbp1}{systolic blood pressure (4 readings)}
#'   \item{sbp2}{systolic blood pressure (4 readings)}
#'   \item{sbp3}{systolic blood pressure (4 readings)}
#'   \item{sbp4}{systolic blood pressure (4 readings)}
#'   \item{dbp1}{diastolic blood pressure (4 readings)}
#'   \item{dbp2}{diastolic blood pressure (4 readings)}
#'   \item{dbp3}{diastolic blood pressure (4 readings)}
#'   \item{dbp4}{diastolic blood pressure (4 readings)}
#'   \item{android_pfat}{percent fat in android}
#'   \item{gynoid_pfat}{percent fat in gyneoid}
#'   \item{selfreported_weight}{self rep weight in lbs}
#'   \item{selfreported_ht}{self rep height in inches}
#' }
#' @source \url{https://www.cdc.gov/nchs/nhanes/index.htm}
"nhanes"