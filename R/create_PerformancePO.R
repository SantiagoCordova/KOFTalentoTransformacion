#' create_PerformancePO
#'
#' @param .pathPO   data path where official PO is saved
#' @param .year Year of interest
#' @param .patPerformance data path to raw performance data
#'
#' @return .data a estandarize po data frame with added performance columns for each choosen year
#' @export
#'
create_PerformancePO <- function(.pathPO, .patPerformance, .year){
  .dataPO  <- .pathPO %>%
    read.csv(., check.names = FALSE)

  .dataPerformance <- read_Performance(.patPerformance) %>%
    munge_YearPerformance(., .year)

  .data <- .dataPO %>% left_join(., .dataPerformance)

  return(.data)
}
