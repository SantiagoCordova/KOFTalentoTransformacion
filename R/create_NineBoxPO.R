#' create_NineBoxPO
#'
#' @param .pathPO   data path where official PO is saved
#' @param .path9Box data path where raw 9BOx data is saved
#' @param .year Year of interest
#'
#' @return .data a estandarize po data frame with added 9Box columns for current palcement and last placement
#' @export
#'
create_NineBoxPO <- function(.pathPO, .path9Box, .year){
  .dataPO  <- .pathPO %>%
    read.csv(., check.names = FALSE)

  .dataNineBox <- read_NineBox(.path9Box) %>%
    munge_YearNineBoxPO(., .dataPO, .year)

  .data <- .dataPO %>% left_join(., .dataNineBox)

  return(.data)
}


