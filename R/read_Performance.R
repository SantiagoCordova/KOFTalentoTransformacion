#' read_Performance
#'
#' @param .path path to Performance Employee Central Report
#'
#' @return data as dataframe with standarize tipes and field names.
#' @export
#'
read_Performance      <- function(.path){
  .data <- .path %>%
    read.csv(., check.names = FALSE)
  names(.data) <- c('ID de usuario/empleado', 'Year', 'Multiple', 'Rating')
  .data <- .data %>%
    mutate_at(c('ID de usuario/empleado', 'Year', 'Multiple', 'Rating'), as.numeric)
  return(.data)
}
