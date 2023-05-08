#' error_Performance
#'
#' @param .data Performance data from read_Performance.R
#' @param .dataPO Employee Standardized data
#'
#' @return KOF employees with multiple grades
#' @export
#'
error_Performance     <- function(.data, .dataPO){
  ERRR <- .data %>%
    dplyr::group_by(`ID de usuario/empleado`, Year) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1L)  %>%
    return()

  ERRR <- ERRR %>%
    left_join(., .dataPO) %>%
    select(Operación, `ID de usuario/empleado`, `Mostrar nombre`, Year, n) %>%
    filter(! Operación %>% is.na())

  return(ERRR)
}
