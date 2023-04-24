#' NineBoxDataQuality
#'
#' @param .path9Box file where raw 9box data is saved
#' @param .pathPO file where PO standarized is saved
#' @param .year 9box year of interest
#'
#' @return .data data frame where an employee have multiple 9box for the same year
#' @export
#'
NineBoxDataQuality <- function(.path9Box, .pathPO, .year){

  NineBox <- read_NineBox(.path9Box)
  .dataPO  <- .pathPO %>%
    read.csv(., check.names = FALSE)

  NineBox <- NineBox %>%
    filter(Year == .year) %>%
    select(`ID de usuario/empleado`, Cal) %>%
    unique()

  .data <- .dataPO %>%
    select(Operación, `ID de usuario/empleado`,`Mostrar nombre`) %>%
    left_join(NineBox, .) %>%
    filter(!(Operación %>% is.na())) %>%
    group_by(`ID de usuario/empleado`, Operación, `Mostrar nombre`) %>%
    summarise(n = n()) %>%
    filter(n > 1) %>%
    left_join(.,NineBox, multiple = "all")

  return(.data)
}


