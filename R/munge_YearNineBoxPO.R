#' munge_YearNineBoxPO
#'
#' @param .data9BOX standarized 9box data frame
#' @param .dataPO   standarized HC data frame
#' @param .year     year of 9Box evaulation
#'
#' @return .data standarized 9Box data frame with users id that exsits on PO for .year evaulation and .year-1 evaluation
#' @export
#'
munge_YearNineBoxPO <- function(.data9BOX, .dataPO, .year){

  # .year = CurrentPlacement
  .yeardata <- .data9BOX %>%
    filter(Year == .year) %>%
    select(`ID de usuario/empleado`, Cal) %>%
    unique()
  .dataYear <- .dataPO %>%
    select(Operación, `ID de usuario/empleado`,`Mostrar nombre`) %>%
    left_join(.yeardata, .) %>%
    filter(!(Operación %>% is.na())) %>%
    group_by(`ID de usuario/empleado`, Operación, `Mostrar nombre`) %>%
    summarise(CurrentPlacement = min(Cal))

  # .year - 1 = LastPlacement
  .yearMonedata <- .data9BOX %>%
    filter(Year == .year - 1) %>%
    select(`ID de usuario/empleado`, Cal) %>%
    unique()
  .dataYearMone <- .dataPO %>%
    select(Operación, `ID de usuario/empleado`,`Mostrar nombre`) %>%
    left_join(.yearMonedata, .) %>%
    filter(!(Operación %>% is.na())) %>%
    group_by(`ID de usuario/empleado`, Operación, `Mostrar nombre`) %>%
    summarise(LastPlacement = min(Cal))

  # Full merge
  .data <- .dataYear %>% full_join(., .dataYearMone)


  return(.data)
}
