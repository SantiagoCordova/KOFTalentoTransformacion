#' create_RFPerformance
#'
#' @param .date date of report
#' @param .dataPOPerformance standarized performance po
#'
#' @return RF Performance data
#' @export
#'
create_RFPerformance <- function(.date, .dataPOPerformance){
  .years <- paste('Performance', .date %>% lubridate::year() - c(0,1), sep = '_')

  .data <- .dataPOPerformance %>%
    select(Operación, `Área Funcional Consolida`, `Posición OH (Picklist Label)`,
           NC_Label, `Rango Antigüedad KOF`, `Rango Antigüedad Posición`, `Rango Edad`,
           Género,
           .years,
           `ID de usuario/empleado`, `Mostrar nombre`,
           `ID Posición`, `Nombre de Posición (Label)`) %>%
    filter(Operación !=  "Coca Cola Company", `ID de usuario/empleado` != 0)  %>%
    rename(abbop = Operación, CurrentPerfomance = .years[1], LastPerformance = .years[2])

  .data <- .data  %>%
    left_join(., operacion.abb) %>%
    left_join(., areafuncon.trad)  %>%
    left_join(., posicionoh.trad)  %>%
    left_join(., nclabel.trad)  %>%
    left_join(., rangAntKof.trad)  %>%
    left_join(., rangAntPos.trad)  %>%
    left_join(., rangEdad.trad)  %>%
    left_join(., genero.trad)  %>%
    mutate(n = 1)

  .data <- .data %>%
    filter(Operación != 'Venezuela') %>%
    mutate(Operación = 'KOF') %>%
    rbind(., .data)

  .data <- .data %>%
    mutate(Date = .date) %>%
    select(Date, Operación, `Functional Area`,	`OH-FL`,
           `Contribution Level`,	`KOF Seniority Rank`,	`Rank Seniority Position`,
           `Age Range`,	`Gender`, `CurrentPerfomance`, `LastPerformance`, `n`,
           `ID de usuario/empleado`, `Mostrar nombre`,
           `ID Posición`, `Nombre de Posición (Label)`)
  return(.data)

}
