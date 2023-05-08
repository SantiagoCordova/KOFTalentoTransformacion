#' create_RFTalento
#'
#' @param .date Date of the reporte
#' @param .dataPO standrarized official PO with data from 9NineBOx
#'
#' @return .data standarized data frame for Talento Reporting Factory
#' @export
#'
create_RFTalento <- function(.date, .dataPONineBox){

  .data <- .dataPONineBox %>%
    select(Operación, `Área Funcional Consolida`, `Posición OH (Picklist Label)`,
           NC_Label, `Rango Antigüedad KOF`, `Rango Antigüedad Posición`, `Rango Edad`,
           Género,
           CurrentPlacement, LastPlacement,
           `ID de usuario/empleado`, `Mostrar nombre`,
           `ID Posición`, `Nombre de Posición (Label)`) %>%
    filter(Operación !=  "Coca Cola Company", `ID de usuario/empleado` != 0)  %>%
    rename(abbop = Operación)

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

  .data <-
    data %>%
    mutate(Date = .date) %>%
    select(Date, Operación, `Functional Area`,	`OH-FL`,
           `Contribution Level`,	`KOF Seniority Rank`,	`Rank Seniority Position`,
           `Age Range`,	`Gender`, `CurrentPlacement`, `LastPlacement`, `n`,
           `ID de usuario/empleado`, `Mostrar nombre`,
           `ID Posición`, `Nombre de Posición (Label)`)

  return(.data)

}



