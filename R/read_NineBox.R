#' read_NineBox
#'
#' @param .path file path where extracted 9Box reported is saved
#'
#' @return .data a standarized data frame
#' @import dplyr purrr stringr readxl
#' @export
#'
read_NineBox <- function(.path){
  .data <- .path %>%
    readxl::read_excel(.) %>%
    as.data.frame()

  .data <- .data %>%
    mutate(`ID de usuario/empleado`  = `ID del usuario evaluado en la calibraciÃ³n`) %>%
    mutate(Year   = `Fecha de inicio del ciclo` %>% lubridate::year()) %>%
    mutate(Pot    = `Nuevo valor` %>% stringr::str_split_i(., ',', 1)) %>%
    mutate(Des    = `Nuevo valor` %>% stringr::str_split_i(., ',', 2)) %>%
    mutate(Cal    = NineBoxCoordstoText(Pot, Des))

  return(.data)
}
