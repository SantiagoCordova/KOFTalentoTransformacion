#' NineBoxCoordstoText
#'
#' @param .x Potential   0,1,2 grade
#' @param .y Performance 0,1,2 grade
#'
#' @return a labeled grade for nine box
#' @export
#'
NineBoxCoordstoText <- function(.x, .y){
  case_when(
    .x == 0 & .y == 0 ~ 1,
    .x == 0 & .y == 1 ~ 2,
    .x == 0 & .y == 2 ~ 4,
    .x == 1 & .y == 0 ~ 3,
    .x == 1 & .y == 1 ~ 5,
    .x == 1 & .y == 2 ~ 7,
    .x == 2 & .y == 0 ~ 6,
    .x == 2 & .y == 1 ~ 8,
    .x == 2 & .y == 2 ~ 9
  )
}
