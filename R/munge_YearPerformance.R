#' munge_YearPerformance
#'
#' @param .data Performance data from read_Performance.R
#' @param .year Year of interest
#'
#' @return Format wide datframe where each column is a year for grades and multiples
#' @export
#'
munge_YearPerformance <- function(.data, .year){
  .data <- .data %>%
    filter(Year >= .year-3)  %>%
    unique() %>%
    group_by(`ID de usuario/empleado`, Year) %>%
    summarise(Multiple = min(Multiple), Rating = min(Rating)) %>%
    ungroup()

  .data_Perf <-
    .data %>%
    select(-'Multiple') %>%
    mutate(Year = paste('Performance', Year, sep = '_')) %>%
    pivot_wider(., names_from = Year, values_from = 'Rating', values_fill = NA)
  .data_Mult <-
    .data %>%
    select(-'Rating') %>%
    mutate(Year = paste('Mult', Year, sep = '_')) %>%
    pivot_wider(., names_from = Year, values_from = 'Multiple', values_fill = NA)

  .data <- left_join(.data_Perf, .data_Mult)



  return(.data)
}
