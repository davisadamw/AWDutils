
excel_col_lookup <- function(colname) {

  letters <- tidyr::crossing(l1 = c("", LETTERS[1:4]),
                             l2 = LETTERS) %>%
    dplyr::mutate(col   = paste0(.data$l1, .data$l2),
                  colno = dplyr::row_number(),
                  .keep = "unused")
  letters %>% dplyr::filter(col == colname) %>% dplyr::pull(.data$colno)
}
