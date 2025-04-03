#### Helper functions for the paper ####

#' Formats author list
#' 
#' @export
pmirdp_make_author_list <- function(x) {
  
  purrr::map_chr(x, function(.x) {
    paste0(.x$name, "$^{\\text{", .x$affil, "}}$")
  }) |>
    paste(collapse = "  \n")
  
}

#' Formats affiliation list
#' 
#' @export
pmirdp_make_affiliation_list <- function(x) {
  
  purrr::map_chr(x, function(.x) {
    paste0("$^{", .x$number, "}$ ", .x$text)
  }) |>
    paste(collapse = "  \n")
  
}