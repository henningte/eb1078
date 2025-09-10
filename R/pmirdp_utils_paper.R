#' Helper function that prepares the author list based on `author_list_edited.csv`
#' 
#' @param check_affiliation Logical value. If `TRUE`, checks whether persons who
#' want to be co-author are dropped because their affiliation information is 
#' missing.
#' 
#' @export
pmirdp_prepare_author_list_affiliations <- function(author_list_edited, check_affiliation = TRUE) {
  
  res1 <- 
    author_list_edited |>
    dplyr::filter(wants_to_become_coauthor) |>
    dplyr::mutate(
      id_author_order = seq_along(sur_name)
    ) |>
    tidyr::pivot_longer(
      dplyr::all_of(c("affiliation_1", "affiliation_2")),
      names_to = "variable",
      values_to = "affiliation"
    ) |>
    dplyr::select(-variable)
  
  res2 <- 
    res1 |>
    dplyr::filter(affiliation != "")
  
  if(check_affiliation && ! all(paste0(res1$given_name, "_", res1$sur_name) %in% paste0(res2$given_name, "_", res2$sur_name))) {
    stop("Some co-authors have no affiliation!")
  }
  
  res_affiliations <- 
    res2 |>
    dplyr::filter(! duplicated(affiliation)) |>
    dplyr::select(affiliation) |>
    dplyr::mutate(
      id_affiliation = seq_along(affiliation)
    )
  
  res2 <- 
    res2 |>
    dplyr::left_join(
      res_affiliations,
      by = "affiliation"
    )
  
  res2 |>
    dplyr::mutate(
      name = paste0(given_name, " ", sur_name)
    ) |>
    dplyr::select(id_author_order, given_name, sur_name, name, affiliation, id_affiliation, email_public)
  
}

#' Helper function that makes the author list to paste into the R Markdown yaml
#' 
#' `cat()` the return value and paste this into the yaml.
#' 
#' @param x Value of `pmirdp_prepare_author_list_affiliations()`.
#' 
#' @param do_add_email Logical value. If `TRUE`, email addresses will be added 
#' to the output.
#' 
#' @export
pmirdp_make_author_list_yaml <- function(x, do_add_email = FALSE) {
  
  res <- 
    x |>
    dplyr::group_by(id_author_order, name) |>
    dplyr::summarise(
      affil = paste(id_affiliation, collapse = ","),
      email = unique(email_public),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      affil =
        dplyr::case_when(
          name == "Henning Teickner" ~ paste0(affil, ",*"),
          TRUE ~ affil
        )
    )
  
  if(! do_add_email) {
    res$email <- ""
  }
  
  purrr::map_chr(seq_len(nrow(res)), function(i) {
    paste0("  - name: ", res$name[[i]], "\n    affil: \"", res$affil[[i]], "\"\n    email:", res$email[[i]], "\n")
  }) |>
    paste(collapse = "")
  
}


#' Helper function that makes the affiliation list to paste into the R Markdown yaml
#' 
#' `cat()` the return value and paste this into the yaml.
#' 
#' @param x Value of `pmirdp_prepare_author_list_affiliations()`.
#' 
#' @export
pmirdp_make_affiliation_list_yaml <- function(x) {
  
  res <- 
    x |>
    dplyr::filter(! duplicated(id_affiliation)) |>
    dplyr::arrange(id_affiliation)
  
  purrr::map_chr(seq_len(nrow(res)), function(i) {
    paste0("  - number: \"", res$id_affiliation[[i]], "\"\n    text: \"", res$affiliation[[i]], "\"\n")
  }) |>
    paste(collapse = "") |>
    paste0("  - number: \"*\"\n    text: \"corresponding author(s): Henning Teickner (henning.teickner@uni-muenster.de)\"\n")
  
}


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



#' Makes a bibtex string of author names
#' 
#' @param x A data frame as returned by `pmirdp_prepare_author_list_affiliations()`.
#' 
#' @export
pmirdp_make_author_list_bibtex <- function(x) {
  
  x |>
    dplyr::filter(! duplicated(name)) |>
    dplyr::mutate(
      name = paste0(sur_name, ", ", given_name)
    ) |>
    dplyr::pull(name) |>
    paste(collapse = " and ")
  
}



#' Gets public emails of co-authors
#' 
#' @export
pmirdp_get_author_public_emails <- function(x) {
  
  x |>
    dplyr::filter(! duplicated(name)) |>
    dplyr::pull(email_public)
  
}

