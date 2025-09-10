#' @export
pmirdp_make_data_overview <- function() {
  
  # connect to database
  con <-
    RMariaDB::dbConnect(
      drv = RMariaDB::MariaDB(),
      dbname = "pmird",
      default.file = "~/my.cnf",
      groups = "rs-dbi"
    )
  
  dm_pmird <-
    pmird::pm_get_dm(con, learn_keys = TRUE)
  
  # general dataset characteristics
  res <-
    dm_pmird %>%
    dm::dm_zoom_to(datasets) %>%
    dm::left_join(samples, by = "id_dataset") %>%
    dm::left_join(data_to_samples, by = "id_sample") %>%
    dm::left_join(data, by = "id_measurement") %>%
    dm::left_join(mir_metadata, by = "id_measurement") %>%
    dm::left_join(macrofossils, by = "id_measurement") %>%
    dm::pull_tbl() %>%
    tibble::as_tibble()
  
  # disconnect
  RMariaDB::dbDisconnect(con)
  
  res
  
}


#' @param x pmirdp_data_overview
#' @export
pmirdp_make_data_overview_2 <- function(x) {
  
  target_peat_property <- c(colnames(pmird::db_template_tables$data), "id_macrofossil_type")
  target_peat_property <- setdiff(target_peat_property, c(target_peat_property[stringr::str_detect(target_peat_property, pattern = "_err$")], c("comments_measurements", "id_measurement", "mirs_file")))
  
  res <- 
    x |>
    dplyr::group_by(id_dataset) %>%
    dplyr::summarise(
      coordinates =
        if(all(is.na(sampling_latitude))) {
          ""
        } else {
          paste0(
            "South: ",
            sampling_latitude |>
              min(na.rm = TRUE) |>
              round(3), 
            "$^\\circ$N, ",
            "North: ",
            sampling_latitude |>
              max(na.rm = TRUE) |>
              round(3), 
            "$^\\circ$N, ",
            "West: ",
            sampling_longitude |>
              min(na.rm = TRUE) |>
              round(3),  
            "$^\\circ$E, ",
            "East: ",
            sampling_longitude |>
              max(na.rm = TRUE) |>
              round(3),  
            "$^\\circ$E"
          ) 
        },
      dates =
        if(all(is.na(sampling_date))) {
          ""
        } else {
          range(sampling_date, na.rm = TRUE) |>
            unique() |>
            paste(collapse = " to ") 
        },
      no_samples = 
        id_sample |>
        unique() |>
        length(),
      no_mirs =
        mirs_file |>
        unique() |>
        na.omit() |>
        length(),
      peat_properties =
        dplyr::across(
          dplyr::any_of(target_peat_property),
          function(x) ! all(is.na(x))
        ),
      mirs_mode =
        if(all(is.na(mir_mode))) {
          "-"
        } else {
          dplyr::case_when(
            mir_mode == "atr_ftir" ~ "ATR-FTIR",
            mir_mode == "absorbance_ftir" ~ "Absorbance-FTIR",
            TRUE ~ NA_character_
          ) |>
            na.omit() |>
            unique() |>
            paste(collapse = ", ")
        },
      references = unique(reference_publication), 
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      peat_properties =
        purrr::map_chr(seq_len(nrow(peat_properties)), function(i) {
          .col_names <- colnames(peat_properties)[unlist(peat_properties[i, ])]
          index <- purrr::map_lgl(.col_names, PeriodicTable::isSymb) & ! .col_names %in% c("C", "N", "H", "O", "P", "S")
          .col_names <- c(.col_names[! index], "trace elements")
          paste(.col_names, collapse = ", ")
        }) |>
        stringr::str_replace(pattern = "id_macrofossil_type", replacement = "macrofossils") %>%
        stringr::str_replace_all(pattern = "\\_", replacement = "\\\\_"),
      references =
        {
          purrr::map(references, function(x) {
            tmp_file <- tempfile("temp_bib", tmpdir = tempdir(), fileext = ".bib")
            x |> writeLines(con = tmp_file)
            #res <-
            tmp_file |>
              bib2df::bib2df() |>
              dplyr::pull(BIBTEXKEY)
            #res <-
            #  paste0("\\cite{", paste(res, collapse = ", "), "}")
          })
        }
    )
  
  # make references for the text
  res_ref <- 
    purrr::map(res$references, function(.x) {
      if(length(.x) == 0) {
        .x
      } else {
        paste0("(ref:", .x |> stringr::str_replace(pattern = "\\.", replacement = "") |> paste(collapse = ""), ") $~$[@", .x |> paste(collapse = "; @"), "]") #---note: The '$~$' is required for rmarkdown to correctly recognize numeric references to tables
      }
    }) |>
    unlist() |>
    unique() |>
    paste(collapse = "  \n")
  
  res <- 
    res |>
    dplyr::mutate(
      references =
        purrr::map_chr(references, function(.x) {
          if(length(.x) == 0) {
            ""
          } else {
            paste0("(ref:", .x |> stringr::str_replace(pattern = "\\.", replacement = "") |> paste(collapse = ""), ")") |>
              paste(collapse = ", ")
          }
        })
    )
  
  list(
    tab = res,
    rmarkdown_references = res_ref
  )
  
  
  
}