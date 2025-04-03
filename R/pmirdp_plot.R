#' Map of sampling locations
#' 
#' @export
pmirdp_make_map_sampling_locations <- function(file_plot = "figures/pdpmp_plot_1.pdf") {
  
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
  
  res <-
    dm_pmird |>
    dm::dm_zoom_to(samples) |>
    dm::pull_tbl() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("sampling_longitude", "sampling_latitude")),
        round, digits = 3
      )
    ) |>
    dplyr::filter(!duplicated(paste0(sampling_longitude, "_", sampling_latitude))) |>
    dplyr::filter(!is.na(sampling_longitude) & !is.na(sampling_longitude)) |>
    sf::st_as_sf(coords = c("sampling_longitude", "sampling_latitude"), crs = 4326)
  
  # disconnect
  RMariaDB::dbDisconnect(con)
  
  # world map
  world <- 
    rnaturalearth::ne_countries(scale = "small", returnclass = "sf") |>
    dplyr::filter(sovereignt != "Antarctica")
  
  # map of sites
  res_plot <- 
    ggplot(data = world) +
    geom_sf(data = world, colour = NA, fill = "lightgrey") +
    geom_sf(data = res) +
    coord_sf(ylim = c(-60, NA), expand = FALSE) +
    theme_bw() +
    labs(x = "Longitude", y = "Latitude")
  
  ggsave(
    file_plot,
    plot = res_plot,
    width = 5.5, height = 2.7, 
    dpi = 300,
    device = cairo_pdf
  )
  
  file_plot
  
}



#' Database schema
#' 
#' @export
pmirdp_make_plot_2 <- function(file_plot = "figures/pdpmp_plot_2.pdf") {
  
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
  
  # disconnect
  RMariaDB::dbDisconnect(con)
  
  dm_pmird |>
    dm::dm_draw(
      graph_name = "pmird database schema"
    )  |>
    DiagrammeRsvg::export_svg() |>
    charToRaw() |> 
    rsvg::rsvg_pdf(file_plot)
  
  file_plot
  
}



#' Results of MIRS quality check
#' 
#' @param x `pmirdp_mir_quality_results$mir_quality_results`
#' 
#' @export
pmirdp_make_plot_3 <- function(x, file_plot = "figures/pdpmp_plot_3.pdf") {
  
  mir_quality_config <- pmird::mir_quality_config
  
  # get data
  mir_quality_results <-
    x |>
    tidyr::pivot_longer(
      cols = dplyr::any_of(c("is_baseline_corrected", "intensity_1400", "mir_water_vapor_contribution_relative", "mir_co2_contribution_relative", "noise_level_relative")),
      names_to = "variable",
      values_to = "value",
      values_transform = list(value = as.numeric)
    ) |>
    dplyr::mutate(
      variable_pretty =
        dplyr::case_when(
          variable == "is_baseline_corrected" ~ "Is~the~spectrum~baseline~corrected*'?'",
          variable == "noise_level_relative" ~ "Relative~noise~level",
          variable == "mir_water_vapor_contribution_relative" ~ "Relative~contribution~of~water~vapor",
          variable == "mir_co2_contribution_relative" ~ "Relative~contribution~of~CO[2]",
          variable == "intensity_1400" ~ "Intensity~at~1400~cm^{-1}"
        )
    ) 
  
  # get points where to abline datasets
  mir_quality_results_abline_id_spectrum <-
    mir_quality_results |>
    dplyr::filter(variable == unique(variable)[[1]]) |>
    dplyr::filter(id_spectrum %in% c(id_spectrum[!duplicated(id_dataset)], max(id_spectrum))) |>
    dplyr::select(id_dataset, id_spectrum)
  
  # make plots
  p_mir_quality_results <- 
    purrr::map(unique(mir_quality_results$variable), function(i) {
      
      # general settings
      point_size <- 0.5
      point_color = "grey50"
      
      # get data
      res <- 
        mir_quality_results |>
        dplyr::filter(variable == i)
      
      # add information on uncertainties (where available)
      res <- 
        switch(
          i,
          "is_baseline_corrected" =,
          "intensity_1400" =,
          "noise_level_relative" = {
            res %>%
              dplyr::mutate(
                value_min = value,
                value_max = value
              )
          },
          "mir_water_vapor_contribution_relative" = {
            res %>%
              dplyr::mutate(
                value_min = value + mir_water_vapor_contribution_relative_sd,
                value_max = value - mir_water_vapor_contribution_relative_sd
              )
          },
          "mir_co2_contribution_relative" = {
            res %>%
              dplyr::mutate(
                value_min = value + mir_co2_contribution_relative_sd,
                value_max = value - mir_co2_contribution_relative_sd
              )
          }
        )
      
      # reformat data
      res <-
        switch(
          i,
          "is_baseline_corrected" = {
            res %>%
              dplyr::mutate(
                value =
                  dplyr::case_when(
                    value == 0 ~ "no",
                    value == 1 ~ "yes"
                  ) %>%
                  factor(levels = c("no", "yes"))
              )
          },
          "noise_level_relative" = {
            res %>%
              dplyr::mutate(
                value = log(value)
              )
          },
          res
        )
      
      # make plot
      p_res <-
        switch(
          i,
          "is_baseline_corrected" = {
            res |>
              ggplot(aes(x = id_spectrum, y = value, ymin = value_min, ymax = value_max)) +
              geom_vline(xintercept = mir_quality_results_abline_id_spectrum$id_spectrum, 
                         color = "lightgrey")
          },
          {
            res |>
              ggplot(aes(x = id_spectrum, y = value, ymin = value_min, ymax = value_max)) +
              geom_vline(xintercept = mir_quality_results_abline_id_spectrum$id_spectrum, 
                         color = "lightgrey")
          }
        )
      
      p_res <-
        p_res +
        theme_classic() +
        labs(
          title = label_parsed(unique(res$variable_pretty))[[1]][[1]], 
          y = element_blank(), 
          x = "Spectrum number"
        )
      
      p_res <-
        switch(
          i,
          "is_baseline_corrected" = {
            p_res +
              geom_point(color = point_color, size = point_size)
          },
          "noise_level_relative" = {
            p_res +
              geom_errorbar(color = "lightgrey", width = 0) +
              geom_point(color = point_color, size = point_size) +
              geom_smooth(aes(group = id_dataset), formula = y ~ 1, method = "lm", se = FALSE, color = "coral")
          },
          "intensity_1400" = {
            p_res +
              geom_errorbar(color = "lightgrey", width = 0) +
              geom_point(color = point_color, size = point_size) +
              geom_hline(yintercept = mir_quality_config$is_baseline_corrected_threshold, color = "grey") +
              geom_smooth(aes(group = id_dataset), formula = y ~ 1, method = "lm", se = FALSE, color = "coral") +
              scale_y_continuous(labels = function(x) x * 1000)
          },
          {
            p_res +
              geom_errorbar(color = "lightgrey", width = 0) +
              geom_point(color = point_color, size = point_size) +
              geom_hline(yintercept = 0, color = "grey") +
              geom_smooth(aes(group = id_dataset), formula = y ~ 1, method = "lm", se = FALSE, color = "coral")
          }
        )
      
      p_res
      
      
    })
  
  # compose plots
  res_plot <-
    patchwork::wrap_plots(p_mir_quality_results[-1]) +
    patchwork::plot_layout(ncol = 2L, nrow = 2L) + 
    patchwork::plot_annotation(
      tag_levels = tag_levels, 
      tag_prefix = tag_prefix, 
      tag_suffix = tag_suffix
    )
  
  ggsave(
    file_plot,
    plot = res_plot,
    width = 10, height = 5.5, 
    dpi = 300,
    device = cairo_pdf
  )
  
  file_plot
  
}


#' Plots of a sample spectrum with the wavenumber ranges highlighted that were used to define MIRS quality indices
#' 
#' @export
pmirdp_make_plot_4 <- function(file_plot = "figures/pdpmp_plot_4.pdf") {
  
  mir_quality_config <- pmird::mir_quality_config
  
  # collect ranges
  d_mir_quality_config_ranges <-
    tibble::tibble(
      variable = c("bc", "co2", "noise", "wv"),
      range = mir_quality_config[stringr::str_detect(names(mir_quality_config), pattern = "^range_") & !names(mir_quality_config) %in% "range_max_all_spectra"]
    ) %>% 
    tidyr::unnest(cols = "range") %>%
    dplyr::mutate(
      variable = factor(variable, levels = c("bc", "wv", "co2", "noise"))
    )
  
  # plot a random spectrum with the ranges highlighted
  p_mir_quality_config_ranges_spectrum_labels <-
    c("Is~a~spectrum~baseline~corrected*'?'", "Relative~contribution~of~water~vapor", "Relative~contribution~of~CO[2]", "Relative~contribution~of~noise") %>% purrr::map(function(x) label_parsed(x)[[1]][[1]])
  
  res_plot <-
    mir_quality_config$mir_co2_reference_spectrum_raw %>%
    dplyr::select(spectra) %>%
    tidyr::unnest(cols = "spectra") %>%
    ggplot() +
    geom_rect(
      data = d_mir_quality_config_ranges, 
      aes(xmin = start, xmax = end, fill = variable), 
      ymin = 0, ymax = 1.5
    ) +
    geom_path(aes(x = x, y = y)) +
    theme_classic() +
    labs(x = "Wavenumber (cm<sup>-1</sup>)", y = "Intensity (-)") +
    scale_fill_manual(
      values = scales::hue_pal()(4), 
      labels = p_mir_quality_config_ranges_spectrum_labels
    ) +
    guides(
      fill = 
        guide_legend(
          title = "Range for assessment of:", 
          title.position = "top",
          nrow = 2,
          byrow = TRUE
        )
    ) +
    theme(
      legend.position = "bottom",
      axis.title.x = ggtext::element_markdown()
    )
  
  ggsave(
    file_plot,
    plot = res_plot,
    width = 5.5, height = 3.5, 
    dpi = 300,
    device = cairo_pdf
  )
  
  file_plot
  
}



#' Plot of reference spectra used to estimate the relative contributions of water vapor and CO2 ###
#' 
#' @export
pmirdp_make_plot_5 <- function(file_plot = "figures/pdpmp_plot_5.pdf") {
  
  mir_quality_config <- pmird::mir_quality_config
  
  d_mir_quality_config_spectra <-
    tibble::tibble(
      variable = rep(c("co2", "wv"), each = 2L),
      is_raw = rep(c(FALSE, TRUE), times = 2L),
      spectra =
        mir_quality_config[stringr::str_detect(names(mir_quality_config), pattern = "(co2|water_vapor)_reference_spectrum")] |>
        purrr::map(function(.x) {
          .x |>
            dplyr::mutate(
              id_measurement = data.table::rleidv(id_measurement)
            ) |>
            dplyr::group_by(id_measurement) |>
            dplyr::summarise(
              sample_id = as.character(sample_id[[1]]),
              measurement_id = measurement_id[[1]],
              spectra = spectra[1],
              measurement_device_target = paste(unique(measurement_device_target), collapse = "; "),
              measurement_device_source = unique(measurement_device),
              .groups = "drop"
            ) |>
            ir::ir_as_ir() |>
            dplyr::mutate(
              measurement_device_target = as.factor(measurement_device_target)
            )
        }),
      title = 
        paste0(
          ifelse(is_raw, "Raw", "Preprocessed"),
          " reference spectrum for ",
          ifelse(variable == "co2", "CO<sub>2</sub>", "water vapor")
        )
    ) |>
    dplyr::arrange(dplyr::desc(is_raw), variable)
  
  res_plot <-
    purrr::map(seq_len(nrow(d_mir_quality_config_spectra)), function(i) {
      
      # get data
      res <- d_mir_quality_config_spectra[i, ]
      plot(res$spectra[[1]]) +
        geom_path(aes(x = x, y = y, color = as.character(measurement_device_source))) +
        theme_classic() +
        labs(
          y = "Absorbance (-)", 
          x = "Wavenumber (cm<sup>-1</sup>)", 
          title = res$title[[1]][[1]]
        ) +
        guides(color = guide_legend(title = "Device", ncol = 1L)) +
        theme(
          axis.title.x = ggtext::element_markdown(),
          plot.title = ggtext::element_markdown(),
          legend.position = "bottom"
        )
      
    }) |>
    patchwork::wrap_plots(ncol = 2L) +
    patchwork::plot_annotation(
      tag_levels = tag_levels, 
      tag_prefix = tag_prefix, 
      tag_suffix = tag_suffix
    ) + 
    patchwork::plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 12),
      legend.text = ggtext::element_markdown(size = 12)
    )
  
  ggsave(
    file_plot,
    plot = res_plot,
    width = 10, height = 7.5, 
    dpi = 300,
    device = cairo_pdf
  )
  
  file_plot
  
}

