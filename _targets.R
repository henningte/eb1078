# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(magrittr)
library(quantities)
library(ir)
library(crew)
library(ggplot2)

crew_sequential <-
  crew::crew_controller_local(
    name = "crew_sequential",
    workers = 1L,
    tasks_max = 1L
  )

crew_parallel_1 <-
  crew::crew_controller_local(
    name = "crew_parallel_1",
    workers = 4L
  )

# Set target options:
tar_option_set(
  packages = c("tibble", "ggplot2"),
  format = "rds",
  controller = crew_controller_group(crew_sequential, crew_parallel_1)
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# general plotting options
tag_levels <- 'a' 
tag_prefix <- '(' 
tag_suffix <- ')'

# Replace the target list below with your own:
list(
  tar_target(
    pmirdp_mir_quality_results_file,
    command = "data/raw_data/mir_quality_results.rds",
    format = "file"
  ),
  tar_target(
    pmirdp_mir_quality_results,
    command = readRDS(pmirdp_mir_quality_results_file)
  ),
  tar_target(
    pmirdp_plot_1,
    command = pmirdp_make_map_sampling_locations(file_plot = "figures/pdpmp_plot_1.pdf"),
    format = "file"
  ),
  tar_target(
    pmirdp_plot_2,
    command = pmirdp_make_plot_2(file_plot = "figures/pdpmp_plot_2.pdf"),
    format = "file"
  ),
  tar_target(
    pmirdp_plot_3,
    command = 
      pmirdp_make_plot_3(
        x = pmirdp_mir_quality_results$mir_quality_results, 
        file_plot = "figures/pdpmp_plot_3.pdf"
      ),
    format = "file"
  ),
  tar_target(
    pmirdp_plot_4,
    command = pmirdp_make_plot_4(file_plot = "figures/pdpmp_plot_4.pdf"),
    format = "file"
  ),
  tar_target(
    pmirdp_plot_5,
    command = pmirdp_make_plot_5(file_plot = "figures/pdpmp_plot_5.pdf"),
    format = "file"
  ),
  tar_target(
    pmirdp_data_overview,
    command = pmirdp_make_data_overview()
  ),
  tar_target(
    pmirdp_data_overview_2,
    command = pmirdp_make_data_overview_2(x = pmirdp_data_overview)
  ),
  tar_render(
    pmirdp_paper,
    path = "pmird-paper.Rmd"
  )
)
