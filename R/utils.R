# utils.R

#' Charger les packages nécessaires
#'
#' Cette fonction charge les packages nécessaires pour le package olympicsWeather.
#'
#' @export
charger_packages <- function() {
  library(dplyr)
  library(httr2)
  library(jsonlite)
  library(tibble)
  library(tidygeocoder)
  library(tidyverse)
  library(opencage)
}
