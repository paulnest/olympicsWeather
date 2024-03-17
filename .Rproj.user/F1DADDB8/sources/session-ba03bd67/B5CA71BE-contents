library(dplyr)
library(httr)
library(httr2)
library(jsonlite)
library(tibble)
library(tidygeocoder)
library(tidyverse)
library(opencage)

#' perform_request
#'
#' @description This function is designed to create a tibble by making a request to an API.
#'
#' @param lat numeric Latitude coordinate.
#' @param lon numeric Longitude coordinate.
#'
#' @return A tibble with 5 rows containing weather forecast information.
#'
#' @examples
#' perform_request(48.85, 2.35)
#'
#' @details This function takes latitude and longitude coordinates as input, constructs a URL to query a weather forecast API, and retrieves weather forecast data in the form of a tibble with 5 rows.
#'
#' The URL used for the API request is "https://api.open-meteo.com/v1/forecast". The function then sends a request to this URL with specified query parameters, including latitude, longitude, and requested hourly weather forecast data such as temperature, apparent temperature, precipitation probability, and precipitation amount.
#'
#' The function returns the retrieved weather forecast data as a tibble.
#'
#' @seealso [request()], [req_url_query()], [req_perform()], [resp_body_json()], [as_tibble()]
#'
#' @importFrom httr request
#' @importFrom httr req_url_query
#' @importFrom httr req_perform
#' @importFrom httr resp_body_json
#' @importFrom tibble as_tibble
#'
#' @export

perform_request <- function(lat, lon) {
  url <- "https://api.open-meteo.com/v1/forecast"
  response_table <-
    request(url) |>
    req_url_query(latitude = lat, longitude = lon,
                hourly = c("temperature_2m",
                           "apparent_temperature",
                           "precipitation_probability",
                           "precipitation"),
                .multi="comma") |>
    req_perform() |>
    resp_body_json() |>
    as_tibble()
  return(response_table)
}

test001 <- perform_request(48.85, 2.35)


#' unnest_response
#'
#' @description This function is designed to flatten weather API response data into a tibble.
#'
#' @param resp Weather API response, typically in the form of a list.
#'
#' @return A tibble with columns for date and time, temperature in Celsius, apparent temperature in Celsius, precipitation probability, and precipitation amount.
#'
#' @examples
#' unnest_response(response_table)
#'
#' @details This function takes as input the weather API response, typically in the form of a list, and extracts relevant data to flatten into a tibble with the following columns:
#'   - date_time: Date and time of the weather forecast.
#'   - temperature_celsius: Temperature in Celsius.
#'   - apparent_temperature_celsius: Apparent temperature in Celsius.
#'   - precipation_probability: Probability of precipitation.
#'   - precipitation: Amount of precipitation.
#'
#' The function then returns this flattened tibble with the data.
#'
#' @seealso [as_tibble()]
#'
#' @importFrom tibble tibble
#'
#' @export

unnest_response <- function(resp) {
  tibble(
    date_heure = unlist(resp$hourly[1][[1]]),
    temperature_celsius = unlist(resp$hourly[2][[1]]),
    temperature_ressentie_celsius = unlist(resp$hourly[3][[1]]),
    precipation_proba = unlist(resp$hourly[4][[1]]),
    precipitation = unlist(resp$hourly[5][[1]])
  )
}

#' Convert Address to GPS Coordinates
#'
#' This function converts a given address into GPS coordinates using the OpenStreetMap geocoding service.
#'
#' @param adresse A character string specifying the address to be converted.
#'
#' @return A numeric vector containing the latitude and longitude coordinates.
#'
#' @import tibble
#' @import opencage
#'
#' @examples
#' address_to_gps("1600 Amphitheatre Parkway, Mountain View, CA")
#'
#' @export

address_to_gps <- function(adresse){
  lieu = tibble(
    adresse) |>
    geocode(address = adresse, method = "osm") |>
    select(lat, long)

  unlist(lieu)
}


#' Get Weather Forecast
#'
#' This function retrieves the weather forecast based on either coordinates (latitude and longitude) or an address.
#'
#' @param add_ou_coord A numeric vector of length 2 representing coordinates (latitude and longitude) or a character string representing an address.
#'
#' @return A tibble containing the weather forecast.
#'
#' @examples
#' get_forecast(c(48.85, 2.35)) # Using coordinates
#' get_forecast("1600 Amphitheatre Parkway, Mountain View, CA") # Using address
#'
#' @export

get_forecast <- function(add_ou_coord) {
  UseMethod("get_forecast")
}

#' @export
get_forecast.numeric <- function(xy) {
  if (is.numeric(xy) && length(xy) == 2) {
    météo_tibble <- unnest_response(perform_request(xy[1], xy[2]))
    print(météo_tibble)
  } else {
    stop("The xy argument must be a numeric vector of length 2.")
  }
}

#' @export
get_forecast.character <- function(address) {
  if (is.character(address) && length(address) == 1) {
    get_forecast.numeric(address_to_gps(address))
  } else {
    stop("The address argument must be a character string of length 1.")
  }
}









