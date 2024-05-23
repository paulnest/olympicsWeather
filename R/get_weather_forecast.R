#' perform_request
#'
#' @description This function is designed to create a tibble by making a request to an API.
#'
#' @param lat numeric Latitude coordinate.
#' @param lon numeric Longitude coordinate.
#'
#' @return A tibble with weather forecast information.
#'
#' @examples
#' perform_request(48.85, 2.35)
#'
#' @details This function takes latitude and longitude coordinates as input, constructs a URL to query a weather forecast API, and retrieves weather forecast data in the form of a tibble.
#'
#' @importFrom httr2 request
#' @importFrom httr2 req_url_query
#' @importFrom httr2 req_perform
#' @importFrom httr2 resp_body_json
#' @importFrom tibble as_tibble
#'
#' @export
perform_request <- function(lat, lon) {
  api_url <- "https://api.open-meteo.com/v1/forecast"
  params <- list(
    latitude = lat,
    longitude = lon,
    hourly = c(
      "temperature_2m",
      "apparent_temperature", "precipitation_probability",
      "precipitation"
    )
  )
  req <- request(api_url) |>
    req_url_query(!!!params, .multi = "comma") |>
    req_perform() |>
    resp_body_json()
  
  as_tibble(req)
}

#' unnest_response
#'
#' @description This function is designed to flatten weather API response data into a tibble.
#'
#' @param resp Weather API response, typically in the form of a list.
#'
#' @return A tibble with columns for date and time, temperature in Celsius, apparent temperature in Celsius, precipitation probability, and precipitation amount.
#'
#' @examples
#' response <- list(
#'   time = c("2024-05-23T00:00:00Z", "2024-05-23T01:00:00Z"),
#'   temperature_2m = c(15.0, 14.5),
#'   apparent_temperature = c(15.0, 14.0),
#'   precipitation_probability = c(0.1, 0.2),
#'   precipitation = c(0.0, 0.1)
#' )
#' unnest_response(response)
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
    date_heure = unlist(resp$time),
    temperature_celsius = unlist(resp$temperature_2m),
    temperature_ressentie_celsius = unlist(resp$apparent_temperature),
    precipation_proba = unlist(resp$precipitation_probability),
    precipitation = unlist(resp$precipitation)
  )
}

#' Convert Address to GPS Coordinates
#'
#' This function converts a given address into GPS coordinates using the OpenStreetMap geocoding service.
#'
#' @param adresse A character string specifying the address to be converted.
#' @param lat numeric Latitude coordinate.
#' @param lon numeric Longitude coordinate.
#'
#' @return A numeric vector containing the latitude and longitude coordinates.
#'
#' @import tibble
#' @import tidygeocoder
#' @export
address_to_gps <- function(adresse, lat, long) {
  lieu <- tibble(
    adresse
  ) |>
    geocode(address = adresse, method = "osm") |>
    dplyr::select(lat, long)
  
  unlist(lieu)
}

#' Prévisions Météo
#'
#' @description Cette fonction permet d'obtenir les prévisions météo en temps réel d'un lieu pour chaque heure des 7 prochains jours. Voici les 4 renseignements obtenus : la "temperature_celsius", la "temperature_ressentie_celsius", la "precipation_proba" et les "precipitation".
#'
#' @param add_ou_coord Adresse littérale ou coordonnées géographiques du lieu souhaité
#'
#' @return un tibble contenant la météo horaire de l'endroit en question des 7 prochains jours
#'
#' @export
#'
#' @importFrom dplyr select
#' @importFrom tidygeocoder geocode
#' @importFrom tibble tibble
#' @importFrom httr2 resp_body_json req_perform req_url_query request
#'
#' @examples
#' get_forecast("Nantes")
#'
#' @examples
#' coord = c(47.218371, -1.553621)
#' get_forecast(coord)
#'
#' @rdname get_forecast
get_forecast = function(add_ou_coord){
  UseMethod("get_forecast")
}

#' @export
get_forecast.numeric = function(xy){
  if (is.numeric(xy) && length(xy) == 2) {
    météo_tibble = unnest_response(perform_request(xy[1],xy[2]))
    print(météo_tibble)
  } else {
    stop("L'argument xy doit être un vecteur numérique de taille 2.")
  }
}

#' @export
get_forecast.character = function(address){
  if (is.character(address) && length(address) == 1){
    get_forecast.numeric(address_to_gps(address))
  } else {
    stop("L'argument address doit être une chaîne de caractères de taille 1.")
  }
}
