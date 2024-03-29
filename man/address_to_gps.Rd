% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_weather_forecast.R
\name{address_to_gps}
\alias{address_to_gps}
\title{Convert Address to GPS Coordinates}
\usage{
address_to_gps(adresse)
}
\arguments{
\item{adresse}{A character string specifying the address to be converted.}
}
\value{
A numeric vector containing the latitude and longitude coordinates.
}
\description{
This function converts a given address into GPS coordinates using the OpenStreetMap geocoding service.
}
\examples{
address_to_gps("1600 Amphitheatre Parkway, Mountain View, CA")

}
