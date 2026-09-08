library(shiny)
library(leaflet)
library(ncdf4)
library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(rsconnect)

rsconnect::writeManifest()

netcdf_root <- "NetCDF"

variables <- c("R99p")

periods <- as.character(
  seq(
    2030,
    2090,
    by = 5
  )
)
