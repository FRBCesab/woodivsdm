#' @title Download Chelsa climate layers
#'
#' @description
#' This R script downloads climate normals from the Chelsa database
#'   for 19 bioclimatic variables for the period 1979-2013. See
#'   \url{http://chelsa-climate.org/} for further informations.
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 15/10/2019
#'



#' ---------------------------------------------------------------------------- @LoadAddings

source(file.path("R", "listChelsaCmip5.R"))
source(file.path("R", "wgetChelsa.R"))


#' ---------------------------------------------------------------------------- @DownloadChelsaData

wget_chelsa(
  vars      = 1:19,
  horizons  = "1979-2013",
  path      = path_climate_data
)
