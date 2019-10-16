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


#' ---------------------------------------------------------------------------- @Parameters

horizon  <- "1979-2013"

vars     <- 1:19
varnames <- paste0("0", vars)
varnames <- substr(varnames, nchar(varnames) - 1, nchar(varnames))



#' ---------------------------------------------------------------------------- @Check

rasters <- list.files(
  path     = file.path(path_climate_data, horizon),
  pattern  = "\\.tif$"
)

rasters <- gsub("CHELSA_bio10_|\\.tif", "", rasters)

varnames <- varnames[-which(varnames %in% rasters)]

if (length(varnames) > 0) {

  vars <- as.numeric(varnames)


#' ---------------------------------------------------------------------------- @DownloadChelsaData

  wget_chelsa(
    vars      = vars,
    horizons  = horizon,
    path      = path_climate_data
  )
}
