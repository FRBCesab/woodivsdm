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



#' ---------------------------------------------------------------------------- @Check

rasters <- list.files(
  path     = file.path(path_climate_data, horizon),
  pattern  = "\\.tif$"
)

rasters <- gsub("CHELSA_bio10_|\\.tif", "", rasters)

cat("\n", emo::ji("check"), "Checking Chelsa rasters")

vars <- varnames[-which(varnames %in% rasters)]

if (length(vars) > 0) {

  vars <- as.numeric(vars)


#' ---------------------------------------------------------------------------- @DownloadChelsaData

  wget_chelsa(
    vars      = vars,
    horizons  = horizon,
    path      = path_climate_data
  )

  cat("\n", emo::ji("check"), "Downloading Chelsa rasters")

} else {

  cat("\n", emo::ji("check"), "Existing Chelsa rasters")
}

cat("\n")
