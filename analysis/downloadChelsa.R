#' Download Chelsa Climate Layers
#'
#' This R script downloads climate normals from the Chelsa database
#' for 19 bioclimatic variables for the period 1979-2013. See
#' \url{http://chelsa-climate.org/} for further informations.
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#' @date 2020/03/02



## Check if rasters are already downloaded ----

cat("\n", emo::ji("check"), "Checking Chelsa rasters")

rasters <- list.files(
  path     = file.path(path_climate_data, horizon),
  pattern  = "\\.tif$"
)

if (length(rasters)) {

  rasters <- gsub("CHELSA_bio10_|\\.tif", "", rasters)

  vars <- varnames[-which(varnames %in% rasters)]

} else {

  vars <- varnames
}


## Download missing Chelsa rasters ----

if (length(vars)) {

  vars <- as.numeric(vars)

  cat("\n")
  
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
