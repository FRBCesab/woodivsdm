#' @title Run project
#'
#' @description
#' Run project.
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 15/10/2019
#'



#' ---------------------------------------------------------------------------- @LoadAddings

source(file.path("R", "listChelsaCmip5.R"))
source(file.path("R", "wgetChelsa.R"))


#' ----------------------------------------------------------------------------- @ProjectSetup

source(file.path("analysis", "projectSetup.R"))


#' ----------------------------------------------------------------------------- @DownloadClimateData

source(file.path("analysis", "downloadChelsa.R"))


#' ----------------------------------------------------------------------------- @CleanClimateData

source(file.path("analysis", "cleanChelsa.R"))
