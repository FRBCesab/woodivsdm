#' @title Run project
#'
#' @description
#' Run project.
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 15/10/2019
#'


#' ----------------------------------------------------------------------------- @ProjectSetup

source(file.path("analysis", "projectSetup.R"))


#' ----------------------------------------------------------------------------- @DownloadClimateData

source(file.path("analysis", "downloadChelsa.R"))


#' ----------------------------------------------------------------------------- @CleanClimateData

source(file.path("analysis", "cleanChelsa.R"))


#' ----------------------------------------------------------------------------- @AddCountryToCell

source(file.path("analysis", "addCountry2Cell.R"))


#' ----------------------------------------------------------------------------- @CleanSpeciesData

source(file.path("analysis", "cleanSpecies.R"))

#' ----------------------------------------------------------------------------- @RunBiomod

source(file.path("analysis", "runBiomod.R"))

#' ----------------------------------------------------------------------------- @ExtractResults

source(file.path("analysis", "plotResults.R"))
