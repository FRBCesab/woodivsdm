#' @title Setup project
#'
#' @description
#' ...
#'   ...
#'   ...
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 15/10/2019
#'




rm(list = ls())

compt <- Sys.time()

cat("\n=======================================\n")

#' ----------------------------------------------------------------------------- @InstallCranLibs

cran_packages <- c(
  "devtools",
  "sf",
  "biomod2",
  "stringr"
)

n_i_p <- cran_packages[!(cran_packages %in% installed.packages())]
lapply(n_i_p, install.packages, dependencies = TRUE)


#' ----------------------------------------------------------------------------- @InstallDevLibs

if (!("emo" %in% installed.packages())) devtools::install_github("hadley/emo")


#' ----------------------------------------------------------------------------- @LoadLibs

i_p <- unlist(lapply(cran_packages, require, character.only = TRUE, quietly = TRUE))

if (sum(i_p) == length(cran_packages)) {

  cat("\n", emo::ji("check"), "Loading packages")

} else {

  cat("\n", emo::ji("warning"), " Some packages failed to load !")

}


#' ----------------------------------------------------------------------------- @LoadRFunctions

rfun <- list.files(
  path        = "R",
  pattern     = "\\.R$",
  full.names  = TRUE
)

rfun <- unlist(lapply(rfun, source, verbose = FALSE))



#' ----------------------------------------------------------------------------- @CreateFolders


dir_names <- c(
  file.path("data", "climate"),
  file.path("data", "gadm"),
  file.path("data", "boundary"),
  file.path("data", "species"),
  file.path("output"),
  file.path("output", "biomod")
)

dir_vars <- c(
  "path_climate_data",
  "path_gadm_data",
  "path_boundary_data",
  "path_species_data",
  "output",
  "path_biomod"
)

sapply(1:length(dir_names), function(i) {

  dir.create(
    path          = dir_names[i],
    showWarnings  = FALSE,
    recursive     = TRUE
  )

  assign(
    x      = dir_vars[i],
    value  = dir_names[i],
    envir  = .GlobalEnv
  )
})

cat("\n", emo::ji("check"), "Creating directories")

rm(list = c("dir_names", "dir_vars", "cran_packages", "n_i_p", "i_p", "rfun"))



#' ---------------------------------------------------------------------------- @GeneralParameters

cat("\n", emo::ji("check"), "Defining user parameters")

lambert  <- "+init=epsg:3035"

horizon  <- "1979-2013"

varnames <- c(4, 6, 12, 14)
varnames <- paste0("0", varnames)
varnames <- substr(varnames, nchar(varnames) - 1, nchar(varnames))


spnames  <- c(
  "AALB",  # montagneuse
  "ABOR",  # east
  "MCOM",  # costal
  "APIN",  # restricted west
  "CGRA",  # restricted west
  "QCOC",  # italy
  "QPUB",  # center + east
  "AUNE",  # widespread
  "CBET",  # east
  "CCOG",  # Italy
  "AMON"   # widespread
)


#' ---------------------------------------------------------------------------- @BiomodAlgorithmsOptions

cat("\n", emo::ji("check"), "Defining biomod2 algorithms parameters")

bm.opt <- biomod2::BIOMOD_ModelingOptions(
  GLM = list(
    type              = "quadratic",
    interaction.level = 0,
    test              = "AIC"
  ),
  GBM = list(
    n.trees = 5000
  ),
  GAM = list(
    k = 3
  )
)


#' ---------------------------------------------------------------------------- @BiomodGeneralOptions

cat("\n", emo::ji("check"), "Defining biomod2 general parameters")

mod.models           <- c("RF")
mod.n.rep            <-  1
mod.data.split       <- 80
mod.var.import       <-  0
mod.models.eval.meth <- "TSS"
prevalence           <-  0.5


#' ---------------------------------------------------------------------------- @BiomodEnsembleOptions

cat("\n", emo::ji("check"), "Defining biomod2 ensemble parameters")

ens.eval.metric                   <- "TSS"
ens.eval.metric.quality.threshold <- 0.01
ens.models.eval.meth              <- "TSS"
ens.prob.mean.weight              <- TRUE
ens.prob.mean.weight.decay        <- "proportional"
ens.committee.averaging           <- TRUE


cat("\n")
