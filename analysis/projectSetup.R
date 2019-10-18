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



#' ----------------------------------------------------------------------------- @CreateFolders

# script_names <- list.files(path = here::here("src"), pattern = "^[0-9]{3}.+\\.R$")
# script_names <- script_names[-1]
# dir_names    <- gsub("\\.R", "", script_names)
# dir_vars     <- stringr::str_extract(dir_names, "^[0-9]{3}[a-z]?")
# dir_vars     <- paste0("res_dir_", dir_vars)

method <- "rep10" # or distance

dir_names <- c(
  file.path("data", "climate"),
  "output",
  file.path("output", method)
)

dir_vars <- c(
  "path_climate_data",
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

rm(list = c("dir_names", "dir_vars", "cran_packages", "n_i_p", "i_p"))



#' ---------------------------------------------------------------------------- @Parameters

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


### Models Parameters             -------------------

mod.models           <- c("RF")
mod.n.rep            <-  1
mod.data.split       <- 80
mod.var.import       <-  0
mod.models.eval.meth <- "TSS"
prevalence           <- 0.5


### Algo Options                  -------------------

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


### Ensemble Parameters           -------------------

ens.eval.metric                   <- "TSS"
ens.eval.metric.quality.threshold <- 0.01
ens.models.eval.meth              <- "TSS"
ens.prob.mean.weight              <- TRUE
ens.prob.mean.weight.decay        <- "proportional"
ens.committee.averaging           <- TRUE



cat("\n", emo::ji("check"), "Defining user parameters")

cat("\n")
