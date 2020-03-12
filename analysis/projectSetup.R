#' Setup Project
#'
#' This R script sets the WOODIV SDM project by:
#'   - installing missing required packages
#'   - loading required packages
#'   - loading required R functions
#'   - creating subfolders
#'   - setting project parameters
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#' @date 2020/03/02




rm(list = ls())


## Install CRAN Packages ----

cran_packages <- c(
  "sf",
  "pROC",
  "rgdal",
  "raster",
  "biomod2",
  "stringr",
  "devtools"
)

nip <- cran_packages[!(cran_packages %in% installed.packages())]
lapply(nip, install.packages, dependencies = TRUE)


## Install GitHub Packages ----

if (!("emo" %in% installed.packages())) {
  devtools::install_github("hadley/emo")
}


## Load All Packages ----

ip <- unlist(
  lapply(
    X              = cran_packages,
    FUN            = require,
    character.only = TRUE,
    quietly        = TRUE
  )
)

if (sum(ip) == length(cran_packages)) {
  cat("\n", emo::ji("check"), "Loading packages")
} else {
  stop("Some packages failed to load!")
}


## Load R Functions ----

rfun <- unlist(
  lapply(
    list.files(
      path        = "R",
      pattern     = "\\.R$",
      full.names  = TRUE
    ),
    source,
    verbose = FALSE
  )
)


## Create subfolders ----


dir_names <- c(
  file.path("data", "climate"),
  file.path("data", "gadm"),
  file.path("data", "boundary"),
  file.path("data", "species"),
  file.path("output"),
  file.path("output", "biomod"),
  file.path("output", "biomod", "archives")
)

dir_vars <- c(
  "path_climate_data",
  "path_gadm_data",
  "path_boundary_data",
  "path_species_data",
  "output",
  "path_biomod",
  "path_biomod_archives"
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

rm(list = c("dir_names", "dir_vars", "cran_packages", "nip", "ip", "rfun"))



## Setup General Parameters ----

cat("\n", emo::ji("check"), "Defining user parameters")

lambert  <- "+init=epsg:3035"

horizon  <- "1979-2013"

varnames <- c(4, 6, 12, 14)
varnames <- paste0("0", varnames)
varnames <- substr(varnames, nchar(varnames) - 1, nchar(varnames))


sp_infos <- read.csv(
  file.path(
    "data",
    "species",
    "spp_area_sampling_italian_peninsula.csv"
  ),
  sep = ";"
)

spnames <- as.character(sp_infos[ , "species"])
spnames <- spnames[-c(1:93)]



## Setup BIOMOD Parameters ----

cat("\n", emo::ji("check"), "Defining biomod2 general parameters")

mod.models           <- "RF"
pa.nb.rep            <- 10
mod.n.rep            <- 10
mod.data.split       <- 70
nb.absences          <-  1
mod.var.import       <-  0
mod.models.eval.meth <- "TSS"
prevalence           <- NULL


cat("\n")
