#' @title Run Biomod models
#'
#' @description
#' This R script...
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 17/10/2019
#'


#' ---------------------------------------------------------------------------- @ImportSpeciesInfos

cat("\n", emo::ji("check"), "Loading species occurrences")

sp_infos <- read.csv(
  file.path(
    "data",
    "species",
    "spp_area.csv"
  )
)


#' ---------------------------------------------------------------------------- @ImportSpeciesOcc

occs <- get(
  load(
    file.path(
      "output",
      "species_occurrences_cleaned"
    )
  )
)


#' ---------------------------------------------------------------------------- @SelectSpecies

occs <- occs[occs[ , "to_aggregate_with"] %in% spnames, ]


#' ---------------------------------------------------------------------------- @ImportPixelCountryRelation

cat("\n", emo::ji("check"), "Loading pixel-country relation")

countries <- get(
  load(
    file.path(
      "output",
      "Pixel_country_matches"
    )
  )
)


#' ---------------------------------------------------------------------------- @ImportClimateData

cat("\n", emo::ji("check"), "Loading climate predictors")

climate <- get(
  load(
    file.path(
      "output",
      "CHELSA_biovars_cropped"
    )
  )
)
climate <- stack(climate)


#' ---------------------------------------------------------------------------- @ChangePath

opath <- getwd()
setwd(path_biomod)



for (spname in spnames) {



#' ---------------------------------------------------------------------------- @SubsetSpeciesOccurrences

  occ <- occs[occs[ , "to_aggregate_with"] == spname, ]

  cells <- unique(cellFromXY(climate, occ[ , c("coordX_laea", "coordY_laea")]))
  xy <- as.data.frame(xyFromCell(climate, 1:length(subset(climate, 1))))
  xy[ , "occurrence"]     <- NA
  xy[cells, "occurrence"] <- 1
  xy[ , "cell_id"] <- 1:length(subset(climate, 1))

  pos <- which(!is.na(subset(climate, 1)[]))
  xy <- xy[pos, ]

  xy <- merge(xy, countries[ , c("cell_id", "country")], by = "cell_id", all = FALSE)

  italy <- sp_infos[sp_infos[ , "species"] == spname, "italy"]

  if (italy == 0) {

    xy <- xy[xy[ , "country"] != "Italy", ]

  }



#' ---------------------------------------------------------------------------- @SetBiomodParameters


  bm.form <- BIOMOD_FormatingData(
    resp.var        = xy[ , 'occurrence'],
    expl.var        = climate,
    resp.xy         = xy[ , c("x", "y")],
    resp.name       = spname,
    PA.nb.rep       = 10,
    PA.nb.absences  = 2 * sum(xy[ , 'occurrence'], na.rm = TRUE),
    PA.strategy     = "random",
    PA.dist.max     = 100000
  )



#' ---------------------------------------------------------------------------- @RunBiomodAlgorithms


  bm.mod <- BIOMOD_Modeling(
    data              = bm.form,
    models            = mod.models,
    models.options    = bm.opt,
    NbRunEval         = mod.n.rep,
    DataSplit         = mod.data.split,
    Prevalence        = NULL,
    VarImport         = mod.var.import,
    models.eval.meth  = mod.models.eval.meth,
    do.full.models    = FALSE,
    modeling.id       = "biomod"
  )



#' ---------------------------------------------------------------------------- @RunEnsembleModel


  bm.em.all <- BIOMOD_EnsembleModeling(
    modeling.output                = bm.mod,
    chosen.models                  = 'all',
    em.by                          = 'all',
    eval.metric                    = ens.eval.metric,
    eval.metric.quality.threshold  = ens.eval.metric.quality.threshold,
    models.eval.meth               = ens.models.eval.meth,
    prob.mean                      = FALSE,
    prob.mean.weight               = ens.prob.mean.weight,
    prob.mean.weight.decay         = ens.prob.mean.weight.decay,
    committee.averaging            = ens.committee.averaging
  )



#' ---------------------------------------------------------------------------- @RunModelProjections


  bm.ef.all <- BIOMOD_EnsembleForecasting(
    EM.output        = bm.em.all,
    new.env          = climate,
    output.format    = ".grd",
    proj.name        = "ENSEMBLE_all",
    selected.models  = "all",
    binary.meth      = ens.models.eval.meth
  )



#' ---------------------------------------------------------------------------- @AverageProjections


  projs <- list.files(
    path        = file.path(
      spname
    ),
    full.names  = TRUE,
    recursive   = TRUE,
    pattern     = paste0("^proj_ENSEMBLE_all_", spname, "_ensemble.grd$")
  )

  projs  <- stack(projs)
  projs  <- subset(projs, grep("wmean", names(projs)))
  projs  <- mean(projs)
  bins   <- projs



#' ---------------------------------------------------------------------------- @ConvertInBinary


  cutoff <- get(
    load(
      file.path(
        spname,
        "models",
        "biomod",
        paste0(
          spname,
          "_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData"
        )
      )
    )
  )

  cutoff <- cutoff@model_evaluation["TSS", "Cutoff"]

  bins[] <- ifelse(projs[] < cutoff, 0, 1)



#' ---------------------------------------------------------------------------- @GetModelPerformances


  eval <- get(
    load(
      file.path(
        spname, ".BIOMOD_DATA", "biomod", "models.evaluation"
      )
    )
  )

  eval <- round(mean(eval[ , "Testing.data", , , ]), 3)



#' ---------------------------------------------------------------------------- @PredMaps


  png(
    file       = paste0(spname, "_bins.png"),
    width      = 12.00,
    height     =  8.00,
    units      = "in",
    res        = 600,
    pointsize  = 6
  )

  plot(bins)
  title(paste0(spname, " (TSS = ", eval, ")"))
  dev.off()

  png(
    file       = paste0(spname, "_prbs.png"),
    width      = 12.00,
    height     =  8.00,
    units      = "in",
    res        = 600,
    pointsize  = 6
  )

  plot(projs)
  title(paste0(spname, " (TSS = ", eval, ")"))
  dev.off()



#' ---------------------------------------------------------------------------- @ObsMaps


  obs <- bins
  obs[][!is.na(obs[])] <- 0
  obs[][xy[!is.na(xy[ , "occurrence"]), "cell_id"]] <- 1

  png(
    file       = paste0(spname, "_obs.png"),
    width      = 12.00,
    height     =  8.00,
    units      = "in",
    res        = 600,
    pointsize  = 6
  )

  plot(obs)
  points(xy[!is.na(xy$occurrence), c("x", "y")], pch = "+")
  title(paste0(spname, " (TSS = ", eval, ")"))
  dev.off()



#' ---------------------------------------------------------------------------- @SaveRasters


  ras <- stack(obs, projs, bins)
  names(ras) <- paste(spname, c("obs", "prbs", "bins"), sep = "_")

  save(ras, file = paste0(spname, "_predictions.RData"))

} # e_o spnames loop



#' ---------------------------------------------------------------------------- @ResetWD


setwd(opath)
