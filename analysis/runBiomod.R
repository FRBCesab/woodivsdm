#' @title Run Biomod models
#'
#' @description
#' This R script...
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 17/10/2019
#'


compt <- Sys.time()

#' ---------------------------------------------------------------------------- @ImportSpeciesOcc

cat("\n", emo::ji("check"), "Loading species occurrences")

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


setwd("output")

for (spname in spnames) {
# spname <- spnames[1]


  occ <- occs[occs[ , "to_aggregate_with"] == spname, ]

  cells <- unique(cellFromXY(climate, occ[ , c("coordX_laea", "coordY_laea")]))
  xy <- as.data.frame(xyFromCell(climate, 1:length(subset(climate, 1))))
  xy[ , "occurrence"]     <- NA
  xy[cells, "occurrence"] <- 1

  pos <- which(!is.na(subset(climate, 1)[]))
  xy <- xy[pos, ]


  bm.form <- BIOMOD_FormatingData(
    resp.var        = xy[ , 'occurrence'],
    expl.var        = climate,
    resp.xy         = xy[ , c("x", "y")],
    resp.name       = spname,
    PA.nb.rep       = 1,
    PA.nb.absences  = 4 * sum(xy[ , 'occurrence'], na.rm = TRUE),
    PA.strategy     = "random"
  )

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

  bm.ef.all <- BIOMOD_EnsembleForecasting(
    EM.output        = bm.em.all,
    new.env          = climate,
    output.format    = ".grd",
    proj.name        = "ENSEMBLE_all",
    selected.models  = "all",
    binary.meth      = ens.models.eval.meth
  )

  tss <- bm.mod@models.evaluation@val[ , "Testing.data", , , ]

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


  png(
    file       = paste0(spname, ".png"),
    width      = 12.00,
    height     =  8.00,
    units      = "in",
    res        = 600,
    pointsize  = 6
  )

  plot(bins)
  points(xy[!is.na(xy$occurrence), c("x", "y")], pch = "+")
  title(spname)
  dev.off()

}

setwd("...")







cat("\n", Sys.time() - compt)
