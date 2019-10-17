#' @title Run Biomod models
#'
#' @description
#' This R script...
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 17/10/2019
#'



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







### Format Data for BIOMOD        -------------------

  bm.form <- BIOMOD_FormatingData(
    resp.var   = sp.occ,
    expl.var   = vars,
    resp.xy    = sp.xy,
    resp.name  = spname
  )


### Change Working Directory      -------------------

  setwd(
    paste(
      root_path,
      "outputs",
      sep = .Platform$file.sep
    )
  )


### Build BIOMOD Single Models    -------------------

  bm.mod <- BIOMOD_Modeling(
    data              = bm.form,
    models            = mod.models,
    models.options    = bm.opt,
    NbRunEval         = mod.n.rep,
    DataSplit         = mod.data.split,
    Prevalence        = prevalence,
    VarImport         = mod.var.import,
    models.eval.meth  = mod.models.eval.meth,
    do.full.models    = FALSE,
    modeling.id       = "biomod"
  )


### Build BIOMOD Ensemble Models  -------------------

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












cat("\n", Sys.time() - compt)
