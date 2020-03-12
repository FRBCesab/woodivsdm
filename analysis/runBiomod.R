#' Calibrates BIOMOD Models
#'
#' This R script runs BIOMOD (calibration + evaluation + projection steps)
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#' @date 2020/03/03



## Import Species Infos (countries to include) ----

cat("\n", emo::ji("check"), "Loading species occurrences")

sp_infos <- read.csv(
  file.path(
    "data",
    "species",
    "spp_area_sampling_italian_peninsula.csv"
  ),
  sep = ";"
)


## Import Species Occurrences ----

occs <- get(
  load(
    file.path(
      "output",
      "species_occurrences_cleaned"
    )
  )
)


## Select Species ----

occs <- occs[occs[ , "to_aggregate_with"] %in% spnames, ]


## Import Pixel Country Relation ----

cat("\n", emo::ji("check"), "Loading pixel-country relation")

countries <- get(
  load(
    file.path(
      "output",
      "Pixel_country_matches"
    )
  )
)


## Import Climate Data ----

cat("\n", emo::ji("check"), "Loading climate predictors")

climate <- raster::stack(
  x = file.path(
    "output",
    "climate",
    paste0("CHELSA_bio10_", varnames, "_cropped.tif")
  )
)


## Change Path ----

opath <- getwd()
setwd(path_biomod)



for (spname in spnames) {


  ## Subset Species Occurrences ----

  occ <- occs[occs[ , "to_aggregate_with"] == spname, ]

  cells <- unique(cellFromXY(climate, occ[ , c("coordX_laea", "coordY_laea")]))

  xy <- as.data.frame(xyFromCell(climate, 1:length(subset(climate, 1))))

  xy[ , "occurrence"]     <- NA
  xy[cells, "occurrence"] <- 1
  xy[ , "cell_id"] <- 1:length(subset(climate, 1))

  pos <- which(!is.na(subset(climate, 1)[]))
  xy <- xy[pos, ]

  xy <- merge(xy, countries[ , c("cell_id", "country")], by = "cell_id", all = FALSE)

  if (sp_infos[sp_infos[ , "species"] == spname, "subsampled_italy"] == 0) {
    xy <- xy[xy[ , "country"] != "Italy", ]
  }

  if (sp_infos[sp_infos[ , "species"] == spname, "subsampled_sardigna"] == 0) {
    xy <- xy[xy[ , "country"] != "Sardinia", ]
  }

  if (sp_infos[sp_infos[ , "species"] == spname, "subsampled_sicily"] == 0) {
    xy <- xy[xy[ , "country"] != "Sicily", ]
  }

  if (sum(xy[ , "occurrence"], na.rm = TRUE) > 9) {


    ## Set BIOMOD Parameters ----

    bm.form <- BIOMOD_FormatingData(
      resp.var        = xy[ , 'occurrence'],
      expl.var        = climate,
      resp.xy         = xy[ , c("x", "y")],
      resp.name       = spname,
      PA.nb.rep       = pa.nb.rep,
      PA.nb.absences  = nb.absences * sum(xy[ , 'occurrence'], na.rm = TRUE),
      PA.strategy     = "random"
    )


    ## Calibration step ----

    bm.mod <- BIOMOD_Modeling(
      data              = bm.form,
      models            = mod.models,
      NbRunEval         = mod.n.rep,
      DataSplit         = mod.data.split,
      Prevalence        = prevalence,
      VarImport         = mod.var.import,
      models.eval.meth  = mod.models.eval.meth,
      do.full.models    = FALSE,
      modeling.id       = "biomod"
    )


    ## Projection step ----

    bm.prj <- BIOMOD_Projection(
      modeling.output     = bm.mod,
      new.env             = climate,
      proj.name           = 'current',
      selected.models     = 'all',
      binary.meth         = NULL,
      compress            = FALSE,
      build.clamping.mask = FALSE
    )


    ## Import Projections ----

    projs <- stack(
      lapply(
        1:(pa.nb.rep * mod.n.rep),
        function(x) {
          raster(
            x = file.path(
              spname,
              "proj_current",
              paste0("proj_current_", spname, ".grd")
            ),
            band = x
          )
        }
      )
    )


    ## Import Calibration rows ----

    calib <- get(
      load(
        file.path(
          spname,
          ".BIOMOD_DATA",
          "biomod",
          "calib.lines"
        )
      )
    )


    ## Evaluation step ----

    metrics <- data.frame()

    for (pa_run in 1:pa.nb.rep) {

      for (eval_run in 1:mod.n.rep) {

        testing  <- which(!calib[ , eval_run, pa_run])

        model <- grep(paste0("_PA", pa_run, "_RUN", eval_run), names(projs))
        pred <- subset(projs, model)

        testing  <- data.frame(
          observed  = xy[testing, "occurrence"],
          predicted = pred[][xy[testing, "cell_id"]]
        )
        testing$observed  <- ifelse(is.na(testing$observed), 0, 1)
        testing$predicted <- testing$predicted / 1000

        attempts <- seq(0, 1, length.out = 100)

        tss_max <- NULL
        for (i in attempts) {
          misc <- table(testing$predicted >= i, testing$observed)
          x    <- misc[4] / (misc[4] + misc[3])
          y    <- misc[1] / (misc[1] + misc[2])
          tss_max <- c(tss_max, x + y - 1)
        }

        cutoff <- median(attempts[which(tss_max == max(tss_max, na.rm = TRUE))])

        misc <- table(testing$predicted >= cutoff, testing$observed)

        ccr   <- (misc[4] + misc[1]) / sum(misc)
        se    <- misc[4] / (misc[4] + misc[3])  # Presence
        sp    <- misc[1] / (misc[1] + misc[2])  # Absence

        tss   <- se + sp - 1

        auc   <- pROC::roc(
          response  = testing$observed,
          predictor = testing$predicted,
          quiet     = TRUE
        )$auc[[1]]

        kap   <- kappa(table(testing$predicted >= cutoff, testing$observed))


        vals <- data.frame(
          pa_run, eval_run, "RF", round(cutoff, digits = 3),
          round(ccr, digits = 3), round(se,  digits = 3), round(sp,  digits = 3),
          round(tss, digits = 3), round(auc, digits = 3), round(kap, digits = 3)
        )

        colnames(vals) <- c(
          "PA", "Run", "SDM", "Cutoff", "CCR", "SN", "SP", "TSS", "AUC", "KAPPA"
        )

        metrics <- rbind(metrics, vals)
      }
    }


    ## Get Occurrences coordinates ----

    coords <- xy[!is.na(xy[ , "occurrence"]), ]
    coords <- coords[coords[ , "occurrence"] == 1, ]
    coords <- coords[ , c("x", "y")]


    ## Create Observation layer ----

    obs <- subset(projs, 1)
    obs[][!is.na(obs[])] <- 0
    obs[][xy[!is.na(xy[ , "occurrence"]), "cell_id"]] <- 1


    ## Average projections (probs) ----

    metrics[ , "TSS"] <- ifelse(metrics[ , "TSS"] < 0, 0, metrics[ , "TSS"])
    weights <- metrics[ , "TSS"] / sum(metrics[ , "TSS"])

    avers   <- projs
    avers[] <- avers[] / 1000 * weights

    prb_mean   <- subset(avers, 1)
    prb_mean[] <- apply(avers[], 1, sum)


    ## Average projections (binary) ----

    cutoff_mean    <- sum(metrics[ , "Cutoff"] * weights)
    bin_mean_tss   <- prb_mean
    bin_mean_tss[] <- ifelse(prb_mean[] < cutoff_mean, 0, 1)

    bin_mean_p10 <- sdm_threshold(prb_mean, coords, type = "p10", binary = TRUE)


    ## Best Model projection (probs) ----

    pos_best <- which(metrics$TSS == max(metrics$TSS, na.rm = TRUE))

    metrics[pos_best, "Best"] <- 1

    prb_best   <- subset(projs, 1)
    prb_best[] <- apply(data.frame(projs[][, pos_best]) / 1000, 1, median, na.rm = TRUE)


    ## Best Model projection (binary) ----

    cutoff_best <- median(metrics[pos_best, "Cutoff"])

    bin_best_tss   <- prb_best
    bin_best_tss[] <- ifelse(prb_best[] < cutoff_best, 0, 1)

    bin_best_p10 <- sdm_threshold(prb_best, coords, type = "p10", binary = TRUE)


    ## Stack projections ----

    projs <- stack(
      obs,
      prb_mean, bin_mean_tss, bin_mean_p10,
      prb_best, bin_best_tss, bin_best_p10
    )

    names(projs) <- paste(
      spname,
      c(
        "obs",
        "prb_mean", "bin_mean_tss", "bin_mean_p10",
        "prb_best", "bin_best_tss", "bin_best_p10"
      ),
      sep = "_"
    )

    system(
      paste0(
        "tar jcf ",
        "archives/biomod_results",
        ".tar.gz2 ",
        spname, "/"
      )
    )

    system(
      paste0(
        "rm -r ",
        spname, "/"
      )
    )

    dir.create(
      path      = file.path(spname, "predictions"),
      recursive = TRUE
    )

    dir.create(
      path      = file.path(spname, "evaluation"),
      recursive = TRUE
    )

    system(
      paste0(
        "mv ",
        "archives/biomod_results",
        ".tar.gz2 ",
        spname, "/"
      )
    )

    for (i in 1:nlayers(projs)) {
      writeRaster(
        x        = subset(projs, i),
        filename = paste0(spname, "/predictions/", names(projs)[i], ".tif"),
        format   = "GTiff"
      )
    }

    save(metrics, file = file.path(spname, "evaluation", paste0(spname, "_metrics.rda")))

  } else {

    dir.create(paste0("__", spname))
  }
}


## Reset Path ----

setwd(opath)
