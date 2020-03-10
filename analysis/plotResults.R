library(raster)

spnames <- list.dirs(
  path       = file.path("output", "biomod"),
  recursive  = FALSE,
  full.names = FALSE
)
spnames <- spnames[grep("^[A-Z]{4}$", spnames)]

occs <- get(
  load(
    file.path(
      "output",
      "species_occurrences_cleaned"
    )
  )
)

latin <- data.frame(
  code  = occs[ , "to_aggregate_with"],
  latin = paste(occs$genus, occs$species)
)

latin <- latin[latin[ , "code"] %in% spnames, ]
latin <- latin[!duplicated(latin$code), ]


shorts  <- c("CCR", "SN", "SP", "TSS", "AUC")
labels  <- c(
  "Correct Classification Rate (CCR)",
  "Sensitivity (SN)", "Specificity (SP)",
  "True Skill Statistics (TSS)",
  "Area Under the ROC Curve (AUC)"
)

for (spname in spnames) {
  
  obs <- raster(
    file.path(
      "output", "biomod", spname, "predictions",
      paste0(spname, "_obs.tif")
    )
  )
  
  n_occ <- sum(obs[], na.rm = TRUE)
  
  if (n_occ > 9) {
    
    eval <- get(
      load(
        file.path(
          "output", "biomod", spname, "evaluation",
          paste0(spname, "_metrics.rda")
        )
      )
    )  
    
    eval <- eval[eval[ , "TSS"] >= 0, ]
    eval <- eval[eval[ , "AUC"] >= 0.5, ]
    
    png(
      filename  = file.path("figures", "evaluation", paste0(spname, ".png")),
      width     = 14,
      height    = 21,
      units     = "cm",
      pointsize = 10,
      bg        = "transparent",
      res       = 600
    )
    
    par(
      family   = "serif",
      xaxs     = "i",
      yaxs     = "i",
      fg       = "#888888",
      cex.axis = 1.25,
      mar      = c(3.75, 4.25, 2.00, 1.50)
    )
    
    par(mfrow = c(3, 2))
    
    plot(
      x    = 0,
      axes = FALSE, 
      ann  = FALSE, 
      type = "n", 
      bty  = "n", 
      xlim = c(0, 1),
      ylim = c(0, 100)
    )
    
    par(xpd = TRUE)
    text(
      x = 0.5, 
      y = 50,
      labels = paste0(
        latin[which(latin$code == spname), "latin"],
        "\n(", spname, ")"
      ),
      cex = 2.50,
      font = 4
    )
    par(xpd = FALSE)
    
    for (metric in shorts) {
      
      if (metric != "AUC") {
        
        vals <- hist(eval[ , metric], breaks <- seq(0.00, 1.00, by = 0.050), plot = FALSE)
        
      } else {
        
        vals <- hist(eval[ , metric], breaks <- seq(0.50, 1.00, by = 0.025), plot = FALSE)
        
      }
      
      width <- vals$mids[1] - vals$breaks[1]
      
      plot(
        x    = 0,
        axes = FALSE, 
        ann  = FALSE, 
        type = "n", 
        bty  = "n", 
        xlim = range(vals$breaks),
        ylim = c(0, 100)
      )
      
      grid(lwd = 0.50, lty = 1, col = "#eeeeee")
      
      for (i in 1:length(vals$mids)) {
        
        rect(
          xleft   = vals$breaks[i],
          xright  = vals$breaks[i] + (2 * width),
          ybottom = 0,
          ytop    = vals$counts[i],
          col     = "#764d28",
          border  = NA,
          lwd     = 1.0
        )
      }
      
      axis(
        side      = 2,
        las       = 1,
        lwd       = 0,
        lwd.ticks = 1,
        col.axis  = par()$fg
      )
      
      axis(
        side      = 1,
        lwd       = 0,
        lwd.ticks = 1,
        col.axis  = par()$fg
      )
      
      par(xpd = TRUE)
      rect(
        xleft   = par()$usr[1],
        xright  = par()$usr[2],
        ybottom = 100,
        ytop    = 110,
        col     = par()$fg,
        border  = par()$fg,
        lwd     = 1.0
      )
      text(x = mean(vals$breaks), y = 105, labels = metric, cex = 1.5, col = "white", font = 2)
      
      par(xpd = FALSE)
      
      mtext(text = "Counts", side = 2, line = 2.40, cex = 1, col = par()$fg, font = 2)
      box("plot", col = par()$fg, lwd = 0.50)
      
      text(
        x      = par()$usr[1],
        y      = 90,
        labels = paste0(metric, " = ", round(mean(eval[ , metric]), 3), " +/- ", round(sd(eval[ , metric]), 3), "SD"),
        col    = par()$fg,
        cex    = 1.15,
        font   = 2,
        pos    = 4
      )
    }
    
    dev.off()
    
    obs <- list.files(
      path       = file.path("output", "biomod", spname, "predictions"),
      pattern    = "obs",
      recursive  = TRUE,
      full.names = TRUE
    )

    obs <- raster(obs)
    
    for (threshold in c("p10", "tss")) {
    
      png(
        filename  = file.path("figures", "mapping", paste0(spname, "_", threshold, ".png")),
        width     = 14,
        height    = 21,
        units     = "cm",
        pointsize = 10,
        bg        = "transparent",
        res       = 600
      )
      
      
      projs <- list.files(
        path       = file.path("output", "biomod", spname, "predictions"),
        pattern    = threshold,
        recursive  = TRUE,
        full.names = TRUE
      )
      
      projs <- stack(lapply(projs, raster))
      
      par(
        family   = "serif",
        xaxs     = "i",
        yaxs     = "i",
        fg       = "#888888",
        cex.axis = 1.25,
        mar      = c(1.00, 1.00, 2.00, 0.25)
      )
      
      par(mfrow = c(3, 1))
      
      plot(obs, legend = FALSE, axes = FALSE, box = TRUE, legend.mar = 0)
      points(xyFromCell(obs, which(obs[] == 1)), pch = "+", cex = 1.5, col = "darkgreen")
      
      mtext(
        text = latin[which(latin$code == spname), "latin"],
        side = 3, line = 0.5, cex = 1, col = par()$fg, font = 2
      )
      
      for (layer in c("mean", "best")) {
        
        ras <- subset(projs, grep(layer, names(projs)))
        plot(ras, legend = FALSE, axes = FALSE, box = TRUE, legend.mar = 0)
        
        mtext(
          text = gsub("_", " ", toupper(names(ras))),
          side = 3, line = 0.5, cex = 1, col = par()$fg, font = 2
        )
      }
      
      dev.off()
    }
  }
}