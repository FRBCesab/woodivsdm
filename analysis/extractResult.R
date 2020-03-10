library(raster)

spnames <- list.dirs(
  path       = file.path("output", "biomod"),
  recursive  = FALSE,
  full.names = FALSE
)

spnames <- spnames[grep("^[A-Z]{4}$", spnames)]

projs <- list.files(
  path       = file.path("output", "biomod"),
  pattern    = "bin_best_p10.tif$",
  recursive  = TRUE,
  full.names = TRUE
)

x = stack(lapply(projs, raster))

rich <- subset(x, 1)
rich[] <- apply(x[], 1, sum)
plot(rich)

eval <- list.files(
  path       = file.path("output", "biomod"),
  pattern    = "rda$",
  recursive  = TRUE,
  full.names = TRUE
)

spnames <- unlist(lapply(strsplit(eval, "/"), function(x) x[3]))

eval <- lapply(1:length(eval), function(i){
  data.frame(
    spname = spnames[i],
    get(load(eval[i]))
  )
})

eval <- do.call(rbind.data.frame, eval)
eval <- eval[eval[ , "TSS"] > 0, ]

projs <- list.files(
  path       = file.path("output", "biomod"),
  pattern    = "obs.tif$",
  recursive  = TRUE,
  full.names = TRUE
)

occ <- lapply(1:length(projs), function(i){

  data.frame(
    spname = spnames[i],
    occurrence = sum(raster(projs[i])[], na.rm = TRUE)
  )
})
occ <- do.call(rbind.data.frame, occ)


eval <- merge(eval, occ, by = "spname", all = TRUE)

# eval <- eval[!is.na(eval$Best), ]

moy <- tapply(eval$TSS, eval$spname, mean)
err <- tapply(eval$TSS, eval$spname, sd)
occ <- tapply(eval$occurrence, eval$spname, function(.) .[1])

synt <- data.frame(
  spname = names(moy),
  occurrence = occ,
  tss_mean = moy,
  tss_sd   = err
)

synt2 <- synt[ , c("spname", "occurrence", "tss_mean")]
synt2 <- data.frame(Metric = "Mean", synt2)
colnames(synt2)[-c(1, 2)] <- c("Occurrence", "TSS")

synt3 <- synt[ , c("spname", "occurrence", "tss_sd")]
synt3 <- data.frame(Metric = "SD", synt3)
colnames(synt3)[-c(1, 2)] <- c("Occurrence", "TSS")

syntt <- rbind(synt2, synt3)
syntt <- syntt[syntt[ , "Occurrence"] > 10, ]

ggplot(data = syntt, aes(x = Occurrence, y = TSS)) +
geom_point() +
geom_smooth() +
facet_wrap(. ~ Metric, scales = "free") +
theme_bw() +
theme(strip.text.x = element_text(size = 12, face = "bold"))
