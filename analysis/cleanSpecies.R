#' @title Clean Species occurrences
#'
#' @description
#' This R script...
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 16/10/2019
#'



#' ---------------------------------------------------------------------------- @ImportSpeciesOcc

cat("\n", emo::ji("check"), "Loading species occurrences")

occs <- get(
  load(
    file.path(
      path_species_data,
      "WOODIV_working_file_310719.RData"
    )
  )
)

occs <- occs@data



#' ---------------------------------------------------------------------------- @ConvertDataType

to_char <- c("Idgrid", "status", "source", "scientific_name", "to_aggregate_with")
for (col in to_char) occs[ , col] <- as.character(occs[ , col])

to_num  <- c("Xcentroid", "Ycentroid")
for (col in to_num) occs[ , col] <- as.numeric(as.character(occs[ , col]))



#' ---------------------------------------------------------------------------- @SelectData

cat("\n", emo::ji("check"), "Subsetting species occurrences")

occs <- occs[!is.na(occs[ , "Idgrid"]), ]

occs <- occs[which(occs[ , "status"] %in% c("N", "N?")), ]

occs <- occs[which(!(occs[ , "source"] %in% "Serra-Diaz" & !(occs[ , "country"] == "Cyprus"))), ]


#' ---------------------------------------------------------------------------- @RemoveDuplicats

cat("\n", emo::ji("check"), "Removing duplicated occurrences")

pos <- which(
  duplicated(
    paste(occs[ , "Idgrid"], occs[ , "to_aggregate_with"], sep = "__")
  )
)

if (length(pos) > 0) occs <- occs[-pos, ]


#' ---------------------------------------------------------------------------- @ExportOccurrences

cat("\n", emo::ji("check"), "Saving cleaned species data")
save(occs, file = file.path("output", paste0("species_occurrences_cleaned")))

cat("\n")
