#' Add Country to Grid Pixels
#'
#' This R script adds country label to each grid cell of the study area
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#' @date 2020/03/03




cat("\n", emo::ji("check"), "Setting Pixel-Country matches")


## Import Study Grid ----
  
ras <- raster::stack(
  x = file.path(
    "output",
    "climate",
    paste0("CHELSA_bio10_", varnames, "_cropped.tif")
  )
)


## Import Mediterranean Countries ----

countries_list <- read.csv(
  file.path(
    path_boundary_data,
    "mediterranean_countries.csv"
  )
)


## List GADM Shapefiles ----

fls <- list.files(
  path = file.path(
    path_gadm_data
  ),
  pattern = "\\.rds$"
)

fls <- gsub("_0_sf\\.rds", "", fls)


## Loop On Countries ----

country_by_pixel <- data.frame()

for (j in 1:nrow(countries_list)) {


  ## Check if Shapefile is already downloaded ----

  iso3 <- as.character(countries_list[j, "Code"])

  pos <- which(fls == iso3)

  if (!length(pos)) {

    cat("\n")

    download.file(
      url       = paste0(
        "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_",
        iso3,
        "_0_sf.rds"
      ),
      destfile  = file.path(
        path_gadm_data,
        paste0(
          iso3,
          "_0_sf.rds"
        )
      )
    )
  }


## Import Shapefiles ----

  shp <- readRDS(
    file.path(
      path_gadm_data,
      paste0(
        iso3,
        "_0_sf.rds"
      )
    )
  )


  ## Get cells ID overlapping the country <j> ----

  cells_infos <- xyFromPoly(x = subset(ras, 1), y = shp, na_rm = TRUE)


  ## Add Country Label ----

  if (nrow(cells_infos) > 0) {

    cells_infos <- data.frame(
      cells_infos,
      country = as.character(countries_list[j, "Pays"])
    )

    country_by_pixel <- rbind(country_by_pixel, cells_infos)
  }
}


## Clean Country Labels ----

country_by_pixel[ , "country"] <- as.character(country_by_pixel[ , "country"])
country_by_pixel[ , "country"] <- gsub("Northern Cyprus", "Cyprus", country_by_pixel[ , "country"])
country_by_pixel[ , "country"] <- gsub("San-Marino", "Italy", country_by_pixel[ , "country"])
country_by_pixel[ , "country"] <- gsub("Vatican city", "Italy", country_by_pixel[ , "country"])
country_by_pixel[ , "country"] <- gsub("Monaco", "France", country_by_pixel[ , "country"])


## Remove Duplicates ----

pos <- which(duplicated(country_by_pixel[ , "cell_id"]))

if (length(pos)) {
  country_by_pixel <- country_by_pixel[-pos, ]
}


## Export Data ----

save(country_by_pixel, file = file.path("output", paste0("Pixel_country_matches")))

cat("\n")
