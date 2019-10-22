#' @title Add Country to Grid Pixels
#'
#' @description
#' This R script...
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 22/10/2019
#'



#' ---------------------------------------------------------------------------- @ImportStudyGrid

ras <- get(
  load(
    file.path(
      output,
      "CHELSA_biovars_cropped"
    )
  )
)


#' ---------------------------------------------------------------------------- @ImportMediterraneanCountries

countries_list <- read.csv(
  file.path(
    path_boundary_data,
    "mediterranean_countries.csv"
  )
)


#' ---------------------------------------------------------------------------- @ListGADMShapefiles

fls <- list.files(
  path = file.path(
    path_gadm_data
  ),
  pattern = "\\.rds$"
)

fls <- gsub("_0_sf\\.rds", "", fls)


#' ---------------------------------------------------------------------------- @LoopOnCountries

country_by_pixel <- data.frame()

for (j in 1:nrow(countries_list)) {


#' ---------------------------------------------------------------------------- @CheckIfShapefileIsDownloaded

  iso3 <- as.character(countries_list[j, "Code"])

  pos <- which(fls == iso3)

  if (length(pos) == 0) {

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


#' ---------------------------------------------------------------------------- @ImportShapefile

  shp <- readRDS(
    file.path(
      path_gadm_data,
      paste0(
        iso3,
        "_0_sf.rds"
      )
    )
  )


#' ---------------------------------------------------------------------------- @GetCellIDForTheCountry

  cells_infos <- xyFromPoly(x = ras, y = shp, na_rm = TRUE)


#' ---------------------------------------------------------------------------- @AddCountryLabel

  if (nrow(cells_infos) > 0) {

    cells_infos <- data.frame(
      cells_infos,
      country = as.character(countries_list[j, "Pays"])
    )

    country_by_pixel <- rbind(country_by_pixel, cells_infos)
  }
}


#' ---------------------------------------------------------------------------- @CleanCountryLabels

country_by_pixel[ , "country"] <- as.character(country_by_pixel[ , "country"])
country_by_pixel[ , "country"] <- gsub("Northern Cyprus", "Cyprus", country_by_pixel[ , "country"])
country_by_pixel[ , "country"] <- gsub("San-Marino", "Italy", country_by_pixel[ , "country"])
country_by_pixel[ , "country"] <- gsub("Vatican city", "Italy", country_by_pixel[ , "country"])
country_by_pixel[ , "country"] <- gsub("Monaco", "France", country_by_pixel[ , "country"])


#' ---------------------------------------------------------------------------- @RemoveDuplicates

pos <- which(duplicated(country_by_pixel[ , "cell_id"]))

if (length(pos) > 0) {

  country_by_pixel <- country_by_pixel[pos, ]
}


#' ---------------------------------------------------------------------------- @SaveData

save(country_by_pixel, file = file.path("output", paste0("Pixel_country_matches")))
