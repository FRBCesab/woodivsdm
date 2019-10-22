#' @title Extract cells ids of a raster overlapping a spatial polygon
#'
#' @description
#' Extract cells ids of a raster (Layer, Stack or Brick) overlapping a spatial polygon (sp or sf).
#'
#' @param x a RasterLayer, RasterBrick or RasterStack.
#' @param y a SpatialPolygons(DataFrame) or sf MULTIPOLYGON.
#' @param na_rm a boolean. If \code{TRUE}, NA cells ids are also returned.
#'
#' @author Nicolas Casajus, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @export
#'
#' @return a vector of cells ids
#'
#' @examples
#'
#' cell_ids <- xyFromPoly(x = ras, y = shp, na_rm = TRUE)



xyFromPoly <- function(x, y, na_rm = TRUE) {


  ## Check x argument   --------

  if (
    !(class(x) %in% c("RasterLayer", "RasterBrick", "RasterStack"))
  ) {

    stop("x must be RasterLayer, RasterBrick or RasterStack.")
  }


  ## Coerce x to RasterLayer   --------

  x <- subset(x, 1)


  ## Check y argument   --------

  if (
    !(class(y)[1] %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")) &&
    !(as.character(st_geometry_type(y)) %in% c("POLYGON", "MULTIPOLYGON"))
  ) {

    stop("y must be SpatialPolygons(DataFrame) or sf (MULTI)POLYGON.")
  }


  ## Check CRS   --------

  if (is.na(raster::projection(x)) || is.na(raster::projection(y))) {

    stop("x and y CRS must be defined.")
  }


  ## Project Spatial Polygon CRS (if required)   --------

  if (raster::projection(x) != raster::projection(y)) {

    if (class(y)[1] %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")) {

      y <- sp::spTransform(y, CRSobj = projection(x))

    } else {

      y <- sf::st_transform(y, crs = projection(x))
    }
  }



  ## Rasterize Spatial Polygon   --------

  y_rasterized <- raster::rasterize(y, x, mask = TRUE)


  ## Get Cells Centroid of Rasterized Polygon   --------

  if (na_rm) {

    cells <- which(!is.na(raster::getValues(y_rasterized)))

  } else {

    cells <- 1:length(raster::getValues(y_rasterized))
  }

  xy_centroids <- raster::xyFromCell(
    object  = x,
    cell    = cells,
    spatial = FALSE
  )


  ## Extract Raster Cells ID   --------

  cells_infos <- raster::extract(x, xy_centroids, cellnumbers = TRUE)

  pos <- which(!is.na(cells_infos[ , 2]))

  cells_infos <- data.frame(
    cell_id = cells_infos[ , "cells"],
    xy_centroids
  )

  cells_infos <- cells_infos[pos, ]

  return(cells_infos)
}
