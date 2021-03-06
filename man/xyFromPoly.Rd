% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xyFromPoly.R
\name{xyFromPoly}
\alias{xyFromPoly}
\title{Extract cells ids of a raster overlapping a spatial polygon}
\usage{
xyFromPoly(x, y, na_rm = TRUE)
}
\arguments{
\item{x}{a RasterLayer, RasterBrick or RasterStack.}

\item{y}{a SpatialPolygons(DataFrame) or sf MULTIPOLYGON.}

\item{na_rm}{a boolean. If \code{TRUE}, NA cells ids are also returned.}
}
\value{
a vector of cells ids
}
\description{
Extract cells ids of a raster (Layer, Stack or Brick) overlapping a spatial polygon (sp or sf).
}
\examples{

cell_ids <- xyFromPoly(x = ras, y = shp, na_rm = TRUE)
}
\author{
Nicolas Casajus, \email{nicolas.casajus@fondationbiodiversite.fr}
}
