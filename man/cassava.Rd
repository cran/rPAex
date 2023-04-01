\name{cassava}
\alias{cassava}
\docType{data}
\title{ Cassava crop}
\description{
The image of the cassava crop corresponds to flight 11 recorded by the drone on February 26, 2015 at the International Potato Center, with a multispectral camera. The cultivation area includes 6 plots of 11 x 6 meters. Due to the size of the images, only image 11 was used in rPAex.
}  
\usage{data("cassava")}
\format{
  A data frame with 262056 observations on the following 5 variables.
  \describe{
    \item{\code{x}}{coordinate X, a numeric vector}
    \item{\code{y}}{coordinate Y, a numeric vector}
    \item{\code{L1}}{Near-Infrared Light (NIR), a numeric vector }
    \item{\code{L2}}{Red band, a numeric vector}
    \item{\code{L3}}{Green band, a numeric vector}
  }
}
\details{
The cassava crop data was built with the TTC_0559_georeferenced.tif image (Loayza, 2018) and the raster package (Hijmans, 2022). The R code instructions that were used for the cassava dataframe were: \cr
library(raster); img = "TTC_0559_georeferenced.tif" \cr
layer1 = raster(img,band=1); layer2 = raster(img,band=2); layer3 = raster(img,band=3) \cr
r = addLayer(layer1,layer2,layer3); e = extent(287688, 287709, 8664174, 8664217)\cr
rc = crop(r, e); bands = values(rc); xy = coordinates(rc); cassava = data.frame(xy,bands)
}
\source{
International Potato Center. CIP - Lima Peru. Dataverse CIP.
}
\references{
Loayza, Hildo; Silva, Luis; Palacios, Susan; Balcazar, Mario; Quiroz, Roberto, 2018, "Dataset for: Modelling crops using high resolution multispectral images", <doi: 10.21223/P3/UVWVLA>, International Potato Center, V1

Hijmans, Robert J. 2022. Raster: Geographic Data Analysis and Modeling. https://CRAN.R-project.org/package=raster.
}
\seealso{\code{\link{evolution}}, \code{\link{EUsPoint}}, \code{\link{imageField}}, \code{\link{borderPoint}}}
\examples{
library(rPAex)
data(cassava)
head(cassava)
}
\keyword{datasets}
