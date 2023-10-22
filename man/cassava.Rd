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
The cassava crop data was built with the TTC_0559_georeferenced.tif image (Loayza, 2018) and the terra package (Hijmans, 2023). 
}
\source{
International Potato Center. CIP - Lima Peru. Dataverse CIP.
}
\references{
Loayza, Hildo; Silva, Luis; Palacios, Susan; Balcazar, Mario; Quiroz, Roberto, 2018, "Dataset for: Modelling crops using high resolution multispectral images", <doi: 10.21223/P3/UVWVLA>, International Potato Center, V1 \cr
Hijmans R (2023). _terra: Spatial Data Analysis. https://CRAN.R-project.org/package=terra
}
\seealso{\code{\link{cropTime}}, \code{\link{EUsPoint}}, \code{\link{imageField}}, \code{\link{borderPoint}}}
\examples{
library(rPAex)
# Generation of cassava data
# download the TTC_0559_georeferenced.tif image from CIP DATAVERSE repository
# library(terra)
# img = "TTC_0559_georeferenced.tif" 
# r<-rast(img)
# e = ext(287688, 287709, 8664174, 8664217)
# rc = crop(r, e)
# cassava = extract(rc,xy="TRUE")
data(cassava)
# Contains 262056 pixels of 5 centimeters each with multispectral data
str(cassava)
}
\keyword{datasets}
