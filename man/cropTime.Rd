\name{cropTime}
\alias{cropTime}
\docType{data}
\title{
Spectral evolution of cassava cultivation
}
\description{
Spectral data of the cassava crop during its development, of the 38 spectral images of the data repository of the International Potato Center, plot number 3 of 6 cultivated, is described in 9 moments of its development, obtaining near-infrared, red responses and green
}
\usage{data("cropTime")}
\format{
  A data frame with 172263 observations on the following 6 variables.
  \describe{
    \item{\code{Flight}}{a numeric vector}
    \item{\code{x}}{coordinate X, a numeric vector}
    \item{\code{y}}{coordinate Y, a numeric vector}
    \item{\code{L1}}{Near-Infrared Light (NIR), a numeric vector }
    \item{\code{L2}}{Red band, a numeric vector}
    \item{\code{L3}}{Green band, a numeric vector}
  }
}
\details{
The images were read with the rast function (Hijmans, 2023), plot 3 was located and the information of the 38 images was obtained with the rPAex imageField function. Due to the size of the images, only 9 images were used as part of the rPAex data. The images were captured with a Remotely Piloted Aircraft System (RPAS), the system included an OKtokoter platform and an multiespectral camera (MicroADC-Tetracam). The multiespectral images are composed of information in the NIR, Red and Green bands. The images were acquired at 95 meteres average flight altitude. drone flight date, began on December 18, 2014, and ended on November 4, 2015 (Loayza, 2018). The cropTime data table was built from the "tif" images and the terra package (Himans, 2023). The R code instructions for reading the images were similar to the "cassava data" construction, in which the rPAex function imageField() was added, to then select plot-3 of 9 images separated in time.
}

\source{
International Potato Center. CIP - Lima Peru. Dataverse CIP.
}
\references{
Loayza, Hildo; Silva, Luis; Palacios, Susan; Balcazar, Mario; Quiroz, Roberto, 2018, 
"Dataset for: Modelling crops using high resolution multispectral images", 
doi: 10.21223/P3/UVWVLA, International Potato Center, V1

Hijmans R (2023). _terra: Spatial Data Analysis_. https://CRAN.R-project.org/package=terra.
}
\seealso{\code{\link{cassava}}, \code{\link{EUsPoint}}, \code{\link{imageField}},
 \code{\link{borderPoint}}}
\examples{
library(rPAex)
data(cropTime)
ndvi<-with(cropTime,(L1-L2)/(L1+L2))
ndviTime<-data.frame(cropTime,ndvi)
fly<-c(1,  2,  6, 11, 20, 23, 30, 36, 38)
dates<-c("2014-12-18","2015-01-06","2015-01-29","2015-02-26","2015-04-22",
         "2015-05-15", "2015-07-03","2015-08-13","2015-08-28")
#x11()
op<-par(mfrow=c(3,3),mar=c(0,0,1,0),cex=0.8)
for(i in 1:9){
  P<-ndviTime[ndviTime$Flight==fly[i],]
  P$ndvi<-(P$L1-P$L2)/(P$L1+P$L2)
  I<-terra::rast(P[,c(2,3,7)],type="xyz")
  terra::image(I,col= hcl.colors(12, "Greens 3", rev = TRUE),axes=FALSE, 
  main=paste("fligth:", fly[i]))
}
par(op)
}
\keyword{datasets}
