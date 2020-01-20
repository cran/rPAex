\name{externalPoint}
\alias{externalPoint}
\title{Information Outside the Experimental Plot
}
\description{
Pixels with information outside the experimental plot under study. The environment outside 
the study area is important for its explanation of the experimental response.
}
\usage{
externalPoint(models,R)
}
\arguments{
  \item{models}{list regression linear models, output borderPoint function}
  \item{R}{Raster image}
}
\details{
out points
}
\value{
 \item{outpoints }{coordenate matrix}
}
\author{
Felipe de Mendiburu
}
\seealso{\code{\link{borderPoint}}, \code{\link{fixedPoint}},  
\code{\link{fourPoint}}, \code{\link{imageField}}, \code{\link{designRaster}}, 
\code{\link{movePlot}}
}
\examples{
library(rPAex)
#
# for image "tiff" format, see rPAex document
# r <- fileRaster(tiff)
#
crop <- system.file("external/corn.csv", package="rPAex")
s<-read.csv(crop,header = TRUE)
r <- raster::rasterFromXYZ(s)
raster::image(r,useRaster=FALSE,main="Corn crop\nArea outside the experimental plot")
#P<-locator(3)
P<-list(x=c(287705.9, 287711.8, 287712.8),y=c(8664203.15, 8664204.8, 8664193.67))
Q<-fourPoint(P)
polygon(Q,border = "blue")
text(Q)
models<-borderPoint(Q)
out<-externalPoint(models,r)
points(out[,1],out[,2],pch=".")
}
\keyword{ dplot }
