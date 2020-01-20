\name{borderPoint}
\alias{borderPoint}
\title{
Build the Edge Equations the Experimental Plot
}
\description{
With 4 points and their x and y coordinates of a Raster image, the function constructs the equations of the edge of the experimental plot. Its usefulness is to discriminate the unused area and also to study the responses of the spectrum electromagnetic radiation of the external environment of the area under study.
}
\usage{
borderPoint(Q)
}
\arguments{
  \item{Q}{list, Matrix, data.frame or arrays with coordinates x,y}
}
\details{
The four points can be obtained from a plot with locator(4) function. 
If the input data is a matrix, its size will be 4 rows and 2 columns.
}
\value{
\item{models}{list regression linear models}
}

\author{
Felipe de Mendiburu
}
\seealso{\code{\link{externalPoint}}, \code{\link{fixedPoint}}, 
\code{\link{fourPoint}}, \code{\link{imageField}}, \code{\link{designRaster}},
\code{\link{movePlot}}}

\examples{
# use corn crop information
library(rPAex)
crop <- system.file("external/corn.csv", package="rPAex")
s<-read.csv(crop,header = TRUE)
r <- raster::rasterFromXYZ(s)
raster::image(r,useRaster=FALSE,main="Area outside the experimental plot")
#P<-locator(3)
# simulated area
P<-list(x=c(287705.9, 287711.8, 287712.8),y=c(8664203.15, 8664204.8, 8664193.67))
Q<-fourPoint(P)
models<-borderPoint(Q)
abline(models$m1)
abline(models$m2)
abline(models$m3)
abline(models$m4)
text(Q,cex=2)
}
\keyword{  regression  }

