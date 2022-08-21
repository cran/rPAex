\name{EUsPoint}
\alias{EUsPoint}
\title{Generates the matrix Q of a particular experimental unit}
\description{
  The Q matrix is formed by 4 points that limits the Experimental unit, this matrix is used by the imageField function to generate the units, by obtaining the Q matrix of an unit, it is possible to generate subplots of the units.
}
\usage{
  EUsPoint(Rbook,unit)
}
\arguments{
  \item{Rbook}{object imagenField}
  \item{unit}{constant. number of experimental unit}
}
\value{
  \item{Q}{matrix, four points}
}

\seealso{\code{\link{imageField}}, \code{\link{designRaster}}
}
\examples{
  library(rPAex)
  crop <- system.file("external/cassava.csv", package="rPAex")
  s<-read.csv(crop,header = TRUE)
  r <- raster::rasterFromXYZ(s)
  #-----
  # P<-locator(3)
  P<-list(x=c(287698.21, 287700.99, 287702.39), y=c(8664200.68, 8664201.57,8664190.63))
  Q<-fourPoint(P)
  #-----
  ny<-11; nx<-3; dy=1; dx=0.9
  raster::image(r,useRaster=FALSE,main="Cassava crop\nnear infrared image")
  Rbook<-imageField(r, Q, ny, nx, dy, dx, plotting = TRUE, border="blue",lwd=1)
  # See experimental unit number 12
  Q<-EUsPoint(Rbook,unit=12)
  polygon(Q,lty=2,density=8)
}
\keyword{ dplot }

