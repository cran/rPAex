\name{EUsPoint}
\alias{EUsPoint}
\title{Generates the matrix Q of a particular experimental unit}
\description{
  The Q matrix is formed by 4 points that limits the Experimental unit, this matrix is used by the imageField function to generate the units, by obtaining the Q matrix of an unit, it is possible to generate subplots of the units.
}
\usage{
  EUsPoint(Rbook,EU)
}
\arguments{
  \item{Rbook}{object imagenField}
  \item{EU}{constant. number of experimental unit}
}
\value{
  \item{Q}{matrix, four points}
}

\seealso{\code{\link{imageField}}, \code{\link{cassava}}
}
\examples{
  library(rPAex)
  library(raster)
  data(cassava)
  e <- extent(287697.8, 287705.8,8664189, 8664203)
  s <- rasterFromXYZ(cassava)
  r <- crop(s,e)
  #-----
  # P<-locator(3)
  P<-list(x=c(287698.21, 287700.99, 287702.39), y=c(8664200.68, 8664201.57,8664190.63))
  Q<-fourPoint(P)
  #-----
  ny<-11; nx<-3; dy=1; dx=0.9
  image(r,useRaster=FALSE,main="Cassava crop\nnear infrared image")
  Rbook<-imageField(r, Q, ny, nx, dy, dx, plotting = TRUE, border="blue",lwd=1)
  # See experimental unit number 11
  Q<-EUsPoint(Rbook,EU=11)
  polygon(Q,lty=2,density=20)
}
\keyword{ dplot }

