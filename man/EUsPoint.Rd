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
  data(cassava)
  s <- terra::rast(cassava,type="xyz")
  # Use image and locator(2)
  x11()
  op<-par(mar=c(2,2,3,2),cex=0.8)
  # terra::image(s)
  # p<-locator(2)
  # e<-terra::ext(unlist(p))
  e <- terra::ext(287691.9, 287708.6, 8664188, 8664203)
  r <- terra::crop(s,e)
  #-----
  # Define the border of the plot
  # p<-locator(3)
  p<-list(x=c(287698.21, 287700.99, 287702.39), y=c(8664200.68, 8664201.57,8664190.63))
  q<-fourPoint(p)
  #-----
  # Define a subarea of 11x3 units by 1x0.9 meters
  ny<-11; nx<-3; dy=1; dx=0.9
  terra::image(r,main="Cassava crop\nnear infrared image",bty="l")
  Rbook<-imageField(r, q, ny, nx, dy, dx, plotting = TRUE, border="blue",lwd=1)
  # identify unit 11 with the spectral information
  Q<-EUsPoint(Rbook,EU=11) 
  polygon(Q,col="black")
  pol <- terra::vect(Q, "polygons")
  R <- terra::extract(r, pol, xy = TRUE)
  head(R)
}
\keyword{ dplot }

