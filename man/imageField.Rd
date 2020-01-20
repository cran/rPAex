\name{imageField}
\alias{imageField}
\title{Matching Pixels With Field Book}
\description{
The function uses the raster image of all bands. It generates the limits of the unit and 
extracts the values of each pixel of the plot n x m units (n, m = 1,2, ...). 
The function requires the dimensions of the unit observed and the number of units 
per row (width) and column (length). The result is a table with image information and 
the characteristics of the experimental unit.
}
\usage{
imageField(r, P, nPl, nPw, long, wide,  ploting = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{raster image}
  \item{P}{References points of de area}
  \item{nPl}{Number of experimental units along the plot (X axis)}
  \item{nPw}{Number of experimental units across the plot (Y axis)}
  \item{wide}{Wide of unit plots}
  \item{long}{Long of unit plots}
  \item{ploting}{Overlap the units in the area, TRUE or FALSE}
  \item{\dots}{Other parameters the plot}
}

\value{
\item{parameters}{Parameters of experimental design in precision agriculture}
\item{Qbase}{Image data frame with location in field}
}

\seealso{\code{\link{borderPoint}}, \code{\link{externalPoint}}, 
\code{\link{fixedPoint}}, \code{\link{fourPoint}}, \code{\link{designRaster}}, 
\code{\link{movePlot}}
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
nPl<-11; nPw<-3; long=1; wide=0.9
raster::image(r,useRaster=FALSE,main="Cassava crop\nnear infrared image")
img<-imageField(r, P=Q, nPl, nPw, long, wide, ploting = TRUE, border="blue",lwd=1)
R<-img$Qbase
head(R)
q<-agricolae::tapply.stat(R[,2:3],R[,1],mean)
text(q[,2],q[,3],q[,1],cex=1)
}
\keyword{ design }
