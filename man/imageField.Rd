\name{imageField}
\alias{imageField}
\title{Matching Pixels With Field Book}
\description{
The function uses the raster image of all bands. It generates the limits of the unit and extracts the values of each pixel of the plot n x m units (n, m = 1,2, ...). The function requires the dimensions of the unit observed and the number of units per row (width) and column (length). The result is a table with image information and the characteristics of the experimental unit.
}
\usage{
imageField(r, Q, ny, nx, dy, dx,  start=1, plotting = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{raster image}
  \item{Q}{References points of de area}
  \item{ny}{Number of experimental units along the plot (y axis)}
  \item{nx}{Number of experimental units across the plot (x axis)}
  \item{dy}{Wide of unit plots}
  \item{dx}{Length of unit plots}
  \item{start}{Number of the first experimental unit}
  \item{plotting}{Overlap the units in the area, TRUE or FALSE}
  \item{\dots}{Other parameters the plot}
}

\value{
\item{parameters}{Parameters of experimental design in precision agriculture}
\item{Qbase}{Image data frame with location in field}
\item{coordinates.EU}{The limits of each experimental unit}
}

\seealso{\code{\link{EUsPoint}}, \code{\link{fixedPoint}}, 
\code{\link{fourPoint}}, \code{\link{designRaster}}, \code{\link{cassava}}
}
\examples{
library(rPAex)
library(raster)
data(cassava)
r <- rasterFromXYZ(cassava)
e <- extent(287697.8, 287705.8,8664189, 8664203)  # cutout of study area
rc <- crop(r, e)
#-----
# P<-locator(3)
P1<-list(x=c(287698.21, 287700.99, 287702.39), 
         y=c(8664200.68, 8664201.57,8664190.63))
P2<-list(x=c(287701.59, 287704.3, 287705.25),y=c(8664198.5, 8664199.3, 8664191.6))        
Q1<-fourPoint(P1)
Q2<-fourPoint(P2)
#-----
dy=1; dx=0.9
raster::image(rc,useRaster=FALSE,main="Cassava crop\nnear infrared image")
img1<-imageField(rc, Q1, ny=11, nx=3, dy, dx, plotting = TRUE, border="blue",lwd=1)
img2<-imageField(rc, Q2, ny=6, nx=3, dy, dx, start=34,plotting = TRUE, border="blue",lwd=1)
R<-rbind(img1$Qbase,img2$Qbase)
head(R)
q<-agricolae::tapply.stat(R[,2:3],R[,1],mean)
text(q[,2],q[,3],q[,1],cex=1)
}
\keyword{ design }

