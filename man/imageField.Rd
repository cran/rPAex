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
data(cassava)
r <- terra::rast(cassava, type="xyz")
# x11()
# terra::image(r)
# p<-locator(2)
# e<-terra::ext(unlist(p))
e <- terra::ext(287691.9, 287708.6, 8664188, 8664203)
rc <- terra::crop(r,e)
# Selection of experimental units, p1 and p2 in terra::image(r)
p1<-list(x=c(287698.34, 287701.14, 287702.33), 
         y=c(8664200.89, 8664201.65, 8664190.67))
p2<-list(x=c(287701.56, 287704.37, 287705.24),
	 y=c(8664198.68, 8664199.44, 8664191.46))        
q1<-fourPoint(p1)
q2<-fourPoint(p2)
# dimension of the experimental unit
dy=1; dx=0.9
op<-par(mar=c(0,0,3,0))
terra::image(rc,main="Selection of experimental units\nCassava crop",axes=FALSE)
img1<-imageField(rc, q1, ny=11, nx=3, dy, dx, plotting = TRUE, border="blue",lwd=1)
img2<-imageField(rc, q2, ny=8, nx=3, dy, dx, start=34,plotting = TRUE, border="blue",lwd=1)
# Spectral data of selected units
R<-rbind(img1$Qbase,img2$Qbase)
head(R)
Q<-agricolae::tapply.stat(R[,2:3],R[,1],mean)
text(Q[,2],Q[,3],Q[,1],cex=1)
par(op)
}
\keyword{ design }

