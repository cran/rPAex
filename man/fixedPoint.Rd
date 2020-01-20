\name{fixedPoint}
\alias{fixedPoint}
\title{
Orientation, Position and Length of the Experimental Unit
}
\description{
Generates a number of equidistant spatial points in an area. Fixed a couple of points in 
the image and the number of segments included, the function determines the position of 
the segments according to the length of the segment. The function relates the real dimension 
of the segment measurement to the image dimension. The function is useful for sizing plot sizes in the field, it also facilitates the generation of experimental units in the field.
}
\usage{
fixedPoint(start, end, npoints, long)
}
\arguments{
  \item{start}{ Starting point
}
  \item{end}{
Point at the end
}
  \item{npoints}{
Number of points
}
  \item{long}{
Segment length
}
}
\details{
This function is used by imageField.
}
\value{
\item{xy}{Data vector with the coordinate of the points }
}

\seealso{\code{\link{borderPoint}}, \code{\link{externalPoint}}, 
\code{\link{fourPoint}}, \code{\link{imageField}}, \code{\link{designRaster}}, 
\code{\link{movePlot}}
}
\examples{
library(rPAex)
prg1 <- system.file("examples/Ex-01.R", package="rPAex")
source(prg1)
r<-data1()
oldpar<-par(mar=c(2,2,4,2),cex=0.8)
raster::image(r,col=col2rgb(10),main="Orientation, position and length of the experimental unit")
# P<-locator(2)
P<-list(x=c(20,80),y=c(40,80))
P<-cbind(x=P$x,y=P$y)
Q<-fixedPoint(start = P[1,],end = P[2,],4,long = 10)
x <- Q[,1]; y <- Q[,2]
s <- seq(length(x)-1)  # one shorter than data
segments(x[s], y[s], x[s+1], y[s+1], col= c(1,0),lwd=2)
text(Q,cex=1.5)
text(20,80,"Total length   = 72.11 units")
text(20,70,"total segments = 4")
text(60,40,"   Free space  = 10.7037 units")
text(60,30,"Segment length = 10 units")
text(50,10,"fixedPoint(start ,end ,npoints = 4,long = 10)")
text(20,35,"start",cex=1.5)
text(80,75,"end",cex=1.5)
par(oldpar)
}
\keyword{ design }
