\name{fourPoint}
\alias{fourPoint}
\title{Generating the Fourth Point of the Study Plot}
\description{
Generate the fourth reference point of the plot according to three defined geo-referential points. This function is important for the correct use of all the functions of the rPAex package. In the image the plot is a parallelogram, the first assigned point must be located in the upper left and continue the second point in the upper right side and the third point in the lower right, always in a clockwise direction.
}
\usage{
fourPoint(P)
}
\arguments{
 \item{P}{the three points list}
}
\value{
\item{P}{matrix, four points}
}

\seealso{\code{\link{borderPoint}}, \code{\link{externalPoint}},  
\code{\link{fixedPoint}}, \code{\link{imageField}}, \code{\link{designRaster}}, 
\code{\link{movePlot}}
}
\examples{
library(rPAex)
prg1 <- system.file("examples/Ex-01.R", package="rPAex")
source(prg1)
r<-data1()
oldpar<-par(mar=c(2,2,4,2),cex=0.8)
raster::image(r,main="Generating the fourth point of the study plot",col=col2rgb(10))
#P<-locator(3)
P<-list(x=c(20,80,80),y=c(70,80,30))
Q<-fourPoint(P)
polygon(Q,lty=2,density=8)
text(Q,cex=2)
points(Q[4,1],Q[4,2],cex=6,col=2,lwd=2)
par(oldpar)
}
\keyword{dplot}
