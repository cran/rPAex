\name{movePlot}
\alias{movePlot}
\title{Rotation and Translation of the Plot Position
}
\description{
The coordinates of the plot generated with the locate() and fourPoint() functions define the experimental units with the field dimensions, In the successive images in time, these may have some difference in position and it is necessary to adapt the experimental units to obtain exactly the information within the unit.
}
\usage{
movePlot(Q,q)
}
\arguments{
  \item{Q}{matrix. Four points of the plot as described by the fourPoint function}
  \item{q}{matrix or list. Two points, the first one sets the position and the second the orientation }
}
\details{
The matrix Q has the points organized according to the fourPoint function. To know the numbering in the plane, execute text(Q). The first must be the upper left and numbered clockwise.
}
\value{
 \item{q}{matrix. Four points of the new plot as described by the fourPoint function}
}
\author{
Felipe de Mendiburu
}

\seealso{ \code{\link{imageField}}
}

\examples{
library(rPAex)
op<-par(mfrow=c(1,3),mar=c(0,0,0,0))
# Coordinates during initial flight
plot(0,0,xlim=c(0.08,0.9),ylim=c(0.1,0.9),axes=FALSE)
# x11(); p0<-locator(3)
p0<-list(x=c(0.20, 0.64, 0.81),y=c(0.71, 0.83, 0.40)) 
Q0<-fourPoint(p0)
dp<-dist(Q0)
text(Q0[,1],Q0[,2],paste("(",Q0[,1],",",Q0[,2], ")",sep=""),cex=1.2)
polygon(Q0,border="blue",lwd=1.5)
centro<-apply(Q0,2,mean)
areaEU<-round(dp[1]*dp[2],4)
text(centro[1],centro[2],paste("Area=",areaEU),col="blue")
text(0.50,0.2,"old plot",cex=1.5)
#-------
# Change of coordinates effect of flight, correction to initial flight
plot(0,0,xlim=c(0.08,0.9),ylim=c(0.1,0.9),axes=FALSE)
polygon(Q0,border="blue",lwd=1.5)
text(Q0,cex=1.5)
# x11(); s <-locator(3)
s<-list(x=c(0.2,0.62),y=c(0.71,0.73))
Qs<-movePlot(Q0,s)
centro<-apply(Qs,2,mean)
polygon(Qs,border="red",lty=2,lwd=1.5)
text(Qs,cex=1.5)
text(centro[1],centro[2]+0.05,paste("Area=",areaEU),col="red")
text(centro[1],0.9,"Change position\n of the new images",cex=1.5)
text(0.50,0.2,"change position",cex=1.5)
#--------
# correction result
plot(0,0,xlim=c(0.08,0.9),ylim=c(0.1,0.9),axes=FALSE)
polygon(Qs,border="red",lwd=1.5)
text(Qs,cex=1.5)
text(centro[1],centro[2],paste("Area=",areaEU),col="red")
text(0.50,0.2,"new plot",cex=1.5)
par(op)
}
\keyword{ dplot }

