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

\seealso{\code{\link{borderPoint}}, \code{\link{fixedPoint}},  
\code{\link{fourPoint}}, \code{\link{imageField}}, \code{\link{designRaster}}
}

\examples{
library(rPAex)
#
prg1 <- system.file("examples/Ex-01.R", package="rPAex")
source(prg1)
r<-data1()
oldpar<-par(mar=c(2,2,4,2),cex=0.8)
raster::image(r,col=col2rgb(10),main="Orientation, position and length of the experimental unit")
P<-list(x=c(23, 68, 78),y=c(77, 85, 45))
Q<-fourPoint(P)
polygon(Q)
R<-imageField(r,Q,3,2,13,22,ploting = TRUE,col=colors()[20])
points(Q[1,1],Q[1,2],cex=3)
q<-list(x=c(30, 60),y=c(70, 80))
arrows(q$x[1],q$y[1],q$x[2],q$y[2],lwd=2,length=0.1)
Qnew<-movePlot(Q,q)
polygon(Qnew,border="brown")
R1<-imageField(r,Qnew,3,2,13,22,ploting = TRUE,col="red",density=20)
points(q$x[1],q$y[1],cex=3)
}
\keyword{ dplot }
