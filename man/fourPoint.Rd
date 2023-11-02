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

\seealso{\code{\link{imageField}}
}
\examples{
library(rPAex)
prg1 <- system.file("examples/Ex-01.R", package="rPAex")
source(prg1)
r<-data1()
# x11()
op<-par(mar=c(2,2,4,2),cex=0.8)
terra::image(r,main="Generating the fourth point of the study plot",col=col2rgb(10),
xlim=c(20,101), ylim=c(0,101),axes=FALSE)
# p<-locator(3)
p<-list(x=c(40, 88, 80) , y=c(83, 82, 19))
q<-fourPoint(p)
polygon(q,lty=2,lwd=2)
text(q,cex=2)
points(q[4,1],q[4,2],cex=6,col=2,lwd=2)
d<-dist(q)
text(64,86,round(d[1],1),cex=1.5)
text(90,52,round(d[3],1),cex=1.5)
text(29,53,round(d[4],1),cex=1.5)
text(58,15,round(d[6],1),cex=1.5)
par(op)
# x11()
op <-par(mar=c(2,2,4,2),cex=0.8)
# An irregular area, use 4 points with p<-locator(4) and apply q<-fourPoint(p) to form the matrix
terra::image(r,bty="l",main="An irregular area")
#p<-locator(4)
p<-list(x=c(40, 88, 80, 29) , y=c(83, 82, 19, 20))
q<-fourPoint(p)
d<-dist(q)
polygon(q)
text(64,86,round(d[1],1),cex=1.5)
text(90,52,round(d[3],1),cex=1.5)
text(29,53,round(d[4],1),cex=1.5)
text(56,15,round(d[6],1),cex=1.5)
nx<-2;ny<-3 
dy=d[3]/ny;dx=d[1]/nx
# EU area= dx*dy
out<-imageField(r,q,ny,nx,dy,dx)
par(op) 
# number of pixels per EU
table(out$Qbase$EU)
}
\keyword{ dplot }

