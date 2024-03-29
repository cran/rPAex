\name{rPAex}
\alias{rPAex-package}
\alias{rPAex}
\docType{package}
\title{
Automatic Detection of Experimental Unit in Precision Agriculture
}
\description{
The package contains functions to manage images obtained by remote sensing of the experimental fields. In the field the characteristics of the plot are defined (number of units per row and column and dimensions in meters or other dimension measures). The program uses the information to generate the limits and record the content of the different layers, as well as the coordinates of the pixels and 
the identification of the observation units in the field. It also allows to extract the experimental designs generated in agricolae package and distribute the treatments in the image according to the distribution of the generated plan. The images used in the examples were obtained from the repository of (Loayza et al. 2018) International Potato Center, V1.
}
\details{
\tabular{ll}{
Package: \tab rPAex\cr
Type: \tab Package\cr
Version: \tab 1.0.5\cr
Date: \tab 2023-11-01\cr
License: \tab GPL\cr
}
}
\references{ 
Loayza, Hildo; Silva, Luis; Palacios, Susan; Balcazar, Mario; Quiroz, Roberto, 2018, 
"Dataset for: Modelling crops using high resolution multispectral images", doi: 10.21223/P3/UVWVLA, International Potato Center, V1.

M. Montalvo, G. Pajares, J. M. Guerrero, J. Romeo, M. Guijarro, A. Ribeiro, J. J. Ruz, and J. Cruz. Automatic detection of crop rows in maize fields with high weeds pressure. Expert Systems with Applications, 39(15):11889-11897, 2012.

X. Zhang, X. Li, B. Zhang, J. Zhou, G. Tian, Y. Xiong, and B. Gu. Automated robust crop-row detection in maize fields based on position clustering algorithm and shortest path method. Computers and electronics in agriculture, 154:165-175, 2018.

F. de Mendiburu. A statistical analisys tool for agricultural research. Masters thesis, 
Universidad Nacional de Ingenieria. Lima-Peru, 8 2009. Degree in systems engineering.

Richards, J. A. Remote sensing digital image analysis: An introduction. 2012
}

\author{
Professor Felipe de Mendiburu\cr
Systems Engineer.\cr
Universidad Nacional de Ingenieria Lima-Peru.\cr
Professor Applied Statistics\cr
Universidad Nacional Agraria La Molina, Lima-Peru.\cr

Professor David Mauricio.\cr
Department of Computer Science.\cr
Universidad Nacional Mayor de San Marcos, Lima-Peru\cr

Rodrigo A. Morales A. PhD \cr
Phytopathologist-Sustainable Agriculture \cr
Agricultural Research Institute of Panama (IDIAP)

Professor Roberto Quiroz.\cr
Centro Agronomico Tropical de Investigacion.\cr
CATIE.\cr
}
\seealso{\code{\link{EUsPoint}}, \code{\link{fixedPoint}}, \code{\link{fourPoint}}, 
\code{\link{imageField}}, \code{\link{borderPoint}}, \code{\link{designRaster}}, 
\code{\link{movePlot}},\code{\link{cassava}},\code{\link{cropTime}}
}

\examples{
# Simple examples of the most important functions
library(rPAex)
# Graeco - latin square design
T1<-c("a","b","c","d")
T2<-c("v","w","x","y")
outdesign <- agricolae::design.graeco(T1,T2,serie=1)
book<-outdesign$book
prg1 <- system.file("examples/Ex-01.R", package="rPAex")
source(prg1)
r<-data1()
# x11()
op<-par(mar=c(2,2,4,2),cex=0.9)
terra::image(r,main="Graeco - latin square design\n
Treatments T1 (a, b, c, d) and T2 (v, w, x, y)",col=col2rgb(2),axes=FALSE,xlim=c(0,101))
axis(1);axis(2)
# 3 points are set in the image, to generate the experimental area
# P<-locator(3)
P<-list(x=c(20,90,80),y=c(80,90,20))
# and complete the fourth point to close the polygon.
Q<-fourPoint(P)
polygon(Q,lwd=3)
R<-imageField(r, Q, ny=4, nx=4, dy=12, dx=12,col=colors()[18])
q<-designRaster(R$Qbase,book)$design
text(q[,6],q[,7]+2,q[,1])
text(q[,6],q[,7]-2,paste(q[,4],q[,5],sep=" - "))
par(op)
# An irregular area, use 4 points with p<-locator(4) and apply q<-fourPoint(p) to form the matrix
# x11()
op<-par(mar=c(2,2,2,2),cex=0.9)
terra::image(r,xlim=c(20,100),ylim=c(10,100),axes=FALSE,main="An irregular area",col=col2rgb(3))
axis(1);axis(2)
# p<-locator(4)
p<-list(x=c(40, 88, 80, 29) , y=c(83, 82, 19, 20))
q<-fourPoint(p)
dist(q)
dx=10;dy=12
nx<-4; ny<-5
S<-imageField(r,q,ny,nx,dy,dx,col=colors()[20])
points(q,cex=4,pch=20,col="white")
text(q,cex=1.5)
M<-agricolae::tapply.stat(S$Qbase[,2:3],S$Qbase$EU)
# identifying EU
text(M[,2],M[,3],M[,1],cex=1)
par(op)
}

\keyword{ package }
%- Experimental Design
