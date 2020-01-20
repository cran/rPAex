# Convert file tiff to raster each band in one layer
fileRaster<-function(file){
n<- raster::nbands(raster::raster(file))
r1<-raster::raster(file,band=1)
xy<-raster::coordinates((r1))
r<-cbind(xy)
for(i in 1:n) {
  r1<-raster::raster(file,band=i)
  L<-raster::values(r1)
  r<-cbind(r,L)
}
colnames(r)[3:(n+2)]<-paste("L",1:n,sep="")
r<-raster::rasterFromXYZ(r)
return(r)
}
