externalPoint <-
function(models,R){
xyz <- raster::rasterToPoints(R) 
x<-xyz[,1];y<-xyz[,2]
m1<-models$m1
m2<-models$m2
m3<-models$m3
m4<-models$m4
e1<-y>=predict(m1,data.frame(x))
e2<-y>=predict(m2,data.frame(x))
e3<-y<=predict(m3,data.frame(x))
e4<-y<=predict(m4,data.frame(x))
a1<-xyz[e1,]
a2<-xyz[e2,]
a3<-xyz[e3,]
a4<-xyz[e4,]
outpoints<-rbind(a1,a2,a3,a4)
return(outpoints)
}
