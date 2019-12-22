data1<-function()
{
  x<-NULL;y<-NULL
  k<-0
  for(i in 0:99){
    for(j in 0:99){
      k<-k+1
      x[k]<-i
      y[k]<-j
    }
  }
  n<-length(x)
  field<- data.frame(x,y)
  field$z3<-round(rnorm(n,130,5),0)
  field$z1<-round(rnorm(n,40,5),0)
  field$z2<-round(rnorm(n,100,5),0)
  r<-raster::rasterFromXYZ(field)
  return(r)
}
