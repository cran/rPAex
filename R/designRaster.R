designRaster<-function(R,book){
ncb<-ncol(book)
ncr<-ncol(R)
y<-book[R$eu,]
z<-cbind(R,y)
out <-agricolae::tapply.stat(z[,2:3],z[,(ncr+1):(ncr+ncb)])
return(list(design=out,rasterField=z))
}
