imageField <-
  function(r, Q,ny,nx,dy,dx,start=1,plotting=TRUE ,...){
    n<-nlayers(r)
    r1<-r
    if(n>1)r1<-raster(r,layer=1)
    X<-r1
    Y<-r1
    xy<-coordinates(r1)
    values(X)<-xy[,1]
    values(Y)<-xy[,2]
    #    check <- ny==1
    #    if(check)ny=2
    eu<-1:(ny*nx)
    xy1<-fixedPoint(start=Q[1,1:2],end=Q[4,1:2],segments=ny,length=dy)
    wz1<-fixedPoint(start=Q[2,1:2],end=Q[3,1:2],segments=ny,length=dy)
    M1 <-fixedPoint(start=xy1[1,1:2],end=wz1[1,1:2],segments=nx,length=dx)
    beta<-(Q[1,2]-Q[4,2])/(Q[1,1]-Q[4,1])
    for(i in 2:(2*ny)){
      N1<-fixedPoint(start=xy1[i,1:2],end=wz1[i,1:2],segments=nx,length=dx)
      M1<-rbind(M1,N1)
    }
    Qin<-M1
    k<-0
    a<-c(1,2,2*(nx+1),2*nx+1)
    #    if(check)ny=1  
    for(j in 1:ny){ 
      for(i in seq(0,2*(nx-1),2)){
        k<-k+1
        area<-rbind(Qin[a+i,])
        pol <- raster::spPolygons(area)
        X0<-suppressWarnings(raster::extract(X, pol))
        Y0<-suppressWarnings(raster::extract(Y, pol))
        x<-X0[[1]];y<-Y0[[1]]; eu<-k
        P<-cbind(eu,x,y)
        for(capa in 1:n){
          r1<-r
          if(n>1) r1<-raster(r,layer=capa)
          z1<-suppressWarnings(raster::extract(r1, pol))
          z<-z1[[1]]
          P<-cbind(P,z)
        }
        P<-P
        if(k==1)Qbase<-P
        else Qbase<-rbind(Qbase,P)
        if(plotting)raster::plot(pol,add=TRUE,...)
      }
      a<-a+4*nx
    }
    if(beta>0){
      s<-Qbase[,1]
      v<-nx*((s-1)%/%nx+1)-(s-nx*( (s-1)%/%nx))+1
      nplot<-nx*ny
      v<-1+abs(v-nplot)
      Qbase[,1]<-v
    }
    colnames(Qbase)[4:(n+3)]<-c(paste("L",1:n,sep=""))
    Qbase[,1] <-Qbase[,1]+start-1
    Qbase<-data.frame(Qbase)
    parameters <- list(Q=Q, ny=ny,nx=nx,dy=dy,dx=dx)
    outPlot <- list(parameters = parameters, Qbase = Qbase, coordinates.unit=Qin)
    return(outPlot)
  }
  
