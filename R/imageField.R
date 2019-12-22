imageField <-
  function(r, P,nPl,nPw,long,wide,ploting=TRUE ,...){
    n<-nlayers(r)
    r1<-r
    if(n>1)r1<-raster(r,layer=1)
    X<-r1
    Y<-r1
    xy<-coordinates(r1)
    values(X)<-xy[,1]
    values(Y)<-xy[,2]
#    check <- nPl==1
#    if(check)nPl=2
    sequence<-1:(nPl*nPw)
    xy1<-fixedPoint(start=P[1,1:2],end=P[4,1:2],npoints=nPl,long=long)
    wz1<-fixedPoint(start=P[2,1:2],end=P[3,1:2],npoints=nPl,long=long)
    M1 <-fixedPoint(start=xy1[1,1:2],end=wz1[1,1:2],npoints=nPw,long=wide)
    beta<-(P[1,2]-P[4,2])/(P[1,1]-P[4,1])
    for(i in 2:(2*nPl)){
      N1<-fixedPoint(start=xy1[i,1:2],end=wz1[i,1:2],npoints=nPw,long=wide)
      M1<-rbind(M1,N1)
    }
    Qin<-M1
    k<-0
    a<-c(1,2,2*(nPw+1),2*nPw+1)
#    if(check)nPl=1  
    for(j in 1:nPl){ 
      for(i in seq(0,2*(nPw-1),2)){
        k<-k+1
        area<-rbind(Qin[a+i,])
        pol <- raster::spPolygons(area)
        X0<-suppressWarnings(raster::extract(X, pol))
        Y0<-suppressWarnings(raster::extract(Y, pol))
        x<-X0[[1]];y<-Y0[[1]]; sequence<-k
        Q<-cbind(sequence,x,y)
        for(capa in 1:n){
          r1<-r
          if(n>1) r1<-raster(r,layer=capa)
          z1<-suppressWarnings(raster::extract(r1, pol))
          z<-z1[[1]]
          Q<-cbind(Q,z)
        }
        Q<-Q
        if(k==1)Qbase<-Q
        else Qbase<-rbind(Qbase,Q)
        if(ploting)raster::plot(pol,add=TRUE,...)
      }
      a<-a+4*nPw
    }
  if(beta>0){
  s<-Qbase[,1]
  v<-nPw*((s-1)%/%nPw+1)-(s-nPw*( (s-1)%/%nPw))+1
  nplot<-nPw*nPl
  v<-1+abs(v-nplot)
  Qbase[,1]<-v
  }
  colnames(Qbase)[4:(n+3)]<-c(paste("L",1:n,sep=""))
  Qbase<-data.frame(Qbase)
    return(Qbase)
  }

