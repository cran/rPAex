movePlot <-
function(Q,q){
q<-matrix(unlist(q),2)
    m1<-(Q[2,2]-Q[1,2])/(Q[2,1]-Q[1,1])
    m2<-(q[2,2]-q[1,2])/(q[2,1]-q[1,1])
angle <- atan(m2)-atan(m1)
P<-rbind(c(cos(angle),sin(angle)),c(-sin(angle),cos(angle)))
Q<-Q%*%P
    delta<-Q[1,]-q[1,]
    Q[,1]<-Q[,1]-delta[1]
    Q[,2]<-Q[,2]-delta[2] 
return(Q) 
}
