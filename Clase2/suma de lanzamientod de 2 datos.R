n=1000
dado<-1:6
b<-rep(0,n)

for(i in 1:n){
  b[i]=sum(sample(dado,2,T))
}
b

for( k in 1:100){
  barplot(table(b[1:k]),xlim=c(0,12), ylim=c(0,30),col='red',
          border = 'white', axes=F,lwd=3)

  Sys.sleep(1)
}

rbind(table(b)/n,round(c(1,2,3,4,5,6,5,4,3,2,1)/36,3))
