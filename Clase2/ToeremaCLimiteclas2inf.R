par(mfrow=c(1,1))
teoremaCentral=function(n,iteraciones,poblacion1=70+2*rchisq(10000,7)){
#########################################

#poblacion1=rnorm(10000,70,5)
par(mfrow=c(2,1))
poblacion=poblacion1[poblacion1<110]
muestra=matrix(iteraciones,iteraciones*n);
dim(muestra)=c(iteraciones,n)
for (i in 1:iteraciones) {muestra[i,]=sample(poblacion,n)}

limites=c(min(poblacion),max(poblacion))
estimado=apply(muestra,1,'mean')
estimado[i]=mean(poblacion) ###Arreglo estetico para el útimo fotograma
.x=seq(min(muestra),max(muestra),length=200)
.y=dnorm(.x,mean(estimado),sd(poblacion))

.yest=dnorm(.x,mean(estimado),sd(estimado))


#No quiero que me enseñe todas las iteraciones. Solo unas pocas, en
#progresión geométrica después de las 14 primeras...
listaiteraciones=c(1:14,round(10^seq(log10(15),log10(iteraciones),length=20)))

intervalosarriba=seq(min(poblacion),max(poblacion),length=12)
if(n==1) intervalosabajo=intervalosarriba else intervalosabajo=seq(min(estimado),max(estimado),length=12)
for(i in listaiteraciones){
if (i==iteraciones) color=rgb(0,0,1)

hist(poblacion,xlim=limites,main="Distribución de la
población",xlab="",ylab="",sub="",breaks=intervalosarriba,lwd=1,col=rgb(.8,.9,.8),border=rgb(0,.5,0),freq=F);
if(i<5) readline("Pulsa <Enter>...");

if(i<iteraciones) {
 points(muestra[i,],rep(0,n));
 }else{
 points(.x,.y,type="l",lwd=1,lty=1,col="gray")
media=mean(estimado)
desviacion=sd(poblacion)
.xsd=c(-1,1)*desviacion+media

.ysd=dnorm(.xsd,media,desviacion)
points(.xsd,.ysd,type="l",lwd=1,lty=1,col="gray")
text(media+desviacion,dnorm(media+desviacion,media,desviacion),bquote(paste(sigma,"=",.(signif(desviacion,2)))),pos=4,,cex=1.25)}

if(i<5) readline("Pulsa <Enter>...");
abline(v=estimado[i],lty=2,lwd=2,col='grey')
points(estimado[i],0,col='black');
if(i>4){
hist(estimado[1:i],xlim=limites,main=paste("Distribución medias,
n=",n),xlab="",ylab="",sub="",col="lightgray",border="gray",freq=F,breaks=intervalosabajo)

 rug(estimado[1:i])
 abline(v=estimado[i],lty=2,lwd=2,col='black')
 points(estimado[i],0,col='black');
} else {
 plot(estimado[1:i],rep(0,i),xlim=limites,ylim=c(0,1),main=paste("Distribución
medias, n=",n),xlab="",ylab="",sub="",frame.plot=F);

 rug(estimado[1:i])
 abline(v=estimado[i],lty=2,lwd=2,col='black')
 points(estimado[i],0,col='black');
}
readline("Pulsa <Enter>...")
}
if (n>1) {
.x=seq(min(muestra),max(muestra),length=200)

.y=dnorm(.x,mean(estimado),sd(estimado))
media=mean(estimado)
desviacion=sd(poblacion)/sqrt(n)
.xsd=c(-1,1)*desviacion+media
.ysd=dnorm(.xsd,mean(estimado),desviacion)
points(.x,.yest,type="l",lwd=3,col="red")

points(.xsd,.ysd,type="l",lwd=1,lty=1,col="darkgray")
text(media+desviacion,dnorm(media+desviacion,media,desviacion),bquote(paste("E.T.=",sigma/sqrt(n),"=",.(signif(desviacion,2)))),pos=4,cex=1.25)

}
}


teoremaCentral(10,1000, )

