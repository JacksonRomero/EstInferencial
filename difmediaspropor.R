
# Diferencia de medias muestrales y de proporciones

#Ejercicio 9
#Datos
n1=36
sigma1=12
n2=49
sigma2<-4

media<-0
sigma<-((sigma1^2/n1)+ (sigma2^2/n2))^0.5

#### Simulación clásica ######

difmu<-rnorm(1e6,0,sigma)
difmu
sum( difmu>=-3 & difmu<=3 )/1e6

####Simulación de MonteCarlo: Aceptación y Rechazo

media+4*sigma
media-4*sigma
curve(dnorm(x,media,sigma),-8.320126,8.320126,
      ylab='', xlab='', axes=F, lwd=2)
axis(1, lwd=2)
abline(v=media,lwd=2, lty=2, col='red')

rect(-3,0,3,dnorm(0,0,sigma))
B=1e6
x12<-runif(B,-3,3)
y12<-runif(B,0,dnorm(0,0,sigma))
###y<=f(x)
ca<-ifelse(y12<=dnorm(x12,media,sigma),'red','black')

points(x12,y12,col=ca, pch=c(20,20))
#animación
#for(k in 1:20){
 # points(x12[1:k], y12[1:k], col=ifelse(y12[1:k]<=dnorm(x12[1:k],media,sigma),'red','black'),
  #      pch=c(17,17))
  #Sys.sleep(1)
#}

(table(ca)[2]/B)*(3--3)*dnorm(0,0,sigma)


#Ejercicio 41:


n11=32
sigma11=4
n22=36
sigma22<-3

media<-0
sigma2<-((sigma11^2/n11)+ (sigma22^2/n22))^0.5

#### Simulación clásica ######

difmu2<-rnorm(1e6,0,sigma2)
difmu2
sum( difmu2>2)/1e6

####Simulación de MonteCarlo: Aceptación y Rechazo

media+4*sigma2
media-4*sigma2
curve(dnorm(x,media,sigma2),-3.464102,3.464102,
      ylab='', xlab='', axes=F, lwd=2)
axis(1, lwd=2)
abline(v=media,lwd=2, lty=2, col='red')

rect(2,0,3.464102,dnorm(2,0,sigma2))
B=1e6
x.1<-runif(B,2,3.464102)
y.2<-runif(B,0,dnorm(2,0,sigma2))
###y<=f(x)
ca<-ifelse(y.2<=dnorm(x.1,media,sigma2),'red','black')

points(x.1,y.2,col=ca, pch=c(20,20))
#animación
#for(k in 1:20){
# points(x12[1:k], y12[1:k], col=ifelse(y12[1:k]<=dnorm(x12[1:k],media,sigma),'red','black'),
#      pch=c(17,17))
#Sys.sleep(1)
#}

(table(ca)[2]/B)*(3.464102-2)*dnorm(2,0,sigma2)


#Ejercicio 45

p1=0.40
p2=0.20
n1=300
n2=100

media3<- p1-p2
media3
sigma3<-( (p1*(1-p1)/n1) +(p2*(1-p2)/n2))^0.5
sigma3
#Simulación clásica
difpro<-rnorm(1e6,media, sigma3)
difpro
sum(difpro>0.10)/1e6
#Simulación de MonteCarlo: Aceptación - Rechazo

media3-4*sigma3

curve(dnorm(x,media3, sigma3), 0.004040821,0.3959592,xlab=''
      ,lwd=2, ylab='', axes=F)
axis(1, lwd=2)
abline(v=media3, lwd=2, lty=2,col='red')
rect(0.1,0,0.3959592,dnorm(0.2,media3, sigma3))

x3<-runif(1e6,0.1,0.3959592)
y3<-runif(1e6,0,dnorm(0.2,media3, sigma3))

ca3<-ifelse(y3<=dnorm(x3,media3, sigma3),'red','blue')
points(x3,y3,col=ca3,pch=c(20,20))

(table(ca3)[2]/1e6)*(0.3959592-0.1)*dnorm(0.2,media3, sigma3)

