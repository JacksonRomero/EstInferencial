
#asignación <- o =

a<-10 #asignamos el valor de 10 a a
a

b=5
b
######################

#Creación de vectores
a1<-1:10 # generar valores entre 1 y 10
a1

a2<-c(1,2,3,6,8,10) #c:concatenar
a2

##orden de los datos
a1[5]
a2[6]

##secuencias
help('seq')

s1<-seq(5,20,l=5)
s1
s2<-seq(5,20,by=2)
s2
s3<-seq(1,6,l=6)
s3
##rep:repetir números
r1<-rep(2,3)
r1
r2<-rep(1:3,2)
r2

r3<-rep(1:3, times=c(3,5,1))
r3
r4<-rep(1:6,1)
r4

#Creación de funciones
 
#hipotenusa
su2<-function(a,b){
  f<-a+b
  return(f)
}

su2(3,7)  
hipo<-function(a,b){
  return((a^2+b^2)^0.5)
}
hipo(3,4)

f<-function(n){
  if(n==0 | n==1){# | es 0, & significa y
    r<- 1
    }else{
    r<-n*f(n-1)}
  return(r)
}

f(0)

#5!=5*4!
#4!=4*3!
#3!=3*2!
#n!=n*(n-1)!  

# Estadística
e<-rnorm(100,980,5)
summary(e)# estadísticos descriptivos


median(e) #mediana
mean(e) #media aritmética
#mode(e) moda

# Lanzamiento de n monedas
## Suma de lanzamiento de 2 monedas
B=100 # repeticiones del Exp. aleatorio
n=2 # número de lanzamientos
r<-rep(0,100)
for (i in 1:B) {
  moneda<-0:1
  r[i]<-sum(sample(moneda,2,replace = TRUE))

  }
table(r)/B

#lanzamiento de n monedas
B=10000 # repeticiones del Exp. aleatorio
n=100 # número de lanzamientos
r<-rep(0,100)
for (i in 1:B) {
  moneda<-c("cara","sello")
  r[i]<-sample(moneda,n,replace = TRUE)
  
}
table(r)/B

# lanzamiento de dados: sapply
## Suma de lanzamiento de 2 dados
d<-sapply(1:1e6,function(x){sum(sample(1:6,2,T))})
pro<-table(d)/1e6
pro

#simulación
for(k in 1:1e2){
  barplot(table(d[1:k]), col='red', lwd=2)
  Sys.sleep(1)
}

#(1,6) 6,1 5,2 2,5 4,3 3,4
1,1 2
1,2 2 ,1 3
round#sirve para dedondeart números
round(rbind(pro,c(1,2,3,4,5,6,5,4,3,2,1)/36),3)#unir por filas dos vectores

#replicate

d1<-replicate(1e6,sum(sample(1:6,2,T)))
pro<-table(d1)/1e6
pro


#integrales: Simulación de Montecarlo
#2-2x 0<x<1
f.MC<-function(x){
  f<-ifelse(x>=0 & x<=1,x^2,0)
}
x<-seq(-1,2,l=100)

plot(x,f.MC(x), type='l',col='black', lwd=2, main ='',
     xlab='',ylab='')
MC<-function(n,a,b){
  x<-runif(n,a,b)
  f<-(b-a)*mean(f.MC(x))
  return(f)  
}
MC(1e7,0,1)
#SMonteCarlo: Aceptación y Rechazo
rect(0.25,0,0.75,f.MC(0.75))
x.mc<-runif(1e6,0.25,0.75)
y.mc<-runif(1e6,0,f.MC(0.75))
cod<-ifelse(y.mc<f.MC(x.mc), 'red','black')
points(x.mc,y.mc, col=cod,pch=c(20,20))

table(cod)/1e4

for(k in 1:1e2){
  x.mc<-runif(1e2,0.25,0.75)
  y.mc<-runif(1e2,0,1.5)
    points(x.mc[1:k],y.mc[1:k], col=ifelse(y.mc[1:k]<f.MC(x.mc[1:k]),'red','black')
           ,pch=c(20,20))
    Sys.sleep(1)
}

(0.75^3-0.25^3)/3
0.4761*(0.75-0.25)*f.MC(0.75)



