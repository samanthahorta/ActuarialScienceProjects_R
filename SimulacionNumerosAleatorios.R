
# a) Cálculo del numero Pi mediante el uso de numeros aleatorios #


# Repetimos el proceso de evaluación 100 veces 
Resultado <- c()
c <- 0:100
for (i in c){
  
  # Veces a lanzar
  N <- 10000                                               
  
  # Se crea vector de x y y de numeros aleatorios de 0 a 1 
  x <- runif(N,0,1)
  y <- runif(N,0,1)
  
  #Guardamos los resultados en un dataframe 
  sim <- data.frame(cbind(x,y))
  
  #Se saca la distancia total entre cada x,x
  sim$dist <- sqrt(x**2+y**2)
  
  #Se condiciona un factor que:
  #dist >= 1 serán igual a 1
  #dist < 1 serán igual a 0 
  sim$s <- as.factor((sim$dist>=1)*1)
  
  #Usamos la formula de (4*Area del circulo)/ Area del cuadrado
  Resultado[i] <- (4*nrow(subset(sim,s=="0")))/nrow(sim)
}

mean <- mean(Resultado)
a <- mean(Resultado)-sd(Resultado,na.rm=F)                                            
b <- mean(Resultado)+sd(Resultado,na.rm=F)  
resul<- cbind(a,mean,b)
resultados <- as.data.frame(resul) 
colnames(resultados) <- c('lim Inf','Media',' lim Sup')

#RESULTADOS a)
resultados                                                           



#### GRÁFICO ####

plot(sim$x,sim$y,pch=c(1,2)[sim$s],col=c(1,6)[sim$s])





# b) Evaluar la integral sin(x) entre los limites (0,pi) #


# INTEGRAL CON MONTE CARLO

# Establecer limites de la integral
a<-0
b<-pi

# Hacer una función G con la función a evaluar, en este caso sin(x)
G <- function(x){
  return(sin(x)) 
}

# Crear funcion monte carlo 
monte_carlo <- function(G,a,b,M){                  # Establecemos parametros: G, limite inferior, limite superior, puntos a lanzar                        
  s=0
  for(i in 1:M){
    s = s + G(a+(b-a)*runif(1,0,1))                #Para generar numeros aleatorios de manera uniforme dentro de la funcion  a evaluar G(x)                    
  }
  return(((b-a)/M)*s)                              #Evaluación de la interal M veces 
}                                                 

# Repetimos el proceso de evaluación 100 veces 
R <- c()
c <- 0:100
for (i in c){
  R[i] <- monte_carlo(G,a,b,10000)
}
  
  

# GRÁFICO 

curve(G,a,b,lwd=2,axes=FALSE,xlab='',ylab='')                              
axis(1)                                                                    
t<-seq(a,b,by=0.01)                                                        
x<- c(a,t,b)                                                                
y<- c(0,G(t),0)
polygon(x,y,col='grey')
abline(h=0,lwd=2)
rect(a,0,b,1,border='darkred',lwd=2)                                        
n<-10000
x1<-runif(n,a,b)                                                           
y1<-runif(n,0,1)                                                           
colour<-ifelse(y1<=G(x1),'darkred','transparent') 
points(x1,y1,col=colour,pch=c(19,19))
prob<-(table(colour)/n)*((b-a)*1)    

#VALOR REAL DE LA INTEGRAK    
integrate(function(x) sin(x), lower = 0, upper = pi)                  


mean <- mean(R)
a <- mean(R)-sd(R,na.rm=F)                                            
b <- mean(R)+sd(R,na.rm=F)  
resul<- cbind(a,mean,b)
resultados <- as.data.frame(resul) 
colnames(resultados) <- c('lim Inf','Media',' lim Sup')

#RESULTADOS b)
resultados                                                           #Mi funcion monte_carlo




