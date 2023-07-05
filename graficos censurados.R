
#Censura a la derecha
#porcentaje de censura
p=0.86
#tamaño de muestra
n=5
q = 20
#se considera un proceso estandarizado
media=0
sigma=1

#identificando el cuantil para datos censurados
k=qnorm(p,0,1,lower.tail=FALSE)

muestra = rnorm(n = n*q,mean = media,sd = sigma)
muestra_obs = muestra
cc = muestra_obs > k
r = sum(1 - cc)
muestra_obs[cc] = k
muestra_iter = muestra_obs

mu0 = mean(muestra_obs)
sigma0 = sd(muestra_obs)

muI = mu0
sigmaI = sigma0
error = 1
i = 0
while(error > 1e-7){
  i = i+1
  print(i)
  muA = muI
  sigmaA = sigmaI
  zc = (k - muI)/sigmaI
  wc = muI + sigmaI*dnorm(zc)/pnorm(zc,lower.tail = F)
  #wc = muI + sigmaI*dnorm(zc)/pnorm(zc,lower.tail = F) # caso t
  muestra_iter[cc] = wc
  muI = mean(muestra_iter)
  lambda = dnorm(zc)/pnorm(zc,lower.tail = F)*(dnorm(zc)/pnorm(zc,lower.tail = F) - zc)
  sigma2I = var(muestra_iter)*(n*q - 1)/(r + (n*q - r)*lambda)
  # lambda = dnorm(zc)/pnorm(zc,lower.tail = F)*(dnorm(zc)/pnorm(zc,lower.tail = F) - zc)
  # sigma2I = var(muestra_iter)*(n*q - 1)/(r + (n*q - r)*lambda) # caso t
  sigmaI = sqrt(sigma2I)
  
  error = max(c(abs(muA - muI),abs(sigmaA - sigmaI)))
}

cbind(muI,sigmaI)

# 
# 
# 
# 


# Fase 2 ------------------------------------------------------------------


#número de simulaciones
RLs=1000
counter = 0
numRLs = 0
vectorRL=0
val = 1.13

#limite inferior grafico
UCL1= muI - val*sigmaI # este se prueba hasta que alcanzo el alpha deseado

#mientras se cumplan el número de simulaciones RLS
while(numRLs< RLs){
  #genera muestra
  a=rnorm(n,muI,sigmaI)
  #se reemplaza los valores censurados por el peso
  a[a>=k]= wc
  #se estandariza la media
  z = mean(a)
  
  counter=counter+1
  
  #identificando si está fuera de control
  if(z<UCL1){
    
    vectorRL[numRLs] = counter
    numRLs = numRLs + 1
    counter = 0
    
  }
  
}

ARL=mean(vectorRL)

#numero medio de muestras hasta que hubo una señal
1/ARL
#1/arl es el valor de alfa.


#número de simulaciones grafico desviacion estandar
RLs=1000
counter = 0
numRLs = 0
vectorRL=0
val = 1.62

#limite inferior grafico
UCL1= val*sigmaI # este se prueba hasta que alcanzo el alpha deseado

#mientras se cumplan el número de simulaciones RLS
while(numRLs< RLs){
  #genera muestra
  a=rnorm(n,muI,sigmaI)
  #se reemplaza los valores censurados por el peso
  a[a>=k]= wc
  #se estandariza la media
  z = sd(a)
  
  counter=counter+1
  
  #identificando si está fuera de control
  if(z>UCL1){
    
    vectorRL[numRLs] = counter
    numRLs = numRLs + 1
    counter = 0
    
  }
  
}

ARL=mean(vectorRL)

#numero medio de muestras hasta que hubo una señal
1/ARL
#1/arl es el valor de alfa.



#***********************************************
#censura a la izquierda
#porcentaje de censura
p=0.84
#tamaño de muestra
n=5
q = 20
#se considera un proceso estandarizado
media=0
sigma=1

#identificando el cuantil para datos censurados
k=qnorm(p,0,1,lower.tail=TRUE)

muestra = rnorm(n = n*q,mean = media,sd = sigma)
muestra_obs = muestra
cc = muestra_obs <k
r = sum(1 - cc)
muestra_obs[cc] = k
muestra_iter = muestra_obs

mu0 = mean(muestra_obs)
sigma0 = sd(muestra_obs)

muI = mu0
sigmaI = sigma0
error = 1
i = 0
while(error > 1e-7){
  i = i+1
  print(i)
  muA = muI
  sigmaA = sigmaI
  zc = (k - muI)/sigmaI
  wc = muI - sigmaI*dnorm(zc)/pnorm(zc,lower.tail = TRUE)
  #wc = muI - sigmaI*dnorm(zc)/pnorm(zc,lower.tail = TRUE) # caso t
  muestra_iter[cc] = wc
  muI = mean(muestra_iter)
  lambda = dnorm(zc)/pnorm(zc,lower.tail = T)*(dnorm(zc)/pnorm(zc,lower.tail = TRUE) + zc)
  sigma2I = var(muestra_iter)*(n*q - 1)/(r + (n*q - r)*lambda)
  # lambda = dnorm(zc)/pnorm(zc,lower.tail = T)*(dnorm(zc)/pnorm(zc,lower.tail = TRUE) + zc)
  # sigma2I = var(muestra_iter)*(n*q - 1)/(r + (n*q - r)*lambda) # caso t
  sigmaI = sqrt(sigma2I)
  
  error = max(c(abs(muA - muI),abs(sigmaA - sigmaI)))
}

cbind(muI,sigmaI)

# 
# 
# 
# 


# Fase 2 ------------------------------------------------------------------


#número de simulaciones
RLs=1000
counter = 0
numRLs = 0
vectorRL=0
val = 1.13

#limite inferior grafico
UCL1= muI + val*sigmaI # este se prueba hasta que alcanzo el alpha deseado

#mientras se cumplan el número de simulaciones RLS
while(numRLs< RLs){
  #genera muestra
  a=rnorm(n,muI,sigmaI)
  #se reemplaza los valores censurados por el peso
  a[a<=k]= wc
  #se estandariza la media
  z = mean(a)
  
  counter=counter+1
  
  #identificando si está fuera de control
  if(z>UCL1){
    
    vectorRL[numRLs] = counter
    numRLs = numRLs + 1
    counter = 0
    
  }
  
}

ARL=mean(vectorRL)

#numero medio de muestras hasta que hubo una señal
1/ARL
#1/arl es el valor de alfa.


#número de simulaciones grafico desviacion estandar
RLs=1000
counter = 0
numRLs = 0
vectorRL=0
val = 1.62

#limite inferior grafico
UCL1= val*sigmaI # este se prueba hasta que alcanzo el alpha deseado

#mientras se cumplan el número de simulaciones RLS
while(numRLs< RLs){
  #genera muestra
  a=rnorm(n,muI,sigmaI)
  #se reemplaza los valores censurados por el peso
  a[a<=k]= wc
  #se estandariza la media
  z = sd(a)
  
  counter=counter+1
  
  #identificando si está fuera de control
  if(z>UCL1){
    
    vectorRL[numRLs] = counter
    numRLs = numRLs + 1
    counter = 0
    
  }
  
}

ARL=mean(vectorRL)

#numero medio de muestras hasta que hubo una señal
1/ARL
#1/arl es el valor de alfa.


#***********************************************
#censura intervalar
#porcentaje de censura
b=1.5
a=0.5
#tamaño de muestra
n=5
q = 20
#se considera un proceso estandarizado
media=0
sigma=1
p=pnorm(b,0,1)-pnorm(a,0,1)
#identificando el cuantil para datos censurados
#k=qnorm(p,0,1,lower.tail=TRUE)

muestra = rnorm(n = n*q,mean = media,sd = sigma)
muestra_obs = muestra
cc = ((muestra_obs >a)&(muestra_obs<=b))
r = sum(1 - cc)
muestra_obs[cc] = (a+b)/2
muestra_iter = muestra_obs

mu0 = mean(muestra_obs)
sigma0 = sd(muestra_obs)

muI = mu0
sigmaI = sigma0
error = 1
i = 0
while(error > 1e-7){
  i = i+1
  print(i)
  muA = muI
  sigmaA = sigmaI
  za=(a-muI)/sigmaI
  zb=(b-muI)/sigmaI
  wc = muI +sigmaI*(dnorm(za)-dnorm(zb))/(pnorm(zb,lower.tail = TRUE)-pnorm(za,lower.tail = TRUE))
  #wc = muI +sigmaI*(dnorm(a)-dnorm(b))/(pnorm(b,lower.tail = TRUE)-pnorm(a,lower.tail = TRUE))# caso t
  muestra_iter[cc] = wc
  muI = mean(muestra_iter)
  mm=(dnorm(za)-dnorm(zb))/(pnorm(zb,lower.tail = TRUE)-pnorm(za,lower.tail = TRUE))
  lambda = mm*(mm + (za-zb))
  sigma2I = var(muestra_iter)*(n*q - 1)/(r + (n*q - r)*lambda)
  # lambda = dnorm(zc)/pnorm(zc,lower.tail = T)*(dnorm(zc)/pnorm(zc,lower.tail = TRUE) + zc)
  # sigma2I = var(muestra_iter)*(n*q - 1)/(r + (n*q - r)*lambda) # caso t
  sigmaI = sqrt(sigma2I)
  
  error = max(c(abs(muA - muI),abs(sigmaA - sigmaI)))
}

cbind(muI,sigmaI)

# 
# 
# 
# 


# Fase 2 ------------------------------------------------------------------


#número de simulaciones
RLs=1000
counter = 0
numRLs = 0
vectorRL=0
val1 = 1.39
val2=-1.39

#limite isuperior grafico
UCL2= muI + val1*sigmaI # este se prueba hasta que alcanzo el alpha deseado
UCL1= muI + val2*sigmaI
#mientras se cumplan el número de simulaciones RLS
while(numRLs< RLs){
  #genera muestra
  a1=rnorm(n,muI,sigmaI)
  #se reemplaza los valores censurados por el peso
  a1[(a1<=b)&(a1>a)]= wc
  #se estandariza la media
  z = mean(a1)
  
  counter=counter+1
  
  #identificando si está fuera de control
  if(z<UCL1 | z>UCL2){
    
    vectorRL[numRLs] = counter
    numRLs = numRLs + 1
    counter = 0
    
  }
  
}

ARL=mean(vectorRL)

#numero medio de muestras hasta que hubo una señal
1/ARL
#1/arl es el valor de alfa.


#número de simulaciones grafico desviacion estandar
RLs=1000
counter = 0
numRLs = 0
vectorRL=0
c4=0.940
val1 = 3.2
b3=c4-val1*sqrt(1-(c4*c4))
b4=c4+val1*sqrt(1-(c4*c4))
b3=ifelse(b3<0,0,b3)


#limite inferior grafico
UCL2= b4*sigmaI
UCL1=b3*sigmaI
# este se prueba hasta que alcanzo el alpha deseado

#mientras se cumplan el número de simulaciones RLS
while(numRLs< RLs){
  #genera muestra
  a1=rnorm(n,muI,sigmaI)
  #se reemplaza los valores censurados por el peso
  a1[(a1<=b)&(a1>a)]= wc
  #se estandariza la media
  z = sd(a1)
  
  counter=counter+1
  
  #identificando si está fuera de control
  if(z<UCL1 | z>UCL2){
    
    vectorRL[numRLs] = counter
    numRLs = numRLs + 1
    counter = 0
    
  }
  
}

ARL=mean(vectorRL)

#numero medio de muestras hasta que hubo una señal
1/ARL
#1/arl es el valor de alfa.
