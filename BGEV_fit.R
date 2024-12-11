# Funcao para desenvolver modelos autorregressivos de medias moveis sem ou com
# váriaveis explicativas (regressão dinâmica) - distribuição BGEV


BGEV.ajuste <- function (y, ar = NA, ma = NA, link_fun ="identity", h1 = 0,
                         X = NA, X_hat = NA, graf = 0, show = 1, xi_bgev = 0)
  
  # PARÂMETROS DA FUNCAO BGEV.fit:
  
  # y - corresponde a serie temporal, ou seja, os dados da modelagem
  
  # ar - vetor que especifica os termos autorregressivos
  
  # ma - vetor que especifica os termos de medias moveis
  
  # link_fun - funcao de ligacao a ser usada - Default: identidade
  
  # h1 - parametro de previsão (horizonte de previsao).
  
  # X - matrix de variaveis explicativas (covariaveis)
  # in-sample - treinamento do modelo

  # X_hat - matrix de variaveis explicativas (covariaveis)
  # out-of-sample - validacao e previsao do modelo
  
  # graf - indica se os graficos para verificar a qualidade de ajuste
  # do modelo serao gerados: 0 = nao, 1 = sim e > 1 sim, com pdf das figuras

  # show - se = 1, printar as informações do modelo na tela

  # xi_bgev - indica qual o chute inicial para o parametro xi da BGEV
  # influencia na funcao quantil e na estimação por max verossimilhanca 

{
  prob_mediana <- 0.5 # probabilidade associada a mediana

  # reserva de memoria e funcao de ligacao
  z <- c()  # reserva de memoria
  link <- make.link(link_fun) # log ou identidade
  linkfun <- link$linkfun     # funcao de ligacao
  linkinv <- link$linkinv     # inversa da funcao de ligacao
  mu.eta <-  link$mu.eta      # derivada da funcao inversa em relacao a eta

  
  y <- as.vector(y)  # Converte y para um vetor
  ynew <- linkfun(y) # Aplica a função de ligação aos dados y
  
  p <- max(ar)          # maximo de termos autorregressivos
  q <- max(ma)          # maximo de termos de medias moveis
  n <- length(y)        # tamanho da serie temporal
  m <- max(p,q,na.rm=T) # maximo entre os termos de p e q
  p1 <- length(ar)      # tamanho - termos autorregressivos definidos
  q1 <- length(ma)      # tamanho - termos de medias moveis definidos
  y_prev <- c(rep(NA,(n+h1))) # reserva de memoria para as previsoes

  
  # Valores iniciais - inicializacao do modelo e matriz de design
  if(any(is.na(ar)==F)) # com termos autorretressivos (AR)
  {
    P <- matrix(rep(NA,(n-m)*p1),ncol=p1)
    
    for(i in 1:(n-m))
    {
      P[i,] <- ynew[i+m-ar] # matriz com termos autorregressivos
    }
    
    Z <- cbind(rep(1,(n-m)),P)   # matriz design
  }else{
    Z <- as.matrix(rep(1,(n-m))) # matriz design
  }
  
  if(any(is.na(X)==T)) # sem variáveis regressoras
  {
    x <- as.matrix(Z)  # matriz design
    Y <- y[(m+1):n]    # valores da serie
    Ynew = linkfun(Y)  # funcao de ligacao aplicada em Y
    ajuste = lm.fit(x, Ynew) # ajuste de um modelo linear inicial
    mqo = c(ajuste$coef) # coeficientes do modelo
    k = length(mqo)      # numero de coeficientes
    n1 = length(Y)       # tamanho dos dados não descartados no modelo
    xi1 = xi_bgev     # xi - parametro de forma e comportamento das caudas 
    delta1 = 0.5  # delta - parametro de forma
    sigma1 = 0.5  # sigma - parametro de precisao
    
  }else{  # com variaveis regressoras
    X_hat <- as.matrix(X_hat) # covariaveis
    X<-as.matrix(X)  # matriz design anterior
    x <- cbind(as.matrix(Z),X[(m+1):n,]) # matriz design completa
    Y <- y[(m+1):n]  # valores da serie temporal
    Ynew = linkfun(Y) # funcao de ligacao aplicada em Y
    ajuste = lm.fit(x, Ynew) # ajuste de um modelo linear inicial
    mqo = c(ajuste$coef)  # coeficientes do modelo
    k = length(mqo)       # numero de coeficientes
    n1 = length(Y)        # tamanho dos dados não descartados no modelo
    xi1 = xi_bgev    # xi - parametro de forma e comportamento das caudas 
    delta1 = 0.5 # delta - parametro de forma
    sigma1 = 0.5 # sigma - parametro de precisao
  }
  
  
  ##########################################################
  ########## BGEV-ARMA model: sem covariaveis ########## 
  
  if(any(is.na(ar)==F) && any(is.na(ma)==F) && any(is.na(X)==T))
  {
    # inicializacao de todos os parametros - chute inicial
    # AR - baseado no que foi obtido em lm.fit()
    reg <- c(mqo, rep(0,q1), sigma1, xi1, delta1)
    
    loglik <- function(z) 
    { 
      # atribuicao de parametros baseado nos chutes iniciais
      beta0 <- z[1]
      phi <- z[2:(p1+1)] 
      theta <- z[(p1+2):(p1+q1+1)]
      sigma <- z[p1+q1+2]
      xi <- z[p1+q1+3]
      delta <- z[p1+q1+4]
      
      error <- rep(0,n) # E(erro) = 0 
      eta <- rep(NA,n)  # reserva de memoria
      
      for(i in (m+1):n) # equacao do modelo (eta) e erros obtidos
      {
        eta[i] <- beta0 + (phi%*%ynew[i-ar]) + (theta%*%error[i-ma])
        error[i] <- ynew[i]-eta[i] 
      }
      
      ETA <- linkinv(eta[(m+1):n]) # inversa da função do preditor linear
      y1 <- y[(m+1):n] # valores da serie temporal
      
      # valor de mu em que eta corresponde a mediana
      # Obs: mu nao e a media na BGEV
      if(xi1 == 0){
        mu1 <- ETA - (-sigma * log(- log(prob_mediana)))^(1/(delta + 1))
      }else{
        mu1 <- ETA - sign((1/xi1) * (((-log(prob_mediana))^(-xi)) - 1)) *
          abs((sigma/xi1) * (((-log(prob_mediana))^(-xi)) - 1))^(1/(delta + 1))
      }
      
      # retorna a log verossimilhança
      # Obs: uso do pacote bgev para usar a funcao bgev
      ll <- suppressWarnings( log( dbgev(y1, mu = mu1, delta = delta1, 
                                         sigma = sigma1, xi = xi1) ) )
      sum(ll)
    } 
    
    # apresentacao dos nomes dos coeficientes
    names_phi <- c(paste("phi",ar,sep=""))
    names_theta <- c(paste("theta",ma,sep=""))
    names_par <- c("beta0",names_phi,names_theta,"sigma","xi","delta") 
    
    # uso da funcao optim para otimizar o modelo
    # estimação por maxima verossimilhanca baseado na BGEV
    opt <- optim(reg, loglik, hessian=F, method = "BFGS", 
                 control = list(fnscale = -1))
    
    z <- c() # reserva de memoria
    z$conv <- opt$conv # convergencia do algoritmo
    coef <- (opt$par)[1:(p1+q1+4)] # coeficientes
    names(coef) <- names_par # nomes dos coeficientes
    z$coeff <- coef # armazenando os coeficientes
    
    
    # atribuicao de parametros baseado no modelo otimizado
    beta0 <-coef[1]
    phi <- coef[2:(p1+1)]
    theta <- coef[(p1+2):(p1+q1+1)]
    sigma <- coef[p1+q1+2]
    xi <- coef[p1+q1+3]
    delta <- coef[p1+q1+4]
    
    z$beta0 <- beta0
    z$phi <- phi
    z$theta <- theta
    z$sigma <- sigma
    z$xi <- xi
    z$delta <- delta
    
    errorhat <- rep(0,n) # E(erro) = 0 
    etahat <- rep(NA,n)  # reserva de memoria
    
    # equacao do modelo (eta) e erros estimados
    # modelo otimizado e novos coeficientes
    for(i in (m+1):n) 
    {
      etahat[i] <- beta0 + (phi%*%ynew[i-ar]) + (theta%*%errorhat[i-ma])
      errorhat[i] <- ynew[i]-etahat[i] # escala do preditor
    }
    
    medianhat <- linkinv(etahat[(m+1):n]) # inversa da função do preditor linear
    y1 <- y[(m+1):n] # valores da série temporal
    
    # valores ajustados do modelo
    z$fitted <- ts(c(rep(NA,m),medianhat),start=start(y),frequency=frequency(y))
    
    # valores de eta estimados
    z$etahat <- etahat
    
    # erros estimados do modelo
    z$errorhat <- errorhat
    
    
    #### PREVISOES #### 
    ynew_prev <- c(ynew,rep(NA,h1))
    y_prev[1:n] <- z$fitted
    
    if(h1 != 0){
      for(i in 1:h1)
      {
        ynew_prev[n+i] <- beta0 + (phi%*%ynew_prev[n+i-ar]) + (theta%*%errorhat[n+i-ma])
        y_prev[n+i] <- linkinv(ynew_prev[n+i])
        errorhat[n+i] <- 0 # valor esperado
      }
    }
    
  }
  
  
  #######################################################################
  ##### BGEV-AR model: sem covariaveis e termos de medias moveis ########
  
  if(any(is.na(ar)==F) && any(is.na(ma)==T) && any(is.na(X)==T))
  {
    # inicializacao de todos os parametros - chute inicial
    # AR - baseado no que foi obtido em lm.fit()
    q1<-0
    reg <- c(mqo, sigma1, xi1, delta1)
    
    loglik <- function(z)
    {
      # atribuicao de parametros baseado nos chutes iniciais
      beta0 <- z[1]
      phi = z[2:(p1+1)]
      sigma <- z[p1+2]
      xi <- z[p1+3]
      delta <- z[p1+4]
      
      eta<-rep(NA,n) # reserva de memoria
      
      for(i in (m+1):n) # equacao do modelo (eta)
      {
        eta[i]<- beta0 + (phi%*%ynew[i-ar])
      }
      
      ETA <- linkinv(eta[(m+1):n]) # inversa da função do preditor linear
      y1 <- y[(m+1):n] # valores da serie temporal
      
      # valor de mu em que eta corresponde a mediana
      # Obs: mu nao e a media na BGEV
      if(xi1 == 0){
        mu1 <- ETA - (-sigma * log(- log(prob_mediana)))^(1/(delta + 1))
      }else{
        mu1 <- ETA - sign((1/xi1) * (((-log(prob_mediana))^(-xi)) - 1)) *
          abs((sigma/xi1) * (((-log(prob_mediana))^(-xi)) - 1))^(1/(delta + 1))
      }
      
      # retorna a log verossimilhança
      # Obs: uso do pacote bgev para usar a funcao bgev
      ll <- suppressWarnings( log( dbgev(y1, mu = mu1, delta = delta1, 
                                         sigma = sigma1, xi = xi1) ) )
      sum(ll)
    }
    
    # apresentacao dos nomes dos coeficientes
    names_phi<-c(paste("phi",ar,sep=""))
    names_par <- c("beta0",names_phi,"sigma","xi","delta")
    
    # uso da funcao optim para otimizar o modelo
    # estimação por maxima verossimilhanca baseado na BGEV
    opt <- optim(reg, loglik, hessian=F, method = "BFGS", 
                 control = list(fnscale = -1))
  
    z <- c() # reserva de memoria
    z$conv <- opt$conv # convergencia do algoritmo
    coef <-c(opt$par)[1:(p1+4)] # coeficientes
    names(coef) <- names_par # nomes dos coeficientes
    z$coeff <- coef # armazenando os coeficientes
    
    # atribuicao de parametros baseado no modelo otimizado
    beta0 <- coef[1]
    phi <- coef[2:(p1+1)]
    sigma <- coef[p1+2]
    xi <- coef[p1+3]
    delta <- coef[p1+4]
    
    z$beta0 <- beta0
    z$phi <- phi
    z$sigma <- sigma
    z$xi <- xi
    z$delta <- delta
    
    errorhat<-rep(0,n) # E(erro) = 0 
    etahat<-rep(NA,n)  # reserva de memoria
    
    
    # equacao do modelo (eta) e erros estimados
    # modelo otimizado e novos coeficientes
    for(i in (m+1):n)
    {
      etahat[i] <- beta0 + (phi%*%ynew[i-ar])
      errorhat[i] <- ynew[i] - etahat[i] # escala do preditor
    }
    
    medianhat <- linkinv(etahat[(m+1):n]) # inversa da função do preditor linear
    y1 <- y[(m+1):n] # valores da série temporal
    
    # valores ajustados do modelo
    z$fitted <- ts(c(rep(NA,m),medianhat),start=start(y),frequency=frequency(y))
    
    # valores de eta estimados
    z$etahat <- etahat
    
    # erros estimados do modelo
    z$errorhat <- errorhat

    #### PREVISOES ####
    ynew_prev <- c(ynew,rep(NA,h1))
    y_prev[1:n] <- z$fitted
    
    if(h1 != 0){
      for(i in 1:h1)
      {
        ynew_prev[n+i] <- beta0 + (phi%*%ynew_prev[n+i-ar])
        y_prev[n+i] <- linkinv(ynew_prev[n+i])
      }
    }
  }
  
  
  #####################################################################
  ##### BGEV-MA model: sem covariaveis e termos autorregressivos #####
  
  if(any(is.na(ar)==T) && any(is.na(ma)==F) && any(is.na(X)==T))
  {
    # inicializacao de todos os parametros - chute inicial
    # AR - apenas o intercepto que foi obtido em lm.fit()
    p1 <- 0
    reg <- c(mqo,rep(0,q1), sigma1, xi1, delta1)
    
    loglik <- function(z)
    {
      # atribuicao de parametros baseado nos chutes iniciais
      beta0 <- z[1]
      theta <- z[2:(q1+1)]
      sigma <- z[q1+2]
      xi <- z[q1+3]
      delta <- z[q1+4]
      
      eta <- error <- rep(0,n) # reserva de memoria
      
      for(i in (m+1):n) # equacao do modelo (eta) e erros obtidos
      {
        eta[i] <- beta0 + (theta%*%error[i-ma])
        error[i] <- ynew[i] - eta[i] # escala do preditor
      }
      
      ETA <- linkinv(eta[(m+1):n]) # inversa da função do preditor linear
      y1 <- y[(m+1):n] # valores da serie temporal
      
      # valor de mu em que eta corresponde a mediana
      # Obs: mu nao e a media na BGEV
      if(xi1 == 0){
        mu1 <- ETA - (-sigma * log(- log(prob_mediana)))^(1/(delta + 1))
      }else{
        mu1 <- ETA - sign((1/xi1) * (((-log(prob_mediana))^(-xi)) - 1)) *
          abs((sigma/xi1) * (((-log(prob_mediana))^(-xi)) - 1))^(1/(delta + 1))
      }
      
      # retorna a log verossimilhança
      # Obs: uso do pacote bgev para usar a funcao bgev
      ll <- suppressWarnings( log( dbgev(y1, mu = mu1, delta = delta1, 
                                         sigma = sigma1, xi = xi1) ) )
      sum(ll)
    }
    
    # apresentacao dos nomes dos coeficientes
    names_theta<-c(paste("theta",ma,sep=""))
    names_par <- c("beta0",names_theta,"sigma","xi","delta")
    
    # uso da funcao optim para otimizar o modelo
    # estimação por maxima verossimilhanca baseado na BGEV
    opt <- optim(reg, loglik, hessian=F, method = "BFGS", 
                 control = list(fnscale = -1))
    
    z <- c() # reserva de memoria
    z$conv <- opt$conv # convergencia do algoritmo
    coef <- (opt$par)[1:(q1+4)] # coeficientes
    names(coef) <- names_par # nomes dos coeficientes
    z$coeff <- coef # armazenando os coeficientes
    
    # atribuicao de parametros baseado no modelo otimizado
    beta0 <-coef[1]
    theta <- coef[2:(q1+1)]
    sigma <- coef[q1+2]
    xi <- coef[q1+3]
    delta <- coef[q1+4]
    
    z$beta0 <- beta0
    z$theta <- theta
    z$sigma <- sigma
    z$xi <- xi
    z$delta <- delta
    
    errorhat <- rep(0,n) # E(erro) = 0 
    etahat <- rep(NA,n) # reserva de memoria
    
    # equacao do modelo (eta) e erros estimados
    # modelo otimizado e novos coeficientes
    for(i in (m+1):n)
    {
      etahat[i] <- beta0 + (theta%*%errorhat[i-ma])
      errorhat[i] <- ynew[i] - etahat[i] # escala do preditor
    }
    medianhat <- linkinv(etahat[(m+1):n])  # inversa da função do preditor linear
    y1 <- y[(m+1):n] # valores da série temporal
    
    # valores ajustados do modelo
    z$fitted <- ts(c(rep(NA,m),medianhat),start=start(y),frequency=frequency(y))
    
    # valores de eta estimados
    z$etahat <- etahat
    
    # erros estimados do modelo
    z$errorhat <- errorhat
    
    #### PREVISOES ####
    ynew_prev <- c(ynew,rep(NA,h1))
    y_prev[1:n] <- z$fitted
    
    
    if(h1 != 0){
      for(i in 1:h1)
      {
        ynew_prev[n+i] <- beta0 + (theta%*%errorhat[n+i-ma])
        y_prev[n+i] <- linkinv(ynew_prev[n+i])
        errorhat[n+i] <- 0 # valor esperado
      }
    }
    
    
  }
  
  
  ##########################################################
  ########### BGEV-ARMAX model: modelo completo ########### 
  
  if(any(is.na(ar)==F) && any(is.na(ma)==F) && any(is.na(X)==F))
  { 
    # inicializacao de todos os parametros - chute inicial
    # AR e Covariaveis - baseado no que foi obtido em lm.fit()
    beta1 <- mqo[(p1+2):length(mqo)]
    reg <- c(mqo[1:(p1+1)], rep(0,q1), sigma1, xi1, delta1, beta1)
    
    loglik <- function(z) 
    {
      # atribuicao de parametros baseado nos chutes iniciais
      beta0 <- z[1]
      phi <- z[2:(p1+1)] 
      theta <- z[(p1+2):(p1+q1+1)]
      sigma <- z[p1+q1+2] # precision parameter
      xi <- z[p1+q1+3]
      delta <- z[p1+q1+4]
      beta <- z[(p1+q1+5):length(z)]
      
      error <- rep(0,n) # E(erro) = 0 
      eta <- rep(NA,n)  # reserva de memoria
      
      for(i in (m+1):n) # equacao do modelo (eta) e erros obtidos
      { 
        eta[i] <- beta0 + X[i,]%*%as.matrix(beta) + (phi%*%(ynew[i-ar]-X[i-ar,]%*%as.matrix(beta) )) + (theta%*%error[i-ma])
        error[i] <- ynew[i]-eta[i] # escala do preditor
      }
      
      ETA <- linkinv(eta[(m+1):n]) # inversa da função do preditor linear
      y1 <- y[(m+1):n] # valores da serie temporal
      
      # valor de mu em que eta corresponde a mediana
      # Obs: mu nao e a media na BGEV    
      if(xi1 == 0){
        mu1 <- ETA - (-sigma * log(- log(prob_mediana)))^(1/(delta + 1))
      }else{
        mu1 <- ETA - sign((1/xi1) * (((-log(prob_mediana))^(-xi)) - 1)) *
          abs((sigma/xi1) * (((-log(prob_mediana))^(-xi)) - 1))^(1/(delta + 1))
      }
      
      # retorna a log verossimilhança
      # Obs: uso do pacote bgev para usar a funcao bgev
      ll <- suppressWarnings( log( dbgev(y1, mu = mu1, delta = delta1, 
                                         sigma = sigma1, xi = xi1) ) )
      sum(ll)
    } 
    
    # apresentacao dos nomes dos coeficientes
    size.beta <- seq(1:length(X[1,]))
    names_beta <- c(paste("beta",size.beta,sep=""))
    names_phi <- c(paste("phi",ar,sep=""))
    names_theta <- c(paste("theta",ma,sep=""))
    names_par <- c("beta0",names_phi,names_theta,"sigma","xi","delta",names_beta)
    
    # uso da funcao optim para otimizar o modelo
    # estimação por maxima verossimilhanca baseado na BGEV
    opt <- optim(reg, loglik, hessian=F, method = "BFGS", 
                 control = list(fnscale = -1))
    
    z <- c() # reserva de memoria
    z$conv <- opt$conv # convergencia do algoritmo
    coef <- (opt$par)[1:(p1+q1+4+ncol(X))]  # coeficientes
    names(coef) <- names_par # nomes dos coeficientes
    z$coeff <- coef # armazenando os coeficientes
    
    # atribuicao de parametros baseado no modelo otimizado
    beta0 <-coef[1]
    phi <- coef[2:(p1+1)]
    theta <- coef[(p1+2):(p1+q1+1)]
    sigma <- coef[p1+q1+2] 
    xi <- coef[p1+q1+3]
    delta <- coef[p1+q1+4] 
    beta <- coef[(p1+q1+5):length(coef)]
    
    z$beta0 <- beta0
    z$phi <- phi
    z$theta <- theta
    z$sigma <- sigma
    z$xi <- xi
    z$delta <- delta
    z$beta <- beta
    
    errorhat <- rep(0,n) # E(erro) = 0 
    etahat <- rep(NA,n) # reserva de memoria
    
    # equacao do modelo (eta) e erros estimados
    # modelo otimizado e novos coeficientes
    for(i in (m+1):n) 
    {
      etahat[i] <- beta0 + X[i,]%*%as.matrix(beta) + (phi%*%(ynew[i-ar]-X[i-ar,]%*%as.matrix(beta) )) + (theta%*%errorhat[i-ma])
      errorhat[i] <- ynew[i] - etahat[i] # escala do preditor
    }
    
    medianhat <- linkinv(etahat[(m+1):n]) # inversa da função do preditor linear
    y1 <- y[(m+1):n] # valores da série temporal
    
    # valores ajustados do modelo
    z$fitted <- ts(c(rep(NA,m),medianhat),start=start(y),frequency=frequency(y))
    
    # valores de eta estimados
    z$etahat <- etahat
    
    # erros estimados do modelo
    z$errorhat <- errorhat
    
    
    #### PREVISOES #### 
    ynew_prev <- c(ynew,rep(NA,h1))
    y_prev[1:n] <- z$fitted
    
    if(h1 != 0){
      X_prev<- rbind(X,X_hat)
      
      for(i in 1:h1)
      {
        ynew_prev[n+i] <- beta0 + X_prev[n+i,]%*%as.matrix(beta) + (phi%*%(ynew_prev[n+i-ar]-X_prev[n+i-ar,]%*%as.matrix(beta) )) + (theta%*%errorhat[n+i-ma])
        y_prev[n+i] <- linkinv(ynew_prev[n+i])
        errorhat[n+i] <- 0 # valor esperado
      }
    }
    
     
  }
  
  
  #################################################################
  ######### BGEV-ARX model: sem termos de medias moveis ########## 
  
  if(any(is.na(ar)==F) && any(is.na(ma)==T) && any(is.na(X)==F))
  { 
    # inicializacao de todos os parametros - chute inicial
    # AR e Covariaveis - baseado no que foi obtido em lm.fit()
    q1 <- 0
    beta1 <- mqo[(p1+2):length(mqo)]
    reg <- c(mqo[1:(p1+1)], sigma1, xi1, delta1, beta1)
    
    loglik <- function(z) 
    {
      # atribuicao de parametros baseado nos chutes iniciais
      beta0 <- z[1]
      phi <- z[2:(p1+1)] 
      sigma <- z[p1+2] # precision parameter
      xi <- z[p1+p1+3]
      delta <- z[p1+4]
      beta <- z[(p1+5):length(z)]
      
      error <- rep(0,n) # E(erro) = 0 
      eta <- rep(NA,n)  # reserva de memoria
      
      for(i in (m+1):n) # equacao do modelo (eta) e erros obtidos
      {
        eta[i] <- beta0 + X[i,]%*%as.matrix(beta) + (phi%*%(ynew[i-ar]-X[i-ar,]%*%as.matrix(beta) )) 
        error[i] <- ynew[i] - eta[i] 
      }
      
      ETA <- linkinv(eta[(m+1):n]) # inversa da função do preditor linear
      y1 <- y[(m+1):n] # valores da serie temporal
      
      # valor de mu em que eta corresponde a mediana
      # Obs: mu nao e a media na BGEV
      if(xi1 == 0){
        mu1 <- ETA - (-sigma * log(- log(prob_mediana)))^(1/(delta + 1))
      }else{
        mu1 <- ETA - sign((1/xi1) * (((-log(prob_mediana))^(-xi)) - 1)) *
          abs((sigma/xi1) * (((-log(prob_mediana))^(-xi)) - 1))^(1/(delta + 1))
      }
      
      # retorna a log verossimilhança
      # Obs: uso do pacote bgev para usar a funcao bgev
      ll <- suppressWarnings( log( dbgev(y1, mu = mu1, delta = delta1, 
                                         sigma = sigma1, xi = xi1) ) )
      sum(ll)
    }
    
    # apresentacao dos nomes dos coeficientes
    size.beta <- seq(1:length(X[1,])) 
    names_phi <- c(paste("phi",ar,sep=""))
    names_beta <- c(paste("beta",size.beta,sep=""))
    names_par <- c("beta0",names_phi,"sigma","xi","delta",names_beta)
    
    # uso da funcao optim para otimizar o modelo
    # estimação por maxima verossimilhanca baseado na BGEV
    opt <- optim(reg, loglik, hessian=F, method = "BFGS", 
                 control = list(fnscale = -1))
    
    z <- c() # reserva de memoria
    z$conv <- opt$conv # convergencia do algoritmo
    coef <- (opt$par)[1:(p1+4+ncol(X))] # coeficientes
    names(coef)<-names_par # nomes dos coeficientes
    z$coeff <- coef # armazenando os coeficientes
    
    # atribuicao de parametros baseado no modelo otimizado
    beta0 <-coef[1]
    phi <- coef[2:(p1+1)]
    sigma <- coef[p1+2] 
    xi <- coef[p1+3] 
    delta <- coef[p1+4] 
    beta <- coef[(p1+5):length(coef)]
    
    z$beta0 <- beta0
    z$phi <- phi
    z$sigma <- sigma
    z$xi <- xi
    z$delta <- delta
    z$beta <- beta
    
    errorhat <- rep(0,n)  # E(erro) = 0  
    etahat <- rep(NA,n)   # reserva de memoria
    
    # equacao do modelo (eta) e erros estimados
    # modelo otimizado e novos coeficientes
    for(i in (m+1):n){
      etahat[i] <- beta0 + X[i,]%*%as.matrix(beta) + (phi%*%(ynew[i-ar]-X[i-ar,]%*%as.matrix(beta) )) 
      errorhat[i] <- ynew[i] - etahat[i] # escala do preditor
    }
    
    medianhat <- linkinv(etahat[(m+1):n]) # inversa da função do preditor linear
    y1 <- y[(m+1):n] # valores da série temporal
    
    # valores ajustados do modelo
    z$fitted <- ts(c(rep(NA,m),medianhat),start=start(y),frequency=frequency(y))
    
    # valores de eta estimados
    z$etahat <- etahat
    
    # erros estimados do modelo
    z$errorhat <- errorhat
    
    #### PREVISOES #### 
    ynew_prev <- c(ynew,rep(NA,h1))
    y_prev[1:n] <- z$fitted
    
    if(h1 != 0){
      X_prev<- rbind(X,X_hat)
      
      for(i in 1:h1)
      {
        ynew_prev[n+i] <- beta0 + X_prev[n+i,]%*%as.matrix(beta) + (phi%*%(ynew_prev[n+i-ar]-X_prev[n+i-ar,]%*%as.matrix(beta) ))
        y_prev[n+i] <- linkinv(ynew_prev[n+i])
        errorhat[n+i] <- 0 # valor esperado
      }
    }
    
  }

  
  ##############################################################
  ######### BGEV-CMAX model: sem termos autorregressivos #######
  
  if(any(is.na(ar)==T) && any(is.na(ma)==F) && any(is.na(X)==F))
  {
    # inicializacao de todos os parametros - chute inicial
    # Covariaveis - baseado no que foi obtido em lm.fit()
    p1 <- 0
    beta1 <- mqo[(2):length(mqo)]
    reg <- c(mqo[1], rep(0,q1), sigma1, xi1, delta1, beta1)
    
    loglik <- function(z)
    {
      # atribuicao de parametros baseado nos chutes iniciais
      beta0 <- z[1]
      theta <- z[(2):(q1+1)]
      sigma <- z[q1+2]
      xi <- z[p1+q1+3]
      delta <- z[q1+4]
      beta <- z[(q1+5):length(z)]
      
      error <- rep(0,n) # E(erro) = 0 
      eta <- rep(NA,n)  # reserva de memoria
      
      for(i in (m+1):n) # equacao do modelo (eta) e erros obtidos
      {
        eta[i] <- beta0 + X[i,]%*%as.matrix(beta) + (theta%*%error[i-ma])
        error[i] <- ynew[i]-eta[i]
      }
      
      ETA <- linkinv(eta[(m+1):n]) # inversa da função do preditor linear
      y1 <- y[(m+1):n] # valores da serie temporal
      
      # valor de mu em que eta corresponde a mediana
      # Obs: mu nao e a media na BGEV
      if(xi1 == 0){
        mu1 <- ETA - (-sigma * log(- log(prob_mediana)))^(1/(delta + 1))
      }else{
        mu1 <- ETA - sign((1/xi1) * (((-log(prob_mediana))^(-xi)) - 1)) *
          abs((sigma/xi1) * (((-log(prob_mediana))^(-xi)) - 1))^(1/(delta + 1))
      }
      
      # retorna a log verossimilhança
      # Obs: uso do pacote bgev para usar a funcao bgev
      ll <- suppressWarnings( log( dbgev(y1, mu = mu1, delta = delta1, 
                                         sigma = sigma1, xi = xi1) ) )
      sum(ll)
    }
    
    # apresentacao dos nomes dos coeficientes
    size.beta <- seq(1:length(X[1,])) 
    names_beta <- c(paste("beta",size.beta,sep=""))
    names_theta<-c(paste("theta",ma,sep=""))
    names_par <- c("beta0",names_theta,"sigma","xi","delta",names_beta)
    
    # uso da funcao optim para otimizar o modelo
    # estimação por maxima verossimilhanca baseado na BGEV
    opt <- optim(reg, loglik, hessian=F, method = "BFGS", 
                 control = list(fnscale = -1))
    
    z <- c() # reserva de memoria
    z$conv <- opt$conv # convergencia do algoritmo
    coef <- (opt$par)[1:(q1+4+ncol(X) )] # coeficientes
    names(coef) <- names_par # nomes dos coeficientes
    z$coeff <- coef # armazenando os coeficientes
    
    # atribuicao de parametros baseado no modelo otimizado
    beta0 <-coef[1]
    theta <- coef[(2):(q1+1)]
    sigma <- coef[q1+2] 
    xi <- coef[q1+3] 
    delta <- coef[q1+4] 
    beta <- coef[(q1+5):length(coef)]
    
    z$beta0 <- beta0
    z$sigma <- sigma
    z$xi <- xi
    z$delta <- delta
    z$beta <- beta
    
    errorhat <- rep(0,n) # E(erro) = 0 
    etahat <- rep(NA,n)  # reserva de memoria
    
    # equacao do modelo (eta) e erros estimados
    # modelo otimizado e novos coeficientes
    for(i in (m+1):n)
    {
      etahat[i] <- beta0 + X[i,]%*%as.matrix(beta) + (theta%*%errorhat[i-ma])
      errorhat[i] <- ynew[i]-etahat[i] # escala do preditor
    }
    
    medianhat <- linkinv(etahat[(m+1):n]) # inversa da função do preditor linear
    y1 <- y[(m+1):n] # valores da série temporal
    
    # valores ajustados do modelo
    z$fitted <- ts(c(rep(NA,m),medianhat),start=start(y),frequency=frequency(y))
    
    # valores de eta estimados
    z$etahat <- etahat
    
    # erros estimados do modelo
    z$errorhat <- errorhat
    

    #### PREVISOES #### 
    ynew_prev <- c(ynew,rep(NA,h1))
    y_prev[1:n] <- z$fitted
    
    
    if(h1 != 0){
      X_prev<- rbind(X,X_hat)
      
      for(i in 1:h1)
      {
        ynew_prev[n+i] <- beta0 + X_prev[n+i,]%*%as.matrix(beta) + (theta%*%errorhat[n+i-ma])
        y_prev[n+i] <- linkinv(ynew_prev[n+i])
        errorhat[n+i] <- 0 # valor esperado
      }
    }
     
  }

  ########################################################################
  
  z$serie <- y # armazenando os valores da serie temporal
  z$names <- names_par # armazenando os nomes dos parametros
  z$forecast <- y_prev[(n+1):(n+h1)] # armazenando as previsoes
  
  # armazenando os valores de mu (baseado na mediana) e dos residuos
  if(xi == 0){
    mu_hat <- medianhat - (-sigma * log(- log(prob_mediana)))^(1/(delta + 1))
  }else{
    mu_hat <- medianhat - sign((1/xi1) * (((-log(prob_mediana))^(-xi)) - 1)) *
      abs((sigma/xi1) * (((-log(prob_mediana))^(-xi)) - 1))^(1/(delta + 1))
  }

  z$residual = qnorm(pbgev(y[(m+1):n], xi=xi, mu=mu_hat, delta = delta, 
                           sigma = sigma))
  resid <- z$residual
  
  z$loglik <- opt$value # valor da logverossimilhanca
  z$counts <- as.numeric(opt$counts[1]) # numero de iteracoes/avaliacoes para convergencia
   
  # Calculo de metricas como AIC, AICc, BIC, Hannan-Quinn
  if(any(is.na(X)==F))
  {
    z$k<- (p1+q1+4+length(beta))
    z$aic <- 2*(z$k)-2*(z$loglik)
    z$aicc <- z$aic + (2 * z$k^2 + 2 * z$k) / (n - z$k - 1)
    z$bic <- log(n)*(z$k) -2*(z$loglik)
  }else{
    z$k<- (p1+q1+4)
    z$aic <- 2*(z$k)-2*(z$loglik)
    z$aicc <- z$aic + (2 * z$k^2 + 2 * z$k) / (n - z$k - 1)
    z$bic <- log(n)*(z$k) -2*(z$loglik)
  }
   
   # armazenando as informacoes do modelo ajustado  
   model_presentation <- cbind(round(z$coef,4))
   colnames(model_presentation)<-c("Estimate")
   z$model <- model_presentation
  
  # print das informacoes do modelo ajustado 
  if(show==1){
    print(model_presentation)
    print(" ",quote=F)
    print(c("Log-likelihood:",round(z$loglik,4)),quote=F)
    print(c("Number of iterations in BFGS optim:",z$counts),quote=F)
    print(c("AIC:",round(z$aic,4),"AICc:",round(z$aicc,4)," BIC:",round(z$bic,4)),quote=F)
    print("Residuals:",quote=F)
    print(summary(as.vector(resid)))
    
  }
    
    ###################################################
    ######### GRAFICOS ################################
    
  if(graf>0)
    {
      
    t<-seq(-5,n+6,by=1)
    
    # residuos x indice
    par(mfrow=c(1,1))
    par(mar=c(2.8, 2.7, 1.2, 1)) 
    par(mgp=c(1.7, 0.45, 0))
    plot(resid,main=" ",xlab="Index",ylab="Residuals", pch = "+",ylim=c(-4,4))
    lines(t,rep(-3,n+12),lty=2,col=1)
    lines(t,rep(3,n+12),lty=2,col=1)
    lines(t,rep(-2,n+12),lty=3,col=1)
    lines(t,rep(2,n+12),lty=3,col=1)
    
    # dados observados x valores ajustados
    max_y<- max(c(z$fitted,y),na.rm=T)
    min_y<- min(c(z$fitted,y),na.rm=T)
    plot(as.vector(z$fitted), as.vector(y), main=" ", pch = "+",
         xlab="Fitted values",ylab="Observed data",
         xlim=c(0.95*min_y,max_y*1.05),
         ylim=c(0.95*min_y,max_y*1.05))
    lines(c(-0.2,1.2),c(-0.2,1.2),lty=2)
    
    # valores ajustados x residuos
    plot(as.vector(z$fitted[(m+1):n]),as.vector(resid), main=" ", pch = "+",
         xlab="Fitted values",ylab="Residuals")
      #lines(c(-0.2,1.2),c(-0.2,1.2),lty=2)
    
    # densidade dos residuos e normalidade
    densidade<-density(resid)
    plot(densidade,ylab="density",main=" ")
    lines(densidade$x,dnorm(densidade$x),lty=2)
    legend("topleft",c("Exact distribution of residuals","Normal approximation"),#pch=vpch,
           pt.bg="white", lty=c(1,2), bty="n")
    
    # graficos acf e pacf
    acf(resid,ylab="ACF",xlab="Lag") 
    pacf(resid,ylab="PACF",xlab="Lag") 
    
    # qqplot
    max_r<- max(resid,na.rm=T)
    min_r<- min(resid,na.rm=T)
    qqnorm(resid, pch = "+",
           xlim=c(0.95*min_r,max_r*1.05),
           ylim=c(0.95*min_r,max_r*1.05),
           main="",xlab="Normal quantiles",
           ylab="Empirical quantiles")
    lines(c(-10,10),c(-10,10),lty=2)
    
    # serie original, ajustada e previsoes 
    par(mfrow=c(1,1))
    plot(y,type="l",ylab="Serie",xlab="Time")
    lines(z$fitted,col="red")
      
    fim<-end(y)[1]+end(y)[2]/12
      
    y_prev <- ts(y_prev, start=start(y), frequency=frequency(y))
    par(mfrow=c(1,1))
    plot(y_prev,type="l",col="red", ylim=c(min(y),max(y)),ylab="Serie",xlab="Time")
    abline(v=fim,lty=2)
    lines(y)
      
    w1<-5
    h1<-4
    
    # geracao de pdf das figuras
    if(graf>1)
      {
      postscript(file = "resid_v_ind.eps",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
      {
        par(mfrow=c(1,1))
        par(mar=c(2.8, 2.7, 1, 1))
        par(mgp=c(1.7, 0.45, 0))
        plot(resid,main=" ",xlab="Index",ylab="Residuals", pch = "+",ylim=c(-4,4))
        lines(t,rep(-3,n+12),lty=2,col=1)
        lines(t,rep(3,n+12),lty=2,col=1)
        lines(t,rep(-2,n+12),lty=3,col=1)
        lines(t,rep(2,n+12),lty=3,col=1)
        }
        dev.off()
        
      pdf(file = "resid_v_fitted.pdf",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
      {
        par(mfrow=c(1,1))
        par(mar=c(2.8, 2.7, 1, 1)) 
        par(mgp=c(1.7, 0.45, 0))
        plot(as.vector(z$fitted[(m+1):n]),as.vector(resid), main=" ", pch = "+",
             xlab="Fitted values",ylab="Residuals",ylim=c(-4,4))
        lines(t,rep(-3,n+12),lty=2,col=1)
        lines(t,rep(3,n+12),lty=2,col=1)
        lines(t,rep(-2,n+12),lty=3,col=1)
        lines(t,rep(2,n+12),lty=3,col=1)
      }
      dev.off()
        
      pdf(file = "obs_v_fit.pdf",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
      {
        par(mfrow=c(1,1))
        par(mar=c(2.8, 2.7, 1, 1)) 
        par(mgp=c(1.7, 0.45, 0))
        plot(as.vector(z$fitted), as.vector(y), main=" ", pch = "+",
             xlab="Fitted values",ylab="Observed data",
             xlim=c(0.95*min_y,max_y*1.05),
             ylim=c(0.95*min_y,max_y*1.05))
        lines(c(-0.2,1.2),c(-0.2,1.2),lty=2)
        }
        dev.off()
        
      pdf(file = "resid_density.pdf",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
      {
        par(mfrow=c(1,1))
        par(mar=c(1.5, 2.7, 1, 1)) 
        par(mgp=c(1.7, 0.45, 0))
          
        plot(densidade,ylab="Density",main=" ",xlab=" ",ylim=c(0,1.15*max(densidade$y)))
        lines(densidade$x,dnorm(densidade$x),lty=2)
        legend("topleft",c("Exact distribution of residuals","Normal approximation"),#pch=vpch,
               pt.bg="white", lty=c(1,2), bty="n")
      }
      dev.off()
        
      pdf(file = "resid_FAC.pdf",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
      {
        par(mfrow=c(1,1))
        par(mar=c(2.8, 2.7, 1, 1)) 
        par(mgp=c(1.7, 0.45, 0))
        acf(resid,ylab="ACF",xlab="Lag") 
      }
      dev.off()
        
      pdf(file = "resid_FACP.pdf",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
      {
        par(mfrow=c(1,1))
        par(mar=c(2.8, 2.7, 1, 1)) 
        par(mgp=c(1.7, 0.45, 0))
        pacf(resid,ylab="PACF",xlab="Lag")
      }
      dev.off()
        
      pdf(file = "qq_plot.pdf",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
      {  
        par(mfrow=c(1,1))
        par(mar=c(2.8, 2.7, 1, 1)) 
        par(mgp=c(1.7, 0.45, 0))
        qqnorm(resid, pch = "+",
               xlim=c(0.95*min_r,max_r*1.05),
               ylim=c(0.95*min_r,max_r*1.05),
               main="",xlab="Normal quantiles",ylab="Empirical quantiles")
        lines(c(-10,10),c(-10,10),lty=2)
      }
      dev.off()
        
      pdf(file = "adjusted.pdf",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
      {
        par(mfrow=c(1,1))
        par(mar=c(2.8, 2.7, 1, 1)) 
        par(mgp=c(1.7, 0.45, 0))
        plot(y,type="l",ylab="Serie",xlab="Time")
        lines(z$fitted,col="red")
      }
      dev.off()
        
      pdf(file = "forecast.pdf",horizontal=F,paper="special",width = 6, height = 4.7,family = "Times")
      {
        par(mfrow=c(1,1))
        par(mar=c(2.8, 2.7, 1, 1))
        par(mgp=c(1.7, 0.45, 0))
        plot(y_prev,type="l",lty=2,col="red", ylim=c(min(y),max(y)),ylab="RH",xlab="Times")
        abline(v=fim,lty=2)
        lines(y)
        legend("bottomleft",c("Observed data","Fitted and forecast values"),#pch=vpch,
               pt.bg="white", lty=c(1,2), bty="n",col=c(1,"red"))
      }
      dev.off()
        
      }    
    }  
    
    return(z)
}

  
  
  

