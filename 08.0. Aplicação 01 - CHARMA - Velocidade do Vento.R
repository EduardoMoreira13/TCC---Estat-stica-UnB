# Pacotes utilizados
library(bgev)        # distribuicao bgev
library(readxl)      # ler Excel
library(tidyverse)   # manipulacao de dataframe
library(forecast)    # acoes com series temporais
library(openxlsx)    # exportar para Excel
source("BGEV_fit.r") # ler a funcao BGEV


# Leitura do Banco de dados 
wind <- read_excel("08.1. Aplicação 01 - CHARMA - Velocidade do Vento.xlsx", col_types = c("date", "numeric"))

# Dados de treinamento e validacao - IN e OUT Sample
training <- wind$average_wind_speed[1:62]
test <- wind$average_wind_speed[65:74] # obs 63 e 64 - NA

# Analise descritiva
summary(training)
sd(training)
var(training)


# Grafico da Serie Temporal
serie_temporal <- ts(training, start = c(2009, 12), frequency = 12)

serie_temporal %>% autoplot() + 
  geom_line(color = "#325ea8", linewidth = 1) +
  xlab("\n Tempo (meses)") +
  ylab("Velocidade Média do Vento (m/s) \n") +
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme_minimal()

serie_temporal %>% plot()  # opcao mais simples


# Criar o histograma - Densidade

sequencia = seq(min(wind$average_wind_speed, na.rm = T), max(wind$average_wind_speed, na.rm = T), length.out = 168)
curvas <- data.frame(
  x = rep(sequencia, 1),
  densidade = c(
    dbgev(sequencia, mu = 3.2, sigma = 0.6, xi = 0.1, delta = 0)
  ),
  distrib = factor(rep(c("μ = 9,00\n\nσ = 6,50\n\nξ = -0,45\n\nδ = 0,00\n"), each = 168))
)

# Criar o gráfico com ggplot2



ggplot() +
  # Adicionar histograma dos dados
  geom_histogram(aes(x = wind$average_wind_speed, y = after_stat(density)), 
                 binwidth = 0.25, fill = "#325ea8", color = "black") +
  
  # Adicionar curvas de distribuição BGEV
  geom_line(data = curvas, aes(x = x, y = densidade, color = distrib),
            color = "#d10a42",linewidth = 1) +
  
  # Adicionar títulos e rótulos aos eixos
  labs(
    title = "Histograma dos Dados com Curva Simulada da Distribuição BGEV \n",
    x = expression("\n Velocidade Média do Vento (m/s)"),
    y = "Densidade \n",
    color = "Parâmetros"
  ) +
  
  # Melhorar tema
  theme_minimal()


# DECOMPOSICAO DA SERIE TEMPORAL VIA STL (OU MSTL)

stl(serie_temporal, s.window=12) %>% plot( main='s.window=12')
mstl(serie_temporal, s.window = "periodic") %>% plot()

mstl(serie_temporal, s.window = "periodic") %>% autoplot() +
  geom_line(color = c("black"), linewidth = 1) +
  theme_minimal()


# Avaliando tendencia e sazonalidade
ndiffs(serie_temporal)
nsdiffs(serie_temporal)
# diff(serie_temporal) %>% nsdiffs() se tiver tendência


# COVARIAVEL DE SAZONALIDADE
x <- ts(training, start= c(2009, 12), frequency = 12)
saz <- decompose(x)
c <- saz$seasonal    # treinamento - IN SAMPLE
xreg <- as.vector(c)
c_hat <- xreg[3:14]  # validacao - OUT SAMPLE


##############################################################

# Ajuste do Modelo - BGEV-ARMAX(1,0) com covariavel sazonalidade
fit_training <- try(BGEV.ajuste(y = serie_temporal, 
                                ar = c(1), ma = NA, 
                                X = c, X_hat = c_hat, 
                                h1 = 12, link_fun ="identity",
                                graf = 0, show = 1, 
                                xi_bgev = 0.65))


# Residuos quantilicos
par(mfrow=c(1,1))
Residuo_quantil <- fit_training$residual
plot(Residuo_quantil)
par(mfrow=c(1,3))
plot(Residuo_quantil)
qqnorm(Residuo_quantil); qqline(Residuo_quantil)
acf(Residuo_quantil, lag.max = 12*5)
pacf(Residuo_quantil, lag.max = 12*5)
hist(Residuo_quantil)

# verificando estacionariedade
tseries::kpss.test(Residuo_quantil)

# verificando independencia
Box.test(Residuo_quantil,lag = 15, type = "Ljung-Box")
Box.test(Residuo_quantil,lag = 20, type = "Ljung-Box")

# Normalidade dos residuos quantilicos
shapiro.test(Residuo_quantil)

# Residuos - diferença entre previsto e observado
par(mfrow=c(1,1))
Residuos <- (training - fit_training$fitted)
Residuos <- ts(Residuos, start = c(2009, 12), frequency = 12)
plot(Residuos)

par(mfrow=c(1,2))
plot(Residuos)
qqnorm(Residuos); qqline(Residuos)
acf(Residuos[2:62], lag.max = 12*5)
pacf(Residuos[2:62], lag.max = 12*5)
par(mfrow=c(1,1))
hist(Residuos)

# verificando estacionariedade
tseries::kpss.test(Residuos)

# verificando independencia
Box.test(Residuos,lag = 15, type = "Ljung-Box")
Box.test(Residuos,lag = 20, type = "Ljung-Box")

# Normalidade dos residuos
shapiro.test(Residuos)


# Medidas de acuracia do modelo BGEV-ARMAX (in-sample)
dif_BGEV <- (training - fit_training$fitted) %>% as.data.frame() %>% remove_missing() %>% unlist()
contagem_NA <- sum(is.na(fit_training$fitted))

(mse_BGEV <- (sum(dif_BGEV^2))/length(dif_BGEV))
(mape_BGEV <- sum(abs(dif_BGEV) / abs(training[(contagem_NA + 1):62])) / length(dif_BGEV))

forecast::accuracy(fit_training$fitted,training)

# Avaliando a capacidade de the predicao do modelo BGEV-ARMAX (out-of-sample)
predict_BGEV <- fit_training$forecast[3:12]

dif_BGEV_pred <- (test-predict_BGEV)
(mse_BGEVp <- (sum(dif_BGEV_pred^2))/length(dif_BGEV_pred))
(mape_BGEVp <- sum(abs(dif_BGEV_pred)/abs(test))/length(dif_BGEV_pred))

forecast::accuracy(predict_BGEV,test)


# Preparação de dados para os gráficos
ordem = wind$time
dados = data.frame(valores_reais = wind$average_wind_speed,
                   ajustados =  c(fit_training$fitted,fit_training$forecast),
                   ordem = ordem)


# Criando um gráfico de linhas com ggplot2
par(mfrow=c(1,1))
ggplot(dados, aes(x = ordem)) +
  geom_line(aes(y = valores_reais), color = "black", linewidth = 1, linetype = "solid") +
  geom_line(aes(y = ajustados), color = "red", linewidth = 1, linetype = "dashed") +
  labs(x = "Eixo X", y = "Eixo Y", title = "Gráfico com Duas Linhas") +
  scale_color_manual(values = c("Linha Azul" = "black", "Linha Vermelha" = "red")) +
  theme(legend.position = "right") +
  theme_minimal() 


#########################################################################

# modelo CHARMA(3,2) com covariaveis - ver artigo
source("charma_fit.r")
tau <- 0.5 # median

fit_charma <- try(charma.fit(y=training, ar=c(1,2,3), ma=c(1,2), 
                             X=c, X_hat = c_hat, h1 = 12, 
                             graf=0), T)

# modelo SARMA(2,0,3)(2,0,0) - ver artigo
sarma_training <- arima(x = training,order = c(2,0,3),
                        seasonal = list(order = c(2, 0, 0), period = 12))


# Preparacao para graficos
sarma_training_pred = as.numeric(predict(sarma_training, n.ahead=12)$pred)
par(mfrow=c(1,1))

# Gráfico 1 - serie temporal e BGEV
plot(wind$average_wind_speed, col="black", type="l", lwd = 2,
     ylim=c(2, (max(c(training, test))+0.75)) , axes =T, main="", xlab="\n Tempo (meses)", ylab="Velocidade Média do Vento (m/s) \n")
lines(c(fit_training$fitted, c(fit_training$forecast)), lty = 2, lwd = 2, col="#db0715")
legend("topleft",legend=c( "Observações","Modelo BGEV"),
       pt.bg="white", lty=c(1,2),lwd = c(2,2), col=c("black","#db0715"), bty="n" )
fim <- (end(wind$average_wind_speed)[1]+end(wind$average_wind_speed)[2]/12)-12
abline(v=fim,lty=2)


# Gráfico 2 - serie temporal e CHARMA
plot(wind$average_wind_speed, col="black", type="l", lwd = 2,
     ylim=c(2, (max(c(training, test))+0.75)) , axes =T, main="", xlab="\n Tempo (meses)", ylab="Velocidade Média do Vento (m/s) \n")
lines(c(fit_charma$fitted, c(fit_charma$forecast)), lty = 2, lwd = 2, col="#432bfc")
legend("topleft",legend=c( "Observações","Modelo CHARMA"),
       pt.bg="white", lty=c(1,2),lwd = c(2,2), col=c("black","#432bfc"), bty="n" )
fim <- (end(wind$average_wind_speed)[1]+end(wind$average_wind_speed)[2]/12)-12
abline(v=fim,lty=2)


# Gráfico 3 - serie temporal e SARMA
plot(wind$average_wind_speed, col="black", type="l", lwd = 2,
     ylim=c(2, (max(c(training, test))+0.75)) , axes =T, main="", xlab="\n Tempo (meses)", ylab="Velocidade Média do Vento (m/s) \n")
lines(c(fitted(sarma_training), c(sarma_training_pred)), lty = 2, lwd = 2, col="#06bd4f")
legend("topleft",legend=c( "Observações","Modelo SARMA"),
       pt.bg="white", lty=c(1,2),lwd = c(2,2), col=c("black","#06bd4f"), bty="n" )
fim <- (end(wind$average_wind_speed)[1]+end(wind$average_wind_speed)[2]/12)-12
abline(v=fim,lty=2)


# Gráfico 4 - serie temporal, BGEV e CHARMA
plot(wind$average_wind_speed, col="black", type="l", lwd = 2,
     ylim=c(2, (max(c(training, test))+0.75)) , axes =T, main="", xlab="\n Tempo (meses)", ylab="Velocidade Média do Vento (m/s) \n")
lines(c(fit_training$fitted, c(fit_training$forecast)), lty = 2, lwd = 2, col="#db0715")
lines(c(fit_charma$fitted, c(fit_charma$forecast)), lty = 2, lwd = 2, col="#432bfc")
legend("topleft",legend=c( "Observações","Modelo BGEV", "Modelo CHARMA"),
       pt.bg="white", lty=c(1,2,2),lwd = c(2,2), col=c("black","#db0715","#432bfc"), bty="n" )
fim <- (end(wind$average_wind_speed)[1]+end(wind$average_wind_speed)[2]/12)-12
abline(v=fim,lty=2)


# Gráficos no GGPLOT
tempo = wind$time
dados = data.frame(valores_reais = wind$average_wind_speed,
                   BGEV =  c(fit_training$fitted,fit_training$forecast),
                   CHARMA =  c(fit_charma$fitted,fit_charma$forecast),
                   ARMA =  c(fitted(sarma_training),sarma_training_pred),
                   tempo = tempo)


ggplot(dados, aes(x = tempo)) +
  geom_line(aes(y = valores_reais), color = "black", linewidth = 1, linetype = "solid") +
  geom_line(aes(y = BGEV), color = "red", linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = CHARMA), color = "#432bfc", linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = ARMA), color = "#06bd4f", linewidth = 1, linetype = "dashed") +
  labs(x = "Eixo X", y = "Eixo Y", title = "") +
  scale_color_manual(values = c("Observações" = "black", "BGEV" = "red",
                                "CHARMA" = "#432bfc", "ARMA" = "#06bd4f")) +
  theme(legend.position = "bottom") +
  theme_minimal() 


#########################################################################



# TESTE - Diferentes valores de Xi - encontrar os melhores
# candidatos em termos de AIC, AICc, BIC, MSE e MAPE

# Lista para armazenar os resultados do ajuste
valores_xi_bgev <- seq(-0.5, 2.0, by = 0.01)
resultados_AIC <- c(rep(0,length(valores_xi_bgev)))
resultados_AICc <- c(rep(0,length(valores_xi_bgev)))
resultados_BIC <- c(rep(0,length(valores_xi_bgev)))
resultados_mse_BGEV <- c(rep(0,length(valores_xi_bgev)))
resultados_mape_BGEV <- c(rep(0,length(valores_xi_bgev)))
resultados_mse_BGEVp <- c(rep(0,length(valores_xi_bgev)))
resultados_mape_BGEVp <- c(rep(0,length(valores_xi_bgev)))
xi_id = c(rep(0,length(valores_xi_bgev)))
contagem <- 1

ar_1 <- c(1,2,3)   # termos autorregressivos
ma_1 <- c(1)       # termos de médias móveis


for (xi in valores_xi_bgev) {
  fit_training <- try(BGEV.ajuste(y = serie_temporal, 
                                  ar = ar_1, 
                                  ma = ma_1, 
                                  X = c, 
                                  X_hat = c_hat, 
                                  h1 = 12, 
                                  link_fun = "identity", 
                                  graf = 0, 
                                  show = 0, 
                                  xi_bgev = xi), 
                      silent = TRUE)
  
  if (inherits(fit_training, "try-error")) {
    # Adiciona NA para todos os resultados em caso de erro
    resultados_AIC[contagem] <- NA
    resultados_AICc[contagem] <- NA
    resultados_BIC[contagem] <- NA
    resultados_mse_BGEV[contagem] <- NA
    resultados_mape_BGEV[contagem] <- NA
    resultados_mse_BGEVp[contagem] <- NA
    resultados_mape_BGEVp[contagem] <- NA
  } else {
    # Adiciona os resultados se o ajuste for bem-sucedido
    resultados_AIC[contagem] <- fit_training$aic
    resultados_AICc[contagem] <- fit_training$aicc
    resultados_BIC[contagem] <- fit_training$bic
    
    dif_BGEV <- (training - fit_training$fitted) %>% as.data.frame() %>% remove_missing() %>% unlist()
    contagem_NA <- sum(is.na(fit_training$fitted))
    
    resultados_mse_BGEV[contagem] <- (sum(dif_BGEV^2)) / length(dif_BGEV)
    resultados_mape_BGEV[contagem] <- sum(abs(dif_BGEV) / abs(training[(contagem_NA + 1):62])) / length(dif_BGEV)
    
    predict_BGEV <- fit_training$forecast[3:12]
    dif_BGEV_pred <- (test - predict_BGEV)
    
    resultados_mse_BGEVp[contagem] <- (sum(dif_BGEV_pred^2)) / length(dif_BGEV_pred)
    resultados_mape_BGEVp[contagem] <- sum(abs(dif_BGEV_pred) / abs(test)) / length(dif_BGEV_pred)
  }
  
  xi_id[contagem] = xi
  contagem <- contagem + 1
}


# Armazenando os resultados em um dataframe
dados_teste = data.frame(AIC = resultados_AIC, AICc = resultados_AICc, 
                         BIC = resultados_BIC, MSE_IN = resultados_mse_BGEV,
                         MAPE_IN = resultados_mape_BGEV, MSE_OUT = resultados_mse_BGEVp, 
                         MAPE_OUT = resultados_mape_BGEVp, Xi_ID = xi_id)


head(dados_teste) # ver resultados


# Salvar os dados lidos em um novo arquivo Excel
novo_arquivo_excel <- "dados_saida(1,0).xlsx"
write.xlsx(dados_teste, novo_arquivo_excel)


