# Umidade Relativa do Ar (%) - Mínimo Mensal


# Pacotes utilizados
library(bgev)        # distribuicao bgev
library(readxl)      # ler Excel
library(tidyverse)   # manipulacao de dataframe
library(forecast)    # acoes com series temporais
library(openxlsx)    # exportar para Excel
source("BGEV_fit.r") # ler a funcao BGEV


# Leitura do Banco de dados 
dados_MAN <- read_excel("06.1. Manaus - Umidade Relativa do Ar - Mínimo.xlsx") %>% 
             arrange(ano, mes)


# Análise descritiva e estudo de associação

summary(dados_MAN$`UMIDADE RELATIVA DO AR (%)`)
sd(dados_MAN$`UMIDADE RELATIVA DO AR (%)`)
var(dados_MAN$`UMIDADE RELATIVA DO AR (%)`)

cor(dados_MAN[1:8], method = "pearson")
cor(dados_MAN[1:8], method = "kendall")
cor(dados_MAN[1:8], method = "spearman")


# Gráfico de dispersão - umidade e temperatura do Ar - Bulbo Seco 

ggplot(dados_MAN, aes(y = `UMIDADE RELATIVA DO AR (%)`, 
                      x = `TEMPERATURA DO AR - BULBO SECO (C)`)) +
  geom_point(color = "#432bfc", size = 3, alpha = 0.8,
             fill = "#ed0909", shape = 21, stroke = 0.9) +  # Adiciona os pontos
  labs(title = "Gráfico de Dispersão",
       y = "Umidade Relativa do Ar Mínima (%) \n",
       x = "\n Temperatura do Ar - Bulbo Seco (°C)") +
  theme_minimal()  # Usar um tema minimalista


# Gráfico de dispersão - umidade e temperatura do Ponto de Orvalho 
ggplot(dados_MAN, aes(y = `UMIDADE RELATIVA DO AR (%)`, 
                       x = `TEMPERATURA DO PONTO DE ORVALHO (C)`)) +
  geom_point(color = "#432bfc", size = 3, alpha = 0.8,
             fill = "#ed0909", shape = 21, stroke = 0.9) +  # Adiciona os pontos
  labs(title = "Gráfico de Dispersão",
       y = "Umidade Relativa do Ar Mínima (%) \n",
       x = "\n Temperatura do Ponto de Orvalho (°C)") +
  theme_minimal() 


# Criar o histograma - Densidade e Frequencia
sequencia = seq(min(dados_MAN$`UMIDADE RELATIVA DO AR (%)`), max(dados_MAN$`UMIDADE RELATIVA DO AR (%)`), length.out = 168)
curvas <- data.frame(
  x = rep(sequencia, 1),
  densidade = c(
    dbgev(sequencia, mu = 39, sigma = 10, xi = -0.7, delta = 0)
  ),
  distrib = factor(rep(c("μ = 39,0\n\nσ = 10,00\n\nξ = -0,70\n\nδ = 0,00\n"), each = 168))
)

# Criar o gráfico com ggplot2
ggplot() +
  # Adicionar histograma dos dados
  geom_histogram(aes(x = dados_MAN$`UMIDADE RELATIVA DO AR (%)`, y = after_stat(density)), 
                 binwidth = 4, fill = "#325ea8", color = "black") +
  
  # Adicionar curvas de distribuição BGEV
  geom_line(data = curvas, aes(x = x, y = densidade, color = distrib),
            color = "#d10a42",linewidth = 1) +
  
  # Adicionar títulos e rótulos aos eixos
  labs(
    title = "Histograma dos Dados com Curva Simulada da Distribuição BGEV \n",
    x = expression("\n Umidade Relativa do Ar Mínima (%)"),
    y = "Densidade \n",
    color = "Parâmetros"
  ) +
  
  # Melhorar tema
  theme_minimal()



# Dados de treinamento e validacao - IN e OUT Sample
treinamento = dados_MAN %>% filter(ano <= 2020)
validacao = dados_MAN %>% filter(ano > 2020)

training <- treinamento$`UMIDADE RELATIVA DO AR (%)`
test <- validacao$`UMIDADE RELATIVA DO AR (%)`


# Grafico da Serie Temporal
serie_temporal <- ts(training, start = c(2010, 01), frequency = 12)

serie_temporal %>% autoplot() + 
  geom_line(color = "#325ea8", linewidth = 1.2) +
  xlab("\n Tempo (meses)") +
  ylab("Umidade Relativa do Ar Mínima (%) \n") +
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme_minimal()

serie_temporal %>% plot()  # opcao mais simples



# DECOMPOSICAO DA SERIE TEMPORAL VIA STL (OU MSTL)

stl(serie_temporal, s.window=12) %>% plot( main='s.window=12')
mstl(serie_temporal, s.window = "periodic") %>% plot()

mstl(serie_temporal, s.window = "periodic") %>% autoplot() +
  geom_line(color = c("black"), linewidth = 1) +
  theme_minimal()


# Avaliando tendencia e sazonalidade
ndiffs(serie_temporal)
nsdiffs(serie_temporal)


# COVARIAVEL DE SAZONALIDADE
x <- ts(training, start= c(2010, 01), frequency = 12)
saz <- decompose(x)
c <- saz$seasonal    # treinamento - IN SAMPLE
xreg <- as.vector(c)
c_hat <- xreg[1:36]  # validacao - OUT SAMPLE


##############################################################

# Ajuste do Modelo - BGEV-SARMAX(2,1) com covariavel sazonalidade
escala = 10
treinamento$sazonalidade = c
validacao$sazonalidade = c_hat
var_exp_treinamento = treinamento[,c(4,5,11)] %>% as.matrix()
var_exp_val = validacao[,c(4,5,11)] %>% as.matrix()


fit_training <- try(BGEV.ajuste(y = training/escala, 
                                ar = c(1,2), ma = c(1), 
                                X = var_exp_treinamento, 
                                X_hat = var_exp_val, 
                                h1 = 36, link_fun ="identity",
                                graf = 0, show = 1, 
                                xi_bgev = 0.87))


# Residuos quantilicos
par(mfrow=c(1,1))
Residuo_quantil <- fit_training$residual * escala
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
Residuos <- (training - fit_training$fitted * escala)
Residuos <- ts(Residuos, start = c(2010, 01), frequency = 12)
plot(Residuos)

par(mfrow=c(1,2))
plot(Residuos)
qqnorm(Residuos); qqline(Residuos)
acf(Residuos[3:62], lag.max = 12*5)
pacf(Residuos[3:62], lag.max = 12*5)
par(mfrow=c(1,1))
hist(Residuos)

# verificando estacionariedade
tseries::kpss.test(Residuos)

# verificando independencia
Box.test(Residuos,lag = 15, type = "Ljung-Box")
Box.test(Residuos,lag = 20, type = "Ljung-Box")

# Normalidade dos residuos
shapiro.test(Residuos)


# Medidas de acuracia do modelo BGEV-SARMAX (in-sample)
dif_BGEV <- (training - fit_training$fitted * escala) %>% as.data.frame() %>% remove_missing() %>% unlist()
contagem_NA <- sum(is.na(fit_training$fitted))

(mse_BGEV <- (sum(dif_BGEV^2))/length(dif_BGEV))
(mape_BGEV <- 100 * sum(abs(dif_BGEV) / abs(training[(contagem_NA + 1):132])) / length(dif_BGEV))

sqrt(mse_BGEV)
forecast::accuracy(fit_training$fitted * escala,training)

# Avaliando a capacidade de the predicao do modelo BGEV-SARMAX (out-of-sample)
predict_BGEV <- fit_training$forecast

dif_BGEV_pred <- (test - predict_BGEV * escala)
dif_BGEV_pred <- ifelse(is.na(dif_BGEV_pred), 0, dif_BGEV_pred)
(mse_BGEVp <- (sum(dif_BGEV_pred^2))/length(dif_BGEV_pred))
(mape_BGEVp <- 100 * sum(abs(dif_BGEV_pred)/abs(test))/length(dif_BGEV_pred))

sqrt(mse_BGEVp)
forecast::accuracy(predict_BGEV * escala,test)


# Preparação de dados para os gráficos
ordem = seq(from = as.Date("2010-01-01"), to = as.Date("2023-12-31"), by = "month")
dados = data.frame(valores_reais = dados_MAN$`UMIDADE RELATIVA DO AR (%)`,
                   ajustados =  c(fit_training$fitted*escala,fit_training$forecast*escala),
                   ordem = ordem)


# Criando um gráfico de linhas com ggplot2
par(mfrow=c(1,1))
ggplot(dados, aes(x = ordem)) +
  geom_line(aes(y = valores_reais), color = "black", linewidth = 1, linetype = "solid") +
  geom_line(aes(y = ajustados), color = "red", linewidth = 1, linetype = "dashed") +
  labs(x = "Eixo X", y = "Eixo Y", title = " ") +
  scale_color_manual(values = c("Linha Azul" = "black", "Linha Vermelha" = "red")) +
  theme(legend.position = "right") +
  scale_x_date(breaks = seq(from = as.Date("2010-01-01"), to = as.Date("2024-12-31"), by = "2 year"),
               labels = scales::date_format("%Y")) +
  theme_minimal() 


#########################################################################


# Gráfico 1 - serie temporal e BGEV
plot(dados_MAN$`UMIDADE RELATIVA DO AR (%)`, col="black", type="l", lwd = 3,
     ylim=c(20, (max(c(training, test))+5)) , axes =T, main="", xlab="\n Tempo (meses)", ylab="Velocidade Média do Vento (m/s) \n")
lines(c(fit_training$fitted*escala, c(fit_training$forecast*escala)), lty = 2, lwd = 3, col="#db0715")
legend("topleft",legend=c( "Observações","Modelo BGEV"),
       pt.bg="white", lty=c(1,2),lwd = c(2,2), col=c("black","#b50707"), bty="n" )
fim <- (end(dados_MAN$`UMIDADE RELATIVA DO AR (%)`)[1]+end(dados_MAN$`UMIDADE RELATIVA DO AR (%)`)[2]/36)-36
abline(v=fim,lty=2, lwd = 2)


#########################################################################


# TESTE - Diferentes valores de Xi - encontrar os melhores
# candidatos em termos de AIC, AICc, BIC, MSE e MAPE

# Lista para armazenar os resultados do ajuste
valores_xi_bgev <- seq(-2.0, 2.0, by = 0.01)
resultados_AIC <- c(rep(0,length(valores_xi_bgev)))
resultados_AICc <- c(rep(0,length(valores_xi_bgev)))
resultados_BIC <- c(rep(0,length(valores_xi_bgev)))
resultados_mse_BGEV <- c(rep(0,length(valores_xi_bgev)))
resultados_mape_BGEV <- c(rep(0,length(valores_xi_bgev)))
resultados_mse_BGEVp <- c(rep(0,length(valores_xi_bgev)))
resultados_mape_BGEVp <- c(rep(0,length(valores_xi_bgev)))
xi_id = c(rep(0,length(valores_xi_bgev)))
contagem <- 1

escala = 10 # garante o funcionamento da função optim em BGEV.ajuste
treinamento$sazonalidade = c # sazonalidade - treinamento
validacao$sazonalidade = c_hat # sazonalidade - validação
var_exp_treinamento = treinamento[,c(4,5,11)] %>% as.matrix()
var_exp_val = validacao[,c(4,5,11)] %>% as.matrix()

ar_1 <- c(1,2,3)  # termos autorregressivos
ma_1 <- c(1,2,3)     # termos de médias móveis


for (xi in valores_xi_bgev) {
  fit_training <- try(BGEV.ajuste(y = serie_temporal/escala, 
                                  ar = ar_1, 
                                  ma = ma_1, 
                                  X = var_exp_treinamento, 
                                  X_hat = var_exp_val, 
                                  h1 = 36, 
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
    
    dif_BGEV <- (training - fit_training$fitted * escala) %>% as.data.frame() %>% remove_missing() %>% unlist()
    contagem_NA <- sum(is.na(fit_training$fitted))
    
    resultados_mse_BGEV[contagem] <- (sum(dif_BGEV^2)) / length(dif_BGEV)
    resultados_mape_BGEV[contagem] <- sum(abs(dif_BGEV) / abs(training[(contagem_NA + 1):132])) / length(dif_BGEV)
    
    predict_BGEV <- fit_training$forecast
    dif_BGEV_pred <- (test - predict_BGEV * escala)
    dif_BGEV_pred <- ifelse(is.na(dif_BGEV_pred), 0, dif_BGEV_pred)
    
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


view(dados_teste) # ver resultados


# Salvar os dados lidos em um novo arquivo Excel
novo_arquivo_excel <- "dados_saida(3,3).xlsx"
write.xlsx(dados_teste, novo_arquivo_excel)


