# Temperatura do Ar (°C) - Mínimo Semanal


# PACOTES UTILIZADOS ----

library(bgev)        # pacote da distribuicao BGEV
library(readxl)      # ler Excel
library(tidyverse)   # manipulacao de dataframe
library(forecast)    # acoes com series temporais
library(openxlsx)    # exportar para Excel
source("BGEV_fit.r") # ler a funcao BGEV - criada por Eduardo Moreira



# LEITURA E ADEQUACAO DA BASE DE DADOS ----

# Leitura dos dados do Rio Grande do Sul
dados_RS <- read_excel("07.2. PA - BGEV - Temperatura Mínima do Ar.xlsx") %>% arrange(ano,semana)



# ANALISE DESCRITIVA E ESTUDO DE ASSOCIACAO ----


# Medidas Resumo e Correlacao

summary(dados_RS$`TEMPERATURA DO AR - BULBO SECO (C)`)
sd(dados_RS$`TEMPERATURA DO AR - BULBO SECO (C)`)
var(dados_RS$`TEMPERATURA DO AR - BULBO SECO (C)`)

cor(dados_RS[3:11], method = "pearson")
cor(dados_RS[3:11], method = "kendall")
cor(dados_RS[3:11], method = "spearman")


# Gráfico de dispersão - temperatura minima do ar e umidade 
ggplot(dados_RS, aes(y = `TEMPERATURA DO AR - BULBO SECO (C)`, 
                       x = `UMIDADE RELATIVA DO AR (%)`)) +
  geom_point(color = "#432bfc", size = 3, alpha = 0.8,
             fill = "#ed0909", shape = 21, stroke = 0.9) +  # Adiciona os pontos
  labs(title = " ",
       y = "Temperatura Mínima Semanal (°C) \n",
       x = "\n Umidade Relativa do Ar (%)") +
 scale_x_continuous(limits = c(75, 105), breaks = seq(75, 105, by = 5)) +
  theme_minimal() 


# Gráfico de dispersão - temperatura minima do ar e temperatura do ponto de orvalho 
ggplot(dados_RS, aes(y = `TEMPERATURA DO AR - BULBO SECO (C)`, 
                     x = `TEMPERATURA DO PONTO DE ORVALHO (C)`)) +
  geom_point(color = "#432bfc", size = 3, alpha = 0.8,
             fill = "#ed0909", shape = 21, stroke = 0.9) +  # Adiciona os pontos
  labs(title = " ",
       y = "Temperatura Mínima Semanal (°C) \n",
       x = "\n Temperatura do Ponto de Orvalho (°C)") +
  theme_minimal() 


# Gráfico de dispersão - temperatura minima do ar e pressao atmosferica
ggplot(dados_RS, aes(y = `TEMPERATURA DO AR - BULBO SECO (C)`, 
                     x = `PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO (mB)`)) +
  geom_point(color = "#432bfc", size = 3, alpha = 0.8,
             fill = "#ed0909", shape = 21, stroke = 0.9) +  # Adiciona os pontos
  labs(title = " ",
       y = "Temperatura Mínima Semanal (°C) \n",
       x = "\n Pressão atmosférica (mB)") +
  theme_minimal() 


# Gráfico de dispersão - temperatura minima do ar e rajada maxima
ggplot(dados_RS, aes(y = `TEMPERATURA DO AR - BULBO SECO (C)`, 
                     x = `VENTO, RAJADA MAXIMA (m/s)`)) +
  geom_point(color = "#432bfc", size = 3, alpha = 0.8,
             fill = "#ed0909", shape = 21, stroke = 0.9) +  # Adiciona os pontos
  labs(title = " ",
       y = "Temperatura Mínima Semanal (°C) \n",
       x = "\n Vento Rajada Máxima (m/s)") +
  theme_minimal() 


# Gráfico de dispersão - temperatura minima do ar e velocidade do vento
ggplot(dados_RS, aes(y = `TEMPERATURA DO AR - BULBO SECO (C)`, 
                     x = `VENTO, VELOCIDADE (m/s)`)) +
  geom_point(color = "#432bfc", size = 3, alpha = 0.8,
             fill = "#ed0909", shape = 21, stroke = 0.9) +  # Adiciona os pontos
  labs(title = " ",
       y = "Temperatura Mínima Semanal (°C) \n",
       x = "\n Velocidade do Vento (m/s)") +
  theme_minimal()


# Criar o histograma - Densidade
sequencia = seq(min(dados_RS$`TEMPERATURA DO AR - BULBO SECO (C)`), max(dados_RS$`TEMPERATURA DO AR - BULBO SECO (C)`), length.out = 168)
curvas <- data.frame(
  x = rep(sequencia, 1),
  densidade = c(
    dbgev(sequencia, mu = 9, sigma = 6.5, xi = -0.45, delta = 0)
  ),
  distrib = factor(rep(c("μ = 9,00\n\nσ = 6,50\n\nξ = -0,45\n\nδ = 0,00\n"), each = 168))
)

# Criar o gráfico com ggplot2



ggplot() +
  # Adicionar histograma dos dados
  geom_histogram(aes(x = dados_RS$`TEMPERATURA DO AR - BULBO SECO (C)`, y = after_stat(density)), 
                 binwidth = 2, fill = "#325ea8", color = "black") +
  
  # Adicionar curvas de distribuição BGEV
  geom_line(data = curvas, aes(x = x, y = densidade, color = distrib),
            color = "#d10a42",linewidth = 1) +

  # Adicionar títulos e rótulos aos eixos
  labs(
    title = "Histograma dos Dados com Curva Simulada da Distribuição BGEV \n",
    x = expression("\n Temperatura Mínima Semanal (°C)"),
    y = "Densidade \n",
    color = "Parâmetros"
  ) +
  
  # Melhorar tema
  theme_minimal()



# DEFININDO A SERIE TEMPORAL - TREINAMENTO E VALIDACAO ----


# Dados de treinamento e validacao - IN e OUT Sample
treinamento = dados_RS %>% filter(ano <= 2022)
validacao = dados_RS %>% filter(ano > 2022)


# Final - treinamento e teste
training <- treinamento$`TEMPERATURA DO AR - BULBO SECO (C)`
test <- validacao$`TEMPERATURA DO AR - BULBO SECO (C)`


# Covariavel da sazonalidade
x <- ts(training, start= c(2020, 01), frequency = 52)
saz <- decompose(x)
c <- saz$seasonal    # treinamento - IN SAMPLE
xreg <- as.vector(c)
c_hat <- xreg[1:length(validacao$`TEMPERATURA DO AR - BULBO SECO (C)`)]  # validacao - OUT SAMPLE


escala = 10 # garante a convergencia do algoritmo em BGEV.ajuste
treinamento$sazonalidade = c  # sazonalidade (IN-SAMPLE)
validacao$sazonalidade = c_hat # sazonalidade (OUT-SAMPLE)


# matriz de dados das variaveis explicativas
var_exp_treinamento = treinamento[,c(4,7,8,12)] %>% as.matrix() 
var_exp_val = validacao[,c(4,7,8,12)] %>% as.matrix()


# Grafico da Serie Temporal
serie_temporal <- ts(training, start = c(2020, 01), frequency = 52)

serie_temporal %>% autoplot() + 
  geom_line(color = "#325ea8", linewidth = 1) +
  xlab("\n Tempo (meses)") +
  ylab("Temperatura Mínima Semanal (°C) \n") +
  theme(axis.text.x=element_text(angle=0, hjust=1)) +
  theme_minimal()

serie_temporal %>% plot()  # opcao mais simples


# Decomposicao da serie temporal via STL (OU MSTL)
stl(serie_temporal, s.window=12) %>% plot( main='s.window=12')
mstl(serie_temporal, s.window = "periodic") %>% plot()

mstl(serie_temporal, s.window = "periodic") %>% autoplot() +
  geom_line(color = c("black"), linewidth = 0.8) +
  theme_minimal()


# Avaliando tendencia e sazonalidade
ndiffs(serie_temporal)
nsdiffs(serie_temporal)


##############################################################


# MODELO BGEV-ARMAX - AJUSTE E RESIDUOS ----


# Ajuste do Modelo - BGEV-ARMAX(1,1) com covariavel sazonalidade

fit_training <- try(BGEV.ajuste(y = training/escala, 
                                ar = c(1), ma = c(1), 
                                X = var_exp_treinamento, 
                                X_hat = var_exp_val, 
                                h1 = length(validacao$`TEMPERATURA DO AR - BULBO SECO (C)`), 
                                link_fun ="identity",
                                graf = 0, show = 1, 
                                xi_bgev = -0.29))


# Residuos quantilicos do Modelo BGEV-ARMAX(1,1) com covariavel sazonalidade
par(mfrow=c(1,1))
Residuo_quantil <- fit_training$residual * escala
plot(Residuo_quantil)
par(mfrow=c(1,2))
plot(Residuo_quantil, ylab = "Resíduos Observados", xlab = "Índice", main = "Resíduos Quantílicos")
qqnorm(Residuo_quantil, ylab = "Resíduos Quantílicos", xlab = "Quantis Teóricos", main = "Q-Q Plot"); qqline(Residuo_quantil)
par(mfrow=c(1,2))
acf(Residuo_quantil, lag.max = 12*5, main = "Gráfico ACF")
pacf(Residuo_quantil, lag.max = 12*5, main = "Gráfico PACF")
par(mfrow=c(1,1))
hist(Residuo_quantil)

# verificando estacionariedade do Modelo BGEV-ARMAX(1,1) com covariavel sazonalidade
tseries::kpss.test(Residuo_quantil)

# verificando independencia do Modelo BGEV-ARMAX(1,1) com covariavel sazonalidade
Box.test(Residuo_quantil,lag = 15, type = "Ljung-Box")
Box.test(Residuo_quantil,lag = 20, type = "Ljung-Box")

# Normalidade dos residuos quantilicos do Modelo BGEV-ARMAX(1,1) com covariavel sazonalidade
shapiro.test(Residuo_quantil)


# Residuos - diferença entre previsto e observado
par(mfrow=c(1,1))
Residuos <- (training - fit_training$fitted * escala)
Residuos <- ts(Residuos, start = c(2020, 01), frequency = 52)
plot(Residuos)

par(mfrow=c(1,2))
plot(Residuos, ylab = "Resíduos Observados", xlab = "Tempo (meses)", main = "Resíduos")
qqnorm(Residuos, ylab = "Resíduos Observados", xlab = "Quantis Teóricos", main = "Q-Q Plot"); qqline(Residuos)
acf(Residuos[2:62], lag.max = 12*5,  main = "Gráfico ACF")
pacf(Residuos[2:62], lag.max = 12*5,  main = "Gráfico PACF")
par(mfrow=c(1,1))
hist(Residuos)

# verificando estacionariedade do Modelo BGEV-ARMAX(1,1) com covariavel sazonalidade
tseries::kpss.test(Residuos)

# verificando independencia do Modelo BGEV-ARMAX(1,1) com covariavel sazonalidade
Box.test(Residuos,lag = 15, type = "Ljung-Box")
Box.test(Residuos,lag = 20, type = "Ljung-Box")

# Normalidade dos residuos do Modelo BGEV-ARMAX(1,1) com covariavel sazonalidade
shapiro.test(Residuos)


# Medidas de acuracia do modelo BGEV-ARMAX (in-sample)
dif_BGEV <- (training - fit_training$fitted * escala) %>% as.data.frame() %>% remove_missing() %>% unlist()
contagem_NA <- sum(is.na(fit_training$fitted))

(mse_BGEV <- (sum(dif_BGEV^2))/length(dif_BGEV))
(mape_BGEV <- 100 * sum(abs(dif_BGEV) / abs(training[(contagem_NA + 1):length(treinamento$`TEMPERATURA DO AR - BULBO SECO (C)`)])) / length(dif_BGEV))

sqrt(mse_BGEV) # conferindo resultados
forecast::accuracy(fit_training$fitted * escala,training)


# Avaliando a capacidade de the predicao do modelo BGEV-ARMAX (out-of-sample)
predict_BGEV <- fit_training$forecast

dif_BGEV_pred <- (test - predict_BGEV * escala)
dif_BGEV_pred <- ifelse(is.na(dif_BGEV_pred), 0, dif_BGEV_pred)
(mse_BGEVp <- (sum(dif_BGEV_pred^2))/length(dif_BGEV_pred))
(mape_BGEVp <- 100 * sum(abs(dif_BGEV_pred)/abs(test))/length(dif_BGEV_pred))

sqrt(mse_BGEVp) # conferindo resultados
forecast::accuracy(predict_BGEV * escala,test)


# Preparação de dados para os gráficos
ordem = 1:208
dados = data.frame(valores_reais = c(training,test),
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
  theme_minimal() 



#########################################################################


# MODELO CHARMA - AJUSTE E RESIDUOS ----

# modelo CHARMA com covariaveis - ver artigo
source("charma_fit.r")
tau <- 0.5 # mediana

var_exp_treinamento = treinamento[,c(4,7,8,12)] %>% as.matrix() 
var_exp_val = validacao[,c(4,7,8,12)] %>% as.matrix()


# Definindo as combinações para AR e MA
ar_combinations <- list(NA, c(1), c(1, 2), c(1, 2, 3))
ma_combinations <- list(NA, c(1), c(1, 2), c(1, 2, 3))

# Lista para armazenar os resultados
results <- list()

# Contador para armazenar a posição do modelo na lista
model_index <- 1

# Loop sobre todas as combinações
for (ar_value in ar_combinations) {
  for (ma_value in ma_combinations) {
    cat("Ajustando modelo com AR =", ar_value, "e MA =", ma_value, "\n")
    
    # Tentar ajustar o modelo
    fit_charma <- try(
      charma.fit(
        y = training,
        ar = ar_value,
        ma = ma_value,
        X = var_exp_treinamento,
        X_hat = var_exp_val,
        h1 = length(validacao$`TEMPERATURA DO AR - BULBO SECO (C)`),
        graf = 0
      ),
      silent = TRUE
    )
    
    # Verificar se o ajuste foi bem-sucedido
    if (inherits(fit_charma, "try-error")) {
      cat("Erro ao ajustar modelo com AR =", ar_value, "e MA =", ma_value, "\n")
      next
    }
    
    # Avaliação in-sample
    dif_CH <- (training - fit_charma$fitted) %>% 
      as.data.frame() %>% 
      remove_missing() %>% 
      unlist()
    contagem_NA <- sum(is.na(fit_charma$fitted))
    mse_CH <- sum(dif_CH^2) / length(dif_CH)
    mape_CH <- 100 * sum(abs(dif_CH) / abs(training[(contagem_NA + 1):length(training)])) / length(dif_CH)
    
    # Avaliação out-of-sample
    predict_CH <- fit_charma$forecast
    dif_CH_pred <- test - predict_CH
    mse_CHp <- sum(dif_CH_pred^2) / length(dif_CH_pred)
    mape_CHp <- 100 * sum(abs(dif_CH_pred) / abs(test)) / length(dif_CH_pred)
    
    # Garantir consistência no armazenamento de AR e MA
    ar_value_str <- ifelse(is.null(ar_value) || all(is.na(ar_value)), "NA", paste(ar_value, collapse = ","))
    ma_value_str <- ifelse(is.null(ma_value) || all(is.na(ma_value)), "NA", paste(ma_value, collapse = ","))
    
    # Salvar os resultados do modelo (incluindo AIC e BIC)
    results[[model_index]] <- data.frame(
      ar = ar_value_str,
      ma = ma_value_str,
      mse_in_sample = mse_CH,
      mape_in_sample = mape_CH,
      mse_out_sample = mse_CHp,
      mape_out_sample = mape_CHp,
      aic = fit_charma$aic,
      bic = fit_charma$bic
    )
    
    cat("Modelo", model_index, "salvo com sucesso!\n")
    model_index <- model_index + 1
  }
}

# OBS: não funciona para valores negativos da variavel resposta


#########################################################################



# GRÁFICOS DOS MODELOS BGEV ----


# Gráfico 1 - serie temporal e BGEV
par(mfrow=c(1,1))
plot(c(training, test), col="black", type="l", lwd = 2,
     ylim=c(-2, (max(c(training, test))+7)) , axes =T, main="", xlab="\n Tempo (Semanas)", ylab="Temperatura Mínima do Ar (°C)")
lines(c(fit_training$fitted*escala, c(fit_training$forecast*escala)), lty = 2, lwd = 2, col="#db0715")
legend("topleft",legend=c( "Observações","Modelo BGEV"),
       pt.bg="white", lty=c(1,2),lwd = c(2,2), col=c("black","#b50707"), bty="n" )
fim <- (end(c(training, test))[1]+end(dados_RS$`TEMPERATURA DO AR - BULBO SECO (C)`)[2]/52)-52
abline(v=fim,lty=2, lwd = 2)



####################################################################



# ALGORITMO PARA ENCONTRAR MODELOS BGEV ADEQUADOS ----


# TESTE - Diferentes valores de Xi - encontrar os melhores
# candidatos em termos de AIC, AICc, BIC, MSE e MAPE

# Lista para armazenar os resultados do ajuste
valores_xi_bgev <- seq(-1.0, 1.0, by = 0.01)
resultados_AIC <- c(rep(0,length(valores_xi_bgev)))
resultados_AICc <- c(rep(0,length(valores_xi_bgev)))
resultados_BIC <- c(rep(0,length(valores_xi_bgev)))
resultados_mse_BGEV <- c(rep(0,length(valores_xi_bgev)))
resultados_mape_BGEV <- c(rep(0,length(valores_xi_bgev)))
resultados_mse_BGEVp <- c(rep(0,length(valores_xi_bgev)))
resultados_mape_BGEVp <- c(rep(0,length(valores_xi_bgev)))
xi_id = c(rep(0,length(valores_xi_bgev)))
contagem <- 1

# Dados de treinamento e validacao - IN e OUT Sample
treinamento = dados_RS %>% filter(ano <= 2022)
validacao = dados_RS %>% filter(ano > 2022)


# Final - treinamento e teste
training <- treinamento$`TEMPERATURA DO AR - BULBO SECO (C)`
test <- validacao$`TEMPERATURA DO AR - BULBO SECO (C)`

escala = 10 # garante a convergencia do algoritmo em BGEV.ajuste
treinamento$sazonalidade = c  # sazonalidade (IN-SAMPLE)
validacao$sazonalidade = c_hat # sazonalidade (OUT-SAMPLE)
treinamento$`RADIACAO GLOBAL (KJ/m²)` = treinamento$`RADIACAO GLOBAL (KJ/m²)` / escala
validacao$`RADIACAO GLOBAL (KJ/m²)` = validacao$`RADIACAO GLOBAL (KJ/m²)` / escala


# matriz de dados das variaveis explicativas
var_exp_treinamento = treinamento[,c(4,7,8,12)] %>% as.matrix() 
var_exp_val = validacao[,c(4,7,8,12)] %>% as.matrix()


ar_1 <- c(1)    # termos autorregressivos
ma_1 <- c(1)     # termos de médias móveis


for (xi in valores_xi_bgev) {
  fit_training <- try(BGEV.ajuste(y = serie_temporal/escala, 
                                  ar = ar_1, 
                                  ma = ma_1, 
                                  X = var_exp_treinamento, 
                                  X_hat = var_exp_val, 
                                  h1 = length(validacao$`TEMPERATURA DO AR - BULBO SECO (C)`), 
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
    resultados_mape_BGEV[contagem] <- 100 * sum(abs(dif_BGEV) / abs(training[(contagem_NA + 1):length(treinamento$`TEMPERATURA DO AR - BULBO SECO (C)`)])) / length(dif_BGEV)
    
    predict_BGEV <- fit_training$forecast
    dif_BGEV_pred <- (test - predict_BGEV * escala)
    dif_BGEV_pred <- ifelse(is.na(dif_BGEV_pred), 0, dif_BGEV_pred)
    
    resultados_mse_BGEVp[contagem] <- (sum(dif_BGEV_pred^2)) / length(dif_BGEV_pred)
    resultados_mape_BGEVp[contagem] <- 100 * sum(abs(dif_BGEV_pred) / abs(test)) / length(dif_BGEV_pred)
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
novo_arquivo_excel <- "dados_saida(1,1).xlsx"
write.xlsx(dados_teste, novo_arquivo_excel)



####################################################################
