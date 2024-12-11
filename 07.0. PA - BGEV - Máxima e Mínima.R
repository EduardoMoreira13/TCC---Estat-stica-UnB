# Temperatura do Ar (°C) - Modelos e Gráficos


# PACOTES UTILIZADOS ----

library(bgev)        # pacote da distribuicao BGEV
library(readxl)      # ler Excel
library(tidyverse)   # manipulacao de dataframe
library(forecast)    # acoes com series temporais
library(openxlsx)    # exportar para Excel
source("BGEV_fit.r") # ler a funcao BGEV - criada por Eduardo Moreira



# LEITURA E ADEQUACAO DA BASE DE DADOS ----

# Leitura dos dados do Rio Grande do Sul
dados_RS1 <- read_excel("07.1. PA - BGEV - Temperatura Máxima do Ar.xlsx") %>% arrange(ano,semana)
dados_RS2 <- read_excel("07.2. PA - BGEV - Temperatura Mínima do Ar.xlsx") %>% arrange(ano,semana)


# DEFININDO A SERIE TEMPORAL - TREINAMENTO E VALIDACAO ----


# Dados de treinamento e validacao - IN e OUT Sample
treinamento1 = dados_RS1 %>% filter(ano <= 2022)
validacao1 = dados_RS1 %>% filter(ano > 2022)

treinamento2 = dados_RS2 %>% filter(ano <= 2022)
validacao2 = dados_RS2 %>% filter(ano > 2022)

# Final - treinamento e teste
training1 <- treinamento1$`TEMPERATURA DO AR - BULBO SECO (C)`
test1 <- validacao1$`TEMPERATURA DO AR - BULBO SECO (C)`

training2 <- treinamento2$`TEMPERATURA DO AR - BULBO SECO (C)`
test2 <- validacao2$`TEMPERATURA DO AR - BULBO SECO (C)`


# Covariavel da sazonalidade
x <- ts(training1, start= c(2020, 01), frequency = 52)
saz <- decompose(x)
c <- saz$seasonal    # treinamento - IN SAMPLE
xreg <- as.vector(c)
c_hat <- xreg[1:length(validacao1$`TEMPERATURA DO AR - BULBO SECO (C)`)]  # validacao - OUT SAMPLE


escala = 10 # garante a convergencia do algoritmo em BGEV.ajuste
treinamento1$sazonalidade = c  # sazonalidade (IN-SAMPLE)
validacao1$sazonalidade = c_hat # sazonalidade (OUT-SAMPLE)


x <- ts(training2, start= c(2020, 01), frequency = 52)
saz <- decompose(x)
c <- saz$seasonal    # treinamento - IN SAMPLE
xreg <- as.vector(c)
c_hat <- xreg[1:length(validacao2$`TEMPERATURA DO AR - BULBO SECO (C)`)]  # validacao - OUT SAMPLE


treinamento2$sazonalidade = c  # sazonalidade (IN-SAMPLE)
validacao2$sazonalidade = c_hat # sazonalidade (OUT-SAMPLE)


# matriz de dados das variaveis explicativas
var_exp_treinamento1 = treinamento1[,c(4,7,8,12)] %>% as.matrix() 
var_exp_val1 = validacao1[,c(4,7,8,12)] %>% as.matrix()

var_exp_treinamento2 = treinamento2[,c(4,7,8,12)] %>% as.matrix() 
var_exp_val2 = validacao2[,c(4,7,8,12)] %>% as.matrix()


# Grafico da Serie Temporal
serie_temporal1 <- ts(training1, start = c(2020, 01), frequency = 52)

serie_temporal2 <- ts(training2, start = c(2020, 01), frequency = 52)


# Ajuste do Modelo - BGEV-ARMAX(1,1) com covariavel sazonalidade

fit_training1 <- try(BGEV.ajuste(y = training1/escala, 
                                ar = NA, ma = c(1), 
                                X = var_exp_treinamento1, 
                                X_hat = var_exp_val1, 
                                h1 = length(validacao1$`TEMPERATURA DO AR - BULBO SECO (C)`), 
                                link_fun ="identity",
                                graf = 0, show = 1, 
                                xi_bgev = -0.23))


fit_training2 <- try(BGEV.ajuste(y = training2/escala, 
                                ar = c(1), ma = c(1), 
                                X = var_exp_treinamento2, 
                                X_hat = var_exp_val2, 
                                h1 = length(validacao2$`TEMPERATURA DO AR - BULBO SECO (C)`), 
                                link_fun ="identity",
                                graf = 0, show = 1, 
                                xi_bgev = -0.29))

#########################################################################


# GRÁFICOS DOS MODELOS BGEV E CHARMA ----


# Gráfico 1 - serie temporal máximo
par(mfrow=c(1,1))
plot(c(training1, test1), col="black", type="l", lwd = 2,
     ylim=c(16, (max(c(training1, test1))+7)) , axes =T, main="", xlab="\n Tempo (Semanas)", ylab="Temperatura Máxima do Ar (°C)")
lines(c(fit_training1$fitted*escala, c(fit_training1$forecast*escala)), lty = 2, lwd = 2, col="#db0715")
legend("topleft",legend=c( "Observações","Modelo BGEV"),
       pt.bg="white", lty=c(1,2),lwd = c(2,2), col=c("black","#b50707"), bty="n" )
fim <- (end(c(training1, test1))[1]+end(dados_RS1$`TEMPERATURA DO AR - BULBO SECO (C)`)[2]/52)-52
abline(v=fim,lty=2, lwd = 2)


# Gráfico 1 - serie temporal mínimo
par(mfrow=c(1,1))
plot(c(training2, test2), col="black", type="l", lwd = 2,
     ylim=c(-2, (max(c(training2, test2))+7)) , axes =T, main="", xlab="\n Tempo (Semanas)", ylab="Temperatura Mínima do Ar (°C)")
lines(c(fit_training2$fitted*escala, c(fit_training2$forecast*escala)), lty = 2, lwd = 2, col="#db0715")
legend("topleft",legend=c( "Observações","Modelo BGEV"),
       pt.bg="white", lty=c(1,2),lwd = c(2,2), col=c("black","#b50707"), bty="n" )
fim <- (end(c(training2, test2))[1]+end(dados_RS2$`TEMPERATURA DO AR - BULBO SECO (C)`)[2]/52)-52
abline(v=fim,lty=2, lwd = 2)


par(mfrow=c(1,1))
plot(c(training1, test1), col="black", type="l", lwd = 2,
     ylim=c(-5, (max(c(training1, test1))+7)) , axes =T, main="", xlab="\n Tempo (Semanas)", ylab="Mínimos e Máximos da Temperatura (°C)")
lines(c(training2, test2), lty = 1, lwd = 2, col="black")


par(mfrow=c(1,1))
plot(c(training1, test1), col="black", type="l", lwd = 2,
     ylim=c(-5, 50) , axes =T, main="", xlab="\n Tempo (Semanas)", ylab="Mínimos e Máximos da Temperatura (°C)")
lines(c(training2, test2), lty = 1, lwd = 2, col="black")
lines(c(fit_training1$fitted*escala, c(fit_training1$forecast*escala)), lty = 2, lwd = 2, col="#db0715")
lines(c(fit_training2$fitted*escala, c(fit_training2$forecast*escala)), lty = 2, lwd = 2, col="#db0715")
legend("topleft",legend=c( "Observações","Modelo BGEV"),
       pt.bg="white", lty=c(1,2),lwd = c(2,2), col=c("black","#b50707"), bty="n" )
fim <- (end(c(training2, test2))[1]+end(dados_RS2$`TEMPERATURA DO AR - BULBO SECO (C)`)[2]/52)-52
abline(v=fim,lty=2, lwd = 2)



####################################################################



