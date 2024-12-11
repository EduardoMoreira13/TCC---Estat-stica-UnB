# Distribuição BGEV - parametro de locação

library(bgev)

valores = seq(-3, 3, by = 0.01)
densidade0 = dbgev(valores, mu = -1, sigma = 1, xi = 0, delta = 1)
densidade2 = dbgev(valores, mu = 0, sigma = 1, xi = 0, delta = 1)
densidade4 = dbgev(valores, mu = 1, sigma = 1, xi = 0, delta = 1)

# Plotar amostra em um gráfico de linhas
plot(valores,densidade0, type = 'l', col = '#0a21d1', lwd = 3,
     main = 'Amostra da Distribuição BGEV',
     xlab = 'Valores Observados (y)', ylab = 'f(y)')

# Adicionar outras curvas de densidade
lines(valores, densidade2, col = 'black', lwd = 3)
lines(valores, densidade4, col = '#d10a42', lwd = 3)

# Adicionar legenda
legend('topleft', legend = c('\u03BC = -1,0',"\u03BC = 0,0","\u03BC = 1,0"), 
       col = c('#0a21d1',"black","#d10a42"), lwd = 3, 
       cex = 1.5,  bty = 'n')



# Distribuição BGEV - parametro de escala e forma

valores = seq(-3, 3, by = 0.01)
densidade0 = dbgev(valores, mu = 0, sigma = 0.5, xi = 0, delta = 1)
densidade2 = dbgev(valores, mu = 0, sigma = 1.0, xi = 0, delta = 1)
densidade4 = dbgev(valores, mu = 0, sigma = 1.5, xi = 0, delta = 1)

# Plotar amostra em um gráfico de linhas
plot(valores,densidade0, type = 'l', col = '#0a21d1', lwd = 3,
     main = 'Amostra da Distribuição BGEV',
     xlab = 'Valores Observados (y)', ylab = 'f(y)')

# Adicionar outras curvas de densidade
lines(valores, densidade2, col = 'black', lwd = 3)
lines(valores, densidade4, col = '#d10a42', lwd = 3)


# Adicionar legenda
legend('topleft', legend = c('\u03C3 = 0,5', '\u03C3 = 1,0',"\u03C3 = 1,5"), 
       col = c('#0a21d1',"black","#d10a42"), lwd = 3, 
       cex = 1.5,  bty = 'n')



# Distribuição BGEV - parametro de forma

valores = seq(-3, 3, by = 0.01)
densidade0 = dbgev(valores, mu = 0, sigma = 1, xi = -1, delta = 1)
densidade2 = dbgev(valores, mu = 0, sigma = 1, xi = 0.0, delta = 1)
densidade4 = dbgev(valores, mu = 0, sigma = 1, xi = 1, delta = 1)


# Plotar amostra em um gráfico de linhas
plot(valores,densidade0, type = 'l', col = '#0a21d1', lwd = 3,
     main = 'Amostra da Distribuição BGEV',
     xlab = 'Valores Observados (y)', ylab = 'f(y)')

# Adicionar outras curvas de densidade
lines(valores, densidade2, col = 'black', lwd = 3)
lines(valores, densidade4, col = '#d10a42', lwd = 3)

# Adicionar legenda
legend('topleft', legend = c('\u03BE = -1,0',"\u03BE = 0,0","\u03BE = 1,0"), 
       col = c('#0a21d1',"black","#d10a42"), lwd = 3, 
       cex = 1.5,  bty = 'n')



# Distribuição BGEV - parametro de forma e bimodalidade

valores = seq(-3, 3, by = 0.01)
densidade0 = dbgev(valores, mu = 0, sigma = 1, xi = 0, delta = 0)
densidade2 = dbgev(valores, mu = 0, sigma = 1, xi = 0, delta = 0.5)
densidade4 = dbgev(valores, mu = 0, sigma = 1, xi = 0, delta = 1.0)


# Plotar amostra em um gráfico de linhas
plot(valores,densidade0, type = 'l', col = '#0a21d1', lwd = 3,
     main = 'Amostra da Distribuição BGEV', ylim = c(0,0.6),
     xlab = 'Valores Observados (y)', ylab = 'f(y)')

# Adicionar outras curvas de densidade
lines(valores, densidade2, col = 'black', lwd = 3)
lines(valores, densidade4, col = '#d10a42', lwd = 3)

# Adicionar legenda
legend('topleft', legend = c('\u03B4 = 0,0',"\u03B4 = 0,5",
                              "\u03B4 = 1,0"), 
       col = c('#0a21d1',"black","#d10a42"), lwd = 3, 
       cex = 1.5,  bty = 'n')

