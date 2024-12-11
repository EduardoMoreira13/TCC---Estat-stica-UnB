# Distribuição GEV - parametro de locação

library(evd)

valores = seq(-3, 3, by = 0.01)
densidade0 = dgev(valores,loc = -1, scale = 1, shape = 1)
densidade1 = dgev(valores,loc = -0.5, scale = 1, shape = 1)
densidade2 = dgev(valores,loc = 0, scale = 1, shape = 1)
densidade3 = dgev(valores,loc = 0.5, scale = 1, shape = 1)
densidade4 = dgev(valores,loc = 1, scale = 1, shape = 1)

# Plotar amostra em um gráfico de linhas
plot(valores,densidade0, type = 'l', col = '#d19c0a', lwd = 2,
     main = 'Amostra da Distribuição GEV',
     xlab = 'Valores Observados (y)', ylab = 'f(y)')

# Adicionar outras curvas de densidade
lines(valores, densidade1, col = '#0ad17b', lwd = 2)
lines(valores, densidade2, col = 'black', lwd = 2)
lines(valores, densidade3, col = '#0a3cd1', lwd = 2)
lines(valores, densidade4, col = '#d10a42', lwd = 2)

# Adicionar legenda
legend('topright', legend = c('\u03BC = -1,0', '\u03BC = -0,5',"\u03BC = 0",
                              "\u03BC = 0,5","\u03BC = 1,0"), 
       col = c('#d19c0a', '#0ad17b',"black","#0a3cd1","#d10a42"), lwd = 2, 
       cex = 1,  bty = 'n')


# Distribuição GEV - parametro de escala

valores = seq(-3, 3, by = 0.01)
densidade0 = dgev(valores,loc = 0, scale = 0.5, shape = 1)
densidade1 = dgev(valores,loc = 0, scale = 1, shape = 1)
densidade2 = dgev(valores,loc = 0, scale = 1.5, shape = 1)
densidade3 = dgev(valores,loc = 0, scale = 2, shape = 1)
densidade4 = dgev(valores,loc = 0, scale = 2.5, shape = 1)

# Plotar amostra em um gráfico de linhas
plot(valores,densidade0, type = 'l', col = '#d19c0a', lwd = 2,
     main = 'Amostra da Distribuição GEV',
     xlab = 'Valores Observados (y)', ylab = 'f(y)')

# Adicionar outras curvas de densidade
lines(valores, densidade1, col = '#0ad17b', lwd = 2)
lines(valores, densidade2, col = 'black', lwd = 2)
lines(valores, densidade3, col = '#0a3cd1', lwd = 2)
lines(valores, densidade4, col = '#d10a42', lwd = 2)

# Adicionar legenda
legend('topright', legend = c('\u03C3 = 0,5', '\u03C3 = 1,0',"\u03C3 = 1,5",
                              "\u03C3 = 2,0","\u03C3 = 2,5"), 
       col = c('#d19c0a', '#0ad17b',"black","#0a3cd1","#d10a42"), lwd = 2, 
       cex = 1,  bty = 'n')


# Distribuição GEV - parametro de forma

valores = seq(-3, 3, by = 0.01)
densidade0 = dgev(valores,loc = 0, scale = 1, shape = -1)
densidade1 = dgev(valores,loc = 0, scale = 1, shape = -0.5)
densidade2 = dgev(valores,loc = 0, scale = 1, shape = 0)
densidade3 = dgev(valores,loc = 0, scale = 1, shape = 0.5)
densidade4 = dgev(valores,loc = 0, scale = 1, shape = 1)

# Plotar amostra em um gráfico de linhas
plot(valores,densidade0, type = 'l', col = '#d19c0a', lwd = 2,
     main = 'Amostra da Distribuição GEV',
     xlab = 'Valores Observados (y)', ylab = 'f(y)')

# Adicionar outras curvas de densidade
lines(valores, densidade1, col = '#0ad17b', lwd = 2)
lines(valores, densidade2, col = 'black', lwd = 2)
lines(valores, densidade3, col = '#0a3cd1', lwd = 2)
lines(valores, densidade4, col = '#d10a42', lwd = 2)

# Adicionar legenda
legend('topright', legend = c('\u03BE = -1,0', '\u03BE = -0,5',"\u03BE = 0",
                              "\u03BE = 0,5","\u03BE = 1,0"), 
       col = c('#d19c0a', '#0ad17b',"black","#0a3cd1","#d10a42"), lwd = 2, 
       cex = 1,  bty = 'n')
