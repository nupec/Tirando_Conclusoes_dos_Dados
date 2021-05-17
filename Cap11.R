# Livro: Análise Estatística com R para Leigos
# Neste Capítulo  11----------------------------------------------------------

# Teste de Hipótse para Duas Amostras

# Página: 209-223

## Testando diferença entre as médias de duas amostras
## Testando médias de amostras emparelhadas
## Testando hipóteses sobre variância
## Entendendo a distruição F

# Pacote necdessário para essa seção
# install.packages("ggplot2") - pacote a ser instalado: retire o # para instalar e depois coloque novamente

# Teste-z para duas amostras em R -----------------------------------------------------
sample1 <- c(100, 118, 97, 92, 118, 125, 136, 95, 111)
sample2 <- c(91, 109, 83, 88, 115, 108, 127, 102, 86)

z.test2(sample1, sample2, 15, 15)

mean1 = mean(sample1)
mean2 = mean(sample2)

z.test2 = function(x, y, popsd1, popsd2){
# Vetor que conterá a probabilidade unicaudal
  one.tail.p <- NULL

# Erro padrão da diferença entre as médias
  std.error <- sqrt((popsd1^2/length(x) +
                     popsd2^2/length(y)))

# Escore-z arredondado
  z.score <- round((mean(x)-mean(y))/std.error,3)

# Probabilidade unicaudal arredondada
  one.tail.p <- round(pnorm(abs(z.score), lower.tail = FALSE),3)

# Concatenar e imprimir
  cat(" mean1 =", mean(x), "  ", "mean2 =", mean(y), "\n",
      "standard error =", std.error,  "\n",
      "z = ", z.score, "\n",
      "one-tailed probability =", one.tail.p, "\n",
      "two-tailed probability =", 2*one.tail.p)
}

# Teste-r em R ------------------------------------------------------------
machine1 <- c(24.58, 22.09, 23.70, 18.89, 22.02, 28.71, 24.44, 20.91, 23.83, 20.83)
machine2 <- c(21.61, 19.06, 20.72, 15.77, 19, 25.88, 21.48, 17.85, 20.86, 17.77)


# O argumento alternative-two-sided reflete o tipo de hipótese alternativa
# especificada no exemplo e o último argumento indica a diferença hipotética
# entre as médias.
t.test(machine1, machine2, var.equal = T, alternative = "two.sided", mu=0)

# Trabalhando com um data frame e uma fórumula -----------------------------

# Outra maneira de executar esse teste é criar um data frame e usar uma formula
# parecida com esta
prod.time = c(machine1, machine2)
machine   = c("machine1", "machine2")

# Transformando um vetor dez repetições
machine <- rep(machine, times = c(10,10))

# E o data frame é:
FarKlemp.frame <- data.frame(machine, prod.time)

# A função t.test(), então é:
with(FarKlemp.frame, t.test(prod.time~machine,
                            var.equal = T,
                            alternative = "two.sided",
                            mu=0))

# Isso produz o mesmo resultado da versão de dois vetores


# Visualizando os resultados ----------------------------------------------
# Em estudo como o da seção anterior, duas maneiras de apreseNtar os resultados
# são o diagrama de caixa (boxplot) e graficos em barras

### Boxplot
with(FarKlemp.frame, boxplot(prod.time~machine, xlab = "Machine",
                             ylab = "Production times (minutes)"))

### Usando ggplot2
library(ggplot2)
ggplot2::ggplot(FarKlemp.frame, aes(x=machine, y = prod.time)) +
  stat_boxplot(geom = "errorbar", width = .5) +
  geom_boxplot()

## Gráfico de barras
#### Vetores necessários
machine.names <- c("machine1", "machine2")
mean.times <- c(mean(machine1), mean(machine2))
se.times <- c(sd(machine1)/sqrt(length(machine1)),
              sd(machine2)/sqrt(length(machine2)))

#### O data frame é:
FKmeans.frame <- data.frame(machine.names,
                            mean.times,
                            se.times)

#### O código para criar a figura é:
ggplot(FKmeans.frame,
       aes(x= machine.names, y = mean.times)) +
       geom_bar(stat = "identity", width = .4, color = "red", fill = "gray") +
       geom_errorbar(aes(ymin = mean.times - se.times,
                         ymax = mean.times + se.times, width = .1 ))

# A primeira função preparao cenário com os mapeamentos estéticos, e a segunda,
# o diagrama de barras. O argumento stat = identity, instrui geom_bar a usar as
# estatísticas tabeladas , em vez de contar as instâncias de machine1 e machine2.
# Os outros argumentos configuram a aparência da barra


# A terceira função é o geom, que diagrama as barras de erro. Os mapeamentos
# estéticos configuram os pontos mínimos e máximos para cada barra de erro.
# O argumento "width" configuram a largura da linha perpendicular no fim de
# cada barra de erro.

ggplot(FKmeans.frame,
  aes(x= machine.names, y = mean.times)) +
  geom_bar(stat = "identity", width = .4, color = "red", fill = "gray") +
  geom_errorbar(aes(ymin = mean.times, ymax = mean.times + se.times,
                width = .1 ))


# Como ps e qs: Variâncias desiguais --------------------------------------

with(FarKlemp.frame, t.test(prod.time~machine,
                            var.equal = FALSE,
                            alternative = "two.sided",
                            mu=0))

with(FarKlemp.frame, t.test(prod.time~machine,
                            var.equal = TRUEN,
                            alternative = "two.sided",
                            mu=0))

# Conjunto Combinado ------------------------------------------------------
# Para os testes-t de amostras emparelhadas usamos a mesma fórmula dos testes-t de
# amostras independentes

pessoas <- c(1,2,3,4,5,6,7,8,9,10)
pesosAntesDoPrograma <- c(198,201,210,185,204,156,167,197,220,186)
pesoDepoisDeUmes <- c(194,203,200,183,200,153,166,197,215,184)
diferenca <- pesosAntesDoPrograma-pesoDepoisDeUmes

df <- data.frame(pessoas, pesos, pesoDepoisDeUmes, diferenca)

t.test(pesosAntesDoPrograma, pesoDepoisDeUmes, alternative = "greater", paired = T)

# Conclusão: Por causa do p-value muito baixo, rejeitamos a hipótese nula
