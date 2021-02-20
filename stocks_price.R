library(ggplot2)
library(hrbrthemes)

setwd("E:/Stocks_Project")
getwd

# carregando dataset 
stocks <- read.csv("EOD-MSFT.csv", fileEncoding = "windows-1252")

# Visualizando dataset
View(stocks)

# Visualizando os primeiros dados do dataset
head(stocks)

# Gerando a m?dia de abertura das A??es
media_open <- mean(stocks$Open)
media_high <- mean(stocks$High)

# Gerando um resumo do Volume Financeiro comparado com a al
summary(stocks[c('Volume', 'High')])

# Qual o valor que ocorre com maior frequ?ncia em Volume Financeiro ?
moda <- function(v){
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v, valor_unico)))]
}

resultado <- moda(stocks$Volume)
resultado

# Qual ? a uniformidade dos dados nos fechamentos da a??o ?
sd(stocks$Close)


# Qual ? o Valor de abertura com mais frequ?ncia ? 

moda <- function(v){
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v, valor_unico)))]
}

resultado <- moda(stocks$Open)
resultado


h <- hist(stocks$Open,
          breaks = 10,
          col="orange",
          xlab= "Values Open of Microsoft until 2017-12-28",
          ylab = "Frequency",
          main = "Qual ? o Valor com mais frequ?ncia ?")

xfit <- seq(min(stocks$Open), max(stocks$Open), length = 40)
yfit <- dnorm(xfit, mean = mean(stocks$Open), sd = sd(stocks$Open))
yfit <- yfit*diff(h$mids[1:2] * length(stocks$Open))
lines(xfit, yfit, col = "green", lwd = 2)