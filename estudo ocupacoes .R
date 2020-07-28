# Carregar pacotes
library(magrittr)
library(tidyverse)
library(survey)
library(srvyr)
library(DBI)


# Abrir conexão com o banco de dados
db <- DBI::dbConnect(odbc::odbc(),"",
                     uid=(""),
                     pwd=(""))


# Carregar a coleta inicial do caged por cbo
caged_cbo <- DBI::dbGetQuery(db,"select competencia, saldo_movimentacao, cbo from caged.view_trabalhador")

cbo<-caged_cbo %>%
  filter(competencia>'2008-01-01')

# tranforma a competencia em data
cbo<-caged_cbo %>% 
  mutate(competencia=as.Date(competencia))

#Soma os saldo dentro e fora do prazo apresentados
cbo <-cbo %>%
  group_by(competencia,cbo) %>% 
  summarise(saldo_movimenta?ao = sum(saldo_movimentacao, na.rm = FALSE))

#transforma as cbos em colunas
frame_cbo<- cbo %>% spread(cbo,saldo_movimentaçao)

#Substitui os valores "NA" por 0
frame_cbo[is.na(frame_cbo)] <- 0

####Saldo Acumulado###

#criando um vetor com as referencias
a<-c(frame_cbo$competencia)
#transformado em data
a<-as.data.frame(a)

#loop de saldo de acumulado em 12 meses para todas as colunas, incluido-as no vetor a
for (i in names(frame_cbo)){
  
  a[[paste(i)]] = zoo::rollsum(frame_cbo[[i]], 12, fill = NA, align = "right")
}

#retirando a coluna que acumula as competências (kkk)
a<-a[-2]

B<-c(frame_cbo$competencia)
B<-as.data.frame(B)

for (i in names(frame_cbo)){
  
  B[[paste(i)]] = cumsum(frame_cbo[[i]])
}


#retorna a tabela para o formato inicial(cbos como variaveis)#
data<-a %>%
  gather("cbo", "saldo", 3:2412)


#Teste para uma CBO
#Filtra dados para apenas uma cbo(vendedor), e soma os saldo dentro e fora do prazo apresentados
teste<-data %>%
  srvyr::filter(cbo==317210) %>% 
  group_by(competencia,cbo) %>% 
  summarise(saldo_movimentaçao = sum(saldo, na.rm = FALSE))

#pacotes séries históricas
#install.packages("forecast")
#install.packages("fpp2")
#install.packages("readxl")

library(forecast)
library(fpp2)
library(readxl)


#Filtra dados para apenas uma cbo(vendedor), e soma os saldo dentro e fora do prazo apresentados
teste<-data %>%
  srvyr::filter(cbo== 521110) %>% 
  group_by(a) %>% 
  summarise(saldo_movimentaçao = sum(saldo, na.rm = FALSE))


#Transformando um Data Frame em uma Série Temporal

Cbo_serie = ts(data = teste$saldo_movimentaçao, start = c(2006,1),frequency = 12)
Cbo_serie
plot.ts(Cbo_serie)
autoplot(Cbo_serie)
d = decompose(Cbo_serie, type = c("additive"), filter = NULL)

plot(d)
tenden<-d$trend
d$season
dia<-Cbo_serie-d$seasonal
plot.ts(dia)

tempo=1:168
tempo
modelo=lm(Cbo_serie~tempo)
summary(modelo)

plot(Cbo_serie)
plot(modelo)





qwe<-d$trend
plot(qwe)
art<-decompose(dia)
plot(art)
-----
#Agora que temos uma série temporal, vamos plotar:
forecast::autoplot(Cbo_serie)
#Observando um gráfico sazonal:
ggseasonplot(Cbo_serie)
#Observando um gráfico sazonal "polar":
ggseasonplot(Cbo_serie, polar = T)

------
ggsubseriesplot(Cbo_serie)
previsao = ses(Cbo_serie, h = 12) # Previsao para os próximos 12 meses
summary(previsao)
autoplot(previsao)
------
modelo_arima = forecast::auto.arima(Cbo_serie)

autoplot(forecast(modelo_arima))

library("seasonal")
ajuste<-seasonal::seas(Cbo_serie)

plot(ajuste)
z
checkX13()
#xts, zoo, TTR, forecast, quantmod and tidyquant
#https://www.pedronl.com/post/previsao-de-series-temporais-com-o-r/
 
  
   d$trend
  write.table(caged_cbo,
              "/u01/u104409/cbo.csv",
              sep = ";",
              dec = ",",
              fileEncoding = "latin1",
              row.names = F,
              na="")
 
  
   write.table(frame_cbo,"saldos.csv",
              row.names = F, sep = ";", dec = ',')
