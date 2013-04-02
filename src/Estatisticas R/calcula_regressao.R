require("aod")
require(ggplot2)

data = read.csv("estatisticas-das-playlists.txt",sep="\t")


#correlacoes entre atributos e popularidade
correlacoes = read.csv("correlacao dos atributos x popularidade.txt",sep="\t")

correlacoes = correlacoes[2:12,c(1,2,5,6)]


#data soh com os melhores atributos
data = data[ , which(names(data) %in% c("pop", as.character(correlacoes$atributo.2)))]


#transforma pop em binario
quantile(data$pop,c(.5))


for (i in 1: length(data$pop)) {
  if (data$pop[i] < 0.5454545) {
    data$pop[i] = 0
  }
  else {
    data$pop[i] = 1
  }
}

rm(i)


#funcao sigmoide
sigmoide = function(z) {
  1/(1 + exp(-z))
}

#30 iterações
it = 30

tabela.acertos = c()
tabela.erros = c()

for (i in 1:it) {
  
  #regressao 
  nData = nrow(data)
  sizeTraining = round(0.75*nrow(data))
  sizeTesting = nrow(data)-sizeTraining
  
  #aleatorizar este baguio
  data <- data[sample(1:nrow(data), length(1:nrow(data))), 1:ncol(data)]
  
  #realizar regressao
  mylogit = glm(pop ~  soma.hottness + quant.musicas, data = data[1:sizeTraining,], family = "binomial")
  
  coef(mylogit)
  
  
  
  #previsao = predict(mylogit)
  testingData = data[(sizeTraining+1):nData,]
  h = sigmoide(predict(mylogit,testingData))
  
  
  vetor.ajustado = c()
  
  for (i in 1:sizeTesting) {
    if(h[i] >= 0.5) {
      vetor.ajustado[i] = 1
    }
    else {
      vetor.ajustado[i] = 0
    }
  }
  
  #erro
  tabela = table(vetor.ajustado == data$pop[(sizeTraining+1):nData])/sizeTesting
  tabela.acertos = c(tabela.acertos,tabela[[2]])
  tabela.erros = c(tabela.erros,tabela[[1]])
  
}






#gera intervalo de confianca
ic = function(x, alpha = 0.05) {
  q = 1 - alpha/2
  s = sd(x)
  xbar = mean(x)
  n = length(x)
  if (n >= 30)
    error = qnorm(q)*s/sqrt(n)
  else
    error = qt(q, df = n-1)*s/sqrt(n)
  
  lower = xbar-error
  upper = xbar+error
  return(data.frame(lower, upper, y=xbar))
}

ic_acertos = 100*ic(tabela.acertos)
ic_erros = 100*ic(tabela.erros)


#gera imagem do intervalo
png("IC_Mean_Acertos_Erros.png")
ggplot()+
  geom_errorbar(aes(x=c("Acertos","Erros"), 
                    ymax=c(ic_acertos$upper, ic_erros$upper), 
                    ymin=c(ic_acertos$lower, ic_erros$lower)))+
  xlab("")+
  ylab("Porcentagem")  
dev.off()




