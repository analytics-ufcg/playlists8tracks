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
table(vetor.ajustado == data$pop[(sizeTraining+1):nData])/sizeTesting 

