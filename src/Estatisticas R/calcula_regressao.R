require("aod")
require(ggplot2)

data = read.csv("estatisticas-das-playlists.txt",sep="\t")


#correlacoes entre atributos e popularidade
correlacoes = read.csv("correlacao dos atributos x popularidade.txt",sep="\t")

correlacoes = correlacoes[2:52,c(1,2,5,6)]

#teste de normalidade
shapiro.test(data$pop)
#p-value < 2.2e-16

#data soh com os melhores atributos
data = data[ , which(names(data) %in% c("pop", as.character(correlacoes$atributo.2)))]


#transforma pop em binario
quantile(data$pop,c(.5))


for (i in 1:length(data$pop)) {
  if (data$pop[i] < 0.5529115) {
    data$pop[i] = 0
  }
  else {
    data$pop[i] = 1
  }
}

rm(i)

data = na.omit(data)

#funcao sigmoide
sigmoide = function(z) {
  1/(1 + exp(-z))
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

#tabela de regressoes
tabela.regressao = cbind(atributos = c(), limite.inf = c(), limite.sup = c())

#gerando regressão p/ um atributo por vez
for (j in correlacoes$atributo) {
      #30 iterações
      it = 10
      
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
        mylogit = glm(pop ~ eval(as.symbol(j)), data = data[1:sizeTraining,], family = "binomial")
        
        coef(mylogit)
        
        
        
        #previsao = predict(mylogit)
        testingData = data[(sizeTraining+1):nData,]
        h = sigmoide(predict(mylogit,testingData))
        
        
        vetor.ajustado = c()
        
        for (k in 1:sizeTesting) {      
          if(h[k] >= 0.5) {
            vetor.ajustado[k] = 1
          }
          else {
            vetor.ajustado[k] = 0
          }
        }
        
        
        #erro
        tabela = table(vetor.ajustado == data$pop[(sizeTraining+1):nData])/sizeTesting
        tabela.acertos = c(tabela.acertos,tabela[[2]])
        tabela.erros = c(tabela.erros,tabela[[1]])
        
      } 
      
      ic_acertos = 100*ic(tabela.acertos)
      ic_erros = 100*ic(tabela.erros)
      tabela.regressao = rbind(tabela.regressao, c(j, ic_acertos$lower, ic_acertos$upper))
}  


tabela.regressao = as.data.frame(tabela.regressao)

colnames(tabela.regressao) = c("atributos", "limite.inf", "limite.sup")

tabela.regressao$limite.inf = as.numeric(levels(tabela.regressao$limite.inf))[tabela.regressao$limite.inf]

tabela.regressao$limite.sup = as.numeric(levels(tabela.regressao$limite.sup))[tabela.regressao$limite.sup]


tabela.regressao$media = (tabela.regressao$limite.sup + tabela.regressao$limite.inf)/2

tabela.regressao = tabela.regressao[order(tabela.regressao$media,decreasing=T),]

write.table(tabela.regressao,"regressao_estatistica_um_atributo.txt",sep="\t",col.names=T,row.names=F)

tabela.regressao = read.csv("regressao_estatistica_um_atributos.txt",sep="\t")




#talvez diminuir a quantidade de atributos baseado na correlação.

correlacoes$atributo = as.character(correlacoes$atributo)
tabela.regressao = cbind(atributos = c(), limite.inf = c(), limite.sup = c())
#gerando regressão p/ 2 atributos por vez
for (j in 1:(length(correlacoes$atributo) - 1)) {
  for (l in (j+1):length(correlacoes$atributo)) {
    #30 iterações
    it = 10
    
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
      mylogit = glm(pop ~ eval(as.symbol(correlacoes$atributo[j])) + eval(as.symbol(correlacoes$atributo[l])), data = data[1:sizeTraining,], family = "binomial")
      
      coef(mylogit)
      
      
      
      #previsao = predict(mylogit)
      testingData = data[(sizeTraining+1):nData,]
      h = sigmoide(predict(mylogit,testingData))
      
      
      vetor.ajustado = c()
      
      for (k in 1:sizeTesting) {      
        if(h[k] >= 0.5) {
          vetor.ajustado[k] = 1
        }
        else {
          vetor.ajustado[k] = 0
        }
      }
      
      
      #erro
      tabela = table(vetor.ajustado == data$pop[(sizeTraining+1):nData])/sizeTesting
      tabela.acertos = c(tabela.acertos,tabela[[2]])
      tabela.erros = c(tabela.erros,tabela[[1]])
      
    } 
    
    ic_acertos = 100*ic(tabela.acertos)
    ic_erros = 100*ic(tabela.erros)
    tabela.regressao = rbind(tabela.regressao, c(paste(correlacoes$atributo[j],correlacoes$atributo[l],sep=" "), ic_acertos$lower, ic_acertos$upper))
  }
}

tabela.regressao = as.data.frame(tabela.regressao)

colnames(tabela.regressao) = c("atributos", "limite.inf", "limite.sup")

tabela.regressao$limite.inf = as.numeric(levels(tabela.regressao$limite.inf))[tabela.regressao$limite.inf]

tabela.regressao$limite.sup = as.numeric(levels(tabela.regressao$limite.sup))[tabela.regressao$limite.sup]


tabela.regressao$media = (tabela.regressao$limite.sup + tabela.regressao$limite.inf)/2

tabela.regressao = tabela.regressao[order(tabela.regressao$media,decreasing=T),]

write.table(tabela.regressao,"regressao_estatistica_dois_atributos.txt",sep="\t",col.names=T,row.names=F)

tabela.regressao = read.csv("regressao_estatistica_dois_atributos.txt",sep="\t")


#3 atributos
correlacoes$atributo = as.character(correlacoes$atributo)
tabela.regressao = cbind(atributos = c(), limite.inf = c(), limite.sup = c())
#gerando regressão p/ 3 atributos por vez
for (j in 1:(length(correlacoes$atributo) - 2)) {
  print(j)
  for (l in (j+1):(length(correlacoes$atributo) - 1)) {
    for (n in (l+1):(length(correlacoes$atributo))) {
      #30 iterações
      it = 10
      
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
        mylogit = glm(pop ~ eval(as.symbol(correlacoes$atributo[j])) + eval(as.symbol(correlacoes$atributo[l])) + eval(as.symbol(correlacoes$atributo[n])), data = data[1:sizeTraining,], family = "binomial")
        
        coef(mylogit)
        
        
        
        #previsao = predict(mylogit)
        testingData = data[(sizeTraining+1):nData,]
        h = sigmoide(predict(mylogit,testingData))
        
        
        vetor.ajustado = c()
        
        for (k in 1:sizeTesting) {      
          if(h[k] >= 0.5) {
            vetor.ajustado[k] = 1
          }
          else {
            vetor.ajustado[k] = 0
          }
        }
        
        
        #erro
        tabela = table(vetor.ajustado == data$pop[(sizeTraining+1):nData])/sizeTesting
        tabela.acertos = c(tabela.acertos,tabela[[2]])
        tabela.erros = c(tabela.erros,tabela[[1]])
        
      } 
      
      ic_acertos = 100*ic(tabela.acertos)
      ic_erros = 100*ic(tabela.erros)
      tabela.regressao = rbind(tabela.regressao, c(paste(correlacoes$atributo[j],correlacoes$atributo[l], correlacoes$atributo[n],sep=" "), ic_acertos$lower, ic_acertos$upper))
    }
  }  
}

tabela.regressao = as.data.frame(tabela.regressao)

colnames(tabela.regressao) = c("atributos", "limite.inf", "limite.sup")

tabela.regressao$limite.inf = as.numeric(levels(tabela.regressao$limite.inf))[tabela.regressao$limite.inf]

tabela.regressao$limite.sup = as.numeric(levels(tabela.regressao$limite.sup))[tabela.regressao$limite.sup]


tabela.regressao$media = (tabela.regressao$limite.sup + tabela.regressao$limite.inf)/2

tabela.regressao = tabela.regressao[order(tabela.regressao$media,decreasing=T),]

write.table(tabela.regressao,"regressao_estatistica_tres_atributos.txt",sep="\t",col.names=T,row.names=F)

tabela.regressao = read.csv("regressao_estatistica_tres_atributos.txt",sep="\t")


#4 atributos
correlacoes = correlacoes[1:10,]

correlacoes$atributo = as.character(correlacoes$atributo)
tabela.regressao = cbind(atributos = c(), limite.inf = c(), limite.sup = c())
#gerando regressão p/ 4 atributos por vez
for (j in 1:(length(correlacoes$atributo) - 3)) {
  print(j)
  for (l in (j+1):(length(correlacoes$atributo) - 2)) {
    for (n in (l+1):(length(correlacoes$atributo) -1)) {
      for (m in (n+1):(length(correlacoes$atributo))) {
        #30 iterações
        it = 10
        
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
          mylogit = glm(pop ~ eval(as.symbol(correlacoes$atributo[j])) + eval(as.symbol(correlacoes$atributo[l])) + eval(as.symbol(correlacoes$atributo[n])) + eval(as.symbol(correlacoes$atributo[m])), data = data[1:sizeTraining,], family = "binomial")
          
          coef(mylogit)
          
          
          
          #previsao = predict(mylogit)
          testingData = data[(sizeTraining+1):nData,]
          h = sigmoide(predict(mylogit,testingData))
          
          
          vetor.ajustado = c()
          
          for (k in 1:sizeTesting) {      
            if(h[k] >= 0.5) {
              vetor.ajustado[k] = 1
            }
            else {
              vetor.ajustado[k] = 0
            }
          }
          
          
          #erro
          tabela = table(vetor.ajustado == data$pop[(sizeTraining+1):nData])/sizeTesting
          tabela.acertos = c(tabela.acertos,tabela[[2]])
          tabela.erros = c(tabela.erros,tabela[[1]])
          
        } 
        
        ic_acertos = 100*ic(tabela.acertos)
        ic_erros = 100*ic(tabela.erros)
        tabela.regressao = rbind(tabela.regressao, c(paste(correlacoes$atributo[j],correlacoes$atributo[l], correlacoes$atributo[n], correlacoes$atributo[m],sep=" "), ic_acertos$lower, ic_acertos$upper))
      }
    }  
  }
}

tabela.regressao = as.data.frame(tabela.regressao)

colnames(tabela.regressao) = c("atributos", "limite.inf", "limite.sup")

tabela.regressao$limite.inf = as.numeric(levels(tabela.regressao$limite.inf))[tabela.regressao$limite.inf]

tabela.regressao$limite.sup = as.numeric(levels(tabela.regressao$limite.sup))[tabela.regressao$limite.sup]


tabela.regressao$media = (tabela.regressao$limite.sup + tabela.regressao$limite.inf)/2

tabela.regressao = tabela.regressao[order(tabela.regressao$media,decreasing=T),]

write.table(tabela.regressao,"regressao_estatistica_quatro_atributos.txt",sep="\t",col.names=T,row.names=F)

tabela.regressao = read.csv("regressao_estatistica_quatro_atributos.txt",sep="\t")






#correlação 

correlacao.spearman = cor(x=data$soma.hottness, y=data$quant.musicas,method="spearman")
#0.8857776

correlacao.kendall = cor(x=data$soma.hottness, y=data$quant.musicas,method="kendall")
#0.7440508

#gera imagem do intervalo
png("IC_Mean_Acertos_Erros.png")
ggplot()+
  geom_errorbar(aes(x=c("Acertos","Erros"), 
                    ymax=c(ic_acertos$upper, ic_erros$upper), 
                    ymin=c(ic_acertos$lower, ic_erros$lower)))+
  xlab("")+
  ylab("Porcentagem")  
dev.off()





