require("Hmisc")
output = "estatisticas-das-playlists.txt"

#junta os atributos de 2 arquivos

data1 = read.csv("media, mediana e sd para todos os atributos das musicas agrupados por playlists.txt",sep="\t")

data2 = read.csv("plays-statistics.txt",sep="\t")

data = cbind(data1,data2)

data3 = read.csv("tags-statistics.txt",sep=",")
data3 = data3[order(data3$id_play),]

data = data[data$idplay %in% data3$id_play, ]

data = cbind(data,data3)


rm(data1)
rm(data2)
rm(data3)

#selecionada os 10% mais e menos populares

quantile(data$pop, c(.10,.90))

data = subset(data, data$pop <= 0.09090909 | data$pop >= 0.54545455)

write.table(data, output, sep="\t", quote=F)

data = read.csv("estatisticas-das-playlists.txt",sep="\t")


#retira atributos desnecessarios
data = data[ , -which(names(data) %in% c("idplay","id_play","id_play.1","user_id","id_user","popularity"))]

#retira plays com NAs

data = na.omit(data)


#monta as imagens dos plots pop x atributo
altura = 900
largura = 900

png('1-9.jpg',width=largura,height=altura)

par(mfrow=c(3,3))
for (i in 1:9) {
  plot(pop ~ data[,i], data = data,xlab=colnames(data)[i]) 
}

dev.off()
#####################

png('10-18.jpg',width=largura,height=altura)

par(mfrow=c(3,3))
for (i in 10:18) {
  plot(pop ~ data[,i], data = data,xlab=colnames(data)[i]) 
}

dev.off()
######################

png('19-27.jpg',width=largura,height=altura)

par(mfrow=c(3,3))
for (i in 19:27) {
  plot(pop ~ data[,i], data = data,xlab=colnames(data)[i]) 
}

dev.off()
######################

png('28-36.jpg',width=largura,height=altura)

par(mfrow=c(3,3))
for (i in 28:36) {
  plot(pop ~ data[,i], data = data,xlab=colnames(data)[i]) 
}

dev.off()
########################

png('37-45.jpg',width=900,height=900)

par(mfrow=c(3,3))
for (i in 37:45) {
  plot(pop ~ data[,i], data = data,xlab=colnames(data)[i]) 
}

dev.off()
########################

png('47-51.jpg',width=900,height=900)

par(mfrow=c(3,2))
for (i in 47:51) {
  plot(pop ~ data[,i], data = data,xlab=colnames(data)[i]) 
}

dev.off()

#termina de gerar as imagens


#Calcular correlação dos atributos com popularidade

#Pearson
vetor.pearson = c()

for (i in 1:ncol(data)) {
  correlacao = cor(x=data$pop, y=data[,i],method="pearson")
  vetor.pearson = rbind(vetor.pearson,c(colnames(data)[i], correlacao))
}

vetor.pearson = vetor.pearson[order(vetor.pearson[,2],decreasing=T),]

colnames(vetor.pearson) = c("atributo", "correlacao.pearson")


#Kendall
vetor.kendall= c()

for (i in 1:ncol(data)) {
  correlacao = cor(x=data$pop, y=data[,i],method="kendall")
  vetor.kendall = rbind(vetor.kendall,c(colnames(data)[i], correlacao))
}

vetor.kendall = vetor.kendall[order(vetor.kendall[,2],decreasing=T),]

colnames(vetor.kendall) = c("atributo", "correlacao.kendall")


#Spearmen
vetor.spearman= c()

for (i in 1:ncol(data)) {
  correlacao = cor(x=data$pop, y=data[,i],method="spearman")
  vetor.spearman = rbind(vetor.spearman,c(colnames(data)[i], correlacao))
}

vetor.spearman = vetor.spearman[order(vetor.spearman[,2],decreasing=T),]

colnames(vetor.spearman) = c("atributo", "correlacao.spearman")


correlacao = as.data.frame(cbind(vetor.kendall,vetor.pearson,vetor.spearman))


write.table(correlacao, "correlacao dos atributos x popularidade.txt", sep="\t", quote=F)


