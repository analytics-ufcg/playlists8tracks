#argumentos = commandArgs()

#script de geração de media, mediana, desvio padrao e para os atributos: "hotness", "tempo", "danceability", "energy", "loudness", "duration", "timesignature" agrupados por playlist

#recebe o arquivo como entrada
data = read.csv("dados musicas playlists.txt",sep="\t",header = F)

#data = argumentos[1]


#arquivo de saida
output = "media, mediana e sd para todos os atributos das musicas agrupados por playlists.txt"

#output = argumentos[2]


#remove a coluna de tags, nome da musica e do artista e id da playlist
data1 = data[,5:11]
data = cbind(data[,1],data1)
rm(data1)


#ordena data frame por id da playslist
data = data[order(data[,1]),]


#renomeia as colunas
colnames(data) = c("idplay", "hotness", "tempo", "danceability", "energy", "loudness", "duration", "timesignature")


#calculo das similaridades

#calcula similaridade de um vetor
calculaSimilaridade = function(vetor) {
  if (length(vetor) <= 1) {
    return (NA)
  }
  vetor.similaridade = c()
  for (i in 1:length(vetor)) { 
    if (i != length(vetor)) {
      vetor.similaridade[i] = sqrt((vetor[i] - vetor[i+1])^2)
    }
    else {
      vetor.similaridade[i] = sqrt((vetor[i] - vetor[1])^2)
    }
  }
  return (vetor.similaridade)
}


#calcula similaridade para uma matrix
calculaSimilaridadeMatrix = function(matrix) {
  matrix.similaridade = c()
  for (i in 1:ncol(matrix)) {
    matrix.similaridade = cbind(matrix.similaridade, calculaSimilaridade(matrix[,i]))
  }
  return (matrix.similaridade)
}


#calcula a similaridade para cada playlist
calculaSimilaridadePlaylist = function(data) {
  idplays = sort(unique(data$idplay))
  similaridades = c()
  for (i in idplays) {
    matrix = subset(data, data$idplay == i)
    similaridades = rbind(similaridades, calculaSimilaridadeMatrix(matrix[,2:8]))
  }
  return (similaridades)
}


#calcula a similaridade por atributo e insere no data frame principal
similaridades = calculaSimilaridadePlaylist(data)

colnames(similaridades) = c("similaridade.hotness", "similaridade.tempo", "similaridade.danceability", "similaridade.energy", "similaridade.loudness", "similaridade.duration", "similaridade.timesignature")

data = cbind(data, similaridades)

rm(similaridades)


#calculo dos atributos
inicio = 2
fim = 15
#calculo da media
data.means = aggregate(data[inicio:fim], list(data$idplay), mean)

colnames(data.means) = c("idplay", "mean.hotness", "mean.tempo", "mean.danceability", "mean.energy", "mean.loudness", "mean.duration", "mean.timesignature",
                         "mean.similaridade.hotness", "mean.similaridade.tempo", "mean.similaridade.danceability", "mean.similaridade.energy", "mean.similaridade.loudness", "mean.similaridade.duration", "mean.similaridade.timesignature")


#calculo da mediana
data.medians = aggregate(data[inicio:fim], list(data$idplay), median)

colnames(data.medians) = c("idplay", "median.hotness", "median.tempo", "median.danceability", "median.energy", "median.loudness", "median.duration", "median.timesignature", 
                           "median.similaridade.hotness", "median.similaridade.tempo", "median.similaridade.danceability", "median.similaridade.energy", "median.similaridade.loudness", "median.similaridade.duration", "median.similaridade.timesignature")


#calculo do desvio padrão
data.sd = aggregate(data[inicio:fim], list(data$idplay), sd)

colnames(data.sd) = c("idplay", "sd.hotness", "sd.tempo", "sd.danceability", "sd.energy", "sd.loudness", "sd.duration", "sd.timesignature",
                      "sd.similaridade.hotness", "sd.similaridade.tempo", "sd.similaridade.danceability", "sd.similaridade.energy", "sd.similaridade.loudness", "sd.similaridade.duration", "sd.similaridade.timesignature")


#calcula soma hottness
data.soma.hottness = aggregate(data$hotness, list(data$idplay), sum)

colnames(data.soma.hottness) = c("idplay","soma.hottness")


#conta quantidade de musicas por playlist
data.quant.musicas = aggregate(data$hotness, list(data$idplay), length)

colnames(data.quant.musicas) = c("idplay","quant.musicas")

#junta todos os atributos e salva em um arquivo
data = cbind(data.means[,(inicio-1):fim], data.medians[,inicio:fim], data.sd[,inicio:fim], data.soma.hottness[,2], data.quant.musicas[,2])

colnames(data)[44:45] = c("soma.hottness","quant.musicas")

write.table(data, output, sep="\t", quote=F)


#rm(list=ls())

