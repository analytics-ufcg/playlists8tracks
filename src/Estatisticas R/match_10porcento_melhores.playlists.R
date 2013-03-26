#Filtra as playlists 10% mais e menos populares, retira os atributos tags e pasta do timbre
output = "dados musicas playlists.txt"


data.plays = read.csv("plays-statistics.txt",sep="\t")
data.plays = data.plays$id_play


data.complete = read.csv("output_all.txt", sep = "\t", header = F)


data.complete = data.complete[data.complete$V1 %in% data.plays, 1:11]


write.table(data.complete, output, sep="\t", quote=F, row.names = F, col.names = F)

