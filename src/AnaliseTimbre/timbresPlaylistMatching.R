tabAll <- read.csv("output_all.txt", sep="\t", header=F)
tabAll = tabAll[, c(1,12)]
tabAll$V12 = as.character(tabAll$V12)
#tabb = as.data.frame(with(tabAll, table(V1)))
#hist(tabb$Freq)

idplay.userid = read.table("mixes-playid-userid.txt")
colnames(idplay.userid) = c("id_play", "user_id")

reggae = tabAll[which(!(tabAll$V1 %in% idplay.userid$id_play)), ]
tabAll = tabAll[!(tabAll$V1 %in% reggae$V1), ]

timbres = as.data.frame(list.files(path = "./timbres", all.files = T, full.names = T))
colnames(timbres) = c("path")
timbres$path = as.character(timbres$path)
timbres$path = substr(timbres$path, 3, nchar(timbres$path))

sub = subset(tabAll, timbres$path %in% tabAll$V12)
sub$V12 = paste("./", sub$V12, sep="")
sub = sub[with(sub, order(V1)), ]

dist = as.data.frame(table(sub$V1))
quantile(as.numeric(dist$Freq), probs=c(.25, .50, .75, .95))

#Tira playlists com menos de 2 musicas
#dist2 = subset(dist, dist$Freq >= quantile(as.numeric(dist$Freq), probs=.25))
dist2 = subset(dist, dist$Freq >= 2)

filtro = subset(sub, sub$V1 %in% dist2$Var1)

popularidade = read.table("prop-e-pop.txt", header=T)
popularidade = popularidade[, c(1, 5)]
popularidade = popularidade[with(popularidade, order(factor(-pop))), ]

pop.filtrado = subset(popularidade, 
                      as.numeric(popularidade$id_play) %in% as.numeric(filtro$V1))
pop.filtrado = subset(pop.filtrado, pop.filtrado$pop <= 1)

play.timbre = subset(filtro, filtro$V1 %in% pop.filtrado$id_play)

write.table(play.timbre, "play-timbre.txt", row.names=F, sep="\t", col.names=F)