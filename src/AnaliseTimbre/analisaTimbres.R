dados = read.table("similaridades.txt", sep="\t", header=F,
                   blank.lines.skip=F)

popu = read.table("prop-e-pop.txt", header=T)

followers = read.table("NumberOfFollowersByUser.txt")

idplay.userid = read.table("mixes-playid-userid.txt")
colnames(idplay.userid) = c("id_play", "user_id")

estatisticas = with(dados, aggregate(V4, list(V1), mean))
estatisticas = cbind(estatisticas, with(dados, aggregate(V4, list(V1), median)$x))
estatisticas = cbind(estatisticas,with(dados, aggregate(V4, list(V1), sd))$x)

colnames(estatisticas) = c("id_play", "media", "mediana", "desvio_padrao")

match = subset(popu, popu$id_play %in% estatisticas$id_play)
match = match[with(match, order(id_play)), ]

estatisticas = cbind(estatisticas, match[,5])
colnames(estatisticas) = c("id_play", "media", "mediana", "desvio_padrao", "pop")

write.table(estatisticas, "plays-statistics.txt", row.names=F,
            sep="\t")