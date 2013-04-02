library("stringr")
library("RecordLinkage")
library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)

## Cálculo de similaridade entre playlists

## Leitura de arquivos
options(stringsAsFactors = FALSE)
dados = read.table("arquivo-coletado-sem-nas.txt",header=T)
colnames(dados) = c("id_playlist","artist","song","id_music","tags")
listens = read.table("NumberOfListensByPlaylist.txt",header=T)
likes = read.table("NumberOfLikesByPlaylist.txt",header=T)
nusers = read.table("NumberOfFollowersByUser.txt",header=T)

## Separando as tags mais relevantes por playlist

bestPlayTags = function(tags){
	res = c()
	w = c()
	for(i in 1:length(tags)){
		vtags = as.vector(tags[i])
		sptags = as.data.frame(strsplit(vtags,","))
		colnames(sptags) = c("V1")
		sptags = as.vector(sptags$V1)
		
		for(t in 1:length(sptags)){
			wo2d = as.data.frame(strsplit(as.vector(sptags[t]),":"))
			print(wo2d[1,])
			if(nrow(wo2d)==2){
				if(as.numeric(wo2d[2,]) > 50){
					res = c(res,str_trim(toupper(wo2d[1,])))	
					w = c(w,wo2d[2,])	
				}else if(as.numeric(wo2d[2,]) < 50){
					break
				
				}
			}
			
		}

		
	}

	## Somando pesos e removendo duplicatas
	if(length(res) ==0){
		res = c("NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")
		w = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
	}
	df = data.frame(tag = res, weight = w)
	df =  aggregate(as.numeric(df$weight), list(df$tag), sum)
	df = df[with(df, order(-x)), ]
	
	## Retornando top 15 tags mais "pesadas"
	return(df[1:15,1])
	
}


countTagsPlaylist = function(tags){
	res = c()
	for(i in 1:length(tags)){
		vtags = as.vector(tags[i])
		sptags = as.data.frame(strsplit(vtags,","))
		colnames(sptags) = c("V1")
		sptags = as.vector(sptags$V1)
		res = c(res,length(sptags))
			
	}
	print(sum(res))
	return(sum(res))
}



touse = read.table("plays-statistics.txt",header=T)
id_play = touse$id_play 
ind = which(dados$id_playlist %in% id_play)
touse = dados[ind,]

nTags = aggregate(touse$tags, list(touse$id_playlist), countTagsPlaylist)



## Testando com 2 playlists de tamanhos diferentes
teste_tags = dados[1:15, c(1,5)]  
t_test1 = as.data.frame(teste_tags[1:8,])  # Todas as tags da playlist 215 que tem 8 musicas
t_test2 = as.data.frame(teste_tags[9:15,]) # Todas as tags da playlist 267 que tem 7 musicas

sim = c()
simJac = c()
N=10
# Comparando músicas de uma mesma playlist
for(i in 1:nrow(t_test1)){
	if((i+1) < nrow(t_test1)){
	sim = c(simPop, levenshteinSim(paste(bestPlayTags(t_test1$tags[i]),collapse=" "),paste(bestPlayTags(t_test1$tags[i+1]),collapse=" ")))
	simJac = c(simJacPop,jaccard(as.vector(bestPlayTags(t_test1$tags[i])),  as.vector(bestPlayTags(t_test1$tags[i]))))
	}	
}
print(mean(sim))
print(mean(simJac))


tagsF = aggregate(teste_tags$tags, list(teste_tags$id_playlist), bestPlayTags)

## Calculando estatísticas
library(vegan)
jaccard.index = jaccard(as.vector(tagsF[1,-1]),  as.vector(tagsF[2,-1]))
#euclidian.distance = eudist(play1,play2)

ldist = adist(as.vector(tagsF[1,-1]),  as.vector(tagsF[2,-1]))

#Dummy test com Levenshtein
# Generalized Levenshtein (edit) distance, giving   the minimal possibly weighted number of insertions,
# deletions and substitutions needed to transform one string into another
#
# Funciona legal porque considera a ordem das palavras, que no nosso caso importa já que as primeiras são as mais "pesadas"
a = c("INDIE","ROCK","POP","FOLK")
b = c("INDIE","POP","FOLK","S")
lind = levenshteinSim(paste(a,collapse=" "), paste(b,collapse=" ")) # res = 0.6315789


## Lendo arquivo com "id_play" "total" "total_coletadas" "favs" "listens" "pop" "prop_coleta"
pop_e_unpop = read.table("pop-e-prop-sem-nas.txt",header=T)


## Usando as 1000 playlists mais populares e as 1000 menos populares,
## mas ambas com mais de 50 listens;


## Calculando similaridade entre populares
pop = subset(pop_e_unpop, pop_e_unpop$pop>0.80 & pop_e_unpop$listens>50) 
pop = pop[sample(nrow(pop), 1000), ]	
indpop= which(tagsTOTAL$id_playlist %in% pop$id_play ==  TRUE) #ocorrencias = 33621
tagspop = tagsTOTAL[c(indpop),]
pop = aggregate(tagspop$tags, list(tagspop$id_playlist), bestPlayTags)

## Calculando similaridade entre não-populares
unpop = subset(pop_e_unpop, pop_e_unpop$pop<0.20 & pop_e_unpop$listens>50) 
unpop = unpop[sample(nrow(unpop), 1000), ]	
indunpop= which(tagsTOTAL$id_playlist %in% unpop$id_play ==  TRUE) # ocorrencias = 9404
tagsunpop = tagsTOTAL[c(indunpop),]
unpop = aggregate(tagsunpop$tags, list(tagsunpop$id_playlist), bestPlayTags)


# Similaridade considerando apenas N primeiras tags
N=10

simPop = c()
simJacPop = c()
for(i in 1:nrow(pop)){
	if((i+1) < nrow(pop)){
		simPop = c(simPop, levenshteinSim(paste(pop$x[i,1:N],collapse=" "), paste(pop$x[i+1,1:N],collapse=" ")))
		simJacPop = c(simJacPop,jaccard(as.vector(pop$x[i,1:N]),  as.vector(pop$x[i+1,1:N])))
	}	
}
print(mean(simPop))
print(mean(simJacPop))


simUnPop = c()
simJacUnPop = c()
for(i in 1:nrow(unpop)){
	if((i+1) < nrow(unpop)){
		simUnPop = c(simUnPop, levenshteinSim(paste(unpop$x[i,1:N],collapse=" "), paste(unpop$x[i+1,1:N],collapse=" ")))
		simJacUnPop = c(simJacPop,jaccard(as.vector(unpop$x[i,1:N]),  as.vector(unpop$x[i+1,1:N])))
	}
}
print(mean(simUnPop))
print(mean(simJacUnPop))


simMix = c()
simJacMix = c()
for(i in 1:nrow(pop)){
	if((i+1) < nrow(pop)){
		simMix = c(simMix, levenshteinSim(paste(pop$x[i,1:N],collapse=" "), paste(unpop$x[i,1:N],collapse=" ")))
		simJacMix = c(simJacMix,jaccard(as.vector(pop$x[i,1:N]),  as.vector(unpop$x[i,1:N])))
	}
}
print(mean(simMix))
print(mean(simJacMix))

png("distribuicao-similaridade.png")
plot(density(simMix),xlim=c(0,1),col="blue",xlab="% Similaridade",ylab="Frequência",main="Distribuição das similaridades\nAzul = Levenhstein  Vermelho = Jaccard")
lines(density(simJacMix),col="red")
dev.off()



bestPlayTags = function(tags){
	res = c()
	w = c()
	for(i in 1:length(tags)){
		vtags = as.vector(tags[i])
		sptags = as.data.frame(strsplit(vtags,","))
		colnames(sptags) = c("V1")
		sptags = as.vector(sptags$V1)
		
		for(t in 1:length(sptags)){
			wo2d = as.data.frame(strsplit(as.vector(sptags[t]),":"))
			print(wo2d[1,])
			if(nrow(wo2d)==2){
				if(as.numeric(wo2d[2,]) >= 40){
					res = c(res,str_trim(toupper(wo2d[1,])))	
					w = c(w,wo2d[2,])	
				}else if(as.numeric(wo2d[2,]) < 50){
					break
				
				}
			}
			
		}

		
	}

	## Somando pesos e removendo duplicatas
	if(length(res) ==0){
		res = c("NA","NA","NA","NA","NA")
		w = c(0,0,0,0,0)
	}
	df = data.frame(tag = res, weight = w)
	df =  aggregate(as.numeric(df$weight), list(df$tag), sum)
	df = df[with(df, order(-x)), ]
	
	## Retornando top 15 tags mais "pesadas"
	return(df[1:5,1])
	
}


jaccard = function(play1,play2){
	intersection = sum((play1 %in% play2) == TRUE)
	total = length(play1)+length(play2)-intersection
	return(intersection/total)
}


notNATags = function(tags){
	indna = c(1,2,which(is.na(tags)))
	tags = tags[-indna]
	t = paste(tags,collapse=" ")
	return(t)
}


jaccard2 = function(play1,play2){
	indna1 = c(which(is.na(play1)))
	indna2 = c(which(is.na(play2)))
	
	if(length(indna1) != 0){
		play1 = play1[-indna1]
	}

	if(length(indna2) != 0){
		play2 = play2[-indna2]
	}
	

	intersection = sum((play1 %in% play2) == TRUE)
	total = length(play1)+length(play2)-intersection
	return(intersection/total)
}

vec = function(tags){
	v = as.data.frame(strsplit(tags,";"))
	colnames(v)=c("tags")
	return(v$tags)
}

simBestTags = function(tags,fun){

	simJac = c()
	
	for(i in 1:length(tags)){
		
		
		if((i+1) < length(tags)){
			#simLev = c(simLev, levenshteinSim(notNATags(tags[i,]), notNATags(tags[i+1,])))
			simJac = c(simJac,jaccard2(vec(tags[i]),  vec(tags[i+1])))
			#print(paste(simJac[i],": ",tags[i],"// ",tags[i+1]))			
		}		
		
	}
	print(simJac[length(simJac)])
	
	if(length(simJac)==0){
		media = NA
		mediana = NA
		desvio.padrao = NA
	}else{
		media = mean(simJac)
		mediana = median(simJac)
		desvio.padrao = sd(simJac)
	}
	
	return(paste(as.character(media),as.character(mediana),as.character(desvio.padrao),sep=" "))
		
}

## Gerando base de 15 tags mais populares para todas as playlists
tagsTOTAL = dados[c(1,4,5)] 
#tagsF = aggregate(tagsTOTAL$tags, list(tagsTOTAL$id_playlist,tagsTOTAL$id_music), bestPlayTags)
#tagsF = read.csv("top-tags-por-musicas.txt")
tagsF = read.csv("top-tags-por-musicas-sem-tag-na.txt")
playS = read.table("plays-statistics.txt",header=T)


valid = which(tagsF$id_play %in% playS$id_play)
ttags = tagsF[valid,]

#testar com base pequena antes
ttags = head(tagsF,1000)
ind = c(3:7)
#tagsFinal = aggregate(ttags[ind], list(ttags$id_play), simBestTags)   
ttags$tags = NA
for(i in 1:nrow(ttags)){
	ttags[i,]$tags = paste(ttags[i,ind],collapse=";")
}
indR = c(3:7)
ttags = ttags[-indR]
tagsFinal = aggregate(ttags$tags, list(ttags$id_play), simBestTags)  

## base total
ttags = tagsF
ind = c(3:7)
#tagsFinal = aggregate(ttags[ind], list(ttags$id_play), simBestTags)   
ttags$tags = NA
#for(i in 1:nrow(ttags)){
#	ttags[i,]$tags = paste(ttags[i,ind],collapse=";")
#}

ttags$tags = with(ttags, paste(x.1,x.2,x.3,x.4,x.5, sep=";"))
#write.table(ttags,"best-tags-sep.txt",row.names=F,col.names=F)


indR = c(2:7)
ttags = ttags[-indR]

tagsFinal = aggregate(ttags$tags, list(ttags$id_play), simBestTags)  
tagsFinalMetricas = data.frame(do.call('rbind', strsplit(as.character(tagsFinal$x),' ',fixed=TRUE)))
tagsFinal = data.frame(tagsFinal,tagsFinalMetricas)
tagsFinal = tagsFinal[-2]
head(tagsFinal,10)
colnames(tagsFinal) = c("id_play","media","mediana","desvio.padrao")
tagsFinal = merge(playS,tagsFinal,by=c("id_play"))
ind=c(2:4)
tagsFinal= tagsFinal[-ind]
write.csv(tagsFinal,"tags-statistics.txt",row.names=F)




##Cálculo de correlação
tagsS = read.csv("tags-statistics.txt")
cor1 = tagsS[sample(nrow(tagsS), 6000), ]	
cor1 = cor(cor1$user_followers,cor1$likes)

corF = c()
for(i in 1:20){
	cor1 = tagsS[sample(nrow(tagsS), 1000), ]	
	cor1 = cor(cor1$user_followers,cor1$likes)	
	corF = c(corF,cor1)
}
print(mean(corF))

pop = tagsS[tagsS$likes>quantile(likes$likes, 0.80),] 
unpop = tagsS[tagsS$likes<quantile(likes$likes, 0.20),] 
bt = read.csv("top-15-tags-per-playlists.txt")
colnames(bt) = c("id_play","x.1","x.2","x.3","x.4","x.5","x.6","x.7","x.8","x.9","x.10","x.11","x.12","x.13","x.14","x.15")
pop = merge(pop,bt,by=c("id_play"))
unpop = merge(unpop,bt,by=c("id_play"))
pop = pop[c(1:11)]
unpop = unpop[c(1:11)]
write.csv(pop,"popdata.txt",row.names=F)
write.csv(unpop,"unpopdata.txt",row.names=F)

png("popularidade-por-ntags.png")
plot(density(pop$ntags),col="blue",xlim=c(0,3000),ylim=c(0,0.002),xlab="Qtde de tags",main="Nº de tags por popularidade",ylab="Densidade")
lines(density(unpop$ntags),col="red")
legend("topright", inset=.05, c("Populares","Não-populares"), fill=c("blue","red"), horiz=TRUE)
dev.off()

testNP = wilcox.test(pop$user_followers,unpop$user_followers,alternative="less")

testNT = wilcox.test(pop$ntags,unpop$ntags,alternative="less")

ptags = pop[c(7:11)]
uptags = unpop[c(7:11)]

ptagsT = as.matrix(ptags$x.1,ptags$x.2,ptags$x.3,ptags$x.4,ptags$x.5) 
uptagsT = as.matrix(uptags$x.1,uptags$x.2,uptags$x.3,uptags$x.4,uptags$x.5) 

ptagsT = as.data.frame(table(ptagsT)) 
uptagsT = as.data.frame(table(uptagsT))

ptagsT = ptagsT[order(-ptagsT$Freq),]
uptagsT = uptagsT[order(-uptagsT$Freq),]

#write.table(ptagsT,file="poptags.txt",row.names=F)
#write.table(uptagsT,file="unpoptags.txt",row.names=F)

#pal <- brewer.pal(9, "PuBu")
#pal <- pal[-(1:2)]
#wordcloud(ptagsT$ptagsT,ptagsT$Freq,scale=c(10,.9),min.freq=10,random.color=TRUE,rot.per=.1, colors=pal,vfont=c("sans serif","plain"))
#wordcloud(uptagsT$uptagsT,uptagsT$Freq,scale=c(10,.9),min.freq=10,random.color=TRUE,rot.per=.1, colors=pal,vfont=c("sans serif","plain"))

png("popularidade-por-seguidores.png")
plot(density(pop$user_followers),col="blue",xlim=c(0,2000),ylim=c(0,0.010),xlab="Qtde de seguidores",main="Seguidores por popularidade",ylab="Densidade")
lines(density(unpop$user_followers),col="red")
legend("topright", inset=.05, c("Populares","Não-populares"), fill=c("blue","red"), horiz=TRUE)
dev.off()


# Sumários
summary(likes$likes)
summary(nusers$count)
summary(listens$listens)

