if (!require("pdc")) install.packages("pdc")
if (!require("dtw")) install.packages("dtw")
if (!require("magrittr")) install.packages("magrittr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("cluster")) install.packages("cluster")
if (!require("factoextra")) install.packages("factoextra")
if (!require("dendextend")) install.packages("dendextend")

#pacote, referente ao doutorado do Prof. Ricardo
require("TSDecomposition")

dados = read.csv("dados08_06_a_04_07.csv",head=T)

dados$TIMESTAMP_ <- strptime(dados$TIMESTAMP_, format= "%Y-%m-%d %H:%M:%S")
df=data.frame(dados$TIMESTAMP_, dados$TEMPERATURA, dados$SOM, dados$LUZ, dados$UMIDADE, dados$QUANT_PESSOAS)
colnames(df) <- c("TIMESTAMP_", "TEMPERATURA","SOM","LUZ","UMIDADE","QUANT_PESSOAS")
head(df)

datas<- strptime(df$TIMESTAMP_, format= "%Y-%m-%d %H:%M:%S")
datas = unclass(datas)
dfCompleto=data.frame(datas$mon,datas$mday,datas$wday,datas$hour,datas$min,df$TEMPERATURA,df$SOM,df$LUZ, df$UMIDADE,
  df$QUANT_PESSOAS)
colnames(dfCompleto) <- c("MES","DATA_DIA","DIA_SEMANA","HORA","MINUTO","TEMPERATURA","SOM","LUZ","UMIDADE","QUANT_PESSOAS")
head(dfCompleto)


#Separando a série temporal em Dias
dia = function(dfCompleto,df,coluna){
  freq_dias=table(dfCompleto$DATA_DIA)
  atributo = data.frame()
  dia = df[which(dfCompleto$DATA_DIA==as.integer(names(freq_dias[1]))),]
  add = (nrow(dia)-min(freq_dias))+1
  atributo = data.frame(as.numeric(dia[add:nrow(dia),coluna]))
  for(i in 2:length(freq_dias)){
    dia = df[which(dfCompleto$DATA_DIA==as.integer(names(freq_dias[i]))),]
    add = (nrow(dia)-min(freq_dias))+1
    atributo <- cbind(atributo, as.numeric(dia[add:nrow(dia),coluna]))
  }
  colnames(atributo) = labels(freq_dias)[[1]]
  atributo
}
#Sensor de luminosidade
lum=dia(dfCompleto,df,4)
pessoas= dia(dfCompleto,df,6)


# Calcula matriz de Distância com o DTW ou MDDL
calculaMatrizDistancia=function(dados){
  tamanho = ncol(dados)
	matrizDist = matrix(nrow=tamanho,ncol=tamanho)
	for(i in 1:tamanho){
		for(j in 1:tamanho){
      if(i<j | i==j){
        next
      }
			#matrizDist[i,j]=mddl(dados[,i],dados[,j])
			matrizDist[i,j]=dtw(dados[,i],dados[,j])$normalizedDistance
		}
	}
	colnames(matrizDist) = labels(dados)[[2]]
	rownames(matrizDist) = labels(dados)[[2]]
	matrizDist
}
luminosidade=calculaMatrizDistancia(lum)
agrupamentoLUZ = hclust(as.dist(luminosidade))
plot(agrupamentoLUZ)
rect.hclust(agrupamentoLUZ, k = 3, border = 2:4)


pessoas= dia(dfCompleto,df,6)
qtdPessoas=calculaMatrizDistancia(pessoas)
agrupamentoPessoas = hclust(as.dist(qtdPessoas))
plot(agrupamentoPessoas)
rect.hclust(agrupamentoPessoas, k = 3, border = 2:4)

#plot do Dendograma
plot(agrupamentoLUZ)
cutree(agrupamentoLUZ,2)

plot(silhouette(cutree(agrupamentoLUZ,2),as.dist(luminosidade)))


abline(h=550,lty=2, lwd=2, col = "#E31A1C")
#abline(h=400000,lty=2, lwd=2, col = "#E31A1C")

#Teste com o GGPLOT2 para um dendograma mais didático
if (!require("magrittr")) install.packages("magrittr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("factoextra")) install.packages("factoextra")
if (!require("dendextend")) install.packages("dendextend")


dend <- agrupamentoLUZ %>% as.dendrogram %>%
   set("branches_k_color", k=2) %>% set("branches_lwd", 2) %>%
   set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>%
   set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
plot(dend)
dev.copy(png,'Dend2Grupos.png')
dev.off()

dend <- agrupamentoPessoas %>% as.dendrogram %>%
   set("branches_k_color", k=3) %>% set("branches_lwd", 2) %>%
   set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>%
   set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
plot(dend)
dev.copy(png,'Dend2GruposPessoas.png')
dev.off()

a=which(grupos==1)
par(mfrow=c(19,1))
for(i in labels(a)){
  plot.ts(lum[,i])
}
###PLOT slide
freq_dias=table(dfCompleto$DATA_DIA)

par(mfrow=c(4,1))
dia = df[which(dfCompleto$DATA_DIA==as.integer(names(freq_dias["23"]))),]
plot(LUZ~TIMESTAMP_, data=dia, type="l", xaxt="n", main="Data: 23/06")
    axis.POSIXct(1, at = seq(dia$TIMESTAMP_[1],dia$TIMESTAMP_[nrow(dia)], by = "hour"), format = "%H")
dia = df[which(dfCompleto$DATA_DIA==as.integer(names(freq_dias["24"]))),]
plot(LUZ~TIMESTAMP_, data=dia, type="l", xaxt="n", main="Data: 24/06")
    axis.POSIXct(1, at = seq(dia$TIMESTAMP_[1],dia$TIMESTAMP_[nrow(dia)], by = "hour"), format = "%H")
dia = df[which(dfCompleto$DATA_DIA==as.integer(names(freq_dias["26"]))),]

plot(LUZ~TIMESTAMP_, data=dia, type="l", xaxt="n", main="Data: 26/06")
        axis.POSIXct(1, at = seq(dia$TIMESTAMP_[1],dia$TIMESTAMP_[nrow(dia)], by = "hour"), format = "%H")
dia = df[which(dfCompleto$DATA_DIA==as.integer(names(freq_dias["22"]))),]

plot(LUZ~TIMESTAMP_, data=dia, type="l", xaxt="n", main="Data: 22/06")
                axis.POSIXct(1, at = seq(dia$TIMESTAMP_[1],dia$TIMESTAMP_[nrow(dia)], by = "hour"), format = "%H")

dev.copy(png,'23e24e26e22.png')
dev.off()

par(mfrow=c(1,2))
mddl(lum[,"23"],lum[,"26"],plot=T)
mddl(lum[,"23"],lum[,"26"],plot=T,shaded=T)
dev.copy(png,'DTWeMDDL.png')
dev.off()


plot.ts(lum[,"23"])

plot.ts(lum[,"24"])
plot.ts(lum[,"26"])

plot.ts(lum[,"22"])

mddl(lum[,"23"],lum[,"26"])
dtw(lum[,"23"],lum[,"26"])
mddl(lum[,"23"],lum[,"26"])

# Utilizando a biblioteca PDC
#Agrupando
clustering <- pdclust(lum)

plot(clustering)
#Para colorir o plot, muito massa.
# 2 Grupos
grupos = cutree(clustering,2)
plot(clustering, cols=grupos)
dev.copy(png,'2grupos.png')
dev.off()
# 3 Grupos
grupos = cutree(clustering,3)
plot(clustering, cols=grupos)
dev.copy(png,'LumDendo.png')
dev.off()

############## Indices
#Silhouette
fviz_nbclust(lum, hcut, method = "silhouette",hc_method = "complete")

fviz_nbclust(pessoas, hcut, nstart = 25,
  method = "gap_stat", nboot = 50) + labs(subtitle = "Gap statistic method")

Silhueta = c()
for(i in 2:15){
  si = silhouette(cutree(agrupamentoLUZ,i),as.dist(luminosidade))
  Silhueta[i] = mean(si[,3])
}

plot(Silhueta,type="b")
abline(v=2,lty=2, lwd=2, col = "#E31A1C")
dev.copy(png,'Silhueta.png')
dev.off()

SilhuetaPessoas = c()
for(i in 2:15){
  si = silhouette(cutree(agrupamentoPessoas,i),as.dist(qutePessoas))
  Silhueta[i] = mean(si[,3])
}

plot(Silhueta,type="b")
