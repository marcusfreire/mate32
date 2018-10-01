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
#Sensor de Quantidade de pessoas
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
#Calculando a Matriz de Distância para sensor de luminosidade
luminosidade=calculaMatrizDistancia(lum)
agrupamentoLUZ = hclust(as.dist(luminosidade))
#Plotando os agrupamentos
plot(agrupamentoLUZ)
rect.hclust(agrupamentoLUZ, k = 2, border = 2:4)
#Indice Silhueta
si = silhouette(cutree(agrupamentoLUZ,2),as.dist(luminosidade))
rownames(si) = rownames(luminosidade)
plot(si)

#Calculando a Matriz de Distância para sensor de Contagem de Pessoas
qtdPessoas=calculaMatrizDistancia(pessoas)
agrupamentoPessoas = hclust(as.dist(qtdPessoas))
#Plotando os agrupamentos
plot(agrupamentoPessoas)
rect.hclust(agrupamentoPessoas, k = 3, border = 2:4)
#Indice Silhueta
plot(silhouette(cutree(agrupamentoPessoas,2),as.dist(qtdPessoas)))
rownames(si) = rownames(qtdPessoas)
plot(si)

#Utilizando GGplot2 para plotar um dendrogram melhor, com varias bilbliotecas
dendLUZ <- agrupamentoLUZ %>% as.dendrogram %>%
   set("branches_k_color", k=2) %>% set("branches_lwd", 2) %>%
   set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>%
   set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
plot(dendLUZ)

dendPessoas <- agrupamentoPessoas %>% as.dendrogram %>%
   set("branches_k_color", k=3) %>% set("branches_lwd", 2) %>%
   set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>%
   set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
plot(dendPessoas)

### Calculando a silhueta para todos os grupos.

SilhuetaLuz = c()
for(i in 2:15){
  si = silhouette(cutree(agrupamentoLUZ,i),as.dist(luminosidade))
  SilhuetaLuz[i] = mean(si[,3])
}

plot(SilhuetaLuz,type="b")
abline(v=2,lty=2, lwd=2, col = "#E31A1C")
dev.copy(png,'SilhuetaLuz.png')
dev.off()

SilhuetaPessoas = c()
for(i in 2:15){
  si = silhouette(cutree(agrupamentoPessoas,i),as.dist(qutePessoas))
  SilhuetaPessoas[i] = mean(si[,3])
}

plot(SilhuetaPessoas,type="b")
dev.copy(png,'SilhuetaPessoas.png')
dev.off()
