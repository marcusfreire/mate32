dados = iris[,1:2]

dist=function(x,y){
	sqrt(sum((x-y)^2))
}

distanciaCentroide=function(dados,tamanho, QteClusters, centroide) {
	matrizDist = matrix(nrow=tamanho,ncol=(QteClusters+1))
	for(i in 1:tamanho){
		for(j in 1:QteClusters){
			matrizDist[i,j]=dist(centroide[j,],dados[i,])
		}
		matrizDist[i,(QteClusters+1)] = which.min(matrizDist[i,])
	}
	matrizDist
}

plotGrupos=function(dados, QteClusters, matrizDist,centroide){
	limitemaximo = ceiling(apply(dados, 2, max))
	limiteminimo = floor(apply(dados, 2, min))
	cores = rainbow(QteClusters)
	plot(dados[which(matrizDist[,QteClusters+1]==1),],xlim=c(limiteminimo[1],limitemaximo[1]),ylim=c(limiteminimo[2],limitemaximo[2]))
	points(centroide[1,],pch=20,col="black")
	#legend("bottomright", legend="1", col=1, bty="n", pch=19)
	for(k in 2:QteClusters){
		points(dados[which(matrizDist[,QteClusters+1]==k),],col=cores[k-1])
		points(centroide[k,],pch=20,col=cores[k-1])
	}
	legend("bottomright", legend=c(1:k), col=c(1,cores[2:k-1]), bty="n", pch=19)

}

kmeans=function(dados,QteClusters){
		tamanho<-nrow(dados)
		centroide=dados[sample(1:tamanho,QteClusters),]
		matrizDist = distanciaCentroide(dados,tamanho, QteClusters, centroide)
		plotGrupos(dados, QteClusters, matrizDist,centroide)
		erro = T
		iteracao = 0
		while(erro){
			iteracao=iteracao+1
			cat("Iteração:",iteracao,"\n")
			flag = rep(1,QteClusters)
			centroideAntigo = centroide
			for(cluster in 1:QteClusters){
				teste = dados[which(matrizDist[,QteClusters+1]==cluster),]
				centroide[cluster,]=apply(teste, 2, mean)
				if ((centroide[cluster,1] == centroideAntigo[cluster,1]) & (centroide[cluster,2] == centroideAntigo[cluster,2]) ){
					flag[cluster] = 0
				}
			}
			cat("Flag",flag,"\n")
			if(!sum(flag)){
				erro=F
				break
			}
			matrizDist = distanciaCentroide(dados,tamanho, QteClusters, centroide)
			plotGrupos(dados, QteClusters, matrizDist,centroide)
			Sys.sleep(3)
		}
		m = matrix(nrow=1,ncol=QteClusters)
		rownames(m) <- "Qte Elementos por Grupos"
		for(k in 1:QteClusters){
			m[1,k]= length(which(matrizDist[,QteClusters+1]==k))
		}
		m
}
