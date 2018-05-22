dados = iris[sample(1:150,5),c(1,2)]

dist=function(x,y){
	sqrt(sum((x-y)^2))
}

calculaMatrizDistancia=function(dados) {
  tamanho = length(dados[,1])
	matrizDist = matrix(nrow=tamanho,ncol=tamanho)
	for(i in 1:tamanho){
		for(j in 1:tamanho){
      if(i>j | i==j){
        next
      }
			matrizDist[i,j]=dist(dados[i,],dados[j,])
		}
		#matrizDist[i,(QteClusters+1)] = which.min(matrizDist[i,])
	}
	matrizDist
}

m = calculaMatrizDistancia(dados)
which.min(m)
which.min(m[c(-17)])
which.min(m[-c(17,16)])
