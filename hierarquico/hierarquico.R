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

#return line and columns, try version#
lineAndColumn=function(data,value){
  tmp = 1
  count = 0
  total = length(data[,1])*length(data[1,])
  while(total){
    cache = 0
    for (i in data[,tmp]) {
      cache = cache + 1
      count = count + 1
      if(count == value){
        c(cache,tmp)
        break
      }
   }
    tmp = tmp + 1
    total = total - 1
  }
}




m = calculaMatrizDistancia(dados)
which.min(m)
which.min(m[c(-17)])
which.min(m[-c(17,16)])
