maxLabel = max(runLabels)
eqTab = matrix(nrow = maxLabel,ncol = maxLabel, FALSE)
i = length(equivalence)
j = 1
while(j <= i){
  eqTab[equivalence[[j]][1],equivalence[[j]][2]] = TRUE
  eqTab[equivalence[[j]][2],equivalence[[j]][1]] = TRUE
  j = j + 1
}
labelFlag = seq(length = maxLabel,0,0)
equaList <- list()
tempList <- c()

for(i in 1:maxLabel){
  if(labelFlag[i]) next
  labelFlag[i] = length(equaList) + 1
  tempList <- c(tempList,i)
  j = 1
  while(j < length(tempList) + 1){
    k = 1
    while(k <= length(eqTab[tempList[j], ])){
      if(eqTab[tempList[j],k] & !labelFlag[k]){
        tempList <- c(tempList, k)
        labelFlag[k] = length(equaList) + 1
      }      
      k = k + 1
    }
    j = j + 1
    equaList[[length(equaList) + 1]] <- tempList
    tempList <- c()
  }
}
