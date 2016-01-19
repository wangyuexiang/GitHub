##########
##########
# Algo2_Functions.R
# define all functions for Algo2_ZoneWindows
##########
### List of all functions
# ZoneLabel

ZoneLabel <- function(transaction) {
  # For each ID, connect the Grid to make Zone
  # Args:
  #	  tansaction:	Entr, Sor, Row, Col, ...
  # Returns:
  #	  tansaction:	Entr, Sor, Row, Col, ..., Zone

	if(nrow(transaction) == 0) {
	# if no transaction, return same data with column Zone
		print("No frequent Grid!")
		return(transaction %>% mutate(Zone = 0))
	}
  
	# add column Zone
  result <- transaction %>% mutate(Zone = 0) %>% slice(0)
  
	# get the list of ID in transaction
	ID.list <- transaction %>% ungroup %>% select(ID) %>% distinct

	# get the number of Row, Col in the GridLimited created
	numRow <- max(GridLimit$Row)
	numCol <- max(GridLimit$Col)

	# for each ID
	for(tempID in ID.list$ID){
		tempOnePerson <- transaction %>% filter(ID == tempID)
		
		# prepare the matrix
		m <- matrix(0, nrow = numRow, ncol = numCol)
		for(i in 1:nrow(tempOnePerson)) m[tempOnePerson$Row[i],tempOnePerson$Col[i]] <- 1

		##########
		### Step 1: find all Runs 
		##########
		NumberOfRuns <- 0;
		rowRun <- c()
		stRun <- c()
		enRun <- c()
		for(i in 1:numRow){
			rowData <- m[i,]
			if(rowData[1] == 1){
				NumberOfRuns <- NumberOfRuns + 1
				stRun <- c(stRun, 1)
				rowRun <- c(rowRun, i)
			}
			j = 1
			while(j < numCol){
				if(rowData[j] == 0 & rowData[j + 1] == 1){
					NumberOfRuns <- NumberOfRuns + 1
					stRun <- c(stRun, j + 1)
					rowRun <- c(rowRun, i)
				} else if(rowData[j] == 1 & rowData[j+1] == 0){
					enRun <- c(enRun, j)
				}
				j = j + 1
			}
			if(rowData[numCol]) enRun <- c(enRun, numCol)
		} 
		# end of step 1
		
		##########
		### Step 2: find all pairs
		##########
		idxLabel = 1
		curRowIdx = 1
		firstRunOnCur = 1
		firstRunOnPre = 1
		lastRunOnPre = 0
		equivalence <- list()
		runLabels <- seq(length = NumberOfRuns, 0,0)
		for(i in 1:NumberOfRuns){
			if(rowRun[i] != curRowIdx){
				curRowIdx = rowRun[i]
				firstRunOnPre = firstRunOnCur
				lastRunOnPre = i - 1
				firstRunOnCur = i
			}
			
			j = firstRunOnPre
			while(j <= lastRunOnPre){
				if((stRun[i] <= enRun[j]) &&
					 (enRun[i] >= stRun[j]) && 
					 rowRun[i] == (rowRun[j] + 1)
					 ){
					if(runLabels[i] == 0) runLabels[i] = runLabels[j]
					else if(runLabels[i] != runLabels[j])
						equivalence[[length(equivalence) + 1]] <-c(runLabels[i],runLabels[j])
				}
				j = j + 1
			}
			if(runLabels[i] == 0){
				runLabels[i] = idxLabel
				idxLabel <- idxLabel + 1
			}
		}
		# end of step 2

		##########
		### Step 3: get equivalent sets
		##########		
		maxLabel = max(runLabels)
		if(maxLabel == 1){
		# no need to connect
			result <- rbind(result, tempOnePerson %>% mutate(Zone = 1))
		} else{
		### 3.1 construct eqTab
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
		### 3.2 construct equaList
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

		### 3.3 find Row,Col - Label relationship			
			zoneLabel <- data.frame(Row = 0, Col = 0, Label = 0)
			for(i in 1:length(rowRun)){
				temp1 <- data.frame(Row = rowRun[i], Col = seq(stRun[i],enRun[i],1), Label = runLabels[i])
				zoneLabel <- rbind(zoneLabel,temp1)
			}
			rm(temp1)
			zoneLabel <- zoneLabel %>% slice(-1)

		### 3.4 find Label - Zone relationship			
			temp <- data.frame(Label = 0, Zone = 0)
			for(i in 1:length(equaList)){
				temp1 <- data.frame(Label = equaList[[i]], Zone = i)
				temp <- rbind(temp,temp1)
			}
			rm(temp1)
			temp <- temp %>% slice(-1)
			zoneLabel <- left_join(zoneLabel, temp) %>% select(-Label)
			
		### 3.5 result for this person	
			tempOnePerson <- left_join(tempOnePerson,zoneLabel)
			result <- rbind(result, tempOnePerson)
			}
}
  return(result)	
}
