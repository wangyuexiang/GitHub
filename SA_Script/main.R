### load package
library(dplyr)
# library(cluster)
# library(zoo)

### get arguments
args <- commandArgs(trailingOnly = TRUE)
# Command Line example
# cd C:\Users\wangy2\Documents\GitHub\SA_Script
# rscript main.r Input.csv Output.csv 2015-8-31 2015-5-1

InputFileName <- toString(args[1])
OutputFileName <- toString(args[2])

end <- as.Date(args[3])
start <- as.Date(args[4])

### input 
input <- read.table(InputFileName, header = T, sep = ";") %>% tbl_df

### load function
source('DataPreparation.R', encoding = 'UTF-8')

####################
####################


##########
### BDD Ref
##########
# JF = read.table("Ref_JF.csv",sep = ";", header=TRUE)
# gares = read.table("Ref_Gares.csv",sep = ";", header=TRUE)


### output
write.table(output, OutputFileName,sep=";",row.name=FALSE,quote=FALSE)
