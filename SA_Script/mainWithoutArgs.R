### load package
library(dplyr)
# library(cluster)
# library(zoo)

InputFileName <- "Input.csv"
OutputFileName <- "Output.csv"

end <- as.Date("2015-8-31")
start <- as.Date("2015-5-1")

### input 
input <- read.table(InputFileName, header = T, sep = ";") %>% tbl_df

### load function
source('DataPreparation.R', encoding = 'UTF-8')

####################
####################




### output
write.table(output, OutputFileName,sep=";",row.name=FALSE,quote=FALSE)
