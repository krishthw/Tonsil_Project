#res <- t.test(weight ~ group, data = my_data, var.equal = TRUE)
library(matrixTests)

rawdata <- read.csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/testexcel.csv")

ChronicMat <- rawdata[rawdata[,1]=="Chronic",-1]
ControlMat  <- rawdata[rawdata[,1]=="Control",-1]
result <- col_t_welch(ChronicMat, ControlMat)

library(tibble) # to convert rownames to column
ttest<-rownames_to_column(result, var="Cell") 
ttest<-cbind(ttest[1],ttest[5:7],ttest[12:13])
