install.packages("matrixTests")
library(matrixTests)

rawdata <- read.csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Phenotypic_groups.csv")

ChronicMat <- rawdata[rawdata[,1]=="Chronic",-1]
ControlMat  <- rawdata[rawdata[,1]=="Control",-1]

p.values<-c()
for (i in seq(1:ncol(mMat))){
  results<-var.test(mMat[,i], yMat[,i])
  p.values<- c(p.values,results$p.value)
}

names(p.values)<-names(mMat)
Ftest <-as.data.frame( p.values)
Ftest<-rownames_to_column(Ftest, var="Cell") 

Ftest <- apply(Ftest,2,as.character) # when it doesn't let you write to csv.

write.csv(Ftest,'Variance_check_phenotypic.csv')

