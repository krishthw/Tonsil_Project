install.packages("readxl")
library("readxl")
my_data <- read_excel("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/RM Tonsil project data_krish.xlsx",sheet="BT")
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
# input data csv file
install.packages("Hmisc")
library(Hmisc)
Pheno=rcorr(as.matrix(my_data),type='pearson') # apply pearson correlation 
CP=flattenCorrMatrix(Pheno$r, Pheno$P) # extract correlation matrix and p-value matrix
CP=subset(CP, sub("^[^.]*", '', row) != sub("^[^.]*", '', column)) # filter out rows for the same group
CP=CP[order(-abs(CP$cor)),] # order data according to corretaion value (Descending)
CP=na.omit(CP) # filter out NA values
#CP=subset(CP, sub('.*\\..', '', row) != sub('.*\\..', '', column))
Pheno_CP=CP[abs(CP$cor)>0.5 & abs(CP$p)<0.1,]
#write.table(Pheno_CP,file="filter_phenotype_analysis.csv",sep=",")
D=Pheno$r
diag(D)=NA
D
install.packages("corrplot")
library(corrplot)
corrplot(Pheno$r, type="full", order="hclust",addrect=5,tl.cex = 0.7,tl.col = "black",na.label=" ",method="square",
         #p.mat = Pheno$P, sig.level = 0.05, insig = "blank",
         title="Correlation combined with the significance test(p<0.05) for B,T pheno and functional",pos.text = "side")

