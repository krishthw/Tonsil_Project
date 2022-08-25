


library(tidyverse)
rawdata<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Functional_Tonsil_data 06_05_2020.csv")

# select chronic data for correlation analysis
Chronic<-rawdata[c(1:12),c(2:86)] # Functional panel
#Chronic<-rawdata[c(1:12),c(3:5,7:15,17:35)] # pheno panel
df<-Chronic[, colSums(Chronic != 0)>0] # select columns with non-zeros
#-----------------------------------------------------------------------
# correlation analysis
Corr=Hmisc::rcorr(as.matrix(df),type='pearson') # apply pearson correlation 

#------------------------------------------------------------------------
# function to extract corr value and p-value only the upper triangle
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
#---------------------------------------------------------------------------------
C=flattenCorrMatrix(Corr$r, Corr$P) # extract correlation matrix and p-value matrix
C=na.omit(C) # filter out NA values
#---------------------------------------------------------------------------------
outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
#-------------------------------------------------------------------------
df1<-as.data.frame(df) # convert tibble to dataframe
#-------------------------------------------------------------------------
# for loop to remove outliers in df1
for (i in seq(1:ncol(df1))) {
  df1[,i] = ifelse(outlier(df1[,i]), as.numeric(NA), as.numeric(df1[,i]))
}
# correlation test for df1 which doesnt contain outliers
Corr1=Hmisc::rcorr(as.matrix(df1),type='pearson')
C1=flattenCorrMatrix(Corr1$r, Corr1$P) # extract correlation matrix and p-value matrix
C1=na.omit(C1) # filter out NA values
C<-C%>%rename(cor1=cor,p1=p)
C1<-C1%>%rename(cor2=cor,p2=p)
# merge results from df and df1
C2<-merge.data.frame(C,C1)
C2=C2[order(-abs(C2$cor1)),]
C2_sig=C2[abs(C2$cor1)>0.5 & abs(C2$p1)<0.05,]
write.table(C2_sig,file="Functional_corr_outliers.csv",sep=",") 

df2<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Selected_135_correlations.csv")
list<-list()
for (i in seq(1:nrow(df2))){
  variable1 <-as.character(df2[i,1])
  variable2<-as.character(df2[i,2])
  print(i)
  df3<-C2_sig %>% dplyr::filter((row == variable1 & column == variable2) |  (row == variable2 & column==variable1))
  list[[i]]<-df3
}

df4 = do.call(rbind, list)
write.table(df4,file="Functional_corr_outliers_selected135.csv",sep=",")
