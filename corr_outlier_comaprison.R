library(tidyverse)
rawdata<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Phenotypic_data_for_corr_plots.csv")
#rawdata<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Phenotypic_data_for_corr_plots.csv")
Chronic<-rawdata
# select chronic data for correlation analysis
#Chronic<-rawdata[c(1:12),c(2:86)] # Functional panel
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
write.table(C2_sig,file="Functional_corr_outliers_test.csv",sep=",") 

df2<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Selected_12_correlations.csv")
list<-list()
for (i in seq(1:nrow(df2))){
  variable1 <-as.character(df2[i,1])
  variable2<-as.character(df2[i,2])
  print(i)
  df3<-C2_sig %>% dplyr::filter((row == variable1 & column == variable2) |  (row == variable2 & column==variable1))
  list[[i]]<-df3
}

df4 = do.call(rbind, list)
write.table(df4,file="Phenotypic_corr_outliers_selected12.csv",sep=",")
#-------------------------
#-------------------------
#-------------------------
#Outliers vs ex_outliers investigation and elimination 
rawdata<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Phenotypic_data_for_corr_plots.csv")
df<-rawdata[, colSums(rawdata != 0)>0]
df1<-as.data.frame(df) # convert tibble to dataframe
# function to identify outliers
outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
# for loop to remove outliers in df1
df2<-df1[,] #create new dataframe df1
for (i in seq(1:ncol(df1))) {
  df2[,i] = ifelse(outlier(df1[,i]), as.numeric(NA), as.numeric(df1[,i]))
}
write.table(df2,file="Phenotypic_data_corr_outliers.csv",sep=",")

#function to identify extrme outliers
exoutlier <- function(x) {
  return(x < quantile(x, 0.25) - 3 * IQR(x) | x > quantile(x, 0.75) + 3 * IQR(x))
}
# for loop to remove outliers in df1
df3<-df1[,] #create new empty dataframe as same cols and rows as in df1
for (i in seq(1:ncol(df1))) {
  df3[,i] = ifelse(exoutlier(df1[,i]), as.numeric(NA), as.numeric(df1[,i]))
}
write.table(df3,file="Phenotypic_data_corr_extreme_outliers.csv",sep=",")
#------------
d1<-df1[c(13,25)]
d2<-df2[c(13,25)]
d3<-df3[c(13,25)]
d1plot<-ggpubr::ggscatter(d1, x = "CD8PCD161P", y = "NKP44P",size=1.6,
                         add = "reg.line",conf.int = TRUE)+
  ggpubr::stat_cor(method = "pearson", label.x.npc="left", label.y.npc="top",vjust=1,r.accuracy=0.01,p.accuracy=0.001,family="Times New Roman",size=6,) 
  
d1plot

d2plot<-ggpubr::ggscatter(d2, x = "CD8PCD161P", y = "NKP44P",size=1.6,
                          add = "reg.line",conf.int = TRUE)+
  ggpubr::stat_cor(method = "pearson", label.x.npc="left", label.y.npc="top",vjust=1,r.accuracy=0.01,p.accuracy=0.001,family="Times New Roman",size=6,) 
d2plot
d3plot<-ggpubr::ggscatter(d3, x = "CD8PCD161P", y = "NKP44P",size=1.6,
                          add = "reg.line",conf.int = TRUE)+
  ggpubr::stat_cor(method = "pearson", label.x.npc="left", label.y.npc="top",vjust=1,r.accuracy=0.01,p.accuracy=0.001,family="Times New Roman",size=6,) 
d3plot
# Winsorization.................
df4<-df1[,]
df4[]<-lapply(df1, function(x) Winsorize(x, minval = NULL, maxval = NULL, probs = c(0.05, 0.95), na.rm = FALSE))
df4
write.table(df4,file="Phenotypic_data_winsorize_rawdata.csv",sep=",")

df5<-df4[,] #create new empty dataframe as same cols and rows as in df1
for (i in seq(1:ncol(df4))) {
  df5[,i] = ifelse(exoutlier(df4[,i]), as.numeric(NA), as.numeric(df4[,i]))
}
df5
write.table(df5,file="Phenotypic_data_corr_extreme_outliers_for_winsorize_data.csv",sep=",")

d4<-df4[c(13,25)]
d4plot<-ggpubr::ggscatter(d4, x = "CD8PCD161P", y = "NKP44P",size=1.6,
                          add = "reg.line",conf.int = TRUE)+
  ggpubr::stat_cor(method = "pearson", label.x.npc="left", label.y.npc="top",vjust=1,r.accuracy=0.01,p.accuracy=0.001,family="Times New Roman",size=6,) 

d4plot

df6<-df1[,]
df6
df6[]<-lapply(df1, function(x) Winsorize(x, minval = tail(Small(x, k=2), 1), maxval =head(Large(x, k=2),1)))
df6 

df7<-df6[,] #create new empty dataframe as same cols and rows as in df1
for (i in seq(1:ncol(df6))) {
  df7[,i] = ifelse(outlier(df6[,i]), as.numeric(NA), as.numeric(df6[,i]))
}
df7

df8<-df6[,]
df8["ILC3NCRN"]<-lapply(df6["ILC3NCRN"], function(x) Winsorize(x, minval = tail(Small(x, k=4), 1), maxval =head(Large(x, k=4),1)))
