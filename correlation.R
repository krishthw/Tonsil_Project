# Data cleaning
rawdata<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Phenotypic_Tonsil_data 06_05_2020.csv")
Chronic<-rawdata[c(1:12),c(2:35)] # select chronic data for correlation analysis
# however this file has column names with paranthesis() included and that cause errors when using them as column names-they take them as functions
# so, let's do it using another file
rawdata<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Phenotypic_data_for_corr_plots.csv")
df<-rawdata[, colSums(rawdata != 0)>0] # select columns with non-zeros
df<-df[-c(1,4,14)]


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

C=C[order(-abs(C$cor)),] # order data according to corretaion value (Descending)
C=na.omit(C) # filter out NA values
C_sig=C[abs(C$cor)>0.5 & abs(C$p)<0.05,]

#write.table(C_sig,file="Functional_correlation_analysis_sig.csv",sep=",")
# corrologram ___________________________________________________________________ 
pdf(file="correlogram_nomenclature_072220.pdf")

library(corrplot)
Corr$r

colnames(Corr$r) <- c("Classical","Intermediate" ,":CD4^'+'", ":CD4^'+'~CD127^'+'",":CD4^'+'~CD8^'+'" , 
                      ":CD4^'-'~CD8^'-'", ":CD8^'+'"  ,":CD8^'+'~CD127^'+'"   ,  ":NKT^'+'"   ,      ":CD4^'+'~CD161^'+'"  ,
                      ":CD8^'+'~CD161^'+'"  , ":CD20^'+'~NKB"     , ":CD20^'+'~CD3^'+'"   , "CD20D"    ,    "CD20DNKB"  ,  
                      ":CD20H"    ,    ":CD20HNKB"  ,   ":CD16^'-'~CD56^'-'"  , ":CD16^'+'"    ,    ":CD56^'+'"  ,     
                      ":NKG2A^'+'"    ,   ":NKp-44^'+'"    ,   ":NKp-44^'-'~NKG2A^'-'", ":CD127P"    ,   ":CD161^'+'"  ,    
                      ":ILC2^'+'"    ,    ":ILC2^'-'"    ,    ":ILC1^'+'"   ,     ":ILC3NCRP"  ,   "ILC3NCRN")
rownames(Corr$r) <- c("Classical","Intermediate" ,":CD4^'+'", ":CD4^'+'~CD127^'+'",":CD4^'+'~CD8^'+'" , 
                      ":CD4^'-'~CD8^'-'", ":CD8^'+'"  ,":CD8^'+'~CD127^'+'"   ,  ":NKT^'+'"   ,      ":CD4^'+'~CD161^'+'"  ,
                      ":CD8^'+'~CD161^'+'"  , ":CD20^'+'~NKB"     , ":CD20^'+'~CD3^'+'"   , "CD20D"    ,    "CD20DNKB"  ,  
                      ":CD20H"    ,    ":CD20HNKB"  ,   ":CD16^'-'~CD56^'-'"  , ":CD16^'+'"    ,    ":CD56^'+'"  ,     
                      ":NKG2A^'+'"    ,   ":NKp-44^'+'"    ,   ":NKp-44^'-'~NKG2A^'-'", ":CD127P"    ,   ":CD161^'+'"  ,    
                      ":ILC2^'+'"    ,    ":ILC2^'-'"    ,    ":ILC1^'+'"   ,     ":ILC3NCRP"  ,   "ILC3NCRN")
correlogram<-corrplot(Corr$r,type="full", order="FPC",tl.cex = 0.5,tl.col ="black",
         p.mat = Corr$P, sig.level = 0.1, insig = "blank",method='square',diag=F,
         title="Correlation combined with the significance test(p<0.05) for Phenotypic data",mar=c(0,0,1,0))
corrRect(c(15,15)) # add rectangles when order is not hclust

dev.off()
rawdata1<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/functional_data_for_corr_plots.csv")
df1<-rawdata1[, colSums(rawdata1 != 0)>0] # select columns with non-zeros
Corr1=Hmisc::rcorr(as.matrix(df1),type='pearson') 
correlogram1<-corrplot(Corr1$r,type="full", order="hclust",addrect=7,tl.cex = 0.3,tl.col ="black",diag = F,
                      p.mat = Corr1$P, sig.level = 0.1, insig = "blank",method='square',
                      title="Correlation combined with the significance test(p<0.05) for Functional data",mar=c(0,0,1,0))

dev.off()


M <- cor(mtcars)[1:5,1:5]
colnames(M) <- c("alpha", "beta", ":alpha+beta", ":a[+]", ":a[beta]")
rownames(M) <- c("alpha", "beta", NA, "$a[0]", "$ a[beta]")
corrplot(M)
