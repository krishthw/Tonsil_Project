library(tidyverse)
rawdata<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Phenotypic_data_for_corr_plots.csv")
selected<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Selected_12_correlations.csv")
df1<-as.data.frame(rawdata) # convert tibble to dataframe

# function to identify outliers
outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
#---------------------------
#outlier eliminated data
df2<-df1[,] #create new dataframe df1
for (i in seq(1:ncol(df1))) {
  df2[,i] = ifelse(outlier(df1[,i]), as.numeric(NA), as.numeric(df1[,i]))
}
# Winsorized data
df3<-df1[,]
df3[]<-lapply(df1, function(x) Winsorize(x, minval = tail(Small(x, k=2), 1), maxval =head(Large(x, k=2),1)))
#winsorize the last column again,
df4<-df3[,]
df4["ILC3NCRN"]<-lapply(df3["ILC3NCRN"], function(x) Winsorize(x, minval = tail(Small(x, k=4), 1), maxval =head(Large(x, k=4),1)))
df4["CD4CD127"]<-lapply(df3["CD4CD127"], function(x) Winsorize(x, minval = tail(Small(x, k=4), 1), maxval =head(Large(x, k=4),1)))
df4["NKP44NNKG2AN"]<-lapply(df3["NKP44NNKG2AN"], function(x) Winsorize(x, minval = tail(Small(x, k=4), 1), maxval =head(Large(x, k=4),1)))

pdf(file="Phenotypic_outlier_winsorized_comparison.pdf")

list1=list()
list2=list()
list4=list()
for (i in seq(1:nrow(selected))){
  colname1 <-as.character(selected[i,1])
  colname2<-as.character(selected[i,2])
  data1<-rawdata %>% dplyr::select (all_of(colname1), all_of(colname2))
  data2<-df2 %>% dplyr::select (all_of(colname1), all_of(colname2))
  data4<-df4 %>% dplyr::select (all_of(colname1), all_of(colname2))
  list1[[i]]<-data1
  list2[[i]]<-data2
  list4[[i]]<-data4

  cplot<-ggpubr::ggscatter(data1, x = colname1, y = colname2,
                           add = "reg.line",conf.int = TRUE,
                           xlab=colname1, ylab=colname2,
                           title="raw")+
    ggpubr::stat_cor(method = "pearson", label.x.npc="left", label.y.npc= "top",r.accuracy=0.01,p.accuracy=0.001) +
    scale_y_continuous(labels = function(x) format(x*100, digits=1, nsmall=1))+
    scale_x_continuous(labels = function(x) format(x*100, digits=1, nsmall=1))
    
  oplot<-ggpubr::ggscatter(data2, x = colname1, y = colname2,
                           add = "reg.line",conf.int = TRUE,
                           title="outliers removed")+
    ggpubr::stat_cor(method = "pearson", label.x.npc="left", label.y.npc= "top",r.accuracy=0.01,p.accuracy=0.001) +
    scale_y_continuous(labels = function(x) format(x*100, digits=1, nsmall=1))+
    scale_x_continuous(labels = function(x) format(x*100, digits=1, nsmall=1))
    
  wplot<-ggpubr::ggscatter(data4, x = colname1, y = colname2,
                           add = "reg.line",conf.int = TRUE,
                           title="winsorized")+
    ggpubr::stat_cor(method = "pearson", label.x.npc="left", label.y.npc= "top",r.accuracy=0.01,p.accuracy=0.001) +
    scale_y_continuous(labels = function(x) format(x*100, digits=1, nsmall=1))+
    scale_x_continuous(labels = function(x) format(x*100, digits=1, nsmall=1))
    grid.arrange(cplot, oplot,wplot, nrow=2,ncol=3)
  
}
dev.off()
data1_all<-dplyr::bind_cols(list1)
write.table(data1_all,file="Phenotypic_selected_12_raw.csv",sep=",")

data2_all<-dplyr::bind_cols(list2)
write.table(data2_all,file="Phenotypic_selected_12_outliersremoved.csv",sep=",")

data4_all<-dplyr::bind_cols(list4)
write.table(data4_all,file="Phenotypic_selected_12_winsorized.csv",sep=",")

data11_all<-as.data.frame(data4_all) # convert tibble to dataframe

df5<-data11_all[,] #create new dataframe df1
for (i in seq(1:ncol(data11_all))) {
  df5[,i] = ifelse(outlier(data11_all[,i]), as.numeric(NA), as.numeric(data11_all[,i]))
}
df5<-df5[!duplicated(lapply(df5, summary))]
boxplot(df5)

df6<-data1_all[!duplicated(lapply(data1_all, summary))]
boxplot(df6)

