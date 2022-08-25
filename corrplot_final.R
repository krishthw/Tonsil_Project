
df1<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Functional_data_for_corr_plots.csv")
df2<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Selected_135_correlations.csv")

# identify outliers--------------------------------------------------------------------
outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

#---------------------------------------------------------------------------------------
pdf(file="Delete_this_Func_corr_outlier_comparison.pdf")

for (i in seq(1:nrow(df2))){
  colname1 <-as.character(df2[i,1])
  colname2<-as.character(df2[i,2])
  df3<-df1 %>% dplyr::select (all_of(colname1), all_of(colname2))
  
  cplot<-ggpubr::ggscatter(df3, x = colname2, y = colname1,
                   add = "reg.line",conf.int = TRUE,
                   xlab=colname2, ylab=colname1,
                   title=paste0(colname1 ," vs " ,colname2))+
    ggpubr::stat_cor(method = "pearson", label.x.npc=0.25, label.y.npc= 1.0,r.accuracy=0.01,p.accuracy=0.001) +
    scale_y_continuous(labels=function(x) paste0(x*100, "%"))+
    scale_x_continuous(labels = scales::number_format(accuracy = 0.0001))+
    theme(axis.text.x=element_text(size=10),
          axis.title.y=element_text(size=12),
          axis.title.x=element_text(size=12),
          axis.text.y=element_text(size=10))
  #ggsave(p,  filename = paste0(colname1, "vs", colname2, ".png"),dpi = 300, type = "cairo",  width = 4, height = 4, units = "in")
  ##---------------------------------------
  
  # plots without outliers
  df4<-as.data.frame(df3)
  df5 <- df4 %>% 
    mutate(col1 = ifelse(outlier(df4[,1]), as.numeric(NA), as.numeric(df4[,1])))%>%
    mutate(col2 = ifelse(outlier(df4[,2]), as.numeric(NA), as.numeric(df4[,2])))
  oplot<-ggscatter(df5, x = "col2", y = "col1",
                   add = "reg.line",conf.int = TRUE,
                   #add.params = list(color = "black",
                   #fill = "lightgray"),
                   #cor.coef = TRUE, cor.method = "pearson",
                   xlab=colname2, ylab=colname1)+
    stat_cor(method = "pearson", label.x.npc=0.25, label.y.npc= 1.0,r.accuracy=0.01,p.accuracy=0.001) +
    #scale_y_continuous(labels = scales::number_format(accuracy = 0.0001))+
    scale_y_continuous(labels=function(x) paste0(x*100, "%"))+
    scale_x_continuous(labels = scales::number_format(accuracy = 0.0001))+
    theme(axis.text.x=element_text(size=10),
          axis.title.y=element_text(size=12),
          axis.title.x=element_text(size=12),
          axis.text.y=element_text(size=10))
  grid.arrange(cplot, oplot, nrow=2,ncol=2)
  #grid.arrange(cplot,oplot)
}
dev.off()

