rawdata<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Phenotypic_data_for_corr_plots.csv")
df<-rawdata[c(1:12),c(22,14)] # select chronic data for correlation analysis

cplot<-ggpubr::ggscatter(df, x = "CD8PCD161P", y = "CD16NCD56N",size=1.6,
                 add = "reg.line",conf.int = TRUE)+
                 #add.params = list(color = "black",size=1))+
  ggpubr::stat_cor(method = "pearson", label.x.=0, label.y=0.05,vjust=1,r.accuracy=0.01,p.accuracy=0.001,family="Times New Roman",size=6,) +
  scale_y_continuous(labels = function(x) format(x*100, digits=1, nsmall=1))+
  scale_x_continuous(labels = function(x) format(x*100, digits=1, nsmall=1),limits=c(min(df["CD8PCD161P"])-0.001,max(df["CD8PCD161P"])+0.001),breaks=seq(min(df["CD8PCD161P"])-0.001,max(df["CD8PCD161P"])+0.001, by = 0.02))+
  theme(axis.line = element_line(size=1,colour="black"),
        axis.text.x = element_text(family="Times New Roman", size=20,colour="black",face="bold"),
        axis.text.y = element_text(family="Times New Roman", size=20,colour="black",face="bold"),
        axis.ticks.y = element_line(size=1),axis.ticks.x = element_line(size=1))+
  xlab("") + ylab("")

cplot  

ggsave(cplot,  filename = "corr_CD16NCD56NvsCD8PCD161P_071420.pdf",dpi = 400,  width = 5, height = 5, units = "in", device = cairo_pdf)

