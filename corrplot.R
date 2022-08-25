library(tidyverse)
df1<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Phenotypic_data_for_corr_plots.csv")
df2<-read_csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Selected_12_correlations.csv")

library(ggpubr)

df2%>%row


cplot<-ggscatter(data, x = "CD16P", y = "NKP44P", 
                 add = "reg.line", #conf.int = TRUE, 
                 #add.params = list(color = "black",
                                   #fill = "lightgray"),
                 #cor.coef = TRUE, cor.method = "pearson",
                 xlab="", ylab="") 

p<-cplot+ stat_cor(method = "pearson", label.x.npc=0.25, label.y.npc= 1.0,r.accuracy=0.01,p.accuracy=0.001) + 
  #scale_y_continuous(labels = scales::number_format(accuracy = 0.0001))+
  scale_y_continuous(labels=function(x) paste0(x*100, "%"))+
  scale_x_continuous(labels = scales::number_format(accuracy = 0.0001))+
  theme(axis.text.x=element_text(size=10),
        axis.title.y=element_text(size=12),
        axis.title.x=element_text(size=12),
        axis.text.y=element_text(size=10))
  #labs(x=expression(NKG2A^{"+"}*'IL22'),
       #y=expression(ILC3LCR^{"+"}*IL22))
p
ggsave(p,  filename = "fig26.png",dpi = 300, type = "cairo",  width = 4, height = 4, units = "in")



