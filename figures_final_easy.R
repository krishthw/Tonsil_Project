df <-data.table::fread("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Phenotypic_Tonsil_data 06_05_2020.csv", select = c(1,15))

library(dplyr)
df.m <- reshape2::melt(df, id.var = "Group")
df.m$Group <-factor(df.m$Group)
left<-"control"
right<- "chronic"
leftpos<-which(levels(df.m$Group)==left)
rightpos<-which(levels(df.m$Group)==right)
df.m2<-df.m%>% 
  dplyr::group_by(variable) %>%
  dplyr::summarise(ypos = max(value)*1.25) %>%
  dplyr::mutate(x = leftpos, xend = rightpos)


#-----------------------------------------------------------------------------------------------------
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
df.m <- data.table::data.table(df.m)

df.m[,y_min := value*0.4, by = variable]
df.m[,y_max:= value*1.1, by = variable]

p<-df.m %>%
  group_by(Group) %>% 
  mutate(outlier = ifelse(is_outlier(value), as.numeric(NA), as.numeric(value))) %>% 
  ggplot2::ggplot(aes(x=Group, y=value))+
  geom_boxplot(width=0.3,aes(fill=Group),outlier.shape = 21,outlier.size = 0.5)+
  geom_point(size=0.5)+
  scale_fill_manual(values=c("firebrick", "royalblue"))+
  scale_x_discrete(limits=c("control", "chronic"),labels=c("control" = "Naive","chronic" = "Chronic"))+
  scale_y_continuous(labels = function(x) format(x*100, digits=1, nsmall=1))+
  geom_blank(aes(y = y_max*1.1))+
  #coord_cartesian(ylim = c(0,y_max))
  theme(legend.position = "none",
        axis.line = element_line(size=1,colour="black"),
        axis.ticks.y = element_line(size=1),axis.ticks.x = element_blank(),
        axis.text = element_text(family="Times New Roman", size=20,colour="black",face="bold"),
        axis.text.y = element_text(family="Times New Roman", size=20,colour="black",face="bold"),
        axis.title = element_text(family="Times New Roman", size=26,face="bold"),
        panel.background = element_rect(fill="white"),
        aspect.ratio = 1)+
  xlab("") + ylab("% Positive Cells ")
p
anno_df = ggpubr::compare_means(value ~ Group, group.by = "variable", data = df.m) 

anno_df1<-anno_df%>%mutate_if(is.numeric, ~round(., 3))
anno_df2<-merge(anno_df1,df.m2,by="variable")
p<-p+geom_segment(data = anno_df2, aes(y = ypos*0.90, yend = ypos*0.90, x = 1, xend = 2)) +
  geom_text(data = anno_df2,size=5,family="Times New Roman",  aes(y = ypos*0.95, x = mean(c(2, 1)), label  =  paste0("p = ", p)))

p

ggsave(p,  filename = "fig_CD8PCD161P_071120.pdf",dpi = 400,  width = 5, height = 5, units = "in", device = cairo_pdf)

