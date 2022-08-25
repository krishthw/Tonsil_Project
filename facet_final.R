df <-data.table::fread("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Functional_Tonsil_data 06_05_2020.csv", select = c(1,26,41,46))

library(dplyr)
df.m <- reshape2::melt(df, id.var = "Group")
df.m$Group <-factor(df.m$Group)
left<-"control"
right<- "chronic"
leftpos<-which(levels(df.m$Group)==left)
rightpos<-which(levels(df.m$Group)==right)
df.m2<-df.m%>% 
  dplyr::group_by(variable) %>%
  dplyr::summarise(ypos = max(value)*1.1) %>%
  dplyr::mutate(x = leftpos, xend = rightpos)


#-----------------------------------------------------------------------------------------------------
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

p<-df.m %>%
  group_by(Group) %>% 
  mutate(outlier = ifelse(is_outlier(value), as.numeric(NA), as.numeric(value))) %>% 
  ggplot(aes(x=Group, y=value))+
  facet_wrap(~variable, ncol = 4,strip.position="bottom")+
  #geom_point(size=0.75)+
  
  geom_boxplot(width=0.3,aes(fill=Group),outlier.shape = 21,outlier.size = 0.5)+
  #geom_point(aes(x = Group, y = outlier),size=0.75)+
  geom_point(size=0.5)+
  scale_fill_manual(values=c("firebrick", "royalblue"))+
  scale_x_discrete(limits=c("control", "chronic"))+
  scale_y_continuous(labels = function(x) format(x*100, digits=1, nsmall=1))+
  coord_cartesian(ylim = c(0,0.8))+
  theme_bw()+
  theme(legend.position = "none",
        strip.background = element_blank(),strip.text=element_blank(),
        #strip.text=element_text(family="Times New Roman", size=10,colour="black",face="bold"),
        panel.spacing =unit(0,"line"),
        axis.text.x.bottom =element_blank(),axis.ticks.x =element_blank(),
        axis.text.y = element_text(family="Times New Roman", size=20,colour="black",face="bold"),
        axis.title = element_text(family="Times New Roman", size=26,face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("") + ylab("% Positive Cells ")

p
anno_df = ggpubr::compare_means(value ~ Group, group.by = "variable", data = df.m) 
anno_df1<-anno_df%>%mutate_if(is.numeric, ~round(., 3))

p<-p+geom_segment(data = anno_df1, aes(y = 0.75, yend = 0.75, x = 1, xend = 2)) +
  geom_text(data = anno_df1,size=5,family="Times New Roman",  aes(y = 0.78, x = mean(c(2, 1)), label  =  paste0("p = ", p)))

p


ggsave(p,  filename = "fig6_panal_TNFA_62320.pdf",dpi = 400,  width = 7.5, height = 5, units = "in", device = cairo_pdf)

