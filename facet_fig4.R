df <-data.table::fread("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Functional_Tonsil_data 06_05_2020.csv", select = c(1,52,57,62))

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


#--------------------------------------------------------------------

p <- df.m %>% 
  group_by(Group) %>% 
  mutate(outlier = ifelse(is_outlier(value), as.numeric(NA), as.numeric(value))) %>% 
  ggplot(aes(x=Group, y=value))+
  geom_boxplot(width=0.3,aes(fill=Group),outlier.shape = 21,outlier.size = 1)+
  geom_point(aes(x = Group, y = outlier),size=1)+
  scale_fill_manual(values=c("firebrick", "royalblue"))+
  xlab("") + ylab("% Positive Cells ")+
   #theme_bw()+
  #facet_grid(cols = vars(variable))+
  #facet_wrap( ~ variable, scales = "free", ncol=3)+
  #guides(fill=guide_legend(title=""))+
  scale_x_discrete(limits=c("control", "chronic"),labels=c("control" = "Naive","chronic" = "Chronic"))+
  scale_y_continuous(labels = function(x) format(x*100, digits=1, nsmall=1))+
  coord_cartesian(ylim = c(0,1.0))+
  #theme_bw(base_rect_size = 1, base_family = "Times New Roman")+
  theme(legend.position = "none",
        axis.line = element_line(size=1,colour="black"),
        axis.ticks.y = element_line(size=1),axis.ticks.x = element_blank(),
        axis.text = element_text(family="Times New Roman", size=10,colour="black",face="bold"),
        axis.text.y = element_text(family="Times New Roman", size=10,colour="black",face="bold"),
        axis.title = element_text(family="Times New Roman", size=15,face="bold"),
        aspect.ratio = 1,
        panel.spacing = unit(0,"line"))+
        facet_wrap(~variable, ncol = 3)


#,panel.border = element_rect(colour = "black",size=1)
        #panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#p + theme_bw(base_rect_size = 1, base_family = "Times New Roman") + theme(panel.grid.major = element_blank(), 
                                                                          #panel.grid.minor = element_blank(), 
                                                                          #legend.position = "none",)
## unpaired two sample Student t test Note: most of t.test in R gives wilcoxonrank t.test
stat.test <- df.m %>%
  group_by(variable) %>%
  rstatix::t_test(value ~ Group) %>%
  rstatix::adjust_pvalue() %>%
  mutate(y.position = 35)%>%
  mutate_if(is.numeric, ~round(., 3))
p#p + stat_compare_means(label = "p.format")


dff <- tbl_df(df.m)
res <- dff %>% group_by(variable) %>% 
  dplyr::do(w = wilcox.test(value~Group, data=., exact=FALSE)) %>%
  dplyr::summarise(variable, Wilcox = w$p.value)%>%
  mutate(y.position = 35)%>%
  mutate_if(is.numeric, ~round(., 3))


#-------------------------------------------------------------
anno_df<-res %>% mutate(y_pos = 1)
#A<-anno_df%>% mutate(significance = ifelse(p<= 0.0001,paste0("****"), ifelse(p <= 0.001,paste0("***"), ifelse(p <= 0.01,paste0("**"),ifelse(p <= 0.05,paste0("*"))))))
df.m3<-merge(df.m2,anno_df,by="variable")

p<-p+ggsignif::geom_signif(data=df.m3, aes(x=x, xend=xend, annotations=Wilcox, y_position=0.85), 
  manual=TRUE)





df.m3<-merge(df.m2,anno_df,by="variable")
p<-p+geom_segment(data = df.m3, aes(y = 0.85, yend = 0.85, x = x, xend = xend)) +
  geom_text(data = df.m3,size=4,family="Times New Roman",  aes(y = 0.90, x = mean(c(2, 1)), label  =  paste0("p = ", Wilcox)))
p



#ggsave(p,  filename = "fig4_NKG2ANNKP44N_61720.pdf",dpi = 400,  width = 5, height = 5, units = "in", device = cairo_pdf)

anno_df1 = compare_means(value ~ Group, group.by = "variable", data = df.m) 
p
p+ggsignif::geom_signif(
  data=anno_df1, 
  aes(xmin=group1, xmax=group2, annotations=p.format, y_position=0.85), 
  manual=TRUE,textsize = 3)  

p+geom_segment(data = anno_df1, aes(y = 0.85, yend = 0.85, x = 1, xend = 2)) +
  geom_text(data = anno_df1,size=4,family="Times New Roman",  aes(y = 0.95, x = mean(c(2, 1)), label  =  paste0("p = ", p.format)))






