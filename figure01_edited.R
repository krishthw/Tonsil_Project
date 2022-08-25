library(tidyverse)

#library(rstatix)   
#library(ggpubr)
library(ggplot2)
#library(plyr)
#require(reshape2)
#library(Rmisc)
#library(data.table)
library(extrafont)
#font()

df <-data.table::fread("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Functional_Tonsil_data 06_05_2020.csv", select = c(1,47))


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
#--------------------------------------------------------------------
p <- ggplot(data = df.m, aes(x=as.factor(Group), y=value)) + 
  geom_boxplot(width=0.3,aes(fill=Group,color=Group), position=position_dodge(),
               outlier.colour = NULL,
               outlier.shape = 20,
               outlier.fill = "red",
               outlier.size = 2)+
  #geom_point(aes(color =as.factor(Group)))+
  #geom_point(aes(y=value, group=Group,color =as.factor(Group)), position = position_dodge(width=1),size=0.75)+
  #geom_point(aes(color=value), position=position_jitterdodge()) +# add points
  xlab("") + ylab("% Positive Cells ")+
  #facet_wrap( ~ variable, scales = "free", ncol=5)+
  #guides(fill=guide_legend(title=""))+
  scale_x_discrete(limits=c("control", "chronic"),labels=c("control" = "Naive","chronic" = "Chronic"))+
  scale_y_continuous(labels = function(x) format(x*100, digits=1, nsmall=1))+
  scale_fill_manual(values=c("firebrick", "royalblue"))+
  scale_colour_manual(values=c("black", "black"))+
  geom_point(aes(color =as.factor(Group)))+
  theme(legend.position = "none",
        axis.line = element_line(size=1,colour="black"),
        axis.ticks.y = element_line(size=1),axis.ticks.x = element_blank(),
        axis.text = element_text(family="Times New Roman", size=20,colour="black",face="bold"),
        axis.text.y = element_text(family="Times New Roman", size=20,colour="black",face="bold"),
        axis.title = element_text(family="Times New Roman", size=26,face="bold"),
        panel.background = element_rect(fill="white"),
        aspect.ratio = 1)
p 
p$out
#------------------------------------------------------------
# unpaired two sample Student t test Note: most of t.test in R gives wilcoxonrank t.test
stat.test <- df.m %>%
  group_by(variable) %>%
  rstatix::t_test(value ~ Group) %>%
  rstatix::adjust_pvalue() %>%
  mutate(y.position = 35)
#-------------------------------------------------------------
anno_df<-stat.test %>% mutate(y_pos = 1)
#A<-anno_df%>% mutate(significance = ifelse(p<= 0.0001,paste0("****"), ifelse(p <= 0.001,paste0("***"), ifelse(p <= 0.01,paste0("**"),ifelse(p <= 0.05,paste0("*"))))))

df.m3<-merge(df.m2,anno_df,by="variable")
p<-p+geom_segment(data = df.m3, aes(y = ypos, yend = ypos, x = x, xend = xend)) +
   geom_text(data = df.m3,size=4,family="Times New Roman",  aes(y = ypos*1.05, x = mean(c(2, 1)), label  =  paste0("p = ", p)))
p




ggsave(p,  filename = "fig3_CD20DNKBCD107A_corr.pdf",dpi = 400,  width = 5, height = 5, units = "in", device = cairo_pdf)










# if you want the end ticks------------ 
p +  geom_segment(data = df2, aes(y = ypos, yend = ypos * .99, x = x, xend = x)) +
  geom_segment(data = df2, aes(y = ypos, yend = ypos *.99, x = xend, xend = xend))
p





p<-p+geom_segment(data = df.m2, aes(y = ypos, yend = ypos, x = x, xend = xend)) +
  geom_text(data = df.m2, aes(y = ypos*1.1, x = mean(c(2, 1)), label = p))


