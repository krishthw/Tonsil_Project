library(tidyverse)
library(rstatix)   
library(ggpubr)
library(ggplot2)
library(plyr)
require(reshape2)
library(Rmisc)
library(data.table)

df <-fread("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Phenotypic_Tonsil_data 06_05_2020.csv", select = c(1,2))

df.m <- reshape2::melt(df, id.var = "Group")
df.m$Group <-factor(df.m$Group)
left<-"control"
right<- "chronic"
leftpos<-which(levels(df.m$Group)==left)
rightpos<-which(levels(df.m$Group)==right)
df.m2<-df.m%>% group_by(variable) %>%
  summarise(ypos = max(value)*1.1) %>%
  mutate(x = leftpos, xend = rightpos)

p <- ggplot(data = df.m, aes(x=as.factor(Group), y=value)) + 
  geom_boxplot(aes(fill=Group,color=Group), position=position_dodge())+
  theme(axis.text.y= element_blank())+
  geom_point(aes(y=value, group=Group), position = position_dodge(width=0.75))+
  #geom_point(aes(color=value), position=position_jitterdodge()) +# add points
  xlab("") + ylab("%")+
  #facet_wrap( ~ variable, scales = "free", ncol=5)+
  #guides(fill=guide_legend(title=""))+
  scale_x_discrete(limits=c("control", "chronic"))+
  scale_y_continuous(labels=function(x) paste0(x*100))+
  scale_fill_manual(values=c("firebrick", "royalblue"))+
  scale_colour_manual(values=c("black", "black"))+
  theme_pubr(legend="none")
  

p#---------
#------------------------------------------------------------
stat.test <- df.m %>%
  group_by(variable) %>%
  t_test(value ~ Group) %>%
  adjust_pvalue() %>%
  mutate(y.position = 35)
#-------------------------------------------------------------
anno_df<-stat.test %>% mutate(y_pos = 1)
A<-anno_df%>% mutate(significance = ifelse(p<= 0.0001,paste0("****"), ifelse(p <= 0.001,paste0("***"), ifelse(p <= 0.01,paste0("**"),ifelse(p <= 0.05,paste0("*"))))))
#---------

df.m3<-merge(df.m2,anno_df,by="variable")
p<-p+geom_segment(data = df.m3, aes(y = ypos, yend = ypos, x = x, xend = xend)) +
   geom_text(data = df.m3, aes(y = ypos*1.05, x = mean(c(2, 1)), label  =  paste0("p = ", p)))
  



p            


# if you want the end ticks------------ 
p +  geom_segment(data = df2, aes(y = ypos, yend = ypos * .99, x = x, xend = x)) +
  geom_segment(data = df2, aes(y = ypos, yend = ypos *.99, x = xend, xend = xend))
p






p<-p+geom_segment(data = df.m2, aes(y = ypos, yend = ypos, x = x, xend = xend)) +
  geom_text(data = df.m2, aes(y = ypos*1.1, x = mean(c(2, 1)), label = p))


