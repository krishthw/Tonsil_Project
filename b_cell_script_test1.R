rawdata<-read.csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/olddata/figure_B_cell_subset.csv")
library(tidyverse)
library(rstatix)   
library(ggpubr)
stat.test <- df.m %>%
  group_by(variable) %>%
  t_test(value ~ Group) %>%
  adjust_pvalue() %>%
  mutate(y.position = 35)
stat.test
library(ggplot2)
library(plyr)

require(reshape2)
df <- read.csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/olddata/figure_B_cell_subset.csv", header=T)
# melting by "Label". `melt is from the reshape2 package. 
# do ?melt to see what other things it can do (you will surely need it)
df.m <- melt(df, id.var = "Group")
df.m$Group<- factor(df.m$Group, levels = c("Control","Chronic"))
p <- ggplot(data = df.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=Group))+theme(axis.text.y= element_blank())+ 
  geom_point(aes(y=value, group=Group), position = position_dodge(width=0.75))+ # add points
  coord_flip()+
  xlab("B cell") + ylab("% Cell number")+
  facet_wrap( ~ variable, scales="free",nrow=5)+
  guides(fill=guide_legend(title=""))
p
# ----------------------

# now to add significance and p value

stat.test <- df.m %>%
  group_by(variable) %>%
  t_test(value ~ Group) %>%
  adjust_pvalue() %>%
  mutate(y.position = 20)
stat.test
# this is mutate y position for my plot o/w it takes default y=35
stat.test<-stat.test %>% mutate(x.position =c(0.5))

#______________________________________

stat_pvalue_manual(
  data,
  label = NULL,
  y.position = "y.position",
  xmin = "group1",
  xmax = "group2",
  x = NULL,
  size = 3.88,
  label.size = size,
  bracket.size = 0.3,
  color = "black",
  linetype = 1,
  tip.length = 0.03,
  remove.bracket = FALSE,
  step.increase = 0,
  step.group.by = NULL,
  hide.ns = FALSE,
  vjust = 0,
  position = "identity",
  ...
)
#------------
p + stat_pvalue_manual(stat.test, label = "p")
p
#p<-p + stat_compare_means(method = "t.test",label.y=1.2) +stat_compare_means()

#p<-p+stat_compare_means(label = "p.signif", method = "t.test")
                   #ref.group = ".all.") 


my_comparisons=list( c("Chronic", "Control"))
p+labs(subtitle = get_test_label(stat.test,p.col="p"))
#p<-p+stat_compare_means(comparisons = my_comparisons)+stat_compare_means(label.y = 7, method = "t.test") # Add global p-value
p
#p<-p+stat_compare_means(method = "stat.test") 
p
p<p+ggsignif::geom_signif(comparisons="Group",test = "stat.test", test.args = list(exact = FALSE))
p
