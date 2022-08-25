df <- read.csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/figure_B_cell_subset.csv", header=T)
# melting by "Label". `melt is from the reshape2 package. 
# do ?melt to see what other things it can do (you will surely need it)
library(ggplot2)
library(plyr)
require(reshape2)
df.m <- melt(df, id.var = "Group")



p1 <- ggboxplot(df.m, x = "Group", y = "value",
               palette = "npg",
               add = "jitter",
               facet.by = "variable") 
p1 <- p1 + facet_wrap( ~ variable, scales="free",ncol=5)
p1 <- p1 + xlab("expression") + ylab("Percentage") + ggtitle("B-cell subset")
p1 <- p1 + guides(fill=guide_legend(title="treatment"))
p1
p1<-p1+geom_boxplot(aes(fill=Group))
p1 <- p1 + theme_gray() # background grey
#ggpar(p, palette = "Dark2" ) # change color
p1# Use only p.format as label. Remove method name.
library(tidyverse)
library(rstatix)   
library(ggpubr)
stat.test <- df.m %>%
  group_by(variable) %>%
  t_test(value ~ Group) %>%
  adjust_pvalue() %>%
  mutate(y.position = 35)
stat.test

p1<-p1 + stat_compare_means(method = "t.test",label.y=1.2) +stat_compare_means()
p1<-p1 + theme(legend.position = "right")
p1
