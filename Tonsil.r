df <-data.table::fread("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data_namal/Tonsil_PHY_data_scientific.csv")
df[df < 0] <- 0 # set all negative values to zero

df[is.na(df)] = 0 # set all NA values to zero

#write.csv(df,"/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data_namal/tonsil_.csv") 

ttest<-c() # create empty vector
wilcox<-c()
for (i in seq(1:(ncol(df)-1))){      # 
  df2<-df %>% dplyr::select(c(1, i+1))
  a<-colnames(df2)
  colname1 <-as.character(a[1])
  colname2 <-as.character(a[2])
  TT<-t.test(get(colname2) ~ get(colname1), data = df2)
  WIL<-wilcox.test(get(colname2) ~ get(colname1), data = df2)
  ttest<-c(ttest,TT$"p.value")
  wilcox<-c(wilcox,WIL$"p.value")
}
names(ttest)<-names(df[,2:36])
ttestresults<-data.frame(ttest) # convert it to a data-frame
ttestresults<-tibble::rownames_to_column(ttestresults,var="Marker")
tsig<-ttestresults %>% filter(ttest<0.05)
write.csv(ttestresults,'ttest_tonsil_phy.csv')


names(wilcox)<-names(df[,2:36])
wilcoxresults<-data.frame(wilcox) # convert it to a data-frame
wilcoxresults<-tibble::rownames_to_column(wilcoxresults,var="Marker")
wsig<-wilcoxresults %>% filter(wilcox<0.05)
write.csv(wilcoxresults,'wilcox_tonsil_phy.csv')

joinsig<-inner_join(tsig, wsig)

dfsig<-df %>% select(one_of(dput(as.character(wsig$Marker)))) # select columns match with rows of anovasig dataframe
dfsig<-cbind(df[,1],dfsig)


#pdf(file="Tonsilfun_wilcox_sig.pdf")


for (i in seq(1:(ncol(df)-1))) {
  df1<-df %>% dplyr::select(c(1, i+1))
  a <- colnames(df1)
  wilcox2 <- wilcox.test(as.formula(paste(a[2], a[1], sep="~")),df1)
  print(i)
  print(wilcox2)
  colname1 <-as.character(a[1])
  colname2 <-as.character(a[2])
  box<-ggplot(df1, aes(x = get(colname1), y = get(colname2 ))) +
    theme(panel.background = element_rect(fill="white"),
          axis.line = element_line(size=1,colour="black"),
          axis.text.x = element_text(size=10,colour="black",face="bold"),
          axis.text.y = element_text(size=10,colour="black",face="bold")) +
    geom_boxplot(outlier.shape = NA)+
    #stat_compare_means()+
    stat_compare_means(comparisons = list(c("control", "CHRONIC")),method="wilcox.test",label="p.signif",label.x.npc="center",tip.length=0.0)+
    scale_x_discrete(limits=c("control", "CHRONIC"),labels=c("control" = "Naive","CHRONIC" = "Chronic"))+
    
    geom_jitter(position=position_jitter(width=.30, height=0), aes(colour=get(colname1),fill=get(colname1)),shape=21,size = 7,color="black")+
    scale_fill_manual(limits=c("control", "CHRONIC"),values = c("cornflowerblue","red2"))+
    xlab("") + ylab("")+  theme(legend.position = "none")
  print(box)
  ggsave(box,  filename = paste0("TonsilPhyall/", colname2, ".png"),dpi = 300, type = "cairo",  width = 4, height = 6, units = "in")
  
  #ggsave(box,  filename = paste0("Tonsilfun/TonsilFunall/", colname2, ".png"),dpi = 300, type = "cairo",  width = 4, height = 6, units = "in")
}
dev.off()

