
library(edgeR) #Main package for DE analysis
library(tidyverse) #Data wrangling package, includes ggplot2, dplyr, tidy, readr
library(RColorBrewer) #Colour scheme for plotting
library(Glimma) #Interactive MD plots
library(gplots)
download.file(url = "https://ndownloader.figshare.com/files/23241812",destfile = "gene_counts.txt")
raw_counts <- read.table(file="gene_counts.txt", 
                         sep = "\t", 
                         header = TRUE)              
head(raw_counts)
names(raw_counts)[7:18] <- c("IT2", "BC2", "IC3", "IT1", "BC3", "BT1", "BC1", "IT3", "IC1", "IC2", "BT2", "BT3")              
raw_counts[1:6, 7:18]              
raw_counts <- raw_counts[ , c(1:6, 9,15,16,7,10,14,8,11,13,12,17,18)]
head(raw_counts[,7:18])              
dge <- DGEList(counts = raw_counts[ , 7:18],
               lib.size = colSums(raw_counts[ , 7:18]),
               norm.factors = rep(1,ncol(raw_counts[ , 7:18])),
               samples = NULL,
               group = NULL,
               genes = raw_counts[ , 1:6])              
group<-as.factor(rep(c("C","T","C","T"), c(3,3,3,3)))
dge$samples$group<-group              
group              
location<-as.factor(rep(c("inland","beach"),c(6,6)))
dge$samples$location<-location
location
dge$samples
dge_orig<-dge
saveRDS(dge_orig, file = "dge_orig.rds")
# 3-------------
#raw counts are converted to CPM and log-CPM values using the cpm function
cpm <- cpm(dge) 
lcpm <- cpm(dge, log=TRUE)

L <- mean(dge$samples$lib.size) * 1e-6 #average library size in Millions
M <- median(dge$samples$lib.size) * 1e-6 #median lib size 

#summary(lcpm)
summary(cpm)[,1:3]
dim(dge) #retrieve dimentions of the dge object

keep.exprs <- filterByExpr(dge)
#Minimum read count can also be explicitly specified, for instance '5' bellow: 
#keep.exprs <- filterByExpr(dge, min.count = 5, min.total.count = 5)

dge <- dge[keep.exprs,, keep.lib.sizes=FALSE]
dim(dge)
lcpm.cutoff <- log2(10/M + 2/L)
nsamples <- ncol(dge)
col <- brewer.pal(nsamples, "Paired")
par(mfrow=c(1,2))
plot(density(lcpm[,1]), col=col[1], lwd=2, ylim=c(0,0.26), las=2, main="", xlab="") #density with lcpm from unfiltered data
title(main="A. Raw data", xlab="Log-cpm")
abline(v=lcpm.cutoff, lty=3)
for (i in 2:nsamples){
  den <- density(lcpm[,i])
  lines(den$x, den$y, col=col[i], lwd=2)
}
#legend("topright", samplenames, text.col=col, bty="n")
lcpm2 <- cpm(dge, log=TRUE) #NOTE: recalculating lcpm from filtered data!
plot(density(lcpm2[,1]), col=col[1], lwd=2, ylim=c(0,0.26), las=2, main="", xlab="")
title(main="B. Filtered data", xlab="Log-cpm")
abline(v=lcpm.cutoff, lty=3)
for (i in 2:nsamples){
  den <- density(lcpm2[,i])
  lines(den$x, den$y, col=col[i], lwd=2)
}
#5-------
par(mfrow=c(1,1))
samplenames<-c("Inland_c3","Inland_c1","Inland_c2","Inland_t2","Inland_t1","Inland_t3",
               "Beach_c2","Beach_c3","Beach_c1","Beach_t1","Beach_t2","Beach_t3")
#Lib sizes:
barplot(dge$samples$lib.size, las = 2, names.arg = samplenames)
dge_unNorm<-dge 
dge <- calcNormFactors(dge, method = "TMM")
dge$samples
par(mfrow=c(1,2)) #create two panels for plotting

lcpm <- cpm(dge_unNorm, log=TRUE)
boxplot(lcpm, las=2, col=col, main="")
title(main="Unnormalized data",ylab="Log-cpm")

lcpm <- cpm(dge, log=TRUE)
boxplot(lcpm, las=2, col=col, main="")
title(main="Normalized data",ylab="Log-cpm")

lcpm <- cpm(dge, log=TRUE)

#assign different colors to the group information
col.group <- dge$samples$group
levels(col.group) <- brewer.pal(nlevels(col.group), "Set1")

col.group <- as.character(col.group)

par(mfrow=c(1,1))
plotMDS(lcpm, col=col.group)
