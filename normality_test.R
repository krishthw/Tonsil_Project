phenodata <- read.csv("/Users/krishanthi/Documents/Liyanage_lab/Liyanage_data/Tonsil Project/Data/Phenotypic_groups.csv")
#Shapiro-Wilk normality test
lshap<- lapply(phenodata[2:(ncol(phenodata))], shapiro.test)
lres <- t(sapply(lshap, `[`, c("statistic","p.value")))

lres <-as.data.frame(lres)

library(tibble) # to convert rownames to column
lres<-rownames_to_column(lres, var="Cell") 
lres<-cbind(lres[1],lres[3])
lres <- apply(lres,2,as.character) # when it doesn't let you write to csv.

write.csv(lres,'Normality_check_phenotypic.csv')

