set.seed(100)

mydf<-
  data.frame(
    day = rep(1:5, each=20),
    id = rep(LETTERS[1:4],25),
    x = runif(100),
    y = sample(1:2,100,T)
  )
ids = levels(as.factor(mydf$id))
p = vector("list", length(ids))
names(p) = ids

for(i in 1:length(ids)){
  p[[i]] = ggplot(mydf[mydf$id == ids[i],], aes(x,y)) + geom_tile() + ggtitle(paste(ids[i])) + facet_wrap(~day, ncol=1)
}

multiplot(p$A, p$B, p$C, p$D) # in Rmisc package

