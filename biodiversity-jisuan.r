生物多样性计算
m = c(
  "manhattan",
  "euclidean",
  "canberra",
  "bray",
  "kulczynski",
  "jaccard",
  "gower",
  "altGower",
  "morisita",
  "horn",
  "mountford",
  "raup" ,
  "binomial",
  "chao",
  "cao" ,
  "mahalanobis"
)



adonis = sapply(m, function(x) {
  a = adonis(otutable ~ group, method = x)
  return(a$aov.tab$R2[1])
})

anosim = sapply(m, function(x) {
  dd = vegdist(otutable, method = x)
  a = anosim(dd, group)
  return(a$statistic)
})

pcoa = sapply(m[-1], function(x) {
  dis = vegdist(otutable, method = x)
  pcoa = pcoa(dis)
  return(pcoa$values$Relative_eig[1:2])
})

a=cbind(adonis,anosim);a=as.data.frame(a);a$a=rownames(a);
b=t(pcoa);b=as.data.frame(b);b$b=rownames(b)
c=full_join(a,b,by=c("a"="b"))


