大论文柱状图绘图
zz=read.table("clipboard",header=T,sep='\t',stringsAsFactors = F)

z$group=substr(z$U.ID,1,1)
z %>% group_by(group) %>% summarise_if(is.numeric, mean)
zz=z %>% group_by(group) %>% summarise_if(is.numeric, mean)

qq=brewer.pal(12,"Paired")[1:10]
ggplot(pie, aes(x = group, y = value, fill = variable)) + 
  geom_bar(stat ="identity", width = 0.8) + 
  scale_fill_manual("", values = qq) +  
  labs(x = "", y = "", title = "") +  
  theme(axis.ticks = element_blank()) +
  scale_x_discrete(labels = c("C" = "Healthy Controls", "T" = "CFS Patients")) +
  scale_y_continuous(labels = percent(c(0, 0.25, 0.5, 0.75, 1)))

for (i in 1:95) {a = sum(dd[i,2:25]);dd[i,2:25]=dd[i,2:25]/a*100}
result=data.frame(name=colnames(dd),ttest=2,wilcox=2)
for (i in 2:25) {
  a = t.test(dd[, i] ~ dd$group);
  b = wilcox.test(dd[, i] ~ dd$group);
  result$ttest[i] = a$p.value;
  result$wilcox[i] = b$p.value;}
  
  大论文网状图绘图
h=brewer.pal(12, "Paired")
edge <-read.table("clipboard",header=T,sep='\t',stringsAsFactors = F)
node <-read.table("clipboard",header=T,sep='\t',stringsAsFactors = F)
net=graph_from_data_frame(d=edge,directed = F,vertices = node)
aa=fortify(net)
ggplot(aa,aes(x=x,y=y,xend=xend,yend=yend))+
  geom_nodes(aes(fill=phylum,size=size),shape=21)+
  geom_edges(aes(color=correlation),size=1)+
  geom_nodetext_repel(aes(label=vertex.names,color=phylum))+
  theme_blank()+guides(size="none")+
  scale_color_manual(values = c("positive"=h[6],"negative"=h[4],
                                "Firmicutes"=h[2],"Proteobacteria"=h[10],
                                "Bacteroidetes"=h[12],"Fusobacteria"="#00BFC4"))+
  scale_fill_manual(values = c("Firmicutes"=h[2],"Proteobacteria"=h[10],
                               "Bacteroidetes"=h[12],"Fusobacteria"="#00BFC4"))+
  guides(color="none")