library(igraph)

setwd("input")
n=1000
friends=data.frame(id=1:n)
friends$jazz=runif(n)>0.5
friends$classical=runif(n)>0.5
friends$programming=runif(n)>0.5
friends$football=runif(n)>0.9
friends[friends$jazz==F & friends$classical == F & friends$programming==F,"football"]=T
edges=data.frame()
communities=data.frame()
communities=rbind(communities, data.frame(field="jazz",count=as.integer(n/10)+1))
communities=rbind(communities, data.frame(field="classical",count=as.integer(n/20)+1))
communities=rbind(communities, data.frame(field="programming",count=as.integer(n/30)+1))
communities=rbind(communities, data.frame(field="football",count=as.integer(n/25)+1))
for (i in 1:nrow(communities)) {
  field=as.character(communities[i,"field"])
  cnt=communities[i,"count"]
  for (id in friends[friends[,field]==T,"id"]) {
    edges=rbind(edges, data.frame(from_id=id, to_id=sample(friends[friends[,field]==T,"id"], size = cnt)))
  }
}
# exclude self-friends
edges=edges[edges$from_id!=edges$to_id,]
edges=unique(edges)
a=aggregate(to_id ~ from_id, edges, length)
names(a)[2]="friends"
km=kmeans(a$friends,3)
labels=data.frame(category=c("low","medium","high"))
labels$center=order(as.numeric(km$centers),decreasing = F)
labels$mean=as.numeric(km$centers)[order(as.numeric(km$centers),decreasing = F)]
labels$name[order(as.numeric(km$centers),decreasing = T)]
a$center=km$cluster
a=merge(a,labels)
friends=merge(friends,a[,c("from_id","category")],by.x="id",by.y="from_id")
edges=merge(edges,a[,c("from_id","category")])

# plot
png('friends.png')
g=graph.edgelist(as.matrix(edges[,c("from_id","to_id")]))
V(g)$color=friends$category
plot(g)
dev.off()

write.table(x=edges[,c("from_id","to_id")],file = "friends/friends.cites",row.names = F, col.names = F,sep = "\t",quote = F)
for (f in communities$field) {
  friends[,f]=as.integer(friends[,f])
}
write.table(x=friends, file = "friends/friends.content",row.names = F, col.names = F,sep = "\t",quote = F)


