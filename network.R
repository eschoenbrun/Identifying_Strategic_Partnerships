install.packages('igraph')
library(igraph)

#load the data
x <- read.csv('C:\\Users\\Stevens\\Desktop\\BIA_658\\Project\\user_business_reduced.csv')

#set up edge list
x3 <- x2 <- x
names(x2) <- c('user_id', 'business1')                                                              
names(x3) <- c('user_id', 'business2')   
merged_data <- merge(x2, x3)  

#business 1 and business 2 online
merged_data <-  merged_data[, 2:3]

#only unique records
test <- merged_data$business1 == merged_data$business2  
merged_data <- merged_data[!test, ] 
a <-unique(merged_data[c("business1","business2")])

#subset 4000 records
b <- a[5000:9000,]

#second unique process now that the data is subset
indx <- !duplicated(t(apply(b, 1, sort)))
b <- b[indx,]

#create the igraph
g2 <- igraph::graph.data.frame(b[, c('business1', 'business2')], directed=F)
g3 <- as.undirected(g2, mode='collapse')
g4 <- delete.vertices(g3, V(g3)[degree(g3)==0])

#colors or sizes of nodes by degree
V(g4)$vertex_degree <-  degree(g4)
nodesize <- degree(g4)

#color of nodes by membership
nodecolor <- membership(eb)

#different measures of modularity - edge betweenness usually is best
wc <- walktrap.community(g4)
modularity(wc)
eb <- edge.betweenness.community(g4, directed=F)
modularity(eb)
fg <- fastgreedy.community(g4)
modularity(fg)
lp <- label.propagation.community(g4)
modularity(lp)


# read this page about kcores
coreness <- graph.coreness(g4)
max_cor <- max(coreness)
color_bar <- heat.colors(max_cor) 
plot(g4, vertex.label=NA, vertex.color = color_bar[coreness], vertex.size = 5)

# plotting with communities
plot(eb, g4, vertex.size = 5, vertex.color = nodecolor, vertex.label = NA, layout=layout_with_drl(g4))

# only plot the communities as nodes
g5 <- simplify(contract(g4, membership(eb))) 
plot(g5, vertex.label = NA)

# if we just want two colors based on one large community and whatever is not included in that
V(g4)$color <- ifelse(membership(eb)==1,"red","blue")
plot(g4, vertex.size = 5, vertex.label = NA, layout=layout.fruchterman.reingold)

#edge bundling - the awesome cool plot: https://cran.r-project.org/web/packages/edgebundleR/edgebundleR.pdf
require(edgebundleR)
edgebundle(g4,tension = 0.1,fontsize = 18,padding=40)

#determining modularity based on records
mods <- sapply(0:ecount(g4), function(i){
  g6 <- delete.edges(g4, eb$removed.edges[seq(length=i)])
  cl <- clusters(g6)$membership
  modularity(g4,cl)
})
plot(mods, pch=20)

#reducing edges and coloring nodes based on clusters
g7<-delete.edges(g4, eb$removed.edges[seq(length=which.max(mods)-1)])
V(g4)$color=clusters(g7)$membership
plot(g4, vertex.label=NA, layout=layout.fruchterman.reingold, vertex.size = 10, vertex.color = c$cluster)

# kmeans- need to add in attribute data: https://cran.r-project.org/web/packages/tclust/vignettes/tclust.pdf
c <- tkmeans(b, k = 10, alpha = 0.1)
plot(c)
col = c$cluster
ctl <-   ctlcurves(b, k = 1:10)
plot.ctlcurves(ctl)


# look into: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html
# look into: https://cran.rstudio.com/web/packages/ggraph/vignettes/Edges.html
# look into: https://cran.rstudio.com/web/packages/ggraph/vignettes/Layouts.html
# look into: https://cran.rstudio.com/web/packages/ggraph/vignettes/Nodes.html
# look into: https://www.slideshare.net/OReillyStrata/visualizing-networks-beyond-the-hairball/59-A_D3_Example_by_M
