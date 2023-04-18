# Author: Fabio Morea @ Area Science Park
# Acknowledgments: this research work is supervided by prof. Domenico De Stefano, within the frame of PhD in Applied Data Science and Artificial Intelligence @ University of Trieste

# Package: test consensus community detection algorithm
# Description: R program to extract information from labour market data.
# Original data is not included in the package as it contains personal information
# Test data is contains no personal information

# SPDX-License-Identifier: CC-BY-4.0

# Github: https://github.com/fabio-morea/benchmark_net

 
## clear terminal
shell("cls")

## debug mode
echo <- T
debug <- F
if (debug){print("Debug mode")}


## load libraries
suppressPackageStartupMessages(library(tidyverse))
library(igraph)
library(glue)
library(ggpubr)
library(RColorBrewer)

source("./code/functions-network-analysis.R")

## load graph
print("Loading graph...")
g <- read_graph("./FLR_benchmark_5.glm", format="graphml")
print(g)
#g <- induced.subgraph(g, V(g)[ V(g)$CL0 == 1]) 
stopifnot(gorder(g) >0)
n_trials = 100
if (debug){n_trials <- 50}

# undirected graph to be used for algorithms that do not support directed
gu <- as.undirected(g,mode = "each")

if (echo) print(paste("repeat clustering ", n_trials, "times ..."))
## CONSENSUS
# resolution is a relevant parameter to define the size of clusters
# alpha is used to induce a variability in the consensus procedure

res=c( 1.0, 1.5 , 2.0 ) 
alpha = 5/100

all_clusters <- cluster_N_times(g=gu, 
	res=res,
	n_trials=n_trials, 
	alpha = alpha,
	clustering_algorithm="Louvian", 
	epsilon = .01) 
 
 
 
as.data.frame(all_clusters,
 row.names = V(gu)$name ) %>% 
 write_csv("./results/clusters_N.csv")

# inspect and compare the clustering results
# in this case compare all N trials and calculate the probability that each company is in the same cluster of any other company
# Then select as a cluster only those who have a probability > threshold 50%

ncompanies <- nrow(all_clusters)
x <- matrix(0, nrow=ncompanies, ncol=ncompanies)
colnames(x)<-V(gu)$name
rownames(x)<-V(gu)$name

for (i in (1:n_trials)){
 	if (echo) print(paste("comparing cluster assignation ", i))
 	nclusters <- max(all_clusters[,i])
	for (k in 1:nclusters) {
		samecluster <- (which(all_clusters[,i]==k))
		nc <- length(samecluster)
		for (t in 1:nc){
			for (j in 1:nc){ 
			x[samecluster[j],samecluster[t]] <- x[samecluster[j],samecluster[t]] +1
			}
		}
	}
}

x<-x/n_trials #normalize

threshold = .5			 # threshold to decide on membership
current.cluster = 0 	

consensus_clusters <- as_tibble_col(rownames(x)) %>% mutate(membership=0)
more_clusers_to_be_found=TRUE
remaining <- x

min_vids <- 2 # counts only clusters with min_vids or more members
min_weight <- (1/100) * sum(V(gu)$str) # counts only clusters above a given treshold
weights <- V(gu)$str

print("identify clusters above min_vids")
ccs <- tibble(name = "x", mbshp = -1, prob = 0.0) %>% head(0)

while (more_clusers_to_be_found){
	
	cluster_ii_members <- which(remaining[1, ] > threshold)
	selected <- remaining[cluster_ii_members, cluster_ii_members]
	remaining<- remaining[-cluster_ii_members, -cluster_ii_members]

	print(paste(current.cluster, sum(weights[cluster_ii_members])))

	if(length(cluster_ii_members) > min_vids & 
		sum(weights[cluster_ii_members]) > min_weight) {
		current.cluster <- current.cluster + 1
		if(echo) print(paste("Processing cluster ", current.cluster, " with ", nrow(selected), "vertices"))
		
		for (j in 1:nrow(selected)) {
			selected[j,j]<- 0.0 #diagonal elements do not matter
			pp <- max(selected[j,])
			nn <- names(selected[1,])[j]
			ccs <- ccs %>% add_row(name=nn , mbshp=current.cluster, prob = pp) 
			print(paste("Adding vid ", nn,pp))
		}
	}
	else{
		if(echo){print(paste("a group below threshold ", current.cluster, " with ", length(cluster_ii_members), "vertices"))
	}
}
	if (length(remaining) > min_vids) {more_clusers_to_be_found=TRUE} 
	else{more_clusers_to_be_found=FALSE}
}
 
# from here on we are back to directed graph g (not gu!)
print("sorting cluster labels...")
V(g)$CL1 <- 0
cl_conv_table = as.data.frame(table(ccs$mbshp)) %>%
	rename(comm_size = Freq) %>%
	rename(cccc=Var1) %>%
	arrange(-comm_size)
 
cl_new_labels <- 1
for (i in cl_conv_table$cccc){
	selected_vids <- ccs %>%
	filter(mbshp == i) %>%
	select(name) %>%
	pull() %>%
	unlist()
	V(g)[ V(g)$name %in% selected_vids ]$CL1 <- cl_new_labels
	cl_new_labels <- cl_new_labels + 1
}

# print("Assign probability")
V(g)$CL1_p <- 0.0
for (i in 1:nrow(ccs)) V(g)[ccs[i,]$name]$CL1_p <- ccs[i,]$prob
windows();hist(V(g)$CL1_p,breaks=100)

 
print("Saving results")
ccs %>% write_csv("./results/clusters_consensus.csv")
#ccs <- read_csv("./results/clusters_consensus.csv")

# create a "community" object, standard igraph output
# clusters_consensus <- make_clusters(
# g,
# membership = V(g)$CL1,
# algorithm = "louvian consensus",
# merges = NULL,
# modularity = FALSE
# )
####

#source("./code/functions-network-analysis.R")
# generate image communities8.png
show_subgraphs (g, 
 clusters_membership = V(g)$CL1, 
 nrows=2,
 ncols=4,
 label="CL1" ) 

## improve histogram of probability:  
V(g)$colorscale <- round(V(g)$CL1_p,1)
V(g)$color <- '#fff25ecc'
V(g)$color[V(g)$colorscale == 0.7] <- '#fc91efcc'
V(g)$color[V(g)$colorscale == 0.8] <- '#fb6a4acc'
V(g)$color[V(g)$colorscale == 0.9] <- '#2638decc'
V(g)$color[V(g)$colorscale == 1.0] <- '#26ab17cc'

df <- tibble(comm=V(g)$CL1,prob=V(g)$CL1_p,ccc=V(g)$color) %>%
	mutate(prob = round(prob,2)) %>%
	group_by(prob,ccc)%>%
	count(prob, ccc)%>%
	mutate(f = n/length(V(g)))%>%
	filter(prob>=0.5)
 

ggplot(df, aes(x=prob, y=f)) +
  geom_col(fill="blue")+
  xlim(c(0.49,1.01))+
  theme_light()

ggsave('./results/figures/probability_dist.png')



g %>% write_graph("./results/communities_consensus.csv", format="graphml")
as_long_data_frame(g) %>% write_csv("./results/communities_consensus_as_df.csv")


#https://colorbrewer2.org/type=sequential&scheme=Reds&n=5
#palette_reds <- ['#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15']
#pal <- brewer.pal(length(unique(V(g)$colorscale)), "Greens")
#palette_reds <- c('#a50f15','#de2d26' ,'#fb6a4a80','#fcae9180','#fee5d980')
V(g)$colorscale <- round(V(g)$CL1_p,1)
V(g)$color <- '#fff25ecc'
V(g)$color[V(g)$colorscale == 0.7] <- '#fc91efcc'
V(g)$color[V(g)$colorscale == 0.8] <- '#fb6a4acc'
V(g)$color[V(g)$colorscale == 0.9] <- '#2638decc'
V(g)$color[V(g)$colorscale == 1.0] <- '#26ab17cc'

print("analysis...")

print("mixmat by clusters")
m <- mixmat(g,"CL1", use.density=TRUE )

print("heatmap by clusters")
mdf <- m %>% ### HEATMAP improve labels sorting
 as.data.frame() %>%
 rownames_to_column("from") %>%
 pivot_longer(-c("from"), names_to = "to", values_to = "weight") %>%
 mutate(from = as.integer(from))%>%
 mutate(to = as.integer(to))

mdf %>% write_csv("./results/matrix.csv")
 

mdf %>%
 ggplot(aes(x=from, y=to, fill=weight)) + 
 geom_raster() + 
 scale_fill_gradient(low = "#ffffff00", high = "#000000") + 
 theme_light()
ggsave("./results/figures/heatmap_clusters.png")

# iter-cluster VS itra-cluster weight
# mdf <- read_csv("./results/matrix.csv") %>% 
# 	mutate(from = as.integer(from)) %>%
# 	mutate(to = as.integer(to)) 

list_clusters <- mdf %>% select(from) %>% distinct() %>% pull()
results <- tibble(cl = 0, inter = 0.0, intra = 0.0, ratio = 0.0) %>% head(0)
for (i in list_clusters){
	subset <- mdf %>% filter(from == i)
	intra <- subset %>% filter(to == i) %>% select(weight) %>% sum()
	inter <- subset %>% filter(to != i) %>% select(weight) %>% sum()
	ratio = intra / inter
	results <- results %>% add_row(
		cl = i, 
		intra = intra,
		inter = inter, 
		ratio = ratio)
}

p1 <- ggplot(results) + 
	geom_point( aes(x = cl, y = inter),size = 3, shape =  16, colour = "#20bf20")+ 
	geom_point( aes(x = cl, y = intra),size = 3, shape =  8, colour = "red")+
	scale_x_continuous(breaks = seq(0, nrow(results)))+
	theme_light()+theme(aspect.ratio=.4)
plot(p1)
ggsave("./results/figures/clusters_inter_intra.png")
	
 
p2<- ggplot(results,aes(x = cl, y = ratio)) + 
	geom_col( fill = "gray", color = "#0000ff00")+
	geom_hline(yintercept = 1, color = "red")+
	scale_x_discrete(limits = as.character(seq(1, nrow(results))))+
	theme_light()+theme(aspect.ratio=.4)

plot(p2)
ggsave("./results/figures/clusters_inter_intra_ratio.png")

figure <- ggarrange(p1, p2,
                    labels = c("intra-cluster and inter-cluste weight", 
								"ratio intra-cluster / inter-cluster"),
                    ncol = 1, nrow = 2)
plot(figure)
ggsave("./results/figures/clusters_inter_intra_2.png")

clusters_graph <- graph_from_data_frame(mdf, directed = FALSE, vertices = NULL)
V(clusters_graph)$core <- graph.coreness(clusters_graph)
V(clusters_graph)$strength <- strength( clusters_graph, loops = FALSE) 

edgew = (E(clusters_graph)$weight/max(E(clusters_graph)$weight)*100)

edgec=ifelse(is.loop(clusters_graph), "#ffffff00","#07d84d6d")
edgec=ifelse(edgew > 1, edgec,"#ffffff00")# no colour for weak links 
vertexs<-V(clusters_graph)$strength * 200 

windows();plot(clusters_graph,
	layout=layout_with_mds,
	edge.color=edgec,
	#edge.width=edgew
  vertex.size=vertexs,
  vertex.color = "#04b0ff",
  vertex.label.font=1,
  vertex.label.color="black"
)

top_clusters <- V(clusters_graph)$name [1:10]
ggg <- induced.subgraph(clusters_graph,vids = top_clusters)
windows();plot( ggg,
 layout=layout.graphopt,
 edge.width = E(ggg)$weight / max(E(ggg)$weight)*10,
 vertex.size= strength(ggg)*100,
 vertex.color = "#04b0ff",
 vertex.label.font=1,
 vertex.label.color="black")

top_clusters <- V(clusters_graph)$name [1:9]
ggg <- induced.subgraph(clusters_graph,vids = top_clusters)
windows();plot( ggg,
 layout=layout.circle,
 edge.width = sqrt(E(ggg)$weight / max(E(ggg)$weight))*20,
 vertex.size= strength(ggg)*100,
 vertex.color = "#04b0ffab",
 vertex.label.font=1,
 vertex.label.color="black")

clusters_graph %>% write_graph("./results/_clusters_graph.csv", format="graphml")
as_long_data_frame(clusters_graph) %>% write_csv("./results/_clusters_graph_as_df.csv")
print("Script completed.")

#
