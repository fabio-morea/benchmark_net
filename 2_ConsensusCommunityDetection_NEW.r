# Author: Fabio Morea @ Area Science Park
# Acknowledgments: this research work is supervided by prof. Domenico De Stefano, within the frame of PhD in Applied Data Science and Artificial Intelligence @ University of Trieste

# Package: test consensus community detection algorithm
# Description: R program to extract information from labour market data.
# Original data is not included in the package as it contains personal information
# Test data is contains no personal information
# SPDX-License-Identifier: CC-BY-4.0
# Github: https://github.com/fabio-morea/benchmark_net


echo = F
## clear terminal
shell("cls")

## load libraries
library(tidyverse, warn.conflicts = FALSE)
library(igraph)
library(glue)
library(ggpubr)
library(RColorBrewer)
library(ggplot2)
library(NMI)


cluster_N_times <- function(g, n_trials, alpha = 0.01 , res , epsilon = .0001) {
	results<-tibble(
		m=0.0,
		nnclust=as.integer(0),
		random_resolution=1.0)%>%
		head(0)

	all_clusters <- c()
	m_best<-99999
	
	for (i in 1:n_trials){
		#print(paste("Trial", i))
		n_items <- length(E(g))
		n_null <- as.integer(alpha * n_items)
		applied_weights <- E(g)$ww 
		applied_weights[sample(n_items,n_null)] <- epsilon
		E(g)$weight <- applied_weights
    	random_resolution = as.numeric(sample(res, 1))
		cluster_tmp <- cluster_louvain(g, weights = applied_weights , resolution = random_resolution)
    	all_clusters <- cbind(all_clusters,cluster_tmp$membership)
		#identify cluster with best modularity
		m <- modularity (g,  cluster_tmp$membership)
		if(m<m_best){
			m_best<-m
			i_best=i
			best_clusters <- cluster_tmp 
		}
		nnclust <- max(best_clusters$membership)
		results <- results %>% add_row(m,nnclust) 
	}
	write.csv(results, "modularity_ncl.csv")
  return(all_clusters)
}


compare_clustering_results <- function(all_clusters, 
					threshold = .5, 	 # proportion of membership to be assigned to community 0
					min_vids = 1, 		 # number of vertices to be assigned to community 0
					min_weight = 1/10000)# fraction of whole network weight to be assigned to community 0
 { 
	
	# inspect and compare the clustering results
	# in this case compare all N trials and calculate the "proportion of membership"i.e. 
	# i.e. how many times each company is in the same cluster of any other company
	# Then select as a cluster only those who have a probability > threshold 50%	

	x <- matrix(0, nrow=nrow(all_clusters), ncol=nrow(all_clusters))
	colnames(x)<-V(g)$name
	rownames(x)<-V(g)$name

	for (i in (1:n_trials)){
		#print(paste("comparing cluster assignation ", i))
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

	x<-x/ncol(all_clusters) #normalize

	current.cluster = 0 	

	consensus_clusters <- as_tibble_col(rownames(x)) %>% mutate(membership=0)
	more_clusers_to_be_found=TRUE
	remaining <- x

	
	min_weight <- (1/10000) * sum(V(g)$str) # counts only clusters above a given treshold
	weights <- V(g)$str

	#print("identify clusters above min_vids")
	ccs <- tibble(name = "x", mbshp = -1, prob = 0.0) %>% head(0)

	while (more_clusers_to_be_found){
		cluster_ii_members <- which(remaining[1, ] > threshold)
		selected <- remaining[cluster_ii_members, cluster_ii_members]
		remaining<- remaining[-cluster_ii_members, -cluster_ii_members]
		#print(paste(current.cluster, sum(weights[cluster_ii_members])))

		if(length(cluster_ii_members) > min_vids & 
			sum(weights[cluster_ii_members]) > min_weight) {
			current.cluster <- current.cluster + 1
			if(echo) print(paste("Processing cluster ", current.cluster, " with ", nrow(selected), "vertices"))
			
			for (j in 1:nrow(selected)) {
				selected[j,j]<- 0.0 #diagonal elements do not matter
				pp <- max(selected[j,])
				nn <- names(selected[1,])[j]
				ccs <- ccs %>% add_row(name=nn , mbshp=current.cluster, prob = pp) 
				#print(paste("Adding vid ", nn,pp))
			}
		}
		else{
			if(echo){print(paste("a group below threshold ", current.cluster, " with ", length(cluster_ii_members), "vertices"))
		}
	}
		if (length(remaining) > min_vids) {more_clusers_to_be_found=TRUE} 
		else{more_clusers_to_be_found=FALSE}
	}
		
	return(ccs)
}


##################################### PARAMETERS

# FLR benchmarl
mu_values = seq(10, 99, 10)

# repeated Louvain with modified params
n_trials = 20
alphas = c(0.0, 0.05, 0.1 )
res = c(0.9, 1.0, 1.1)
epsilon = 1/1000

# consensus
reps = 10

#####################################

dfresults <- tibble(	rep = 0,
						trial = 0,
						mu = 0, 
						a = 0.0,
						r = 0.0,
						method = "_",
						modularit = 0.0,
						nc = 0, 
						nmi = 0.0, 
						membership = list() )%>% head(0)

j <- 0

for (mui in mu_values){
	
	filename = paste0("FLR_benchmark_", mui,".gml")
	print(paste("Mu ", mui, "Loading graph...", filename))
	g <- read_graph(filename, format="gml")
	E(g)$ww <- rep(1,length(E(g)))
	V(g)$str <- strength(g)
	V(g)$name <- paste0("V" , V(g)$label)
	true_labels <- data.frame(V(g)$name, V(g)$community)

	for (alpha in alphas){
		print(paste("Alpha", alpha))
		for (rep in 1:reps){
			print(paste("Repetition", rep))

			all_clusters <- c()
			for (i in 1:n_trials){
				print(paste("Trial", i))
				n_items <- length(E(g))
				n_null <- as.integer(alpha * n_items)
				applied_weights <- E(g)$ww 
				applied_weights[sample(n_items,n_null)] <- epsilon
				E(g)$weight <- applied_weights
				resol = as.numeric(sample(res, 1))
				cluster_tmp <- cluster_louvain(g, weights = applied_weights , resolution = resol)
				all_clusters <- cbind(all_clusters,cluster_tmp$membership)
				m <- modularity (g,  cluster_tmp$membership)	
				mbs <- list(cluster_tmp$membership)		
					
				louvain_labels <- data.frame(V(g)$name, all_clusters[,i])
				n_m_i = NMI(true_labels,louvain_labels)$value

				j <- j + 1 
				dfresults[j,] <- list(
									rep = rep,
									trial = i,
									mu = mui, 
									a = alpha,
									r = resol,
									method = "LV",
									modularit = m,
									nc = max(cluster_tmp$membership), 
									nmi = n_m_i ,
									membership  = as.list(mbs)
									)			
			}
	
			ccs <- compare_clustering_results(all_clusters, 
								threshold = .5, 	 # proportion of membership below which a node is assigned to community 0
								min_vids = 4, 		 # number of vertices below which a node is assigend to community 0
								min_weight = 1/1000)  # (community weight / total network weight) below which a node is assigned to community 0


			V(g)$comm_louvain <- 0
			cl_conv_table = as.data.frame(table(ccs$mbshp)) %>%
				rename(comm_size = Freq) %>%
				rename(cccc=Var1) %>%
				arrange(-comm_size)
		
			#print("assigning cluster labels...")
			cl_new_labels <- 1
			for (clid in cl_conv_table$cccc){
				selected_vids <- ccs %>%
				filter(mbshp == clid) %>%
				select(name) %>%
				pull() %>%
				unlist()

				V(g)[ V(g)$name %in% selected_vids ]$comm_louvain <- cl_new_labels
				cl_new_labels <- cl_new_labels + 1
			}	
			cons_labels <- data.frame(V(g)$name, V(g)$comm_louvain)
			n_m_i = NMI(true_labels,cons_labels)$value

			j <- j + 1 
			dfresults[j,] <- list(
								rep = rep,
								trial = i,
								mu = mui, 
								a = alpha,
								r = resol,
								method = "CONS",
								modularit = modularity(g, V(g)$comm_louvain + 1 ),
								nc = max(V(g)$comm_louvain + 1), 
								nmi = n_m_i ,
								membership  = list(V(g)$comm_louvain)
								)	
			
			# as.data.frame(cons_labels, row.names = V(g)$name ) %>% 
			# 	write_csv(paste0("results/cons_labels",mui,".csv"))
		}
	}
}

library(rjson)
library(jsonlite) 
json_data <- toJSON(dfresults)#, 		file = "results/summary_results.csv")
 
write(json_data, file="results.JSON")
 
print("Done!")


 
 
