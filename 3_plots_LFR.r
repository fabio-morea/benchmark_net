
library(ggplot2)
library(tidyverse)
library(igraph)

df3 <-  data.frame(x = rep(1, 37))
## community size distribution on LFR benchmark
i <- 1
for (mui in seq(10, 99, 10)) {
  	filename = paste0("FLR_benchmark_", mui,".gml")
	## load graph
	print(paste("Loading graph...", filename))
	g <- read_graph(filename, format="gml")
    #print(as.vector(table(V(g)$community)))
    com_sizes <- as.vector(table(V(g)$community))
    com_sizes <- sort(com_sizes, decreasing = TRUE)
    com_sizes <- c(com_sizes, rep(NA, 37))
    com_sizes <- com_sizes[1:37]
    df3[,i] <- com_sizes
    i = i + 1
}
#print(df3)
write.csv(df3, "df3.csv")

df4 <- df3 %>%
    pivot_longer(everything(), names_to = "mu", values_to = "Size")  

p4 <- ggplot(df4, aes(x = mu, y=Size))+
    geom_line(color='gray', size = 2, alpha = 0.5)+
    geom_point(color='black', size = 2, alpha = 0.5)+
    theme_light() +
    ylim(0, 100)  + 
    scale_x_discrete(labels= "")+
    xlab("") + ggtitle("LFR")


print(p4)
ggsave(width = 2, height = 3,"results/plot_comm_size.png")

 

p4h <- ggplot(df4, aes(  x=Size))+
    geom_histogram(, bins = 10,color = 'white', fill = 'gray')+
    theme_light() + ggtitle("LFR - community size distribution")
print(p4h)
ggsave(width = 6, height = 3,"results/plot_comm_size_hist.png")
