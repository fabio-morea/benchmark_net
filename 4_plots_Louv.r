
library(ggplot2)
library(tidyverse)

data_summary <- function(data, varname, groupnames){
    # Function to calculate the mean and the standard deviation  for each group
    # data : a data frame
    # varname : the name of a column containing the variable to be summariezed
    # groupnames : vector of column names to be used as grouping variables
    require(plyr)
    summary_func <- function(x, col){
        c(mean = mean(x[[col]], na.rm=TRUE),
        sd = sd(x[[col]], na.rm=TRUE))
    }
    data_sum <-ddply(data, groupnames, .fun=summary_func, varname)
    data_sum <- rename(data_sum, c("mean" = varname))
    data_sum[is.na(data_sum)] <- 0
    return(data_sum)
}


df <- read.csv(file = "results/summary_results.csv")

df$nmi <- df$value

df1 <- data_summary(df, varname="nmi", 
                    groupnames=c("method", "mu"))
#print(df1)


p1 <- ggplot(df1, aes(x = mu, y = nmi, group = method, color = method)) +
        geom_line( ) +
        geom_point(aes(shape = method)) +
        scale_color_manual(values=c("red", "blue")) +
        geom_errorbar(aes(ymin=nmi-sd, ymax=nmi+sd), width=.2,position=position_dodge(0.05))+
        ylim(0,1) + theme_bw() +
        xlab("mu") +
        ylab("nmi") +
        ggtitle("Louvain Community detection: comparing single runs with consensus")
print(p1)
ggsave("results/plot_nmi.png")

##############

 
 

df2 <- data_summary(df, varname="nc", 
                    groupnames=c("method", "mu"))
 
p2 <- ggplot(df2, aes(x = mu, y = nc, group = method, color = method)) +
        geom_line( ) +
        geom_point(aes(shape = method)) +
        geom_errorbar(aes(ymin=nc-sd, ymax=nc+sd), width=.2,position=position_dodge(0.05))+
        scale_color_manual(values=c("red", "blue")) +
        theme_bw() +
        #ylim(0,100) +
        xlab("mu") +
        ylab("nc") +
        geom_hline( yintercept = 37.0)+
        ggtitle("Louvain Community detection: comparing single runs with consensus")

print(p2)
ggsave("results/plot_number_of_communities.png")




## community size distribution on LFR benchmark
# for (mui in seq(10, 99, 5)) {
#   	filename = paste0("FLR_benchmark_", mui,".gml")
# 	## load graph
# 	print(paste("Loading graph...", filename))
# 	g <- read_graph(filename, format="gml")
#     print(V(g)$community)
# }

## community size distribution on 100 Louvain
plot_comm_dist <- function(filename){

    mm  <- read.csv(file = filename)
    df3 <-  data.frame(x = rep(1, 200))
     
    for (i in 1:ncol(mm)){ 
        com_sizes <- as.vector( table(mm[,i]) )
        com_sizes <- sort(com_sizes, decreasing = FALSE)
        com_sizes <- c(com_sizes, rep(NA, 100))
        com_sizes <- com_sizes[1:100]
        df3[,i] <- com_sizes

    }
    df3 <- df3[,2:ncol(mm)] 
    # df3 <- df3[ ,order(df3[1,]) ]
    # names(df3) <- paste0("t", 1:length(df3))

    df4 <- df3 %>%
        pivot_longer(everything(), names_to = "trial", values_to = "Size")  
     

    p4 <- ggplot(df4, aes(x = trial, y=Size))+
        geom_line(color='gray', size = 2, alpha = 0.5)+
        geom_point( color = "blue", size = 2, alpha = 0.5)+
        theme_light() +
        #ylim(0, 200)  + 
        scale_x_discrete(labels= NULL)+
        geom_hline( yintercept = 20.0)+
        geom_hline( yintercept = 50.0)+

        xlab("") + ggtitle(paste("Modularity based community detection", filename))

    print(p4)
    ggsave(paste0("results/plot_comm_size", filename, ".png"))
}
for (mui in seq(10,99,10)){
    plot_comm_dist(paste0("results/mixing_matrix", mui, ".csv"))

}
 



# p4h <- ggplot(df4, aes(  x=Size))+
#     geom_histogram(color = 'white', fill = 'blue')+
#     theme_light() 
# print(p4h)
# ggsave(width = 6, height = 3,"results/plot_comm_size_hist.png")