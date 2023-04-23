 
library(ggplot2)
library(tidyverse)



## community size distribution on consensus
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
    #df3 <- df3[,] 
    print(head(df3))

    
    p4 <- ggplot(df3, aes(x = x, y=V2))+
        geom_line(color='gray', size = 2, alpha = 0.2)+
        geom_point(color='red', size = 2, alpha = 0.2)+
        theme_light() +
        ylim(0, 100)  + 
        scale_x_discrete(labels= NULL)+
        xlab("") + ggtitle(paste("CONSENSUS community size distribution", filename))

    print(p4)
    ggsave(paste0("results/plot_comm_size", filename, ".png"))

    p4h <- ggplot(df3, aes(  x=V2))+
    geom_histogram(bins = 4, color = 'white', fill = 'red')+
    theme_light() 
    print(p4h)
    ggsave(width = 6, height = 3,"results/plot_comm_size_hist.png")

}
for (mui in seq(10,99,10)){
    plot_comm_dist(paste0("results/cons_labels", mui, ".csv"))

}