
library(ggplot2)


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
        geom_errorbar(aes(ymin=nmi-sd, ymax=nmi+sd), width=.2,position=position_dodge(0.05))+
        ylim(0,1) + theme_bw() +
        xlab("mu") +
        ylab("nmi") +
        ggtitle("Louvain Community detection: comparing single runs with consensus")
print(p1)
ggsave("results/plot_nmi.png")


df2 <- data_summary(df, varname="nc", 
                    groupnames=c("method", "mu"))
 


p2 <- ggplot(df2, aes(x = mu, y = nc, group = method, color = method)) +
        geom_line( ) +
        geom_point(aes(shape = method)) +
        geom_errorbar(aes(ymin=nc-sd, ymax=nc+sd), width=.2,position=position_dodge(0.05))+
        theme_bw() +
        xlab("mu") +
        ylab("nc") +
        ggtitle("Louvain Community detection: comparing single runs with consensus")

print(p2)
ggsave("results/plot_number_of_communities.png")



library(tidyverse)

mm  <- read.csv(file = "results/mixing_matrix.csv")
p3 <- ggplot()
 
df3 <-  data.frame(x = rep(36, 100))
for (i in 1:ncol(mm)){ 
    com_sizes <- as.vector( table(mm[,i]) )

    # sort list in descending order
    com_sizes <- sort(com_sizes, decreasing = TRUE)

    # add 100 NA values to the list
    com_sizes <- c(com_sizes, rep(NA, 100))

    # keep only first 100 values of  com_sizes
    com_sizes <- com_sizes[1:100]

  
    df3[,i] <- com_sizes
}
df3 <- df3[,2:ncol(mm)]
df4 <- df3 %>%
    pivot_longer(everything(), names_to = "trial", values_to = "Size")  


p4 <- ggplot(df4, aes(x = trial, y=Size, group = trial))+
    geom_line(color='black')+
    geom_point(color='red')+
    theme_void() 
print(p4)
ggsave("results/plot_comm_size.png")