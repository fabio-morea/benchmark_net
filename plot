
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




df <- read.csv(file = "results.csv")
df$nmi = df$value

df1 <- data_summary(df, varname="nmi", 
                    groupnames=c("method", "mu"))
 


p1 <- ggplot(df1, aes(x = mu, y = nmi, group = method, color = method)) +
        geom_line( ) +
        geom_point(aes(shape = method)) +
        geom_errorbar(aes(ymin=nmi-sd, ymax=nmi+sd), width=.2,position=position_dodge(0.05))+
        ylim(0,1) + theme_bw() +
        xlab("mu") +
        ylab("nmi") +
        ggtitle("Louvain Community detection: comparing single runs with consensus")

print(p1)
ggsave("results_nmi.png")


df2 <- data_summary(df, varname="nc", 
                    groupnames=c("method", "mu"))
 


p2 <- ggplot(df2, aes(x = mu, y = nc, group = method, color = method)) +
        geom_line( ) +
        geom_point(aes(shape = method)) +
        geom_errorbar(aes(ymin=nc-sd, ymax=nc+sd), width=.2,position=position_dodge(0.05))+
        theme_bw() +
        xlab("mu") +
        ylab("nc") +
        ylim(0,60)+
        ggtitle("Louvain Community detection: comparing single runs with consensus")

print(p2)
ggsave("results_number_of_communities.png")

 