## load libraries
library(tidyverse, warn.conflicts = FALSE)
library(jsonlite) 


## load data: ###############################################################
json_data <- fromJSON("results_postSDS.JSON")

cols_to_extract <- c('mu', 'nmi', 'method', "a",'rep', 'trial')
tmp_list <- list()
nn <- length(cols_to_extract)
for (i in 1:nn){
    tmp_list[[i]] <- pluck(json_data, cols_to_extract[i]) 
}
df <- data.frame(tmp_list)
names(df) <- cols_to_extract

robs <-   pluck(json_data, "rob_mem")
n_trials = length(robs)
n_nodes <- 1000
# extract robustness, which is a list of 1000 where method == CONS
# and an empty list otherwise
for (i in 1:n_trials){
    if (df$method[i] == "CONS"){
        df$rob[i] <- list(robs[[i]])
    } else {
        df$rob[i] <- list(rep(-1, n_nodes))
    }
} 

# identify the node
df$id <- seq.int(nrow(df))
df <- df  %>% filter( method == "CONS")
print(str(df))
# tidy dataset ready for plotting
df2 <- df %>%
    unnest(rob) %>%
    pivot_longer(cols = c(rob), names_to = 'tmp', values_to = 'rob')  %>% 
    select(-tmp) %>%  
    arrange(rep, trial, mu, a, method, id, nmi, rob)

 
p <- df2  %>% ggplot( aes(x = rob)) +
    facet_grid(cols = vars(a)) +
        geom_density( adjust = 3,  color = "red") +
        geom_histogram(aes(y = ..density..),
                 position = "identity",
                 alpha = 0.5) +
                labs(title = "Robust Histogram",
                    x = "Robustness of membership",
                    y = "Percentage") 
print(p)

 

