## load libraries
library(tidyverse, warn.conflicts = FALSE)
library(jsonlite) 

## summary function
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

## extract values function
extract_values <- function(jd, cols_to_extract){
    tmp_list <- list()
    nn <- length(cols_to_extract)
    for (i in 1:nn){
        tmp_list[[i]] <- pluck(json_data, cols_to_extract[i]) 
    }
    df <- data.frame(tmp_list)
    names(df) <- cols_to_extract
    return(df)
}

################################################################
# read_json <- function(filename, cols_to_extract){
#     json_data <- fromJSON("results_postSDS2.JSON") %>% 
#         select(cols_to_extract)

#     tmp_list <- list()
#     nn <- length(cols_to_extract)
#     for (i in 1:nn){
#         tmp_list[[i]] <- pluck(json_data, cols_to_extract[i]) 
#     }
#     df <- data.frame(tmp_list)
#     names(df) <- cols_to_extract
#     return(df)
# }

# df <- read_json(    filename = "results_postSDS2.JSON", 
#                     cols_to_extract = c('mu', 'nmi', 'method', "a",'rep', 'trial'), 
#                  )
                  

## PLOT NMI ##############################
json_data <- fromJSON("results_postSDS.JSON")
#json_data <- fromJSON("results.JSON")

df <- extract_values(json_data, c('mu', 'nmi', 'method', "a") )
alphas = list( unique( df$a[ (df$method == "LV") ] ))
df <- data_summary(df, varname="nmi", groupnames=c("method", "mu" , "a" ))

plt <- ggplot(df, aes(x = mu, y = nmi, group = method, color = method)) +
        facet_grid(cols = vars(a)) +
        geom_line( ) +
        geom_point(aes(shape = method)) +
        scale_color_manual(values=c("red", "blue")) +
        geom_errorbar(aes(ymin=nmi-sd, ymax=nmi+sd), width=.2,position=position_dodge(0.05))+
        ylim(0,1) + theme_bw() +
        xlab("mu") +
        ylab("Normalized Mutual Information") +
        ggtitle(paste( 'Consensus community detection VS Louvain with Alpha =' , alphas))
print(plt)
ggsave("results/plot_nmi.png")


# ## PLOT NMI ##############################
# df <- extract_values(json_data, c('mu', 'nmi', 'method', "a", "modularit") )
# alphas = list( unique( df$a[ (df$method == "LV") ] ))
# df <- data_summary(df, varname="nmi", groupnames=c("method", "mu" , "a", "modularit" ))

# plt <- ggplot(df, aes(x = modularit, y = nmi, group = method, color = method)) +
#         facet_grid(cols = vars(a), rows = vars(mu)) +
#         geom_line( ) +
#         geom_point(aes(shape = method)) +
#         scale_color_manual(values=c("red", "blue")) +
#         #geom_errorbar(aes(xmin=nmi-sd, xmax=nmi+sd), width=.2,position=position_dodge(0.05))+
#         theme_bw() +
#         xlab("modularity") +
#         ylab("Normalized Mutual Information") +
#         ggtitle(paste( 'Consensus community detection VS Louvain with Alpha =' , alphas))
# print(plt)
# ggsave("results/plot_m_nmi.png")


## PLOT Number of communities ##############################
df <- extract_values(json_data, c('mu', 'nc', 'method', 'a') )
alphas = list( unique( df$a[ (df$method == "LV") ] ))
df <- data_summary(df, varname="nc", groupnames=c("method", "mu", "a" ))
plt <- ggplot(df, aes(x = mu, y = nc, group = method, color = method )) +
        facet_grid(cols = vars(a)) +
        geom_hline(yintercept = 37) +
        geom_line( ) +
        geom_point(aes(shape = method)) +
        scale_color_manual(values=c("red", "blue")) +
        geom_errorbar(aes(ymin=nc-sd, ymax=nc+sd), width=.2,position=position_dodge(0.05))+
        theme_bw() +
        xlab("mu") +
        ylab("number of communities") +
        ggtitle(paste( 'Consensus community detection VS Louvain with Alpha =' , alphas))
print(plt)
ggsave("results/plot_nc.png")


## PLOT Number of communities ##############################
# df <- extract_values(json_data, c('mu', 'nc', 'method', "nmi", 'a') )
# alphas = list( unique( df$a[ (df$method == "LV") ] ))
# df <- data_summary(df, varname="nc", groupnames=c("method", "mu", "nmi" , "a"))
# plt <- ggplot(df, aes(x = nmi, y = nc, group = method, color = method )) +
#         facet_grid(rows = vars(mu), cols = vars(a)) +
#         geom_hline(yintercept = 37) +
#         geom_line( ) +
#         geom_point(aes(shape = method)) +
#         scale_color_manual(values=c("red", "blue")) +
#         #geom_errorbar(aes(ymin=nc-sd, ymax=nc+sd), width=.2,position=position_dodge(0.05))+
#         theme_bw() +
#         xlab("nmi") +
#         ylab("number of communities") +
#         ggtitle(paste( 'NMI NC Alpha =' , alphas))
# print(plt)
# ggsave("results/plot_nmi_nc.png")


## PLOT Number of communities ##############################
# df <- extract_values(json_data, c('mu', 'nc', 'method', "nmi", 'a', 'rep', 'trial') )  %>% 
#     filter(trial == 1)%>%
#     filter(a == 0)
    
# alphas = list( unique( df$a[ (df$method == "LV") ] ))
# df <- data_summary(df, varname="nc", groupnames=c("method", "mu", "nmi" , "a"))
# plt <- ggplot(df, aes(x = nmi, y = nc, group = method, color = method )) +
#         facet_grid(rows = vars(mu), cols = vars(a)) +
#         geom_hline(yintercept = 37) +
#         geom_line( ) +
#         geom_point(aes(shape = method)) +
#         scale_color_manual(values=c("red", "blue")) +
#         #geom_errorbar(aes(ymin=nc-sd, ymax=nc+sd), width=.2,position=position_dodge(0.05))+
#         theme_bw() +
#         xlab("nmi") +
#         ylab("number of communities") +
#         ggtitle(paste( 'NMI NC Alpha =' , alphas))
# print(plt)
# ggsave("results/plot_nmi_nc.png")





## PLOT modularity - NMI ##############################
df <- extract_values(json_data, c('mu', 'method', "nmi", 'a', 'rep', 'trial', 'modularit') )  %>% 
    filter(a == 0.1)
    
alphas = list( unique( df$a[ (df$method == "LV") ] ))
df <- data_summary(df, varname="nc", groupnames=c("method", "mu", "nmi" , "a", "modularit"))
plt <- ggplot(df  %>% 
                filter(mu %in% c( 20, 50, 80)), 
                aes(x = modularit, y = nmi, group = method, color = method )) +
        facet_grid(cols = vars(mu) ) +
        geom_line( ) +
        geom_point(aes(shape = method), size = 5, alpha = .2) +
        scale_color_manual(values=c("red", "blue")) +
        theme_bw() +
        ylim(0,1)+
        xlab("modularity") +
        ylab("NMI") +
        ggtitle(paste( 'NMI NC Alpha =' , alphas))
print(plt)
ggsave("results/plot_mod_nmi.png")
print("done")


 
print("done")