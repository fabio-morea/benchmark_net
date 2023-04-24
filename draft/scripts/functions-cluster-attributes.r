# functions-cluster-attributes

get.professional.groups <- function(g, cluster_name){
  prof_groups <- E(g)$PG
  n <- length(prof_groups)
  summary <- 
    as.data.frame(table(prof_groups )) %>%
    mutate(rel_freq = Freq/n) %>%
    mutate(cl_name = cluster_name)
    if (echo){print(summary)}
  return(summary)
}

get.locations <- function(g, cluster_name){
  locs <- E(g)$LOC
  n <- length(locs)
  summary <- 
    as.data.frame(table(locs)) %>%
    mutate(rel_freq = Freq/n) %>%
    mutate(cl_name = cluster_name)
    if (echo){print(summary)}
  return(summary)
}

get.sectors <- function(g, cluster_name){
  sectors <- E(g)$NACE_group
  n <- length(sectors)
  summary <- 
    as.data.frame(table(sectors)) %>%
    mutate(rel_freq = Freq/n) %>%
    mutate(cl_name = cluster_name)
    if (echo){print(summary)}
  return(summary)
}

scatter_strength_core <- function(g,gi){
    library(infotheo)
    data <- tibble(
            core =  as.integer(V(g)$core), 
            stre = as.numeric(V(g)$str), 
            name = V(g)$name)%>%
            mutate(community = if_else(name %in% V(gi)$name, 1, 0))


    # mut_inf <- mutinformation(data$core,data$stre, method="emp")
    # entr    <- sqrt(entropy(data$core) * entropy(data$stre) )
    # NMI     <- round(mut_inf/ entr,3)

   scatterplot <- ggplot(data) + theme_classic()+ 
              geom_point(aes(y = stre, x = core, 
                        colour = as.factor(community),
                        alpha = as.factor(community), 
                        size = as.factor(community)))+
                        scale_colour_manual(values=c("black","red"))+
                        scale_shape_manual(values=c(1,19))+
                        scale_size_manual(values=c(3,5))+
                        scale_alpha_manual(values=c(.2,1.0))+
                        scale_x_continuous(breaks=seq(1,max(V(g)$core,1)))+
                        scale_y_continuous(trans="log10")+
                        theme(panel.grid.major = element_line(color = "gray"))+
                        xlab("Coreness")+ylab("Strength")
                        #labs(title = "Comparison of strength and coreness",
                              #subtitle = glue("Normalized Mutual Information ", NMI),
                              #caption = glue("number of vertices: ",length(V(g))))

    

    return(scatterplot)
}