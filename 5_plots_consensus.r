 
library(tidyverse, warn.conflicts = FALSE)
library(jsonlite) 

json_data <- fromJSON("results.JSON")
cols_to_extract <- c('mu', 'nmi', 'method', "a")
tmp_list <- list()
nn <- length(cols_to_extract)
for (i in 1:nn){
    tmp_list[[i]] <- pluck(json_data, cols_to_extract[i]) 
}
df <- data.frame(tmp_list)
names(df) <- cols_to_extract
df$mbs <- pluck(json_data, "membership")

for (i in (1:nrow(df))){
    com_sizes <- as.vector(table(df$mbs[1]))
    com_sizes <- sort(com_sizes, decreasing = TRUE)
    com_sizes <- c(com_sizes, rep(NA, 100))
    com_sizes <- com_sizes[1:100]
    df$cs[i] <- list(com_sizes)
}

len <- lengths(df$nmi)
df$id <- seq.int(nrow(df))

df1 <- data.frame(  nmi = rep(df$nmi, len ),
                    id = rep(df$id, len ),
                    cs = unlist(df$cs),
                    a = rep(df$a, len ),
                    mu = rep(df$mu, len )
                    ) %>%
        filter(a == 0) %>% 
        group_by(id) %>%
        arrange(id, cs) %>%  
        na.omit()   

print(head(df1))
#print(head(df))
    
p4 <- ggplot(df1, aes(x = nmi, y=cs, group = id, color = mu))+
    geom_line( size = 2, alpha = 0.2)+
    geom_point( size = 2, alpha = 0.2)+
    theme_light()  +
    facet_grid(rows = vars(mu)) 

print(p4) 
ggsave("plot comm distrib.png")


