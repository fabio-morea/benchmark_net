 
library(tidyverse, warn.conflicts = FALSE)
library(jsonlite) 

json_data <- fromJSON("results.JSON")
cols_to_extract <- c('mu', 'nmi', 'method', "a",'rep', 'trial')
tmp_list <- list()
nn <- length(cols_to_extract)
for (i in 1:nn){
    tmp_list[[i]] <- pluck(json_data, cols_to_extract[i]) 
}
df <- data.frame(tmp_list)
names(df) <- cols_to_extract
df$mbs <- pluck(json_data, "membership")

for (i in (1:nrow(df))){
    
    com_sizes <- as.vector(table(df$mbs[i]))
    #com_sizes <- sort(com_sizes, decreasing = TRUE)
    com_sizes <- c(com_sizes, rep(NA, 100))
    com_sizes <- com_sizes[1:100]
    df$cs[i] <- list(com_sizes)
    
}

len <- lengths(df$nmi)
df$id <- seq.int(nrow(df))

df1 <- data.frame(  nmi = rep(df$nmi, len ),
                    id = rep(df$id, len ),
                    r = rep(df$rep, len ),
                    t = rep(df$trial, len ),
                    cs = unlist(df$cs),
                    a = rep(df$a, len ),
                    method = rep(df$method, len),
                    mu = rep(df$mu, len ),
                    modularit = rep(df$modularit, len)
                    ) %>%
        filter(a < 1) %>% 
        filter( mu %in% c(10,50,80))  %>% 
        group_by(id) %>%
        #arrange(id, cs) %>%  
        na.omit()   

print(head(df1))
#print(head(df))
    
# p4 <- ggplot(df1, aes(x = id, y=cs, group = method, color = method))+
#     geom_line( size = 2, alpha = 0.2)+
#     geom_point( size = 1, alpha = 0.2)+
#     scale_color_manual(values=c("red", "blue")) +
#     theme_light()  +

#     facet_grid(rows = vars(mu)) 

 
p <- ggplot(df1 %>% 
        filter(mu == 10)%>% 
        filter(r <= 99) , 
    aes(x = nmi, y=cs, group = id, color = method))+
    #facet_grid(cols = vars(a)) +

    geom_line( size = 2, alpha = 0.2)+
    geom_point( size = 1, alpha = 0.2)+
    geom_hline(yintercept = 20)+
    geom_hline(yintercept = 50)+
    scale_color_manual(values=c("red", "blue")) +
    theme_light()  +
    #ylim(0,200)+
    ggtitle("community size distribution:  mu = 10")
print(p) 
ggsave("plot comm distrib_mu10.png")

p <- ggplot(df1 %>% 
            filter(mu == 50)%>% 
            filter(r <= 99),
    aes(x = nmi, y=cs, group = id, color = method))+
    #facet_grid(cols = vars(a)) +
    geom_line( size = 2, alpha = 0.2)+
    geom_point( size = 1, alpha = 0.2)+
    geom_hline(yintercept = 20)+
    geom_hline(yintercept = 50)+
    scale_color_manual(values=c("red", "blue")) +
    theme_light()  +
    #ylim(0,200)+
    ggtitle("community size distribution:  mu = 50")
print(p) 
ggsave("plot comm distrib_mu50.png")


p <- ggplot(df1 %>% 
            filter(mu == 80) %>% 
            filter(r <= 99),
    aes(x = nmi, y=cs, group = id, color = method))+
    #facet_grid(cols = vars(a)) +
    geom_line( size = 2, alpha = 0.2)+
    geom_point( size = 1, alpha = 0.2)+
    geom_hline(yintercept = 20)+
    geom_hline(yintercept = 50)+
    scale_color_manual(values=c("red", "blue")) +
    theme_light()  +
    #ylim(0,200)+
    ggtitle("community size distribution:  mu = 80")
print(p) 
ggsave("plot comm distrib_mu80.png")

p <- ggplot(df1 %>%  
            filter(a == 0.1),
    aes(x = nmi, y=cs, group = id, color = method))+
    #facet_grid(cols = vars(mu)) +
    geom_line( size = 2, alpha = 0.2)+
    geom_point( size = 1, alpha = 0.2)+
    geom_hline(yintercept = 20)+
    geom_hline(yintercept = 50)+
    scale_color_manual(values=c("red", "blue")) +
    theme_light()  +
    ylim(0,200)+
    ggtitle("community size distribution:  mu = 80")
print(p) 
ggsave("plot comm distrib_mu10 50 80.png")


p <- ggplot(df1 %>% 
                    filter(mu == 50) %>% 
                    filter(method == "LV")  %>% 
                    filter(a == 0.1)  %>% 
                    filter(r < 99), 
    aes(x = modularit, y=cs, group = id, color = method))+
    geom_line( size = 2, alpha = 0.2)+
    geom_point( size = 1, alpha = 0.2)+
    geom_hline(yintercept = 20)+
    geom_hline(yintercept = 50)+
    scale_color_manual(values=c( "blue")) +
    theme_light()  +
    #ylim(0,200)+
    ggtitle("community size distribution (independent trials):  mu = 80")
print(p) 
ggsave("plot comm distrib_modularit_nmi.png")