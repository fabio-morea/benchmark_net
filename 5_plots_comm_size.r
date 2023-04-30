 
library(tidyverse, warn.conflicts = FALSE)
library(jsonlite) 

json_data <- fromJSON("results_postSDS.JSON")
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
    com_sizes <- c(com_sizes, rep(NA, 100))
    com_sizes <- com_sizes[1:100]
    df$cs[i] <- list(com_sizes)
    }

len <- lengths(df$nmi)
df$id <- seq.int(nrow(df))

# tidy dataset ready for plotting
df1 <- df %>%
    unnest( cs) %>%
    pivot_longer(cols = c(cs), names_to = 'tmp', values_to = 'cs')  %>% 
    select( rep, trial, mu, a, method, id, nmi, cs) %>%  
    arrange(rep, trial, mu, a, method, id, nmi, cs) %>% 
    na.omit()   
 

  
    
p <- df1 %>% 
        filter (mu > 30) %>% 
        filter(a <= 0.2) %>%
    ggplot(aes(x = cs)) +
    facet_grid(cols = vars(a), rows = vars(mu)) +
    geom_histogram()+
    theme_light() 
print(p)

p <- ggplot(df1 %>% 
            filter( mu > 20)   , 
        aes(x = id, y=cs, group = id, color = method))+
    geom_line( linewidth = 2, alpha = 0.2)+
    geom_point(  alpha = 0.2)+
    scale_color_manual(values=c("red", "blue")) +
    theme_light()   

print(p)


 

p <- ggplot(df1 %>% 
        filter(mu == 80)%>% 
            filter( a == 0.2)  , 
    aes(x = nmi, y=cs, group = id, color = method))+
    geom_line( size = 2, alpha = 0.2)+
    geom_point( size = 1, alpha = 0.2)+
    geom_hline(yintercept = 20)+
    geom_hline(yintercept = 50)+
    scale_color_manual(values=c("red", "blue")) +
    theme_light()  +
    ggtitle("community size distribution:  mu = 80")
print(p) 
ggsave("plot comm distrib_mu80.png")

 
p <- ggplot(df1 %>% 
            filter(mu == 50)  %>% 
            filter(rep == 1) %>% 
            filter(a == 0.5),
    aes(x = nmi, y=cs, group = id, color = method))+
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


# p <- ggplot(df1 %>% 
#             filter(mu == 80) %>% 
#             filter(r <= 99),
#     aes(x = nmi, y=cs, group = id, color = method))+
#     #facet_grid(cols = vars(a)) +
#     geom_line( size = 2, alpha = 0.2)+
#     geom_point( size = 1, alpha = 0.2)+
#     geom_hline(yintercept = 20)+
#     geom_hline(yintercept = 50)+
#     scale_color_manual(values=c("red", "blue")) +
#     theme_light()  +
#     #ylim(0,200)+
#     ggtitle("community size distribution:  mu = 80")
# print(p) 
# ggsave("plot comm distrib_mu80.png")

# p <- ggplot(df1 %>%  
#             filter(a == 0.1),
#     aes(x = nmi, y=cs, group = id, color = method))+
#     #facet_grid(cols = vars(mu)) +
#     geom_line( size = 2, alpha = 0.2)+
#     geom_point( size = 1, alpha = 0.2)+
#     geom_hline(yintercept = 20)+
#     geom_hline(yintercept = 50)+
#     scale_color_manual(values=c("red", "blue")) +
#     theme_light()  +
#     ylim(0,200)+
#     ggtitle("community size distribution:  mu = 80")
# print(p) 
# ggsave("plot comm distrib_mu10 50 80.png")


# p <- ggplot(df1 %>% 
#                     filter(mu == 50) %>% 
#                     filter(method == "LV")  %>% 
#                     filter(a == 0.1)  %>% 
#                     filter(r < 99), 
#     aes(x = modularit, y=cs, group = id, color = method))+
#     geom_line( size = 2, alpha = 0.2)+
#     geom_point( size = 1, alpha = 0.2)+
#     geom_hline(yintercept = 20)+
#     geom_hline(yintercept = 50)+
#     scale_color_manual(values=c( "blue")) +
#     theme_light()  +
#     #ylim(0,200)+
#     ggtitle("community size distribution (independent trials):  mu = 80")
# print(p) 
# ggsave("plot comm distrib_modularit_nmi.png")