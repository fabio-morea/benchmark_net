
library(ggplot2)
df <- read.csv(file = "df.csv")
df$mu = df$X5
df$NMI = df$X1 
print(df)

ggp <- ggplot(df, aes( mu,  NMI)) + 
                geom_line() + 
                geom_point(shape = 19) + 
                ylim(0.0, 1.05) +
                theme_light()
print(ggp)