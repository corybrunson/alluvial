library(riverplot)

x <- riverplot.example()


nodes <- c( LETTERS[1:3] )
edges <- list( A= list( C= 10 ), B= list( C= 10 ) )

r <- makeRiver( nodes, 
                edges, 
                # node_xpos= c( 1,1,2 ),
                node_labels= c( A= "Node A", B= "Node B", C= "Node C" ),
                node_styles = list(
                  A=list(col="yellow"),
                  C=list(col="blue")
                )
)


r <- makeRiver( nodes, edges, node_xpos= c( 1,1,2 ),
                node_labels= c( A= "Node A", B= "Node B", C= "Node C" ),
                node_styles= list( A= list( col= "yellow" )) )
plot( r )




tit <- as.data.frame(Titanic)
tit3d <- aggregate( Freq ~ Class + Sex + Survived, data=tit, sum)
alluvial(tit3d[,1:3], freq=tit3d$Freq, alpha=1, xw=0.2,
         col=ifelse( tit3d$Survived == "No", "red", "gray"),
         layer = tit3d$Sex != "Female", sinecurve=TRUE,
         border="white")

tit2d <- aggregate( Freq ~ Class + Survived, data=tit, sum)
alluvial(tit2d[,1:2], freq=tit2d$Freq, alpha=1, xw=0.2,
         col=ifelse( tit2d$Survived == "No", "red", "gray"),
         sinecurve=TRUE,
         border="white")


library(dplyr)
library(tidyr)
isf <- sapply(tit3d, is.factor)
tit3d[isf] <- lapply(tit3d[isf], as.character)

tit3d %>% select(-Freq) %>% gather(variable, value) %>% unique() %>%
  mutate(vv=paste(variable, value, sep="."))



plot(1:2)
stripe(1, 2, 1, 1.4, width=0.2, grad=c("yellow", "blue"), alpha=0.5, nsteps=100)
stripe(1, 2, 1.4, 1, width=0.2, grad=c("orange", "lightskyblue"), alpha=0.8)
