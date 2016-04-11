library(igraph)
            
A1=matrix(c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
            1,0,0,0,1,1,0,0,0,1,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,
            0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,
            1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,
            0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
            1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,
            0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,1,1,0,0,0,0,1,0,0,0,0,0,0),
            nrow=15, ncol=15, byrow=TRUE);

g1=graph.adjacency(A1, mode=c("directed"), weighted=NULL)

authority_score(g1, scale = TRUE, weights = NULL, options = arpack_defaults)$vector
hub_score(g1, scale = TRUE, weights = NULL, options = arpack_defaults)$vector

A2=matrix(c(0, 1, 0, 0, 0,
            0, 0, 0, 1, 0,
            1, 1, 0, 0, 0,
            1, 0, 0, 0, 1,
            1, 0, 0, 0, 0),
            nrow=5, ncol=5, 
            byrow=TRUE);

g2=graph.adjacency(A2, mode=c("directed"), weighted=NULL)

authority_score(g2, scale = TRUE, weights = NULL, options = arpack_defaults)$vector
hub_score(g2, scale = TRUE, weights = NULL, options = arpack_defaults)$vector

A3=matrix(c(0, 1, 1, 0,
            0, 0, 1, 1,
            1, 0, 0, 1,
            0, 0, 1, 0),
          nrow=4, ncol=4, 
          byrow=TRUE);

g3=graph.adjacency(A3, mode=c("directed"), weighted=NULL)

authority_score(g3, scale = TRUE, weights = NULL, options = arpack_defaults)$vector
hub_score(g3, scale = TRUE, weights = NULL, options = arpack_defaults)$vector





