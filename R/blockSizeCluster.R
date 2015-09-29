blockSizeCluster <- function(x,var=8)
{
# uses blockSize to get an idea of blockSize
estim <- blockSize(x)$nrows[1]
at = 1
ind2 = 0
res <- list()
while (at <= nrow(x))
{
    ind2 <- ind2+1
    atin <- at
    size <- sample(max(1,(estim-var)):(estim+var),1) # min is 1 row
    at <- (at+size)
    if (at > nrow(x))
    {
        size <- nrow(x)-(atin-1)
    }
    
res[[ind2]] <- list(row=atin,nrows=size)
}
res <- matrix(unlist(res),byrow=T,ncol=2)
res <- list(row=res[,1],nrows=res[,2],n=nrow(res))
return(res)
}

