outlierValue <- function(x){ return( 3 * IQR(x) )}

reLabel <- function(cluster){
  
  cluster = ifelse(cluster==1, "prefer open",
                   ifelse(cluster==2, "weak preference",
                          ifelse(cluster==3, "prefer forest",
                                 ifelse(cluster==4, "prefer intermediate",cluster))))
  
  cluster = factor(cluster, 
                   levels=c("prefer forest","prefer intermediate","weak preference",
                            "prefer open"))
  
  return(cluster)
  
}