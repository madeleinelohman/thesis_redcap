

num.clust <- function(df, pc.res, min.clust, max.clust, pc.want, index, bound_vals, 
                      bound_min, states2){
  
  ### Put desired PCs into data frame and combine with geographic data
  pcs <- paste0("PC", pc.want)
  dat <- as.data.frame(pc.res$x[,pc.want])
  dat <- cbind(df, dat)
  dat <- dat[,c(pcs, "geometry")]
  
  ### Calculate neighborhood matrices
  rook_w <- rook_weights(dat) 
  
  ### Specific data values we want
  data <- dat[pcs] %>%
    st_drop_geometry()
  data.eval <- cbind(states2, data)
  
  ### Create an empty dataframe to figure out ideal number of clusters
  eval <- data.frame(clusters=min.clust:max.clust, RS=NA, Index=NA)
  
  for(i in min.clust:max.clust){
    cr_q <- redcap(i, rook_w, data.eval, "fullorder-completelinkage", 
                   scale_method="raw",
                   bound_vals, min_bound)
    eval$RS[i-(min.clust-1)] <- cr_q$`The ratio of between to total sum of squares`
    eval$Index[i-(min.clust-1)] <- unlist(intCriteria(as.matrix(data.eval), as.vector(as.integer(cr_q$Clusters)), crit=index))
  }
  
  RS <- ggplot(eval, aes(x=clusters, y=RS)) +
    geom_line() +
    theme_classic() +
    labs(x='Clusters', y="Goodness of classification (RS)") 
  
  INDEX <- ggplot(eval, aes(x=clusters, y=Index)) +
    geom_line() +
    theme_classic() +
    labs(x='Clusters', y=index)
  
  best.want <- which.max(eval$Index == eval$Index[bestCriterion(eval$Index[!is.nan(eval$Index)], index)])
  
  n.clust.want <- eval[best.want, "clusters"]
  
  return(list(eval=eval, want=n.clust.want, rs.plot=RS,
              index.plot=INDEX))
}

# "S_Dbw"
# "Calinski_Harabasz"
# "Davies_Bouldin"

# test_ch <- num.clust(counties.ppr, pc.res, 5, 40, 1:3, "Davies_Bouldin")
# print(test_ch$rbsste.plot)
# print(test_ch$index.plot)
# test_ch$eval
# test_ch$want
