
num.clust <- function(df, pc.res, max.clust, pc.want, index){
  
  ### Put desired PCs into data frame and combine with geographic data
  pcs <- paste0("PC", pc.want)
  dat <- as.data.frame(pc.res$x[,pc.want])
  dat <- cbind(df, dat)
  dat <- dat[,c(pcs, "geometry")]
  
  ### Calculate neighborhood matrices
  queen_w <- queen_weights(dat) 
  
  ### Specific data values we want
  data <- dat[pcs] %>%
    st_drop_geometry()
  data.eval <- as.matrix(data)
  
  ### Create an empty dataframe to figure out ideal number of clusters
  eval <- data.frame(clusters=2:max.clust, RBTSSE=NA, Index=NA)
  for(i in 2:max.clust){
    ### Run REDCAP!!
    # Queen neighborhood
    cr_q <- redcap(i, queen_w, data, "fullorder-completelinkage")
    ### Evaluate REDCAP!
    eval$RBTSSE[i-1] <- cr_q$`The ratio of between to total sum of squares`
    eval$Index[i-1] <- unlist(intCriteria(data.eval, cr_q[["Clusters"]], crit=index))
    
  }
  
  RBTSSE <- ggplot(eval, aes(x=clusters, y=RBTSSE)) +
    geom_line() +
    theme_minimal() +
    labs(x='Clusters', y="Goodness of classification (RBTSSE)") 
  
  INDEX <- ggplot(eval, aes(x=clusters, y=Index)) +
    geom_line() +
    theme_minimal() +
    labs(x='Clusters', y=index)
  
  return(list(eval.df=eval, RBTSSE.plot=RBTSSE, INDEX.plot=INDEX))
  # return(list(eval.df=eval, RBTSSE.plot=RBTSSE))
}

# "S_Dbw"
# "Calinski_Harabasz"
# "Davies_Bouldin"

source("testing_n_clust.R")
test_sdw <- num.clust(dat, pc.res, 15, 1:2, "S_Dbw")
print(test_sdw$RBTSSE.plot)
print(test_sdw$INDEX.plot)


test_ch <- num.clust(counties.ppr, pc.res, 30, 1:2, "Calinski_Harabasz")
print(test_ch$RBTSSE.plot)
print(test_ch$INDEX.plot)


