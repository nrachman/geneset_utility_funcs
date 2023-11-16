make_indices <- function(geneset.list, universe, min.geneset.size){
  # Get indices object that can be used for cameraPR
  
  # Arguments:
  #  geneset.list: a list containing character vectors of genesets
  #  universe: the background, all genes that should be used as background for enrichment,
  #    typically the rownames of the expressionset
  #  min.geneset.size: The smallest number of genes allowed in a geneset,
  #    genesets with fewer genes than min.geneset.size in the universe will be removed
  
  # Value
  #   Returns a list of vectors of indices that can be used by limma::cameraPR to compute 
  
  indices <- lapply(geneset.list, function(geneset){
    which(universe %in% geneset)
  })
  indices <- indices[sapply(indices, length) >= min.geneset.size]
  
  indices
}