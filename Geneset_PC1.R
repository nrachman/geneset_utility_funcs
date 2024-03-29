make_indices <- function(geneset.list, universe, min.geneset.size = 5){
  indices <- lapply(geneset.list, function(geneset){
    which(universe %in% geneset)
  })
  indices <- indices[sapply(indices, length) >= min.geneset.size]
  
  indices
}

genesets_pc1 <- function(X, indices){
  # X is a samples X features matrix with gene names as the column names
  # indices is a named list with gene set indices from make_indices
  
  #returns a sample X geneset matrix with geneset PC1 scores in the columns and other diagnostics
  
  pc1.mat <- matrix(ncol = length(indices), nrow = nrow(X))
  colnames(pc1.mat) <- names(indices)
  rownames(pc1.mat) <- rownames(X) #Sample Names
  
  pc1.vexp <- vector(length = length(indices))
  names(pc1.vexp) <- names(indices)
  
  pc1.mean.cor <- vector(length = length(indices))
  names(pc1.mean.cor) <- names(indices)
  
  for(i in seq_along(indices)){
    keep.cols <- indices[[i]]
    X.sub <- X[, keep.cols]

    prcomp.obj <- prcomp(X.sub, scale. = TRUE, center = TRUE)
    pc1.vec <- prcomp.obj[["x"]][, "PC1"]
    stopifnot(all(rownames(pc1.mat) == names(pc1.vec)))
    
    #flip if not correlated with the genes themselves
    pc1.direction <- mean(cor(pc1.vec, X.sub))
    if(pc1.direction <= 0){
      pc1.vec <- -pc1.vec
    }
    
    pc1.mat[, i] <- pc1.vec
    
    pc.vexp <- prcomp.obj$sdev ^ 2 / sum(prcomp.obj$sdev ^ 2)
    pc1.vexp[[i]] <- pc.vexp[[1]]
    
    pc1.mean.cor[[i]] <- abs(pc1.direction)
  }
  
  
  out <- list(pc1.mat=pc1.mat, pc1.vexp=pc1.vexp, pc1.mean.cor=pc1.mean.cor)
  return(out)
}
