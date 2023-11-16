multiRankSumTestWithCorrelation <- function(indices, statistics, inter.gene.cor = 0, df = Inf){
  #compare to limma::RankSumTestWithCorrelation
  #simply rearrange to allow for multiple tests at once to and compute ranks and ties only once instead of for each iteration
  nsets <- length(indices)
  n <- length(statistics)
  r <- rank(statistics)
  
  TIES <- (length(r) != length(unique(r)))
  if (TIES) {
    NTIES <- table(r)
    adjustment <- sum(NTIES * (NTIES + 1) * (NTIES - 1))/(n * (n + 1) * (n - 1))
  }
  
  pval.mat <- matrix(NA, nrow = nsets, ncol = 3)
  colnames(pval.mat) <- c("U", "less", "greater")
  for(i in seq_len(nsets)){
    correlation <- ifelse(length(inter.gene.cor) == 1, inter.gene.cor, inter.gene.cor[[i]])
    index <- indices[[i]]
    r1 <- r[index]
    n1 <- length(r1)
    n2 <- n - n1
    U <- n1 * n2 + n1 * (n1 + 1)/2 - sum(r1)
    mu <- n1 * n2/2
    if (correlation == 0 || n1 == 1) {
      sigma2 <- n1 * n2 * (n + 1)/12
    }
    else {
      sigma2 <- asin(1) * n1 * n2 + asin(0.5) * n1 * n2 * (n2 - 1) + 
        asin(correlation/2) * n1 * (n1 - 1) * n2 * (n2 - 1) + 
        asin((correlation + 1)/2) * n1 * (n1 - 1) * n2
      sigma2 <- sigma2/2/pi
    }
    if(TIES){
      sigma2 <- sigma2 * (1 - adjustment)
    }
    
    zlowertail <- (U + 0.5 - mu)/sqrt(sigma2)
    zuppertail <- (U - 0.5 - mu)/sqrt(sigma2)
    pvalues <- c(less = pt(zuppertail, df = df, lower.tail = FALSE), 
                 greater = pt(zlowertail, df = df))
    
    pval.mat[i, ] <- c(U=U, pvalues)
  }
  pval.mat
}

nr_cameraPR.default <- function(statistic,index,use.ranks=FALSE,inter.gene.cor=0.01,sort=TRUE,...)
  #chnaged source code of cameraPR to accomodate multiRankSumTestWithCorrelation
  
  #	Competitive gene set test allowing for correlation between genes: pre-ranked statistic.
  #	Gordon Smyth
  #	Created 18 April 2017.
{
  #	Issue warning if extra arguments found
  dots <- names(list(...))
  if (length(dots)) 
    warning("Extra arguments disregarded: ", sQuote(dots))
  #	Check statistic
  if (is.list(statistic)) 
    stop("statistic should be a numeric vector")
  storage.mode(statistic) <- "numeric"
  if (anyNA(statistic)) 
    stop("NA values for statistic not allowed")
  G <- length(statistic)
  ID <- names(statistic)
  if (G < 3) 
    stop("Two few genes in dataset: need at least 3")
  
  #	Check index
  if (!is.list(index)) 
    index <- list(set1 = index)
  nsets <- length(index)
  
  #	Check inter.gene.cor
  if (anyNA(inter.gene.cor)) 
    stop("NA inter.gene.cor not allowed")
  if (any(abs(inter.gene.cor) >= 1)) 
    stop("inter.gene.cor too large or small")
  if (length(inter.gene.cor) > 1L) {
    if (length(inter.gene.cor) != nsets) 
      stop("Length of inter.gene.cor doesn't match number of sets")
    fixed.cor <- FALSE
  }
  else {
    fixed.cor <- TRUE
    inter.gene.cor <- rep_len(inter.gene.cor, nsets)
  }
  
  #	Set df
  if (use.ranks) 
    df.camera <- Inf
  else df.camera <- G - 2L
  
  #	Global statistics
  meanStat <- mean(statistic)
  varStat <- var(statistic)
  
  if (is.character(index)){
    index <- lapply(index, function(iset){
      which(ID %in% iset)
    })
  }
  StatInSet <- lapply(index, function(iset){
    statistic[iset]
  })
  
  m <- sapply(StatInSet, length)
  NGenes <- m
  
  if (use.ranks) {
    pval.mat <- multiRankSumTestWithCorrelation(index, statistics = statistic, inter.gene.cor = inter.gene.cor, df = Inf)
    Down <- pval.mat[, "less"]
    Up <- pval.mat[, "greater"]
  } else {
    vif <- 1 + (m - 1) * inter.gene.cor
    m2 <- G - m
    meanStatInSet <- sapply(StatInSet, mean)
    delta <- G/m2 * (meanStatInSet - meanStat)
    varStatPooled <- ((G - 1L) * varStat - delta^2 * 
                        m * m2/G)/(G - 2L)
    two.sample.t <- delta/sqrt(varStatPooled * (vif/m + 
                                                  1/m2))
    Down <- pt(two.sample.t, df = df.camera)
    Up <- pt(two.sample.t, df = df.camera, lower.tail = FALSE)
  }
  
  #	Assemble into data.frame  
  TwoSided <- 2 * pmin(Down, Up)
  D <- (Down < Up)
  Direction <- rep_len("Up", nsets)
  Direction[D] <- "Down"
  if (fixed.cor) 
    tab <- data.frame(NGenes = NGenes, Direction = Direction, 
                      PValue = TwoSided, stringsAsFactors = FALSE)
  else tab <- data.frame(NGenes = NGenes, Correlation = inter.gene.cor, 
                         Direction = Direction, PValue = TwoSided, stringsAsFactors = FALSE)
  rownames(tab) <- names(index)
  
  #	Add FDR
  if (nsets > 1L) 
    tab$FDR <- p.adjust(tab$PValue, method = "BH")
  
  #	Sort by p-value
  if (sort && nsets > 1L) {
    o <- order(tab$PValue)
    tab <- tab[o, ]
  }
  tab
}
