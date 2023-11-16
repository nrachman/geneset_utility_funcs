#Ripped from tmod::tmodHGtest()
#With some modifications to work with custom genesets and return overlap

module_phyper <- function (fg, bg, modules = NULL, qval = 0.05, order.by = "pval", 
    filter = FALSE, nodups = TRUE) 
{
    mset <- .getmodules2(modules, mset)
    fg <- as.character(fg)
    bg <- as.character(bg)
    if (nodups) {
        fg <- unique(fg)
        bg <- unique(bg)
    }
    bg <- setdiff(bg, fg)
    if (length(bg) == 0) 
        stop("Insufficient bg")
    if (sum(bg %in% mset$GENES$ID) == 0) {
        warning("No genes in bg match any of the genes in the GENES")
    }
    if (sum(fg %in% mset$GENES$ID) == 0) {
        warning("No genes in fg match any of the genes in the GENES")
        return(NULL)
    }
    if (filter) {
        fg <- fg[fg %in% mset$GENES$ID]
        bg <- bg[bg %in% mset$GENES$ID]
    }
    tot <- unique(c(fg, bg))
    n <- length(tot)
    k <- length(fg)
    mod.test <- function(m) {
        mg <- mset$MODULES2GENES[[m]]
        q <- sum(fg %in% mg)
        m <- sum(tot %in% mg)
        if (m == 0) {
            E <- NA
        }
        else {
            E <- (q/k)/(m/n)
        }
        if (q == 0 || m == 0) 
            return(c(b = q, B = m, n = k, N = n, E = E, P.Value = 1))
        pv <- phyper(q - 1, m, n - m, k, lower.tail = FALSE)
        c(b = q, B = m, n = k, N = n, E = E, P.Value = pv)
    }
    ret <- .tmodTest(mod.test, NULL, qval = qval, order.by = order.by, 
        mset = mset, cols = cols)
    ret
}
