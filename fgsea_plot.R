library(fgsea)
library(ggplot2)
library(cowplot)

addTitlePlotGseaTable <- function(fgseaRes, main = "", pathways = "top", nPathways = 20){
  if(pathways[1] == "top"){
    topPathwaysUp <- fgseaRes[ES > 0][head(rev(order(NES)), n=nPathways), pathway]
    topPathwaysDown <- fgseaRes[ES < 0][head(order(NES), n=nPathways), pathway]
    topPathways <- c(topPathwaysUp, rev(topPathwaysDown))
    p <- plotGseaTable(examplePathways[topPathways], exampleRanks, fgseaRes, 
              gseaParam=0.5, render = FALSE)
  
  }else{
  }



  p_with_title <- plot_grid(ggplot() + 
                            labs(title = main), p, ncol = 1, rel_heights = c(.05, 1)) 
}

PdfPlotGseaTable <- function(fgseaRes, main = "", nPathways = 20, 
                             figPath = "gseaPlot.pdf",
                             height = 10, width = 14){

  pdf(figPath, height = height, width = width)
  p <- addTitlePlotGseaTable(fgseaRes, main, nPathways)
  print(p)
  dev.off()
}

data(examplePathways)
data(exampleRanks)

set.seed(42)

fgseaRes <- fgsea(pathways = examplePathways, 
                  stats    = exampleRanks,
                  minSize  = 15,
                  maxSize  = 500, 
                  nperm = 1000)


pdf("~/sg_data/users/rachmaninoffn2/scratch/fgsea_test.pdf")
print(p_with_title)
dev.off()


