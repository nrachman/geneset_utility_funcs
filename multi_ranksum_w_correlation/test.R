library(limma)
library(readr)

setwd("/Volumes/SG_DATA/")
#Source Scripts ----------------------------------------------------------
source("users/rachmaninoffn2/dev/R/multi_ranksum_w_correlation/faster_camera.R")
source("users/rachmaninoffn2/dev/R/multi_ranksum_w_correlation/make_indices.R")

#Set paths ---------------------------------------------------------------
#Inputs
TOPTAB.IN.PATH <- "PROJECTS/Nicaragua_Project/pilot_analysis/data/analysis_out/subject_anova/anova_lm/anova_res.tsv"
COMBINED.GENESETS.IN.PATH <- "PROJECTS/Monogenic_Project/Gene_sets/processed/combined_gene_sets.RDS"
ENSG.HGNC.PATH <- "PROJECTS/Nicaragua_Project/pilot_analysis/ref/ensg_to_hgnc.rds"

#Load data ---------------------------------------------------------------
toptab <- read_tsv(TOPTAB.IN.PATH)
genesetLL <- readRDS(COMBINED.GENESETS.IN.PATH)
ensg.hgnc.dat <- readRDS(ENSG.HGNC.PATH)

#Change rownames to gene names -------------------------------------------
toptab <- toptab[!is.na(toptab$gene.names) & toptab$gene.names != "", ]

#concatenate geneset list into single list
geneset.list <- genesetLL$go.bp

#Run enrichment -----------------------------------------------------------
universe <- toptab$gene.names
indices <- make_indices(geneset.list, universe, 5)

#wilcoxon, i.e. use.ranks = TRUE -----------------------------------------
tic <- Sys.time()
enrich.dat.camera <- cameraPR(toptab$`F value`, indices, use.ranks = TRUE, sort = TRUE, inter.gene.cor = .01)
toc <- Sys.time()
toc-tic

tic <- Sys.time()
enrich.dat.mine <- nr_cameraPR.default(toptab$`F value`, indices, use.ranks = TRUE, sort = TRUE, inter.gene.cor = .01)
toc <- Sys.time()
toc-tic

#print the number of results that are different between the two methods
print(sum(rownames(enrich.dat.camera) != rownames(enrich.dat.mine)))
for(nm in names(enrich.dat.camera)){
  print(nm)
  print(sum(enrich.dat.camera[[nm]] != enrich.dat.mine[[nm]]))
}


#Using parametric t-test based approach, i.e. use.ranks = FALSE ----------
tic <- Sys.time()
enrich.dat.camera <- cameraPR(toptab$`F value`, indices, use.ranks = FALSE, sort = TRUE, inter.gene.cor = .01)
toc <- Sys.time()
toc-tic

tic <- Sys.time()
enrich.dat.mine <- nr_cameraPR.default(toptab$`F value`, indices, use.ranks = FALSE, sort = TRUE, inter.gene.cor = .01)
toc <- Sys.time()
toc-tic

#print the number of results that are different between the two methods
print(sum(rownames(enrich.dat.camera) != rownames(enrich.dat.mine)))
for(nm in names(enrich.dat.camera)){
  print(nm)
  print(sum(enrich.dat.camera[[nm]] != enrich.dat.mine[[nm]]))
}


