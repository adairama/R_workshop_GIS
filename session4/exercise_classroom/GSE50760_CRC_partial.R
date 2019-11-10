###########
## Setup ##
###########

setwd("C:/Users/aramasamy/Desktop/R_workshop")  ## CHANGE THIS

pacman::p_load(tidyverse, readxl, janitor, 
               ggfortify, ggrepel, DESeq2, 
               EnhancedVolcano, ComplexHeatmap, genefilter)
rm(list=ls())


##################
## Read in data ##
##################

fn <- "data/GSE50760_data.xlsx"  ## CHANGE THIS IF NEEDED

## Read in pheno
levs <- c("normal", "primary", "metastasized")
pheno <- read_excel(fn, sheet="pheno") %>%
  mutate(type=factor(type, levels=levs)) %>%
  column_to_rownames("run_id")

## Read in expression data
raw_count <- read_excel(fn, sheet="counts") %>% 
  column_to_rownames("gene_id")

## Variance Stabilizing Transformation 
expr <- raw_count %>% 
  data.matrix() %>% 
  vst()

## What does VST do?
par(mfrow=c(1,2))
boxplot( raw_count[ , 1:5], main="Raw counts")
boxplot( expr[ , 1:5],      main="Normalized expression")


##########################
## PCA to find outliers ##
##########################

identical( rownames(pheno), colnames(expr) )

## Filter to top 10% most variable gene and transpose
input <- varFilter(expr, var.cutoff=0.9) %>% 
  t()

dim(input)

## PCA
pc <- prcomp(input, scale.=T)

summary(pc)
# Importance of components:
#                            PC1     PC2      PC3     PC4
# Standard deviation     33.8153 18.6338 14.76354 9.32578
# Proportion of Variance  0.4479  0.1360  0.08537 0.03407
# Cumulative Proportion   0.4479  0.5839  0.66927 0.70334

autoplot(pc, x=1, y=2, data=pheno, col="type", size=3,
         label=T, label.repel=T, main="PC1 vs PC2") +
  theme_bw()

autoplot(pc, x=2, y=3, data=pheno, col="type", size=3,
         label=T, label.repel=T, main="PC2 vs PC3") +
  theme_bw()

autoplot(pc, x=1, y=3, data=pheno, col="type", size=3,
         label=T, label.repel=T, main="PC1 vs PC3") +
  theme_bw()

rm(levs, expr, input, pc)


########################
## Remove the outlier ##
########################

identical( rownames(pheno), colnames(raw_count) )

toRemove <- c("SRR975588")  ## Samples to remove

## remove from expression data
expr <- raw_count %>%
  select(-toRemove) %>%
  data.matrix() %>%
  vst()

## remove from clinical data using position matching
toRemove <- which(pheno$sample_id %in% toRemove)
pheno <- pheno[ -toRemove, ]

## Check and cleanup
identical(rownames(pheno), colnames(expr))

rm(toRemove, raw_count)


# if all else fails:
#  Manually delete the rows/cols corresponding to the sample ID in Excel.
#  Save file with different name and read it in again.


##########################
## Aside: Volcano plots ##
##########################

## Read in Differential Gene Expression Results
dge <- read_excel(fn, sheet="dge") %>% data.frame()
rownames(dge) <- dge$gene_id

## How many genes were returned
nrow(dge)

## Look at the top 20 highest expressing genes. Does it make sense?
dge %>% 
  arrange(desc(baseMean)) %>% 
  select(symbol, baseMean) %>% 
  head(20)


## Volcano plots for metastatic signature ##
ymax <- -log10(min(dge$FDR_metvspri)) + 0.5

EnhancedVolcano(dge,
                lab   = dge$symbol,
                x     = "LFC_metvspri",
                y     = "FDR_metvspri",
                title = "Metastatic Signature",
                ylab  = "-log10 FDR",
                FCcutoff=log2(2), pCutoff=0.05, 
                ylim=c(0, ymax), caption=NULL, subtitle=NULL,
                legendPosition="none")


## Volcano plots for tumorigenesis signature ##
ymax <- -log10(min(dge$FDR_privsnorm)) + 0.5

EnhancedVolcano(dge,
                lab   = dge$symbol,
                x     = "LFC_privsnorm",
                y     = "FDR_privsnorm",
                title = "Tumorigenesis Signature",
                ylab  = "-log10 FDR",
                FCcutoff=log2(2), pCutoff=0.05, 
                ylim=c(0, ymax), caption=NULL, subtitle=NULL,
                legendPosition="none")

# Notes
# 1. Enhancedvolcano plot does not label all points unless you set `DrawConnectors=TRUE`
# 2. You can add in subtitle="" if you see "EnchancedVolcano" in your figure.
# 3. subtitle=NULL might give an error with older version of Enhancedvolcanosub

rm(ymax, fn)


########################
## Plot a single gene ##
########################

gene <- "IGFBP1"
ensg <- dge %>% filter(symbol==gene) %>% pull(gene_id) 
ensg

identical( rownames(pheno), colnames(expr) )
tmp <- data.frame( pheno, gene=expr[ensg, ] )
head(tmp) %>% select(-geo_accession)


### TYPE YOUR COMMANDS BELOW ###
### ??? ###




##############################
## Choose genes for heatmap ##
##############################

## How many genes are significantly up or down regulated for metastasis and tumorgenesis
## Save these as separate objects

### TYPE YOUR COMMANDS BELOW ###
### ??? ###




## Choose the 10 genes from each list (sort it by p-value)

### TYPE YOUR COMMANDS BELOW ###
### ??? ###




## Subset the expression data

### TYPE YOUR COMMANDS BELOW ###
### ??? ###



#######################
## Heatmap with iris ##
#######################

## Iris does not have rownames which is required by pheatmap for matching column/row annotations
rownames(iris) <- paste0("flower", 1:150)

## Transpose it. Rows represent variables (or genes). Column represents samples.
iris_num <- iris %>% select(-Species) %>% t()
dim(iris_num)

## Naive version
pheatmap(iris_num, main="Naive call to pheatmap()")

## Better version
ann_col <- iris %>% select(Species) 
head(ann_col)

ann_colors = list(
  Species = c(setosa="blue", 
              versicolor="black", 
              virginica="red")
)

pheatmap(iris_num, 
         scale="row",
         annotation_col    = ann_col,
         annotation_colors = ann_colors,
         show_colnames     = F,
         clustering_distance_rows="correlation",
         clustering_distance_cols="correlation",
         main = "Improved heatmap")

rm(iris_num)



###################################################
## Heatmap for top 40 genes from the CRC dataset ##
###################################################

### TYPE YOUR COMMANDS BELOW ###
### ??? ###

