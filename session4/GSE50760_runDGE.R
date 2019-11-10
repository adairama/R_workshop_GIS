###########
## Setup ##
###########

setwd("C:/HCP Anywhere/Mywork/Workshop/Heatmap_MDS/")
pacman::p_load(tidyverse, readxl, broom, janitor, DESeq2)
rm(list=ls())


##################
## Read in data ##
##################

## Read in expression data
raw_count <- read_excel("GSE50760_data.xlsx", sheet="counts") %>% 
  column_to_rownames("gene_id")

hist(colSums(raw_count))  # From 2.4 to 7.3 million reads


## Read in pheno
pheno <- read_excel("GSE50760_data.xlsx", sheet="pheno") %>% 
  mutate(type    = factor(type,levels=c("normal", "primary", "metastasized")),
         subject = factor(subject)) %>% 
  column_to_rownames("sample_id")

pheno %>% tabyl(subject, type)


##Read in gene information
gene.info <- read_excel("GSE50760_data.xlsx", sheet="gene_info") %>% 
  column_to_rownames("gene_id")


##################################
## Generate DESeqDataSet Object ##
##################################

dds <- DESeqDataSetFromMatrix( countData = raw_count,
                               colData   = pheno,
                               rowData   = gene.info,
                               design    = ~ type + subject)


#####################
## Remove outliers ##
#####################

## SRR975588 was a clear outlier on the PCA plot. Possibly others exist too.
toDelSample <- which(colnames(dds) == "SRR975588")
dds <- dds[ , -toDelSample]
dim(dds)


###########################################
## Differential Gene Expression Analysis ##
###########################################

dds_seq <- DESeq(dds)  ## About 5min
resultsNames(dds_seq)

## Extract results for metastasized vs. primary
res.metvspri <- results(dds_seq, contrast=c("type", "metastasized", "primary")) %>% 
  data.frame() %>% 
  rownames_to_column("gene_id") %>% 
  mutate(baseMean = round(baseMean, 1),
         log2FoldChange = round(log2FoldChange, 2), 
         pvalue = formatC(pvalue, format="e", digits=1), 
         padj   = formatC(padj,   format="e", digits=1)) %>% 
  select(gene_id, baseMean, LFC_metvspri=log2FoldChange, P_metvspri=pvalue, FDR_metvspri=padj)

## Extract results for primary vs. normal
res.privsnorm <- results(dds_seq, contrast = c("type", "primary", "normal")) %>% 
  data.frame() %>% 
  rownames_to_column("gene_id") %>% 
  mutate(log2FoldChange = round(log2FoldChange,2), 
         pvalue = formatC(pvalue, format="e", digits=1), 
         padj   = formatC(padj,   format="e", digits=1)) %>% 
  select(gene_id, LFC_privsnorm=log2FoldChange, P_privsnorm=pvalue, FDR_privsnorm=padj)

dge <- plyr::join_all(list(gene.info %>% rownames_to_column("gene_id"),
                           res.metvspri, res.privsnorm)) %>% 
  select(-bp_length) %>% 
  column_to_rownames("gene_id")  

write.csv(dge, "DGE_results.csv")
