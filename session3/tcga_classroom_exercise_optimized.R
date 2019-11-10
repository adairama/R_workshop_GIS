###########
## Setup ## 
###########

setwd("C:/Users/aramasamy/Desktop/R_workshop")
pacman::p_load(tidyverse, janitor)
rm(list=ls())


#################################
## Cleanup the expression data ##
#################################

fn <- "data_tcga/brca_RNA_Seq_v2_expression_median_sel.txt"

expr <- read_delim(fn, delim="\t") %>% 
  column_to_rownames("Hugo_Symbol") %>% 
  t()

expr <- log2( expr + 1 )


###########################
## Read in clinical data ##
###########################

fn <- "data_tcga/brca_tcga_clinical_data.tsv"

pheno <- read_delim(fn, delim="\t") %>% 
  
  clean_names() %>% 
  
  rename(DFS=disease_free_status, 
         DFS_months=disease_free_months) %>% 
  
  filter(cancer_type=="Breast Cancer", 
         sex=="Female", 
         DFS_months > 0) %>% 
    
  mutate(event = ifelse(DFS=="DiseaseFree", 0, 1)) %>% 

  select(-patient_id, -DFS, -starts_with("overall"))
  

##########
## Join ##
##########

## Need to convert expr to data frame first
expr <- expr %>% 
  data.frame() %>% 
  rownames_to_column("sample_id")
  
comb <- inner_join(pheno, expr)
write.csv(comb, file="TCGA_breast_comb.csv", quote=F, row.names=F)
