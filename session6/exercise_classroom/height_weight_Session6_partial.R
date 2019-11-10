#############################
## Setup and load packages ##
#############################

setwd("C:/Users/aramasamy/Desktop/R_workshop")  # change as needed
pacman::p_load(tidyverse, readxl, gridExtra, janitor, broom, EnvStats, skimr)
rm(list=ls())


#########################
## Read in and explore ##
#########################

## Read and convert units
hw <- read_excel("data/session1_data.xlsx", 
                 sheet="height_weight") %>% 
  
  mutate(Height=2.54*Height,    # Height from inches to cm
         Weight=Weight/2.205)   # Weight from pounds to kg


## Visualize
hw %>% skim()  
# Height ranges: 137.92 - 200.66cm
# Weight ranges:  29.34 - 122.45kg


############
## t-test ##
############


## YOUR TASK 1: Make a plot to show how the Weight distributed by Gender?


## Overall summary of weights
hw %>% 
  summarise(
    count = n(),
    mean_weight = mean(Weight), sd_weight = sd(Weight) )
    

## YOUR TASK 2: Calculate the mean and sd of weight for males and for females separately




## Calculate t-test statistics by hand (assuming equal variance)



## t-test function()
hw %>% 
  t.test(Weight ~ Gender, data=.) %>% 
  tidy()


##################
## Correlations ##
##################

## YOUR TASK 3: Plot weight vs height





# Correlation using base R grammar which is less flexible
cor( hw$Height, hw$Weight )             

# using tidyverse grammar
hw %>% summarise(cor(Height, Weight))


## YOUR TASK 4: Calculate correlation by gender



#####################################################################
## Weight difference for males and females with 167 - 171cm height ##
#####################################################################

## YOUR TASK 5: Calculate the number of males and females in this bin and their weight average




######################################
## Weight difference per height bin ##
######################################

## See the full script for solution to this section.

q5 <- quantile(hw$Height, seq(0, 1, by=0.2))
q5

hw <- hw %>% 
  mutate(Height_bin = cut(Height, q5, include.lowest=T))

hw %>% tabyl(Height_bin)  # check to see if roughly equal distribution

hw %>% 
  tabyl(Height_bin, Gender) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns("front")

hw %>% 
  group_by(Height_bin) %>%
  do(tidy( t.test(Weight ~ Gender, data=.))) %>% 
  select(Height_bin, average_female=estimate1, average_male=estimate2, 
         female_vs_male=estimate, p.value)


#######################
## Linear regression ##
#######################

## YOUR TASK 6: Add Gender into the linear model and summarize the model.




## YOUR TASK 7: What is the expected weight for a 150cm tall man?





## (Optional) Generate a reference table ##






#################################
## Correlation in Iris dataset ##
#################################

data(iris)


## YOUR TASK 8: Calculate the correlation between Sepal Length and Sepal width for the whole dataset (i.e. ignoring Species info) and within each Species.







## YOUR TASK 9: Plot the Sepal Length vs Sepal Width and color by Species





