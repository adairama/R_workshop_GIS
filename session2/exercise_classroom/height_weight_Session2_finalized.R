#############################
## Setup and load packages ##
#############################

setwd("C:/Users/aramasamy/Desktop/R_workshop")  # change as needed
pacman::p_load(tidyverse, readxl, gridExtra, janitor, EnvStats)
rm(list=ls())

hw <- read_excel("data/session1_data.xlsx", sheet="height_weight")


###############################
## Exploratory data analysis ##
###############################

g.h <- ggplot(hw, aes(x=Height, fill=Gender)) + 
  geom_density(alpha=0.4) +
  labs(x="", y="", title="\nHeight", tag="A.") 

g.w <- ggplot(hw, aes(x=Weight, fill=Gender)) + 
  geom_density(alpha=0.4) +
  labs(x="", y="", title="\nWeight", tag="B.") 

g.hw <- ggplot(hw, aes(x=Height, y=Weight, col=Gender)) + 
  geom_point(alpha=0.1) + 
  geom_smooth(method="lm") +
  labs(x="Height", y="Weight", tag="C.", col=NULL) 

grid.arrange(g.h  + theme_bw() + theme(legend.position="none"),
             g.w  + theme_bw() + theme(legend.position="none"), 
             g.hw + theme_bw() + theme(legend.position="top"), 
             nrow=1,
             top="Height and weight for 10,000 people")
rm(g.h, g.w, g.hw)


##################
## Data cleanup ##
##################

hw <- hw %>% 
  
  mutate(Height_cm = 2.54*Height,
         Weight_kg = Weight / 2.205,
         BMI       = Weight_kg / (Height_cm/100)^2) %>% 
  
  rename(Height_inches = Height, 
         Weight_pounds = Weight)
  
ggplot(hw, aes(x=BMI, fill=Gender)) + 
  geom_density(alpha=0.4) +
  labs(x="", y="", title="BMI") +
  geom_vline(xintercept=c(18.5, 25, 30), lty=3)


## Add BMI class according to WHO definition
mylabs   <- c("underweight", "normal weight", "overweight", "obese")
mybreaks <- c(-Inf, 18.5, 25, 30, Inf)

hw <- hw %>% 
  mutate(BMI_class = cut(BMI, mybreaks, labels=mylabs))


##############################################
## Cross-table between gender and BMI class ##
##############################################

## Simple table
tb <- hw %>% 
  tabyl(Gender, BMI_class)
tb     


## Prettier version
tb %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns("front") %>% 
  knitr::kable(format = "markdown")

# |Gender |underweight |normal weight |overweight   |obese      |
# |:------|:-----------|:-------------|:------------|:----------|
# |Female |42 (0.8%)   |3890 (77.8%)  |1068 (21.4%) |0 (0.0%)   |
# |Male   |0 (0.0%)    |228  (4.6%)   |4521 (90.4%) |251 (5.0%) |

## All the obese people are men? This data seems to be an unfair representation!!!


#########################################
## Calculate the mean and SD by Gender ##
#########################################

## The hard way
hw %>% 
  filter(Gender=="Male") %>% 
  summarise(mean(BMI), sd(BMI))

hw %>% 
  filter(Gender=="Female") %>% 
  summarise(mean(BMI), sd(BMI))


## The easier way
hw %>% 
  group_by(Gender) %>% 
  summarise( BMI_mean = mean(BMI), BMI_SD = sd(BMI))
#   Gender BMI_mean BMI_SD
# 1 Female     23.4   1.95
# 2 Male       27.5   1.51
