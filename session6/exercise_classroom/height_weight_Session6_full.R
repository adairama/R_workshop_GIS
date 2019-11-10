#############################
## Setup and load packages ##
#############################

setwd("C:/Users/aramasamy/Desktop/R_workshop")  # change as needed
pacman::p_load(tidyverse, readxl, gridExtra, janitor, broom, EnvStats, skimr)
rm(list=ls())


#########################
## Read in and explore ##
#########################

## Convert height from inches to cm & weight from pounds to kg


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


## YOUR TASK 1: How is the Weight distributed by Gender
ggplot(hw, aes(x=Gender, y=Weight)) + geom_boxplot()
ggplot(hw, aes(x=Weight, fill=Gender)) + geom_density(alpha=0.5)

## Overall summary of weights
hw %>% 
  summarise(
    count = n(),
    mean_weight = mean(Weight), sd_weight = sd(Weight) )
    

## YOUR TASK 2: Calculate the mean and sd of weight for males and for females separately
hw %>% 
  group_by(Gender) %>%
  summarise(
    count = n(),
    mean_weight = mean(Weight), sd_weight = sd(Weight),
    mean_height = mean(Height), sd_height = sd(Height),
    cor_hw      = cor(Weight, Height)
  )


## Calculate t-test statistics by hand (assuming equal variance)
((61.6 - 84.8)/sqrt(8.63^2 + 8.97^2)) * sqrt(5000)


## t-test function()
hw %>% 
  t.test(Weight ~ Gender, data=.) %>% 
  tidy()


##################
## Correlations ##
##################

## YOUR TASK 3: Plot weight vs height
ggplot(hw, aes(x=Height, y=Weight, col=Gender)) + 
  geom_point(alpha=0.3) + 
  geom_smooth(method="lm") +
  labs(x="Height (cm)", y="Weight (kg)") +
  theme_bw()

# Correlation using base R grammar which is less flexible
cor( hw$Height, hw$Weight )             

# using tidyverse grammar
hw %>% summarise(cor(Height, Weight))


## YOUR TASK 4: Calculate correlation by gender
hw %>% 
  group_by(Gender) %>% 
  summarise(cor(Height, Weight))


#####################################################################
## Weight difference for males and females with 167 - 171cm height ##
#####################################################################

## YOUR TASK 5: Calculate the number of males and females in this bin and their weight average
hw %>% 
  filter(Height > 167, Height < 171) %>% 
  group_by(Gender) %>% 
  summarise(count = n(),
            mean_weight = mean(Weight))


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
fit <- lm( Weight ~ Height + Gender, data=hw )
fit %>% tidy()
#   term        estimate std.error statistic p.value
# 1 (Intercept)  -111.     1.04       -107.        0
# 2 Height          1.07   0.00643     166.        0
# 3 GenderMale      8.79   0.126        69.9       0


## YOUR TASK 7: What is the expected weight for a 150cm tall man?

# Linear regression equations:
#   Expected weight for male   = -111 + 1.07 x Height + 8.79



## (Optional) Generate a reference table ##
x <- seq(130, 200, by=10)

testM <- data.frame(Gender="Male", Height=x) 
testM$exp_weight_male <- predict(fit, testM)
testM <- testM %>% select(-Gender)

testF <- data.frame(Gender="Female", Height=x) 
testF$exp_weight_female <- predict(fit, testF)
testF <- testF %>% select(-Gender)

full_join(testM, testF) %>% round(1) %>% kable()
#
#   | Height | exp_weight_male | exp_weight_female |
#   |--------|-----------------|-------------------|
#   |  130   |      36.4       |       27.7        |
#   |  140   |      47.1       |       38.3        |
#   |  150   |      57.8       |       49.0        |
#   |  160   |      68.5       |       59.7        |
#   |  170   |      79.1       |       70.3        |
#   |  180   |      89.8       |       81.0        |
#   |  190   |      100.5      |       91.7        |
#   |  200   |      111.1      |       102.4       |


#################################
## Correlation in Iris dataset ##
#################################

data(iris)


## YOUR TASK 8: Calculate the correlation between Sepal Length and Sepal width for the whole dataset (i.e. ignoring Species info) and within each Species.
iris %>% 
  summarise(cor(Sepal.Length, Sepal.Width))

iris %>% 
  group_by(Species) %>% 
  summarise(cor(Sepal.Length, Sepal.Width))


## YOUR TASK 9: Plot the Sepal Length vs Sepal Width and color by Species
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) + 
  geom_point() + 
  geom_smooth(method="lm") + theme_bw()

