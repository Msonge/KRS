#' ---
#' title: Effect of elephant damage, biotic & abiotic factors on the growth rate of trees in Serengeti
#' author: "Charles Msonge"
#' date: November 2019
#' file: "htl_document"
#' ---
#'
#' This script is about manipulating the elephant damage on tree dataset
#' inorder to see the effect of various attributes such as
#' fire, rainfall, site, elephant damage, etc on the growth rate of trees
#' in the serengeti savannah ecosystem.
#'
#' Setting up the script
#' =====================

# Clearing R's memory
rm(list = ls())

# Loading packages
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(MASS, warn.conflicts = FALSE)
library(lme4)

# importing the elephant damage tree dataset
elephant.damage <- read.csv("D:\\Autumn semester\\KRS\\final assignment\\elephant_damage_trees.csv")

#' Exploring the dataset
#' =====================
head(elephant.damage) #returns the first six rows of the dataframe
str(elephant.damage)  #returns the structure of the dataframe
names(elephant.damage) #returns the names of the variables (column names)

# changing the fire variable into a factor
elephant.damage$Fire <- as.factor(elephant.damage$Fire)
hist(elephant.damage$Ele_damage)

attach(elephant.damage) #attaches the dataframe in R studio
#' Explanatory Variable
#' ====================
# SITE <- Factor with 8 levels (categorical variable)
# SPECIES <- Factor with 17 levels (categorical variables)
# Elephant_damage <- discrete variable
# Fire <- Factor with 2 levels (categorical variable)
# Rain <- Factor with 2 levels (categorical variable)
# LivestockArea <- Factor with 2 levels (categorical variable)
# KmTovill <- continous variable

#' Response variable
#' =================
# DBH_growth_cm <- continous variable

#' Interactions
#' ============
# Elephant_damage * site
# Elephant_damage * rainfall
# Elephant_damage * fire
# Elephant_damage * species
# Elephant_damage * LivestockArea
# Elephant_damage * KmTovill

#' Random effects
#' ==============
# Site

#' plotting
#' ========
# a plot of tree growth rate against elephant damage
ggplot(elephant.damage, aes(x=Ele_damage, y=DBH_growth_cm)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  theme_bw()

# a plot of tree growth rate against species
ggplot(elephant.damage, aes(x=SPECIES, y=DBH_growth_cm)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# a plot of tree growth rate against fire
ggplot(elephant.damage, aes(x=Fire, y=DBH_growth_cm)) +
  geom_boxplot() +
  theme_bw()

# a plot of tree growth rate against rainfall
ggplot(elephant.damage, aes(x=RAIN, y=DBH_growth_cm)) +
  geom_boxplot() +
  theme_bw()

# a plot of tree growth rate against site
ggplot(elephant.damage, aes(x=SITE, y=DBH_growth_cm)) +
  geom_boxplot() +
  theme_bw()

# a plot of tree growth rate against livestock grazing
ggplot(elephant.damage, aes(x=LivestockArea, y=DBH_growth_cm)) +
  geom_boxplot() +
  theme_bw()
# a plot of tree growth rate against distance from the nearest village
ggplot(elephant.damage, aes(x=kmToVill, y=DBH_growth_cm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

#' Fitting models
#' ==============
#'firting the most complex model
model1<-lmer(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
               (1|SITE) + Ele_damage*RAIN + Ele_damage*Fire + Ele_damage*LivestockArea +
               Ele_damage*kmToVill)

model2 <- lmer(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
                 (1|SITE) + Ele_damage*Fire + Ele_damage*LivestockArea +
                 Ele_damage*kmToVill)


# likelihood ratio test
anova(model1, model2)

# the complex model(model1) is most likely to explain the variations in mortality of trees

model3 <- lmer(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
                 (1|SITE) + Ele_damage*RAIN + Ele_damage*Fire + Ele_damage*LivestockArea)

#likelihood ratio test
anova(model1,model3)

# the simplest model(model3) is most likely to explain the variations in mortality of trees

model4<- lmer(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
                (1|SITE) + Ele_damage*RAIN + Ele_damage*Fire)

#likelihood ratio test
anova(model3, model4)

# the simplest model(model4) is most likely to explain the variations in mortality of trees

model5 <- lmer(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
                 (1|SITE) + Ele_damage*RAIN)

#likelihood ratio test
anova(model4, model5)

# the simplest model(model5) is most likely to explain the variations in mortality of trees

model6<- lmer(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea +
                (1|SITE) + Ele_damage*RAIN)

#likelihood ratio test
anova(model5, model6)

# the simplest model(model6) is most likely to explain the variations in mortality of trees

model7<- lmer(DBH_growth_cm~Ele_damage + RAIN + Fire +
                (1|SITE) + Ele_damage*RAIN)

#likelihood ratio test
anova(model6, model7)

# the complex model(model6) is most likely to explain the variations in mortality of trees

model8<- lmer(DBH_growth_cm~Ele_damage + RAIN + LivestockArea +
                (1|SITE) + Ele_damage*RAIN)

#likelihood ratio test
anova(model6, model8)

# the complex model(model6) is most likely to explain the variations in mortality of trees
# model6 <- lmer(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + (1|SITE) + Ele_damage*RAIN)

