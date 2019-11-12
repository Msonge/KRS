#' ---
#' title: effect of anthropogenic & abiotic factors and elephant damage on the growth rate of trees
#' author: "Charles Msonge"
#' date: November 2019
#' file: "htl_document"
#' ---
#'
#' this script is about manipulating the elephant damage tree dataset
#' inorder to see the effect of various explanatory variables such as
#' fire, rainfall, site, elephant damage, etc on the growth rate of trees
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

# importing the elephant damage tree dataset
elephant.damage <- read.csv("D:\\Autumn semester\\KRS\\final assignment\\elephant_damage_trees.csv")

#' Exploring the dataset
#' =====================
head(elephant.damage) #returns the first six rows of the dataframe
str(elephant.damage)  #returns the structure of the dataframe
names(elephant.damage) #returns the names of the variables (column names)

attach(elephant.damage) #attaches the dataframe in R studio

# changing the fire variable into a factor
elephant.damage$Fire <- as.factor(elephant.damage$Fire)

# TREE_ID <- Factor with 435 levels (categorical variable)

#' Explanatory Variable
#' ====================
# SITE <- Factor with 8 levels (categorical variable)
# SPECIES <- Factor with 17 levels (categorical variables)
# Elephant_damage <- discrete (covariate)
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
# Species

#' plotting
#' ========
# a plot of tree growth rate against elephant damage
ggplot(elephant.damage, aes(x=Ele_damage, y=DBH_growth_cm)) +
  geom_jitter(size=2, alpha=0.5, colour="blue") +
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
