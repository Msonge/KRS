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
model1<-glm(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
              SITE + SPECIES + Ele_damage*SITE + Ele_damage*RAIN + Ele_damage*Fire +
              Ele_damage*SPECIES + Ele_damage*LivestockArea + Ele_damage*kmToVill,
            family = gaussian)

model2 <- glm(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
                SITE + SPECIES + Ele_damage*SITE + Ele_damage*RAIN + Ele_damage*Fire +
                Ele_damage*SPECIES + Ele_damage*LivestockArea, family = gaussian)

# likelihood ratio test
logLik(model1)
logLik(model2)
1-pchisq((2*(-408.2142--409.9742)), 1)

# the likelihood ratio (0.06063245) which is greater than 0.05 suggests that we should adopt the
# simpler model (model2)

model3 <- glm(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
                SITE + SPECIES + Ele_damage*SITE + Ele_damage*RAIN + Ele_damage*Fire +
                Ele_damage*SPECIES, family = gaussian)

# likelihood ratio test
logLik(model3)
1-pchisq((2*(-409.9742--409.9742)), 0)

# the likeihood ratio (1) which is greater than 0.05 suggests that we should adopt the
# simpler model (model3)

model4 <- glm(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
                SITE + SPECIES + Ele_damage*SITE + Ele_damage*RAIN + Ele_damage*Fire,
              family = gaussian)

# likelihood ratio test
logLik(model4)
1-pchisq((2*(-409.9742--419.7237)), (49-38))

# the likeihood ratio (0.05270364) which is greater than 0.05 suggests that we should adopt the
# simpler model (model4)

model5 <- glm(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
                SITE + SPECIES + Ele_damage*SITE + Ele_damage*RAIN,
              family = gaussian)

# likelihood ratio test
logLik(model5)
1-pchisq((2*(-419.7237--420.5298)), 1)

# the likeihood ratio (0.2041828) which is greater than 0.05 suggests that we should adopt the
# simpler model (model5)


model6 <- glm(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
                SITE + SPECIES + Ele_damage*SITE, family = gaussian)

# likelihood ratio test
logLik(model6)
1-pchisq((2*(-420.5298--420.5326)), 1)

# the likeihood ratio (0.9403475) which is greater than 0.05 suggests that we should adopt the
# simpler model (model6)


model7 <- glm(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
                SITE + SPECIES, family = gaussian)

# likelihood ratio test
logLik(model7)
1-pchisq((2*(-420.5326--431.114)), (36-29))

# the likeihood ratio (0.003536524) which is lesser than 0.05 suggests that we should adopt the
# complex model (model6)

model8<- glm(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
               SITE + Ele_damage*SITE, family = gaussian)

# likelihood ratio test
logLik(model8)
1-pchisq((2*(-420.5326--425.9039)),16)

# the likeihood ratio (0.8250876) which is greater than 0.05 suggests that we should adopt the
# simpler model (model8)

model9 <- glm(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea + kmToVill +
                Ele_damage*SITE, family = gaussian)


# likelihood ratio test
logLik(model9)
1-pchisq((2*(-425.9039--425.9039)), 0)

# the likeihood ratio (1) which is greater than 0.05 suggests that we should adopt the
# simpler model (model9)

model10<- glm(DBH_growth_cm~Ele_damage + RAIN + Fire + LivestockArea +
                Ele_damage*SITE, family = gaussian)

#likelihood ratio test
logLik(model10)
1-pchisq((2*(-425.9039--427.1463)), 1)

# the likeihood ratio (0.114951) which is greater than 0.05 suggests that we should adopt the
# simpler model (model10)

model11 <- glm(DBH_growth_cm~Ele_damage + RAIN + Fire +
                 Ele_damage*SITE, family = gaussian)


# likelihood ratio test
logLik(model11)
1-pchisq((2*(-427.1463--427.1463)), 0)

# the likeihood ratio (1) which is greater than 0.05 suggests that we should adopt the
# simpler model (model11)


model12 <- glm(DBH_growth_cm~Ele_damage + RAIN +
                 Ele_damage*SITE, family = gaussian)

# likelihood ratio test
logLik(model12)
1-pchisq((2*(-427.1463--454.4257)), 1)

# the likeihood ratio (1.508793e-13) which is lesser than 0.05 suggests that we should adopt the
# complex model (model11)

model13 <- glm(DBH_growth_cm~Ele_damage + Fire +
                 Ele_damage*SITE, family = gaussian)

# likelihood ratio test
logLik(model13)
1-pchisq((2*(-427.1463--427.935)), 1)

# the likeihood ratio (0.2091356) which is greater than 0.05 suggests that we should adopt the
# simpler model (model13)


#' Model of choice
#' ===============
#' After performing the likelihood test in these models without considering random effects
#' and fixed effects, I came to a preliminary conclusion that the best fit model to this point
#' is model13.
#'
#' model13 <- glm(DBH_growth_cm~Ele_damage + Fire +
#' Ele_damage*SITE, family = gaussian)
