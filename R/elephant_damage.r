#' Growth rate of trees in the serengeti savannah ecosystem
#'
#' A dataset containing information on elephant damage and other 8 attributes which have impact on the growth rate of trees in the serengeti savannah ecosystem
#'
#' @format A data frame with 435 rows and 9 variables:
#' \describe{
#'    \item{TREE_ID}{A unique identity given to each tree}
#'    \item{SITE}{Abbreviations of site names given to 8 sites in the study area}
#'    \item{SPECIES}{Taxonomical names given to the tree species which were measured in the study area}
#'    \item{Ele_damage}{Estimated percentage  of canopy loss, 0=no elephant damage while 10=total canopy loss (elephant damage)}
#'    \item{Fire}{Fire presence in the 1 year period, 0=no fire while 1=fire occured on the particular site}
#'    \item{RAIN}{Amount of rainfall in the sites, low or high}
#'    \item{LivestockArea}{Y=areas where livestock grazing is permitted while N=areas where grazing is restricted}
#'    \item{kmTovill}{Distance to the nearest village from site}
#'    \item{DBH_growth_cm}{growth rate measured as the difference in DBH (Diameter at breast height) in the 1 year study period}
#'
#'    }
#' @source \url{https://github.com/Msonge/KRS}
"elephant_damage"
