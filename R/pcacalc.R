#' pc analysis
#'
#' this program takes input as data frame and performs principal component analysis on the data.
#'
#' @export
#' @import tidyverse
#' @author Anushka Jain
#' @name pca
#' @title pca
#' @param df data frame
#' @return plots and data frames

pcacalc <- function(df) {
  results <- prcomp(df%>% select(-id, -diagnosis, -X), scale=true)
  biplot(results, scale = 0)
}
