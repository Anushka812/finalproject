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
  map_int(df, function(.x) sum(is.na(.x)))
  results <- prcomp(df%>% select(-id, -diagnosis, -X))
  biplot(results, scale = 0)
}
