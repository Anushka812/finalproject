#' random forest analysis
#'
#' this program takes input as data frame and performs random forest analysis on the data.
#'
#' @export
#' @import randomForest
#' @author Anushka Jain
#' @name randomforest
#' @title correlation
#' @param df data frame
#' @return plots

randforcalc <- function(df) {
  as.factor(df$diagnosis)
  round(prop.table(table(df$diagnosis)), 2)
  df_corr <- df %>% select(-id, -diagnosis, -X)
  set.seed(1)
  model <- randomForest(formula = df$radius_mean ~ ., data = df)
  return(model)
  plot(model)
  predict(model)
}
