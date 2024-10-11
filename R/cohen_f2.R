cohen_f2 <- function(baseModel, comparisonModel){
  stopifnot(is.list(baseModel), exists('model', where=baseModel))

  R2 <- summary(baseModel)$r.squared

  if(missing(comparisonModel)) {

    f_2 <- R2/(1 - R2)

  } else {

    stopifnot(is.list(comparisonModel), exists('model', where=comparisonModel))

    R2C <- summary(comparisonModel)$r.squared

    f_2 <- (R2C - R2)/(1 - R2C)
  }

  return(f_2)

}
