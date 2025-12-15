# Package-private environment for cached objects
.timefitteR_env <- new.env(parent = emptyenv())

# internal: initialise cache keys (do not build heavy objects here)
.timefitteR_env$model_registry <- NULL

.get_model_registry <- function() {
  if (is.null(.timefitteR_env$model_registry)) {
    .timefitteR_env$model_registry <- .build_model_registry()
  }
  .timefitteR_env$model_registry
}
