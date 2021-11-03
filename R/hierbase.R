#' hierbase: Enabling Hierarchical Inference
#'
#' The hierbase package provides the functions to perform hierarchical inference.
#' The main workflow consists of two function calls.
#'
#' @section Functions:
#' The building of the hierarchical tree can be achieved by either of the functions
#' \code{\link{cluster_vars}} or \code{\link{cluster_positions}}.
#' The function \code{\link{advance_hierarchy}} (with 'build-in' group test functions) 
#' and \code{\link{run_hierarchy}} (with a user specified group test function) perform 
#' the hierarchical testing by going top down through the hierarchical tree and obviously 
#' require the hierarchical tree as an input.
#' 
#' @references 
#' Meinshausen, N. (2008). Hierarchical testing of variable importance. 
#' Biometrika, 95(2), 265-278.
#' Renaux, C., Buzdugan, L., Kalisch, M., and Bühlmann, P. (2020). Hierarchical inference for 
#' genome-wide association studies: a view on methodology with software. 
#' Computational Statistics, 35(1), 1-40.
#'
#' @docType package
#' @name hierbase
NULL
