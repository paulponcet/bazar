#' @title 
#' Functions exported by a package
#' 
#' @description 
#' \code{get_all_funs} provides all the functions exported by a given 
#' installed package. 
#' 
#' @param pkg
#' character. The package of interest. (Must be installed already.)
#' 
#' @return 
#' A character vector, the functions exported. 
#' 
#' @export
#' 
#' @examples 
#' get_all_funs("stats")
#' 
get_all_funs <- 
function(pkg)
{
  if (pkg %in% c("Deducer", "rattle", "RGtk2")) {
    warning(paste0("get_all_funs() returns 'character(0)' for package ", pkg))
    return(character(0))
  }
  tryCatch(suppressWarnings(getNamespaceExports(pkg)), 
           error = function(e) character(0))
  #unclass(lsf.str(envir = asNamespace(pkg), all = TRUE))
}
