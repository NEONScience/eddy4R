##############################################################################################
#' @title Definition function: Install dependent packages

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function defintion. Installs dependent packages from a source package \code{DESCRIPTION} file, without installing the source package itself. Useful e.g. for re-packing source packages with Roxygen, because devtools::document() requires all dependent packages to be installed. In case during development new dependent packages have been added to the \code{DESCRIPTION} file but are not yet installed, devtools::document() stops with an error. Calling eddy4R.base::def.inst.depe() prior to devtools::document() mitigates this error.

#' @param DirPack Path to the package \code{DESCRIPTION} file, defaults to the current working directory. Character vector with single entry [-].
#' @param Depe Defines what type of dependencies are installed. Defaults to \code{TRUE} which includes all of \code{c("Depends", "Imports", "LinkingTo")}. \code{c("Depends", "Imports", "LinkingTo")} can also be specified individually. Logical vector with single entry or character vector [-].
#' @param Repo The base URL(s) of the repositories to use, e.g., the URL of a CRAN mirror such as "https://cloud.r-project.org". Defaults to \code{base::getOption("repos")[1]}. Character vector [-].
#' @param Lib Path to the libraries in which the dependent packages are installed. Character vector [-].

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' \url{https://gist.github.com/jtilly/ac1b028f3666ce1d82c2}. \cr

#' @keywords dependencies, devtools, document, error, package, Roxygen

#' @examples
#' eddy4R.base::def.inst.depe()

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Jan Tilly (2016-12-12)
#     original creation
#   Stefan Metzger (2018-02-17)
#     adjusted to eddy4R syntax
##############################################################################################


def.inst.depe = function(
  DirPack = ".",
  Depe = TRUE,
  Repo = base::getOption("repos")[1],
  Lib = .libPaths()[1]) {
  
  # default dependencies
  if(Depe == TRUE) {
    Depe = c("Depends", "Imports", "LinkingTo")
  }
  
  # returns string w/o leading or trailing whitespace
  trim = function (x) base::gsub("^\\s+|\\s+$", "", x)
  
  # http://stackoverflow.com/questions/30223957/elegantly-extract-r-package-dependencies-of-a-package-not-listed-on-cran
  dscr = base::read.dcf(file.path(DirPack, "DESCRIPTION"))
  depe = base::unique(base::gsub("\\s.*", "", trim(base::unlist(
                                   base::strsplit(dscr[, base::intersect(Depe, base::colnames(dscr))], ","),
                                   use.names = FALSE))))
  
  # install dependencies that aren't already installed
  depe = depe[!(depe %in% c("R", base::rownames(utils::installed.packages())))]
  if(base::length(depe) > 0) {
    base::message(base::paste("Also installing:", paste(depe)))
    utils::install.packages(pkgs = depe, lib = Lib, repos = Repo)
  }
}
