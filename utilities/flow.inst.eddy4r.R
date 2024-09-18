##############################################################################################
#' @title Installing eddy4R incl. all dependencies

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Cove Sturtevant \email{csturtevant@battelleecology.org}

#' @description 
#' Workflow. Installation sequence for eddy4R.base, eddy4R.turb, eddy4R.erf, and eddy4R.qaqc incl. all of their dependencies.

#' @param Currently none

#' @return Currently none

#' @references Currently none.

#' @keywords eddy4R, installer, eddy-covariance, NEON QA/QC, turbulent flux, environmental response function

#' @examples Currently none

#' @seealso Currently none

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-11-29)
#     original creation
#   Cove Sturtevant (2016-01-11)
#     new repo download path to include updates to eddy4R.qaqc
#   Stefan Metzger (2016-04-10)
#     fixed repo download from NEONScience:NEON-FIU-algorithm/master
#   Stefan Metzger (2016-04-18)
#     prepare automated installation sequence that call be called from url
#   David Durden (2016-09-27)
#     update all Bioconductor packages
#   Stefan Metzger(2016-10-04)
#     update installation sequence, include REddyProc package
#   Stefan Metzger(2016-12-07)
#     update installation sequence, accomodate eddy4R.base and eddy4R.qaqc in subtree
#   Cove Sturtevant (2016-12-08)
#     added installation of som package
#   David Durden (2019-12-01)
#     Updated to match current NEON-FIU-algorithm installation script
#   David Durden (2023-03-15)
#     Updated for eddy4R.turb and Waves package move to public repo
#   David Durden (2023-09-22)
#     Updated to new Bioconductor and to install via github
##############################################################################################


# 1. install Bioconductor repository
base::print("Sourcing Bioconductor repository.")
#base::source("https://bioconductor.org/biocLite.R")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager", dependencies=TRUE, repos='http://cran.rstudio.com/')

base::print("Updated Bioconductor installation.")


# 3. install remotes package
utils::install.packages("remotes", dependencies=TRUE, repos='http://cran.rstudio.com/')
#utils::install.packages("rlang", dependencies=TRUE, repos='http://cran.rstudio.com/', type="source")

# 4. install eddy4R packages from Github
#BiocManager::install("EBImage")

# 4.2 specify Github repository NameRepo, branch NameRefe, and package directory NameDir
NameRepo <- "NEONScience/eddy4R"
NameRefe <- "deve"
NameDir <- c("pack/Waves", "pack/eddy4R.base", "pack/eddy4R.turb",  "pack/eddy4R.qaqc",  "pack/eddy4R.stor")

# 4.3 actual installation
# attention: dependency libraries needed for rhdf5 and EBImage, current linux versions included in docker build are: fftw3, hdf5-helpers, libhdf5-cpp-103, libhdf5-103, zliblg-dev, libsz2
# see https://github.com/NEONScience/NEON-FIU-algorithm/wiki/2.0-Install-R,-Rstudio-and-eddy4R
base::sapply(NameDir, function(idx) {
  remotes::install_github(repo=NameRepo,
                          ref=NameRefe,
                          subdir=idx,
                          dependencies=c(NA, TRUE)[2],
                          repos=c(BiocManager::repositories(),   # for dependencies on Bioconductor packages
                                  "https://cran.rstudio.com/")        # for dependencies on CRAN packages
                          
  )
})

# 5. install REddyProc
utils::install.packages(pkgs="REddyProc",
                        dependencies=c(NA, TRUE)[2],
                        repos=c("https://cran.rstudio.com/",              # for dependencies on CRAN packages
                                "http://R-Forge.R-project.org")           # for REddyProc on R-Forge
)
