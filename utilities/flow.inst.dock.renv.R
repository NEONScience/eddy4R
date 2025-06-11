##############################################################################################
#' @title Installing eddy4R into docker image incl. all dependencies

#' @author
#' David Durden \email{ddurden@battelleecology.org} \cr
#' Stefan Metzger \email{eddy4R.info@gmail.com}
#' Cove Sturtevant \email{csturtevant@battelleecology.org}

#' @description
#' Workflow. Installation sequence for eddy4R.base, eddy4R.turb, eddy4R.erf, and eddy4R.qaqc from local files incl. all of their dependencies into a docker image.

#' @param Currently none

#' @return Currently none

#' @references Currently none.

#' @keywords eddy4R, installer, eddy-covariance, NEON QA/QC, turbulent flux, environmental response function

#' @examples Currently none

#' @seealso Currently none

# changelog and author contributions / copyrights
#   David Durden (2017-09-06)
#     original creation, creating for building without pulling from Github
#   David Durden (2017-09-06)
#     bug fix for plotly package
#   David Durden (2018-10-30)
#     updating version of Noble package
#   David Durden (2018-12-09)
#     updating Noble package dependencies, include "rjson"
#   David Durden (2020-08-11)
#     updating Noble package dependencies, include "rjson"
#   Chris Florian (2022-03-21)
#     updating with renv fix to solve build issue for 1.3.0 release
#   Stefan Metzger (2022-11-10)
#     resolved merge conflict by updating renv statements from https://github.com/NEONScience/NEON-FIU-algorithm/blob/2db4f917d6e9afefbbae60cf2e549392794e21db/ext/shared/flow/flow.inst/flow.inst.dock.renv.R#L64 to https://github.com/NEONScience/NEON-FIU-algorithm/blob/452ab622a6a4b4efd11194c8210f5d9ecc97c301/ext/shared/flow/flow.inst/flow.inst.dock.renv.R#L62
#   David Durden (2023-03-15)
#     Updated for eddy4R.turb and Waves package move to public repo
##############################################################################################


# 1. install Bioconductor repository
# base::print("Sourcing Bioconductor repository.")
# #base::source("https://bioconductor.org/biocLite.R")
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager", dependencies=TRUE, repos='http://cran.rstudio.com/')
# 
# base::print("Updated Bioconductor installation.")
# 2. update packages # Commenting out the update of all packages

# 2.1 CRAN and R-Forge packages
# utils::update.packages(checkBuilt=TRUE,
#                        ask=FALSE,
#                        repos= c("https://cran.rstudio.com/",         # for CRAN packages
#                                 "http://R-Forge.R-project.org")      # for R-Forge packages
#                        )

# 2.2. Bioconductor packages
#base::print(paste0(Sys.time()," Installing BiocManager."))
#BiocInstaller::biocLite(ask = FALSE)
#BiocManager::install()

#Failsafe for ff package issue ##TODO: Need to fix this once CRAN issue resolved
#install.packages(c("ff","ffbase","bit"),repos="https://mran.microsoft.com/snapshot/2019-12-12", dependencies = TRUE)

# 3. install CRAN packages
packReq <-(c("devtools"))
 
base::print(paste0(Sys.time()," Check if required functions are installed."))
lapply(packReq, function(x) {
  print(x)
  if(require(x, character.only = TRUE) == FALSE) {
    install.packages(x, dependencies = TRUE)
  }})

#Installing certain version of renv
devtools::install_version("renv", version = "0.17.3", repos='http://cran.rstudio.com/')
 
#Renv info
devtools::package_info("renv")

#Provide renv consent to change files  
renv::consent(provided=TRUE)

# 4. install eddy4R packages from local clone

# 4.1 load and attach devtools package
#install.packages("rlang", repos = "https://cran.rstudio.com/")

# # 4.1.1 install rlogging function from NEON-IS-data-processing
# devtools::install_github(repo="NEONScience/NEON-IS-data-processing",
#                           ref="master",
#                           subdir="pack/NEONprocIS.base",
#                           dependencies=c(NA, TRUE)[2],
#                           repos=c(BiocManager::repositories(),   # for dependencies on Bioconductor packages
#                                   "https://cran.rstudio.com/",
#                                   "https://mran.microsoft.com/snapshot/2021-05-17")       # for CRAN
# )

# 4.2 specify base directory for repo clone NameDirRepo and for packages
NameDirRepo <- "/home/eddy/eddy4R"

#Dealing with bioconductor
install.packages("BiocManager")
BiocManager::install(version = "3.12")
renv::install("bioc::EBImage")

#Restore dependencies using renv
renv::restore(lockfile=paste0(NameDirRepo,"/renv.lock"))

#base::library(devtools)

NameDirPack <- c( "pack/eddy4R.base", "pack/eddy4R.turb", "pack/eddy4R.stor", "pack/eddy4R.qaqc","pack/Waves")
Dir <- paste(NameDirRepo, NameDirPack, sep = "/")

# 4.3 actual installation
# attention: works for private repository only if Sys.setenv(GITHUB_PAT = "MyAccessToken") is assigned
# see https://github.com/NEONScience/NEON-FIU-algorithm/wiki/2.0-Install-R,-Rstudio-and-eddy4R
base::print(paste0(Sys.time(), " Beginning installation of eddy4R packages."))
base::sapply(Dir, function(idx) {
  base::print(paste0(Sys.time(), " Installing package from ", idx, "."))
  
  #Install packages and dependencies from CRAN and bioconductor
  renv::install(packages = idx)
  
#   ,
#                     dependencies=c(NA, TRUE)[1],
#                     upgrade = FALSE,
#                     repos=c(BiocManager::repositories(),   # for dependencies on Bioconductor packages
#                             "https://cran.rstudio.com/",      # for CRAN
#                             "https://mran.microsoft.com/snapshot/2019-11-14")         # for robfilter dependency issue
#   )
#   
 })

# 4.4. download dependencies of the R-package "som"
# target directory
NameDirDld <- c("/home/eddy/depe")

# create target directory
base::dir.create(NameDirDld, recursive = TRUE)


# 5. install REddyProc
renv::install(packages = "REddyProc@1.2"#,
                        # dependencies=c(NA, TRUE)[2],
                        # repos=c("https://cran.rstudio.com/",              # for dependencies on CRAN packages
                        #         "http://R-Forge.R-project.org")           # for REddyProc on R-Forge
)
