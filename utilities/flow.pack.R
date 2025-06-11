# following http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
# Run this code in sections to define the package name, create the package, and package/install it.
# The Defining section (1st) and Packaging and Installing section (3rd) can be run without the second
# section if the package already exists and you want to update it


#-----------------------------------------------------
# 1. Define working directory and name(s) of package(s)

# working directory
DirWrk00 <-
  #Docker default
  "/home/eddy/eddy4R"
  

  # Adam
  # "/home/younga1/eddy/code/NEON-FIU-algorithm-amyoung01"

  # Dave
  # "/home/ddurden/eddy/code/eddy4R_ddurden"
  #"/FIUdata/ddurden/eddy4R_ddurden"
  
  # Natchaya
  #"C:/Users/NDURDEN/Documents/Github/NEON-FIU-algorithm/NEONInc/0-NEONInc-default"
  #"~/eddy/docker/NEON-FIU-algorithm/0-Ndurden-DEFAULT"
  #"~/eddy/docker/NEON-FIU-algorithm-default"
  
  # Cove
  #"C:/Users/csturtevant/R/NEON-FIU-algorithm-covesturtevant"
  #"C:/Users/csturtevant/R/NEON-FIU-algorithm"
  #"/FIUdata/csturtevant/NEON-FIU-algorithm-covesturtevant"
  #"~/eddy/Github/Docker/NEON-FIU-algorithm/NEON-FIU-algorithm"
  #"~/eddy/Github/Docker/NEON-FIU-algorithm/NEON-FIU-algorithm-covesturtevant"
  #"/home/som/Github/Docker/NEON-FIU-algorithm/NEON-FIU-algorithm-covesturtevant"
  #"/scratch/SOM/Github/RstudioServer/NEON-FIU-algorithm/NEON-FIU-algorithm-covesturtevant"
  
  # Stefan
  # "D:/Arbeit/GitHub/NEON-FIU-algorithm/NEONScience/0-NEONScience-DEFAULT"
  # "D:/Arbeit/GitHub/NEON-FIU-algorithm/stefanmet/0-stefanmet-DEFAULT"
  # "~/eddy/Github/NEON-FIU-algorithm/NEONScience/0-NEONScience-DEFAULT"
  # "~/eddy/Github/NEON-FIU-algorithm/stefanmet/0-stefanmet-DEFAULT"
  
  # Hongyan 
  #"/FIUdata/hluo/NEON-FIU-algorithm-hongyan"
  
  # Ke
 #"~/eddy/NEON-FIU-algorithm-kexu2014"
  
  # Robert
  # "/scratch/SOM/Github/RstudioServer/NEON-FIU-algorithm/NEON-FIU-algorithm-rlee/"

  # Guy
  # "/scratch/SOM/Github/RstudioServer/NEON-FIU-algorithm/NEON-FIU-algorithm-glitt/"


#name(s) of package(s)
namePack <- c("eddy4R.base",
              "eddy4R.turb",
              "eddy4R.qaqc",
              "eddy4R.stor",
              "Waves"
              )[1:5]



###
# start loop around packages
for(idxNamePack in namePack) {
###



  # Install and load required packages
  
    # install libraries in case missing
    if(FALSE){withr::with_libpaths("C:/Program Files/R/R-3.2.3/library")} #Changed due to updates in devtools, change to TRUE and change path for custom library location
    if(!("devtools" %in% rownames(installed.packages()))) install.packages("devtools")
    if(!("roxygen2" %in% rownames(installed.packages()))) install.packages("roxygen2")

    # most current development version of roxygen2:
    # if(!("roxygen2" %in% rownames(installed.packages()))) devtools::install_github("klutometis/roxygen")
  
    # load libraries
    lapply(c("devtools","roxygen2"), library, character.only = TRUE)[[1]]

  
  
  
  #-----------------------------------------------------
  # 2. Create the Package
  
  # Set the working directory
  DirWrk <-  paste0(DirWrk00, "/pack")
  setwd(DirWrk)
  
  # Test if package name exists, if so don't re-create (just update) it
  dlist <- dir(path=".",pattern=idxNamePack,full.names=FALSE)
  
  # Create package (adjust name here)
  if(length(which(idxNamePack==dlist)) == 0) create(idxNamePack)
  
  
  
  #-----------------------------------------------------
  # 3. Package it up & Install it
  
  # 1. adjust the contents of the DESCRIPTION file
  # 2. add files...
  #     ... functions to package subdirectory /R
  #     ... demo files to package subdirectory /demo
  #     ... vignette files to package subdirectory /vignettes
  # 3. Add comments to the beginning of each function with the format: #'  -- These
  #     will be used to create help documentation using the roxygen2 package 
  
  # change working directory to package directory
  setwd(paste0(DirWrk, "/", idxNamePack))
  
  # If the data-raw folder does not exist, create it. This is where the flow scripts to
  # create package data should be kept. This folder will be included in the .Rbuildignore
  # file of the package
  dlist <- dir(path=".",pattern="data-raw",full.names=FALSE)
  if(length(dlist) == 0) {
    
    # Create data-raw folder and add it to .Rbuidignore (default when using use_data_raw function)
    usethis::use_data_raw()
    
  } else {
    
    # If data-raw folder exists already, make sure it is included in .Rbuildignore
    usethis::use_build_ignore("data-raw", escape = TRUE)
  }
  
  # install dependent packages
  # devtools::document() requires all dependent packages to be installed
  # in case during development new dependent packages have been added to the \code{DESCRIPTION} file but are not yet installed,
  # devtools::document() stops with an error
  # calling eddy4R.base::def.inst.depe() prior to devtools::document() mitigates this error
  if(idxNamePack == "eddy4R.base") {
    
    #TODO: Currently, the if statement for eddy4R.base allow it to be installed in order to access the eddy4R.base::def.inst.depe() function. Finding a solution that doesn't require the if statement should be explored.
    
    # in case of eddy4R.base, resolve dependencies through pre-installing the package once without updated documentation
    
      # remove any existing version of package in the library location
      # commented out because throwing errors
      # remove.packages(idxNamePack, lib=DirLib)
    
    # Create Roxygen documentation: added to prevent namespace errors when changing function names
      devtools::document()
      
      # install the package
      setwd("..")
      
      #Install packages
      devtools::install(idxNamePack,
              dependencies=c(NA, TRUE)[2],
              upgrade = FALSE,
              repos=c(BiocManager::repositories(),   # for dependencies on Bioconductor packages
                      "https://cran.rstudio.com/",         # for dependencies on CRAN packages
                      "http://R-Forge.R-project.org",      # for REddyProc on R-Forge
                      "https://mran.microsoft.com/snapshot/2019-11-14") #TODO: robfilter temporary fix needs to be removed
      )
      # https://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
      # detach(paste0("package:", idxNamePack), unload=TRUE, character.only = TRUE)
      base::unloadNamespace(ns = idxNamePack)
      library(paste0(idxNamePack), character.only = TRUE)
      setwd(paste0(DirWrk, "/", idxNamePack))
    
  } else {
  
    # once eddy4R.base is installed, calling eddy4R.base::def.inst.depe() is faster for subsequent packages
    eddy4R.base::def.inst.depe()  
    
  }

  # Create Roxygen documentation 
  devtools::document()
  
  # remove any existing version of package in the library location
  remove.packages(idxNamePack)
  
  # Install the package
  setwd("..")
  install(idxNamePack,
          dependencies=FALSE
          )
  # https://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
  # detach(paste0("package:", idxNamePack), unload=TRUE, character.only = TRUE)
  base::unloadNamespace(ns = idxNamePack)
  library(paste0(idxNamePack), character.only = TRUE)



###
}

# end loop around packages
###



#test package by calling some help files that should have been generated
# ?def.cart.az
# ?fd_FootFluxKm01
# ?ersp
