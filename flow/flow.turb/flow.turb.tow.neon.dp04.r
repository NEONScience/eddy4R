# TEST VERSION 1
##############################################################################################
#' @title Workflow for processing NEON eddy-covariance turbulence data to dp04 fluxes with eddy4R

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' David Durden \email{eddy4R.info@gmail.com}

#' @description
#' Workflow. Processing NEON eddy-covariance turbulence data to dp04 fluxes using the eddy4R family of R-packages and their dependencies. Based on "flow.turb.tow.neon.r" SHA 813e8bff (2017-07-10).

#' @param Currently none

#' @return Currently none

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr
#' Metzger, S., Durden, D., Sturtevant, C., Luo, H., Pingintha-Durden, N., Sachs, T., Serafimovich, A., Hartmann, J., Li, J., Xu, K., and Desai, A. R.: eddy4R: A community-extensible processing, analysis and modeling framework for eddy-covariance data based on R, Git, Docker and HDF5, Geosci. Model Dev. Discuss., 2017, 1-26, doi:10.5194/gmd-2016-318, 2017.

#' @keywords eddy-covariance, NEON, CPER, turbulent flux

#' @examples Currently none

#' @seealso Currently none

# changelog and author contributions / copyrights
#   Stefan Metzger (2017-07-25)
#     original creation
#   Stefan Metzger (2017-08-01)
#     adjust default behaviour to downloading / working with gold file -> permits inclusion in test worfklow
#   David Durden (2017-09-10)
#     Adding switches for writing dp04 output
#   Stefan Metzger (2017-09-16)
#     control multi-day dp0p read-in through workflow parameters (PrdWndwCalc, PrdIncrCalc)
#     control multi-day planar-fit through workflow parameters (PrdWndwPfDcmp, PrdIncrPfDcmp)
#   David Durden (2017-09-25)
#     Updating to new ENV variables
#   Stefan Metzger (2017-09-30)
#     re-enable combining files from multiple days: Docker froze as ffdfappend() fails on some ffdf
#     activate planar-fit determination (on multi-day period) incl. filter for sensor flags
#   Stefan Metzger (2017-10-01)
#     create initial structure for "default mode", "environmental variable mode" and "user selection mode"
#   Stefan Metzger (2017-10-02)
#     transition to new definition of workflow parameters (eddy4R.base:def.para.flow.ecte.R)
#   Stefan Metzger (2017-10-03)
#     cleanup after transition
#   Stefan Metzger (2017-10-03)
#     completed transition to moving planar-fit window
#   Stefan Metzger (2017-10-15)
#     MVP candidate for Wavelet-based high-frequency correction
#   Stefan Metzger (2017-10-21)
#     add footprint model
#   David Durden (2018-01-12)
#     Updating to new naming convention and adding footprint output to dp04 HDF5 output
#   Stefan Metzger (2018-01-20)
#     hardening and testing of "host" mode (environment variables)
#   Stefan Metzger (2018-02-24)
#     updating Docker run command line examples
#   Natchaya P-Durden (2018-03-30)
#     applied term name convention; replaced levl by lvl
#   Natchaya P-Durden (2018-04-12)
#    applied eddy4R term name convention; replaced pos by set
#   Ke Xu (2018-04-19)
#     applied term name convention; replaced dataIn by dataInp
#   Natchaya P-Durden (2018-05-11)
#     rename function from def.agr.ecte.dp01() to def.dp01.agr.ecte()
#   Natchaya P-Durden (2018-05-22)
#     rename function from def.neon.read.hdf5.para() to def.hdf5.read.para()
#     rename function from def.neon.read.hdf5.qfqm() to def.hdf5.read.qfqm()
#     rename function from wrap.neon.dp01() to wrap.dp01()
#     rename function from wrap.neon.dp01.agr.prd() to wrap.dp01.agr.prd()
#     rename function from wrap.neon.read.hdf5.eddy() to wrap.hdf5.read()
#   Natchaya P-Durden (2018-05-23)
#     rename function from wrap.neon.dp01.qfqm.ec() to wrap.dp01.qfqm.ecte()
#   David Durden (2018-09-28)
#     adding failsafe for freqSamp metadata missing or set to NA
#   David Durden (2018-10-22)
#     adding failsafe for freqSamp metadata missing or set to NA and "NA" as character
#   Natchaya P-Durden (2018-11-07)
#     adding read-in the values of gasRefe from HDF5
#   Natchaya P-Durden (2018-12-04)
#     adding read-in the standard deviation values of gasRefe
#   Natchaya P-Durden (2019-01-03)
#     adding correction values of rtioMoleDryCo2 into dp01 processing
#   Natchaya P-Durden (2019-01-29)
#     adding dp04 processing for irga correction sub dataproducts
#   Natchaya P-Durden (2019-02-14)
#     adding degree of freedom of gasRefe in Para$Cal
#   David DUrden (2019-02-16)
#      modifying failsafe for planar fit to ensure greater than 2 non-NA data entries exist in wind vectors (required by lm.fit function)
#   David Durden (2019-02-16)
#      modifying failsafe for planar fit to ensure greater than 2 non-NA data entries exist in wind vectors (required by lm.fit function)
#   David Durden (2019-02-16)
#      modifying failsafe for planar fit to ensure greater than 2 non-NA data entries exist in a combination of all wind vectors [not enough to test individually] (required by lm.fit function)
#   Natchaya P-Durden (2019-02-19)
#     output scale from MLFR
#     replace rtioMoleDryCo2RefeSd in Para$Cal with rtioMoleDryCo2RefeSe
#   Natchaya P-Durden (2019-02-21)
#     change class of scal to numeric
#   Natchaya P-Durden (2019-02-28)
#     update Dropbox links for inpRefe and outRefe
#   Natchaya P-Durden (2019-09-16)
#     adding itc calculation
#   Natchaya P-Durden (2019-09-19)
#     replace qfFinl unit from "-" to NA
#   Natchaya P-Durden (2019-09-29)
#     update dp04 qfqm outputs
#   Natchaya P-Durden (2019-10-25)
#     update ITCs calculation for sensible heat flux
#   Natchaya P-Durden (2019-10-31)
#     adding stationarity calculation and output
#   Natchaya P-Durden (2019-11-15)
#     update input parameters of def.itc and def.stna
#     assign unit attribuites for dp04 qfqm
#   Natchaya P-Durden (2020-01-16)
#     adding rtioMoleDryH2oCor
#   Natchaya P-Durden (2020-03-05)
#     Set all thresholds to screen linear coefficients (wrap.irga.vali ()) to FALSE.
#   Chris Florian (2021-08-18)
#     naming convention update for the coefficients from the validation regressing from 'coef' to 'valiCoef'
#     adding additoinal benchmarking regression coefficient attributes
#   David Durden (2021-11-16)
#     adding stability parameter and Obukhov length to HDF5 output
#   David Durden (2022-01-26)
#     updating planar fit terms and functions
#   Chris Florian (2022-02-08)
#     updating footprint terms and functions
#   Adam Young (2024-05-08)
#     Updated arguments for discrete wavelet analysis
#   Natchaya P-Durden (2024-05-08)
#     adding quality indicator flags calculation for stationarity and ITCs tests
#   David Durden (2026-04-30)
#     Removing AMRS data processing after removing from dp0p files
#   Adam Young (2026-07-28)
#     Major updates to Irga Validation:
#       -Packaged gasRefe data into data.frame generated by eddy4R.base::wrap.irga.gas.refe.R
#       -Refactored irga validation so it only needs to run 1 day at a time
#       -Employed last-valid validation which uses the last available validation up to 
#        two weeks prior using accs package, also checks to make sure no installs
#        of LI-7200 occurred within last-valid validation time frame.
#       -Performs correction on rtioMoleDryCo2 within this turb workflow, instead
#        of eddy4R.base::wrap.irga.vali.R, as it was previously
##############################################################################################



##############################################################################################
# USER SELECTIONS


# deploy workflow file in default mode (dflt, uses web-based gold file), in environmental-variables-from-host mode (host, for batch-processing from command line),
# or in user selection mode (slct, for interactive data analysis in Rstudio) [character]
# see bottom of this section for command line instruction templates


  # create list to hold parametric information in a central place
  Para <- list()
  
  # Get URL of cds validation to retrieve/post to
  # For testing: Sys.setenv(SAE_VALIDATION_RESTURL = "https://cds-int.svcs-nonprod.gcp.neoninternal.org/cds-api")
  resturl <- Sys.getenv("SAE_VALIDATION_RESTURL")
  ectype <- "ECTE"


  # user selection in case corresponding environmenal variable is not assigned
  if(!"METH" %in% base::names(base::Sys.getenv())) {
    Para$Flow$Meth <- c("dflt",  "host", "slct")[1]
    # if corresponding environmenal variable is assigned, use that value
  } else {
    Para$Flow$Meth <- base::Sys.getenv("METH")
  }
  
  
  # in case user selection mode is chosen (Para$Flow$Meth == "slct"), workflow parameters can be modified
  if(Para$Flow$Meth == "slct") {
    
    # user-customizeable access to input and output data directories
    Para$Flow$DirUsr <- c(
      dd = "/eddy/data/turbTow",
      nd = "/eddy/data/irga",
      sm = "/eddy/data/turbTow",
      ay = "/eddy/data"
    )["dd"]
    
    # for a detailed description of all default workflow parameters see ?eddy4R.base::def.para.flow.ecte, section Overview of workflow parameters
    Para$Flow$DateOut <- as.character(seq.Date(as.Date("2023-06-07"), as.Date("2023-06-07"), by = 1))
    Para$Flow$DirInp <- base::paste0("/home/", base::Sys.getenv("USER"), Para$Flow$DirUsr, "/irgaValiRefactor/WREF/inp")
    Para$Flow$DirMnt <- base::paste0("/home/", Sys.getenv("USER"), "/eddy")
    Para$Flow$DirOut <- base::paste0("/home/", Sys.getenv("USER"), Para$Flow$DirUsr, "/irgaValiRefactor/WREF/out")
    Para$Flow$DirTmp <- base::paste0("/home/", Sys.getenv("USER"), "/eddy/tmp")
    Para$Flow$DirWrk <- NA
    Para$Flow$FileInp <- base::dir(Para$Flow$DirInp, pattern = "*.h5")
    Para$Flow$FileOutBase <- "NEON.D16.WREF.DP4.00200.001.ecte"
    Para$Flow$NameDataExt <- NA
    Para$Flow$OutMeth <- c("hdf5", "diag")[1]
    Para$Flow$OutSub <- NA
    Para$Flow$PrdIncrCalc <- 1
    Para$Flow$PrdIncrPf <- 1
    Para$Flow$PrdWndwCalc <- 1
    Para$Flow$PrdWndwPf <- NA
    Para$Flow$Read <- c("hdf5", "ff")[1]
    Para$Flow$VersDp <- "001"
    Para$Flow$VersEddy <- "dp04"

  }


  # "default" mode command line instruction template to execute this workflow with the stefanmet/eddy4r-deve Docker image
  # 1. run the unmodified workflow file via:

    # docker run --rm -it -v /FIUdata/IPT_data/dynamic/WG_SCI/docker/deve/latest:/home/$USER/eddy \
    # -e USER=$USER \
    # -e USERID=$UID \
    # stefanmet/eddy4r-deve:deve \
    # Rscript /home/$USER/eddy/Github/NEON-FIU-algorithm/NEONScience/0-NEONScience-DEFAULT/flow/flow.turb/flow.turb.tow.neon.dp04.r

    # abbreviated alternative for gold file testing on the command line
      # docker run --rm -it \
      # stefanmet/eddy4r-deve:deve \
      # Rscript /home/eddy/NEON-FIU-algorithm/ext/shared/flow/flow.pack/flow.test.gold.eddy.R


  # "user selection" mode command line instruction template to execute this workflow with the stefanmet/eddy4r-deve Docker image
  # 1. modify above user selections (select Para$Flow$Meth == "slct") and save the workflow file
  # 2. run the modified workflow file with the same docker run command as for "default" mode

 
  # "host" mode command line instruction template to execute this workflow with the stefanmet/eddy4r-deve Docker image
  # 1. modify below environmental variables as needed (the lines startig with -e). For a detailed description of all default workflow parameters see
  #    ?eddy4R.base::def.para.flow.ecte, section "Overview of workflow parameters"
  # 2. run the unmodified workflow file via:

    # docker run --rm -it \
    # -v /FIUdata/IPT_data/dynamic/WG_SCI/docker/deve/latest:/home/$USER/eddy \
    # -e USER=$USER \
    # -e USERID=$UID \
    # -e DATEOUT=2017-09-05:2017-09-06:2017-09-07:2017-09-08:2017-09-09:2017-09-10:2017-09-11:2017-09-12:2017-09-13:2017-09-14:2017-09-15:2017-09-16:2017-09-17:2017-09-18:2017-09-19:2017-09-20:2017-09-21:2017-09-22:2017-09-23:2017-09-24:2017-09-25:2017-09-26 \
    # -e DIRINP=/home/$USER/eddy/data/turbTow/inpRefe/v20180128_DD_CPER \
    # -e DIRMNT=/home/$USER/eddy \
    # -e DIROUT=/home/$USER/eddy/data/turbTow/out/dp01 \
    # -e DIRTMP=/home/$USER/eddy/tmp \
    # -e DIRWRK=NA \
    # -e FILEINP=NA \
    # -e FILEOUTBASE=NEON.D10.CPER.DP4.00200.001.ecte \
    # -e METH=host \
    # -e NAMEDATAEXT=NA \
    # -e OUTMETH=hdf5 \
    # -e OUTSUB=NA \
    # -e PRDINCRCALC=1 \
    # -e PRDINCRPF=1 \
    # -e PRDWNDWCALC=22 \
    # -e PRDWNDWPF=9 \
    # -e READ=hdf5 \
    # -e VERSDP="001" \
    # -e VERSEDDY=dp04 \
    # stefanmet/eddy4r-deve:deve \
    # Rscript /home/$USER/eddy/Github/NEON-FIU-algorithm/NEONScience/0-NEONScience-DEFAULT/flow/flow.turb/flow.turb.tow.neon.dp04.r



##############################################################################################
# INITIALIZE ENVIRONMENT

library(eddy4R.base)

# define global environment
eddy4R.base::def.env.glob()

logLevel <- Sys.getenv('LOG_LEVEL')
rlog <- Logger.Singleton$new(logging_level = logLevel) #class defined in eddy4R.base
rlog$info("Start ECTE L1 daily transition from flow.turb.tow.neon.dp04.r")


  #Call the R HDF5 Library
  packReq <- c("parsedate")
  
  lapply(packReq, function(x) {
    tryCatch({rlog$debug(x)}, error=function(cond){print(x)})
    if(require(x, character.only = TRUE) == FALSE) {
      install.packages(x)
      library(x, character.only = TRUE)
    }})
  #Remove workflow package list
  rm(packReq)
  # load and attach packages

    # names of packages
    namePack <- c("DataCombine", "eddy4R.base", "eddy4R.turb", "ff", "ffbase", "methods", "rhdf5", "deming") #, "snowfall")
    # ff: needs to be loaded and attached for arithmetic to work properly on ff objects
    # ffbase: needs to be loaded and attached for arithmetic to work properly on ff objects
    # methods: the default for Rscript (and apparently R CMD BATCH) omits package "methods"
    # (https://stat.ethz.ch/R-manual/R-devel/library/utils/html/Rscript.html)
    # this in turn brakes splus2R calls (...could not find function "is"...)
    # hence, exlicitly load and attach package 'methods'

    # load and attach
    tmp <- sapply(namePack, library, character.only = TRUE); rm(tmp)



# collect, and check for presence and absence of workflow parameters, create and set directories, and download reference data (if applicable)
Para$Flow <- eddy4R.base::def.para.flow.ecte(ParaFlow = Para$Flow,
    UrlInpRefe = "https://storage.googleapis.com/neon-ec-goldfiles/EC-turbulence-processing/inpRefe_20190717.zip",
    UrlOutRefe = "https://storage.googleapis.com/neon-ec-goldfiles/EC-turbulence-processing/outRefe_20240812.zip")


# set package options

  #Para$Flow$DirTmp <- base::paste0("/home/", Sys.getenv("USER"), "/eddy/tmp")
  # ff
  # http://stackoverflow.com/questions/19687041/r-temporary-directory-set-to-external-drive
  # changing fftempdir also requires setting the default finalizer to "deleteIfOpen"
  # create fftempdir
  # dir.create(Para$Flow$DirTmp, recursive = TRUE, showWarnings = FALSE)
  # select options
  options("fftempdir" = Para$Flow$DirTmp,
          "fffinalizer" = "deleteIfOpen")
  # clean up fftempdir from previous runs
  invisible(base::file.remove(base::dir(base::getOption("fftempdir"), pattern = "*.ff", full.names = TRUE)))
  # getOption("fftempdir")
  # getOption("fffinalizer")


  # #initialize cluster
  # sfInit(parallel = TRUE, cpus = sfClusterCpus)
  #
  # #load and attach packages in cluster
  # tmp <- sapply(namePack, sfLibrary, character.only=TRUE); rm(namePack, tmp)



# derive logic numbers for efficiently looping around moving planar-fit window


  # initial checks

    # check if length(Para$Flow$FileInp) covers Para$Flow$PrdWndwCalc
    if(Para$Flow$PrdWndwCalc > base::length(Para$Flow$FileInp)) {
      msg <- "number of daily input files < Para$Flow$PrdWndwCalc."
      tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
      stop(msg)
    }

    # # catch condition where PrdWndwCalc < PrdWndwPf
    # if(Para$Flow$PrdWndwCalc < Para$Flow$PrdWndwPf) {
    #   msg <- "please check: Para$Flow$PrdWndwCalc needs to be >= Para$Flow$PrdWndwPf."
    #   tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
    #   stop(msg)
    # }

    # catch condition where PrdIncrCalc != 1 | PrdIncrPf != 1
    if(Para$Flow$PrdIncrCalc != 1 | Para$Flow$PrdIncrPf != 1) {
      msg <- "method for Para$Flow$PrdIncrCalc != 1 | Para$Flow$PrdIncrPf != 1 not currently implemented."
      tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
      stop(msg)
    }

    # check that Para$Flow$DateOut is a strictly regular, daily sequence
    # i.e. it can be used together with Para$Flow$PrdIncrPf = 1
    if(!base::all(base::diff(base::as.Date(Para$Flow$DateOut)) == 1)) {
      msg <- "please ensure that Para$Flow$DateOut consists of subsequent days."
      tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
      stop(msg)
    }


  # derive looping logic numbers

    # determine center days in planar fit window for controlling downstream computation
    # later, for Para$Flow$PrdIncrPf > 1 this permits calculating outputs for more than a single central day, e.g. 3 days (not yet supported)
    dateCalcCntr <- Para$Flow$DateOut[base::seq(from = 1, to = base::length(Para$Flow$DateOut), by = Para$Flow$PrdIncrPf)]

    # for each center day, determine the date sequence for calculation
    for(idx in dateCalcCntr) {

      Para$Flow$DateCalc[[idx]] <- base::as.character(base::seq.Date(
        from = base::as.Date(idx) - ((base::as.integer(Para$Flow$PrdWndwCalc) - 1) / 2),
        to = base::as.Date(idx) + ((base::as.integer(Para$Flow$PrdWndwCalc) - 1) / 2),
        by = "day"))

    }; rm(idx)

    # check if there is an input file for each day over all planar fit windows

      # determine if there is an input file corresponding to each day
      dateCalcAll <- sapply(base::sort(base::unique(base::unlist(Para$Flow$DateCalc))), function(x)
        base::any(base::grepl(pattern = base::paste0(".*", x, ".*.h5?"), Para$Flow$FileInp))
        )

      # issue an error in case one or more files are missing
      if(!base::all(dateCalcAll)) {
        msg <- base::paste("dp0p data for", base::paste0(base::names(dateCalcAll)[!dateCalcAll], collapse = ", "), "is missing.")
        tryCatch({rlog$fatal(msg)}, error=function(cond){print(msg)})
        stop(msg)
      }

    # clean up
    base::rm(dateCalcCntr, dateCalcAll)


# prepare objects for remaining workflow


  # create lists to hold ff objects

    # measurement data
    data <- list()

    #data for validation

    vali <- list()

    # qaqc data
    qfqmFlag <- list()


  # keep track of iterations

    # around the center days of the moving planar-fit window
    numDateCalcCntr <- 0

    # around days covered under the moving planar-fit window
    numDateCalcWndw <- 0



###
# start: main loop around center days in moving planar fit window
# funcSf <- function(DateInp02) {
for(dateCalcCntr in base::names(Para$Flow$DateCalc)) {
# dateCalcCntr <- base::names(Para$Flow$DateCalc)[1]
numDateCalcCntr <- numDateCalcCntr + 1
###

rlog$info(paste0("In moving planar-fit window day ", numDateCalcCntr))

  ##############################################################################################
  # ASSEMBLE PARAMETERS FROM HDF5 FILE


  # determine full input file path for current center day in moving planar fit window
  # this day is used as reference for reading parametric information
  DirFilePara <- base::file.path(Para$Flow$DirInp, grep(pattern = base::paste0(".*", dateCalcCntr, ".*.h5?"), Para$Flow$FileInp, value = TRUE))


  # prepare subsequent iterations

    # reset / clean up parameters and data from previous dateCalcCntr
    if(numDateCalcCntr != 1) {

      # site-specific, scientific parameters, calibration parameter
      Para$Site <- NULL; Para$Sci <- NULL ; Para$Cal <- NULL

      # purge ff entries that are not needed for the current planar-fit window

        # data$soni: remove planar-fit columns that were added during post-processing in previous planar-fit window
        # these are later being re-added, as result of the new planar fit for the current window
        data$soni <- data$soni[!base::grepl(names(data$soni), pattern = "*Pf*")]

        # remove rows that are in the datelist of the previous, but not the current planar-fit window
        # see https://stackoverflow.com/questions/13806353/delete-rows-ff-package
        # loop around all entries of data (~100 s)
        for(sens in base::names(data)) {

          # distinguish between ff objects for sensors (have timestamp in "time" column) and for time (has time in "UTC" column)
          if(sens != "time") {

            # currently requires workaround (ffbase::subset.ffdf cannot be used) because qfqm does not include "time" as separate column
            # data[[sens]] <- ffbase::subset.ffdf(x = data[[sens]], subset = base::as.Date(time) %in% base::as.Date(Para$Flow$DateCalc[[dateCalcCntr]]))  # works
            # qfqmFlag[[sens]] <- ffbase::subset.ffdf(x = qfqmFlag[[sens]], subset = base::as.Date(time) %in% base::as.Date(Para$Flow$DateCalc[[dateCalcCntr]])) # does not work

              # determine indices that are still needed
              idx <- base::as.Date(data[[sens]]$time) %in% base::as.Date(Para$Flow$DateCalc[[dateCalcCntr]])
              idx <- ffwhich(x = idx, expr = idx == TRUE)

              # retrieve orginal units; only necessary for data as qfqmFlag does not drop units
              tmpUnit <- sapply(base::names(data[[sens]]), function(x) base::attr(x = data[[sens]][[x]], which = "unit"))

              # subset data and qfqmFlag
              data[[sens]] <- data[[sens]][idx,]
              qfqmFlag[[sens]] <- qfqmFlag[[sens]][idx,]
              # for irgaTurb validation
              if (sens == "irgaTurb"){
                vali[[sens]] <- vali[[sens]][idx,]
                # re-apply orginal units; only necessary for data as qfqmFlag does not drop units
                sapply(base::names(vali[[sens]]), function(x) base::attr(x = vali[[sens]][[x]], which = "unit") <<- tmpUnit[[x]])
              }
              # re-apply orginal units; only necessary for data as qfqmFlag does not drop units
              sapply(base::names(data[[sens]]), function(x) base::attr(x = data[[sens]][[x]], which = "unit") <<- tmpUnit[[x]])

              # clean up
              rm(idx, tmpUnit)

          } else {

            # in case of the time ffdf, ffbase::subset.ffdf works for both, data and qfqmFlag
            # TODO: units are being dropped in both cases
            data[[sens]] <- ffbase::subset.ffdf(x = data[[sens]], subset = base::as.Date(UTC) %in% base::as.Date(Para$Flow$DateCalc[[dateCalcCntr]]))
            qfqmFlag[[sens]] <- ffbase::subset.ffdf(x = qfqmFlag[[sens]], subset = base::as.Date(UTC) %in% base::as.Date(Para$Flow$DateCalc[[dateCalcCntr]]))
            # for irgaTurb validation
            if (sens == "irgaTurb"){
              vali[[sens]] <- ffbase::subset.ffdf(x = vali[[sens]], subset = base::as.Date(UTC) %in% base::as.Date(Para$Flow$DateCalc[[dateCalcCntr]]))
            }

          }

        }; rm(sens)

    }


    # determine which (additional) days of data need to be read from file
    if(numDateCalcCntr == 1) {
      dateCalcWndwAll <- Para$Flow$DateCalc[[dateCalcCntr]]
    } else {
      dateCalcWndwAll <- Para$Flow$DateCalc[[dateCalcCntr]][!(ff(base::as.Date(Para$Flow$DateCalc[[dateCalcCntr]])) %in% base::as.Date(data$time$UTC))[]]
    }


  # site-specific parameters

    # site group

      # get parameters
      Para$Site <- def.hdf5.read.para(
        DirFileParaLoca = DirFilePara,
        GrpName = "site",
        SetPara = c("DistZaxsGrndOfst", "DistZaxsLvlMeasTow", "DistZaxsTow", "ElevRefeTow", "LatTow", "LonTow", "ZoneTime", "ZoneUtm")
      )
      idx01 <- base::names(Para$Site)

      # TODO: remove failsafes / backwards compatibility upon next major release
      if(base::length(idx01) == 0) {

        Para$Site <- NULL

      } else {

        # make sure measurement height etc. are converted to numeric (can contain leading zeroes which triggers an exception in def.hdf5.read.para())
        idx02 <- idx01[idx01 %in% c("DistZaxsGrndOfst", "DistZaxsLvlMeasTow", "DistZaxsTow", "ElevRefeTow", "LatTow", "LonTow")] # "ZoneUtm", removing conversion to numeric to allow update to grab zoneUtm from database as character
        base::invisible(
          sapply(idx02, function(x)
            Para$Site[[x]] <<- base::as.numeric(base::as.character(Para$Site[[x]])))
        )
        base::rm(idx02)

      }; base::rm(idx01)

    # NEON-specific 4-letter code for the site location (Loc)
    Para$Site$Loc <- eddy4R.base::def.para.site(FileInp = DirFilePara)$Loc

    # tower top level (LvlTowr)
    # TODO: this appears inconsistent with hdf5 metadata spreadsheet:
    # LvlMeasTow is documented there as "A list containing the identifier number associated with the meaurement level heights"
    Para$Site$LvlTowr <- eddy4R.base::def.para.site(FileInp = DirFilePara)$LvlTowr

    #get the level for gasRefe
    #list all group hdf5 path
    listPara <- rhdf5::h5ls(DirFilePara, datasetinfo = FALSE)
    Para$Site$LvlTowrGasRefe <- unique(sapply(1:length(unique(listPara$group[(grep("gasRefe/",listPara$group))])), function(xx) strsplit(unique(listPara$group[(grep("gasRefe/",listPara$group))]), "gasRefe/")[[xx]][2]))



  # environmental parameters

    # site group

      # get parameters
      # TODO: remove failsafes / backwards compatibility upon next major release
      # Para$Env$ZoneTime <- NULL; idx01 <- base::names(Para$Env)
      Para$Env <- def.hdf5.read.para(
        DirFileParaLoca = DirFilePara,
        GrpName = "site",
        SetPara = c("DistZaxsDisp", "ZoneTime")
      )
      Para$Env$ZoneTime <- NULL
      idx01 <- base::names(Para$Env)

      # TODO: remove failsafes / backwards compatibility upon next major release
      if(base::length(idx01) == 0) {

        Para$Env <- NULL

      } else {

        # make sure displacement height etc. are converted to numeric (can contain leading zeroes which triggers an exception in def.hdf5.read.para())
        idx02 <- idx01[idx01 %in% c("DistZaxsDisp")]
        base::invisible(
          sapply(idx02, function(x)
            Para$Env[[x]] <<- base::as.numeric(base::as.character(Para$Env[[x]])))
        )
        base::rm(idx02)

      }; base::rm(idx01)


  # scientific parameters

    # site group
    Para$Sci <- def.hdf5.read.para(
      DirFileParaLoca = DirFilePara,
      GrpName = "site",
      SetPara = c("Pf$AngEnuXaxs", "Pf$AngEnuYaxs", "Pf$Ofst")
    )

      # data product level group
      Para$Sci$dp01 <- def.hdf5.read.para(
        DirFileParaLoca = DirFilePara,
        GrpName = paste0("/", Para$Site$Loc, "/dp01"),
        SetPara = c("Dspk$Br86$MaxReso", "Dspk$Br86$NumBin", "Dspk$Br86$NumWndw")
      )

        # data product group
        Para$Sci$dp0p$irgaTurb <- def.hdf5.read.para(
          DirFileParaLoca = DirFilePara,
          GrpName = paste0("/", Para$Site$Loc, "/dp0p/data/irgaTurb"),
          SetPara = c("FreqSamp", "Sens")
        )

        Para$Sci$dp0p$mfcSampTurb <- def.hdf5.read.para(
          DirFileParaLoca = DirFilePara,
          GrpName = paste0("/", Para$Site$Loc, "/dp0p/data/mfcSampTurb"),
          SetPara = c("FreqSamp", "Sens")
        )

        Para$Sci$dp0p$valvValiNemaTurb <- def.hdf5.read.para(
          DirFileParaLoca = DirFilePara,
          GrpName = paste0("/", Para$Site$Loc, "/dp0p/data/valvValiNemaTurb"),
          SetPara = c("FreqSamp", "Sens")
        )

        Para$Sci$dp0p$soni <- def.hdf5.read.para(
          DirFileParaLoca = DirFilePara,
          GrpName = paste0("/", Para$Site$Loc, "/dp0p/data/soni"),
          SetPara = c("AngNedZaxs","AngShdwMin","AngShdwMax","FreqSamp", "Sens")
        )

        # # Para$Sci$dp0p$amrs <- def.hdf5.read.para(
        #   DirFileParaLoca = DirFilePara,
        #   GrpName = paste0("/", Para$Site$Loc, "/dp0p/data/amrs"),
        #   SetPara = c("FreqSamp", "Sens")
        # )

        Para$Sci$dp0p$gasRefe <- def.hdf5.read.para(
          DirFileParaLoca = DirFilePara,
          GrpName = paste0("/", Para$Site$Loc, "/dp0p/data/gasRefe"),
          SetPara = c("FreqSamp", "Sens")
        )

        for(idx in c("co2Turb", "h2oTurb", "soni")) {

          Para$Sci$dp01[[idx]] <- def.hdf5.read.para(
            DirFileParaLoca = DirFilePara,
            GrpName = paste0("/", Para$Site$Loc, "/dp01/data/", idx),
            SetPara = c("PrdIncrAgrDflt", "PrdWndwAgrDflt")
          )

        }; rm(idx)

        # data table

          # co2Turb/RtioMoleDryCo2
          Para$Sci$dp01$co2Turb$RtioMoleDryCo2 <- def.hdf5.read.para(
            DirFileParaLoca = DirFilePara,
            GrpName = paste0("/", Para$Site$Loc, "/dp01/data/co2Turb/",Para$Site$LvlTowr,"_30m/rtioMoleDryCo2"),
            SetPara = c("Lag$TimeDiff")
          )

          # h2oTurb/RtioMoleDryH2o
          Para$Sci$dp01$h2oTurb$RtioMoleDryH2o <- def.hdf5.read.para(
            DirFileParaLoca = DirFilePara,
            GrpName = paste0("/", Para$Site$Loc, "/dp01/data/h2oTurb/",Para$Site$LvlTowr,"_30m/rtioMoleDryH2o"),
            SetPara = c("Lag$TimeDiff")
          )


  # map parameters to internal process (failsafes and backward compatibility)
  # TODO: remove mapping / clean up for next major release

    # c2r: VersDp output version
    Para$Flow$VersDp <- paste0(Para$Flow$VersDp, "_", format(Sys.time(), "%Y%m%d_%H%M%S_%Z"))

    #Sampling frequency for ECTE instrumentation TODO: Change to similar logic as AngZaxsSoniInst
    FreqSamp <- list(
      "irgaTurb" = base::ifelse(!is.null(Para$Sci$dp0p$irgaTurb$FreqSamp), ifelse(!is.na(Para$Sci$dp0p$irgaTurb$FreqSamp)&!Para$Sci$dp0p$irgaTurb$FreqSamp == "NA",
                                base::as.numeric(Para$Sci$dp0p$irgaTurb$FreqSamp), 20), 20),
      "mfcSampTurb" = base::ifelse(!is.null(Para$Sci$dp0p$mfcSampTurb$FreqSamp), ifelse(!is.na(Para$Sci$dp0p$mfcSampTurb$FreqSamp)&!Para$Sci$dp0p$mfcSampTurb$FreqSamp == "NA",
                                   base::as.numeric(Para$Sci$dp0p$mfcSampTurb$FreqSamp), 20), 20),
      "valvValiNemaTurb" = base::ifelse(!is.null(Para$Sci$dp0p$valvValiNemaTurb$FreqSamp), ifelse(!is.na(Para$Sci$dp0p$valvValiNemaTurb$FreqSamp)&!Para$Sci$dp0p$valvValiNemaTurb$FreqSamp == "NA", base::as.numeric(Para$Sci$dp0p$valvValiNemaTurb$FreqSamp), 0.2), 0.2),
      "soni" = base::ifelse(!is.null(Para$Sci$dp0p$soni$FreqSamp), ifelse(!is.na(Para$Sci$dp0p$soni$FreqSamp)&!Para$Sci$dp0p$soni$FreqSamp == "NA",
                            base::as.numeric(Para$Sci$dp0p$soni$FreqSamp), 20), 20),
      "gasRefe" = base::ifelse(!is.null(Para$Sci$dp0p$gasRefe$FreqSamp), ifelse(!is.na(Para$Sci$dp0p$gasRefe$FreqSamp)&!Para$Sci$dp0p$gasRefe$FreqSamp == "NA",
                                                                          base::as.numeric(Para$Sci$dp0p$gasRefe$FreqSamp), 1), 1)
    )
    
    #Logic for AngZaxsSoniInst metadata, error if the sensor is installed and metadata is missing; replace with 0 if not installed
    if(is.na(Para$Sci$dp0p$soni$AngNedZaxs)|is.null(Para$Sci$dp0p$soni$AngNedZaxs)&Para$Sci$dp0p$soni$Sens == "instFalse"){Para$Sci$dp0p$soni$AngNedZaxs <- 0L}
    if(is.na(Para$Sci$dp0p$soni$AngNedZaxs)|is.null(Para$Sci$dp0p$soni$AngNedZaxs)&Para$Sci$dp0p$soni$Sens == "instTrue") stop("The soni sensor is installed, but the installation angle metadata (AngNedZaxs) is missing")


    # site info

      # create list with defaults for backward compatibility / failsafe: call site info
      # TODO: remove failsafe for next major release
      SiteInfo <- def.site.info(loc = Para$Site$Loc)
      # SiteInfo <- list()

      # time difference between local time and UTC
      if(!base::is.null(Para$Site$ZoneTime)) {

        # start date and time of dataset in UTC
        timeTmp01 <- base::as.POSIXlt(x = base::paste0(dateCalcCntr, "T00:00:00Z"), format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
        timeTmp02 <- timeTmp01

        mapZoneTime <- c("AST" = -4L, "AKST" = -9L, "CST" = -6L, "EST" = -5L, "HST" = -10L, "MST" = -7L, "PST" = -8L, "PST/MST" = -7L)
        
        # assign local timezone attribute, if available in database
        if(Para$Site$ZoneTime %in% base::OlsonNames()) {

          attributes(timeTmp02)$tzone <- Para$Site$ZoneTime

        } else {

          base::warning(base::paste("Time zone attribute", Para$Site$ZoneTime,
                                    "not available in R base::OlsonNames() database. Continue with local time equals UTC time."))

        }

        # time difference between local time and UTC
        SiteInfo$TimeDiffUtcLt <- mapZoneTime[Para$Site$ZoneTime]
        #Add to site parameters metadata
        Para$Site$TimeDiffUtcLt <- ifelse(Para$Site$ZoneTime %in% names(mapZoneTime), mapZoneTime[Para$Site$ZoneTime], "NA")

        # clean up
        rm(timeTmp01, timeTmp02)

        # # alternative solution using lubridate
        # timeTmp01 <- base::as.POSIXlt(x = base::paste0(dateCalcCntr, "T00:00:00Z"), format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
        # timeTmp02 <- lubridate::force_tz(lubridate::with_tz(timeTmp01, tzone = Para$Site$ZoneTime), tzone = "UTC")
        # SiteInfo$TimeDiffUtcLt <- as.numeric(difftime(timeTmp02, timeTmp01, units = "hours"))

      }

      # cartesian (UTM) coordinates
      if(!base::is.null(Para$Site$LonTow) & !base::is.null(Para$Site$LatTow)) {

        # polar (LL) coordinates in radians
        tmp01 <- eddy4R.base::def.unit.conv(data = t(as.matrix(c(Para$Site$LonTow, Para$Site$LatTow))), unitFrom = "deg", unitTo = "rad", MethGc = FALSE)

        # conversion from polar (LL) to cartesian (UTM) coordinates
        tmp02 <- data.frame(proj4::ptransform(tmp01, "+proj=latlong +ellps=WGS84", paste("+proj=utm +zone=", Para$Site$ZoneUtm, " +ellps=WGS84", sep=""))[1:2])
        dimnames(tmp02)[[2]][1:2] <- c("easting","northing")

        # assign
        SiteInfo$ZoneUtm$Zone <- Para$Site$ZoneUtm
        SiteInfo$ZoneUtm$Estg <- tmp02$easting
        SiteInfo$ZoneUtm$Nthg <- tmp02$northing

        #clean up
        rm(tmp01, tmp02)

      }

      # measurement height above ground
      # in case not available, omits zero offset of tower reference point with respect to ground surface
      if(!base::is.null(Para$Site$DistZaxsLvlMeasTow)) {

        SiteInfo$DistZaxsMeas <- Para$Site$DistZaxsLvlMeasTow[base::length(Para$Site$DistZaxsLvlMeasTow)] +
          ifelse(!base::is.na(ifelse(!base::is.null(Para$Site$DistZaxsGrndOfst), Para$Site$DistZaxsGrndOfst, NA)), Para$Site$DistZaxsGrndOfst, 0)

      }

      # zero plane displacement
      if(!base::is.null(Para$Env$DistZaxsDisp)) SiteInfo$DistZaxsDisp <- Para$Env$DistZaxsDisp

      # elevation of ground surface asl
      if(!base::is.null(Para$Site$ElevRefeTow)) SiteInfo$ElevAslTow <- Para$Site$ElevRefeTow

    # c2r: ParaSci\Dp0p thresholds for range test, only for scientific variables
    # also, only these variables undergo de-spiking

      #assign list
      Rng <- list()

      # irgaTurb data
      Rng$irgaTurb <- data.frame(
        "densMoleCo2" = c(-20,31) * 1e-3,       #[mol m-3]
        "densMoleH2o" = c(0,1500) * 1e-3,     #[mol m-3]
        "presAtm" = c(50,120) * 1e3,          #[Pa]
        "presDiff" = c(-10,1) * 1e3,          #[Pa]
        "rtioMoleDryCo2" = c(-200,2000) * 1e-6, #[mol mol-1] #TODO: change during refactoring
        "rtioMoleDryH2o" = c(-20,80) * 1e-3,    #[mol mol-1]
        "tempIn" = c(220,330),                #[K]
        "tempOut" = c(220,330)                #[K]
      )

      # soni data
      Rng$soni <- data.frame(
        "veloXaxs"=c(-50,50),            #[m s-1]
        "veloYaxs"=c(-50,50),            #[m s-1] 
        "veloZaxs"=c(-10,10),            #[m s-1]
        "veloSoni"=c(300,400)            #[m s-1]
      )

      # amrs
  #    Rng$amrs <- data.frame(
  #      "angXaxs"=c(-360,360),            #[deg]
  #      "angYaxs"=c(-360,360),            #[deg]
  #      "angZaxs"=c(-360,360)             #[deg]
  #    )
      
      Para$Cal <- eddy4R.base::wrap.irga.gas.refe(inp = Para[c("Flow", "Site")], gasRefeFreqSamp = FreqSamp$gasRefe, dateParaCal = dateCalcCntr)

      # # read-in the standard gas concentration from HDF5 and save them in Para$Cal
      # if (length(Para$Flow$DateCalc[[dateCalcCntr]]) > 1){
      # dateParaCal <- as.character(c(as.Date(dateCalcCntr)-1, as.Date(dateCalcCntr), as.Date(dateCalcCntr)+1))
      # }else{dateParaCal <- dateCalcCntr}
      # 
      # for(dateCalcWndw in dateParaCal) {
      #   #dateCalcWndw <- dateCalcWndwAll[1]
      #   Para$Cal$rtioMoleDryCo2Refe01[[dateCalcWndw]] <- list()
      #   Para$Cal$rtioMoleDryCo2Refe02[[dateCalcWndw]] <- list()
      #   Para$Cal$rtioMoleDryCo2RefeSe01[[dateCalcWndw]] <- list()
      #   Para$Cal$rtioMoleDryCo2RefeSe02[[dateCalcWndw]] <- list()
      #   Para$Cal$rtioMoleDryCo2RefeDf01[[dateCalcWndw]] <- list()
      #   Para$Cal$rtioMoleDryCo2RefeDf02[[dateCalcWndw]] <- list()
      #   Para$Cal$rtioMoleDryCo2RefeTime01[[dateCalcWndw]] <- list()
      #   Para$Cal$rtioMoleDryCo2RefeTime02[[dateCalcWndw]] <- list()
      # 
      #   for(idxLvl in Para$Site$LvlTowrGasRefe){
      #     #idxLvl <- Para$Site$LvlTowrGasRefe[1]
      #     # tmp <- wrap.hdf5.read(
      #     #   DirInpLoca = Para$Flow$DirInp,
      #     #   SiteLoca = Para$Site$Loc,
      #     #   DateLoca = dateCalcWndw,
      #     #   VarLoca = "gasRefe",
      #     #   FreqLoca = FreqSamp[["gasRefe"]],
      #     #   LvlTowr = idxLvl,
      #     #   RngLoca = Rng,
      #     #   DespLoca = list(widt = 9, #Para$Sci$dp01$`Dspk$Br86$NumWndw`,    # c2r: WndwDspkBr86 de-spiking median filter window width [s]
      #     #                   nbin = 2, #Para$Sci$dp01$`Dspk$Br86$NumBin`,     # c2r: NumDspkBr86Bin de-spiking histogram bins initial number/step size
      #     #                   rest = 10 #Para$Sci$dp01$`Dspk$Br86$MaxReso`     # c2r: ThshDspkBr86Reso de-spiking resolution threshold
      #     #   )
      #     # )
      #     
      #     tmp <- eddy4R.base::def.hdf5.read.qfqm(
      #       DirInpLoca = Para$Flow$DirInp,
      #       SiteLoca = Para$Site$Loc,
      #       DateLoca = dateCalcWndw,
      #       VarLoca = "gasRefe",
      #       FreqLoca = FreqSamp[["gasRefe"]], 
      #       LvlTowr = idxLvl)
      #     # print(paste0(idx," and ", idxLvl))
      # 
      #     # assign the first value to represent rtioMoleDryCo2Refe on that day
      #     Para$Cal$rtioMoleDryCo2Refe01[[dateCalcWndw]][[idxLvl]] <- tmp$rtioMoleDryCo2Refe[1]
      #     Para$Cal$rtioMoleDryCo2RefeTime01[[dateCalcWndw]][[idxLvl]] <- tmp$time[1]
      # 
      #     # read-in sd values of rtioMoleDryCo2Refe from attributes
      #     if (is.character(attributes(tmp)$sd[["rtioMoleDryCo2Refe"]])) {
      #       Para$Cal$rtioMoleDryCo2RefeSe01[[dateCalcWndw]][[idxLvl]] <- as.numeric(attributes(tmp)$sd[["rtioMoleDryCo2Refe"]])
      #       Para$Cal$rtioMoleDryCo2RefeSe02[[dateCalcWndw]][[idxLvl]] <- as.numeric(attributes(tmp)$sd[["rtioMoleDryCo2Refe"]])
      #     } else{
      #       Para$Cal$rtioMoleDryCo2RefeSe01[[dateCalcWndw]][[idxLvl]] <- attributes(tmp)$sd[["rtioMoleDryCo2Refe"]]
      #       Para$Cal$rtioMoleDryCo2RefeSe02[[dateCalcWndw]][[idxLvl]] <- attributes(tmp)$sd[["rtioMoleDryCo2Refe"]]
      #     }
      # 
      #     # read-in DfSd values of rtioMoleDryCo2Refe from attributes
      #     if (is.character(attributes(tmp)$DfSd[["rtioMoleDryCo2Refe"]])) {
      #       Para$Cal$rtioMoleDryCo2RefeDf01[[dateCalcWndw]][[idxLvl]] <- as.numeric(attributes(tmp)$DfSd[["rtioMoleDryCo2Refe"]])
      #       Para$Cal$rtioMoleDryCo2RefeDf02[[dateCalcWndw]][[idxLvl]] <- as.numeric(attributes(tmp)$DfSd[["rtioMoleDryCo2Refe"]])
      #     } else{
      #       Para$Cal$rtioMoleDryCo2RefeDf01[[dateCalcWndw]][[idxLvl]] <- attributes(tmp)$DfSd[["rtioMoleDryCo2Refe"]]
      #       Para$Cal$rtioMoleDryCo2RefeDf02[[dateCalcWndw]][[idxLvl]] <- attributes(tmp)$DfSd[["rtioMoleDryCo2Refe"]]
      #     }
      # 
      #     #indicate condition when there are more than one gasRefe values in one day
      #     Para$Cal$rtioMoleDryCo2Refe02[[dateCalcWndw]][[idxLvl]] <- tmp$rtioMoleDryCo2Refe[which(tmp$rtioMoleDryCo2Refe != tmp$rtioMoleDryCo2Refe[1])]
      #     Para$Cal$rtioMoleDryCo2RefeTime02[[dateCalcWndw]][[idxLvl]] <- tmp$time[which(tmp$rtioMoleDryCo2Refe != tmp$rtioMoleDryCo2Refe[1])]
      # 
      #     if (length(Para$Cal$rtioMoleDryCo2Refe02[[dateCalcWndw]][[idxLvl]]) > 0){
      #       Para$Cal$rtioMoleDryCo2Refe02[[dateCalcWndw]][[idxLvl]] <- Para$Cal$rtioMoleDryCo2Refe02[[dateCalcWndw]][[idxLvl]][1]
      #       Para$Cal$rtioMoleDryCo2RefeTime02[[dateCalcWndw]][[idxLvl]] <- Para$Cal$rtioMoleDryCo2RefeTime02[[dateCalcWndw]][[idxLvl]][1]
      #     }else{
      #       Para$Cal$rtioMoleDryCo2Refe02[[dateCalcWndw]][[idxLvl]] <- Para$Cal$rtioMoleDryCo2Refe01[[dateCalcWndw]][[idxLvl]]
      #       Para$Cal$rtioMoleDryCo2RefeTime02[[dateCalcWndw]][[idxLvl]] <- Para$Cal$rtioMoleDryCo2RefeTime01[[dateCalcWndw]][[idxLvl]]
      #     }
      #     # unit attributes
      #     attributes(Para$Cal$rtioMoleDryCo2Refe01[[dateCalcWndw]][[idxLvl]])$unit <- attributes(tmp)$unit[["rtioMoleDryCo2Refe"]]
      #     attributes(Para$Cal$rtioMoleDryCo2Refe02[[dateCalcWndw]][[idxLvl]])$unit <- attributes(tmp)$unit[["rtioMoleDryCo2Refe"]]
      #   }
      # 
      # }; rm(dateCalcWndw, idxLvl, tmp)

  ##############################################################################################
  #READ LIST OF FILES


  ###
  # start: loop around days in current planar-fit window
  for(dateCalcWndw in dateCalcWndwAll) {
  # dateCalcWndw <- dateCalcWndwAll[1]
  numDateCalcWndw <- numDateCalcWndw + 1
  ###

    ###
    # begin: read raw data
    if(Para$Flow$Read == "hdf5") {
    ###

      ##############################################################################################
      #DATA IMPORT

      # create directory structure
      dir.create(paste(Para$Flow$DirWrk, "/", Para$Site$Loc, "/", Para$Flow$VersDp, "/30-min rawdata", sep=""), recursive = TRUE, showWarnings = FALSE)

      # assign list for inputs
      inp <- list()

      # start loop around instruments
      for(Var in c("irgaTurb", "mfcSampTurb", "valvValiNemaTurb", "soni")){
      # Var <- c("irgaTurb", "mfcSampTurb", "valvValiNemaTurb", "soni", "amrs")[1]

        # call read-in wrapper, assign result as temporary variable
        # tmp <- eddy4R.base::wrap.hdf5.read(
        #   DirInpLoca = Para$Flow$DirInp,
        #   SiteLoca = Para$Site$Loc,
        #   DateLoca = dateCalcWndw,
        #   VarLoca = Var,
        #   FreqLoca = FreqSamp[[Var]],
        #   LvlTowr = Para$Site$LvlTowr,
        #   Rng = TRUE,
        #   RngLoca = Rng,
        #   DespLoca = list(widt = 9, #Para$Sci$dp01$`Dspk$Br86$NumWndw`,    # c2r: WndwDspkBr86 de-spiking median filter window width [s]
        #                   nbin = 2, #Para$Sci$dp01$`Dspk$Br86$NumBin`,     # c2r: NumDspkBr86Bin de-spiking histogram bins initial number/step size
        #                   rest = 10 #Para$Sci$dp01$`Dspk$Br86$MaxReso`     # c2r: ThshDspkBr86Reso de-spiking resolution threshold
        #                   )
        #   )
        
        tmp <- eddy4R.base::def.hdf5.read.qfqm(
          DirInpLoca = Para$Flow$DirInp,
          SiteLoca = Para$Site$Loc,
          DateLoca = dateCalcWndw,
          VarLoca = Var,
          FreqLoca = FreqSamp[[Var]],
          DataType = "data",
          LvlTowr = Para$Site$LvlTowr
        )

        #Read in quality flags from HDF5 file
        if(!Var %in% "valvValiNemaTurb"){

        tmpQfqm <- eddy4R.base::def.hdf5.read.qfqm(
          DirInpLoca = Para$Flow$DirInp,
          SiteLoca = Para$Site$Loc,
          DateLoca = dateCalcWndw,
          VarLoca = Var,
          FreqLoca = FreqSamp[[Var]],
          DataType = "qfqm",
          LvlTowr = Para$Site$LvlTowr
             )

        #Remove time from set of flags
        tmpQfqm <- tmpQfqm[,grep("time", names(tmpQfqm), invert = TRUE)]
        }

        # assign result as ffdf to inp

          # time domain incl. unit assignment
          if(Var == "irgaTurb") {

            inp$time <- ff:::as.ffdf.data.frame(data.frame(UTC = tmp$time))
            base::attr(x = inp$time$UTC, which = "unit") <- "YYYY-MM-DD hh:mm:ss.sss"

          }

          # sensor data incl. unit assignment
          inp$data[[Var]] <- ff:::as.ffdf.data.frame(tmp)
          for(idx in base::names(tmp)) base::attr(x = inp$data[[Var]][[idx]], which = "unit") <-
            base::attr(x = tmp, which = "unit")[[idx]]

          if(exists("tmpQfqm")) inp$qfqm[[Var]] <- ff:::as.ffdf.data.frame(tmpQfqm)

        # clean up
        rm(tmp)
        if(exists("tmpQfqm")) rm(tmpQfqm)
        invisible(gc())

      # end loop around instruments
      }; rm(Var)

      # derived quantities: daily extent, native resolution

        # actual calculation
        inp <- eddy4R.base::wrap.derv.prd.day(
          inpList = inp,
          ZoneTime = Para$Sci$ZoneTime,
          AngZaxsSoniInst = as.numeric(Para$Sci$dp0p$soni$AngNedZaxs)
        )  ##TODO remove as.numeric after the parameter read function is updated

        # print message to screen
        msg <- paste0("dataset ", dateCalcWndw, ": derived quantities calculated (daily extent, native resolution)")
        tryCatch({rlog$info(msg)}, error=function(cond){print(msg)})

      # save pre-processed raw data
      if(FALSE) {

        # print message to screen
        msg <- paste0("dataset ", dateCalcWndw, " saving pre-processed raw data begin")
        tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})

        # directory for saving ffData objects
        DirFf <- paste0(Para$Flow$DirInp, "/ffData/", dateCalcWndw)
        base::dir.create(path = DirFf, showWarnings = FALSE, recursive = TRUE)

        # start loop around data, qfqm
        for(Var01 in c("data", "qfqm")){
        # Var01 <- c("data", "qfqm")[1]

          # start loop around instruments
          for(Var02 in names(inp[[Var01]])){
          # Var02 <- names(inp[[Var01]])[1]

            # assign data as temporary variable; ffsave can only store individual ff objects, not list-nested ff objects
            tmp <- inp[[Var01]][[Var02]]

            # save to file
            ffsave(list = "tmp", file = paste0(DirFf, "/", Var01, "_", Var02), compress = TRUE)

            # clean up
            rm(tmp)
            invisible(gc())

          # end loop around instruments
          }; rm(Var02)

        # end loop around data, qfqm
        }; rm(Var01)

        # write time

          # assign data as temporary variable; ffsave can only store individual ff objects, not list-nested ff objects
          tmp <- inp$time

          # save to file
          ffsave(list = "tmp", file = paste0(DirFf, "/time"), compress = TRUE)

          # clean up
          rm(DirFf, tmp)
          invisible(gc())

      msg <- paste0("dataset ", dateCalcWndw, " saving pre-processed raw data complete")
      tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
      }



    ###
    # mid: read raw data
    } else {
    msg <- paste0("dataset ", dateCalcWndw, " loading pre-processed raw data begin")
    tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
    ###



      # load pre-processed high-frequency data from daily file

        # assign input data list
        inp <- list()

        # directory for reading ffData objects
        DirFf <- paste0(Para$Flow$DirInp, "/ffData/", dateCalcWndw)

        # start loop around data, qfqm
        for(Var01 in c("data", "qfqm")){
        # Var01 <- c("data", "qfqm")[1]

          # start loop around instruments
          for(Var02 in c("irgaTurb", "mfcSampTurb", "soni", "valvValiNemaTurb")){
          # Var02 <- c("amrs", "irgaTurb", "mfcSampTurb", "soni", "valvValiNemaTurb")[1]

            # qfqm does not have valvValiNemaTurb data.frame
            if(Var01 == "qfqm" && Var02 == "valvValiNemaTurb") next

            # load from file
            ffload(file = paste0(DirFf, "/", Var01, "_", Var02), overwrite = TRUE)

            # assign to inp list
            inp[[Var01]][[Var02]] <- tmp

            # clean up
            rm(tmp)
            invisible(gc())

          # end loop around instruments
          }; rm(Var02)

        # end loop around data, qfqm
        }; rm(Var01)

        # load time

          # load from file
          ffload(file = paste0(DirFf, "/time"), overwrite = TRUE)

          # assign to inp list
          inp$time <- tmp

          # clean up
          rm(DirFf, tmp)
          invisible(gc())



    ###
    msg <- paste0("dataset ", dateCalcWndw, " loading pre-processed raw data complete")
    tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
    }
    # end: read raw data
    ###



    # assign daily data and attributes to file-backed objects; RAM 2.8 GB

      # case #1: first day (creation)
      if(numDateCalcWndw == 1) {

        # data$temp$irgaTurb
        data$irgaTurb <- inp$data$irgaTurb
        qfqmFlag$irgaTurb <- inp$qfqm$irgaTurb
        vali$irgaTurb <- inp$vali$irgaTurb

        # data$temp$mfcSampTurb
        data$mfcSampTurb <- inp$data$mfcSampTurb
        qfqmFlag$mfcSampTurb  <- inp$qfqm$mfcSampTurb

        # data$temp$mfcSampTurb
        data$valvValiNemaTurb <- inp$data$valvValiNemaTurb
        #qfqmFlag$valvValiNemaTurb  <- inp$qfqm$valvValiNemaTurb

        # data$temp$soni
        data$soni <- inp$data$soni
        qfqmFlag$soni <- inp$qfqm$soni

        # # data$temp$amrs
        # data$amrs <- inp$data$amrs
        # qfqmFlag$amrs <- inp$qfqm$amrs

        # time objects
        data$time <- inp$time
        qfqmFlag$time <- inp$time

      # case #2: subsequent day (appending)
      } else {

        # option using ffdfrbind.fill()
        # internally uses ffdfappend(), in addition fills columns that are empty in one of the ffdf objects, and controls generation of clones
        # potentially more robust

          # data
          data$irgaTurb <- ffbase::ffdfrbind.fill(data$irgaTurb, inp$data$irgaTurb, clone = FALSE)
          data$mfcSampTurb <- ffbase::ffdfrbind.fill(data$mfcSampTurb, inp$data$mfcSampTurb, clone = FALSE)
          data$valvValiNemaTurb <- ffbase::ffdfrbind.fill(data$valvValiNemaTurb, inp$data$valvValiNemaTurb, clone = FALSE)
          data$soni <- ffbase::ffdfrbind.fill(data$soni, inp$data$soni, clone = FALSE)
          # data$amrs <- ffbase::ffdfrbind.fill(data$amrs, inp$data$amrs, clone = FALSE)
          data$time <- ffbase::ffdfrbind.fill(data$time, inp$time, clone = FALSE)

          #validation data
          vali$irgaTurb <- ffbase::ffdfrbind.fill(vali$irgaTurb, inp$vali$irgaTurb, clone = FALSE)

          # qfqm
          qfqmFlag$irgaTurb <- ffbase::ffdfrbind.fill(qfqmFlag$irgaTurb, inp$qfqm$irgaTurb, clone = FALSE)
          qfqmFlag$mfcSampTurb <- ffbase::ffdfrbind.fill(qfqmFlag$mfcSampTurb, inp$qfqm$mfcSampTurb, clone = FALSE)
          # qfqmFlag$valvValiNemaTurb <- ffbase::ffdfrbind.fill(qfqmFlag$valvValiNemaTurb, inp$qfqm$valvValiNemaTurb, clone = FALSE)
          qfqmFlag$soni <- ffbase::ffdfrbind.fill(qfqmFlag$soni, inp$qfqm$soni, clone = FALSE)
          # qfqmFlag$amrs <- ffbase::ffdfrbind.fill(qfqmFlag$amrs, inp$qfqm$amrs, clone = FALSE)
          qfqmFlag$time <- ffbase::ffdfrbind.fill(qfqmFlag$time, inp$time, clone = FALSE)

          # # option using ffdfappend() directly
          #
          #   # data
          #   data$irgaTurb <- ffbase::ffdfappend(x = data$irgaTurb, dat = inp$data$irgaTurb)
          #
          #   #qfqm
          #   qfqmFlag$irgaTurb <-  ffbase::ffdfappend(x =qfqmFlag$irgaTurb, dat = inp$qfqm$irgaTurb)

      }

      # clean up
      inp <- NULL
      inpQfqm <- NULL
      invisible(gc())



  ###
  }
  # end: loop around days in current planar-fit window
  ###


  ###
  # begin: derived quantities (experimental)
  msg <- paste0("dataset ", dateCalcCntr, " derived quantities (experimental) begin")
  tryCatch({rlog$info(msg)}, error=function(cond){print(msg)})
  ###

    # soni

      # wind vector

        # planar fit rotation

          # determine planar fit coefficients if at least one of the pre-provided values is NA
          # TODO: consider 30-min pre-aggregation + robust regression
          # TODO: consider tower shadowing
          # TODO: move conditional statement into eddy4R.turb::eddy4R.turb::def.pf.derv.coef()
          if(is.na(Para$Sci$`Pf$AngEnuYaxs`) | is.na(Para$Sci$`Pf$AngEnuXaxs`) | is.na(Para$Sci$`Pf$Ofst`)) {

            # subset permissible sensor flags (and optionally individual days)
            # TODO: currently set to != 1 because golden dataset includes mostly -1
            # once golden dataset updated, change logic to == 0
            # idx01 <- which(# as.Date(data$time$UTC)[] == dateCalc[9] &
            #                  qfqmFlag$soni$qfSoniCode[] != 1 &
            #                  qfqmFlag$soni$qfSoniComm[] != 1 &
            #                  qfqmFlag$soni$qfSoniData[] != 1 &
            #                  qfqmFlag$soni$qfSoniSgnlHigh[] != 1 &
            #                  qfqmFlag$soni$qfSoniSgnlLow[] != 1 &
            #                  qfqmFlag$soni$qfSoniSgnlPoor[] != 1 &
            #                  qfqmFlag$soni$qfSoniTemp[] != 1 &
            #                  qfqmFlag$soni$qfSoniTrig[] != 1 &
            #                  qfqmFlag$soni$qfSoniUnrs[] != 1)

            # subset by non-tower-shadow
            # threshold values for CPER
            # idx02 <- which(data$soni$angZaxsErth[] < def.unit.conv(data = 90 - 10, unitFrom = "deg", unitTo = "rad", MethGc = FALSE) |
            #                  data$soni$angZaxsErth[] > def.unit.conv(data = 90 + 35 + 10, unitFrom = "deg", unitTo = "rad", MethGc = FALSE))

            # combine idx01 and idx02 using %in%
            # idx <- idx01[which(idx01 %in% idx02)]
            #idx <- idx01

            # failsafe: test that greater than 2 non-NA data entries (required by lm.fit function) must exists in all of veloXaxs, veloYaxs, veloZaxs, and that length(idx) > 0
            if(length(intersect(intersect(which(!is.na(data$soni$veloXaxs[])),which(!is.na(data$soni$veloYaxs[]))),which(!is.na(data$soni$veloZaxs[])))) > 2) {

              # determine planar fit coefficients in units radians and m s-1
              coefPf <- eddy4R.turb::def.pf.derv.coef(veloXaxs = data$soni$veloXaxs[],
                                                      veloYaxs = data$soni$veloYaxs[],
                                                      veloZaxs = data$soni$veloZaxs[])

            } else {

              coefPf <- data.frame(AngEnuYaxs = 0, AngEnuXaxs = 0, Ofst = 0)
              dimnames(coefPf)[[1]] <- ""

            }

          # assign results in units radians and m s-1
          Para$Sci$`Pf$AngEnuYaxs` <- coefPf$AngEnuYaxs #alpha
          Para$Sci$`Pf$AngEnuXaxs` <- coefPf$AngEnuXaxs #beta
          Para$Sci$`Pf$Ofst` <- coefPf$Ofst #Offset

          # clean up
          rm(coefPf, idx, idx01)

          }

          # # data analysis
          #
          #   # wind direction
          #   def.unit.conv(data = mean(data$soni$angZaxsErth[idx,], na.rm = TRUE), unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
          #   # plot(def.unit.conv(data = data$soni$angZaxsErth[idx,], unitFrom = "rad", unitTo = "deg", MethGc = FALSE), type = "l")
          #
          #   # 3-D wind speed
          #   plot(data$soni$veloXaxs[idx,], type = "l")
          #   lines(data$soni$veloYaxs[idx,], col=2)
          #   lines(data$soni$veloZaxs[idx,], col=4)
          #
          #   # Pfit coefficients
          #   def.unit.conv(data = coefPf$angEnuYaxs, unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
          #   def.unit.conv(data = coefPf$EnuXaxs, unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
          #   coefPf$Ofst

          # apply planar fit rotation
          data$tmpPf <- ff:::as.ffdf.data.frame(eddy4R.turb::def.pf.rot(
            # measured wind velocity vector [m s-1]
            veloWind = data.frame(
              veloXaxs=data$soni$veloXaxs[],
              veloYaxs=data$soni$veloYaxs[],
              veloZaxs=data$soni$veloZaxs[]
            ),
            # pitch rotation angle [rad] - Alpha
            AngEnuYaxs = Para$Sci$`Pf$AngEnuYaxs`,
            # roll rotation angle [rad] - Beta
            AngEnuXaxs = Para$Sci$`Pf$AngEnuXaxs`,
            # vertical wind offset from regression [m s-1] - Offset
            Ofst = Para$Sci$`Pf$Ofst`
          ))

        # assign meteorological wind vector
        # TODO: need to preserve attributes(data$soni)$unit when assigning ff::as.ff()
        data$soni$veloXaxsPf <- ff::as.ff(data$tmpPf$veloXaxs)
          base::attr(x = data$soni$veloXaxsPf, which = "unit") <- "m s-1"
        data$soni$veloYaxsPf <- ff::as.ff(data$tmpPf$veloYaxs)
          base::attr(x = data$soni$veloYaxsPf, which = "unit") <- "m s-1"
        data$soni$veloZaxsPf <- ff::as.ff(data$tmpPf$veloZaxs)
          base::attr(x = data$soni$veloZaxsPf, which = "unit") <- "m s-1"

        # clean up
        data$tmpPf <- NULL
        invisible(gc())

      # # omit CSAT3 data when flow is through tower structure or other booms and sensors
      # # permissible: 59...269 degree (include 10 degree safety margin on both sides)
      # # loosing approx. 10% of data
      # crit <- def.pol.cart(matrix(c(rawdata$veloYaxs, rawdata$u_met), ncol=2))
      # whr_not <- which(crit < Para$Sci$dp0p$soni$AngNedZaxs - (180 - 25 - 10) | crit > Para$Sci$dp0p$soni$AngNedZaxs + ( 90 - 15 - 10))
      # whr_col <- grep(c("SONIC|MET"), names(rawdata))
      # whr_col <- names(rawdata)[whr_col][!(names(rawdata)[whr_col] %in% "QF_SONIC")]
      # if(length(whr_not) > 0) rawdata[whr_not,whr_col] <- NA
      # rm(whr_not, whr_col)

    # irgaTurb

      # # omit irgaTurb data when flow rate is below specification
      # var <- c(
      #   "FD_mole_CO2_7200", "FD_mole_H2O_7200", "FW_mole_CO2_7200", "FW_mole_H2O_7200",
      #   "rho_mole_CO2_7200", "rho_mole_CO2_comp_p_7200", "rho_mole_CO2_comp_T_7200", "rho_mole_CO2_comp_Tp_7200", "densMoleAirDry",
      #   "rho_mole_H2O_7200", "rho_mole_H2O_comp_p_7200", "rho_mole_H2O_comp_T_7200", "rho_mole_H2O_comp_Tp_7200"
      # )
      # whr_not <- which(rawdata$Q_SLPM_7200 < 10)
      # if(length(whr_not) > 0) rawdata[whr_not,var] <- NA
      # rm(var, whr_not)

  ###
  # end: derived quantities (experimental)
  ###

        #implement validation of irgaTurb
        msg <- paste0("dataset ", dateCalcCntr, " retrieving validation coefs from cdsWebApp")
        tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
        tmpVali <- list()
        FracSlp <- c(0.70, 1.5)
        
        # ----------------------------------------------------------------------
        # **TEMPORARY**: NEED THE FOLLOWING JUST FOR RELEASE-2027 TO RE-CREATE 
        # VALI TABLE AND ADD IT TO h5 FILES. 
        # TO DO: REMOVE FROM FUTURE RELEASES AND IMPORT/SAVE VALI TABLE DURING 
        # RUNNING OF flow/flow.turb/flow.turb.irga.vali.r
        
        varNames <- c("rtioMoleDryCo2", "rtioMoleDryH2o")
        maxAttempts <- c(2, 1)
        
        tmpValiRfmt <- lapply(seq_along(varNames), 
                              function(idx) {
                                
                                tmpSubVali <- eddy4R.base::wrap.irga.vali(
                                  data = vali, 
                                  qfqmFlag = qfqmFlag, 
                                  gasRefe = Para$Cal[[dateCalcCntr]], 
                                  DateProc = dateCalcCntr, 
                                  varName = varNames[idx],
                                  ScalMax = FALSE,
                                  FracSlp = c(0.70, 1.5), 
                                  OfstMax = FALSE, 
                                  valiSeMax = 0.07,
                                  evalSlpMax = 1.05,
                                  evalSlpMin = 0.95,    
                                  evalOfstMax = 100,
                                  evalOfstMin = -100, 
                                  maxAttempts = maxAttempts[idx]
                                )
                                
                                if (varNames[idx] == "rtioMoleDryCo2") {
                                  
                                  tmpRtioMoleDryCo2Mlf <- data.frame(coef = tmpSubVali$valiPrd_1$modelFit$coef[1:2], 
                                                                     se = tmpSubVali$valiPrd_1$modelFit$se[1:2], 
                                                                     scal = c(tmpSubVali$valiPrd_1$modelFit$coef[3], NA),
                                                                     qfEvalThsh = c(NA, tmpSubVali$valiPrd_1$valiEvalPass), 
                                                                     qfGasRmv = c(NA, tmpSubVali$valiPrd_1$qfGasRmv), 
                                                                     qfValiCrit = c(NA, ifelse(length(tmpSubVali) < 2, 0, 1)), 
                                                                     evalCoef = tmpSubVali$valiPrd_1$evalCoef, 
                                                                     evalCoefSe = tmpSubVali$valiPrd_1$evalCoefSe, 
                                                                     evalSlpThsh = c(0.95, 1.05), 
                                                                     evalOfstThsh = c(-100, 100))
                                  
                                  tmpSubValiDf <- lapply(tmpSubVali, function(x) x$data)
                                  
                                  tmpSubValiDf <- do.call(rbind, tmpSubValiDf)
                                  rownames(tmpSubValiDf) <- NULL
                                  
                                  list(mlf = tmpRtioMoleDryCo2Mlf, df = tmpSubValiDf)
                                  
                                } else if (varNames[idx] == "rtioMoleDryH2o") {
                                  
                                  tmpSubVali[[1]]$data
                                  
                                }
                                
                              })
        
        names(tmpValiRfmt) <- varNames
        
        tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf <- tmpValiRfmt$rtioMoleDryCo2$mlf
        tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Vali <- tmpValiRfmt$rtioMoleDryCo2$df
        tmpVali[[dateCalcCntr]]$rtioMoleDryH2oVali <- tmpValiRfmt$rtioMoleDryH2o
        
        rm(tmpValiRfmt, varNames, maxAttempts)
        # ----------------------------------------------------------------------
        
        
        tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor <- vali$irgaTurb
        
        start_date_m15_str <- paste0(as.character(as.Date(dateCalcCntr) - 15), "T00:00:00Z")
        start_date_m1_str <- paste0(as.character(as.Date(dateCalcCntr) - 1), "T00:00:00Z")
        end_date_1_str <- paste0(as.character(as.Date(dateCalcCntr) + 1), "T23:59:59Z")
        
        # Need to get recent installs of 7200, don't want to use last valid 
        # validation if an install occurred afterwards as it will be unreliable. 
        # Find LI-7200 install dates by matching to "7200" to model number field.
        recentAssetInstalls <- accs::get.assetinstalls.bysite(resturl = resturl, 
                                                              site = Para$Site$Loc, 
                                                              startdate = start_date_m15_str, 
                                                              enddate = end_date_1_str)
        
        if (!is.null(recentAssetInstalls) && nrow(recentAssetInstalls) > 0) {
          
          recentAssetInstalls <- recentAssetInstalls[grepl(pattern = "7200", recentAssetInstalls$modelNumber), ]
          rownames(recentAssetInstalls) <- NULL
          
          installDatesPosix <- as.POSIXct(recentAssetInstalls$installdate, 
                                          format = "%Y-%m-%dT%H:%M:%S", 
                                          tz = "UTC")
          
        } else {
          
          # Non-sensical date to fill in to ensure it is not empty for last
          # valid validation tests/conditions
          installDatesPosix <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
          
        }
        
        valiResults <- accs::get.validation.bydaterange(
          resturl = resturl, 
          site = Para$Site$Loc,
          ectype = ectype,
          start_date = start_date_m1_str,
          end_date = end_date_1_str
        )
        
        qfLastValid <- NULL

        # Check to make sure no empty length or null vectors are passed along, 
        # use NA instead. Expecting at least 3 validation periods to be read in
        # (even if qfRngVali == -1). If not available then don't do the 
        # validation
        if (!is.null(valiResults) && nrow(valiResults) >= 3) {
          
          xNum <- as.numeric(strptime(valiResults$date, 
                                      format = "%Y-%m-%dT%H:%M:%OS", 
                                      tz = "UTC"))
          
          qfRngVali <- as.numeric(valiResults$validationRangeQf)
          offstVals <- as.numeric(valiResults$validationOffset)
          slopeVals <- as.numeric(valiResults$validationSlope)
          
          idxBadVali <- which(is.na(qfRngVali) | qfRngVali != 0)
          
          offstVals[idxBadVali] <- NA_real_
          slopeVals[idxBadVali] <- NA_real_
          
          # Needed below to identify whether extra qfIrgaTurbGas flags should
          # be raised in period around validation or not. If last-valid 
          # validation is used for center date (dateCalcCntr) then these
          # flags should not be raised because time stamp value will in many 
          # cases
          qfLastValid <- as.numeric(valiResults$validationRangeQf)
          qfLastValid[is.na(qfLastValid)] <- -1
          
        } else {
          
          xNum <- NA_real_
          qfRngVali <- NA_real_
          offstVals <- NA_real_
          slopeVals <- NA_real_
          
          idxBadVali <- numeric(length = 0) # Will skip the following for loop
          
        }
        
        # For each bad validation period, attempt to get last valid validation
        # to see if we can use that instead.
        for (idx in idxBadVali) {
          # idx <- idxBadVali[1] # For testing
          
          # Retrieve last valid validation for a period that did not pass
          tmpLastValidVali <- accs::get.last.valid.validation(
            resturl = resturl, 
            site = Para$Site$Loc, 
            ectype = ectype, 
            date = valiResults$date[idx]
          )
          
          # If the last valid validation is not NA, do subsequent checks:
          #
          # 1. Does the last valid validation occur prior to our two-week
          #    look back period? If so, do not use and go to next iteration in 
          #    loop
          #
          # 2. Was the LI-7200 replaced after the most recent valid validation? 
          #    If so do not use it and go to next iteration.
          #
          # If all these checks pass, then re-assign bad or missing offst and 
          # slope value in current valiResults table to those in the last valid 
          # validation,but keep timestamps the same.
          
          lastValidValiExists <- !is.null(tmpLastValidVali) &&
            "date" %in% names(tmpLastValidVali) &&
            length(tmpLastValidVali$date) > 0 &&
            !is.na(tmpLastValidVali$date[1])

          if (lastValidValiExists) {
            
            tmpLastValidValiDateTime <- as.POSIXct(tmpLastValidVali$date[1], 
                                                   format = "%Y-%m-%dT%H:%M:%S", 
                                                   tz = "UTC")
            start_date_m15_DateTime <- as.POSIXct(start_date_m15_str, 
                                                  format = "%Y-%m-%dT%H:%M:%S", 
                                                  tz = "UTC")
            current_DateTime <- as.POSIXct(valiResults$date[idx], 
                                       format = "%Y-%m-%dT%H:%M:%S", 
                                       tz = "UTC")
            
            # Only use the last valid validation if both the following 
            # conditions fail:
            # Last validation is too old
            cond1 <- tmpLastValidValiDateTime <= start_date_m15_DateTime
            # LI-7200 was installed after the most recent last-valid validation
            cond2 <- any(installDatesPosix > tmpLastValidValiDateTime & 
                           installDatesPosix < current_DateTime, na.rm = TRUE)
            
            # If either of those conditions exist, skip and move to next 
            # iteration
            if (cond1 || cond2) next 
            
            # reassign values if above checks pass
            offstVals[idx] <- as.numeric(tmpLastValidVali$validationOffset[1])
            slopeVals[idx] <- as.numeric(tmpLastValidVali$validationSlope[1])
            
            # Flag to indicate that last valid validation was used
            qfLastValid[idx] <- -1
            
          }
          
        }
        
        options(digits = 15) # Testing to confirm fractional seconds are used
        
        xOutNum <- as.numeric(vali$irgaTurb$time[])
                
        # Final check to make sure slope is within FracSlp if other checks pass
        slopeVals[slopeVals < FracSlp[1] | slopeVals > FracSlp[2]] <- NA_real_
        
        # Final double check to make sure zero-length vectors don't accidentally
        # allow na.approx to attempt interpolation in following lines for offset 
        # or slope. First offset or slope coef value can be NA/missing if the 
        # rest are available as this is during the day prior to DateProc. This 
        # is considering a situation where there is an extended period of no 
        # validations, and then once it starts working again the day prior might 
        # not have a last-valid one but we still want to allow the validation on 
        # current day to proceed.
        
        canInterpOffset <- (
          length(xNum) > 1 &&
            length(offstVals) > 1 &&
            length(xNum) == length(offstVals) &&
            all(!is.na(xNum)) &&
            all(!is.na(offstVals[-1]))
        )
        
        if (canInterpOffset) {
          offstInterp <- zoo::na.approx(offstVals, x = xNum, xout = xOutNum, 
                                        na.rm = FALSE, rule = 2)
        } else {
          offstInterp <- rep(NA_real_, length(xOutNum))
        }
        
        canInterpSlope <- (
          length(xNum) > 1 &&
            length(slopeVals) > 1 &&
            length(xNum) == length(slopeVals) &&
            all(!is.na(xNum)) &&
            all(!is.na(slopeVals[-1]))
        )
        
        if (canInterpSlope) {
          slopeInterp <- zoo::na.approx(slopeVals, x = xNum, xout = xOutNum, 
                                        na.rm = FALSE, rule = 2)
        } else {
          slopeInterp <- rep(NA_real_, length(xOutNum))
        }
        
        # Apply interpolation to 20Hz for offset and slope values
        n <- nrow(vali$irgaTurb)
        tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryCo2Cor <- ff(vmode = "double", length = n)
        tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryCo2Cor[] <- offstInterp + tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryCo2[]*slopeInterp
        
        # Remove validation sampling periods from the corrected Co2 data. 
        # Will need to be adjusted in future if h2o validations and corrections 
        # are applied (e.g. offset correction or via Picarro comparison)
        gasRefeNames <- c(
          "qfIrgaTurbValiGas02",
          "qfIrgaTurbValiGas03",
          "qfIrgaTurbValiGas04",
          "qfIrgaTurbValiGas05"
          )
        
        # Option 1: Use 'qfIrgaTurbVali' flags to do this removal. This captures 
        # all parts of the validation sampling. Refer to 
        # eddy4R.base::wrap.derv.prd.day.R for details.
        tmpQfVali01 <- !is.na(qfqmFlag$irgaTurb$qfIrgaTurbVali[]) &
          qfqmFlag$irgaTurb$qfIrgaTurbVali[] == 1
        
        # Option 2: use gases that indicate validation periods: 
        # 'qfIrgaTurbGas02, ..., 'qfIrgaTurbGas05'
        tmpQfVali02 <- unname(rowSums(qfqmFlag$irgaTurb[, gasRefeNames], 
                                          na.rm = TRUE) > 0)
        
        # Choose one (or select both via '|')
        tmpQfVali <- tmpQfVali01 | tmpQfVali02
        
        # Clean up
        rm(tmpQfVali01, tmpQfVali02)
        
        # ----------------------------------------------------------------------
        # **TEMPORARY (07/2026)**: AS A FINAL CHECK, NEED TO REMOVE THE HIGH 
        # FREQUENCY MEASUREMENTS FOR FINAL 3.5 MINUTES OF THE VALIDATION PERIOD 
        # THIS SHHOULD REMOVE MOST OF THE REMNANTS OF ANY HIGH REFERENCE GAS
        # FROM NON-VALIDATION PERIOD. TOTAL TIME PERIOD TO EXCLUDE IS 23.5 MIN.
        # 
        # TO DO: ADD TO MORE APPROPRIATE FUNCTION (e.g., 
        #   eddy4R.base::def.qf.irga.vali.R)
        if (canInterpSlope && canInterpOffset && !is.null(qfLastValid)) {
          
          # Empty ff vector of zeros
          extraQfVali <- rep(0,  n) #ff(vmode = "double", length = n) 
          extraQfValiPrd <- which(as.Date(valiResults$date) == dateCalcCntr)
          
          for (idxExtraQfVali in extraQfValiPrd) {
            # idxExtraQfVali <- extraQfValiPrd[1] # For testing
            
            # 3.5 minutes prior to start of validation period. This is will
            # be already covered by removing qfIrgaTurbValiGas values
            # previously but acts as an extra check. 
            timeBgnExtraQf <- as.POSIXct(valiResults$date[idxExtraQfVali], 
                                         format = "%Y-%m-%dT%H:%M:%S", 
                                         tz = "UTC") - 60.0*3.5
            
            # 3.5 minutes after validation period ends. This period is currently
            # not being flagged by the qfIrgaTurbValiGas variables previously 
            # applied. It allows a sample of the high reference gas to be 
            # included which is skewing the aggregation results.
            timeEndExtraQf <- as.POSIXct(valiResults$date[idxExtraQfVali], 
                                         format = "%Y-%m-%dT%H:%M:%S", 
                                         tz = "UTC") + 60.0*20.0
            
            # This check is probably unnecessary, but just to be sure, if the 
            # gap is greater than expected 23.5 minutes do not apply, go to next 
            # iteration/validation period. 
            #
            # Add 1/20 second as a small buffer for edge cases.
            # 
            # Also, do not attempt if the last valid validation was used for 
            # this date (dateCalcCntr), as the time stamp is automatically set 
            # to start of day and will just potentially flag good data. If there
            # is a validation period that was originally available but did not
            # work (==1) then you can still go ahead and add additional quality 
            # flags. Only relevent when == -1 as the time stamp will be 
            # '00:00:00' at the start of the day.
            
            skipExtraFlag <- 
              (timeEndExtraQf - timeBgnExtraQf) > (60*23.5 + 1/20) || 
              qfLastValid[idxExtraQfVali] == -1
            
            if (skipExtraFlag) {
              
              rm(timeBgnExtraQf, timeEndExtraQf)
              next
              
            }
            
            idxToFlag <- which(
              tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$time[] >= timeBgnExtraQf & 
                tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$time[] <= timeEndExtraQf
            )
            
            extraQfVali[idxToFlag] <- 1
            
            tmpQfVali <- tmpQfVali | extraQfVali == 1
            
            rm(timeBgnExtraQf, timeEndExtraQf)
            
          }
          
          rm(idxExtraQfVali, extraQfVali, extraQfValiPrd)
          
        }
        # ----------------------------------------------------------------------
        
        # Apply quality flags to rtioMoleDryCo2Cor
        tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryCo2Cor[][tmpQfVali] <- NA_real_
        
        # Set empty values for h2oTurb.rtioMoleDryH2oCor
        tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryH2oCor <- ff(vmode = "double", length = n)
        tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryH2oCor[] <- NA_real_
        
        # Set attributes/units for rtioMoleDryH2oCor and rtioMoleDryCo2Cor
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryCo2Cor)$virtual <- attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryCo2)$virtual
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryCo2Cor)$unit <- attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryCo2)$unit
        
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryH2oCor)$virtual <- attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryH2o)$virtual
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryH2oCor)$unit <- attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryH2o)$unit
        
        rm(xNum, xOutNum, offstVals, slopeVals, offstInterp, slopeInterp, n, tmpQfVali)
        
        # Add coefficient values to attributes for hdf5 files
        
        # First convert offset coefficient values to proper units of umolCo2 mol-1
        # Offset coefficient
        tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$coef[1] <- eddy4R.base::def.unit.conv(data = tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$coef[1],
                                                                                        unitFrom = "intl",
                                                                                        unitTo = "umol mol-1")
        tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$evalCoef[1] <- eddy4R.base::def.unit.conv(data = tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$evalCoef[1],
                                                                                            unitFrom = "intl",
                                                                                            unitTo = "umol mol-1")
        
        # Offset coefficient standard errors
        tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$se[1] <- eddy4R.base::def.unit.conv(data = tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$se[1],
                                                                                      unitFrom = "intl",
                                                                                      unitTo = "umol mol-1")
        tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$evalCoefSe[1] <- eddy4R.base::def.unit.conv(data = tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$evalCoefSe[1],
                                                                                            unitFrom = "intl",
                                                                                            unitTo = "umol mol-1")
        
        # Add values for hdf5 files
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Vali)$valiCoef <- as.numeric(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$coef)
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Vali)$valiCoefSe <- as.numeric(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$se)
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Vali)$scal <- as.numeric(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$scal[1])
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Vali)$qfEvalThsh <- as.integer(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$qfEvalThsh[2])
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Vali)$qfGasRmv <- as.integer(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$qfGasRmv[2])
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Vali)$qfValiCrit <- as.integer(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$qfValiCrit[2])
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Vali)$evalCoef <- as.numeric(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$evalCoef)
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Vali)$evalCoefSe <- as.numeric(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$evalCoefSe)
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Vali)$evalSlpThsh <- as.numeric(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$evalSlpThsh)
        attributes(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Vali)$evalOfstThsh <- as.numeric(tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Mlf$evalOfstThsh)
        
        
        # determine indices for dateCalcCntr
        tmpVali$idx$irgaTurb <- eddy4R.base::def.idx.agr(time = tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$time, PrdAgr = 1800, FreqLoca = 20)
        invisible(gc())
        msg <- paste0("dataset ", dateCalcCntr, " applying the validation complete")
        tryCatch({rlog$info(msg)}, error=function(cond){print(msg)})
        #get rid of data$valvValiNemaTurb
        #data$valvValiNemaTurb <- NULL



  # working parameters and variables

    # assign list for working parameters and variables
    wrk <- list()
    wrk$dp01 <- list()
    wrk$dp01AgrSub <- list()
    waveOutSpec <- list()

    #Assign list for outputs from qfqm
    qfqm <- list()


    # begin and end time for each averaging interval
    invisible(lapply(names(data)[which(!(names(data) %in% c("valvValiNemaTurb")))], function(x) {

      # indices for entire dataset
      if(x == "amrs") {
        wrk$idx[[x]] <<- eddy4R.base::def.idx.agr(time = data$amrs$time, PrdAgr = 1800, FreqLoca = 40)
      } else {
        wrk$idx[[x]] <<- eddy4R.base::def.idx.agr(time = data$time$UTC, PrdAgr = 1800, FreqLoca = 20)
      }

      # subset for output date
      # TODO: needs to produce daily files for length(Para$Flow$DateOut) > 1
      wrk$idx[[x]] <<- wrk$idx[[x]][which(as.Date(wrk$idx[[x]]$timeBgn) == dateCalcCntr),]

      }))

    # determine the output periods to run depending on Para$Flow$OutSub.
    # if is.na(Para$Flow$OutSub) then all output periods are processed, else only those specified in Para$Flow$OutSub
    if(base::any(base::is.na(Para$Flow$OutSub))) Para$Flow$OutSub <- 1:max(sapply(names(wrk$idx), function(x) length(wrk$idx[[x]]$idxBgn)))



  ###
  # begin: loop around aggregation interval: 60 s
  msg <- paste0("dataset ", dateCalcCntr, " DP01 calculation begin")
  tryCatch({rlog$info(msg)}, error=function(cond){print(msg)})
  numAgr <- 0
  for(idxAgr in Para$Flow$OutSub) {
  # idxAgr <- 1
  numAgr <- numAgr + 1
  # create a list identifier for the Aggregation loops
  lvlAgr <- paste0("numAgr", ifelse(numAgr < 10, paste0("0",numAgr) ,numAgr))
  ###

  msg <- paste0("Para$Flow$OutSub loop numAgr: ", numAgr)
  tryCatch({rlog$info(msg)}, error=function(cond){print(msg)})

    # assign list
    wrk$data <- list()

    # reduce observations to current aggregation interval
    # loop around sensors
    for(idxSens in names(data)[which(!(names(data) %in% c("valvValiNemaTurb")))]){

      # assign data
      wrk$data[[idxSens]] <- data[[idxSens]][wrk$idx[[idxSens]]$idxBgn[idxAgr]:wrk$idx[[idxSens]]$idxEnd[idxAgr],]
      wrk$qfqm[[idxSens]] <- qfqmFlag[[idxSens]][wrk$idx[[idxSens]]$idxBgn[idxAgr]:wrk$idx[[idxSens]]$idxEnd[idxAgr],]

      # assign units around variables
      # units are present for POSIXct, but don't show in str()
      for(idxVar in base::names(wrk$data[[idxSens]])) {

        base::attr(x = wrk$data[[idxSens]][[idxVar]], which = "unit") <-
          base::attr(x = data[[idxSens]][[idxVar]], which = "unit")

      }; rm(idxVar)

    }; rm(idxSens)

    #adding rtioMoleDryCo2Cor and rtioMoleDryH2oCor
    wrk$data$irgaTurb$rtioMoleDryCo2Cor <- tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryCo2Cor[tmpVali$idx$irgaTurb$idxBgn[idxAgr]:tmpVali$idx$irgaTurb$idxEnd[idxAgr]]
    wrk$data$irgaTurb$rtioMoleDryH2oCor <- tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryH2oCor[tmpVali$idx$irgaTurb$idxBgn[idxAgr]:tmpVali$idx$irgaTurb$idxEnd[idxAgr]]
    #copied raw rtioMoleDryCo2 to rtioMoleDryCo2Raw
    wrk$data$irgaTurb$rtioMoleDryCo2Raw <- wrk$data$irgaTurb$rtioMoleDryCo2
    wrk$data$irgaTurb$rtioMoleDryH2oRaw <- wrk$data$irgaTurb$rtioMoleDryH2o
    #replaced the original (raw rtioMoleDryCo2) by rtioMoleDryCo2Cor
    wrk$data$irgaTurb$rtioMoleDryCo2 <- wrk$data$irgaTurb$rtioMoleDryCo2Cor
    #Note: rtioMoleDryH2o are the original (raw rtioMoleDryCo2) during sampling NaN during validation; keeping as raw initially
    #these will replace later when implementing the validation of rtioMoleDryH2o

    #wrk$data$irgaTurb$rtioMoleDryH2o <- tmpVali[[datePfCntr]]$rtioMoleDryCo2Cor$rtioMoleDryH2o[tmpVali$idx$irgaTurb$idxBgn[idxAgr]:tmpVali$idx$irgaTurb$idxEnd[idxAgr]]

    #assign unit
    base::attr(x = wrk$data$irgaTurb$rtioMoleDryCo2Cor, which = "unit") <-  base::attr(x = tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryCo2Cor, which = "unit")
    base::attr(x = wrk$data$irgaTurb$rtioMoleDryH2oCor, which = "unit") <-  base::attr(x = tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Cor$rtioMoleDryH2oCor, which = "unit")
    base::attr(x = wrk$data$irgaTurb$rtioMoleDryCo2Raw, which = "unit") <-  base::attr(x = data$irgaTurb$rtioMoleDryCo2, which = "unit")
    base::attr(x = wrk$data$irgaTurb$rtioMoleDryH2oRaw, which = "unit") <-  base::attr(x = data$irgaTurb$rtioMoleDryH2o, which = "unit")
    base::attr(x = wrk$data$irgaTurb$rtioMoleDryCo2, which = "unit") <-  base::attr(x = data$irgaTurb$rtioMoleDryCo2, which = "unit")
    base::attr(x = wrk$data$irgaTurb$rtioMoleDryH2o, which = "unit") <-  base::attr(x = data$irgaTurb$rtioMoleDryH2o, which = "unit")

    # overview
    # plot(wrk$data$soni$veloXaxs, type="l")
    # str(data)


    # lag-time correction

      # select variables for which to perform
      var <- c("rtioMoleDryCo2", "rtioMoleDryH2o", "rtioMassH2o", "presH2o", 
               "rtioMoleDryCo2Cor", "rtioMoleDryCo2Raw", "rtioMoleDryH2oCor", "rtioMoleDryH2oRaw")

      ###
      # begin: loop around lag correction variables
      tmpRun <- 0
      for(idxVar in var) {
      # idxVar <- var[2]
      tmpRun <- tmpRun + 1
      ###

        # determine lag interactively or use pre-determined lag?
        if((base::length(base::grep(pattern = "CO2", x = idxVar, ignore.case = TRUE)) > 0 && is.na(Para$Sci$dp01$co2Turb$RtioMoleDryCo2$`Lag$TimeDiff`)) |
           (base::length(base::grep(pattern = "H2O", x = idxVar, ignore.case = TRUE)) > 0 && is.na(Para$Sci$dp01$h2oTurb$RtioMoleDryH2o$`Lag$TimeDiff`))) {

          # actual cross-correlation
          lag <- def.lag(
            refe = wrk$data$soni$veloZaxsPf,
            meas = wrk$data$irgaTurb[[idxVar]],
            # dataRefe = wrk$data$soni,
            # max. 2 s lag time
            # atm. transport time = 1 s = separation distance 0.15 m / minimum mean horizontal wind speed 0.15 m s-1
            # tube transport time = 0.15 s = volume of tube and cell 0.03 L / flow rate 12 L min-1 * 60 s min-1
            lagMax = 2 * FreqSamp$irgaTurb,
            lagCnst = TRUE,
            # only negative lags permitted (reference leads)
            lagNgtvPstv = c("n", "p", "np")[1],
            # consider positive and negative extrema
            lagAll = TRUE,
            freq = FreqSamp$irgaTurb,
            hpf = TRUE
          )

        } else {

          # assign pre-determined lag-times from Para list
          lag <- list()
          lag$lag <- ifelse(base::length(base::grep(pattern = "CO2", x = idxVar, ignore.case = TRUE)) > 0,
                               Para$Sci$dp01$co2Turb$RtioMoleDryCo2$`Lag$TimeDiff`,
                               Para$Sci$dp01$h2oTurb$RtioMoleDryH2o$`Lag$TimeDiff`)
          lag$corrCros <- NaN

        }

        # shift and reassign data
        # perform here, so all subsequent analyses (EC, spectra) can use the corrected data for any important variable
        # this means that flow rates, air densities are not shifted individually for CO2 and H2O
        # that should be alright, as no high-frequency calculations are performed anymore at this stage
        # only statistics (averages...) is calculated over marginally different samples
        tmpAttr <- attributes(wrk$data$irgaTurb[[idxVar]])$unit
        if(!is.na(lag$lag)) wrk$data$irgaTurb[[idxVar]] <- DataCombine:::shift(VarVect = wrk$data$irgaTurb[[idxVar]], shiftBy = - lag$lag, reminder = FALSE)
        attributes(wrk$data$irgaTurb[[idxVar]])$unit <- tmpAttr; rm(tmpAttr)

        # store lag times
        if(tmpRun == 1) {
          tmpLag <- lag$lag / FreqSamp$irgaTurb
          tmpCorrCros <- lag$corrCros
        } else {
          tmpLag <- c(tmpLag, lag$lag / FreqSamp$irgaTurb)
          tmpCorrCros <- c(tmpCorrCros, lag$corrCros)
        }

      ###
      # end: loop around lag correction variables
      }
      ###

      # clean up
      names(tmpLag) <- var
        tmpLag <- data.frame(t(tmpLag))
        wrk$Lag$Lag[[lvlAgr]] <- tmpLag
      names(tmpCorrCros) <- var
        tmpCorrCros <- data.frame(t(tmpCorrCros))
        wrk$Lag$CorrCros[[lvlAgr]] <- tmpCorrCros
      rm(tmpRun, lag, idxVar, var)


    # derived quantities (after synchronization)

      # fast air temperature
      wrk$data$soni$T_air_SONIC <- unlist(wrk$data$soni$tempSoni / (1 + 0.51 * wrk$data$irgaTurb$rtioMassH2o))
      base::attr(x = wrk$data$soni$T_air_SONIC, which = "unit") <- "K"

      # water vapor saturation pressure (ambient)
      if(!is.na(mean(wrk$data$soni$T_air_SONIC, na.rm=TRUE))) {

        wrk$data$irgaTurb$presH2oSatAtm <- unlist(def.pres.h2o.sat.temp.mag(temp=wrk$data$soni$T_air_SONIC))

      } else {

        wrk$data$irgaTurb$presH2oSatAtm <- rep(NaN, length(wrk$data$soni$T_air_SONIC))

      }
      base::attr(x = wrk$data$irgaTurb$presH2oSatAtm, which = "unit") <- "Pa"

      # RH (ambient) incl. adjustment of partial pressure from cell to ambient (Dalton's law)
      wrk$data$irgaTurb$rhAtm <- def.rh.pres.h2o.pres.sat.h2o(presH2o = wrk$data$irgaTurb$presH2o, presH2oSat = wrk$data$irgaTurb$presH2oSatAtm) *
        mean(wrk$data$irgaTurb$presAtm, na.rm=TRUE) / mean(wrk$data$irgaTurb$presSum, na.rm=TRUE)

      # dew point (ambient)
      wrk$data$irgaTurb$tempDew <- def.temp.dew.pres.h2o.temp.mag(presH2o = wrk$data$irgaTurb$presH2o, temp = wrk$data$soni$T_air_SONIC)


    # assemble data for Level 1 data product generation
    # can also happen after dp01 calculation

      # assign lists

        # for data
        wrk$tmp$data <- list()

        # for qfqm
        wrk$tmp$qfqm <- list()

        # for uncertainty
        wrk$tmp$ucrt <- list()

      # assemble data

        # for soni
        wrk$tmp$data$soni <- data.frame(stringsAsFactors = FALSE,
                                        "veloXaxsErth" = wrk$data$soni$veloXaxsPf,
                                        "veloYaxsErth" = wrk$data$soni$veloYaxsPf,
                                        "veloZaxsErth" = wrk$data$soni$veloZaxsPf,
                                        "veloXaxsYaxsErth" = wrk$data$soni$veloXaxsYaxsErth,
                                        "angZaxsErth" = wrk$data$soni$angZaxsErth,
                                        "tempSoni" = wrk$data$soni$tempSoni,
                                        "tempAir" = wrk$data$soni$T_air_SONIC
                                        )

        # for amrs
        # TODO: needs to be transformed from ENU to NED
        # wrk$tmp$data$amrs <- data.frame(stringsAsFactors = FALSE,
        #                                     "angNedXaxs" = wrk$data$amrs$angXaxs,
        #                                     "angNedYaxs" = wrk$data$amrs$angYaxs,
        #                                     "angNedZaxs" = wrk$data$amrs$angZaxs)

        # for co2Turb
        wrk$tmp$data$co2Turb <- data.frame(stringsAsFactors = FALSE,
                                           "rtioMoleDryCo2" = wrk$data$irgaTurb$rtioMoleDryCo2,
                                           "rtioMoleDryCo2Cor" = wrk$data$irgaTurb$rtioMoleDryCo2Cor,
                                           "rtioMoleDryCo2Raw" = wrk$data$irgaTurb$rtioMoleDryCo2Raw,
                                           "densMoleCo2" = wrk$data$irgaTurb$densMoleCo2,
                                           "presAtm" = wrk$data$irgaTurb$presAtm,
                                           "presSum" = wrk$data$irgaTurb$presSum,
                                           "frt00Samp" = wrk$data$mfcSampTurb$frt00,
                                           "tempAve" = wrk$data$irgaTurb$tempMean
                                           )

        # for h2oTurb
        wrk$tmp$data$h2oTurb <- data.frame(stringsAsFactors = FALSE,
                                           "rtioMoleDryH2o" = wrk$data$irgaTurb$rtioMoleDryH2o,
                                           "rtioMoleDryH2oCor" = wrk$data$irgaTurb$rtioMoleDryH2oCor,
                                           "rtioMoleDryH2oRaw" = wrk$data$irgaTurb$rtioMoleDryH2oRaw,
                                           "densMoleH2o" = wrk$data$irgaTurb$densMoleH2o,
                                           "tempDew" = wrk$data$irgaTurb$tempDew,
                                           "presAtm" = wrk$data$irgaTurb$presAtm,
                                           "presSum" = wrk$data$irgaTurb$presSum,
                                           "frt00Samp" = wrk$data$mfcSampTurb$frt00,
                                           "tempAve" = wrk$data$irgaTurb$tempMean
                                           )

    # calculate data products

      # 1 and 2 minute data products
      wrk$dp01AgrSub[[lvlAgr]] <- eddy4R.base::wrap.dp01.agr.prd(inpList = wrk)


      # 30-minute data products
      # call dp01 processing, assign each result as list element numAgr in wrk$dp01
      # http://stackoverflow.com/questions/26843861/replace-rbind-in-for-loop-with-lapply-2nd-circle-of-hell
      wrk$dp01[[lvlAgr]] <- eddy4R.base::wrap.dp01(
        # assign data: data.frame or list of type numeric or integer
        data = wrk$tmp$data,
        # if data is a list, which list entries should be processed into Level 1 data products?
        # defaults to NULL which expects data to be a data.frame
        idx = c("soni", "co2Turb", "h2oTurb")
      )

    # calculate uncertainty for dp01

      # loop around data products
      for(idxData in base::names(wrk$tmp$data)){
      # idxData <- base::names(wrk$tmp$data)[3]

        # calculate random sampling uncertainty

          # mean
          tmp01 <- eddy4R.ucrt::def.ucrt.samp.filt.fmt(
            data = lapply(wrk$tmp$data[[idxData]], function(x)
              eddy4R.ucrt::def.ucrt.samp.filt(data = x, PrdFilt = list(min = 10, max = 180), NumFilt = 10,
                                              Freq = ifelse(idxData == "amrs", 40, 20), prdRpt = c("01m" = 60, "30m" = 3600))),
            idxData = idxData,
            stat = "mean",
            PrdRefe = 30 * 60
          )

          # variance

            # calculate the square of the immmidiate deviations
            tmp02 <- wrk$tmp$data[[idxData]]
            base::invisible(base::sapply(base::names(tmp02), function(x) tmp02[[x]] <<- (tmp02[[x]] - base::mean(tmp02[[x]], na.rm = TRUE))^2))

            # calculate random sampling uncertainty
            tmp03 <- eddy4R.ucrt::def.ucrt.samp.filt.fmt(
              data = lapply(tmp02, function(x)
                eddy4R.ucrt::def.ucrt.samp.filt(data = x, PrdFilt = list(min = 10, max = 180), NumFilt = 10,
                                                Freq = ifelse(idxData == "amrs", 40, 20), prdRpt = c("01m" = 60, "30m" = 3600))),
              idxData = idxData,
              stat = "vari",
              PrdRefe = 30 * 60
            )

          # combine results for mean and variance
          tmp04 <- eddy4R.ucrt::appendList(x = tmp01, val = tmp03)

          # for some reason the variance data.frames are being changed to lists; revert
          tmp04[[idxData]]$"01m"$vari <- base::as.data.frame(tmp04[[idxData]]$"01m"$vari)
          tmp04[[idxData]]$"30m"$vari <- base::as.data.frame(tmp04[[idxData]]$"30m"$vari)

          # move standard error under /ucrt and remove from /data
          # TODO: move actual calculation from dp01 wrapper here

            # 1-minute
            tmp04[[idxData]]$"01m"$se <- wrk$dp01AgrSub[[lvlAgr]]$data[[idxData]]$se
            wrk$dp01AgrSub[[lvlAgr]]$data[[idxData]]$se <- NULL

            # 30-minute
            tmp04[[idxData]]$"30m"$se <- wrk$dp01[[lvlAgr]][[idxData]]$se
            wrk$dp01[[lvlAgr]][[idxData]]$se <- NULL

          # combine results for various data products
          if(idxData == base::names(wrk$tmp$data)[1]) {

            wrk$ucrt[[lvlAgr]] <- tmp04

          } else (

            wrk$ucrt[[lvlAgr]] <- eddy4R.ucrt::appendList(x = wrk$ucrt[[lvlAgr]], val = tmp04)

          )

      }; rm(idxData, tmp01, tmp02, tmp03, tmp04)


    # calculate quality metric, qmAlpha, qmBeta, qfFinl for dp01

      # message to screen
      msg <- base::paste("Beginning the qfqm data processing for data in loop...", idxAgr)
      tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})

      # actual function call
      wrk$qfqmOut[[lvlAgr]] <- eddy4R.base::wrap.dp01.qfqm.ecte(qfqm = wrk$qfqm,
                                                                    idx = c("soni","co2Turb", "h2oTurb"),
                                                                    MethMeas = "ecte",
                                                                    RptExpd = TRUE )
      #Tower shadow here!
      #Logging for tower shadow flag
      msg <- base::paste("Tower shadow testing!")
      tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
      #flag for the 30-min data
      #check 30-min wind angle and set the 30-min shadow flag
      qfShdw <- eddy4R.qaqc::def.qf.shdw(angWindMean = wrk[["dp01"]][[lvlAgr]][["soni"]][["mean"]][["angZaxsErth"]], ThshAngMin = Para$Sci$dp0p$soni$AngShdwMin, ThshAngMax = Para$Sci$dp0p$soni$AngShdwMax, Site = Para$Site$Loc, Date = dateCalcCntr, AngZaxsSoniInst = Para$Sci$dp0p$soni$AngNedZaxs)
      #wrk$qfqmOut[[lvlAgr]][['soni']][['qfshdw']]['angZaxsErth'] <- def.qf.shdw(angWindMean = wrk[["dp01"]][[lvlAgr]][["soni"]][["mean"]][["angZaxsErth"]], Site = Para$Site$Loc)
      #if tower shadow flag is 1, raise the qfFinl flag to 1
        #Testing
        # message to screen

        #Create empty data.frame with same structure
        wrk[["qfqmOut"]][[lvlAgr]][["soni"]][["qfShdw"]] <- wrk[["qfqmOut"]][[lvlAgr]][["soni"]][["qfFinl"]]
        #Set all qfShdw to 0 originally
        wrk[["qfqmOut"]][[lvlAgr]][["soni"]][["qfShdw"]][1,] <- qfShdw
        #Apply flag to qfFinl
        ifelse(qfShdw == 1, wrk[["qfqmOut"]][[lvlAgr]][["soni"]][["qfFinl"]] <- wrk[["qfqmOut"]][[lvlAgr]][["soni"]][["qfShdw"]], wrk[["qfqmOut"]][[lvlAgr]][["soni"]][["qfFinl"]] <- wrk[["qfqmOut"]][[lvlAgr]][["soni"]][["qfFinl"]])
        

      #qfShdw flag for the 1-min data
       qfShdw <-  eddy4R.qaqc::def.qf.shdw(angWindMean = wrk[["dp01AgrSub"]][[lvlAgr]][["data"]][["soni"]][["mean"]][["angZaxsErth"]], ThshAngMin = Para$Sci$dp0p$soni$AngShdwMin, ThshAngMax = Para$Sci$dp0p$soni$AngShdwMax, Site = Para$Site$Loc, Date = dateCalcCntr, AngZaxsSoniInst = Para$Sci$dp0p$soni$AngNedZaxs)
       
       
       #Create empty data.frame with same structure
       wrk[["dp01AgrSub"]][[lvlAgr]][["qfqm"]][["soni"]][["qfShdw"]] <- wrk[["dp01AgrSub"]][[lvlAgr]][["qfqm"]][["soni"]][['qfFinl']]
       #Set all qfShdw to 0 originally
       wrk[["dp01AgrSub"]][[lvlAgr]][["qfqm"]][["soni"]][["qfShdw"]][1:nrow(wrk[["dp01AgrSub"]][[lvlAgr]][["qfqm"]][["soni"]][["qfShdw"]]),] <- qfShdw
      

       #Test if qfShdw flag is raised
       if(any(qfShdw == 1)){
         #Set qfFinl for when qfShdw is raised
         wrk[["dp01AgrSub"]][[lvlAgr]][["qfqm"]][["soni"]][['qfFinl']][which(qfShdw == 1),] <- 1
       }#End test if qfShdw flag is raised
      #Logging for tower shadow flag
       msg <- base::paste("Tower shadow flag set!")
       tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
       
    # re-introduce flux calculation based on flow.turb.tow.neon.r (SHA 512e6831)

      # assemble data for eddy flux calculation
      wrk$tmp$dp04 <- data.frame(
        # UTC time [hh.hh]
        t_utc = wrk$data$time$UTC_frac,
        # fractional day of year at UTC [dd.dd]
        t_doy_utc = wrk$data$time$DOY_frac,
        # fractional day of year at location [dd.dd]
        t_doy_local = wrk$data$time$DOY_frac + SiteInfo$TimeDiffUtcLt / 24,
        # UTM easting [m]
        d_x_utm = rep(SiteInfo$ZoneUtm$Estg, length(wrk$data$time$UTC)),
        # UTM northing [m]
        d_y_utm = rep(SiteInfo$ZoneUtm$Nthg, length(wrk$data$time$UTC)),
        # relative measurement height, i.e. distance above displacement height [m]
        d_z_m = rep(SiteInfo$DistZaxsMeas - SiteInfo$DistZaxsDisp, length(wrk$data$time$UTC)),
        # ground distance travelled [s]
        # used as indipendent variable for detrending etc. when AlgBase != mean
        d_xy_travel = (wrk$data$time$UTC_frac - min(wrk$data$time$UTC_frac, na.rm=TRUE)) * 3600,
        # length of air parcel flown through [s]
        d_xy_flow = (wrk$data$time$UTC_frac - min(wrk$data$time$UTC_frac, na.rm=TRUE)) * 3600,
        # aircraft heading [deg]
        PSI_aircraft = rep(Para$Sci$dp0p$soni$AngNedZaxs, length(wrk$data$time$UTC)),
        # motion of air relative to aircraft [m s-1]; later used for power spectra
        uvw_aircraft = sqrt(wrk$data$soni$veloXaxsPf^2 + wrk$data$soni$veloYaxsPf^2),
        # latitudinal wind speed, positive from west [m s-1]
        u_met = wrk$data$soni$veloXaxsPf,
        # longitudinal wind speed, positive from south [m s-1]
        v_met = wrk$data$soni$veloYaxsPf,
        # magnitude of horizontal wind vector speed [m s-1]
        uv_met = sqrt(wrk$data$soni$veloXaxsPf^2 + wrk$data$soni$veloXaxsPf^2),
        # vertical wind speed, positive from below [m s-1]
        w_met = wrk$data$soni$veloZaxsPf,
        # ambient pressure [Pa = kg m-1 m-2]
        p_air = wrk$data$irgaTurb$presAtm,
        # air temperature [K]
        T_air = wrk$data$soni$T_air_SONIC,
        # H2O dry mole fraction [mol mol-1]
        FD_mole_H2O = wrk$data$irgaTurb$rtioMoleDryH2o,
        # CO2 dry mole fraction [mol mol-1]
        FD_mole_CH4 = wrk$data$irgaTurb$rtioMoleDryCo2,
        # terrain altitude asl [m]
        d_z_terrain = rep(SiteInfo$ElevAslTow, length(wrk$data$time$UTC)),
        # boundary layer height above ground [m]
        d_z_ABL = rep(1e3, length(wrk$data$time$UTC))
      )

      # workaround for bug associated with atomic vector when calling REYNflux_FD_mole_dry() directly
      tmp <- data.frame(sapply(1:length(wrk$tmp$dp04), function(x) as.numeric(wrk$tmp$dp04[,x])))
      names(tmp) <- names(wrk$tmp$dp04)
      wrk$tmp$dp04 <- tmp
      rm(tmp)
      invisible(gc())

      # map data for eddy flux calculation - 2022/23 refactoring
      
        # def.stat.sta.diff
        # independent variable, required only if AlgBase != mean
        # can be assigned flexibly for tower, aircraft etc. with corresponding units
        # fixed platform, e.g. tower
        # wrk$tmp$dp04$idep <- wrk$tmp$dp04$t_utc
        #   wrk$tmp$dp04$t_utc <- NULL
        #   base::attr(x = wrk$tmp$dp04$idep, which = "unit") <- "h"
        #   base::attr(x = wrk$tmp$dp04$d_xy_travel, which = "unit") <- "s"
        # moving platform, e.g. aircraft
        wrk$tmp$dp04$idep <- wrk$tmp$dp04$d_xy_travel
          wrk$tmp$dp04$d_xy_travel <- NULL
          base::attr(x = wrk$tmp$dp04$idep, which = "unit") <- "s"
          base::attr(x = wrk$tmp$dp04$t_utc, which = "unit") <- "h"
        
        # def.flux.vect
        wrk$tmp$dp04$veloXaxs <- wrk$tmp$dp04$u_met
          wrk$tmp$dp04$u_met <- NULL
          base::attr(x = wrk$tmp$dp04$veloXaxs, which = "unit") <- "m s-1"
        wrk$tmp$dp04$veloYaxs <- wrk$tmp$dp04$v_met
          wrk$tmp$dp04$v_met <- NULL
          base::attr(x = wrk$tmp$dp04$veloYaxs, which = "unit") <- "m s-1"
        wrk$tmp$dp04$veloZaxs <- wrk$tmp$dp04$w_met
          wrk$tmp$dp04$w_met <- NULL
          base::attr(x = wrk$tmp$dp04$veloZaxs, which = "unit") <- "m s-1"
        
        # def.flux.sclr
        wrk$tmp$dp04$presAtm <- wrk$tmp$dp04$p_air
          wrk$tmp$dp04$p_air <- NULL
          base::attr(x = wrk$tmp$dp04$presAtm, which = "unit") <- "Pa"
        wrk$tmp$dp04$tempAir <- wrk$tmp$dp04$T_air
          wrk$tmp$dp04$T_air <- NULL
          base::attr(x = wrk$tmp$dp04$tempAir, which = "unit") <- "K"
        wrk$tmp$dp04$rtioMoleDryH2o <- wrk$tmp$dp04$FD_mole_H2O
          wrk$tmp$dp04$FD_mole_H2O <- NULL
          base::attr(x = wrk$tmp$dp04$rtioMoleDryH2o, which = "unit") <- "molH2o mol-1Dry"
        wrk$tmp$dp04$rtioMoleDryCo2 <- wrk$tmp$dp04$FD_mole_CH4
          wrk$tmp$dp04$FD_mole_CH4 <- NULL
          base::attr(x = wrk$tmp$dp04$rtioMoleDryCo2, which = "unit") <- "molCo2 mol-1Dry"
          
          
          # Add the raw CO2 dry mole fraction [mol mol-1] before validation application
          wrk$tmp$dp04$rtioMoleDryCo2Raw <- wrk$data$irgaTurb$rtioMoleDryCo2Raw
          # replace H2o dry mole fraction [mol mol-1] by wrk$data$irgaTurb$rtioMoleDryH2oRaw ==> Add if changed by validation
          wrk$tmp$dp04$rtioMoleDryH2oCor <- wrk$data$irgaTurb$rtioMoleDryH2oCor
          
        
        # def.var.abl
        wrk$tmp$dp04$distZaxsMeas <- wrk$tmp$dp04$d_z_m
          wrk$tmp$dp04$d_z_m <- NULL
          base::attr(x = wrk$tmp$dp04$distZaxsMeas, which = "unit") <- "m"
        wrk$tmp$dp04$distZaxsAbl <- wrk$tmp$dp04$d_z_ABL
          wrk$tmp$dp04$d_z_ABL <- NULL
          base::attr(x = wrk$tmp$dp04$distZaxsAbl, which = "unit") <- "m"
        
        # optional: pass-through
        base::attr(x = wrk$tmp$dp04$t_doy_utc, which = "unit") <- "d"
        base::attr(x = wrk$tmp$dp04$t_doy_local, which = "unit") <- "d"
        base::attr(x = wrk$tmp$dp04$d_x_utm, which = "unit") <- "m"
        base::attr(x = wrk$tmp$dp04$d_y_utm, which = "unit") <- "m"
        base::attr(x = wrk$tmp$dp04$d_xy_flow, which = "unit") <- "s"
        wrk$tmp$dp04$PSI_aircraft <- eddy4R.base::def.conv.poly(data = wrk$tmp$dp04$PSI_aircraft, 
                                                                coefPoly = eddy4R.base::IntlConv$DegRad)
          attributes(wrk$tmp$dp04$PSI_aircraft)$unit <- "rad"
        base::attr(x = wrk$tmp$dp04$uvw_aircraft, which = "unit") <- "m s-1"
        base::attr(x = wrk$tmp$dp04$uv_met, which = "unit") <- "m s-1"
        base::attr(x = wrk$tmp$dp04$d_z_terrain, which = "unit") <- "m"

      
      # time-domain eddy flux calculation
      wrk$reyn <- eddy4R.turb::wrap.flux(
        data = wrk$tmp$dp04,
        AlgBase = "mean",
        SlctPot = FALSE,
        PresPot = eddy4R.base::IntlNatu$Pres00,
        ListGasSclr = list(rtioMoleDryCo2 = list(Conv = "densMoleAirDry", Unit = base::data.frame(InpVect = "m s-1", InpSclr = "molCo2 mol-1Dry", Conv = "mol m-3", Out = "mol m-2 s-1"), NameOut = "fluxCo2"), rtioMoleDryCo2Raw = list(Conv = "densMoleAirDry", Unit = base::data.frame(InpVect = "m s-1", InpSclr = "molCo2 mol-1Dry", Conv = "mol m-3", Out = "mol m-2 s-1"), NameOut = "fluxCo2Raw"), rtioMoleDryH2oCor = list(Conv = "convH2oEngy", Unit = base::data.frame(InpVect = "m s-1", InpSclr = "molH2o mol-1Dry", Conv = "kg m-1 s-1", Out = "W m-2"), NameOut = "fluxH2oEngyCor"))
      )
      

################################################################################################      
      #stationarity
################################################################################################       
      # print message to screen
      msg <-"Begin: Stationarity calculation..."
      tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
      wrk$stna <- eddy4R.turb::def.stna(
        data=wrk$tmp$dp04,
        MethStna=c(1, 2, 3)[3],
        whrVar=c("veloFricXaxsSq", "veloFricYaxsSq", "veloFric", "fluxTempEngy", "fluxH2oEngy", "fluxCo2", "fluxCo2Raw"),
        NumSubSamp=6,
        corTempPot=FALSE,
        presTempPot=eddy4R.base::IntlNatu$Pres00,
        vrbs = FALSE,
        Thsh = 100,
        Perc = FALSE,
        ListGasSclr = list(rtioMoleDryCo2 = list(Conv = "densMoleAirDry", Unit = base::data.frame(InpVect = "m s-1", InpSclr = "molCo2 mol-1Dry", Conv = "mol m-3", Out = "mol m-2 s-1"), NameOut = "fluxCo2"), rtioMoleDryCo2Raw = list(Conv = "densMoleAirDry", Unit = base::data.frame(InpVect = "m s-1", InpSclr = "molCo2 mol-1Dry", Conv = "mol m-3", Out = "mol m-2 s-1"), NameOut = "fluxCo2Raw"))
      )
      #add quality indicator flags
      wrk$stna$qifStnaTrnd <- as.data.frame(ifelse(is.na(wrk$stna$qiStnaTrnd), NaN,
                                                   ifelse(wrk$stna$qiStnaTrnd > 1, 2,
                                                          ifelse(wrk$stna$qiStnaTrnd > 0.3 & wrk$stna$qiStnaTrnd <= 1,  1, 0))))
    
      wrk$stna$qifStnaSubSamp <- as.data.frame(ifelse(is.na(wrk$stna$qiStnaSubSamp), NaN,
                                                      ifelse(wrk$stna$qiStnaSubSamp > 1, 2,
                                                             ifelse(wrk$stna$qiStnaSubSamp > 0.3 & wrk$stna$qiStnaSubSamp <= 1,  1, 0))))
      #overall quality indicator flags for stationarity test
      #if both qifStnaTrnd & qifStnaSubSamp are NaN, qifStna is equal NaN
      wrk$stna$qifStna <- as.data.frame(ifelse(is.na(wrk$stna$qifStnaTrnd) & is.na(wrk$stna$qiStnaSubSamp), NaN,
                                               ifelse(is.na(wrk$stna$qifStnaTrnd) & !is.na(wrk$stna$qiStnaSubSamp), wrk$stna$qiStnaSubSamp,
                                                      ifelse(!is.na(wrk$stna$qifStnaTrnd) & is.na(wrk$stna$qiStnaSubSamp), wrk$stna$qifStnaTrnd,
                                                             ifelse(wrk$stna$qifStnaTrnd >= wrk$stna$qifStnaSubSamp, wrk$stna$qifStnaTrnd, wrk$stna$qifStnaSubSamp)))))
      colnames(wrk$stna$qifStna) <- colnames(wrk$stna$qifStnaTrnd)
      
      #Integral Turbulence Characteristics (ITCs)
      # print message to screen
      msg <- "Begin: Integral Turbulence Characteristics calculation..."
      tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
      #data frame for deviations
      tmpSd <- data.frame(
        u_hor = wrk$reyn$sd$veloXaxsHor,
        w_hor = wrk$reyn$sd$veloZaxsHor,
        T_air = wrk$reyn$sd$tempAir
      )
      
      #data frame for scales
      tmpScal <- data.frame(
        u_star = wrk$reyn$mean$veloFric,
        T_star_SL = wrk$reyn$mean$tempScalAtmSurf
      )
      
      #calculation
      wrk$itc <- def.itc(
        stblObkv = wrk$reyn$mean$paraStbl,  #stability
        lat = Para$Site$LatTow,
        VarInp = c("veloXaxs","veloZaxs","temp","all")[4],
        sd = tmpSd,
        varScal = tmpScal,
        Thsh = 100,
        Perc = FALSE,
        CorTemp = FALSE
      );rm (tmpSd, tmpScal)
      
      #add quality indicator flags
      wrk$itc$qifItc <- as.data.frame(ifelse(is.na(wrk$itc$qiItc), NaN,
                                             ifelse(wrk$itc$qiItc > 1, 2,
                                                   ifelse(wrk$itc$qiItc > 0.3 & wrk$itc$qiItc <= 1,  1, 0))))
      
      

    # footprint modeling (60 ms; 5 MB RAM)
    # TODO: should come after spectral correction of all turbulence statistics

      # print message to screen
      msg <- "Begin: Footprint calculation..."


      # target number of cells in both directions of a square footprint weight matrix
      wrk$foot$numCell <- 301

      # assemble turbulence statistics
      # TODO: cross-wind distribution sensitive against boundary layer height; either NK's 2015 model, oder ABL parameterization / download
      wrk$foot$statTurb <- data.frame(
        # wind direction [degree clockwise]
        angZaxsErth = eddy4R.base::def.conv.poly(data = wrk$reyn$mean$angZaxsErth,
                                                 coefPoly = eddy4R.base::IntlConv$RadDeg),
        # cell size [m]
        distReso = wrk$reyn$mean$distZaxsMeas,
        # standard deviation of the cross-wind [m s-1]
        veloYaxsHorSd = wrk$reyn$sd$veloYaxsHor,
        # standard deviation of the vertical wind [m s-1]
        veloZaxsHorSd = wrk$reyn$sd$veloZaxsHor,
        # friction velocity [m s-1]
        veloFric = wrk$reyn$mean$veloFric,
        # relative measurement height above displacement [m]
        distZaxsMeasDisp = wrk$reyn$mean$distZaxsMeas,
        # roughness length [m]
        distZaxsRgh = eddy4R.turb::def.dist.rgh(
        distZaxsMeas = wrk$reyn$mean$distZaxsMeas,
        distObkv = wrk$reyn$mean$distObkv,
        veloXaxs = wrk$reyn$mean$veloXaxsHor,
        veloFric = wrk$reyn$mean$veloFric,
        RngStblObkv=c(-2, 1)),
        # boundary layer height [m]
        distZaxsAbl = ifelse(!is.na(wrk$reyn$mean$distZaxsAbl), wrk$reyn$mean$distZaxsAbl, 1e3)
      )
      base::attr(x = wrk$foot$statTurb$angZaxsErth, which = "unit") <- "deg"

      # round cell size if > 10 m
      if(wrk$foot$statTurb$distReso > 10) wrk$foot$statTurb$distReso <- base::round(x = wrk$foot$statTurb$distReso, digits = -1)

      # constrain turbulence statistics in accordance with range of Kljun et al. (2004) parameterization

        # assemble thresholds c(min, max)
        wrk$foot$thshTurb <- data.frame(
          # wind direction [degree clockwise]
          angZaxsErth = c(0, 360),
          # cell size [m]
          distReso = c(1, 500),
          # standard deviation of the cross-wind [m s-1]
          veloYaxsHorSd = c(0, 10),
          # standard deviation of the vertical wind [m s-1]
          veloZaxsHorSd = c(0.23, 1.23),
          # friction velocity [m s-1]
          veloFric = c(0.2, 10),
          # relative measurement height above displacement [m]
          distZaxsMeasDisp = c(1, 500),
          # roughness length [m]
          # tighten constraint for large values because CPER otherwise fails with 5 m roughness length and 6.8 m measurement height
          # used relationship to relative measurment height so increasing impact of surrounding terrain features with measurement height is considered
          # alternatively, could also be parameterized from displacement height or canopy height
          distZaxsRgh = c(1e-5, wrk$foot$statTurb$distZaxsMeasDisp / 10),
          # boundary layer height [m]
          distZaxsAbl = c(wrk$foot$statTurb$distZaxsMeasDisp + 1, 1e4)
        )

        # apply thresholds

          # minima
          idxThshMin <- which(wrk$foot$statTurb < wrk$foot$thshTurb[1,])
          wrk$foot$statTurb[,idxThshMin] <- wrk$foot$thshTurb[1,idxThshMin]

          # maxima
          idxThshMax <- which(wrk$foot$statTurb > wrk$foot$thshTurb[2,])
          wrk$foot$statTurb[,idxThshMax] <- wrk$foot$thshTurb[2,idxThshMax]

          # determine footprint quality flag

            # set default
            wrk$foot$qf <- 0

            # set low in case at least one turbulence statistics entry has been constrained
            if(base::length(idxThshMin) != 0 | base::length(idxThshMax) != 0) wrk$foot$qf <- -1

            # set high in case at least one turbulence statistics entry is missing; over-rules -1
            if(!base::all(!base::is.na(wrk$foot$statTurb))) wrk$foot$qf <- 1

          # clean up
          rm(idxThshMax, idxThshMin)

      # calculate footprint weight matrix only if all turbulence statistics are available
      if(wrk$foot$qf != 1) {

        # footprint calculation
        wrk$foot$data <-
          eddy4R.turb::def.foot.k04(
          angZaxsErth = wrk$foot$statTurb$angZaxsErth,
          distReso = wrk$foot$statTurb$distReso,
          veloYaxsHorSd = wrk$foot$statTurb$veloYaxsHorSd,
          veloZaxsHorSd = wrk$foot$statTurb$veloZaxsHorSd,
          veloFric = wrk$foot$statTurb$veloFric,
          distZaxsMeasDisp = wrk$foot$statTurb$distZaxsMeasDisp,
          distZaxsRgh = wrk$foot$statTurb$distZaxsRgh,
          distZaxsAbl = wrk$foot$statTurb$distZaxsAbl,
          thsh = 0.9
        )
        
        # Find if dimensions (rows and columns) of square footprint matrix are 
        # odd or even using modulus operator (%%).
        # NEEDS TO BE ODD!!!
        # Quick fix: If it is even, remove rightmost column and bottom row.
        #
        # TO DO: modify/investigate footprint code (def.foot.k04.R) to figure out
        # why even dimensions are being produced.
        if (nrow(wrk$foot$data$wghtFootXaxsYaxsItgr) %% 2 == 0) {
          wrk$foot$data$wghtFootXaxsYaxsItgr <- wrk$foot$data$wghtFootXaxsYaxsItgr[-nrow(wrk$foot$data$wghtFootXaxsYaxsItgr), -ncol(wrk$foot$data$wghtFootXaxsYaxsItgr)]
        }


        # padding / cutting

          # # rotation by EBImage::rotate() could result in even-numbered dimensions
          # # fixed in footK04
          # if(base::nrow(wrk$foot$data$PHI) %% 2 == 0) {
          #
          #   wrk$foot$data$PHI <-
          #     base::cbind(
          #       rep(0, (base::nrow(wrk$foot$data$PHI) + 1)),
          #       base::rbind(
          #         wrk$foot$data$PHI,
          #         rep(0, base::ncol(wrk$foot$data$PHI))
          #         )
          #     )
          #
          #   }

          # pad in case the source matrix is smaller than the target matrix
          if(base::nrow(wrk$foot$data$wghtFootXaxsYaxsItgr) < wrk$foot$numCell) {

            # number of columns / rows to pad on each side of the source matrix
            numPad <- (wrk$foot$numCell - base::nrow(wrk$foot$data$wghtFootXaxsYaxsItgr)) / 2

            # actual padding
            wrk$foot$data$wghtFootXaxsYaxsItgr <- cbind(
              base::matrix(data = 0, nrow = wrk$foot$numCell, ncol = numPad),
              base::rbind(
                base::matrix(data = 0, nrow = numPad, ncol = base::ncol(wrk$foot$data$wghtFootXaxsYaxsItgr)),
                wrk$foot$data$wghtFootXaxsYaxsItgr,
                base::matrix(data = 0, nrow = numPad, ncol = base::ncol(wrk$foot$data$wghtFootXaxsYaxsItgr))
              ),
              base::matrix(data = 0, nrow = wrk$foot$numCell, ncol = numPad)
            )

            # clean up
            rm(numPad)

          }

          # cut in case the source matrix is larger than the target matrix
          if(base::nrow(wrk$foot$data$wghtFootXaxsYaxsItgr) > wrk$foot$numCell) {

            # number of columns / rows to cut from each side of the source matrix
            numCut <- (base::nrow(wrk$foot$data$wghtFootXaxsYaxsItgr) - wrk$foot$numCell) / 2

            # indeces of cells to retain
            idxCut <- (numCut + 1):(base::nrow(wrk$foot$data$wghtFootXaxsYaxsItgr) - numCut)

            # actual cutting
            wrk$foot$data$wghtFootXaxsYaxsItgr <- wrk$foot$data$wghtFootXaxsYaxsItgr[idxCut,idxCut]

            # clean up
            rm(numCut, idxCut)

          }

          # normalize to sum of unity
          wrk$foot$data$wghtFootXaxsYaxsItgr <- wrk$foot$data$wghtFootXaxsYaxsItgr / base::sum(wrk$foot$data$wghtFootXaxsYaxsItgr)

      # assign footprint weight matrix with NaN if not all turbulence statistics are available
      } else {

        wrk$foot$data$wghtFootXaxsYaxsItgr <- matrix(data=NaN, nrow = wrk$foot$numCell, ncol = wrk$foot$numCell)
        wrk$foot$data$qiFootXaxsFrac <- NaN
        wrk$foot$data$distFootXaxsThshFootCum <- NaN
        wrk$foot$data$distFootXaxsMax <- NaN
        wrk$foot$data$distFootYaxsThshFootCum <- NaN

      }

      # print message to screen
      msg <- "Complete: Footprint calculation"
      tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})

      
    # Wavelet-based high-frequency response correction for fluxes; consumes about 8 s per 30 min of data, or ~7 min per day of data
    # TODO: consider for refactoring eddy4R.turb::REYNflux_FD_mole_dry()
    # most consistent to (i) perform high-frequency correction,  (ii) Wavelet-reconstruction of scalar time-series,
    # (iii) subsequent calculation of all dp01, dp04 and ucrt based on reconstructed time-series
      # dp01: automatically includes high-frequency spectral correction for standard deviations and standard errors
        # not currently done to avoid inconsistencies: high-frequency correction for principal measurement quantities cover only a fraction of the dp01 sub-products
        # however, noise correction in particular for CO2 can be on the order of 20 - 50% variance
      # dp04: automatically propagates high-frequency response correction into all derived quantities
      # (I, d_L_v_0, sigma, w_star, t_star, T_star_SL, T_star_ML, FD_mole_H2O_star_SL, FD_mole_H2O_star_ML)

      # print message to screen
      msg <- "Begin: Wavelet-based high-frequency response correction..."
      tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})

      #aggregate data
      dfWave <- data.frame(
        veloZaxsHor = wrk$reyn$diff$veloZaxsHor,
        veloXaxsHor = wrk$reyn$diff$veloXaxsHor,
        veloYaxsHor = wrk$reyn$diff$veloYaxsHor,
       # uv_met = wrk$reyn$imfl$uv_met,
        tempAir = wrk$reyn$diff$tempAir,
       # T_v_0 = wrk$reyn$imfl$T_v_0,
       rtioMoleDryH2o = wrk$reyn$diff$rtioMoleDryH2o,
       rtioMoleDryCo2 = wrk$reyn$diff$rtioMoleDryCo2,
       rtioMoleDryCo2Raw = wrk$reyn$diff$rtioMoleDryCo2Raw)


      #Call the wavelet wrapper function
      #waveOut <- eddy4R.turb::wrap.wave(dfInp = dfWave, FreqSamp = FreqSamp$soni, paraStbl = wrk$reyn$mean$paraStbl)

      waveOut <- eddy4R.turb::wrap.wave(dfInp = dfWave, FuncWave = "haar", FreqSamp = FreqSamp$soni, zeroPad = FALSE, ThshMiss = 0.1, init = c(3, 5)) #paraStbl = wrk$reyn$mean$paraStbl)
      #Repete processing for rtioMoleDryCo2Raw: Call the wavelet wrapper function
      #waveOut01 <- eddy4R.turb::wrap.wave(dfInp = dfWave01, FuncWave = "haar", FreqSamp = FreqSamp$soni, zeroPad = FALSE, ThshMiss = 0.1, init = c(3, 5)) # paraStbl = wrk$reyn01$mean$paraStbl)
      
      #Output wavelet spec and cosp
      waveOutSpec[[lvlAgr]] <- waveOut[names(waveOut) != "wave"]
      

      # print message to screen
      msg <- "Complete: Wavelet-based high-frequency response correction"
      tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})


    # assign results
    # output for core data sub-products; many additional variables are available for assignment from wrk$reyn
    # incl. application of correction factors from Wavelet-based high-frequency correction to Reynolds-decomposed fluxes
    # if Wavelet flag is set high (1), correction factor is automatically set to unity (1); so no need for conditional statement

      # heat flux (NEON.DP4.00002; http://data.neonscience.org/data-product-view?dpCode=DP4.00002.001)
      wrk$dp04[[lvlAgr]]$fluxTemp$turb <- data.frame(flux = waveOut$cov$tempAir$coefCor * wrk$reyn$mean$fluxTempEngy)
      base::attr(x = wrk$dp04[[lvlAgr]]$fluxTemp$turb$flux, which = "unit") <- "W m-2"
      #gethering qf from both dp01 and dp04
      wrk$dp04[[lvlAgr]]$qf$fluxTemp$turb <- data.frame(itcVeloZaxsErth = wrk$itc$qiItc$w_hor,
                                                        qiItcVeloZaxsErth = wrk$itc$qifItc$w_hor,
                                                        itcTempAir= wrk$itc$qiItc$T_air,
                                                        qiTempAir = wrk$itc$qifItc$T_air,
                                                        qfItc = as.integer(wrk$itc$qfItc$F_H_kin),
                                                        qiItc = wrk$itc$qifItc$F_H_kin,
                                                        stnaSubSamp = wrk$stna$qiStnaSubSamp$fluxTempEngy,
                                                        qiStnaSubSamp = wrk$stna$qifStnaSubSamp$fluxTempEngy,
                                                        stnaTrnd = wrk$stna$qiStnaTrnd$fluxTempEngy,
                                                        qiStnaTrnd = wrk$stna$qifStnaTrnd$fluxTempEngy,
                                                        qfStna = as.integer(wrk$stna$qfStna$fluxTempEngy),
                                                        qiStna = wrk$stna$qifStna$fluxTempEngy,
                                                        qfFinlTempAir = wrk$qfqmOut[[lvlAgr]]$soni$qfFinl$tempAir,
                                                        qfFinlVeloZaxsErth = wrk$qfqmOut[[lvlAgr]]$soni$qfFinl$veloZaxsErth)
      base::attr(x = wrk$dp04[[lvlAgr]]$qf$fluxTemp$turb, which = "unit") <- c("-", "NA", "-", "NA", "NA", "NA", "-", "NA", "-", "NA", "NA", "NA", "NA", "NA")
    
      # momentum flux (NEON.DP4.00007; http://data.neonscience.org/data-product-view?dpCode=DP4.00007.001)
      wrk$dp04[[lvlAgr]]$fluxMome$turb <- data.frame(veloFric = (
        (waveOut$cov$veloXaxsHor$coefCor * wrk$reyn$mean$veloFricXaxsSq)^2 + 
          (waveOut$cov$veloYaxsHor$coefCor * wrk$reyn$mean$veloFricYaxsSq)^2)^(1/4))
      base::attr(x = wrk$dp04[[lvlAgr]]$fluxMome$turb$veloFric, which = "unit") <- "m s-1"
      wrk$dp04[[lvlAgr]]$qf$fluxMome$turb <- data.frame(itcVeloXaxsErth = wrk$itc$qiItc$u_hor,
                                                        qiItcVeloXaxsErth = wrk$itc$qifItc$u_hor,
                                                        itcVeloZaxsErth = wrk$itc$qiItc$w_hor,
                                                        qiItcVeloZaxsErth  = wrk$itc$qifItc$w_hor,
                                                        qfItc = as.integer(wrk$itc$qfItc$u_star),
                                                        qiItc = wrk$itc$qifItc$u_star, 
                                                        stnaSubSamp = wrk$stna$qiStnaSubSamp$veloFric,
                                                        qiStnaSubSamp = wrk$stna$qifStnaSubSamp$veloFric,
                                                        stnaTrnd = wrk$stna$qiStnaTrnd$veloFric,
                                                        qiStnaTrnd = wrk$stna$qifStnaTrnd$veloFric,
                                                        qfStna = as.integer(wrk$stna$qfStna$veloFric),
                                                        qiStna = wrk$stna$qifStna$veloFric,
                                                        qfFinlVeloXaxsErth = wrk$qfqmOut[[lvlAgr]]$soni$qfFinl$veloXaxsErth,
                                                        qfFinlVeloZaxsErth = wrk$qfqmOut[[lvlAgr]]$soni$qfFinl$veloZaxsErth)
      base::attr(x = wrk$dp04[[lvlAgr]]$qf$fluxMome$turb, which = "unit") <- c("-", "NA", "-", "NA", "NA", "NA", "-", "NA", "-", "NA", "NA", "NA", "NA", "NA")
      
      
      # H2o flux (NEON.DP4.00137; http://data.neonscience.org/data-product-view?dpCode=DP4.00137.001)
      wrk$dp04[[lvlAgr]]$fluxH2o$turb <- data.frame(flux = waveOut$cov$rtioMoleDryH2o$coefCor * wrk$reyn$mean$fluxH2oEngy)
      base::attr(x = wrk$dp04[[lvlAgr]]$fluxH2o$turb$flux, which = "unit") <- "W m-2"
      #adding fluxCor which is NaN and will change later after implementing h2o validation
      wrk$dp04[[lvlAgr]]$fluxH2o$turb$fluxCor <- NaN
      base::attr(x = wrk$dp04[[lvlAgr]]$fluxH2o$turb$fluxCor, which = "unit") <- "W m-2"
      #adding fluxRaw which is calculated from rtioMoleDryH2oRaw
      wrk$dp04[[lvlAgr]]$fluxH2o$turb$fluxRaw <- waveOut$cov$rtioMoleDryH2o$coefCor * wrk$reyn$mean$fluxH2oEngy
      base::attr(x = wrk$dp04[[lvlAgr]]$fluxH2o$turb$fluxRaw, which = "unit") <- "W m-2"
      wrk$dp04[[lvlAgr]]$qf$fluxH2o$turb <- data.frame(itcVeloZaxsErth = wrk$itc$qiItc$w_hor,
                                                       qiItcVeloZaxsErth  = wrk$itc$qifItc$w_hor,
                                                       qfItc = as.integer(wrk$itc$qfItc$w_hor),
                                                       qiItc = wrk$itc$qifItc$w_hor,
                                                       stnaSubSamp = wrk$stna$qiStnaSubSamp$fluxH2oEngy,
                                                       qiStnaSubSamp = wrk$stna$qifStnaSubSamp$fluxH2oEngy,
                                                       stnaTrnd = wrk$stna$qiStnaTrnd$fluxH2oEngy,
                                                       qiStnaTrnd = wrk$stna$qifStnaTrnd$fluxH2oEngy,
                                                       qfStna = as.integer(wrk$stna$qfStna$fluxH2oEngy),
                                                       qiStna = wrk$stna$qifStna$fluxH2oEngy,
                                                       qfFinlRtioMoleDryH2o = wrk$qfqmOut[[lvlAgr]]$h2oTurb$qfFinl$rtioMoleDryH2o,
                                                       qfFinlVeloZaxsErth = wrk$qfqmOut[[lvlAgr]]$soni$qfFinl$veloZaxsErth)
      base::attr(x = wrk$dp04[[lvlAgr]]$qf$fluxH2o$turb, which = "unit") <- c("-", "NA", "NA", "NA", "-", "NA", "-", "NA", "NA", "NA", "NA", "NA")
      
      # Co2 flux (NEON.DP4.00067; http://data.neonscience.org/data-product-view?dpCode=DP4.00067.001)
      wrk$dp04[[lvlAgr]]$fluxCo2$turb <- data.frame(flux = waveOut$cov$rtioMoleDryCo2$coefCor * wrk$reyn$mean$fluxCo2)
      base::attr(x = wrk$dp04[[lvlAgr]]$fluxCo2$turb$flux, which = "unit") <- "mol m-2 s-1"
      #adding fluxCor which is calculated from rtioMoleDryCo2Cor (equal to flux)
      wrk$dp04[[lvlAgr]]$fluxCo2$turb$fluxCor <- wrk$dp04[[lvlAgr]]$fluxCo2$turb$flux
      #adding fluxRaw which is calculated from rtioMoleDryCo2Raw
      wrk$dp04[[lvlAgr]]$fluxCo2$turb$fluxRaw <- waveOut$cov$rtioMoleDryCo2Raw$coefCor * wrk$reyn$mean$fluxCo2Raw
      base::attr(x = wrk$dp04[[lvlAgr]]$fluxCo2$turb$fluxRaw, which = "unit") <- "mol m-2 s-1"
      wrk$dp04[[lvlAgr]]$qf$fluxCo2$turb <- data.frame(itcVeloZaxsErth = wrk$itc$qiItc$w_hor,
                                                       qiItcVeloZaxsErth  = wrk$itc$qifItc$w_hor,
                                                       qfItc = as.integer(wrk$itc$qfItc$w_hor),
                                                       qiItc = wrk$itc$qifItc$w_hor,
                                                       stnaSubSamp = wrk$stna$qiStnaSubSamp$fluxCo2,
                                                       qiStnaSubSamp = wrk$stna$qifStnaSubSamp$fluxCo2,
                                                       stnaTrnd = wrk$stna$qiStnaTrnd$fluxCo2,
                                                       qiStnaTrnd = wrk$stna$qifStnaTrnd$fluxCo2,
                                                       qfStna = as.integer(wrk$stna$qfStna$fluxCo2),
                                                       qiStna = wrk$stna$qifStna$fluxCo2,
                                                       qfFinlRtioMoleDryCo2 = wrk$qfqmOut[[lvlAgr]]$co2Turb$qfFinl$rtioMoleDryCo2,
                                                       qfFinlVeloZaxsErth = wrk$qfqmOut[[lvlAgr]]$soni$qfFinl$veloZaxsErth)
      base::attr(x = wrk$dp04[[lvlAgr]]$qf$fluxCo2$turb, which = "unit") <- c("-", "NA", "NA", "NA", "-", "NA", "-", "NA", "NA", "NA", "NA", "NA")
      
      # footprint characteristics (NEON.DP4.00201; http://data.neonscience.org/data-product-view?dpCode=DP4.00201.001)

        # turbulence and footprint statistics
        wrk$dp04[[lvlAgr]]$foot$stat <- data.frame(
          angZaxsErth = wrk$foot$statTurb$angZaxsErth,
          distReso = wrk$foot$statTurb$distReso,
          veloYaxsHorSd = wrk$foot$statTurb$veloYaxsHorSd,
          veloZaxsHorSd = wrk$foot$statTurb$veloZaxsHorSd,
          veloFric = wrk$foot$statTurb$veloFric,
          distZaxsMeasDisp = wrk$foot$statTurb$distZaxsMeasDisp,
          distZaxsRgh = wrk$foot$statTurb$distZaxsRgh,
          distObkv = wrk$reyn$mean$distObkv, #Obukhov length (L)
          paraStbl = wrk$reyn$mean$paraStbl, #stability parameter (z/L)
          distZaxsAbl = wrk$foot$statTurb$distZaxsAbl,
          distXaxs90 = wrk$foot$data$distFootXaxsThshFootCum,   # along-wind distance of the 90 percent crosswind-integrated cumulative footprint
          distXaxsMax = wrk$foot$data$distFootXaxsMax,   # one-sided cross-wind distance of the 90 percent along-wind integrated cumulative footprint
          distYaxs90 =  wrk$foot$data$distFootYaxsThshFootCum)   # along-wind distance of contribution peak
        base::attr(x = wrk$dp04[[lvlAgr]]$foot$stat, which = "unit") <- c("deg", "m", "m s-1", "m s-1", "m s-1", "m", "m", "m","-","m", "m", "m", "m")

        # footprint weight matrix

          # create valid, ISO8601-conform object name for footprint matrices
          nameGrid <- parsedate::format_iso_8601(wrk$idx$time$timeBgn[idxAgr])
          nameGrid <- gsub("-", "", nameGrid)
          nameGrid <- gsub(":", "", nameGrid)
          nameGrid <- gsub("\\+.*", "", nameGrid)
          nameGrid <- paste0(nameGrid, "Z")

          # assign footprint weight matrix
          wrk$dp04[[lvlAgr]]$foot$grid$turb[[nameGrid]] <- wrk$foot$data$wghtFootXaxsYaxsItgr

    # clean up
    rm(dfWave, nameGrid, waveOut)
    wrk$data <- NULL
    wrk$foot <- NULL
    wrk$reyn <- NULL
    wrk$tmp <- NULL
    invisible(gc())

  ###
  # end loop around aggregation interval
  }
  msg <- paste0("dataset ", dateCalcCntr, " DP01 calculation complete")
  tryCatch({rlog$info(msg)}, error=function(cond){print(msg)})
  ###



  # concatenate results

    
    # dp01
    out <- eddy4R.base::def.dp01.agr.ecte(inpList = wrk, MethSubAgr = TRUE, MethUcrt = TRUE, RptExpd = TRUE)
    #adding irgaTurb validation data to out
    out$vali$data$co2Turb$rtioMoleDryCo2Vali <- tmpVali[[dateCalcCntr]]$rtioMoleDryCo2Vali
    out$vali$data$h2oTurb$rtioMoleDryH2oVali <- tmpVali[[dateCalcCntr]]$rtioMoleDryH2oVali

    # dp04

      # assign list
      out$dp04 <- list()

      # loop around all data products except footprint results
      for(idxDp04 in base::names(wrk$dp04$numAgr01)[!base::names(wrk$dp04$numAgr01) == "foot"]) {
      # idxDp04 <- names(wrk$dp04$numAgr01)[1]

        # transfer of data
        out$dp04$data[[idxDp04]]$turb <- base::do.call(rbind, base::lapply(base::names(wrk$dp04),
                                                                       function(x) wrk$dp04[[x]][[idxDp04]]$turb))
        # transfer of qf
        out$dp04$qfqm[[idxDp04]]$turb <- base::do.call(rbind, base::lapply(base::names(wrk$dp04),
                                                                           function(x) wrk$dp04[[x]]$qf[[idxDp04]]$turb))
        
      }; base::rm(idxDp04)
      # now footprint results

        # turbulence and footprint stats
        out$dp04$data$foot$stat <- base::do.call(rbind, base::lapply(base::names(wrk$dp04),
                                                                           function(x) wrk$dp04[[x]]$foot$stat))

        # footprint weight matrices
        invisible(lapply(base::names(wrk$dp04), function(x)
          out$dp04$data$foot$grid$turb[[base::names(wrk$dp04[[x]]$foot$grid$turb)]] <<-
            t(wrk$dp04[[x]]$foot$grid$turb[[base::names(wrk$dp04[[x]]$foot$grid$turb)]])
          ))

        #DP04 low resolution flagging
        out$dp04$qfqm$fluxCo2$turb$qfFinl  <- ifelse(out$qfqm$soni$qfFinl$veloZaxsErth|out$qfqm$co2Turb$qfFinl$rtioMoleDryCo2|out$dp04$qfqm$fluxCo2$turb$qfItc|out$dp04$qfqm$fluxCo2$turb$qfStna == 1, 1L, 0L)
        out$dp04$qfqm$fluxCo2$turb <- as.data.frame(out$dp04$qfqm$fluxCo2$turb)
        out$dp04$qfqm$fluxH2o$turb$qfFinl  <- ifelse(out$qfqm$soni$qfFinl$veloZaxsErth|out$qfqm$h2oTurb$qfFinl$rtioMoleDryH2o|out$dp04$qfqm$fluxH2o$turb$qfItc|out$dp04$qfqm$fluxH2o$turb$qfStna == 1, 1L, 0L)
        out$dp04$qfqm$fluxH2o$turb <- as.data.frame(out$dp04$qfqm$fluxH2o$turb)
        out$dp04$qfqm$fluxTemp$turb$qfFinl  <- ifelse(out$qfqm$soni$qfFinl$veloZaxsErth|out$qfqm$soni$qfFinl$tempAir|out$dp04$qfqm$fluxTemp$turb$qfItc|out$dp04$qfqm$fluxTemp$turb$qfStna == 1, 1L, 0L)
        out$dp04$qfqm$fluxTemp$turb <- as.data.frame(out$dp04$qfqm$fluxTemp$turb)
        out$dp04$qfqm$fluxMome$turb$qfFinl  <- ifelse(out$qfqm$soni$qfFinl$veloZaxsErth|out$qfqm$soni$qfFinl$veloXaxsErth|out$dp04$qfqm$fluxMome$turb$qfItc|out$dp04$qfqm$fluxMome$turb$qfStna == 1, 1L, 0L)
        out$dp04$qfqm$fluxMome$turb <- as.data.frame(out$dp04$qfqm$fluxMome$turb)
        out$dp04$qfqm$foot$turb$qfFinl  <- ifelse(out$qfqm$soni$qfFinl$veloZaxsErth|out$qfqm$soni$qfFinl$veloXaxsErth == 1, 1L, 0L)
        out$dp04$qfqm$foot$turb <- as.data.frame(out$dp04$qfqm$foot$turb)
        
        #Units for dp04 flags
        lapply(names(out$dp04$qfqm), function(x){
          #Attribute units for qf
          lapply(names(out$dp04$qfqm[[x]]$turb), function(y){
            nameQf <- grep(pattern = "qf", x = y, ignore.case = TRUE, value =  TRUE)
            if (length(nameQf) == 0) {
              attributes(out$dp04$qfqm[[x]]$turb[[y]])$unit <<- "-"} else{
                attributes(out$dp04$qfqm[[x]]$turb[[y]])$unit <<- "NA"
              }
          })})

        
        
        
        #Lag metadata
        out$Lag$Lag <- data.table::rbindlist(wrk$Lag$Lag)
        out$Lag$CorrCros <- data.table::rbindlist(wrk$Lag$CorrCros)

    # clean up
    wrk$dp01 <- NULL
    wrk$dp01AgrSub <- NULL
    wrk$qfqmOut <- NULL
    wrk$dp04 <- NULL

  # output hdf5 files
  if("hdf5" %in% Para$Flow$OutMeth){

    # call the NEON HDF5 structure generating for expanded file
    eddy4R.base::def.hdf5.crte(Date = dateCalcCntr, Site = Para$Site$Loc, LvlTowr = Para$Site$LvlTowr, FileOutBase = Para$Flow$FileOutBase,
                               DirOut = Para$Flow$DirOut, MethExpd = TRUE, MethDp04 = TRUE)

    # call the NEON HDF5 structure generating for basic file
    eddy4R.base::def.hdf5.crte(Date = dateCalcCntr, Site = Para$Site$Loc, LvlTowr = Para$Site$LvlTowr, FileOutBase = Para$Flow$FileOutBase,
                               DirOut = Para$Flow$DirOut, MethExpd = FALSE, MethDp04 = TRUE)

    # TODO: needs to be further hardened to only work on files that are being created as part of this workflow
    FileOut <- base::list.files(path = Para$Flow$DirOut, pattern = base::paste0(Para$Flow$FileOutBase,".*", dateCalcCntr, ".*.h5?"), full.names = TRUE)

    # call the wrapper functio to package and write data to outpu HDF5 files, both basic and expanded
    base::lapply(base::seq_along(FileOut), function(x) eddy4R.base::wrap.hdf5.wrte.dp01(inpList = out, FileInp = DirFilePara, FileOut = FileOut[x],
                                                                                        SiteLoca = Para$Site$Loc, LvlTowr = Para$Site$LvlTowr, MethDp04 = TRUE, Meta = Para))
    
    
    #Output wavelet diagnostic
    saveRDS(waveOutSpec, file = paste0(Para$Flow$DirOut,"/waveOutSpec_",Para$Site$Loc,"_",dateCalcCntr,".rds"))
    
  }

  # start: save diagnostic results to files and create overview plots
  if("diag" %in% Para$Flow$OutMeth){

    # create directory
    dir.create(paste(Para$Flow$DirOut, "/", Para$Site$Loc, "/", Para$Flow$VersDp, "/time", sep=""), showWarnings = FALSE, recursive = TRUE)

    #Download file description readme and object list for testing against gold files
    eddy4R.base::def.dld.zip(Inp = list(Url = "https://storage.googleapis.com/neon-ec-goldfiles/EC-turbulence-processing/fileDesc_20231106.zip",
                                         Dir = paste0(Para$Flow$DirOut, "/", Para$Site$Loc, "/", Para$Flow$VersDp)))
    # write timestamp to file
    utils::write.csv(out$time$co2Turb, file = base::paste(Para$Flow$DirOut, "/", Para$Site$Loc, "/", Para$Flow$VersDp, "/time/", dateCalcCntr,
                                                         "_", dateCalcCntr, "_time.csv", sep=""),
                    na = "NA", row.names = FALSE, quote=TRUE)


    ###
    # start: for loop around dp01 output data products
    for(idxDp01 in names(out$data)[which(names(out$data) != "time")]) {
    # idxDp01 <- names(out$data)[2]
    ###


      # create directory
      dir.create(paste(Para$Flow$DirOut, "/", Para$Site$Loc, "/", Para$Flow$VersDp, "/", idxDp01, sep=""), showWarnings = FALSE)

      ###
      # begin: loop around data sub-products
      for(idxDp01Sub in names(out$data[[idxDp01]])) {
      # idxDp01Sub <- names(out$data[[idxDp01]])[1]
      ###



        #convert to scientific notation with 10 sigificant digits (allows resolving 0.05s in 365 days)

          # assign individual data.frame
          dataInp <- out$data[[idxDp01]][[idxDp01Sub]]

          # covert to scientific notation
          dataRpt <- data.frame(sapply(1:ncol(dataInp), function(x) formatC(dataInp[,x], digits=10, format="e")))

          # assign variable names
          names(dataRpt) <- names(dataInp)



        #write to file
        utils::write.csv(dataRpt, file = paste(Para$Flow$DirOut, "/", Para$Site$Loc, "/", Para$Flow$VersDp, "/", idxDp01, "/", dateCalcCntr,
                                        "_", dateCalcCntr, "_", idxDp01, "_", idxDp01Sub, ".csv", sep=""),
                  na = "NaN", row.names = FALSE, quote=FALSE)

        # clean up
        rm(dataRpt)

        #overview plots for different data frames
        png(filename=paste(Para$Flow$DirOut, "/", Para$Site$Loc, "/", Para$Flow$VersDp, "/", idxDp01, "/", dateCalcCntr,
                           "_", dateCalcCntr, "_", idxDp01, "_", idxDp01Sub, "_%02d.png", sep=""),
            width = 1000, height = 1000, units = "px", pointsize = 20, bg = "white")
        cexvar=3; par(mfrow=c(3,1), las=0, cex.axis=cexvar*0.45, cex.lab=cexvar*0.45, font.lab=2, mar=c(5,5,2,6),
                      mgp=c(3.0,0.8,0), family="times", lwd=cexvar, cex.main=cexvar*0.45, lab=c(5, 2, 7),
                      xaxs="i", las=0)
        opt <- options("scipen" = -3)


        ###
        #start loop around variables
        for(idxDp01SubVar in names(dataInp)) {
        # idxDp01SubVar <- names(dataRpt)[1]
        ###



          #define variables
          idep <- out$time$co2Turb$timeBgn
              # 1:nrow(dataInp)
          depe <- dataInp[[idxDp01SubVar]]
          #ylim <- range(depe, na.rm=TRUE)

          #actual plotting
          if(
            length(which(depe > -Inf)) > 0 &
            length(which(depe < Inf))  > 0 &
            length(which(!is.na(depe))) > 0
          ) {

            plot(depe ~ idep, type="l", col=1, main=idxDp01SubVar,
                 xlab = "UTC time",
                 ylab = paste(idxDp01SubVar, " [", attributes(depe)$unit, "]", sep=""))

          }



        ###
        rm(idep, depe)
        }
        # end: loop around variables
        ###



        # close graphics device
        dev.off()
        rm(cexvar, dataInp, idxDp01SubVar)



      ###
      }; rm(idxDp01Sub)
      # end: loop around data sub-products
      ###



    ###
    }; rm(idxDp01)
    # end: for loop around dp01 output data products
    ###


    ###
    # start: loop around dp04 output data products
    for(idxDp04 in names(out$dp04$data)) {
    # idxDp04 <- names(out$dp04$data)[5]
    ###



      # create directory
      dir.create(paste(Para$Flow$DirOut, "/", Para$Site$Loc, "/", Para$Flow$VersDp, "/", idxDp04, sep=""), showWarnings = FALSE)



      ###
      # begin: loop around data sub-products
      for(idxDp04Sub in names(out$dp04$data[[idxDp04]])[!names(out$dp04$data[[idxDp04]]) == "grid"]) {
      # idxDp04Sub <- names(out$dp04$data[[idxDp04]])[1]
      ###


        # convert to scientific notation with 10 sigificant digits (allows resolving 0.05s in 365 days)

        # assign individual data.frame
        dataInp <- out$dp04$data[[idxDp04]][[idxDp04Sub]]

        # convert to scientific notation
        dataRpt <- data.frame(sapply(1:ncol(dataInp), function(x) formatC(dataInp[,x], digits=10, format="e")))

        # assign variable names
        names(dataRpt) <- names(dataInp)

        #write to file
        utils::write.csv(dataRpt, file = paste0(Para$Flow$DirOut, "/", Para$Site$Loc, "/", Para$Flow$VersDp, "/", idxDp04, "/", dateCalcCntr,
                                              "_", dateCalcCntr, "_", idxDp04, "_", idxDp04Sub, ".csv"),
                        na = "NaN", row.names = FALSE, quote = FALSE)

        # clean up
        rm(dataRpt)

        #overview plots for different data frames
        png(filename=paste(Para$Flow$DirOut, "/", Para$Site$Loc, "/", Para$Flow$VersDp, "/", idxDp04, "/", dateCalcCntr,
                          "_", dateCalcCntr, "_", idxDp04, "_", idxDp04Sub, "_%02d.png", sep=""),
           width = 1000, height = 1000, units = "px", pointsize = 20, bg = "white")
        cexvar=3; par(mfrow=c(3,1), las=0, cex.axis=cexvar*0.45, cex.lab=cexvar*0.45, font.lab=2, mar=c(5,5,2,6),
                     mgp=c(3.0,0.8,0), family="times", lwd=cexvar, cex.main=cexvar*0.45, lab=c(5, 2, 7),
                     xaxs="i", las=0)
        opt <- options("scipen" = -3)


        ###
        # start loop around variables
        for(idxDp04SubVar in names(dataInp)) {
        # idxDp04SubVar <- names(dataInp)[1]
        ###



          # define variables
          idep <- out$time$co2Turb$timeBgn
          # 1:nrow(dataInp)
          depe <- dataInp[[idxDp04SubVar]]
          #ylim <- range(depe, na.rm=TRUE)

          #actual plotting
          if(
            length(which(depe > -Inf)) > 0 &
            length(which(depe < Inf))  > 0 &
            length(which(!is.na(depe))) > 0
          ) {

           plot(depe ~ idep, type = "l", col = 1, main = paste(idxDp04, idxDp04Sub, idxDp04SubVar, sep = " "),
                xlab = "UTC time",
                ylab = paste(idxDp04, " ", idxDp04Sub, " ", idxDp04SubVar, " [", attributes(depe)$unit, "]", sep=""))

          }



        ###
        rm(idep, depe)
        }
        }
        # end: loop around variables
        ###



        # close graphics device
        dev.off()
        rm(cexvar, dataInp, idxDp04SubVar)



      ###
      }; rm(idxDp04Sub)
    # end: loop around dp04 output data products
    ###



  ###
  }; rm(idxDp04)
  # end: save diagnostic results to files and create overview plots
  ###


###
}
 # end: main loop around center days in moving planar fit window
###


# clean up fftempdir

    # # remove files in fftempdir
    # invisible(base::file.remove(base::dir(base::getOption("fftempdir"), full.names = TRUE)))

    # remove .ff files from fftempdir
    # base::unlink(base::getOption("fftempdir"), recursive = TRUE)
    invisible(base::file.remove(base::dir(base::getOption("fftempdir"), pattern = "*.ff", full.names = TRUE)))



# # actual processing via snowfall
#
#   # export all objects to worker processes
#   sfExportAll()
#
#   # process in parallel around days
#   sfOut <- sfClusterApplyLB(DateOut01, funcSf)
