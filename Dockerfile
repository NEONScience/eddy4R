# start with the ropensci image including debian:testing, r-base, rocker/rstudio, rocker/hadleyverse
# https://hub.docker.com/r/rocker/ropensci/
FROM rocker/ropensci:latest

# maintainer handle
MAINTAINER "Stefan Metzger" eddy4R.info@gmail.com

# install OS-side dependencies

	# update the list of available packages and their versions
  RUN apt-get update

  # install dependencies
    RUN apt-get install -y \
    # EBImage
      fftw3 \
      fftw3-dev \
      libjpeg-dev \
      libtiff-dev \
    # REddyProc
      libudunits2-0 \
      libudunits2-dev \
      libnetcdf-dev \
      udunits-bin

# eddy4R-Docker example workflow and data

  # create directory
  RUN mkdir -p /home/eddy
  
  # provide read and write access to web-hosted installation script
  RUN chmod -R 777 /home/eddy

# install the eddy4R packages via web-hosted installation script
RUN R -e 'source("https://www.dropbox.com/s/xmgsctjbrekfyw8/flow.inst.eddy4r.R?dl=1")'

  # provide read and write access to Rstudio users for example workflow and data location
  RUN chmod -R 777 /home/eddy

# provide read and write access to Rstudio users for default R library location
RUN chmod -R 777 /usr/local/lib/R/site-library
