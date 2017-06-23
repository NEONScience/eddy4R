The eddy-covariance flux processor eddy4R consists of modular software packages in the [R-language for statistical computing](https://www.r-project.org). The namesake [eddy4R Docker image](https://hub.docker.com/r/stefanmet/eddy4r/) is based on on the [rocker/ropensci image](https://hub.docker.com/r/rocker/ropensci), and adds all eddy4R packages and their dependencies as builds from a Dockerfile. Build additions on the OS-side:
- fftw3
- fftw3-dev

Build additions on the R-side via the [eddy4R installation script](https://www.dropbox.com/s/6kiiehesiuozzl8/flow.inst.R?dl=1):
- eddy4R.base
- eddy4R.erf
- eddy4R.turb
- eddy4R.qaqc
- imports (eddy4R.base, eddy4R.erf, eddy4R.turb, eddy4R.qaqc, Waves)
- suggests (eddy4R.base, eddy4R.erf, eddy4R.turb, eddy4R.qaqc, Waves)
- Waves

For additional information please see our [GitHub wiki](https://github.com/NEONInc/NEON-FIU-algorithm/wiki) (login required).
