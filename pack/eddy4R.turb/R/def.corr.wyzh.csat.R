

#-------------------------------------------------------------------------------
# def.corr.wyzh.csat
#
# Apply Wyngaard & Zhang (1985) sinusoidal shadowing correction in the
# Horst et al. (2015) formulation for CSAT3 geometry. Converts sonic-frame
# wind to transducer frame, applies element-wise correction, and maps back.
#
#-------------------------------------------------------------------------------

#' @title Definition function: Wyngaard–Zhang sinusoidal shadowing correction (CSAT3 formulation)
#' 
#' @author
#' John Frank  \cr
#' David Durden \email{ddurden@battelleecology.org}
#' 
#'
#' @description
#' Applies the Wyngaard & Zhang (1985) sinusoidal shadowing correction using the
#' Horst et al. (2015) formulation for CSAT3 transducer geometry. Input wind is
#' in sonic coordinates (N × 3). Output includes corrected wind (sonic and
#' transducer frames), per-transducer correction coefficients, and wind–transducer
#' angles.
#'
#' @param inpVeloSoni Matrix or data.frame (N × 3) of wind components in sonic
#'   coordinates \[m s^-1\]; columns correspond to (veloXaxs [u], veloYaxs [v], veloZaxs [w]). If a data.frame is
#'   provided, all three columns must be numeric and will be coerced to a matrix.
#' @param MtrxSoniTran 3 × 3 matrix mapping sonic → transducer coordinates
#'   (columns are unit transducer vectors in sonic coords). Default is CSAT3.
#' @param MtrxTranSoni 3 × 3 matrix mapping transducer → sonic coordinates
#'   (inverse of \code{MtrxSoniTran}). Default matches the inverse of the default above.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{veloWindSoniCorr}: N × 3 data.frame (sonic coords) after correction.
#'   \item \code{veloWindTranCorr}: N × 3 matrix (transducer coords) after correction.
#'   \item \code{coefWyzh}: N × 3 element-wise correction coefficients per transducer.
#'   \item \code{angTranDeg}: N × 3 angles (deg) between wind and transducer axes.
#' }
#'
#' @details
#' For each sample and transducer axis, the angle \eqn{\theta} between the wind
#' vector and the axis is computed, and the Wyngaard sinusoidal coefficient is
#' \deqn{ c(\theta) = 1 / (0.84 + 0.16 \sin \theta) .}
#' The correction is applied element-wise in the transducer frame, and the
#' corrected vectors are mapped back to sonic coordinates.
#'
#' @examples
#' set.seed(1)
#' V <- matrix(rnorm(300), ncol = 3)
#' out <- def.corr.wyzh.csat(V)
#' str(out)
#'
#' # With a data.frame:
#' DF <- as.data.frame(V)
#' out2 <- def.corr.wyzh.csat(DF)
#' all.equal(out, out2)  # should be TRUE
#'
#' @keywords eddy-covariance sonic-anemometer correction csat3 wyngaard zhang
#' @export
#' 
#' 
#' 
def.corr.wyzh.csat <- function(
  inpVeloSoni,
  MtrxSoniTran = matrix(
    c( 1/4,  0.433012701892219,  0.866025403784439,
       -1/2,  0.000000000000000,  0.866025403784439,
       1/4, -0.433012701892219,  0.866025403784439),
    nrow = 3, ncol = 3
  ),
  MtrxTranSoni = matrix(
    c( 2/3, -4/3,  2/3,
       1.154700538379252,  0.000000000000000, -1.154700538379252,
       0.384900179459751,  0.384900179459751,  0.384900179459751),
    nrow = 3, ncol = 3
  )
) {
  # --- coerce/validate input --------------------------------------------------
  if (is.data.frame(inpVeloSoni)) {
    if (ncol(inpVeloSoni) != 3L)
      stop("inpVeloSoni data.frame must have exactly 3 columns.")
    if (!all(vapply(inpVeloSoni, is.numeric, logical(1))))
      stop("All columns of inpVeloSoni data.frame must be numeric.")
    
    
    # Reorder if named as expected
    nameVarExpc <- c("veloXaxs", "veloYaxs", "veloZaxs")
    if (all(nameVarExpc %in% names(inpVeloSoni))) {
      inpVeloSoni <- inpVeloSoni[,nameVarExpc]
    }
    
    inpVeloSoni <- as.matrix(inpVeloSoni)
  }
  stopifnot(
    is.matrix(inpVeloSoni), ncol(inpVeloSoni) == 3,
    is.matrix(MtrxSoniTran), all(dim(MtrxSoniTran) == c(3, 3)),
    is.matrix(MtrxTranSoni), all(dim(MtrxTranSoni) == c(3, 3))
  )
  numRow <- nrow(inpVeloSoni)
  if (numRow == 0) stop("inpVeloSoni has zero rows")
  
  # --- map to transducer frame -----------------------------------------------
  # veloWindTran = V_sonic · (unit transducer vectors in sonic coords)
  veloWindTran <- inpVeloSoni %*% MtrxSoniTran          # N × 3
  
  # wind magnitude for angle calc (avoid div-by-zero)
  magVeloSoni <- sqrt(rowSums(inpVeloSoni^2))       # N
  denm  <- pmax(magVeloSoni, .Machine$double.eps)  # N
  
  # --- wind–transducer angles (degrees) --------------------------------------
  angCosTran <- abs(veloWindTran) / denm                   # vectorized over columns
  angCosTran[angCosTran > 1] <- 1                    # numeric safety
  angTranDeg <- acos(angCosTran) * 180 / pi      # N × 3
  
  # --- Wyngaard–Zhang sinusoidal coefficient ---------------------------------
  coefWyzh <- 1 / (0.84 + 0.16 * sin(angTranDeg * pi / 180))  # N × 3
  
  # --- apply correction in transducer frame ----------------------------------
  veloWindTranCorr <- veloWindTran * coefWyzh          # element-wise correction
  veloWindSoniCorr     <- veloWindTranCorr %*% MtrxTranSoni  # back to sonic frame
  
  # --- convert output data.frame ----------------------------------
  veloWindSoniCorr <- as.data.frame(veloWindSoniCorr)
  names(veloWindSoniCorr) <- c("veloXaxs", "veloYaxs", "veloZaxs")
  rownames(veloWindSoniCorr) <- NULL
  
  
  # --- return ----------------------------------------------------------------
  list(
    veloWindSoniCorr = veloWindSoniCorr,
    veloWindTranCorr = veloWindTranCorr,
    coefWyzh         = coefWyzh,
    angTranDeg       = angTranDeg
  )
}
