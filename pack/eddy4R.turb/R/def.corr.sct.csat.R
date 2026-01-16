
#-------------------------------------------------------------------------------
# def.corr.sct.csat
#
# Apply Self/Cross/Top (SCT) shadowing correction (Frank, 2025) for CSAT3.
# Converts sonic-frame wind to transducer frame, computes SCT coefficients,
# applies correction element-wise, and maps back to sonic coordinates.
#-------------------------------------------------------------------------------

#' @title Definition function: Self/Cross/Top shadowing correction (CSAT3 formulation)
#'
#' @author
#' John Frank  \cr
#' David Durden \email{ddurden@battelleecology.org}
#'
#' @description
#' Applies the Self/Cross/Top (SCT) shadowing correction (Frank, 2025) using CSAT3
#' transducer geometry. Input wind is in sonic coordinates (N × 3). Output includes
#' corrected wind (sonic and transducer frames), per-transducer SCT correction
#' coefficients, wind–transducer angles, and wind–top-structure angles.
#'
#' @param inpVeloSoni Matrix or data.frame (N × 3) of wind components in sonic
#'   coordinates \[m s^-1\]; columns correspond to (veloXaxs [u], veloYaxs [v], veloZaxs [w]).
#'   If a data.frame is provided, all three columns must be numeric and will be coerced
#'   to a matrix. If names match \code{c("veloXaxs","veloYaxs","veloZaxs")}, they will be
#'   ordered accordingly.
#' @param MtrxSoniTran 3 × 3 matrix mapping sonic → transducer coordinates
#'   (columns are unit transducer vectors in sonic coords). CSAT3 by default.
#' @param MtrxTranSoni 3 × 3 matrix mapping transducer → sonic coordinates
#'   (inverse of \code{MtrxSoniTran}). Default matches the inverse of the default above.
#' @param VectTop Numeric length-3 unit vector for “top/bottom” (support structure)
#'   direction (e.g., \code{c(0, 0, 1)}).
#' @param ParaD0 Numeric vector (length 3): model parameter d0 for self (1), cross (2), top (3).
#'   Default: \code{c(0.00159718301425092, 0.0113301152734556, 0.0633033802230689)}.
#' @param ParaD1 Numeric vector (length 3): model parameter d1 for self (1), cross (2), top (3).
#'   Default: \code{c(29.2463049972615, 24.8602835257800, 18.5970206839229)}.
#' @param ParaD2 Numeric vector (length 3): model parameter d2 for self (1), cross (2), top (3).
#'   Default: \code{c(0.836978194375597, 0.874138513424717, 0.809209610043660)}.
#' @param ParaD3 Positive numeric scalar: global scaling to minimize change in horizontal
#'   wind relative to Wyngaard correction. Default: \code{1.06606236917833}.
#' @param ParaP  Positive numeric scalar: Minkowski exponent used to combine components
#'   (default 8, following your script).
#'
#' @return A list with:
#' \itemize{
#'   \item \code{veloWindSoniCorr}: N × 3 data.frame (sonic coords) after SCT correction;
#'         columns \code{veloXaxs, veloYaxs, veloZaxs}.
#'   \item \code{veloWindTranCorr}: N × 3 matrix (transducer coords) after SCT correction.
#'   \item \code{coefSct}: N × 3 matrix of SCT correction coefficients per transducer.
#'   \item \code{angTranDeg}: N × 3 matrix of angles (deg) between wind and transducer axes.
#'   \item \code{angTopDeg}:  N × 1 numeric vector of angles (deg) between wind and \code{VectTop}.
#' }
#'
#' @details
#' Angles \eqn{\theta} are computed between the wind vector and (i) each transducer axis,
#' and (ii) the top-structure direction. For a given parameter triple (d0, d1, d2),
#' the model error is
#' \deqn{
#'   \mathrm{err}(\theta) = \frac{(1 - d_2)\,\tanh\{d_0 (\theta - d_1)\}}
#'                               {\tanh\{d_0 (90 - d_1)\}} + d_2 \, .
#' }
#' The correction coefficient is \eqn{c(\theta) = 1 / \mathrm{err}(\theta)}.
#' Self, cross, and top terms are combined via a Minkowski-like synthesis with exponent
#' \code{ParaP}, then shifted (+1) and scaled by \code{ParaD3}. The correction is applied
#' element-wise in the transducer frame and mapped back to sonic coordinates.
#'
#' @examples
#' set.seed(1)
#' V <- matrix(rnorm(300), ncol = 3)
#' out <- def.corr.sct.csat(
#'   inpVeloSoni = V,
#'   VectTop     = c(0, 0, 1)
#'   # defaults for ParaD0..ParaD3 and ParaP are used
#' )
#' str(out)
#'
#' @keywords eddy-covariance sonic-anemometer correction csat3 sct
#' @export
def.corr.sct.csat <- function(
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
  ),
  VectTop = c(0,0,1),
  ParaD0 = c(0.00159718301425092, 0.0113301152734556, 0.0633033802230689),
  ParaD1 = c(29.2463049972615,   24.8602835257800,   18.5970206839229),
  ParaD2 = c(0.836978194375597,  0.874138513424717,  0.809209610043660),
  ParaD3 = 1.06606236917833,
  ParaP  = 8
) {
  # --- coerce/validate input --------------------------------------------------
  if (is.data.frame(inpVeloSoni)) {
    if (ncol(inpVeloSoni) != 3L)
      stop("inpVeloSoni data.frame must have exactly 3 numeric columns.")
    if (!all(vapply(inpVeloSoni, is.numeric, logical(1))))
      stop("All columns of inpVeloSoni data.frame must be numeric.")
    nameVarExpc <- c("veloXaxs", "veloYaxs", "veloZaxs")
    if (all(nameVarExpc %in% names(inpVeloSoni))) {
      inpVeloSoni <- inpVeloSoni[, nameVarExpc]
    }
    inpVeloSoni <- as.matrix(inpVeloSoni)
  }
  stopifnot(
    is.matrix(inpVeloSoni), ncol(inpVeloSoni) == 3,
    is.matrix(MtrxSoniTran), all(dim(MtrxSoniTran) == c(3, 3)),
    is.matrix(MtrxTranSoni), all(dim(MtrxTranSoni) == c(3, 3)),
    is.numeric(VectTop), length(VectTop) == 3,
    is.numeric(ParaD0), length(ParaD0) == 3,
    is.numeric(ParaD1), length(ParaD1) == 3,
    is.numeric(ParaD2), length(ParaD2) == 3,
    is.numeric(ParaD3), ParaD3 > 0,
    is.numeric(ParaP),  ParaP  > 0
  )
  numRow <- nrow(inpVeloSoni)
  if (numRow == 0) stop("inpVeloSoni has zero rows")
  
  # --- map to transducer frame -----------------------------------------------
  veloWindTran <- inpVeloSoni %*% MtrxSoniTran  # N × 3
  
  # wind magnitude for angle calc (avoid div-by-zero)
  magVeloSoni <- sqrt(rowSums(inpVeloSoni^2))   # N
  denm <- pmax(magVeloSoni, .Machine$double.eps)
  
  # --- angles (degrees) -------------------------------------------------------
  # wind–transducer axes
  angCosTran <- abs(veloWindTran) / denm        # N × 3 (recycling over columns)
  angCosTran[angCosTran > 1] <- 1
  angTranDeg <- acos(angCosTran) * 180 / pi     # N × 3
  
  # wind–top direction
  angCosTop <- abs(drop(inpVeloSoni %*% VectTop)) / denm  # N
  angCosTop[angCosTop > 1] <- 1
  angTopDeg <- acos(angCosTop) * 180 / pi        # N
  
  # --- helper: error model -> coefficient ------------------------------------
  def.err.sct.tanh <- function(angDeg, ParaD0j, ParaD1j, ParaD2j) {
    # angDeg can be N×3 (matrix) or N (vector)
    (1 - ParaD2j) * tanh(ParaD0j * (angDeg - ParaD1j)) / tanh(d0j * (90 - ParaD1j)) + ParaD2j
  }
  
  # --- evaluate error & coefficients -----------------------------------------
  errSelf  <- def.err.sct.tanh(angTranDeg, ParaD0[1], ParaD1[1], ParaD2[1])  # N × 3
  errCros <- def.err.sct.tanh(angTranDeg, ParaD0[2], ParaD1[2], ParaD2[2])  # N × 3
  errTop   <- def.err.sct.tanh(angTopDeg,  ParaD0[3], ParaD1[3], ParaD2[3])  # N
  
  coefSelf  <- 1 / errSelf
  coefCros <- 1 / errCros
  coefTop   <- 1 / errTop
  
  # --- Minkowski-like synthesis per transducer arm ---------------------------
  coefSct <- matrix(0, numRow, 3)
  # Column 1 combines: self@1, cross@2 & @3, and top
  coefSct[, 1] <- (((coefSelf[, 1]  - 1)^ParaP +
                      (coefCros[, 2] - 1)^ParaP +
                      (coefCros[, 3] - 1)^ParaP +
                      (coefTop        - 1)^ParaP)^(1/ParaP)) + 1
  # Column 2 combines: self@2, cross@1 & @3, and top
  coefSct[, 2] <- (((coefCros[, 1] - 1)^ParaP +
                      (coefSelf[, 2]  - 1)^ParaP +
                      (coefCros[, 3] - 1)^ParaP +
                      (coefTop        - 1)^ParaP)^(1/ParaP)) + 1
  # Column 3 combines: self@3, cross@1 & @2, and top
  coefSct[, 3] <- (((coefCros[, 1] - 1)^ParaP +
                      (coefCros[, 2] - 1)^ParaP +
                      (coefSelf[, 3]  - 1)^ParaP +
                      (coefTop        - 1)^ParaP)^(1/ParaP)) + 1
  
  # global scaling
  coefSct <- coefSct / ParaD3
  
  # --- apply correction in transducer frame ----------------------------------
  veloWindTranCorr <- veloWindTran * coefSct              # element-wise
  veloWindSoniMat  <- veloWindTranCorr %*% MtrxTranSoni   # back to sonic frame
  
  # --- convert sonic output to data.frame ------------------------------------
  veloWindSoniCorr <- as.data.frame(veloWindSoniMat)
  names(veloWindSoniCorr) <- c("veloXaxs", "veloYaxs", "veloZaxs")
  # rownames(veloWindSoniCorr) <- NULL  # optional
  
  # --- return ----------------------------------------------------------------
  list(
    veloWindSoniCorr = veloWindSoniCorr,  # data.frame (N × 3)
    veloWindTranCorr = veloWindTranCorr,  # matrix (N × 3)
    coefSct          = coefSct,           # matrix (N × 3)
    angTranDeg       = angTranDeg,        # matrix (N × 3)
    angTopDeg        = angTopDeg          # vector (N)
  )
}
