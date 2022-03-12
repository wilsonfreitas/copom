#' Flat forward interpolation considering COPOM meetings
#'
#' The package \code{flatforwardCOPOM} implements flatforwardCOPOM
#' interpolation to spot rate curves.
#' This interpolation considers that the interest rate between COPOM meetings
#' in Brazil are flat, instead of being flat betweet bonds maturities.
#' This is mainly relevant for the short term of the term structure where
#' the interpolation is used to price private bonds and interest rate
#' derivatives.
#' This is discussed in the book Brazilian Derivatives and Securities.
#'
#' @name flatforwardCOPOM-package
#' @aliases flatforwardCOPOM
#' @author Wilson Freitas \email{wilson.freitas@gmail.com}
#' @references Marcos C. S. Carreira and Richard J. Brostowicz.
#'             Brazilian Derivatives and Securities, Palgrave Macmillan, 2016
#' @importFrom fixedincome forwardrate maturities as.spotratecurve term
#' @importFrom fixedincome compound prepare_interpolation toyears spotratecurve
#' @importFrom fixedincome spotratecurve rates
#' @importFrom bizdays bizdays
#' @importFrom grDevices xy.coords
#' @importFrom stats optim approxfun
#' @importFrom methods new
#' @docType package
NULL