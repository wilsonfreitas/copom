#' Tools to study the decisions of the COPOM meetings
#'
#' It implements tools to study the impact of decision made in the COPOM
#' meetings on the term structure of interest rates in Brazil
#' The flatforwardCOPOM interpolation to spot rate curves is implemented.
#' This interpolation considers that the interest rate between COPOM meetings
#' in Brazil are flat, instead of being flat betweet bonds maturities.
#' This is mainly relevant for the short term of the term structure where
#' the interpolation is used to price private bonds and interest rate
#' derivatives.
#' This is discussed in the book Brazilian Derivatives and Securities.
#' There are also classes to evaluate how the FOCUS report expectations impact
#' the spot rate curve and functions to help with visualization.
#'
#' @name copom-package
#' @aliases copom
#' @author Wilson Freitas \email{wilson.freitas@gmail.com}
#' @references Marcos C. S. Carreira and Richard J. Brostowicz.
#'             "Brazilian Derivatives and Securities", Palgrave Macmillan, 2016
#' @importFrom fixedincome forwardrate maturities as.spotratecurve term
#' @importFrom fixedincome compound prepare_interpolation toyears spotratecurve
#' @importFrom fixedincome spotratecurve implied_rate interpolation<-
#' @importFrom fixedincome interpolation_error shift as.spotrate
#' @importFrom fixedincome fit_interpolation
#' @importFrom bizdays bizdays
#' @importFrom grDevices xy.coords
#' @importFrom stats optim approxfun
#' @importFrom methods new
#' @docType package
NULL