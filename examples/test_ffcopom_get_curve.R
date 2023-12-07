
library(rb3)
library(bizdays)
library(fixedincome)
library(copom)
library(tidyverse)
source("examples/utils-functions.R")

refdate <- getdate("last bizday", Sys.Date(), "Brazil/ANBIMA")
di1_curve <- get_di1_curve(refdate)

copom_dates <- get_copom_dates(refdate, 6)
interpolation(di1_curve) <- interp_flatforwardcopom(copom_dates, "optimize")
interpolation(di1_curve)@moves$moves * 1e4
