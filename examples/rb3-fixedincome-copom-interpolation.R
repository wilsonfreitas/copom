source("examples/utils-functions.R")

# refdate <- getdate("last bizday", Sys.Date(), "Brazil/ANBIMA")
refdate <- as.Date("2022-09-05")
di1_curve <- get_di1_curve(refdate)

di1_curve_part <- fixedincome::first(di1_curve, "3 years")
copom_dates <- get_copom_dates(refdate, 8)
interpolation(di1_curve_part) <- interp_flatforwardcopom(copom_dates, "optimize")
interpolation(di1_curve_part)@moves$moves * 1e4

plot(di1_curve_part, show_forward = TRUE)
