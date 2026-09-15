##Clear libraries

rm(list=ls()) #Reset workspace

##Load libraries

library("ncdf4") #Load Netcdf manipulation library

##Create functions

##Extract dates from a netCDF
get_dates <- function(nc) {
  
  time_vals <- ncvar_get(nc, "time")
  
  time_units <- ncatt_get(nc, "time", "units")$value
  # e.g. "days since 1950-01-01"
  
  origin <- sub(".*since ", "", time_units)
  
  as.Date(time_vals, origin = origin)
}
get_periods <- function(dates) {
  
  years <- as.numeric(format(dates, "%Y"))
  
  list(
    hist = which(years >= 1990 & years <= 2009),
    
    fut = list(
      "2030" = which(years >= 2021 & years <= 2040),
      "2035" = which(years >= 2026 & years <= 2045),
      "2040" = which(years >= 2031 & years <= 2050),
      "2045" = which(years >= 2036 & years <= 2055),
      "2050" = which(years >= 2041 & years <= 2060),
      "2055" = which(years >= 2046 & years <= 2065),
      "2060" = which(years >= 2051 & years <= 2070),
      "2065" = which(years >= 2056 & years <= 2075),
      "2070" = which(years >= 2061 & years <= 2080),
      "2075" = which(years >= 2066 & years <= 2085),
      "2080" = which(years >= 2071 & years <= 2090),
      "2085" = which(years >= 2076 & years <= 2095),
      "2090" = which(years >= 2081 & years <= 2100)
    )
  )
}
get_agg_fun <- function(varname){
  
  if(varname %in% c(
    "R99p",
    "R20mm",
    "FFDIgt50",
    "WSDI"
  )){
    sum
  } else {
    mean
  }
  
}
calc_hist_mean <- function(nc, varname, hist_period){
  
  dat <- ncvar_get(nc, varname)
  
  agg_fun <- get_agg_fun(varname)
  
  apply(
    dat[,,hist_period],
    c(1,2),
    agg_fun,
    na.rm = TRUE
  )
  
}
calc_pct_change <- function(nc_hist, nc_future, varname, hist_period, future_period){
  
  hist <- ncvar_get(nc_hist, varname)
  fut  <- ncvar_get(nc_future, varname)
  
  agg_fun <- get_agg_fun(varname)
  
  hist_mean <- apply(
    hist[,,hist_period],
    c(1,2),
    agg_fun,
    na.rm = TRUE
  )
  
  fut_mean <- apply(
    fut[,,future_period],
    c(1,2),
    agg_fun,
    na.rm = TRUE
  )
  
  ifelse(
    hist_mean == 0,
    NA,
    ((fut_mean - hist_mean) / hist_mean) * 100
  )
}
write_model_pct_nc <- function(template_nc, hist_mean, pct_change_list, model_name, scenario, outfile){
  
  src_var <- template_nc$var[[varname]]
  
  spatial_dims <- src_var$dim[
    !tolower(sapply(src_var$dim, function(x) x$name)) %in%
      c("time","times")
  ]
  
  dim1 <- ncdim_def(
    spatial_dims[[1]]$name,
    spatial_dims[[1]]$units,
    spatial_dims[[1]]$vals
  )
  
  dim2 <- ncdim_def(
    spatial_dims[[2]]$name,
    spatial_dims[[2]]$units,
    spatial_dims[[2]]$vals
  )
  
  period_values <- c(
    0,
    as.numeric(names(pct_change_list))
  )
  
  period_dim <- ncdim_def(
    "period",
    "year",
    vals = period_values
  )
  
  var_def <- ncvar_def(
    name = paste0(varname, "_summary"),
    units = src_var$units,
    dim = list(dim1, dim2, period_dim),
    missval = -9999,
    longname = paste(
      varname,
      "historical mean and future percentage changes"
    ),
    prec = "float"
  )
  
  nc_out <- nc_create(outfile, var_def)
  
  nper <- length(pct_change_list)
  
  out_array <- array(
    NA,
    dim = c(
      dim(hist_mean)[1],
      dim(hist_mean)[2],
      nper + 1
    )
  )
  
  # Historical mean = first layer
  out_array[,,1] <- hist_mean
  
  # Percentage change layers
  for(i in seq_len(nper)){
    out_array[,,i + 1] <- pct_change_list[[i]]
  }
  
  out_array[is.na(out_array)] <- -9999
  
  ncvar_put(
    nc_out,
    var_def,
    out_array
  )
  
  ncatt_put(
    nc_out,
    0,
    "model",
    model_name
  )
  
  ncatt_put(
    nc_out,
    0,
    "scenario",
    scenario
  )
  
  ncatt_put(
    nc_out,
    0,
    "historical_period",
    "1990-2009"
  )
  
  ncatt_put(
    nc_out,
    0,
    "calculation",
    "(future_mean - historical_mean)/historical_mean * 100"
  )
  
  ncatt_put(
    nc_out,
    "period",
    "labels",
    paste(
      c("historical_mean", names(pct_change_list)),
      collapse = ","
    )
  )
  
  nc_close(nc_out)
}

##Import files

base_dir <- "C:/Users/MorganBennet/Documents/Nandos/"

nc_ACCESS_hist <- nc_open(paste0(base_dir,"")) #Load historical run
nc_ECEarth_hist <- nc_open(paste0(base_dir,"")) #Load historical run
nc_MPI_hist <- nc_open(paste0(base_dir,"")) #Load historical run
nc_NORESM_hist <- nc_open(paste0(base_dir,"")) #Load historical run
nc_UKESM_hist <- nc_open(paste0(base_dir,"")) #Load historical run

nc_ACCESS_126 <- nc_open(paste0(base_dir,"")) #Load SSP1
nc_ECEarth_126 <- nc_open(paste0(base_dir,"")) #Load SSP1
nc_MPI_126 <- nc_open(paste0(base_dir,"")) #Load SSP1
nc_NORESM_126 <- nc_open(paste0(base_dir,"")) #Load SSP1
nc_UKESM_126 <- nc_open(paste0(base_dir,"")) #Load SSP1

nc_ACCESS_370 <- nc_open(paste0(base_dir,"")) #Load SSP3
nc_ECEarth_370 <- nc_open(paste0(base_dir,"")) #Load SSP3
nc_MPI_370 <- nc_open(paste0(base_dir,"")) #Load SSP3
nc_NORESM_370 <- nc_open(paste0(base_dir,"")) #Load SSP3
nc_UKESM_370 <- nc_open(paste0(base_dir,"")) #Load SSP3

##Create .nc file list for SSP

nc_Histlist <- list(
  ACCESS = nc_ACCESS_hist,
  ECEarth = nc_ECEarth_hist,
  MPI = nc_MPI_hist,
  NORESM = nc_NORESM_hist,
  UKESM = nc_UKESM_hist)

nc_126list <- list(
  ACCESS = nc_ACCESS_126,
  ECEarth = nc_ECEarth_126,
  MPI = nc_MPI_126,
  NORESM = nc_NORESM_126,
  UKESM = nc_UKESM_126)

nc_370list <- list(
  ACCESS = nc_ACCESS_370,
  ECEarth = nc_ECEarth_370,
  MPI = nc_MPI_370,
  NORESM = nc_NORESM_370,
  UKESM = nc_UKESM_370)

##Settings

varname <- "WSDI" #R20mm, R99p, FFDIgt50, TX90p, WSDI

scenario_lists <- list(
  SSP126 = nc_126list,
  SSP370 = nc_370list
)

dates_hist <- get_dates(nc_ACCESS_hist)
dates_fut <- get_dates(nc_ACCESS_370)
periods_hist <- get_periods(dates_hist)$hist
period_fut <- get_periods(dates_fut)$fut

output_dir <- "C:/Users/MorganBennet/Documents/Nandos/PctChange"
dir.create(output_dir,
           recursive = TRUE,
           showWarnings = FALSE)

##Code

for(scenario in names(scenario_lists)){
  
  message("Processing model outputs for ", scenario)
  
  future_list <- scenario_lists[[scenario]]
  
  for(mod in names(nc_Histlist)){
    
    message("   Model: ", mod)
    
    hist_mean <- calc_hist_mean(
      nc = nc_Histlist[[mod]],
      varname = varname,
      hist_period = period_hist
    )
    
    pct_list <- lapply(names(period_fut), function(per){
      
      calc_pct_change(
        nc_hist       = nc_Histlist[[mod]],
        nc_future     = future_list[[mod]],
        varname       = varname,
        hist_period   = period_hist,
        future_period = period_fut[[per]]
      )
      
    })
    
    names(pct_list) <- names(period_fut)
    
    outfile <- file.path(
      output_dir,
      paste0(
        varname,
        "_",
        scenario,
        "_",
        mod,
        "_PctChange.nc"
      )
    )
    
    write_model_pct_nc(
      template_nc    = nc_Histlist[[mod]],
      hist_mean = hist_mean,
      pct_change_list = pct_list,
      model_name     = mod,
      scenario       = scenario,
      outfile        = outfile
    )
    
    message("Saved: ", outfile)
  }
}
