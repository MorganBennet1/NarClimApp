#========================================================
# CREATE FUNCTIONS
#========================================================

get_model <- function(filename){
  
  x <- basename(filename)
  
  x <- sub("\\.nc$", "", x)
  
  parts <- strsplit(x, "_")[[1]]
  
  paste(
    parts[3:(length(parts)-1)],
    collapse = "_"
  )
}

extract_point <- function(
    ncfile,
    lon,
    lat,
    periods_selected
){
  
  nc <- nc_open(ncfile)
  
  on.exit(
    nc_close(nc)
  )
  
  var_name <- names(
    nc$var
  )[1]
  
  vals <- ncvar_get(
    nc,
    var_name
  )
  
  dim_names <- names(
    nc$dim
  )
  
  lon_name <- dim_names[
    grepl(
      "lon|longitude|x",
      tolower(dim_names)
    )
  ][1]
  
  lat_name <- dim_names[
    grepl(
      "lat|latitude|y",
      tolower(dim_names)
    )
  ][1]
  
  lons <- nc$dim[[lon_name]]$vals
  lats <- nc$dim[[lat_name]]$vals
  
  period_vals <- nc$dim$period$vals
  
  ix <- which.min(
    abs(lons - lon)
  )
  
  iy <- which.min(
    abs(lats - lat)
  )
  
  map_df(
    periods_selected,
    function(period){
      
      pindex <- which(
        period_vals == as.numeric(period)
      )
      
      tibble(
        Period = period,
        Value = vals[ix, iy, pindex]
      )
      
    }
  )
}
