library(rnoaa)
library(ncdf4)

buoys(dataset = "cwind")

library("rerddap")
servers()
ed_search_adv(query= "salinity")
ed_datasets(which= "tabledap", url= "http://www.neracoos.org/erddap/")

?rnoaa
?buoy

Examples
  # Get buoy station information
  x <- buoy_stations()
  # refresh stations as needed, takes a while to run
  # you shouldn't need to update very often
  # x <- buoy_stations(refresh = TRUE)
  if (interactive() && requireNamespace("leaflet")){
    library("leaflet")
    z <- leaflet(data = na.omit(x))
    z <- leaflet::addTiles(z)
    leaflet::addCircles(z, ~lon, ~lat, opacity = 0.5)
  }
  
  # Get available buoys
  buoys(dataset = 'cwind')
  
  # Get data for a buoy
  ## if no year or datatype specified, we get the first file
  buoy(dataset = 'cwind', buoyid = 46085)
  
  # Including specific year
  buoy(dataset = 'cwind', buoyid = 41001, year = 1999)
  
  # Including specific year and datatype
  buoy(dataset = 'cwind', buoyid = 45005, year = 2008, datatype = "c")
  buoy(dataset = 'cwind', buoyid = 41001, year = 1997, datatype = "c")
  
  # Other datasets
  buoy(dataset = 'ocean', buoyid = 41029)
  
  # curl debugging
  buoy(dataset = 'cwind', buoyid = 46085, verbose = TRUE)
  
  # some buoy ids are character, case doesn't matter, we'll account for it
  buoy(dataset = "stdmet", buoyid = "VCAF1")
  buoy(dataset = "stdmet", buoyid = "wplf1")
  buoy(dataset = "dart", buoyid = "dartu")
  
}

## End(Not run)



whichsalin <- ed_search(query = "SST")

