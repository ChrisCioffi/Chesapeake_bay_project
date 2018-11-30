library(tidyverse)
#so carto gets crabby when you try to make a chloropleth with a shapefile and a datafile. So I'm joining them in R. I've "flattened" out the countoes_lower_48 shapefile and exported it from Carto. now I'm koining it with the manure and acres dataset and the county names dataset to get the state names. I will then filter out the states I want, West Virginia, Delaware, Pennsylvania, New York, Maryland, Virginia, and D.C...I'm having trouble finding DC. Perhaps I'll find that out later. 

#Let's first read in the files.
shapefile <- read_csv("counties_lower_48.csv")
manure <- read_csv("Manure_farmland.csv")
county_names <- read_csv("County_Names.csv")

#make sure the fields I want to join are integers
class(shapefile$atlas_stco)
class(manure$FIPS)
#whoops, atlas_stco is a character. Let's makle it an ingeger 
shapefile$atlas_stco <- as.integer(shapefile$atlas_stco)

#OK now we're going to join three datasets so we can have the state name as well as the counties and the shapefile.
new_lower_48_shapefile <- left_join(shapefile, manure, by = c("atlas_stco" = "FIPS") )
lower_48_with_states <- left_join(new_lower_48_shapefile, county_names, by = c("atlas_stco" = "FIPSTEXT"))


#alright, let's make sure we've got the fields we want, and the states we want. 
chesapeake_bay_states <- lower_48_with_states %>%  
  select(the_geom, cartodb_id, atlas_name, atlas_acre, atlas_area, `Acres of Cropland and Pastureland Treated with Animal Manure as Percent of Total Cropland Acreage_valueNumeric`, `Acres of Total Cropland as Percent of Land Area in Acres: 2012_valueNumeric`, StateName) %>%
  filter(StateName %in%  c("Virginia", "West Virginia", "Delaware", "Pennsylvania", "New York", "Maryland") )
  

#now lets export it back out as a CSV for importing back into Carto.

 write_csv(chesapeake_bay_states, "chesapeake_with_manure_usage.csv")

 
