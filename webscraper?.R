library(rvest)
library(zipcode)
library(tidyverse)
library(stringi)
library(sf)
library(tigris)
#I'm hoping to figure out how to get all 137 pages of this stupid dable into one dataframe. 
# this is page 1 'https://farm.ewg.org/top_recips.php?fips=24000&progcode=total_cr&yr=2017&regionname=Maryland'
#page 2 is #https://farm.ewg.org/top_recips.php?fips=24000&progcode=total_cr&page=1&yr=2017 the last page is https://farm.ewg.org/top_recips.php?fips=24000&progcode=total_cr&page=137&yr=2017
#mostly this is an exploration of how I can pull all pages from a table into one dataframe using the Rvest package. 

#Specifying the url for desired website to be scrapped
url <- 'https://farm.ewg.org/top_recips.php?fips=24000&progcode=total_cr&yr=2017&regionname=Maryland'

#Reading the HTML code from the website
webpage <- read_html(url)

#ok. so now store this page in an xml document called tdist
tdist <- read_html('https://farm.ewg.org/top_recips.php?fips=24000&progcode=total_cr&yr=2017&regionname=Maryland')
#ok now we're going to pull out the data in the table
page1 <- tdist %>%
  #use the html_node command and find the  table poart of the xml document. The priblem with this is, unlike this example https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/, all the table elements have the identical <td> tag. So luckily it's been solved wirth the html_node command. http://bradleyboehmke.github.io/2015/12/scraping-html-tables.html
  html_node("table") %>%
  html_table(header = T)
#now make it a dataframe.
page1 <- data.frame(page1)

#so the above is a great way to scrape that page. But I want things on multiple pages. That means I'm sort of out of luck. But I do know that the pages have similar numbers. So it shouldn't be too hard, right?

#https://github.com/simonmunzert/web-scraping-with-r-extended-edition/blob/master/02-scraping-with-rvest.r

## dealing with multiple pages ---------- taken simonmunzert tutorial. Thanks, bud. 


## example: fetching and analyzing jstatsoft download statistics

# set temporary working directory

tempwd <- ("/GitHub/Chesapeake_maps/temp_maryland_project")
dir.create(tempwd)
setwd(tempwd)

browseURL("https://farm.ewg.org/top_recips.php?fips=24000&progcode=total_cr&yr=2017&regionname=Maryland")

# construct list of urls
baseurl <- "https://farm.ewg.org/top_recips.php?fips=24000&progcode=total_cr&page="
volurl <- paste0(1:137)
brurl <- paste(volurl,"&yr=2017",sep="")
#pastes the numbers and the base url creates above before adding them all together with no spaces 
urls_list <- paste(baseurl,brurl,sep="")
names <- paste(volurl, brurl,sep="")



# download pages
folder <- "/GitHub/Chesapeake_maps/html_articles/"
dir.create(folder)
for (i in 1:length(urls_list)) {
  if (!file.exists(paste0(folder, names[i]))) {
    download.file(urls_list[i], destfile = paste0(folder, names[i]))
    Sys.sleep(runif(1, 0, 1))
  }
}

# check success
list_files <- list.files(folder)
list_files_path <-  list.files(folder, full.names = TRUE)
length(list_files)


# import pages and extract content. Basically I'm creating a loop that says 

table_out <- list()# Creates an empty list.
#for each one in my list
for (i in 1:length(list_files_path)) {
  #grab out the html from my lists
  html_out <- read_html(list_files_path[i])
#find the table and collect it into a list
    table_out[[i]] <- html_table(html_out, fill = TRUE)
  # Writes the table to next element in the list.
}
################# this also works ####

#for (i in 1:length(list_files_path)) {
#  html_out <- read_html(list_files_path[i])
#identify the html node caled table and turn it into a list
#  table_out[[i]] <-html_out %>% html_nodes("table") %>%
#    html_table(header = T)
# Writes the table to next element in the list.
#}

#ok, create a dataframe out of all of my lists
df_out <- map_dfr(table_out, 1)
#make the names consistent between the extra first dataframe and the later ones
names(df_out) <- c("Rank", "Recipient", "Location", "Conservation Reserve Program 2017")
names(page1) <-c("Rank", "Recipient", "Location", "Conservation Reserve Program 2017")
#make one dataframe out of all this stupid stuff. 
df_out1 <- rbind(df_out, page1)
#get a drink
#have_drink(chris, whisky, one_ice_cube = T)



#############TIME to figure out where these places are##############

#Stolen from other code I've stolen from a different project I was working on about the gubernitorial election. 

contributions_all_zips1 <- stri_extract_all_regex(df_out1$Location, "(?<!\\d)(\\d{5}(?:[-\\s]\\d{4})?)\\b")
contributions_all_5digitzips <- stri_extract_all_regex(contributions_all_zips1, "\\d{5}")
#I was getting some extra 5 digit combos from addresses, so I used this lapply command to just grab the last one, since the zip is always last. 
contributions_all_zips2 <- map(contributions_all_5digitzips, function(x) x[length(x)])
#creates the dataframe hogan_c as new hogan_c
contributions_all_zipz_df <- data_frame(contributions_all_zips2)
#changes the list to a numeric value. This helps the group by function which do4sn't like lists.
contributions_all_zipz_df %>% mutate_if(is.list, as.character) -> contributions_all_zipz_df
#changes the column header in the new dataframe to zip_codes. this will be helpful later on because it is unique. 
colnames(contributions_all_zipz_df)[which(names(contributions_all_zipz_df) == "contributions_all_zips2")] <- "zip_codes"

#binds it to the hogan contributions database
newall_df_with_zips <- cbind(contributions_all_zipz_df, df_out1)


#ok, so now that we have our new list with the added zip codes, we can go through and group them and count them. 


##########################4##########################

#now let's combine with the zip codfes database so we can group by state and county 

#pulls the database of zip codes/state information 
data(zipcode)
#creates a merged list of zip codes and the hogan information. 
Farm_list_fromzips <-  left_join(newall_df_with_zips, zipcode, by =c("zip_codes" = "zip"))

## so now that we've got the two tables merged, It's time to sort out the Maryland contributors
#removes dollar sign from the items, which will be helpful later
Farm_list_fromzips[] <- lapply(Farm_list_fromzips, gsub, pattern="\\$", replacement='')

#filters to just Maryland farms
maryland_farms <- Farm_list_fromzips %>% 
  filter(state == "MD")

#and finally, let's get rid of commas so they're going to be numbers. 
maryland_farms$`Conservation Reserve Program 2017` <- as.numeric(gsub(",","",maryland_farms$`Conservation Reserve Program 2017`))
  
#so I'd like to group and sum the funds by zip code so we could build a chloropleth 
maryland_farms_zips <- maryland_farms %>%
  group_by(zip_codes)%>%
  summarise(total_funds = sum(`Conservation Reserve Program 2017`))

#Now let's join it with a shapefile so we can get the whoel thing to look like a fun, fun map. 
maryland_boundaries<- read_csv("maryland_political_boundaries_zip_codes_11_digit.csv")

Maryland_zips_shape <- left_join(maryland_boundaries, maryland_farms_zips, by =c("zipcode1" = "zip_codes"))


#now let's export a map that shows the amount of money based on a value 

write_csv(Maryland_zips_shape, "Maryland_zips_shape.csv")


# set sf option
options(tigris_class = "sf")
maryland <- tracts(state="MD",cb=T)
# If cb is set to TRUE, download a generalized (1:500k) counties file. Defaults to FALSE
# (the most detailed TIGER file).

maryland_farms$latitude <- as.numeric(maryland_farms$latitude)
maryland_farms$longitude <- as.numeric(maryland_farms$longitude)

class(maryland_farms$longitude)

ggplot(maryland) +
  geom_sf() +
  geom_point(data=maryland_farms, aes(x=longitude, y=latitude), color="red") +
  #geom_point(data=stops_spatial, aes(x=lon, y=lat), color="blue") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="Farms that got CREP funding")

