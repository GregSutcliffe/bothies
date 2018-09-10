library(leaflet)
library(rvest)
library(dplyr)

url <-"https://en.wikipedia.org/wiki/List_of_Mountain_Bothies_Association_bothies"
bothies <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[3]/div[3]/div[4]/div/table[1]') %>%
  html_table()
bothies <- bothies[[1]] %>%
  transmute(name = `Name[23]District`,
            elevation_estate = `Elevation[26][note 4]Estate[23]`,
            sleeps = `Sleeps[26] [note 3]`,
            gridref_latlong = `Grid ref[23]Lat/long[26][note 2]`)

bothies <- bothies %>% 
  mutate(name = gsub("([[:lower:]]|\\))([[:upper:]])", "\\1:\\2", name)) %>%
  separate(name,c('name','area'),sep=':',fill='right') %>%
  na.omit

bothies <- bothies %>% 
  mutate(elevation_estate = gsub("(\\))", "\\1:", elevation_estate)) %>%
  separate(elevation_estate,c('elevation'),sep=':',extra='drop')

bothies <- bothies %>%
  separate(gridref_latlong,c('blah','blah1','latlong'),extra= 'drop',sep='/') %>%
  separate(latlong,c('lat','long'),sep=';') %>%
  mutate(long = trimws(long)) %>%
  separate(long,c('long'),extra='drop',sep=' ') %>%
  mutate(long = gsub("^(-?[[:digit:]]*\\.[[:digit:]]*).*","\\1", long)) %>%
  mutate(long = as.numeric(long), lat = as.numeric(lat)) %>%
  select(-blah,-blah1)

bothies <- bothies %>% separate(sleeps,c('sleeps'),extra='drop',sep='\\[')

bothies <- bothies %>%
  separate(elevation,c('meters'),remove = F, extra = 'drop', sep = 'm') %>%
  mutate(meters=as.numeric(gsub("^([[:digit:]]*).*","\\1",meters)))

getColor <- function(.data) {
  sapply(.data$meters, function(meters) {
    if(meters <= 200) {
      "green"
    } else if(meters <= 400) {
      "orange"
    } else {
      "red"
    } })
}

bothies <- mutate(bothies, tooltip = paste(sep = "<br/>",
                                           name,
                                           area,
                                           paste(sep=':',"Sleeps",sleeps)
))

icons <- awesomeIcons(
  icon = 'map-marker-alt',
  iconColor = 'black',
  library = 'fa',
  markerColor = getColor(bothies)
)

library(htmlwidgets)
library(htmltools)

rr <- tags$div(
  strong("Mountain Association Bothies in Great Britain"),
  br(),
  a(href=url,"Source: Wikipedia:Mountain Association Bothies"),
  br(),
  a(paste('Generated at:',Sys.time()))
)  

map_leaflet <- leaflet(data = bothies) %>%
  addTiles() %>%
  addAwesomeMarkers(~long, ~lat, 
             icon = icons,
             popup = ~as.character(tooltip),
             label = ~as.character(name)
             ) %>%
  addControl(rr, position = "bottomleft") %>%
  addLegend(title = 'Bothy Elevation',
            colors = c('green','orange','red'),
            labels=c('<200m','<400m','>400m')) 

saveWidget(map_leaflet, file="/tmp/map.html")
map_leaflet