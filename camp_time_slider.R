library(sf)
library(plotly)
library(raster)
library(rjson)
library(lubridate)
library(RColorBrewer)
library(viridis)


####---- Generic Data Processing ----####

#### Read in CSV and geoJSON data into lists ####

options(digits = 2)                                               #sets rounding digits for area and density

temp = list.files(path = "CSV/",pattern="*.csv")                  #list of all CSV viles

x <- seq(1, length(temp),1)                                       #sequence as long as number of CSVs, used for loop functions

mydata <- list()                                                  #empty list to fill

for (i in x){
  thing <- read.csv(file = paste0("CSV/",temp[[i]]))              #reads in each CSV from temp list
  mydata[[i]] <- thing                                            #adds data to data list
}

for (i in x){
  mydata[[i]]$Shape_Area <- round(mydata[[i]]$Shape_Area, digits = 0) #rounds shape area column
  mydata[[i]]$Density <- round((mydata[[i]]$Density), digits = 3) #rounds density column
}

temp = list.files(path = "JSON/", pattern = "*.geojson")          #list of all geoJSON files

mygeo = list()                                                    #empy list to fill

for (i in x){
  thing <- fromJSON(file = paste0("JSON/",temp[[i]]))             #reads in each geoJSON file from temp list
  mygeo[[i]] <- thing                                             #adds data to geo list
}

####---- Plotly Map ----####

#### Create Labels from Dates ####

sf <- stamp("Jan 13", order = "Obd")                            #create date stamp for labeling

for (i in x){
  mydata[[i]]$Labels <- mydata[[i]]$Date                          #create new labal column & fill with dates

  mydata[[i]]$Labels <- sf(mdy(mydata[[i]]$Labels))               #updates label dates with stamp dates
  
}

#### Loop for creating maps and associated slider steps ####

steps <- list()                                                   #empty list to hold slider details
fig <- plot_ly()                                                  #new empty figure for plotly

for (i in x) {                                                    #loops through each geoJSON & CSV to create maps, and sliders 
  
  
  camp <- c(paste0("Camp: ", mydata[[i]]$Camp, "<br>"))
  tents <- c(paste0("Tents: ", mydata[[i]]$Tents))                #counts tents for hover text
  area <- c(paste0(" Area: ", mydata[[i]]$Shape_Area))             #states area for hover text
  density <- c(paste0(" Density: ", (mydata[[i]]$Density*100)))    #states density for hover text
  hover <- array(c(camp,tents,area,density), 
                 dim = c(length(mydata[[i]]$Shape_Area),4))        #tents, area, and density are put into an array for hover text 
  
  
  
  fig <- fig %>% add_trace(                                        #add polygons/data to map in multiple layers
    type = "choroplethmapbox",
    geojson = mygeo[[i]],                                          #area info 
    locations = mydata[[i]]$OBJECTID,                              #links geoJSON and CSV data
    z = (mydata[[i]]$Density * 100),                               #CSV data used for choropleth
    colorscale = "Viridis",
    featureidkey = "properties.OBJECTID",
    showscale = T,
    zmin = 0,                                                      #sets color bar scale
    zmax = 4,                                                      #sets color bar scale
    marker=list(line=list(                                         #removes boarders from polys
      width=0)),
    colorbar = list(len = 1,                                       #sets consistent colorbar for all maps 
                    y = 0.5, 
                    yanchor = "middle",
                    thicknessmode = "fraction",
                    thickness = .02,
                    ticklabelposition = "inside",
                    ticklen = 8,
                    tickfont = list(
                      size = 11,
                      color = "#33303f"
                    ),
                    title = list(
                      text = "Density",
                      side = "bottom",
                      font = list(
                        size = 16,
                        color = "#33303f"
                      )
                    )
                    ),
    hovertext = hover,                                             #sets hover text to created array
    hoverinfo = "text"                                             #only show array dada (no location values)
  )
  
  step <- list(args = list('visible', rep(FALSE, length(mygeo))),  #creates a list for step values set to FALSE for each map
                      method = 'restyle', 
                      label = print(mydata[[i]]$Labels[[1]]))      #labels each map with its associated date
  
  step$args[[2]][i] = TRUE                                         #turns on the single map selected, set to true, all other maps left invisible
  steps[[i]] = step                                                #adds on/off list to steps list 
} 

#### Create Layout for Map ####

fig <- fig %>% layout(                                            #sets look of map, title, sliders, etc...
  mapbox = list(
    style = "carto-darkmatter",                                   #map style set
    center = list(lon = -111.898, lat = 40.755),                  #center corrdinates of map set
    zoom = 12.5),                                                 #zoom of map on cords set
  margin = list( 
    t = 65
  ),
  title = list(
    text = "Salt Lake City Encampments",                          #map title is set and stylized
    font = list(
      size = 28,
      color = "#33303f"
    ),
    yanchor = "top",
    pad = list(
      t = 10
    )
  ), 
  sliders = list(list(active = 0,                               #turns sliders on
                      bgcolor = "#838289",                       #stylize sliders
                      bordercolor = "#33303f",                   #stylize sliders
                      activebgcolor = "#292929",                 #stylize sliders
                      pad = list(                                #stylize sliders
                        b = 10
                      ),
                      currentvalue = list(                       #use current value feature
                        prefix = "Date: ",                       #call it date and pull label (ie date)
                        offset = 20,
                        font = list(
                          size = 15,
                          color = "#33303f"
                        )
                        ), 
                      font = list(
                        color = "#33303f"
                      ),
                      steps = steps,                             #use steps data 
                      transition = list(
                        duration = 0
                      )))
)

#### Set Mapbox Token, Output Map, Save as HTML ####
fig <- fig %>%                                                  #key for html mapbox pages
  config(mapboxAccessToken = Sys.getenv("pk.eyJ1IjoiZ2thc3NpZGF5IiwiYSI6ImNrbGJqOHVnbDJ3Y3AzMnFtMWpxMTJnZTAifQ.gpfgCnyTuXAsEutgOWY0mg"))   

fig                                                             #view map in RStudio

htmlwidgets::saveWidget(as_widget(fig), 
                        paste0(mydata[[length(temp)]]$Labels[[1]],"_map.html")) #save map as html file

####---- Plotly Plot ----####

#### Organize/Colate Data For Plots ####

encampments <- c()                                                #empty list to fill

for (i in x){                                                     #cylces through each date
  
  y <- seq(1, length(mydata[[i]]$Camp),1)                         #gets number of camps for the date
  
  for (j in y){                                                   #runs through each camp for the date
    
    if (mydata[[i]]$Camp[j] %in% encampments) {                   #if the camp is already in encampments skip
      
    
      } else {
        
      encampments <- append(encampments, mydata[[i]]$Camp[j])     #add camp to encampments if it isn't already
      
    }
    
  }

}

encampments <- sort(encampments)                                  #alphabetizes encampments list

dates <- c()                                                      #empty list to fill

for (i in x){                                                     #cyles through each file
  
  y <- seq(1, length(mydata[[i]]$Date),1)                         #list of date entries for file
  
  for (j in y){                                                   #cylces through date entries
     
    if (mydata[[i]]$Date[j] %in% dates) {                         #if the date is already in dates skip
      
      dates <- dates
      
    } else {                                                      #add date to dates if it isn't on the list
      
      dates <- append(dates,mydata[[i]]$Date[j])
      
    }
    
  }
  
}

counts <- list()                                          #empty list to fill with camp/tent dataframes

for (i in x){                                             #loops through each CSV
  
   c <- c(mydata[[i]]$Camp)                               #creates list of camps
   t <- c(mydata[[i]]$Tents)                              #creates list of tent counts
   
   counts[[i]] <- data.frame(c,t)                         #turns camps/tents into data frame and adds to list
    
}

encampments.df <- as.data.frame(encampments)                     #sets top row of df as encampment names

for (i in x) {                                                   #loops through each CSV
  
  z <- seq(1, length(encampments), 1)                            #makes sequence equal to number of encampments
  q <- c()                                                       #empty list to fill with tent counts per encampment
  
  for (j in z){                                                  #loops through each encampment name
    
    if(encampments[[j]] %in% counts[[i]]$c){                     #checks if encampment is in current data set
      l <- which(counts[[i]] == encampments[[j]], arr.ind = T)   #gives df row & column for encampment 
      q <- append(q, counts[[i]]$t[l[1]])                        #finds tent count associated with encampment and adds it df
    }else{
      q <- append(q, NA)                                          #if the encampment isn't in the data set to 0
    }
  }
  encampments.df <- cbind(encampments.df, q)                     #adds all encampment tent counts for date to df
} 

row.names(encampments.df) <- encampments                         #name rows encampments
encampments.df[1] <- NULL                                        #remove excess 'encampments column
colnames(encampments.df) <- dates                                #name columns from dates
encampments.df <- t(encampments.df)                              #transpose df, turns it into a matrix
encampments.df <- as.data.frame(encampments.df)                  #convert back to final df


encampments.df <- cbind(encampments.df, "Total Tents" = rowSums(encampments.df, na.rm = T)) #add total tents count summed row


#### Loop for creating plots and associated buttons ####

fig2 <- plot_ly()                                                           #empty plot to fil
buttons <- list()                                                           #empty list for buttons
encampments <- append(encampments, "Total Tents")                           #add Total Tents to encampment names

for (i in seq(1, length(encampments),1)){
  
  if ( i == length(encampments)){                                           #if last column (ie Total Tents)
    
    fig2 <- fig2 %>% add_trace(encampments.df,                                  
                               x = dates,
                               y = encampments.df[,i], 
                               type = 'scatter', 
                               mode = 'lines+markers',
                               name = encampments[i],
                               line = list(color = 'rgba(67,67,67,1)'),     #specify black color
                               marker = list(size = 8,                      #larger marker
                                             color = 'rgba(67,67,67,1)'))   #specify black color
    
  }else{
    
    fig2 <- fig2 %>% add_trace(encampments.df,                              #other encampments get generic plotly color
                            x = dates,
                            y = encampments.df[,i], 
                            type = 'scatter', 
                            mode = 'lines+markers',
                            name = encampments[i],
                            marker = list(size = 7))                          
  }
  
  if ( i == length(encampments)){
    
    step <- list(args = list('visible', rep(TRUE, length(encampments)+1)),  #Total Tents step sets all plots to visible/TRUE
                 method = 'restyle', 
                 label = "Total Tents")
    buttons[[length(encampments)+1]] = step     
    
  }else{
    
  step <- list(args = list('visible', rep(FALSE, length(encampments)+1)),   #creates a list for button values set to FALSE for each line
               method = 'restyle', 
               label = print(encampments[i]))                               #labels each line with its associated camp
  
  step$args[[2]][i] = TRUE                                                  #turns on the single line selected, set to true, all other lines left invisible
  buttons[[i+1]] = step 
  
  }
}

step <- list(args = list('visible', rep(TRUE, length(encampments)+1)),      #creates a list of all lines tured on
             method = 'restyle', 
             label = "All Camps")                                           #label this step All Camps

step$args[[2]][length(encampments)] = FALSE                                 #turns off All Tents (final line)
buttons[[1]] = step                                                         #assigns all on camps to first button

#### Create Layout for Plot ####

fig2 <- fig2 %>% layout(title = "SLC Encampments",                          #adds title
                      showlegend = T,                                       #turns on legend
                      updatemenus =list(list(active = -1,                   #adds buttons
                                             type = 'buttons',
                                             buttons = buttons)),
                      yaxis = list(title = "Tents"),                        #labels y-axis
                      xaxis = list(title = "Date",                          
                                   type = 'catagory',                       #labels x-axis by dates 
                                   categoryarray = dates,                   #use dates to order x-axis
                                   categoryorder = 'array',                  
                                   rangeslider = list(type = 'catagory')))  #addes range slider              


#### Output Plot and Save as HTML ####

fig2

htmlwidgets::saveWidget(as_widget(fig2), 
                        paste0(mydata[[length(temp)]]$Labels[[1]],"_plot.html")) #save map as html file

#### Testing ####

colorscale <- as.data.frame(c(0,1))
colorscale2 <- as.data.frame(c('rgb(0,255,255)', '#7FFFD4'))
colorscale <- cbind(colorscale, colorscale2)
steps <- list()                                                   #empty list to hold slider details
fig <- plot_ly()                                                  #new empty figure for plotly

for (i in x) {                                                    #loops through each geoJSON & CSV to create maps, and sliders 
  
  
  camp <- c(paste0("Camp: ", mydata[[i]]$Camp))
  tents <- c(paste0(" Tents: ", mydata[[i]]$Tents))                #counts tents for hover text
  hover <- array(c(camp,tents), 
                 dim = c(length(mydata[[i]]$Shape_Area),2))        #tents, area, and density are put into an array for hover text 
  
  
  
  fig <- fig %>% add_trace(                                        #add polygons/data to map in multiple layers
    type = "choroplethmapbox",
    geojson = mygeo[[i]],                                          #area info 
    locations = mydata[[i]]$OBJECTID,                              #links geoJSON and CSV data
    z = 1,                               #CSV data used for choropleth
    colorscale = colorscale,
    featureidkey = "properties.OBJECTID",
    showscale = F,
    marker=list(line=list(                                         #removes boarders from polys
      width=1, color = "Black")),
    hovertext = hover,                                             #sets hover text to created array
    hoverinfo = "text"                                             #only show array dada (no location values)
  )
  
  step <- list(args = list('visible', rep(FALSE, length(mygeo))),  #creates a list for step values set to FALSE for each map
               method = 'restyle', 
               label = print(mydata[[i]]$Labels[[1]]))      #labels each map with its associated date
  
  step$args[[2]][i] = TRUE                                         #turns on the single map selected, set to true, all other maps left invisible
  steps[[i]] = step                                                #adds on/off list to steps list 
} 

#### Create Layout for Map ####

fig <- fig %>% layout(                                            #sets look of map, title, sliders, etc...
  mapbox = list(
    style = "white-bg",                                   #map style set
    center = list(lon = -111.898, lat = 40.755),                  #center corrdinates of map set
    zoom = 13.5,
    layers = list(list(
      opacity = .5,
      below = 'traces',
      sourcetype = "raster",
      source = list("https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer/tile/{z}/{y}/{x}")))),                                                 #zoom of map on cords set
  margin = list( 
    t = 65
  ),
  title = list(
    text = "Salt Lake City Encampments",                          #map title is set and stylized
    font = list(
      size = 28,
      color = "#33303f"
    ),
    yanchor = "top",
    pad = list(
      t = 10
    )
  ), 
  sliders = list(list(active = 0,                               #turns sliders on
                      bgcolor = "#838289",                       #stylize sliders
                      bordercolor = "#33303f",                   #stylize sliders
                      activebgcolor = "#292929",                 #stylize sliders
                      pad = list(                                #stylize sliders
                        b = 10
                      ),
                      currentvalue = list(                       #use current value feature
                        prefix = "Date: ",                       #call it date and pull label (ie date)
                        offset = 20,
                        font = list(
                          size = 15,
                          color = "#33303f"
                        )
                      ), 
                      font = list(
                        color = "#33303f"
                      ),
                      steps = steps,                             #use steps data 
                      transition = list(
                        duration = 0
                      ))),
  clickmode = "select"
)

#### Set Mapbox Token, Output Map, Save as HTML ####
fig <- fig %>%                                                  #key for html mapbox pages
  config(mapboxAccessToken = Sys.getenv("pk.eyJ1IjoiZ2thc3NpZGF5IiwiYSI6ImNrbGJqOHVnbDJ3Y3AzMnFtMWpxMTJnZTAifQ.gpfgCnyTuXAsEutgOWY0mg"))   

fig                                                             #view map in RStudio
