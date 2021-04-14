library(sf)
library(plotly)
library(raster)
library(rjson)
library(RColorBrewer)
library(lubridate)

#### Read in SHP, convert to MULTISTRING, extract geometry ####

temp = list.files(path = "SHP/",pattern="*.shp")                  #list of all SHP files

x <- seq(1, length(temp),1)                                       #sequence as long as number of CSVs, used for loop functions

mydata <- list()                                                  #empty list to fill

for (i in x){
  thing <- read_sf(paste0("SHP/",temp[[i]]))                      #reads in each CSV from temp list
  mydata[[i]] <- thing                                            #adds data to data list
}
                                      
for (i in x){
  mydata[[i]] <- st_cast(mydata[[i]], "MULTILINESTRING")          #converts polygons to multilinestrings
  
}
st_crs(mydata[[12]])
#### Read in CSV for Dates ####
temp = list.files(path = "CSV/",pattern="*.csv")                  #list of all CSV files
 
mydates <- list()                                                 #empty list to fill
alldates <- list()

for (i in x){
  thing <- read.csv(file = paste0("CSV/",temp[[i]]))              #reads in each CSV from temp list
  mydates[[i]] <- thing                                           #adds data to dates list
}

sf <- stamp("Jan 13", order = "Obd")                              #create date stamp for labeling

for (i in x){
  mydates[[i]]$Labels <- mydates[[i]]$Date                        #create new labal column & fill with dates
  
  mydates[[i]]$Labels <- sf(mdy(mydates[[i]]$Labels))             #updates label dates with stamp dates
  
  alldates[[i]] <- mydates[[i]]$Labels[1]
}

for (i in x){
  
  mydates[[i]]$geometry <- mydata[[i]]$geometry
  
}

encampments <- c()                                                #empty list to fill

for (i in x){                                                     #cylces through each date
  
  y <- seq(1, length(mydates[[i]]$Camp),1)                         #gets number of camps for the date
  
  for (j in y){                                                   #runs through each camp for the date
    
    if (mydates[[i]]$Camp[j] %in% encampments) {                   #if the camp is already in encampments skip
      
      
    } else {
      
      encampments <- append(encampments, mydates[[i]]$Camp[j])     #add camp to encampments if it isn't already
      
    }
    
  }
  
}

encampments <- sort(encampments)                                  #alphabetizes encampments list

pal <- rainbow(length(encampments))

ground <- png::readPNG("ground_layer.png")


#### Plot Camps in Space for each Date ####

fig <- plot_ly()                                                #empty plotly object
steps <- list()                                                 #empty list for buttons
date.count <- data.frame()

for (i in x){                                                   #for each date
  
  length <- seq(1, length(mydates[[i]]$geometry), 1)             #sequence equal to number of camps
  group <- paste0("group",i)                                    #sets group name for date
  date.count.new <- cbind(i,length(mydates[[i]]$geometry))
  date.count <- rbind(date.count, date.count.new)
           
  
  for (j in length){                                            #for each camp on this date
    
    data <- data.frame(                                         #data frame of xy coords, z set to index of observation
      x = mydates[[i]]$geometry[[j]][[1]][,1],
      y = mydates[[i]]$geometry[[j]][[1]][,2],
      z = mydates[[i]]$Labels[1]
    )
    
    color <- match(c(mydates[[i]]$Camp[j]), encampments)
    
    camp.name <- c(paste0("Camp: ", mydates[[i]]$Camp[j], "<br>"))   #name camp from SHP for hover text
    date <- c(paste0("Date: ", mydates[[i]]$Labels[j]))         #name date from SHP for hover text
    hover <- c(paste0(camp.name,date))                               #camp and date are put into an array for hover text 
    
    if (j == 1){                                                #add first camp to plot for that date
  
    fig <- fig %>%add_trace(date, x = data$x, 
                            y = data$y, 
                            z = data$z,                         #height is the observation index
                            type = 'scatter3d',
                            mode = 'lines', 
                            line = list(color = pal[color],         #palette based on date
                                        width = 7),              #make visible
                            showlegend = T,                     #show legend (for date group)
                            hovertext = hover,                  #sets hover text to created array
                            hoverinfo = "text",
                            opacity = .8,                       #80% opacity
                            name = mydates[[i]]$Labels[j],      #names it the encampment
                            legendgroup = group)                #sets legend group to date
    }else{
     fig <- fig %>%add_trace(date, x = data$x,                  #adds additional camps and removes them from legend
                              y = data$y, 
                              z = data$z,
                              type = 'scatter3d',
                              mode = 'lines', 
                              line = list(color = pal[color],
                                          width = 7),
                              showlegend = F,                   #do not add camp to legend      
                              hovertext = hover,                                             
                              hoverinfo = "text",
                              opacity = .8,
                              name = mydates[[i]]$Labels[j],
                              legendgroup = group)
    }
  }
  
}

date.count[nrow(date.count),2]
date.count[2,2]

for (i in x){
  
  prev.row <- i-1
  previous.date <- date.count[prev.row,3]
  new.date <- date.count[i,2]  
  
  if (i == 1){
    
    date.count[i,3] <- date.count[i,2]
    
  } else {
    
    date.count[i,3] <- previous.date + new.date
    
  }
}


for (i in x ){

step <- list(args = list('visible', rep(FALSE, date.count[nrow(date.count),3])),  #creates a list for step values set to FALSE for each map
             method = 'restyle', 
             label = print(mydates[[i]]$Labels[[1]]))      #labels each map with its associated date


step$args[[2]][1:date.count[i,3]] = TRUE                                         #turns on the single map selected, set to true, all other maps left invisible
steps[[i]] = step 

}

#### 3D Layout ####
xrange <- list(-12461000, -12453000)
yrange <- list(4974000, 4979000)
zrange <- list(0, length(alldates))

fig <- fig %>% layout(scene = list(
                            bgcolor = 'rgb(220,220,220)',
                            aspectmode = 'manual',         #sets aspect ratio
                            aspectratio = list(
                              x= 1,
                              y =.76,
                              z = .05
                            ),
                            xaxis = list(
                              autorange = FALSE,
                              range = xrange,
                              showgrid = FALSE,
                              title = list(
                                text = "East - West"
                              ),
                              showticklabels = FALSE,
                              showbackground = TRUE,
                              backgroundcolor = 'rgb(240,240,240)'
                            ),
                            yaxis = list(
                              autorange = FALSE,
                              range = yrange,
                              showgrid = FALSE,
                              title = list(
                                text = "North- South"
                              ),
                              showticklabels = FALSE,
                              showbackground = TRUE,
                              backgroundcolor = 'rgb(240,240,240)'
                            ),
                            zaxis = list(
                              autorange = FALSE,
                              range = zrange,
                              showgrid = FALSE,
                              title = list(
                                text = "Time"
                              ),
                              type = 'category',
                              categoryorder = "array",
                              categoryarray = alldates,
                              showticklabels = FALSE,
                              showbackground = TRUE,
                              backgroundcolor = 'rgb(240,240,240)'
                            )),
                      legend = list(
                        itemsizeing = "constant",
                        itemwidth = 60,
                        traceorder = "reversed",
                        itemclick = "toggleothers",
                        title = list(
                          text = "Dates"
                        ),
                        bgcolor = 'rgb(220,220,220)',
                        bordercolor = 'rgb(0,0,0)',
                        borderwidth = 1
                      ),
                      title = list(
                        text = "SLC Homeless Encampments"
                      ),
                      sliders = list(list(active = length(alldates)-1,                               #turns sliders on
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
                                          )))) 
                    
                        
fig

htmlwidgets::saveWidget(as_widget(fig), 
                        paste0(mydates[[length(temp)]]$Labels[[1]],"_3D_by_date.html")) #save map as html file


#### Testing ####

fig2 <- plot_ly()                                                #empty plotly object
length <- seq(1, length(mydates), 1)                          #sequence equal to number of dates

for (i in encampments){                                         #for each camp
  
  color <- match(c(i), encampments)
  
  group <- paste0(i)                                            #sets group name for camp
  
  camp <- data.frame()                                          #empty dataframe
  
  for (j in length){                                            #for each date
    
    if (i %in% mydates[[j]]$Camp){                              #if that camp is on that date
    
    new.camp <- cbind(j, match(c(i), mydates[[j]]$Camp))        #create a df row of date and camp geometry index
    
    camp <- rbind(camp,new.camp)                                #bind row to camp df
    
    }else{
      
    }
  }
  
  
          
  for (k in seq(1, nrow(camp), 1)){                             #for every row in the camp df
    
    current.date <- camp[k,1]                                   #retrieve the date index
    current.camp <- camp[k,2]                                   #retrieve the camp geometry index
  
    data <- data.frame(                                         
            x = mydates[[current.date]]$geometry[[current.camp]][[1]][,1],  #coords from camp geometry on that date
            y = mydates[[current.date]]$geometry[[current.camp]][[1]][,2],
            z = mydates[[current.date]]$Labels[1])                          #z value equal to date
    
            camp.name <- c(paste0("Camp: ", i, "<br>"))                      #name camp 
            date <- c(paste0("Date: ", mydates[[current.date]]$Labels[1]))   #name date 
            hover <- c(paste0(camp.name,date))                               #camp and date are put into an array for hover text 
    
    if (k == 1){           #for first mapped camp
    
            fig2 <- fig2 %>%add_trace(date, x = data$x, 
                                      y = data$y, 
                                      z = data$z,                         
                                      type = 'scatter3d',
                                      mode = 'lines', 
                                      line = list(width = 7,
                                                  color = pal[color]),              
                                      showlegend = T,                      #show legend (for camp group)
                                      visible = T,                         #make visible
                                      hovertext = hover,                  #sets hover text to created array
                                      hoverinfo = "text",
                                      opacity = .8,                       #80% opacity
                                      name = i,                           #names it the encampment
                                      legendgroup = group)                #sets legend group to encampment

    }else{
    
      fig2 <- fig2 %>%add_trace(date, x = data$x, 
                                y = data$y, 
                                z = data$z,                         
                                type = 'scatter3d',
                                mode = 'lines', 
                                line = list(width = 7,
                                            color = pal[color]),              
                                showlegend = F,                     #do not att additional camp maps to legend
                                visible = T,      
                                hovertext = hover,                  #sets hover text to created array
                                hoverinfo = "text",
                                opacity = .8,                       #80% opacity
                                name = i,                           #names it the encampment
                                legendgroup = group)                #sets legend group to date
      
    }
}
}

fig2 <- fig2 %>% layout(scene = 
                        list(
                          aspectmode = 'data',         #sets aspect ratio
                          aspectratio = list(
                            x= 1,
                            y =.8,
                            z = .1
                          ),
                          xaxis = list(
                            title = list(
                              text = "East - West"
                            ),
                            showaxeslabels = FALSE,
                            showbackground = TRUE,
                            backgroundcolor = 'rgb(220,220,220)'
                          ),
                          yaxis = list(
                            title = list(
                              text = "North- South"
                            ),
                            showaxeslabels = FALSE,
                            showbackground = TRUE,
                            backgroundcolor = 'rgb(220,220,220)'
                          ),
                          zaxis = list(
                            title = list(
                              text = "Dates"
                            ),
                            type = 'category',
                            categoryorder = "array",
                            categoryarray = alldates,
                            showaxeslabels = FALSE
                          )),
                        images = list(
                          visible = TRUE,
                          layer = "below",
                          source = raster2uri(as.raster(ground)),
                          x = -12461000, y = 4974000, z = 'Feb 05',
                          xanchor = 'left', yanchor = 'bottom',
                          sizing = 'stretch'
                        ),
                        legend = list(
                          itemsizeing = "constant",
                          itemwidth = 60,
                          traceorder = "reversed",
                          itemclick = "toggleothers",
                          title = list(
                            text = "Camps"
                          ),
                          bgcolor = 'rgb(220,220,220)',
                          bordercolor = 'rgb(0,0,0)',
                          borderwidth = 1
                        ))               

fig2


htmlwidgets::saveWidget(as_widget(fig2), 
                        paste0(mydates[[length(temp)]]$Labels[[1]],"_3D_by_camp.html")) #save map as html file


rainbow(12)

camp <- data.frame()
new.camp <- cbind(3,7)
camp <- rbind(camp, new.camp)
new.camp <- cbind(3,7)
camp <- rbind(camp, new.camp)
camp[3,1]
nrow(camp)
pal[1]
