library(sf)
library(plotly)
library(raster)
library(rjson)
library(RColorBrewer)
library(lubridate)
library(stringr)
library(dplyr)
library(viridis)

#### Read in SHP, convert to MULTISTRING, extract geometry ####

temp = list.files(path = "SHP/",pattern="*.shp")                  #list of all SHP files

x <- seq(1, length(temp),1)                                       #sequence as long as number of CSVs, used for loop functions

shapes <- list()                                                  #empty list to fill

for (i in x){
  thing <- read_sf(paste0("SHP/",temp[[i]]))                      #reads in each CSV from temp list
  shapes[[i]] <- thing                                            #adds data to data list
}
                                      
for (i in x){
  shapes[[i]] <- st_cast(shapes[[i]], "MULTILINESTRING")          #converts polygons to multilinestrings
  
}

#### Read in CSV for Dates ####
temp = list.files(path = "CSV/",pattern="*.csv")                  #list of all CSV files
 
csv <- list()                                                 #empty list to fill
alldates <- list()

for (i in x){
  thing <- read.csv(file = paste0("CSV/",temp[[i]]))              #reads in each CSV from temp list
  csv[[i]] <- thing                                           #adds data to dates list
}

sf <- stamp("Jan 13", order = "Obd")                              #create date stamp for labeling

for (i in x){
  csv[[i]]$Labels <- csv[[i]]$Date                        #create new labal column & fill with dates
  
  csv[[i]]$Labels <- sf(mdy(csv[[i]]$Labels))             #updates label dates with stamp dates
  
  alldates[[i]] <- csv[[i]]$Labels[1]
}

for (i in x){
  
  csv[[i]]$geometry <- csv[[i]]$geometry
  
}

encampments <- c()                                                #empty list to fill

for (i in x){                                                     #cylces through each date
  
  y <- seq(1, length(csv[[i]]$Camp),1)                         #gets number of camps for the date
  
  for (j in y){                                                   #runs through each camp for the date
    
    if (csv[[i]]$Camp[j] %in% encampments) {                   #if the camp is already in encampments skip
      
      
    } else {
      
      encampments <- append(encampments, csv[[i]]$Camp[j])     #add camp to encampments if it isn't already
      
    }
    
  }
  
}


#dates <- c()                                                      #empty list to fill

#for (i in x){                                                     #cyles through each file
  
#  y <- seq(1, length(csv[[i]]$Date),1)                         #list of date entries for file
  
#  for (j in y){                                                   #cylces through date entries
    
#    if (csv[[i]]$Date[j] %in% dates) {                         #if the date is already in dates skip
      
#      dates <- dates
      
#    } else {                                                      #add date to dates if it isn't on the list
#      
#      dates <- append(dates,csv[[i]]$Date[j])
      
#    }
    
#  }
  
#}

counts <- list()                                          #empty list to fill with camp/tent dataframes

for (i in x){                                             #loops through each CSV
  
  c <- c(csv[[i]]$Camp)                               #creates list of camps
  t <- c(csv[[i]]$Tents)                              #creates list of tent counts
  
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
colnames(encampments.df) <- alldates                                #name columns from dates
encampments.df <- t(encampments.df)                              #transpose df, turns it into a matrix
encampments.df <- as.data.frame(encampments.df)                  #convert back to final df

pal <- rainbow(length(encampments))
large.tent <- max(encampments.df, na.rm = T)
pal2 <- magma(large.tent)

encampments.df <- cbind(encampments.df, "Total Tents" = rowSums(encampments.df, na.rm = T)) #add total tents count summed row




#### Loop to create Geo Ref Dataframe #### 

encampments.df.geo <- encampments.df
encampments.df.geo <- encampments.df.geo[,-ncol(encampments.df)]

for ( i in seq(1, nrow(encampments.df.geo),1)){
  
  for ( j in seq(1, ncol(encampments.df.geo),1)){
    
    if (is.na(encampments.df[alldates[[i]],encampments[[j]]])){
      
    } else {
      
      encampments.df.geo[i,j] <- paste0("shapes[[",i,"]]$geometry[[",match(encampments[[j]], csv[[i]]$Camp),"]][[1]]")
    
    }
       
  }
  
}

summary(encampments.df.geo[1,1])

#### Dates 3D Map ####
               
fig2 <- plot_ly()

for ( i in seq(1, nrow(encampments.df.geo),1)){
  
  date <- as.list(encampments.df.geo[i,])
  
  for (j in seq(1, ncol(encampments.df.geo),1)){
    
      first.camp <- which(!is.na(encampments.df.geo[,j])) %>%
        min(.)
      
      camp <- as.data.frame(eval(parse(text = encampments.df.geo[i,j])))
      
      group <- paste(encampments[[j]])
      
      camp.name <- c(paste0("Camp: ", encampments[[j]], "<br>"))                      #name camp 
      date <- c(paste0("Date: ", alldates[[i]]))   #name date 
      hover <- c(paste0(camp.name,date))
      
      if (!is.na(encampments.df[i,j])){
      
      colors <- as.data.frame(rep(encampments.df[i,j], nrow(camp)))
      camp <- cbind(camp,colors)
      
      fig2 <- fig2 %>%add_trace(date, x = camp$V1, 
                              y = camp$V2, 
                              z = alldates[[i]],                         
                              type = 'scatter3d',
                              mode = 'lines', 
                              line = list(width = 7,
                                          color = camp[,4],
                                          cmin = 2,
                                          cmax = large.tent,
                                          colorscale = 'Electric',
                                          coloraxis = 'coloraxis'),     
                              showlegend = F,                      #show legend (for camp group)
                              visible = T,                         #make visible
                              hovertext = hover,                  #sets hover text to created array
                              hoverinfo = "text",
                              opacity = .8,                       #80% opacity
                              name = paste0(group),                           #names it the encampment
                              legendgroup = group)
      
      } else {
        
        fig2 <- fig2 %>%add_trace(date, x = camp$V1, 
                                y = camp$V2, 
                                z = alldates[[i]],                         
                                type = 'scatter3d',
                                mode = 'lines', 
                                line = list(width = 7,
                                            color = "red"),     
                                showlegend = F,                      #show legend (for camp group)
                                visible = T,                         #make visible
                                hovertext = hover,                  #sets hover text to created array
                                hoverinfo = "text",
                                opacity = .8,                       #80% opacity
                                name = paste0(group),                           #names it the encampment
                                legendgroup = group)
        
      }
      
      
      
     
        
          }
}

steps <- list()
rep <- rep(FALSE, nrow(encampments.df.geo) * ncol(encampments.df.geo))
final.end <- length(rep)
last.camp <- 1

for ( i in seq(1, nrow(encampments.df.geo),1)){
  
  step <- list(args = list(list(visible = rep), list(showlegend = rep)), 
               method = 'update', 
               label = print(alldates[[i]]))      #labels each map with its associated date
  
  last.camp2 <- which(!is.na(encampments.df.geo[i,])) %>%
    max(.)
  
  if (last.camp < last.camp2){
    
    last.camp <- last.camp2

  } 
  
  leg.start <- ((i - 1) * ncol(encampments.df.geo)) + 1
  leg.end <- leg.start + (last.camp - 1)
  
  end <- ncol(encampments.df.geo) * i
  step$args[[1]]$visible[1 : end] = TRUE
  step$args[[1]]$showlegend[1 : final.end] = FALSE
  step$args[[1]]$showlegend[leg.start : leg.end] = TRUE
  step$args[[2]]$showlegend[leg.start : leg.end] = TRUE
  steps[[i]] = step
}

buttons <- list()

for ( i in seq(1, nrow(encampments.df.geo),1)){

  button <- list(args = list(list(visible = rep), list(showlegend = rep)), method = 'update', label = print(alldates[[i]]))      #labels each map with its associated date
  
  end <- ncol(encampments.df.geo) * i
  start <- (ncol(encampments.df.geo) * (i - 1)) + 1
                                            #turns on the single map selected, set to true, all other maps left invisible
  button$args[[1]]$visible[start : end] = TRUE
  button$args[[1]]$showlegend[start : end] = TRUE
  button$args[[2]]$showlegend[start : end] = TRUE

  buttons[[i]] = button
}


xrange <- list(-12461000, -12453000)
yrange <- list(4974000, 4979000)
zrange <- list(0, length(alldates))

fig2 <- fig2 %>% layout(
  scene = list(
    camera = list(
      eye = list(
        x = 0,
        y = -1.25,
        z = .5
      )
    ),
    bgcolor = 'rgb(220,220,220)',
    aspectmode = 'manual',         #sets aspect ratio
    aspectratio = list(
      x= 1,
      y =.76,
      z = .1
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
    itemclick = "toggleothers",
    itemdoubleclick = 'toggle',
    title = list(
      text = "Camps"
    ),
    bgcolor = 'rgb(220,220,220)',
    bordercolor = 'rgb(0,0,0)',
    borderwidth = 1,
    xanchor = 'right',
    x = -0.02
  ),
  title = list(
    text = paste0("SLC Homeless Encampments: ", alldates[[1]], " - ", alldates[[length(alldates)]])
  ),
  sliders = list(list(active = length(alldates) - 1,                               #turns sliders on
                      bgcolor = "#838289",                       #stylize sliders
                      bordercolor = "#33303f",                   #stylize sliders
                      activebgcolor = "#292929",                 #stylize sliders
                      pad = list(                                #stylize sliders
                        b = 10
                      ),
                      currentvalue = list( 
                        prefix = "Feb 05 - ",
                        visible = T,                     #call it date and pull label (ie date)
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
                      )),
                 list(active = length(alldates) - 1,
                      bgcolor = "#838289",                       #stylize sliders
                      bordercolor = "#33303f",                   #stylize sliders
                      activebgcolor = "#292929",                 #stylize sliders
                      pad = list(                                #stylize sliders
                        b = 10
                      ),
                      currentvalue = list(  
                        prefix = "Date: ",
                        visible = T,                   #use current value feature
                        offset = 20,
                        font = list(
                          size = 15,
                          color = "#33303f"
                        )
                      ), 
                      font = list(
                        color = "#33303f"
                      ),
                      steps = buttons,                             #use steps data 
                      transition = list(
                        duration = 0
                      ),
                      yanchor = "top",
                      y = -.2)
                 ),
              coloraxis = list(
                cmax = large.tent,
                cmin = 2,
                colorscale = 'Electric'
              ),
  margin = list(r = 20)) 


fig


#### Camps 3D Map ####

fig <- plot_ly()

for ( i in seq(1, nrow(encampments.df.geo),1)){
  
  for (j in seq(1, ncol(encampments.df.geo),1)){
    
    first.camp <- which(!is.na(encampments.df.geo[,j])) %>%
      min(.)
    
    group <- paste(encampments[[j]])
    
    camp.name <- c(paste0("Camp: ", encampments[[j]], "<br>"))                      #name camp 
    tents <- c(paste0("Tents: ", encampments.df[i,j]))   #name date 
    hover <- c(paste0(camp.name,tents))
    
    if (is.na(encampments.df.geo[i,j])){
      
      camp <- as.data.frame(eval(parse(text = encampments.df.geo[first.camp,j])))
      colors <- as.data.frame(rep(encampments.df[i,j], nrow(camp)))
      camp <- cbind(camp,colors)
        
      fig <- fig %>%add_trace(date, x = camp$V1, 
                              y = camp$V2, 
                              z = alldates[[i]],                         
                              type = 'scatter3d',
                              mode = 'lines', 
                              line = list(width = 7,
                                          color = camp[,4],
                                          cmin = 2,
                                          cmax = large.tent,
                                          colorscale = 'Electric',
                                          coloraxis = 'coloraxis'),              
                              showlegend = F,                      #show legend (for camp group)
                              visible = F,                         #make visible
                              hovertext = hover,                  #sets hover text to created array
                              hoverinfo = "text",
                              opacity = .8,                       #80% opacity
                              name = paste0(encampments[[j]]),                           #names it the encampment
                              legendgroup = group)
      
      } else if ( i == first.camp ){
        
        camp <- as.data.frame(eval(parse(text = encampments.df.geo[i,j])))
        colors <- as.data.frame(rep(encampments.df[i,j], nrow(camp)))
        camp <- cbind(camp,colors)
        
        
        fig <- fig %>%add_trace(date, x = camp$V1, 
                                y = camp$V2, 
                                z = alldates[[i]],                         
                                type = 'scatter3d',
                                mode = 'lines', 
                                line = list(width = 7,
                                            color = camp[,4],
                                            cmin = 2,
                                            cmax = large.tent,
                                            colorscale = 'Electric',
                                            coloraxis = 'coloraxis'),          
                                showlegend = T,                      #show legend (for camp group)
                                visible = T,                         #make visible
                                hovertext = hover,                  #sets hover text to created array
                                hoverinfo = "text",
                                opacity = .8,                       #80% opacity
                                name = paste0(encampments[[j]]),                           #names it the encampment
                                legendgroup = group)
        
      } else {
        
        camp <- as.data.frame(eval(parse(text = encampments.df.geo[i,j])))
        
        fig <- fig %>%add_trace(date, x = camp$V1, 
                                y = camp$V2, 
                                z = alldates[[i]],                         
                                type = 'scatter3d',
                                mode = 'lines', 
                                line = list(width = 7,
                                            color = pal2[encampments.df[i,j]]),              
                                showlegend = F,                      #show legend (for camp group)
                                visible = T,                         #make visible
                                hovertext = hover,                  #sets hover text to created array
                                hoverinfo = "text",
                                opacity = .8,                       #80% opacity
                                name = paste0(encampments[[j]]),                           #names it the encampment
                                legendgroup = group)
        
      }
    
  }
  
}

buttons <- list()

for ( i in seq(1, ncol(encampments.df.geo),1)){
  button <- list(args = list('visible', rep(FALSE, nrow(encampments.df.geo) * ncol(encampments.df.geo))),  #creates a list for step values set to FALSE for each map
                 method = 'restyle', 
                 label = print(encampments[[i]]))      #labels each map with its associated date
  
  last.camp <- which(!is.na(encampments.df.geo[,i])) %>%
    max(.)
  
  onlist <- list()
  
  for ( j in seq(1, last.camp, 1)) {
    
    if (is.na(encampments.df.geo[j,i])){
      
      on <- NA
      
    } else {
      
      on <- i + (ncol(encampments.df.geo) * (j - 1))
      
    }
  
  onlist[[j]] <- on
  
  }
  
  for ( k in seq(1, last.camp, 1)){
  
    if (is.na(onlist[[k]])){
      
      button$args[[2]][onlist[[k]]] = FALSE
      
    } else {
      
      button$args[[2]][onlist[[k]]] = TRUE
      
    }
  
  
  }
  
  buttons[[i]] = button
  
}

fig <- fig %>% layout(scene = list(
  bgcolor = 'rgb(220,220,220)',
  aspectmode = 'data',
  xaxis = list(
    autorange = T,
    showgrid = FALSE,
    title = list(
      text = "East - West"
    ),
    showticklabels = FALSE,
    showbackground = TRUE,
    backgroundcolor = 'rgb(240,240,240)'
  ),
  yaxis = list(
    autorange = T,
    showgrid = FALSE,
    title = list(
      text = "North- South"
    ),
    showticklabels = FALSE,
    showbackground = TRUE,
    backgroundcolor = 'rgb(240,240,240)'
  ),
  zaxis = list(
    autorange = T,
    showgrid = FALSE,
    title = list(
      text = ""
    ),
    type = 'category',
    categoryorder = "array",
    categoryarray = alldates,
    showticklabels = TRUE,
    showbackground = TRUE,
    backgroundcolor = 'rgb(240,240,240)'
  )),
  legend = list(
    itemsizeing = "constant",
    itemwidth = 60,
    itemclick = "toggleothers",
    title = list(
      text = "Camps"
    ),
    bgcolor = 'rgb(220,220,220)',
    bordercolor = 'rgb(0,0,0)',
    borderwidth = 1
  ),
  title = list(
    text = "SLC Homeless Encampments"
  ),
  coloraxis = list(
    cmax = large.tent,
    cmin = 2,
    colorscale = 'Electric',
    colorbar = list(
      xanchor = 'right',
      x = -0.02
    )
  ))
 
fig
