library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(sf)
library(plotly)
library(raster)
library(rjson)
library(lubridate)
library(RColorBrewer)
library(viridis)
library(colorspace)


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
alldates <- NULL
alldatelabs <- list()

for (i in x){
  thing <- read.csv(file = paste0("CSV/",temp[[i]]), stringsAsFactors = F)              #reads in each CSV from temp list
  
  csv[[i]] <- thing                                           #adds data to dates list
}

sf <- stamp("Jan 13", order = "Obd")                              #create date stamp for labeling

for (i in x){
  csv[[i]]$Labels <- csv[[i]]$Date                        #create new labal column & fill with dates
  
  csv[[i]]$Labels <- sf(mdy(csv[[i]]$Labels))             #updates label dates with stamp dates
  
  alldatelabs[[i]] <- csv[[i]]$Labels[1]
  alldates[[i]] <- csv[[i]]$Date[1]
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
      
      encampments <- c(encampments, csv[[i]]$Camp[j])     #add camp to encampments if it isn't already
      
    }
    
  }
  
}

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
colnames(encampments.df) <- alldatelabs                                #name columns from dates
encampments.df <- t(encampments.df)                              #transpose df, turns it into a matrix
encampments.df <- as.data.frame(encampments.df)                  #convert back to final df

pal <- rainbow(length(encampments))
large.tent <- max(encampments.df, na.rm = T)
pal2 <- magma(large.tent)

encampments.df <- cbind(encampments.df, "Total Tents" = rowSums(encampments.df, na.rm = T)) #add total tents count summed row

encampments.df.dates <- cbind(encampments.df, "Dates" = as.data.frame(alldates))

#### Loop to create Geo Ref Dataframe #### 

encampments.df.geo <- encampments.df.dates
encampments.df.geo <- encampments.df.geo[,-(ncol(encampments.df.dates)-1)]

for ( i in seq(1, nrow(encampments.df.geo),1)){
  
  for ( j in seq(1, (ncol(encampments.df)-1),1)){
    
    if (is.na(encampments.df[alldatelabs[[i]],encampments[[j]]])){
      
    } else {
      
      encampments.df.geo[i,j] <- paste0("shapes[[",i,"]]$geometry[[",match(encampments[[j]], csv[[i]]$Camp),"]][[1]]")
      
    }
    
  }
  
}

#### Dates 3D Map ####

dates.3D <- plot_ly(height = 700)

for ( i in seq(1, nrow(encampments.df.geo),1)){
  
  for (j in seq(1, (ncol(encampments.df.geo)-1),1)){
    
    first.camp <- which(!is.na(encampments.df.geo[,j])) %>%
      min(.)
    
    camp <- as.data.frame(eval(parse(text = encampments.df.geo[i,j])))
    
    group <- paste(encampments[[j]])
    
    camp.name <- c(paste0("Camp: ", encampments[[j]], "<br>"))  
    date <- c(paste0("Date: ", alldatelabs[[i]], "<br>"))                    
    tents <- c(paste0("Tents: ", encampments.df[i,j]))   
    hover <- c(paste0(camp.name, date, tents))
    
    if (!is.na(encampments.df[i,j])){
      
      colors <- as.data.frame(rep(encampments.df[i,j], nrow(camp)))
      camp <- cbind(camp,colors)
      
      dates.3D <- dates.3D %>%add_trace(date, x = camp$V1, 
                                y = camp$V2, 
                                z = alldates[[i]],                         
                                type = 'scatter3d',
                                mode = 'lines', 
                                scene = 'scene2',
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
      
      dates.3D <- dates.3D %>%add_trace(date, x = camp$V1, 
                                y = camp$V2, 
                                z = alldates[[i]],                         
                                type = 'scatter3d',
                                mode = 'lines', 
                                scene = 'scene2',
                                line = list(width = 7,
                                            color = "#0000FF00"),     
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
rep <- rep(FALSE, nrow(encampments.df.geo) * (ncol(encampments.df.geo)-1))
final.end <- length(rep)
last.camp <- 1
camp.list <- list()
camp.leg <- list()
camp.leg2 <- list()
new.leg <- list()




for ( i in seq(1, nrow(encampments.df.geo),1)){
  
  if (i == 1){
    
  step <- list(args = list(list(visible = rep), list(showlegend = F)), 
               method = 'update', 
               label = print(alldatelabs[[i]]))      #labels each map with its associated date
  
  camp.leg[[i]] <- which(!is.na(encampments.df.geo[i,]))
  
  step$args[[1]]$showlegend[1 : final.end] = FALSE
  
  for (l in camp.leg[[i]]){
    
    step$args[[1]]$showlegend[camp.leg[[i]][l]] <- F
    step$args[[2]]$showlegend[camp.leg[[i]][l]] <- F
    
  }
  
  } else {
    
    step <- list(args = step$args, 
                 method = 'update', 
                 label = print(alldatelabs[[i]]))      #labels each map with its associated date
    
    camp.leg2[[i]] <- which(!is.na(encampments.df.geo[i,]))
    new.camps <- setdiff(camp.leg2[[i]], camp.leg[[i-1]])
    camp.leg[[i]] <- append(camp.leg[[i-1]], new.camps)
    
    for (k in new.camps){
      
      camp.leg[[i]][k] <- ((i-1) * (ncol(encampments.df.geo)-1)) + k

    }
    
    step$args[[1]]$showlegend[1 : final.end] = FALSE
    
    for (p in camp.leg[[i]]){
      
      step$args[[1]]$showlegend[p] <- F
      step$args[[2]]$showlegend[p] <- F
      
    }
    
  }
  
  camp.list[[i]] <- which(!is.na(encampments.df.geo[i,])) 
  
  for (j in camp.list[[i]]){
    
    camp.list[[i]][j] <- ((i-1) * (ncol(encampments.df.geo)-1)) + j
    
    step$args[[1]]$visible[camp.list[[i]][j]] <- TRUE
    
  }
   
  
  
  steps[[i]] = step
  
}

xrange <- list(-12461000, -12453000)
yrange <- list(4974000, 4979000)
zrange <- list(0, length(alldates))

dates.3D <- dates.3D %>% layout(
  scene2 = list(
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
    borderwidth = 1
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
                      ))),
  coloraxis = list(
    cmax = large.tent,
    cmin = 2,
    colorscale = 'Electric',
    colorbar = list(
      xanchor = 'right',
      x = -0.02,
      title = list(
        text = "Tents"
      ),
      bgcolor = 'rgb(220,220,220)',
      bordercolor = 'rgb(0,0,0)',
      borderwidth = 1
  )),
  paper_bgcolor = 'rgb(250,250,250)'
  ) 


dates.3D


#### Camps 3D Map ####

camps.3D <- plot_ly(height = 700)

for ( i in seq(1, nrow(encampments.df.geo),1)){
  
  for (j in seq(1, (ncol(encampments.df.geo)-1),1)){
    
    first.camp <- which(!is.na(encampments.df.geo[,j])) %>%
      min(.)
    
    group <- paste(encampments[[j]])
    
    camp.name <- c(paste0("Camp: ", encampments[[j]], "<br>"))  
    date <- c(paste0("Date: ", alldatelabs[[i]], "<br>"))                    
    tents <- c(paste0("Tents: ", encampments.df[i,j]))   
    hover <- c(paste0(camp.name, date, tents))
    
    if (is.na(encampments.df.geo[i,j])){
      
      camp <- as.data.frame(eval(parse(text = encampments.df.geo[first.camp,j])))
      colors <- as.data.frame(rep(encampments.df[i,j], nrow(camp)))
      camp <- cbind(camp,colors)
      
      camps.3D <- camps.3D %>%add_trace(date, x = camp$V1, 
                              y = camp$V2, 
                              z = alldatelabs[[i]],                         
                              type = 'scatter3d',
                              mode = 'lines', 
                              scene = 'scene1',
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
      
      
      camps.3D <- camps.3D %>%add_trace(date, x = camp$V1, 
                              y = camp$V2, 
                              z = alldatelabs[[i]],                         
                              type = 'scatter3d',
                              mode = 'lines', 
                              scene = 'scene1',
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
      colors <- as.data.frame(rep(encampments.df[i,j], nrow(camp)))
      camp <- cbind(camp,colors)
      
      camps.3D <- camps.3D %>%add_trace(date, x = camp$V1, 
                              y = camp$V2, 
                              z = alldatelabs[[i]],                         
                              type = 'scatter3d',
                              mode = 'lines', 
                              scene = 'scene1',
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
                              name = paste0(encampments[[j]]),                           #names it the encampment
                              legendgroup = group)
      
    }
    
  }
  
}

buttons <- list()

for ( i in seq(1, (ncol(encampments.df.geo)-1),1)){
  button <- list(args = list('visible', rep(FALSE, nrow(encampments.df.geo) * (ncol(encampments.df.geo)-1))),  #creates a list for step values set to FALSE for each map
                 method = 'restyle', 
                 label = print(encampments[[i]]))      #labels each map with its associated date
  
  last.camp <- which(!is.na(encampments.df.geo[,i])) %>%
    max(.)
  
  onlist <- list()
  
  for ( j in seq(1, last.camp, 1)) {
    
    if (is.na(encampments.df.geo[j,i])){
      
      on <- NA
      
    } else {
      
      on <- i + ((ncol(encampments.df.geo)-1) * (j - 1))
      
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

camps.3D <- camps.3D %>% layout(
  scene1 = list(
  camera = list(
    eye = list(
      x = 0,
      y = -2.25,
      z = 1.5
    )
  ),
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
    itemclick = "toggle",
    title = list(
      text = "Camps"
    ),
    bgcolor = 'rgb(220,220,220)',
    bordercolor = 'rgb(0,0,0)',
    borderwidth = 1
  ),
  coloraxis = list(
    cmax = large.tent,
    cmin = 2,
    colorscale = 'Electric',
    colorbar = list(
      xanchor = 'right',
      x = -0.02,
      title = list(
        text = "Tents"
      ),
      bgcolor = 'rgb(220,220,220)',
      bordercolor = 'rgb(0,0,0)',
      borderwidth = 1
    )),
  paper_bgcolor = 'rgb(250,250,250)')

camps.3D

#### Camp Plot ####

abatements <- read.csv("Abatements.csv", stringsAsFactors = F)

plot.camps <- plot_ly(height = 700)                                                           #empty plot to fil

for (i in seq(1, ncol(encampments.df),1)){
  
  if ( i == ncol(encampments.df)){                                           #if last column (ie Total Tents)
    
    plot.camps <- plot.camps %>% add_trace(encampments.df,                                  
                                           x = as.Date(encampments.df.dates[,ncol(encampments.df.dates)], format = "%m/%d/%Y"),
                                           y = encampments.df[,i], 
                                           visible = T,
                                           type = 'scatter', 
                                           mode = 'lines+markers',
                                           name = "Total Tents",
                                           line = list(color = 'rgba(0,0,0)'),     #specify black color
                                           marker = list(size = 8,                      #larger marker
                                                         color = 'rgba(0,0,0)'))   #specify black color
    
  }else{
    
    plot.camps <- plot.camps %>% add_trace(encampments.df,                              #other encampments get generic plotly color
                                           x = as.Date(encampments.df.dates[,ncol(encampments.df.dates)], format = "%m/%d/%Y"),
                                           y = encampments.df[,i], 
                                           visible = T,
                                           type = 'scatter', 
                                           mode = 'lines+markers',
                                           name = encampments[i],
                                           marker = list(size = 7))                          
  }
  
}

### Create Abatement Lines & Annotations 

line <- list(
  type = "line",
  line = list(color = "red"),
  xref = "x",
  yref = "y"
)

lines <- list()
for (i in seq(1, nrow(abatements),1)) {
  line[["y0"]] <- 0
  line[["y1"]] <- max(encampments.df[,1:(ncol(encampments.df)-1)], na.rm = T)
  line[c("x0", "x1")] <- abatements[i,1]
  lines <- c(lines, list(line))
}

line.height <- as.data.frame(rep(max(encampments.df[,1:(ncol(encampments.df)-1)], na.rm = T), nrow(abatements)))

abatements <- cbind(abatements, line.height)

annotations <- list()
for ( i in seq_len(nrow(abatements))){
  
  annotation <- list( x = abatements[i,1],
                      y = abatements[i,3],
                      text = abatements[i,2],
                      showarrow = T)
  
  annotations[[i]] <- annotation
  
}



abate.button <- rep(TRUE, ncol(encampments.df))
abate.button[ncol(encampments.df)+1] <- FALSE
button <- list(list(args = list(list(), list(annotations = NULL, shapes = NULL)), 
                    args2 = list(list(), list(annotations = annotations, shapes = lines)),
                    method = 'update', 
                    label = "Abatements"))    #labels each map with its associated date
  


### Create Layout for Plot ###

plot.camps <- plot.camps %>% layout(title = "Encampment Stats",                          #adds title
                                    showlegend = T,
                                    paper_bgcolor = 'rgb(250,250,250)',
                                    plot_bgcolor = 'rgb(240,240,240)',
                                    legend = list(
                                      itemsizeing = "constant",
                                      itemwidth = 60,
                                      bgcolor = 'rgb(220,220,220)',
                                      bordercolor = 'rgb(0,0,0)',
                                      borderwidth = 1,
                                      yanchor = "top",
                                      y = .92
                                    ),
                                    yaxis = list(title = "Tents"),                        #labels y-axis
                                    xaxis = list(
                                      rangeselector = list(
                                        buttons = list(
                                          list(
                                            count = 3,
                                            label = "3 mo",
                                            step = "month",
                                            stepmode = "backward"),
                                          list(
                                            count = 6,
                                            label = "6 mo",
                                            step = "month",
                                            stepmode = "backward"),
                                          list(
                                            count = 1,
                                            label = "1 yr",
                                            step = "year",
                                            stepmode = "backward"),
                                          list(
                                            count = 1,
                                            label = "YTD",
                                            step = "year",
                                            stepmode = "todate"),
                                          list(step = "all"))),
                                    rangeslider = list(type = "date")),
                                    updatemenus = list(
                                      list(
                                        type = 'buttons',
                                        buttons = button,
                                        xanchor = 'left',
                                        x = 1.02
                                      )
                                    ))  #addes range slider              


plot.camps

####---- Generic Data Processing ----####

#### Read in CSV and geoJSON data into lists ####

temp = list.files(path = "JSON/", pattern = "*.geojson")          #list of all geoJSON files

json = list()                                                    #empy list to fill

for (i in x){
  thing <- fromJSON(file = paste0("JSON/",temp[[i]]))             #reads in each geoJSON file from temp list
  json[[i]] <- thing                                             #adds data to geo list
}


#### Loop for creating maps and associated slider steps ####

colorscale <- as.data.frame(c(0,1))
colorscale2 <- as.data.frame(c('rgb(235, 219, 47)', 'rgb(235, 219, 47)'))
colorscale <- cbind(colorscale, colorscale2)

steps <- list()                                                   #empty list to hold slider details
map.camp <- plot_ly(height = 700)                                                  #new empty figure for plotly

for (i in x) {                                                    #loops through each geoJSON & CSV to create maps, and sliders 
  
  
  camp <- c(paste0("Camp: ", csv[[i]]$Camp,""))
  tents <- c(paste0("<br>","Tents: ", csv[[i]]$Tents))  
  date <- c(paste0("<br>", "Date: ", alldatelabs[[i]]))#counts tents for hover text
  hover <- c(paste0(camp, date, tents))    #tents, area, and density are put into an array for hover text 
  
  if ( i == length(x)){
  
  map.camp <- map.camp %>% add_trace(                                        #add polygons/data to map in multiple layers
    type = "choroplethmapbox",
    geojson = json[[i]],                                          #area info 
    locations = csv[[i]]$OBJECTID,                              #links geoJSON and CSV data
    z = 1,                               #CSV data used for choropleth
    colorscale = colorscale,
    featureidkey = "properties.OBJECTID",
    showscale = F,
    marker=list(line=list(                                         #removes boarders from polys
      width=1, color = 'rgb(173, 0, 113)')),
    hovertext = hover,                                             #sets hover text to created array
    hoverinfo = "text",
    visible = T#only show array dada (no location values)
  )
  
  } else {
    
    map.camp <- map.camp %>% add_trace(                                        #add polygons/data to map in multiple layers
      type = "choroplethmapbox",
      geojson = json[[i]],                                          #area info 
      locations = csv[[i]]$OBJECTID,                              #links geoJSON and CSV data
      z = 1,                               #CSV data used for choropleth
      colorscale = colorscale,
      featureidkey = "properties.OBJECTID",
      showscale = F,
      marker=list(line=list(                                         #removes boarders from polys
        width=1, color = 'rgb(173, 0, 113)')),
      hovertext = hover,                                             #sets hover text to created array
      hoverinfo = "text",
      visible = F#only show array dada (no location values)
    )
    
    
    
  }
  step <- list(args = list('visible', rep(FALSE, length(json))),  #creates a list for step values set to FALSE for each map
               method = 'restyle', 
               label = print(csv[[i]]$Labels[[1]]))      #labels each map with its associated date
  
  step$args[[2]][i] = TRUE                                         #turns on the single map selected, set to true, all other maps left invisible
  steps[[i]] = step                                                #adds on/off list to steps list 
} 

#### Create Layout for Map ####

map.camp <- map.camp %>% layout(                                            #sets look of map, title, sliders, etc...
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
    r = 75
  ), 
  sliders = list(list(active = length(steps)-1,                               #turns sliders on
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
  clickmode = "select",
  paper_bgcolor = 'rgb(250,250,250)'
  )


#### Set Mapbox Token, Output Map, Save as HTML ####
map.camp <- map.camp %>%                                                  #key for html mapbox pages
  config(mapboxAccessToken = Sys.getenv("pk.eyJ1IjoiZ2thc3NpZGF5IiwiYSI6ImNrbGJqOHVnbDJ3Y3AzMnFtMWpxMTJnZTAifQ.gpfgCnyTuXAsEutgOWY0mg"))   

map.camp                                                             #view map in RStudio

#### Dash Text ####

explanation_text <- "

## The City

Salt Lake City, like most urban areas in the United States, is home to an assortment of homeless encampments.
These spaces are seen by the city and state as hinderances to growing downtown rejuvenation projects as they are 
considered dangerous, unsightly, and criminal. One way in which the city tracks the activities of the homeless is
through the app *SLC Mobile*, which offers users the ability to request cleanup services.

> SLC Mobile is a real time, free, civic engagement platform that allows the public to conveniently communicate 
non-emergency civic issues directly to city government from their smart devices. Download this app to report community 
issues or problems; you can even submit photos.

These cleanup service calls are aggregated by the city and presented on their [Homeless Services Dashboard](https://www.slc.gov/hand/homeless-services-dashboard/)
as a [map of homelessness throughout Salt Lake City](https://slcgov.maps.arcgis.com/apps/webappviewer/index.html?id=41d87239080d4963aa2c64ea60994c21). Of course
this map does not represent the true geography of homelessness in SLC. Rather it reveals the areas in which the
homeless are most likely to be seen as unsightly and in need of removal. 

## The Maps

The maps and statistics presented below attempt to illuminate the geography of homelessness in Salt Lake City through in situ data collection. 
Every three days the city is canvased by car and camps are documented by video. Using these recordings the spatial extent and number 
of tents present on the streets can be mapped in fine spatial and temporal detail. Camps here are defined as any public 
space with two or more tents visible from the road. The outlined camps represent the spatial extent between these tents. 

While more intensive than the city's mapping, this methodology is not a perfect record of SLC Homeless Encampments. 
Limited by funding and time, these maps do not record any encampent not visible from the road. This means that large 
encampents, sometimes over 50 tents, which exist is green spaces along the Jordan River or up mountain trails are excluded. 
Relying on the knowledge of the homeless and local activists to locate camps, there are certainly blind spots in the canvasing
. Many encampments are found only once they have grown large enough to 'draw attention.' 

## The Purpose

Acknowledging the limitiations of these maps, they should not be read as an exhaustive catalogue of homeless encampments in Salt Lake City.
Rather, they reveal the ways in which camps grow, shrink, adapt, and respond to stresses overtime. Since the beginning of this project the 
Salt Lake City Health Department and Police Department have kept up their ongoing raids, sweeps, and abatements of these camps. Sometimes
warned, sometimes not, the inhabitants of these communities must pick up their belongings, when allowed, and move along. The relatively
constant number of tents on the streets throughout time clearly demonstrates the futility of these activities to 'eliminate homelessness.'
Critically examining how the homeless maneuver around these sweeps, adapting to new locations and rebuilding on old sites, provides insight
into these communities.

In contrast to the images of criminality and danger presented by the city, these encampents are communities of resiliance.
***
"
map.title_text <- "Encampments Choropleth Map"

map.exp_text <- "
Use this map to investigate how camps move, grow, and adapt overtime. The date slider below the map can be used to
interactively explore the extent of camps for a given date, or moved quickly to see how a specific location has changed.
Hovering over an encampment gives its name, the specific date being viewed, and how many tents were documented at that time.
"
plot.title_text <- "Encampment Statistics"

plot.exp_text <- "
This plot visualizes how the total number of tents in each camp, and city wide, has changed over time. The legend on the right
can be used to turn off specific camps, or double clicked to isolate a single camp. Removing the total camps plot line gives a 
more detailed view of the camps. The range slider along the bottom can be used to investigate specific time periods, or 
select the buttons in the upper left for time periods of general interest. Hovering over a line marker will indicate the date of 
the recording, the number of tents present, and the name of the encampment. Also make sure to select the 'Abatements' button in
the upper right. This will reveal when sweeps accured and to which encampments.
"

dates.title_text <- "City Wide Encampments Time-Cube"

dates.exp_text <- "
 This representation allows you to see how camps have persisted 
throughout time more clearly. The slider along the bottom of the time-cube allows you to stack camp maps on top of each
other to percieve their shifts and removals overtime. Each camp is color coded based on the number of tents it contains. Hovering
over a camp will give you the camp's name, date of mapping, and number of tents present. Unlike the choropleth map, the time-cube allows
us to better see the sporadic spread of smaller camps shifting around the larger ersistent encampments. 
"

camps.title_text <- "Individual Encampment Time-Cubes"

camps.exp_text <- "
This is a time-cube which represents the geographic extent of camps in the x and y dimensions and the time of recordings in the
z dimension. The older maps are at the bottom of the cube, newer maps at the top. Double-clicking an encampment from the legend on the right
will provide a detailed view of that camp overtime. These camp specific time-cubes allow us to investigate how an encampment 
conforms to its available geography, spreading to incorporate new residents, or shrinking after a sweep. Unlike the city wide time-cube,
these intimate views allow for nuanced analysis of camp specific changes. Again hoving over a specific slice will gie the camp
name, date, and tent count. The tent counts are also visually encoded with a color scale.
"
#### Dash ####

pageTitle <- htmlH1(
  'Mapping SLC Encampments', 
  style = list(
    textAlign = 'center'
  )
)

pageSubTitle <- htmlH2(
  paste0(csv[[1]][["Date"]][1], " - ", csv[[length(csv)]][["Date"]][1]),
  style = list(
    textAlign = 'center'
  )
)

graph.3D.dates <- dccGraph(
  id = '3D.Dates',
  figure = dates.3D
)

graph.3D.camps <- dccGraph(
  id = '3D.Camps',
  figure = camps.3D
)

graph.plot <- dccGraph(
  id = 'plot',
  figure = plot.camps
)

graph.map <- dccGraph(
  id = 'map',
  figure = map.camp
)

explanation <- htmlDiv(
  list(
    dccMarkdown(children = explanation_text)
  )
)

map.title <- htmlH3(
  map.title_text,
  style = list(
    textAlign = 'center'
  )
)
map.exp <- htmlDiv(
  list(
    dccMarkdown(children = map.exp_text)
  )
)

plot.title <- htmlH3(
  plot.title_text,
  style = list(
    textAlign = 'center'
  )
)

plot.exp <- htmlDiv(
  list(
    dccMarkdown(children = plot.exp_text)
  )
)

dates.title <- htmlH3(
  dates.title_text,
  style = list(
    textAlign = 'center'
  )
)

dates.exp <- htmlDiv(
  list(
    dccMarkdown(children = dates.exp_text)
  )
)

camps.title <- htmlH3(
  camps.title_text,
  style = list(
    textAlign = 'center'
  )
)

camps.exp <- htmlDiv(
  list(
    dccMarkdown(children = camps.exp_text)
  )
)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

app$layout(
  htmlDiv(
    list(
      pageTitle,
      pageSubTitle,
      explanation,
      map.title,
      map.exp,
      graph.map,
      plot.title,
      plot.exp,
      graph.plot,
      dates.title,
      dates.exp,
      graph.3D.dates,
      camps.title,
      camps.exp,
      graph.3D.camps
      )
    )
  )
app$run_server(host='0.0.0.0', dev_tools_hot_reload=FALSE)