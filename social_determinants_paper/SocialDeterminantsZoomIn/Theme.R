#' Gives the theme categorical colors in order for the given number
#'  of clusters
#'
#' @param n_clusters 
#'
#' @return a vector of n_clusters hex values 
#'
#' @examples
#' scale_fill_manual(values = theme.categorical.colors(3))
#' 
#' scale_fill_manual(values = theme.categorical.colors(max(mort.cluster.ord()$cluster)))
#' 
#' @author Ross DeVito
#' @export
theme.categorical.colors <- function(n_clusters) {
  
  if (n_clusters == 3) {
    return(c("#fed976", "#fd8d3c", "#bd0026"))
  }
  if (n_clusters == 4) {
    return(c("#fed976", "#fd8d3c", "#fc4e2a", "#bd0026"))
  }
  if (n_clusters == 5) {
    return(c("#fed976", "#fd8d3c", "#fc4e2a", 
             "#e31a1c", "#bd0026"))
  }
  if (n_clusters == 6) {
    return(c("#fed976", "#fd8d3c", "#fc4e2a", 
             "#e31a1c", "#bd0026", "#800026"))
  }
}

theme.cat.accent.color <- function() {
  return ("#0571b0")
}

#' Gives the theme categorical colors with accent color appended
#'
#' @param n_clusters 
#'
#' @return a vector of (n_clusters + 1) hex values where the first n_clusters
#'          values are the values that would be returned by 
#'          theme.categorical.colors(n_clusters) and the last value is 
#'          theme.cat.accent.color()
#'
#' @examples
#' scale_fill_manual(values = theme.categorical.colors(3))
#' 
#' scale_fill_manual(values = theme.categorical.colors(max(mort.cluster.ord()$cluster)))
#' 
#' @author Ross DeVito
#' @export
theme.categorical.colors.accent <- function(n_clusters) {
  return(append(theme.categorical.colors(n_clusters), theme.cat.accent.color()))
}


theme.text <- function() {
  theme(
    text = element_text(family = "Bookman"),
    title = element_text(color = "gray25"),
    plot.caption = element_text(
      color = "gray30",
      size = 7.5
    ),
    plot.subtitle = element_text(size = 9)
  )
}

theme.background <- function() {
  theme(
    plot.margin = unit(c(5, 10, 5, 10), units = "mm")
  )
}

base.geo <- function(data) {
  ggplot(data, aes(long, lat, group = group, fill = VAR_)) +
    geom_polygon(size = 0.3, color = "white", alpha = 0.9)
}

theme.line.mort <- function() {
  theme_minimal() + 
    theme.text() + 
    theme.background() +
    theme(
      axis.text.x = element_text(
        angle = 20
      ),
      panel.grid.major.x = element_blank() 
    )
}

color.line.cluster <- function(state.choice, n.clusters) {
  
  if (state.choice != "US"){
    scale_color_manual(
      name = "Cluster",
      #c("#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000")
      values = colorRampPalette(
        c("#fef0d9","#fdcc8a","#fc8d59", "#000000")
        
      )(n.clusters),
      guide = guide_legend(reverse = T)
    )
    
  } else {
    scale_color_manual(
      name = "Cluster",
      #c("#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000")
      values = colorRampPalette(
        c("#fef0d9","#fdcc8a","#fc8d59","#e34a33")
        
      )(n.clusters),
      guide = guide_legend(reverse = T)
    )
  }
  
}

labs.line.mort <- function(state.choice, death.cause) {
  labs(
    title = paste(
      state.choice,
      paste("Death of", death.cause, "Trend"),
      sep = ' - '
    ),
    caption = "Data Source: CDCWONDER Multi-Cause of Death",
    x = "Period", y = "Mortality Rate (# per 100k)"
  )
}

theme.geo.cluster <- function() {
  theme_void() +
    theme(
      text = element_text(color = "#22211d"), 
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = NA, color = NA),
      
      plot.title = element_text(
        size = 16, hjust = 0.01, 
        margin = ggplot2::margin(b = -0.1, t = 0.4, l = 2, unit = "cm")
      ),
      
      legend.position = c(0.3, 0.02),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
    ) + 
    theme.text() + 
    theme.background()
}

color.geo.cluster <- function(n) {
  scale_fill_manual(
    name = "Cluster",
    #c("#ED3F3C0F","#00ACAC0F","#203F9A0F","#F8A51A0F")
    #c("#ABB7B7FF","#BDC3C7FF","#D2D7D3FF","#ECF0F1FF")
    #c("#E9E9E9FF","#D4D4D4FF","#9A9A9AFF","#707070FF")
    #c("#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000")
    #c("#c6c6c6","#9e9e9e","#787878","#565656")
    values = colorRampPalette(
      c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")
    )(n),
    guide = guide_legend(
      keyheight = unit(2, units = "mm"), 
      keywidth = unit(8, units = "mm"), 
      label.position = "bottom", 
      title.position = "top", 
      nrow = 1
    )
  )
}



labs.geo.cluster <- function(state.choice) {
  labs(
    title = paste(state.choice, "Trend Cluster Geo-Distribution"),
    x = NULL, y = NULL
  )
}

# geoTitle <- function(state.choice, death.cause) {
#   return (tags$div(title="This map visualizes the counties 
# within the selected state and 
# divides them into three clusters 
# of counties that have similar 
# rates of premature deaths. The 
# Low cluster is the grouping of 
# counties that have relatively lower 
# deaths compared to the rest of the 
# state. The Medium cluster is the 
# grouping of counties that have average 
# premature deaths compared to the 
# rest of the state. The High cluster 
# is the grouping of counties that 
# have relatively higher deaths compared 
# to the rest of the state.",
#     HTML(paste0(state.choice, "Trend Cluster<br/>Geo Distribution", sep = " ")),icon("info-circle")))
# }

geoTitle <- function(state.choice, death.cause) {
  return (tags$div())
}

# draw.geo.cluster: Used in app.R to draw state and US maps
draw.geo.cluster <- function(state.choice, death.cause, mort.cluster, n_clusters) {
  
  dataset <- geo.map.fetch(state.choice, mort.cluster) %>% 
    dplyr::rename(VAR_ = cluster)
  lat_long <- getLatLong(state.choice, dataset)
  shapes <- readRDS(paste("../shape_files/", state.choice, ".Rds", sep = ""))
  
  colors <- rev(theme.categorical.colors(n_clusters))
  labels <- c("Low", "Medium", "High")
  
  if (state.choice == "DE") {
    colors <- rev(colors)
    labels <- c("Kent", "New Castle", "Sussex")
  } else if (state.choice == "HI") {
    colors <- rev(colors)
    labels <- c("Kalawao", "Honolulu" , "Maui", "Hawaii", "Kauai")
  } else if (state.choice == "RI") {
    colors <- rev(colors)
    labels <- c("Bristol", "Washington", "Newport", "Kent", "Providence")
  } else {
    colors <- rev(colors)
  }
  
  dataset <- dataset %>% dplyr::distinct(county_name, county_fips, VAR_)
  shapes.data <- as.data.frame(shapes)
  shapes.data$county_fips <- paste(as.data.frame(shapes)$STATEFP, as.data.frame(shapes)$COUNTYFP, sep = '')
  dataset <- left_join(shapes.data, dataset, by = c("county_fips" = "county_fips"))
  
  if (state.choice != "US"){
    return (leaflet(shapes, 
                    options = leafletOptions()) %>%
              fitBounds(lat1 = lat_long[1], 
                        lng1 = lat_long[2], 
                        lat2 = lat_long[3], 
                        lng2 = lat_long[4]) %>%
              addPolygons(stroke = TRUE, 
                          smoothFactor = 0.1, 
                          fillOpacity = 1,
                          weight = 1,
                          color = "white",
                          opacity = 1,
                          fillColor = colors[as.numeric(dataset$VAR_)],
                          label = dataset$county_name,
                          layerId = dataset$county_name) %>%
              addControl(geoTitle(state.choice, death.cause),
                         position = "topleft",
                         className="map-title") %>%
              addLegend("topright",
                        colors = rev(colors),
                        labels = rev(labels),
                        title = "Risk\nGroups:",
                        opacity = 1) %>%
              setMapWidgetStyle(style = list(background = "transparent"))
    )
  }else{
    return (leaflet(shapes, 
                    options = leafletOptions()) %>%
              fitBounds(lat1 = lat_long[1], 
                        lng1 = lat_long[2], 
                        lat2 = lat_long[3], 
                        lng2 = lat_long[4]) %>%
              addPolygons(stroke = TRUE, 
                          smoothFactor = 0.1, 
                          fillOpacity = 1,
                          weight = 0,
                          color = "white",
                          opacity = 1,
                          fillColor = colors[as.numeric(dataset$VAR_)],
                          label = dataset$county_name) %>%
              addControl(geoTitle(state.choice, death.cause),
                         position = "topleft",
                         className="map-title") %>%
              setMapWidgetStyle(style = list(background = "transparent"))
    )
  }
  
}

# draw.sd.geo: Used in app.R to overlay SD rates on state and US maps
draw.sd.geo <- function(sd.rates, state.choice, death.cause) {
  
  dataset <- geo.map.fetch(state.choice, sd.rates) %>% 
    dplyr::rename(VAR_ = rate)
  lat_long <- getLatLong(state.choice, dataset)
  shapes <- readRDS(paste("../shape_files/", state.choice, ".Rds", sep = ""))
  
  # colors <- theme.categorical.colors(n_clusters)
  # labels <- c("Low", "Medium", "High")
  
  dataset <- dataset %>% dplyr::distinct(county_name, county_fips, VAR_)
  dataset$county_fips <- substr(dataset$county_fips, 3, 5)
  dataset <- left_join(as.data.frame(shapes)['COUNTYFP'], dataset, by = c("COUNTYFP" = "county_fips"))
  
  if (state.choice != "US"){
    return (leaflet(shapes, 
                    options = leafletOptions()) %>%
              fitBounds(lat1 = lat_long[1], 
                        lng1 = lat_long[2], 
                        lat2 = lat_long[3], 
                        lng2 = lat_long[4]) %>%
              addPolygons(stroke = TRUE, 
                          smoothFactor = 0.1, 
                          fillOpacity = 1,
                          weight = 1,
                          color = "white",
                          opacity = 1,
                          fillColor = colors[as.numeric(dataset$VAR_)],
                          label = dataset$county_name,
                          layerId = dataset$county_name) %>%
              addControl(geoTitle(state.choice, death.cause), 
                         position = "topleft", 
                         className="map-title") %>%
              setMapWidgetStyle(style = list(background = "transparent"))
    )
  }else{
    return (leaflet(shapes, 
                    options = leafletOptions()) %>%
              fitBounds(lat1 = lat_long[1], 
                        lng1 = lat_long[2], 
                        lat2 = lat_long[3], 
                        lng2 = lat_long[4]) %>%
              addPolygons(stroke = TRUE, 
                          smoothFactor = 0.1, 
                          fillOpacity = 1,
                          weight = 0,
                          color = "white",
                          opacity = 1,
                          fillColor = colors[as.numeric(dataset$VAR_)],
                          label = dataset$county_name) %>%
              addControl(geoTitle(state.choice, death.cause), 
                         position = "topleft", 
                         className="map-title") %>%
              setMapWidgetStyle(style = list(background = "transparent"))
    )
  }
}


theme.geo.mort <- function() {
  theme_void() +
    theme(
      text = element_text(color = "#22211d"), 
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = NA, color = NA),
      
      plot.title = element_text(
        size = 16, hjust = 0.01, 
        margin = ggplot2::margin(b = -0.1, t = 0.4, l = 2, unit = "cm")
      ),
      
      legend.position = c(0.32, 0.02),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
    ) + 
    theme.text() + 
    theme.background()
}

color.geo.mort <- function(death.cause) {
  
  bins <- bin.geo.mort(death.cause)
  
  #c("#fff7ec","#fee8c8","#fdd49e","#fdbb84","#fc8d59","#ef6548","#d7301f","#b30000","#7f0000")
  # original
  #c("#faebeb", "#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000", "#450000")
  vals <- colorRampPalette(
    c("#faebeb", "#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000", "#450000")
  )(length(bins))
  
  names(vals) <- cut(seq(0, 600, 5), bins) %>% 
    unique()
  
  scale_fill_manual(
    name = "Rate", 
    values = vals,
    guide = guide_legend(
      keyheight = unit(2, units = "mm"), 
      keywidth = unit(4, units = "mm"), 
      label.position = "bottom", 
      title.position = "top", 
      nrow = 1
    )
  )
}

labs.geo.mort <- function(state.choice, period.choice, death.cause) {
  labs(
    title = paste(
      paste(state.choice, paste("- Death of", death.cause)), 
      paste("Rate", period.choice, sep = '\n')
    ),
    # subtitle = period.choice,
    # caption = "Data Sorce:\n\t1.CDCWONDER Multi-Cause of Death",
    x = NULL, y = NULL
  )
}

bin.geo.mort <- function(death.cause) {
  bin <- list(
    "Despair" = c(-1, 5, 10, 15, 25, 50, 75, 100, Inf),
    "Cardiovascular" = c(seq(0, 315, 45), Inf),
    "Assault" = c(seq(0, 35, 5), Inf),
    "Cancer" = c(seq(0, 315, 45), Inf),
    "All Cause" = c(seq(0, 560, 80), Inf)
  )
  return(bin[[death.cause]])
}

draw.geo.mort <- function(state.choice, period.choice, mort.data, death.cause) {
  if (state.choice != "US"){
    
    geo.map.fetch(state.choice, mort.data) %>% 
      dplyr::rename(VAR_ = death_rate) %>%
      base.geo() + 
      labs.geo.mort(state.choice, period.choice, death.cause) + 
      color.geo.mort(death.cause) + 
      theme.geo.mort() + 
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)
    
    
  }else{
    geo.map.fetch("US", mort.data) %>% 
      dplyr::rename(VAR_ = death_rate) %>%
      #ggplot(aes(long, lat, group = group, fill = VAR_, color = VAR_, text = county_name)) +
      
      ggplot(VAR_, aes(x = long, y = lat, group = group)) + 
      geom_polygon(aes(fill = group, color = group))+
      
      #geom_polygon(size = 0, color = "white",alpha = 0.9) +
      #base.geo() + 
      labs.geo.mort(state.choice, period.choice, death.cause) + 
      color.geo.mort(death.cause) +
      theme.geo.mort() + 
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)
  }
  
}

# get_title <- function(state.choice, death.cause, period) {
#   return (tags$div(title="This map is a visualization of each
# of the individual counties within
# the selected state. Lighter colors
# indicate lower premature deaths, 
# darker colors are higher. The key 
# for each of the colors appears beneath 
# the map of the state. The desired time 
# range can be selected beneath the key.",
#     HTML(paste(state.choice, " - Death of", death.cause, "Rate", period)),icon("info-circle")))
# }

get_title <- function(state.choice, death.cause, period) {
  return (tags$div())
}

# get_sd_title <- function(state.choice, sd.choice, period) {
#   return (tags$div(title="This map is a visualization of each
# of the individual counties within
# the selected state. Lighter colors
# indicate lower values for the selected
# determinant, darker colors are higher. 
# Values for 2015-2017 are shown",
#                    HTML(paste(state.choice, " - ", sd.choice, "Rate", period)),icon("info-circle")))
# }

get_sd_title <- function(state.choice, sd.choice, period) {
  return (tags$div())
}

getLatLong <- function(state.choice, dataset) {
  
  max.long <- max(dataset$long)
  max.lat <- max(dataset$lat)
  min.long <- min(dataset$long)
  min.lat <- min(dataset$lat)
  
  switch(state.choice,
         'AK'= {
           min.lat = 50.0
           max.lat = 70.0
           min.long = -164.0
           max.long = -120.0
         },
         "HI"= {
           min.lat = 18.46
           max.lat = 22.46
           min.long = -160.505
           max.long = -154.505
         },
         "MN"= {
           min.lat = 43.0
           max.lat = 49.9
           min.long = -98.3655146
           max.long = -89.3655146
         },
         "ID"= {
           min.lat = 40.4945756
           max.lat = 50.4945756
           min.long = -110.1424303
           max.long = -118.1424303
         },
         "LA" = {
           min.lat = 28.9733766
           max.lat = 33.9733766
           min.long = -96.4299097
           max.long = -87.4299097
         },
         "MI" = {
           min.lat = 40.9435598
           max.lat = 48.9435598
           min.long = -90.4158049
           max.long = -82.4158049
         },
         "NV" = {
           min.lat = 34.502032
           max.lat = 42.502032
           min.long = -120.0230604
           max.long = -114.0230604
         }, 
         "OH" = {
           min.lat = 37.1903624
           max.lat = 43.1903624
           min.long = -86.6692525
           max.long = -79.6692525
         },
         "WV" = {
           min.lat = 36.9201705
           max.lat = 40.9201705
           min.long = -83.1816905
           max.long = -77.1816905
         })
  
  if (state.choice == 'AR' | state.choice == 'MN') {
    padding = 0.5
  } else {
    padding = 0.05
  }
  
  return(c(min.lat - padding, min.long - padding, max.lat + padding, max.long + padding))
}

geo.label <- function(death.cause) {
  switch(death.cause,
         "Despair" = return(
           c("[0,5]", "[5,10]", "[10,15]", "[15,25]", "[25,50]", "[50,75]", "[75,100]", "[100,Inf]")
         ),
         "Assault" = return(
           c("[0,5]", "[5,10]", "[10,15]", "[15,20]", "[20,25]", "[25,30]", "[30,35]", "[35,Inf]")
         ),
         "Cancer" = return(
           c("[0,45]", "[45,90]", "[90,135]", "[135,180]", "[180,225]", "[225,270]", "[270,315]", "[315,Inf]")
         ),
         "Cardiovascular" = return(
           c("[0,45]", "[45,90]", "[90,135]", "[135,180]", "[180,225]", "[225,270]", "[270,315]", "[315,Inf]")
         ),
         "All Cause" = return(
           c("[0,80]", "[80,160]", "[160,240]", "[240,320]", "[320,400]", "[400,480]", "[480,560]", "[560,Inf]")
         ))
}

geo.plot <- function(state.choice, death.cause, mort.data, period) {
  dataset <- geo.map.fetch(state.choice, mort.data) %>% 
    dplyr::rename(VAR_ = death_rate)
  lat_long <- getLatLong(state.choice, dataset)
  
  shapes <- readRDS(paste("../shape_files/", state.choice, ".Rds", sep = ""))
  
  # These only make sense for categorical!
  colors <- c("#faebeb", "#ffc4c4", "#ff8f8f", "#ff5454", "#ff1414", "#a80000", "#450000", "#000000")
  labels <- geo.label(death.cause) 
  
  dataset <- dataset %>% dplyr::distinct(county_name, county_fips, VAR_)
  shapes.data <- as.data.frame(shapes)
  shapes.data$county_fips <- paste(as.data.frame(shapes)$STATEFP, as.data.frame(shapes)$COUNTYFP, sep = '')
  dataset <- left_join(shapes.data, dataset, by = c("county_fips" = "county_fips"))

  return (leaflet(shapes, 
                    options = leafletOptions()) %>%
              fitBounds(lat1 = lat_long[1], 
                        lng1 = lat_long[2], 
                        lat2 = lat_long[3], 
                        lng2 = lat_long[4]) %>%
              addPolygons(stroke = TRUE, 
                          smoothFactor = 0.1, 
                          fillOpacity = 1,
                          weight = 1,
                          color = "white",
                          opacity = 1,
                          fillColor = colors[as.numeric(dataset$VAR_)],
                          label = dataset$county_name,
                          layerId = dataset$county_name) %>%
              addControl(get_title(state.choice, death.cause, period), 
                         position = "topleft", 
                         className="map-title") %>%
              addLegend("bottomleft",
                        colors = colors[8],
                        labels = labels[8],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[7],
                        labels = labels[7],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[6],
                        labels = labels[6],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[5],
                        labels = labels[5],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[4],
                        labels = labels[4],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[3],
                        labels = labels[3],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[2],
                        labels = labels[2],
                        title = "&nbsp;",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors = colors[1],
                        labels = labels[1],
                        title = "Mortality&nbsp;&nbsp;Rate&nbsp;&nbsp;per&nbsp;&nbsp;100,000:",
                        opacity = 1) %>% 
              setMapWidgetStyle(style = list(background = "transparent"))
    )
}

geo.us.plot <- function(death.cause, period) {
  if (death.cause == "Cardiovascular") {
    death.cause = "Cardio"
  }
  
  switch(period,
         "2000-2002" = {
           image_name = "1"
         },
         "2003-2005" = {
           image_name = "2"
         },
         "2006-2008" = {
           image_name = "3"
         },
         "2009-2011" = {
           image_name = "4"
         },
         "2012-2014" = {
           image_name = "5"
         },
         "2015-2017" = {
           image_name = "6"
         })
  return (
    leaflet() %>%
      fitBounds(lat1 = 21.68154, 
                lng1 = -125.46167, 
                lat2 = 49.43436, 
                lng2 = -66.89990) %>%
      setMapWidgetStyle(style = list(background = "transparent")) %>%
      htmlwidgets::onRender(paste("
        function(el, x) {
          console.log(this);
          var myMap = this;
          var imageUrl = 'National_image/", death.cause, "/", image_name, ".png';
          var imageBounds = [[21.68154, -125.46167], [49.43436, -66.89990]];
          
          L.imageOverlay(imageUrl, imageBounds).addTo(myMap);
        }
      ", sep = ''))
  )
}

geo.sd.plot <- function(state.choice, sd.choice, sd.data, period, legend_title="Rate") {
  #  browser()
  dataset <- geo.map.fetch(state.choice, sd.data) %>% 
    dplyr::rename(VAR_ = VAR) %>%
    dplyr::rename(county_name = county_name.y)
  lat_long <- getLatLong(state.choice, dataset)
  
  shapes <- readRDS(paste("../shape_files/", state.choice, ".Rds", sep = ""))
  
  dataset <- dataset %>% dplyr::distinct(county_name, county_fips, VAR_)
  shapes.data <- as.data.frame(shapes)
  
  shapes.data$county_fips <- paste(as.data.frame(shapes)$STATEFP, as.data.frame(shapes)$COUNTYFP, sep = '')
  dataset <- left_join(shapes.data, dataset, by = c("county_fips" = "county_fips"))
  
  pal <- colorBin(c("#7ccaf2", "#2a9df4", "#0277bd", "#03254c"), 
                  domain = dataset$VAR, bins = 3)
  
  epsg2163 <- leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "EPSG:2163",
    proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
    resolutions = 2^(16:7))
  
  if (state.choice != "US"){
    plot <- leaflet(shapes, 
                    options = leafletOptions()) %>%
      fitBounds(lat1 = lat_long[1], 
                lng1 = lat_long[2], 
                lat2 = lat_long[3], 
                lng2 = lat_long[4]) %>%
      addPolygons(stroke = TRUE, 
                  smoothFactor = 0.1, 
                  fillOpacity = 1,
                  weight = 1,
                  color = "white",
                  opacity = 1,
                  fillColor = ~pal(dataset$VAR),
                  label = dataset$county_name,
                  layerId = dataset$county_name) %>%
      addControl(get_sd_title(state.choice, sd.choice, period), 
                 position = "topleft", 
                 className="map-title") %>%
      addLegend("topright",
                pal = pal,
                values = ~dataset$VAR,
                title = legend_title,
                opacity = 1) %>% 
      setMapWidgetStyle(style = list(background = "transparent"))
    
    
    if (is.null(county_polygon)){
      return(plot)
    }
    return(
      plot %>% addPolylines(stroke = TRUE, 
                            weight = 2,
                            color="#000000",
                            data = county_polygon,
                            group="highlighted_polygon",
                            dashArray = "4 2 4")
    )
  }else{
    return (leaflet(shapes, 
                    options = leafletOptions(crs = epsg2163)) %>%
              fitBounds(lat1 = lat_long[1], 
                        lng1 = lat_long[2], 
                        lat2 = lat_long[3], 
                        lng2 = lat_long[4]) %>%
              addPolygons(stroke = TRUE, 
                          smoothFactor = 0.1, 
                          fillOpacity = 1,
                          weight = 0,
                          color = ~pal(dataset$VAR),
                          opacity = 1,
                          # fillColor = colors[as.numeric(dataset$VAR_)],
                          label = dataset$county_name) %>%
              addControl(get_sd_title(state.choice, sd.choice, period), 
                         position = "topleft", 
                         className="map-title") %>%
              setMapWidgetStyle(style = list(background = "transparent"))
            
    )
  }
  
  
}

kmean.us.plot <- function(death.cause) {
  return (
    leaflet() %>%
      fitBounds(lat1 = 21.68154, 
                lng1 = -125.46167, 
                lat2 = 49.43436, 
                lng2 = -66.89990) %>%
      setMapWidgetStyle(style = list(background = "transparent")) %>%
      htmlwidgets::onRender(paste("
          function(el, x) {
            var myMap = this;
            var imageUrl = 'National_geo_image/", death.cause, ".png';
            var imageBounds = [[21.68154, -125.46167], [49.43436, -66.89990]];
            
            L.imageOverlay(imageUrl, imageBounds).addTo(myMap);
          }
        ", sep = ''))
  )
}
