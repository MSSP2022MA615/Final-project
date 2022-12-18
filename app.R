library(shiny)
library(shinythemes)
library(mapsapi)
library(shiny)
library(mapsapi)
library(tidyverse)
library(DT)
library(plotly)
library(leaflet)
library(mapsapi)
library(kableExtra)
library(tidyr)
library(lubridate)
library(DT)
library(data.table)
library(RColorBrewer)

## load files for shiny

######## May 2022 no changes of service during this week
## load feed from 0514 to 0521
may<- read_csv("may.csv", show_col_types = FALSE)

# check non-numeric stop-id in May data
# x_test <- may$stop_id
#  may$stop_id[is.na(x_test)] 
# this helps me separate ferry and commuter info

# ferry in May
# boat_trips_may<- may %>% filter(route_desc == "Ferry") %>% distinct(trip_id,.keep_all = T)
# Boat-Lewis				
# Boat-Hingham				
# Boat-Charlestown				

# Other potential values:
# Boat-Logan				
# Boat-George				
# Boat-Rowes
# Boat-Long-South
# Boat-Hull	
# Boat-Long	

# Commuter rail in May
# CR_trips_may<- may %>% filter(route_desc == "Commuter Rail") %>% distinct(trip_id,.keep_all = T)


########### 
########### BUS
sl_routes<- read_csv("sl_routes.csv")

sl_routes_traveltimes<- read_csv("sl_routes_traveltimes.csv")

########### 
########### T
# load t stops & travel times 
red_travel_times<- read_csv("red_travel_times.csv", show_col_types = FALSE) 
orange_travel_times<- read_csv("orange_travel_times.csv", show_col_types = FALSE)
# blue_travel_times<- read_csv("blue_travel_times.csv", show_col_types = FALSE)
# greenb_travel_times<- read_csv("greenb_travel_times.csv", show_col_types = FALSE)
# greend_travel_times<- read_csv("greend_travel_times.csv", show_col_types = FALSE)
# greene_travel_times<- read_csv("greene_travel_times.csv", show_col_types = FALSE)
# mattpan_travel_times<- read_csv("mattpan_travel_times.csv", show_col_types = FALSE)

# ## red line density plot table
# red_to_north<- red_travel_times %>% filter(direction_id ==1)
# red_dept_dens_n<- with(red_to_north, tapply(travel_time_sec, INDEX = dept_name, density)) # now this is a nested list contains density travel times grouped by each stop
# 
# # Northbound
# red_to_north_df<- data.frame(
#   x = unlist(lapply(red_dept_dens_n, "[[", "x")),
#   y = unlist(lapply(red_dept_dens_n, "[[", "y")),
#   dept_name = rep(names(red_dept_dens_n), each = length(red_dept_dens_n[[1]]$x))
# )
# 
# # Southbound
# red_to_south <- red_travel_times %>% filter(direction_id == 0)
# red_dept_dens_s<- with(red_to_south, tapply(travel_time_sec, INDEX = dept_name, density))
# red_to_south_df<- data.frame(
#   x = unlist(lapply(red_dept_dens_s, "[[", "x")),
#   y = unlist(lapply(red_dept_dens_s, "[[", "y")),
#   dept_name = rep(names(red_dept_dens_s), each = length(red_dept_dens_s[[1]]$x))
# )

## Orange line density plot table

orange_to_north<- orange_travel_times %>% filter(direction_id ==1)
orange_dept_dens_n<- with(orange_to_north, tapply(travel_time_sec, INDEX = dept_name, density)) # now this is a nested list contains density travel times grouped by each stop

# use double bracket to unlist and store the associated density into a dataframe
orange_to_north_df<- data.frame(
  x = unlist(lapply(orange_dept_dens_n, "[[", "x")),
  y = unlist(lapply(orange_dept_dens_n, "[[", "y")),
  dept_name = rep(names(orange_dept_dens_n), each = length(orange_dept_dens_n[[1]]$x))
)


orange_to_south <- orange_travel_times %>% filter(direction_id == 0)
orange_dept_dens_s<- with(orange_to_south, tapply(travel_time_sec, INDEX = dept_name, density))
orange_to_south_df<- data.frame(
  x = unlist(lapply(orange_dept_dens_s, "[[", "x")),
  y = unlist(lapply(orange_dept_dens_s, "[[", "y")),
  dept_name = rep(names(orange_dept_dens_s), each = length(orange_dept_dens_s[[1]]$x))
)


###########-------------------
# now create shiny 

# set up api_key first
# api_key = "AIzaSyAZZEueevQyJrIRJUregAEbUC775LEfrxg"
# 
# #### Distance Matrix
# # example: obtain a matrix of driving distance and duration between 2 locations
# 
# locations = c("South Station", "Kenmore")
# doc = mp_matrix(
#   origins = locations,
#   destinations = locations,
#   # transit_mode =
#   # arrival_time = The desired time of arrival for transit directions, as POSIXct
#   # departure_time =
#   # avoid = "ferries" or "indoor"
#   key = api_key,
#   quiet = TRUE
# )

##D # Public transport times
##D doc = mp_matrix(
##D   origins = locations,
##D   destinations = locations,
##D   mode = "transit",
##D   departure_time = Sys.time() + as.difftime(10, units = "mins"),
##D   key = key
##D )
##D mp_get_matrix(doc, value = "distance_m")
##D mp_get_matrix(doc, value = "distance_text")
##D mp_get_matrix(doc, value = "duration_s")
##D mp_get_matrix(doc, value = "duration_text")
##D 


################### 
########################## UI
ui <- fluidPage(
  
  navbarPage(
    "MBTA Transit Services",
    id = "main_navbar",
    theme = "cosmo",
    tabPanel("MBTA", leafletOutput("map"),textOutput("intro"),height = 700),
    tabPanel(
      "Buses",
      fluidRow(column(width = 3,selectInput(inputId = "stop_selected", label = "bus_stop", choices = unique(sl_routes_traveltimes$stop_name),selected = NULL))
               )
      ,fluidRow(column(DTOutput("bus_table")))
      ),
    tabPanel("BUS EDA",
      # Tab: BUS
      plotlyOutput("SLboxplot2")
    ),
  tabPanel("Rapid Transit EDA", 
           sidebarLayout(
             sidebarPanel(
               selectInput("lines", "Red Line vs Orange Line:", choices = c("Northbound","Southbound"), selected = NULL)
               ),
             mainPanel(plotlyOutput("traveltimes_dens")))
    )
  )
)







server <- function(input, output, session){
  # Tab:MBTA
  output$map <- renderLeaflet({
    # Boston view
    leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>% addTiles() %>%
      addPolylines(red_travel_times$dept_lon,red_travel_times$dept_lat,color="red")}) # %>% 
      # addCircleMarkers(head(red_travel_times$dept_lon),head(red_travel_times$dept_lat),radius=1)})
      # addPolylines(unique(greenb_travel_times$dept_lon)[1:5],unique(greenb_travel_times$dept_lat)[1:5],color="green")%>%
      # addCircleMarkers(unique(greenb_travel_times$dept_lon)[1:5],unique(greenb_travel_times$dept_lat)[1:5],radius=1,popup = unique(greenb_travel_times$dept_name))[1:5]}) #%>%
      # 
      # addPolylines(unique(greenc_travel_times$dept_lon),unique(greenc_travel_times$dept_lat,color="green")) %>%
      # addCircleMarkers(unique(greenc_travel_times$dept_lon),unique(greenc_travel_times$dept_lat),radius=1,popup = unique(greenc_travel_times$dept_name)) %>%
      # 
      # addPolylines(unique(greend_travel_times$dept_lon),unique(greend_travel_times$dept_lat),color="green") %>%
      # addCircleMarkers(unique(greend_travel_times$dept_lon),unique(greend_travel_times$dept_lat),radius=1,popup = unique(greend_travel_times$dept_name)) %>%
      
      # addPolylines(unique(greene_travel_times$dept_lon),unique(greene_travel_times$dept_lat),color="green") %>%
      # addCircleMarkers(unique(greene_travel_times$dept_lon),unique(greene_travel_times$dept_lat),radius=1,popup = unique(greene_travel_times$dept_name)) %>%
      
      # addPolylines(unique(orange_travel_times$dept_lon),unique(orange_travel_times$dept_lat,color="orange")) %>%
      # addCircleMarkers(unique(orange_travel_times$dept_lon),unique(orange_travel_times$dept_lat),radius=1,popup = unique(orange_travel_times$dept_name))})

  
    output$intro <- renderText({
     "The app displays MBTA transit information and my exploratory analysis of it. Information used for visualization is extracted from hostrical records of MBTA services and from current google Directions API. 
      I selected data from Novemenber 2021 to October 2o22 to explore."})
    
  # Tab bus
    
    output$bus_table <- renderDT({
      sl_routes_traveltimes %>% filter(stop_name == input$stop_selected) %>% dplyr::select(route_id,direction_id, headway, scheduled_headway) %>% distinct(route_id,.keep_all = T)
    })
    
  output$SLboxplot2 <- renderPlotly(
    plot_ly(data = sl_routes_traveltimes, y=~headway, color = ~stop_name, type = "box") %>%
      layout(title="Box plot of silver line travel times by stop", yaxis=list(title="Time",range=c(0,1000)))
  )
 
  # Tab:rapid transit:
  # subwayline <- reactive({
  #   input$lines })
  
  output$traveltimes_dens <- renderPlotly(
    # if (subwayline() == "Red Line"){
    # # Northbound density plot
    # red_n<- plot_ly(red_to_north_df, x = ~x, y = ~y, color = ~dept_name)%>% add_lines() %>% 
    #   layout(title = "Northbound density plot by stop", yaxis=list(title="Density"), xaxis=list(title="Northbound Travel Times",range = c(0,4000)))
    # 
    # # Southbound density plot
    # # red_s <- plot_ly(red_to_south_df, x = ~x, y = ~y, color = ~dept_name)%>% add_lines() %>% 
    # #   layout(title = "Southbound density plot by stop", yaxis=list(title="Density"), xaxis=list(title="SouthBound Travel Times",range = c(0,2000)))
    # }else{
      org_n <- plot_ly(orange_to_north_df, x = ~x, y = ~y, color = ~dept_name)%>% add_lines() %>% 
        layout(title = "Northbound density plot by stop", yaxis=list(title="Density"), xaxis=list(title="Northbound Travel Times",range = c(0,3000)))
      
      # org_s <- plot_ly(orange_to_south_df, x = ~x, y = ~y, color = ~dept_name)%>% add_lines() %>% 
      #   layout(title = "Southbound density plot by stop", yaxis=list(title="Density"), xaxis=list(title="SouthBound Travel Times",range = c(0,2000)))
  
  )
  
}

shinyApp(ui, server)
