#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("plotly")
library("tidyverse")
library("data.table")
library("gridExtra")
library("knitr")
library("gganimate")
library(shinydashboard)
library("maps")
library(zipcode)

rmdfiles <- c("about.Rmd")
sapply(rmdfiles, knit, quiet = T)

# Load athletes_events data 
data <- read_csv("fastfood_dataset_challenege.csv")

# Loaction selector

store_id = unique(data['store_id'])

#store location map data

data(zipcode)
us<-map_data('state')

order_total_by_location <- data %>%
    mutate(store_id = as.character(store_id)) %>%
    mutate(zip = as.character(postalcode)) %>%
    group_by(store_id, zip, location_no ) %>%
    summarize(total = sum(order_total))

order_total_by_location<- merge(order_total_by_location, zipcode, by='zip')

## Menu items popular
popular_menu_items <- data %>%
    mutate(item_no = as.character(item_no))  %>%
    group_by(item_no, store_id) %>%
    summarize(totalqty = sum(qty)) %>%
    arrange(desc(totalqty))

## Regular customers
regular_customers <- data %>%
    mutate(customer_id = as.character(customer_id))  %>%
    group_by(customer_id, store_id, order_id) %>%
    summarize(order_total = sum(order_total)) %>%
    group_by(customer_id, store_id)  %>%
    summarize(visit= n())%>%
    arrange(desc(visit))

## Order total Month year

order_total_monthyear <- data %>%
    mutate(store_id = as.character(store_id)) %>%
    group_by(year, month, store_id) %>%
    summarize(total= sum(order_total))%>%
    arrange(desc(year, month))

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Fast food chain store level analysis"),
    dashboardSidebar(
        sidebarUserPanel(name="Fast-food"),
        sidebarMenu(
            menuItem("About", tabName = "About", icon = icon("book")),
            menuItem("Store Locations", tabName = "USMap", icon = icon("map-marker")),
            menuItem("Customers", tabName = "customers", icon = icon("list-alt")),
            menuItem("Menu items",tabName = "MenuItems",icon = icon("list")),
            # menuItem("Order Amount", tabName = "OrderAmount", icon = icon("bar-chart-o")),
            menuItem("Time series", tabName = "TimeSeries", icon = icon("calendar"))
        )
    ),
    dashboardBody(
        
        tabItems(
            tabItem(tabName = "About", includeMarkdown("about.md")),
            
            tabItem(tabName = "USMap",
                    mainPanel(
                        tabsetPanel(
                            tabPanel("Map", plotlyOutput("storeLocationMap", height = 700, width = 1150)),
                            tabPanel("Orders")
                        )
                    )
            ),
            tabItem(tabName = "customers",
                    
                    fluidRow(box(
                        width = 9, (plotlyOutput(
                            "regular_customer", height = 700, width = 800
                        ))
                    ),
                    (
                        box(
                            title = "Select store",
                            width = 3,
                            selectizeInput("selected_store",
                                           "Select store",
                                           store_id, selected = "1153")
                        )))
                    
                    
            ),
            
            tabItem(tabName = "MenuItems",
                    
                    fluidRow(box(
                        width = 9, (plotlyOutput(
                            "popular_menu", height = 700, width = 800
                        ))
                    ),
                    (
                        box(
                            title = "Select store",
                            width = 3,
                            selectizeInput("selected",
                                           "Select store",
                                           store_id, selected = "1153")
                        )))
                    
                    
                    ),
                    
            
            
            tabItem(tabName = "TimeSeries", 
                fluidRow(box(
                width = 9, (plotlyOutput(
                    "order_over_time", height = 700, width = 800
                ))
            )))            
            
            ))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Location map #####
    
    output$storeLocationMap = renderPlotly(
        ggplot(data = order_total_by_location, aes(x = longitude, y = latitude)) +
            geom_polygon(
                data = us,
                aes(x = long, y = lat, group = group),
                color = "#888888",
                fill = "#f2caae"
            ) +
            geom_point(aes(text=paste(total, ",", zip, ":", store_id)), color = 'red') +
            theme(
                panel.background = element_rect(
                    fill = "lightblue",
                    colour = "lightblue",
                    size = 0.5,
                    linetype = "solid"
                ),
                panel.grid.major = element_line(
                    size = 0.5,
                    linetype = 'solid',
                    colour = "lightblue"
                ),
                panel.grid.minor = element_line(
                    size = 0.25,
                    linetype = 'solid',
                    colour = "lihtblue"
                )
            )
    )
    
    reactive_menu_loc = reactive({
        
        popular_menu_items %>% 
            filter(store_id ==input$selected)
    })
    
    output$popular_menu = renderPlotly({
        tmp = reactive_menu_loc()
        
        ggplot(tmp[1:10,] %>% arrange(desc(totalqty)), aes(x= item_no, y=totalqty))+
            geom_col(fill= "blue")+
            theme_minimal() +
            coord_flip()
    })
    
    reactive_cust_loc = reactive({
        
        regular_customers %>% 
            filter(store_id ==input$selected_store)
    })
    
    output$regular_customer = renderPlotly({
        tmp1 = reactive_cust_loc()
        
        ggplot(tmp1[1:10,] %>% arrange(), aes(x= customer_id, y=visit))+
            geom_col(fill= "blue")+
            theme_minimal() +
            coord_flip()
    })
    
    reactive_order_time = reactive({
        
        order_total_monthyear 
    })
    
    output$order_over_time = renderPlotly({
        tmp2 = reactive_order_time()
        
        ggplot(order_total_monthyear, aes(x= month, y=total, group= store_id, color = store_id))+
            geom_line()+
            theme_minimal() 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
