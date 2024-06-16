#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(leaflet)  
library(tidyverse) 
library(shinybusy)
library(RColorBrewer)
library(viridis) 

my_dataset <- readRDS(file = "data/cdc_demo.rds")

server <- function(input, output, session) {
    
    #Get summary statistics for the state ####
    the_mean_sd <- reactive({
        temp_mean <- round(mean(my_dataset[[input$prevalence_input]], na.rm = TRUE), digits = 1)
        temp_sd <- round(sd(my_dataset[[input$prevalence_input]], na.rm = TRUE), digits = 1)
        paste0("<b>Mean (SD):</b> ", temp_mean, "% (", temp_sd, ")")
    })
    
    the_median_iqr <- reactive({
        temp_median <- round(median(my_dataset[[input$prevalence_input]], na.rm = TRUE), digits = 1)
        temp_q1 <- quantile(my_dataset[[input$prevalence_input]], prob=0.25)[[1]]
        temp_q3 <- quantile(my_dataset[[input$prevalence_input]], prob=0.75)[[1]]
        paste0("<b>Median (Q1; Q3):</b> ", temp_median, "% (", temp_q1, "; ", temp_q3, ")")
    })
    
    the_ci <- reactive({
        temp_mean_ci <- mean(my_dataset[[input$prevalence_input]], na.rm = TRUE)
        temp_z <- 1.96
        temp_sd_ci <- sd(my_dataset[[input$prevalence_input]], na.rm = TRUE)
        temp_n <- nrow(my_dataset)
        
        lower_ci <- temp_mean_ci  - temp_z * temp_sd_ci / sqrt (temp_n)
        lower_ci <- round(lower_ci, digits = 1)
        upper_ci <- temp_mean_ci  + temp_z * temp_sd_ci / sqrt (temp_n)
        upper_ci <- round(upper_ci, digits = 1)
        
        paste0("<b>95% CI:</b> ", lower_ci, "\u2013", upper_ci, "%")
    })
    
    the_mean_sd_ci <- reactive({
        paste0(the_mean_sd(), ", ", the_ci())
    }) 
    
    #Render Summary stats #####
    output$mean_sd_ouput <- renderText(the_mean_sd())
    output$median_iqr_ouput <- renderText(the_median_iqr())
   # output$ci_ouput <- renderText(the_ci())
    output$mean_sd_ci_ouput <- renderText(the_mean_sd_ci())
    
    #Calculdate bin width #### 
    calculated_bin_width <- reactive({ 
        req(input$prevalence_input) 
        temp_bindwidth <- round(diff(range(my_dataset[[input$prevalence_input]], na.rm = TRUE))/10)
        if(temp_bindwidth == 0){temp_bindwidth <- 1}
        temp_bindwidth
    }) 
    
    #Update the field for bin width ####
    observeEvent(input$prevalence_input, {
        updateNumericInput(session, "bin_width", value = calculated_bin_width())
    })
    
    
    #Decide on final bin width #####
    final_bin_width <- reactive({
        if(is.na(input$bin_width)){calculated_bin_width} else {input$bin_width}
    })
    
    
    #Plot histogram for sidebar ######
    prevalence_histogram <- reactive({
        req(final_bin_width())
        req(input$prevalence_input)
        req(my_dataset)
        
        my_histogram <-  ggplot(data = my_dataset,
                                aes(x = .data[[input$prevalence_input]])) + 
            geom_histogram(color = "black" , fill = "#19d68f", show.legend = FALSE, 
                           binwidth = final_bin_width()) +
            xlab("Crude prevalence (%)") +
            ylab("Count")+ 
            theme_bw() 
        my_histogram
    })
    output$state_histogram <- renderPlot({ 
        validate(
            need(!is.null(prevalence_histogram), "Data is loading, please wait...")
        )
        req(prevalence_histogram())
        prevalence_histogram()}, res = 120)
    
    #Create Label for the markers #### 
    my_dataset_plot <- reactive({
        req(input$prevalence_input)
        my_dataset %>%
            mutate(the_label = paste0(StateDesc, ", ", CountyName, ", Tract FIPS ", TractFIPS,"<br>Total population: ",
                                      format(TotalPopulation, big.mark = "," ), "<br> Mean crude prevalence: ", 
                                      .data[[input$prevalence_input]],"%" ))
    })
    
    # Dynamic range slider for prevalence filter ####
    output$prevalence_slider <- renderUI({
        req(input$prevalence_input)
        sliderInput("prevalence_filter", "Crude prevalence (%) filter",  
                    min(my_dataset[[input$prevalence_input]]), 
                    max(my_dataset[[input$prevalence_input]]), 
                    value = range(my_dataset[[input$prevalence_input]]), step = 1) 
    })
    
    # Data subsetting #####
    filteredData <- reactive({
        req(input$prevalence_input, input$range, my_dataset_plot(), input$prevalence_filter)
        
        my_dataset_plot() %>% 
            filter(TotalPopulation >= input$range[1], 
                   TotalPopulation <= input$range[2], 
                   .data[[input$prevalence_input]] >= input$prevalence_filter[1], 
                   .data[[input$prevalence_input]] <= input$prevalence_filter[2]
            )
        
        # my_dataset_plot()[my_dataset_plot()$TotalPopulation >= input$range[1] & my_dataset_plot()$TotalPopulation <= input$range[2], ]
    })
    
    # Save color palette as reactive expression #####
    colorpal <- reactive({
        if(input$colors %in% c("viridis", "magma", "inferno", "plasma")) {the_reverse = TRUE} else {the_reverse = FALSE}
        colorNumeric(input$colors, domain = my_dataset_plot()[[input$prevalence_input]], 
                     reverse = the_reverse)
    })
    
    #Render the base map, fixed #####
    reactive_map <- reactive({
        leaflet(data = my_dataset_plot()) %>% addTiles()  %>%
            fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
    })
    
    output$map <- renderLeaflet(reactive_map()) 
    
    # Observer: partial changes of the map #####
    observe({
        req(filteredData()) #TODO
        pal <- colorpal()
        
        leafletProxy("map", data = filteredData()) %>%
            clearShapes() %>% #to clear features
            clearMarkers() %>%  #to clear any circle markers
            addCircleMarkers(
                lng =  ~ lng,
                lat =  ~ lat,
                radius = ~ input$radius_scaling * case_when(
                    input$scaling_factor == "Uniform" ~ 3, 
                    input$scaling_factor == "Population" ~ 0.5 * log(TotalPopulation), 
                    input$scaling_factor == "Prevalence" ~ 0.5 *  filteredData()[[input$prevalence_input]],
                    .default = 1
                ),
                popup =  ~ the_label,
                color =  ~ "#333333", #pal(filteredData()[[input$prevalence_input]]),
                weight = 0.5,
                fillOpacity = 0.7
                , fillColor = ~ pal(filteredData()[[input$prevalence_input]])
                
            )
    })
    
    # Extra observer updating the legend #####
    observe({
        req(filteredData()) #TODO
        proxy <- leafletProxy("map", data = my_dataset) #TODO
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        if (input$legend) {
            pal <- colorpal()
            proxy %>% addLegend(position = "topright",
                                pal = pal, 
                                values = ~ my_dataset[[input$prevalence_input]], 
                                title = "Crude prevalence (%)"
            )
        }
    }) 
    
} 
