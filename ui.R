library(shiny)
library(leaflet)  
library(tidyverse) 
library(shinybusy)
library(RColorBrewer)
library(viridis) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Load data, prepare variables #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Since the CDC data set is very large, a pre-processed subset has been prepared 
#for this demo to speed up load time. 
#In Brief, the CDC data were downloaded and filtered for California data. 
#Columns for the 95% CI for individual census tracts for omitted. 
#The Geolocation-column was splitted into 2 columns, lat and lng, to get the coordinates for
#each census tract.
#All rows with missing values have been removed. 

my_dataset <- readRDS(file = "data/cdc_demo.rds")
the_prevalences <- colnames(my_dataset %>% dplyr::select(contains("_CrudePrev")))  

health_outcomes_prevalences <- c("Arthritis" =  "ARTHRITIS_CrudePrev"
                                 ,"High blood pressure" = "BPHIGH_CrudePrev"
                                 ,"Cancer (excluding skin cancer)" =  "CANCER_CrudePrev"
                                 ,"Current asthma" = "CASTHMA_CrudePrev"
                                 , "Coronary heart disease" = "CHD_CrudePrev"
                                 , "COPD (chronic obstructive pulmonary disease)" = "COPD_CrudePrev"
                                 , "Depression" = "DEPRESSION_CrudePrev"
                                 , "Diabetes" = "DIABETES_CrudePrev"
                                 , "High cholesterol (during last 5 yrs)" = "HIGHCHOL_CrudePrev"
                                 , "CKD (chronic kidney disease)" = "KIDNEY_CrudePrev"
                                 , "Obesity" = "OBESITY_CrudePrev"
                                 , "Stroke" =  "STROKE_CrudePrev"
                                 , "All teeth lost (≥ 65 yrs old, 2020)" = "TEETHLOST_CrudePrev")

prevention_prevalences <- c( "No health insurance (18\u201364 yrs, 2021)" = "ACCESS2_CrudePrev"
                             ,"Taking antihypertensives" = "BPMED_CrudePrev"
                             , "Cervical cancer screening (women, 21\u201365 yrs)" = "CERVICAL_CrudePrev"
                             , "Routine checkup within past year" = "CHECKUP_CrudePrev"
                             , "Cholesterol screening" = "CHOLSCREEN_CrudePrev"
                             , "Colorectal cancer screening (50\u201375 yrs, 2021)" = "COLON_SCREEN_CrudePrev"
                             , "Up-to-date core prevention (men, ≥ 65 yrs old)" = "COREM_CrudePrev"
                             , "Up-to-date core prevention (women, ≥ 65 yrs old)" = "COREW_CrudePrev"
                             , "Dental visit (≥ 18 yrs old, 2020)" =  "DENTAL_CrudePrev"
                             , "Mammography (women 50–74 yrs old, 2020)" = "MAMMOUSE_CrudePrev")


disability_prevalences <- c(  "Hearing disability" = "HEARING_CrudePrev"
                              , "Vision disability" = "VISION_CrudePrev"
                              , "Cognition disability" = "COGNITION_CrudePrev"
                              , "Mobility disability" = "MOBILITY_CrudePrev"
                              , "Selfcare disability" = "SELFCARE_CrudePrev"
                              , "Independent living disability" = "INDEPLIVE_CrudePrev"
                              , "Any disability" = "DISABILITY_CrudePrev")

risk_prevalences <- c( "Binge drinking" = "BINGE_CrudePrev"
                       , "Current smoking" = "CSMOKING_CrudePrev"
                       , "Physical inactivity in leisure time" = "LPA_CrudePrev"
                       , "Sleeping < 7 h (≥ 18 yrs old, 2020)" = "SLEEP_CrudePrev"
)

status_prevalences <- c( "Fair or poor health" = "GHLTH_CrudePrev", 
                         "Mental health not good for ≥ 14 d" = "MHLTH_CrudePrev", 
                         "Physical health not good for ≥ 14 d" = "PHLTH_CrudePrev"
)

prevalence_choices = list(`Health outcomes` = health_outcomes_prevalences,
                          Prevention = prevention_prevalences,
                          Disabilities = disability_prevalences,
                          Risks = risk_prevalences,
                          `Health status` = status_prevalences
)

palettes_brewer_seq <- rownames(subset(brewer.pal.info, category %in% c("seq")))
palettes_viridis <- c("viridis", "magma", "inferno", 
                      "plasma")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#UI code #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- fluidPage( 
    title = "Demo: Epidemiological GIS using CDC PLACES 2023 Data",
    titlePanel( 
       div(style = "padding-left: 20px",
           "Demo: Epidemiological GIS using CDC PLACES 2023 Data"
            ,HTML(
                "<h4 style='font-size: 16px;'>
            Dr. Mathieu Licht \u2013 June 16, 2024
            </h4>
         "
            )
        )
    )
    , add_busy_spinner(spin = "fading-circle")
    , sidebarLayout(
        sidebarPanel(
            h4("Settings")
            , selectInput("prevalence_input", "Prevalence (all ≥ 18 yrs old, 2021, unless stated otherwise)", choices = prevalence_choices)
            , fluidRow(
                column(6, sliderInput("range", "Total population filter", min(my_dataset$TotalPopulation), 
                                                  max(my_dataset$TotalPopulation), 
                                                  value = range(my_dataset$TotalPopulation), step = 100))
               ,column(6, uiOutput("prevalence_slider") )
            )
            , fluidRow(
                column(3, selectInput("colors", "Color palette", 
                                      choices = list(Viridis = palettes_viridis,
                                                     Brewer = palettes_brewer_seq), 
                                      selected = "magma"
                ))
                ,column(4, selectInput("scaling_factor", "Circle size", choices = c("Uniform", "Population","Prevalence"), 
                                        selected = "Population"))
                ,column(3, numericInput("radius_scaling", "Scaling", value = 1, step = 0.5, max = 50, min = 0))
                ,column(2, checkboxInput("legend", "Show legend", TRUE))
            )
            , h4("State overview")
            , fluidRow(
                column(9, 
                        htmlOutput("mean_sd_ci_ouput")
                      , htmlOutput("median_iqr_ouput")
                ), 
                column(3, numericInput("bin_width",label = "Bin width", value = NA, min = 0, step = 1))
            )  
            ,div(
                style = "width: 100%; height: 182px;",
               plotOutput("state_histogram", width = "100%", height = "100%")
            )
        ) 
        # Show a plot of the generated distribution
        , mainPanel(
            tabsetPanel(id = "tabs"  
                        , type = "tabs"
                        , tabPanel("Interactive map"  
                                   , HTML("<div>
                        <h3>Overview</h3>
                        <p>This is a Shiny app-demo of an epidemiological geographic information system (GIS) using data from the CDC. You can view the model-based, crude 
                         prevalence of different health outcomes for the state of California (USA), stratified by census tract FIPS codes. 
                         Data were retrieved from the 
                         <a href='https://data.cdc.gov/500-Cities-Places/PLACES-Census-Tract-Data-GIS-Friendly-Format-2023-/yjkw-uj5s/about_data' 
                         target='_blank'>PLACES</a>  service.
                         </p>
                       </div>")
                        , 
                        fluidRow(leafletOutput("map", width = "100%", height =  "460px") )
                        )
                        , tabPanel("How to use"
                                   , HTML(
                                       "<div>
                       <h3>Side bar </h3>
                       <h4>Settings </h4>
                        <ul>
                          <li><b>Prevalence:</b> Select the health-related prevalence to display on the map.</li>
                          <li><b>Total population filter:</b> Filter for specific population sizes of the census tracts.</li>
                          <li><b>Crude prevalence (%) filter:</b> Filter for specific prevalence values (%).</li>
                          <li><b>Color palette:</b> Select a color palette for the prevalence markers on the map.</li>
                          <li><b>Circle size:</b> Select if prevalence markers are uniform in size or drawn according to population size or prevalence.</li> 
                          <li><b>Scaling:</b> The higher the value, the bigger the markers are drawn. <b>Note: </b> For most prevalences and zoom levels,
                          values up to 5 should suffice.</li> 
                          <li><b>Show legend:</b> Select to show the legend on the map.</li>
                        </ul>
                        <h4>State overview</h4>
                      <p>Summary statistics and distribution (histogram) for the selected prevalence across all census tracts in California.</p>
                        <ul>
                          <li><b>Mean (SD):</b> The mean and standard deviation of the selected prevalence. </li>
                          <li><b>Median (Q1; Q3):</b> The median, first and third quartile of the selected prevalence.</li>
                          <li><b>95% CI:</b> The 95% confidence interval for the mean of the selected prevalence.</li>
                          <li><b>Count (histogram):</b> The number of census tracts.</li>
                          <li><b>Bin width (%):</b> The bin width for plotting the histogram. By default, the bin width for each prevalence is set to result in approximately 
                          10&nbsp;bins.</li>
                        </ul>
                       <h3>Map</h3>
                       <ul>
                         <li><b> On-screen plus/minus-button or mousewheel:</b> Zoom in/out the map.</li>
                         <li><b>Drag left mouse button:</b> Move the map.</li>
                         <li><b>Click on prevalence markers:</b> Display information for: county name, census tract FIPS code, total population, and mean prevalence.</li>
                       </ul>
                       </div>
                       " 
                                   )
                        )
                        , tabPanel("Data reference"
                                   ,HTML(
                                       "<div>
                            <p>
                            <br>
                            Center for Disease Control and Prevention. (2023, August). <i>PLACES: Census Tract Data (GIS Friendly Format), 2023 release</i>. 
                            U.S. Department of Health & Human Services. Retrieved June 01, 2024, from 
                            <a href='https://data.cdc.gov/500-Cities-Places/PLACES-Census-Tract-Data-GIS-Friendly-Format-2023-/yjkw-uj5s/about_data' 
                            target='_blank'>https://data.cdc.gov/500-Cities-Places/PLACES-Census-Tract-Data-GIS-Friendly-Format-2023-/yjkw-uj5s/about_data</a>.
                            </p>
                            </div>"
                                   )
                        )
            ) #end tabset panel
        ) #end main panel
    ) #end sidebar layout
) #end fluid page 



