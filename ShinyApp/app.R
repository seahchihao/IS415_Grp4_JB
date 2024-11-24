#========================#
###### Load Package #####
#========================#
pacman::p_load(shiny, shinydashboard, shinythemes, fresh, plotly, bslib,
                sf, tmap, tidyverse, RColorBrewer, sfdep, spdep, tidygeocoder,
                rsconnect, olsrr, ggstatsplot, gtsummary, 
                performance, see
)

# create a colour scheme using fresh package
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E" # change the default light_blue colour
  )
)

#========================#
####### Load Data ########
#========================#
study_area <- read_rds("data/study_area.rds")
property <- read_rds("data/property.rds") %>% 
    filter(!`Property Type` %in% c("Flat", "Low-Cost Flat", "Low-Cost House") )
prox_factor_list <- read_rds("data/amenities.rds")

zoom_limits <- c(10, 25)

tall_map <- 720
medium_map <- 540
short_map <- 360

#========================#
##### Shiny UI Tabs ######
#========================#

# Home Tab
homepage_tab <- tabItem(
    tabName = "home",
    
    # Introduction/Welcome Section
    fluidRow(
        box(
            title = "Welcome to 'JB: The New Frontier?'", 
            status = "info", 
            solidHeader = TRUE, 
            width = 12,
            h3("Explore Johor Bahru's Real Estate Market"),
            p("Dive into the factors shaping housing prices in Johor Bahru.",
            "Navigate the application to uncover insights from geospatial data, analyse trends, and understand how amenities and geographic factors influence property values."),
            tags$ul(
                tags$li("Explore property clusters and proximity to amenities."),
                tags$li("Identify hot and cold spots using spatial autocorrelation analysis."),
                tags$li(tags$del("Understand factors influencing housing prices through regression models."))
            ),
            tags$p("Learn more about our project: ",
                   tags$a(href = "https://is415-grp4-jb.netlify.app/", 
                          "Visit our website", target = "_blank")),
            tags$hr(),
            h4("Get started by exploring the tabs in the sidebar menu!")
        )
    )
    
    # # Summary and Data Previews
    # fluidRow(
    #     box(
    #         title = "Preview: Property Data Table", 
    #         status = "warning", 
    #         solidHeader = TRUE, 
    #         width = 6,
    #         DT::dataTableOutput("home_table_preview"),
    #         footer = "Explore a sample of the property data. Full details available in the 'Property Data Table' tab."
    #     ),
    #     box(
    #         title = "Preview: Summary Statistics", 
    #         status = "warning", 
    #         solidHeader = TRUE, 
    #         width = 6,
    #         verbatimTextOutput("summary_stats"),
    #         footer = "Quick insights into key statistics for the dataset."
    #     )
    # ),
    
    # # Proximity Mapping Preview
    # fluidRow(
    #     box(
    #         title = "Preview: Proximity Mapping", 
    #         status = "warning", 
    #         solidHeader = TRUE,
    #         width = 12,
    #         tmapOutput("home_map_preview", width = "100%", height = short_map),
    #         footer = "Visualize spatial relationships between amenities and properties. Full mapping available in the 'Proximity Mapping' tab."
    #     )
    # )
)

# Property Data Table Tab
property_data_tab <- tabItem(
    tabName = "property_table",
    
    # Introduction Row
    fluidRow(
        box(
            title = "Property Data Table",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            p("Hi! You can use this page to plot various graphs derived from the property data."),
            tags$ul(
                tags$li("To start, (Step 1) Select the price and land area ranges by controlling the slider inputs on the left."),
                tags$li("(Step 2) Filter the property data by property types (if needed). If no property types is selected, all property types will be included in the calculations."),
            p("These inputs will affect both the plots and the data table to the right."),
                tags$li("(Step 3) Further adjust the distribution plots and view the graphical plots to the right"),
            h6("Note: you can download each plots as a png image by hovering over the plot and clicking on the camera icon.")
            ),

            p("You can also view the specific property rows in the data table at the end of the page. Control what columns to show in the table using the inputs to the left.")
        )
    ),
    
    # Inputs and Statistics Row
    fluidRow(
        column(
            width = 4,
            box(
                title = strong("Step 1. Filter by Price Range and Land/Parcel Area"),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                "Select and drag on the dots to adjust the price range.",
                h6("Note: the min and max possible values are calculated based on the min and max price values of the data set."),
                sliderInput(
                    "price_range", 
                    "Price Range:", 
                    min = min(property$Price_MYR, na.rm = TRUE), 
                    max = max(property$Price_MYR, na.rm = TRUE), 
                    value = c(min(property$Price_MYR, na.rm = TRUE), max(property$Price_MYR, na.rm = TRUE)), 
                    step = 1000,
                    pre = "MYR",
                    ticks = FALSE
                ),

                "Select and adjust the range to filter properties based on land area.",
                h6("Note: the min and max possible values are calculated based on the land area values in the data set."),
                sliderInput(
                    "area_range", 
                    "Land Area Range (sq m):", 
                    min = min(property$`Land/Parcel Area`, na.rm = TRUE),
                    max = max(property$`Land/Parcel Area`, na.rm = TRUE),
                    value = c(min(property$`Land/Parcel Area`, na.rm = TRUE), max(property$`Land/Parcel Area`, na.rm = TRUE)),
                    step = 10,
                    pre = "sq m", 
                    ticks = FALSE
                )
            ),
            
            box(
                title = strong("Step 2. Filter by Property Type"),
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                p("Type or select the property types by clicking on the text box.",
                    "Remove any selection using backspace or delete keys."),
                checkboxGroupInput(
                    inputId = "property_types", 
                    label = "Select Property Type:", 
                    choices = c(
                        "1 - 1 1/2 Storey Semi-Detached" = "1 - 1 1/2 Storey Semi-Detached", 
                        "1 - 1 1/2 Storey Terraced" = "1 - 1 1/2 Storey Terraced", 
                        "2 - 2 1/2 Storey Semi-Detached" = "2 - 2 1/2 Storey Semi-Detached", 
                        "2 - 2 1/2 Storey Terraced" = "2 - 2 1/2 Storey Terraced", 
                        "Cluster House" = "Cluster House",
                        "Condominium/Apartment" = "Condominium/Apartment",
                        "Detached" = "Detached",
                        "Town House" = "Town House"
                    )
                )
            )
        ),
        
        column(
            width = 8,
            fluidRow(
                width = 6,
                valueBox(
                    value = textOutput("num_properties"), 
                    subtitle = "Number of Properties Selected", 
                    icon = icon("home"), 
                    color = "yellow", 
                    width = 4
                ),
                box(
                    title = "Average Price",
                    status = "warning",
                    solidHeader = TRUE,
                    width = 4,
                    textOutput("avg_price")
                ),
                box(
                    title = "Median Price",
                    status = "warning",
                    solidHeader = TRUE,
                    width = 4,
                    textOutput("median_price")
                )
            ),
                        
            fluidRow(
                width = 6,
                box(
                    title = "Standard Deviation",
                    status = "warning",
                    solidHeader = TRUE,
                    width = 4,
                    textOutput("sd_price")
                ),
                box(
                    title = "Minimum Price",
                    status = "warning",
                    solidHeader = TRUE,
                    width = 4,
                    textOutput("min_price")
                ),
                box(
                    title = "Maximum Price",
                    status = "warning",
                    solidHeader = TRUE,
                    width = 4,
                    textOutput("max_price")
                )
            ),
            
            fluidRow(
                box(
                    title = strong("Step 3. Plot Price Distribution Graphs"), 
                    status = "primary", 
                    width = 12,
                    solidHeader = TRUE,
                    tabBox(
                        width = NULL,
                        tabPanel(
                            title ="Histogram of Prices",
                            fluidRow(
                                box(
                                    title = "Options (Click to Expand)",
                                    status = "primary",
                                    solidHeader = FALSE,
                                    width = 12,
                                    collapsible = TRUE,
                                    collapsed = TRUE,

                                    # Binwidth input
                                    sliderInput(
                                        "histogram_bin_width", 
                                        "Bin Width:",
                                        min = 100000,
                                        max = 2000000,
                                        value = 500000,
                                        step = 50000,
                                        pre = "MYR"
                                    ),
                                    # Title input
                                    textInput(
                                        inputId = "histogram_title",
                                        label = "Plot Title", 
                                        value = "Histogram of Property Prices"
                                    )
                                )
                            ),
                            fluidRow(
                                box(
                                    title = "Histogram Plot",
                                    status = "warning",
                                    solidHeader = TRUE,
                                    width = 12,
                                    plotlyOutput("price_histogram"),
                                    br(),
                                    p("A histogram is used to observe the distribution of property prices.",
                                    "Each bar represents the frequency of properties within a certain price range.",
                                    "The x-axis represents price ranges, while the y-axis represents the number of properties within each range.",
                                    "This allows you to easily spot trends like most common price ranges or identify skewed data.")
                                )
                            )
                        ),
                        tabPanel(
                            title ="Density Plot of Prices",
                            fluidRow(
                                box(
                                    title = "Options (Click to Expand)",
                                    status = "primary",
                                    solidHeader = FALSE,
                                    width = 12,
                                    collapsible = TRUE,
                                    collapsed = TRUE,

                                    # Title input
                                    textInput(
                                        inputId = "densityplot_title",
                                        label = "Plot Title", 
                                        value = "Density Plot of Property Prices"
                                    )
                                )
                            ),
                            fluidRow(
                                box(
                                    title = "Density Plot of Prices",
                                    solidHeader = TRUE,
                                    status = "warning",
                                    width = 12,
                                    plotlyOutput("price_density"),
                                    br(),
                                    p("The density plot is a smoothed version of the histogram.",
                                    "The method uses Kernel Density Estimation (KDE) based on the price values only; which is different from the KDE used in geospatial context.",
                                    "It shows the estimated distribution of property prices using a continuous curve.",
                                    "The x-axis represents price ranges, while the y-axis shows the probability density.", 
                                    "This plot helps to identify the shape of the price distribution (e.g., whether it is skewed, normal, or multimodal).")
                                )
                            )
                        ),
                        tabPanel(
                            title ="Boxplot of Prices", 
                            fluidRow(
                                box(
                                    title = "Options (Click to Expand)",
                                    status = "primary",
                                    solidHeader = FALSE,
                                    width = 12,
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    
                                    # Color input
                                    selectInput(
                                        "boxplot_color", 
                                        "Choose Boxplot Color", 
                                        choices = c("orange", "blue", "green", "purple"),
                                        selected = "orange"
                                    ),

                                    # Show Notches checkbox
                                    checkboxInput(
                                        "boxplot_show_notches", 
                                        "Show Notches", 
                                        value = FALSE
                                    ),

                                    # Title input
                                    textInput(
                                        inputId = "boxplot_plot_title",
                                        label = "Plot Title", 
                                        value = "Boxplot of Property Prices"
                                    )
                                )
                            ),
                            fluidRow(
                                box(
                                    title = "Price Boxplot",
                                    status = "warning",
                                    solidHeader = TRUE,
                                    width = 12,
                                    plotlyOutput("price_boxplot"),
                                    br(),
                                    p("The boxplot provides a summary of the distribution of property prices, showing the median (middle line), the interquartile range (box), and potential outliers (points outside the whiskers).",
                                    "This allows you to quickly see the range of property prices, the central tendency, and the variability in the data.")
                                )
                            )
                        ),
                        tabPanel(
                            title = "Scatter Plot",
                            fluidRow(
                                box(
                                    title = "Options (Click to Expand)",
                                    status = "primary",
                                    solidHeader = FALSE,
                                    width = 12,
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    
                                    # Land variable input
                                    selectInput(
                                        inputId = "scatterplot_land_variable",
                                        label = "Select Size Variable:",
                                        choices = c("Land/Parcel Area" = "Land/Parcel Area", 
                                                    "Main Floor Area" = "Main Floor Area"),
                                        selected = "Land/Parcel Area"
                                    ),
                                    # Title input
                                    textInput(
                                        inputId = "scatterplot_title",
                                        label = "Plot Title", 
                                        value = "Scatterplot of Property Prices vs Area"
                                    )
                                )
                            ),
                            fluidRow(
                                box(
                                    title = "Scatter Plot",
                                    status = "warning",
                                    solidHeader = TRUE,
                                    width = 12,
                                    plotlyOutput("price_scatter", height = "400px"),
                                    br(),
                                    p("This scatter plot shows the relationship between the selected property size variable (Land/Parcel Area or Main Floor Area) and the property price.",
                                    "By changing the size variable, you can explore how different types of property sizes impact the price.",
                                    "The x-axis represents the selected size variable, while the y-axis represents the price in MYR.")
                                )
                            )
                        ),
                        tabPanel(
                            title = "Violin Plot of Prices",
                            box(
                                title = "Violin Plot of Prices",
                                status = "warning",
                                solidHeader = TRUE,
                                width = 12, 
                                plotlyOutput("price_violin"),
                                br(),
                                p("The violin plot combines aspects of a boxplot and a density plot.",
                                "It shows the distribution of property prices for each property type.", 
                                "The width of the 'violin' at different price levels indicates the density of properties within that price range.",
                                "This plot helps you understand the spread of prices across different property types and detect any potential skewness in the distribution.")
                            )
                        )
                    )
                )
            )
        )
    ),

    fluidRow(
        column(
            width = 3,
            box(
                title = "Columns to Show for Data Table", 
                status = "primary",
                solidHeader = TRUE,
                width = 12, 
                checkboxGroupInput(
                    "show_vars", 
                    "Columns to show:", 
                    names(property), 
                    selected = names(property)
                )
            )
        ),
        column(
            width = 9,
            box(
                title = "Property Data Table", 
                status = "warning",
                width = 12,
                solidHeader = TRUE,
                DT::dataTableOutput("property_table")
            )
        )
    )
)

# Proximity Factors Mapping Tab
proximity_mapping_tab <- tabItem(
    tabName = "proximity_mapping",
    fluidRow(
        box(
            title = "Proximity Factors Mapping", 
            status = "warning", 
            solidHeader = TRUE,
            width = 12,
            tmapOutput("amenity_map", height = short_map)
        )
    ),
    fluidRow(
        box(
            title = "Proximity Factors Controls", 
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            
            # Control whether to show layer types (Points / Lines / Polygons)
            checkboxInput("show_points", label = "Show Points", value = TRUE),
            checkboxInput("show_lines", label = "Show Lines", value = TRUE),
            checkboxInput("show_polygons", label = "Show Polygons", value = FALSE),
            checkboxInput("cluster_points", label = "Enable Point Clustering*", value = TRUE),
            h6("*once enabled, points that appear too close will be clustered together")
        ),
        box(
            title = "Select Proximity Factors", 
            status = "primary",
            solidHeader = TRUE,
            width = 9,
            
            fluidRow(
                column(
                    width = 4,
                    # Group: Government Facilities and Coastlines
                    strong("Government Facilities & Coastlines"),
                    checkboxGroupInput(
                        inputId = "gov_facilities_coastlines",
                        label = NULL,
                        choices = c(
                            "Customs Checkpoint" = "customs_facilities",
                            "Natural Coastlines of Johor" = "coastlines",
                            "Waterfronts and Beaches" = "waterfronts_and_beaches"
                        )
                    ),
                    
                    # Group: Education
                    strong("Educational Institutions"),
                    checkboxGroupInput(
                        inputId = "education",
                        label = NULL,
                        choices = c(
                            "Kindergartens" = "kindergartens",
                            "Schools" = "schools",
                            "Universities/Colleges" = "universities_and_colleges"
                        )
                    ),
                    # Group: Government & Public Buildings
                    strong("Government & Public Buildings"),
                    checkboxGroupInput(
                        inputId = "gov_buildings",
                        label = NULL,
                        choices = c(
                            "Offices and Townhalls" = "offices,townhalls",
                            "Police and Fire Stations" = "police_and_fire_stations",
                            "Hospitals and Specialists" = "hospitals_and_specialists",
                            "Cemeteries (Landuse)" = "cemetery"
                        ),
                        selected = "offices,townhalls"
                    )
                ),
                
                column(
                    width = 4,
                    # Group: Land Use (Commercial & Industrial)
                    strong("Land Use: Commercial & Industrial"),
                    checkboxGroupInput(
                        inputId = "land_use_commercial_industrial",
                        label = NULL,
                        choices = c(
                            "Commercial (Landuse)" = "commercial",
                            "Industrial (Landuse)" = "industrial"
                        )
                    ),
                    # Group: Recreation & Nature
                    strong("Recreation & Nature"),
                    checkboxGroupInput(
                        inputId = "recreation_nature",
                        label = NULL,
                        choices = c(
                            "Parks and Green Spaces" = "parks_and_green_spaces",
                            "Theme Parks and Resorts" = "theme_parks_and_resorts"
                        )
                    ),
                    # Group: Places of Worship
                    strong("Places of Worship"),
                    checkboxGroupInput(
                        inputId = "religious_places",
                        label = NULL,
                        choices = c(
                            "Buddhist Temples" = "buddhist_temples",
                            "Churches" = "churches",
                            "Hindu Temples" = "hindu_temples",
                            "Mosques" = "mosques"
                        )
                    )
                ),
                column(
                    width = 4,
                    # Group: Commercial Services
                    strong("Commercial & Public Services"),
                    checkboxGroupInput(
                        inputId = "commercial_services",
                        label = NULL,
                        choices = c(
                            "Marketplace and Malls" = "marketplace,malls",
                            "Restaurant, Cafe, and Fast Food Restaurants" = "restaurant, cafe, fast_food",
                            "Supermarket and Convenience Stores" = "supermarket,convenience",
                            "Petrol Stations" = "petrol_stations"
                        )
                    ),
                    # Group: Transport
                    strong("Transport & Infrastructure"),
                    checkboxGroupInput(
                        inputId = "transport_infrastructure",
                        label = NULL,
                        choices = c(
                            "Airport" = "airport",
                            "Bus Stops" = "bus_stops",
                            "Bus Terminals" = "bus_terminals",
                            "Carparks" = "carparks"
                        )
                    )
                )
            )
        )
    ),
    fluidRow(
        box(
            title = "Proximity Factors Data Table", 
            status = "primary", 
            width = 12,
            solidHeader = TRUE,
            DT::dataTableOutput("proximity_factor_table")
        )   
    ) 
)

# Property Plot Tab
property_plot_tab <- tabItem(
    tabName = "property_plot",
    fluidRow(
        box(
            width = 4,
            title = "Options",
            status = "primary",
            solidHeader = FALSE,

            checkboxGroupInput(
                inputId = "property_plot_property_types",
                label = "Include: ",
                choices = c(
                    "1 - 1 1/2 Storey Semi-Detached" = "1 - 1 1/2 Storey Semi-Detached", 
                    "1 - 1 1/2 Storey Terraced" = "1 - 1 1/2 Storey Terraced", 
                    "2 - 2 1/2 Storey Semi-Detached" = "2 - 2 1/2 Storey Semi-Detached", 
                    "2 - 2 1/2 Storey Terraced" = "2 - 2 1/2 Storey Terraced", 
                    "Cluster House" = "Cluster House",
                    "Condominium/Apartment" = "Condominium/Apartment",
                    "Detached" = "Detached",
                    "Town House" = "Town House"
                ),
                selected = "1 - 1 1/2 Storey Semi-Detached"
            ),
            selectInput(
                inputId = "property_plot_currency",
                label = "Currency",
                choices = list(
                    "MYR" = "Price_MYR",
                    "SGD" = "Price_SGD",
                    "USD" = "Price_USD"
                ),
                selected = "Price_MYR"
            ),
            sliderInput(
                inputId = "property_plot_num_classes",
                label = "Number of classes",
                min = 2,
                max = 20,
                value = 6
            ),
            selectInput(
                inputId = "property_plot_classification",
                label = "Classification method:",
                choices = list(
                    "sd" = "sd",
                    "equal" = "equal",
                    "pretty" = "pretty",
                    "quantile" = "quantile",
                    "kmeans" = "kmeans"
                ),
                selected = "kmeans"
            ),
            sliderInput(
                inputId = "property_plot_transparency",
                label = "Level of transparency",
                min = 0,
                max = 1,
                value = c(0.9)
            )
        ),
        box(
            title = "Map of all property transactions in JB and Kulai between 2023 and 2024.",
            width = 8,
            status = "warning",
            solidHeader = TRUE,
            p("Hover over a point to reveal its details."),
            tmapOutput(
                "base_map_plot",
                width = "100%",
                height = tall_map
            )
        )
    )
)

# Hexagonal Grid Tab
hexagonal_grid_tab <- tabItem(
    tabName = "hexagonal_grid",
    fluidRow(
        box(
            width = 12,
            title = "Map of the hexagonal grid plotted over JB and Kulai.",
            status = "warning",
            solidHeader = TRUE,
            p("Click on 'Generate Map' to begin."),
            p(actionButton("hexagonal_grid_update", "Generate Map")),
            strong("Select a price variable to plot and select mapping parameters on the input panels below."),
            strong("To update the map, click on 'Update Plot' below."),
            tmapOutput("hex_grid",
                width = "100%",
                height = short_map
            )
        )
    ),
    fluidRow(  
        box(
            title = "Inputs",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
                column(
                    width = 6,
                    box(
                        width = 12,
                        checkboxGroupInput(
                            inputId = "hexagonal_grid_property_types",
                            label = "Include: ",
                            choices = c(
                                "1 - 1 1/2 Storey Semi-Detached" = "1 - 1 1/2 Storey Semi-Detached", 
                                "1 - 1 1/2 Storey Terraced" = "1 - 1 1/2 Storey Terraced", 
                                "2 - 2 1/2 Storey Semi-Detached" = "2 - 2 1/2 Storey Semi-Detached", 
                                "2 - 2 1/2 Storey Terraced" = "2 - 2 1/2 Storey Terraced", 
                                "Cluster House" = "Cluster House",
                                "Condominium/Apartment" = "Condominium/Apartment",
                                "Detached" = "Detached",
                                "Town House" = "Town House"
                            ),
                            selected = "1 - 1 1/2 Storey Semi-Detached"
                        )
                    ),
                    box(
                        width = 12,
                        selectInput(
                            inputId = "hexagonal_grid_mapping_variable",
                            label = "Mapping variable",
                            choices = list(
                                "Density" = "density",
                                "Average Property Prices" = "avg_price",
                                "Median Property Prices" = "median_price",
                                "Maximum Property Prices" = "max_price"
                            ),
                            selected = "median_price"
                        )
                    ),
                    box(
                        width = 12,
                        selectInput(
                            inputId = "hexagonal_grid_classification",
                            label = "Classification method:",
                            choices = list(
                                "sd" = "sd",
                                "equal" = "equal",
                                "pretty" = "pretty",
                                "quantile" = "quantile",
                                "kmeans" = "kmeans"
                            ),
                            selected = "kmeans"
                        )
                    )
                ),
                column(
                    width = 6,
                    box(
                        width = 12,
                        sliderInput(
                            inputId = "hexagonal_grid_num_classes",
                            label = "Number of classes",
                            min = 2,
                            max = 20,
                            value = 6
                        )
                    ),
                    box(
                        width = 12,
                        selectInput(
                            inputId = "hexagonal_grid_colour",
                            label = "Colour scheme:",
                            choices = list(
                                "blues" = "Blues",
                                "reds" = "Reds",
                                "greens" = "Greens",
                                "Yellow-Orange-Red" = "YlOrRd",
                                "Yellow-Orange-Brown" = "YlOrBr",
                                "Yellow-Green" = "YlGn",
                                "Orange-Red" = "OrRd"
                            ),
                            selected = "YlOrRd"
                        )
                    ),
                    box(
                        width = 12,
                        sliderInput(
                            inputId = "hexagonal_grid_cell_size",
                            label = "Cell size (metres):",
                            min = 300,
                            max = 10000,
                            value = 1000
                        )
                    ),
                    box(
                        width = 12,
                        sliderInput(
                            inputId = "hexagonal_grid_transparency",
                            label = "Level of transparency",
                            min = 0,
                            max = 1,
                            value = 0.9
                        )
                    )
                )
            ),
            fluidRow(
                box(
                    actionButton("hexagonal_grid_update", "Update Plot")
                )
            )
        )
    )
)

# Local Moran I Tab
local_moran_tab <- tabItem(
    tabName = "local_moran",
    box(
        title = "Inputs",
        status = "primary",
        solidHeader = TRUE,
        width = 4,
        checkboxGroupInput(
            inputId = "types_to_keep_lmi",
            label = "Include: ",
            choices = c(
                    "1 - 1 1/2 Storey Semi-Detached" = "1 - 1 1/2 Storey Semi-Detached", 
                    "1 - 1 1/2 Storey Terraced" = "1 - 1 1/2 Storey Terraced", 
                    "2 - 2 1/2 Storey Semi-Detached" = "2 - 2 1/2 Storey Semi-Detached", 
                    "2 - 2 1/2 Storey Terraced" = "2 - 2 1/2 Storey Terraced", 
                    "Cluster House" = "Cluster House",
                    "Condominium/Apartment" = "Condominium/Apartment",
                    "Detached" = "Detached",
                    "Town House" = "Town House"
                ),
            selected = "1 - 1 1/2 Storey Semi-Detached"
        ),
        selectInput(
            inputId = "mukim_lm",
            label = "District (Mukim)",
            choices = c(
                "MUKIM BUKIT BATU" = "MUKIM BUKIT BATU",
                "MUKIM SEDENAK" = "MUKIM SEDENAK",
                "MUKIM KULAI" = "MUKIM KULAI",
                "MUKIM TANJUNG KUPANG" = "MUKIM TANJUNG KUPANG",
                "MUKIM PULAI" = "MUKIM PULAI",
                "MUKIM JELUTONG" = "MUKIM JELUTONG",
                "MUKIM SENAI" = "MUKIM SENAI",
                "BANDAR KULAI" = "BANDAR KULAI",
                "MUKIM TEBRAU" = "MUKIM TEBRAU",
                "BANDAR JOHOR BAHRU" = "BANDAR JOHOR BAHRU",
                "MUKIM PLENTONG" = "MUKIM PLENTONG",
                "MUKIM SUNGAI TIRAM" = "MUKIM SUNGAI TIRAM"
            ),
            selected = "BANDAR JOHOR BAHRU"
        ),
        selectInput(
            inputId = "variable",
            label = "Subject variable",
            choices = list(
                "Average Property Prices" = "avg_price",
                "Median Property Prices" = "median_price",
                "Maximum Property Prices" = "max_price"
            ),
            selected = "median_price"
        ),
        actionButton("MoranUpdate", "Update Plot"),
        radioButtons(
            inputId = "MoranConf",
            label = "Confidence level",
            choices = c(
                "0.95" = 0.05,
                "0.99" = 0.01
            ),
            selected = 0.05,
            inline = TRUE
        ),
        selectInput(
            inputId = "LisaClass", 
            label = "Lisa Classification",
            choices = c(
                "mean" = "mean",
                "median" = "median"
            ),
            selected = "mean"
        ),
        selectInput(
            inputId = "localmoranstats", 
            label = "Output variable",
            choices = c(
                "local moran(ii)" = "local moran(ii)",
                "expectation(eii)" = "expectation(eii)",
                "variance(var_ii)" = "variance(var_ii)",
                "std deviation(z_ii)" = "std deviation(z_ii)",
                "P-value" = "p_value"
            ),
            selected = "local moran(ii)"
        ),
        sliderInput(
            inputId = "opacity",
            label = "Level of transparency",
            min = 0,
            max = 1,
            value = 0.9
        )
    ),                
    box(
        title = "Output",
        status = "warning",
        solidHeader = TRUE,
        width = 8,
        p("The selected output variable map will be displayed on the left, and the LISA class map on the right.", 
        "Customise the parameters in the left panel, then click 'Update Plot' to begin."),
        fluidRow(
            column(6, tmapOutput("LocalMoranMap", width = "100%", height = medium_map)),
            column(6, tmapOutput("LISA", width = "100%", height = medium_map))
        )
    )
)

# Hot Cold Analysis Tab
hot_cold_analysis_tab <- tabItem(
    tabName = "hot_cold_analysis",
    box(
        title = "Inputs",
        status = "primary",
        solidHeader = TRUE,
        width = 4,
        checkboxGroupInput(
            inputId = "types_to_keep_lgi",
            label = "Include: ",
            choices = c(
                        "1 - 1 1/2 Storey Semi-Detached" = "1 - 1 1/2 Storey Semi-Detached", 
                        "1 - 1 1/2 Storey Terraced" = "1 - 1 1/2 Storey Terraced", 
                        "2 - 2 1/2 Storey Semi-Detached" = "2 - 2 1/2 Storey Semi-Detached", 
                        "2 - 2 1/2 Storey Terraced" = "2 - 2 1/2 Storey Terraced", 
                        "Cluster House" = "Cluster House",
                        "Condominium/Apartment" = "Condominium/Apartment",
                        "Detached" = "Detached",
                        "Town House" = "Town House"
                    ),
            selected = "1 - 1 1/2 Storey Semi-Detached"
        ),
        selectInput(
            inputId = "mukim_gi",
            label = "District (Mukim)",
            choices = list(
                "MUKIM BUKIT BATU" = "MUKIM BUKIT BATU",
                "MUKIM SEDENAK" = "MUKIM SEDENAK",
                "MUKIM KULAI" = "MUKIM KULAI",
                "MUKIM TANJUNG KUPANG" = "MUKIM TANJUNG KUPANG",
                "MUKIM PULAI" = "MUKIM PULAI",
                "MUKIM JELUTONG" = "MUKIM JELUTONG",
                "MUKIM SENAI" = "MUKIM SENAI",
                "BANDAR KULAI" = "BANDAR KULAI",
                "MUKIM TEBRAU" = "MUKIM TEBRAU",
                "BANDAR JOHOR BAHRU" = "BANDAR JOHOR BAHRU",
                "MUKIM PLENTONG" = "MUKIM PLENTONG",
                "MUKIM SUNGAI TIRAM" = "MUKIM SUNGAI TIRAM"
            ),
            selected = "BANDAR JOHOR BAHRU"
        ),
        selectInput(
            inputId = "variable",
            label = "Subject variable",
            choices = list(
                "Average Property Prices" = "avg_price",
                "Median Property Prices" = "median_price",
                "Maximum Property Prices" = "max_price"
            ),
            selected = "median_price"
        ),
        radioButtons(
            inputId = "BandwidthType",
            label = "Bandwidth",
            choices = c(
                "Fixed" = FALSE,
                "Adaptive" = TRUE
            ),
            selected = "TRUE",
            inline = TRUE
        ),
        sliderInput(
            inputId = "k",
            label = "k-means K value (adaptive only)",
            min = 2, max = 15,
            value = 8, step = 1
        ),
        selectInput(
            "GiWeights", 
            "Spatial Weights Style",
            choices = c(
            "W: Row standardised" = "W",
            "B: Binary" = "B",
            "C: Globally standardised" = "C",
            "U: C / no of neighbours" = "U",
            "minmax" = "minmax",
            "S: Variance" = "S"
            ),
            selected = "B"
        ),
        actionButton("GiUpdate", "Update Plot"),
        sliderInput(
            inputId = "opacity",
            label = "Level of transparency",
            min = 0,
            max = 1,
            value = 0.9
        )
    ),
    box(
        title = "Output",
        status = "warning",
        solidHeader = TRUE,
        width = 8,
        p("The hot/cold spot map will be displayed on the left, and the price map on the right.",
        "Customise the parameters in the left panel, then click 'Update Plot' to begin."),
        tmapOutput(
          "Gi",
          width = "100%",
          height = tall_map
        )
    )
)

#========================#
######## Shiny UI ########
#========================#

# Dashboard Page
ui <- dashboardPage(
    dashboardHeader(
        title = "JB: The New Frontier?",
        titleWidth = 300
    ),
    
    dashboardSidebar(
        sidebarMenu(
            id = "sidebarID",
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Exploratory Data Analysis", 
                    icon = icon("chart-bar"),
                    menuSubItem("Property Data Table", tabName = "property_table", icon = icon("table")),
                    menuSubItem("Proximity Factors Mapping", tabName = "proximity_mapping", icon = icon("map-marker-alt")),
                    menuSubItem("Property (Points)", tabName = "property_plot", icon = icon("map-marker-alt")),
                    menuSubItem("Property (Hexbin)", tabName = "hexagonal_grid", icon = icon("border-all"))
            ),
            menuItem("Hot/Cold Spot Analysis", 
                    icon = icon("thermometer-half"),
                    menuSubItem("Local Moran I", tabName = "local_moran", icon = icon("area-chart")),
                    menuSubItem("Local Gi*", tabName = "hot_cold_analysis", icon = icon("thermometer-half"))
            )
        )
    ),

    dashboardBody(
        use_theme(mytheme),
        tabItems(
            homepage_tab,

            property_data_tab,
            proximity_mapping_tab,
            property_plot_tab,
            hexagonal_grid_tab,

            local_moran_tab,
            hot_cold_analysis_tab
        )
    )
)

#========================#
###### Shiny Server ######
#========================#

server <- function(input, output, session) {
    
    # ==========================================================
    # HOME PAGE
    # ==========================================================
    
    # Data Table Preview
    output$home_table_preview <- DT::renderDataTable({
        DT::datatable(
            property %>%
                head(5),  # Show only the first 5 rows
            options = list(
                paging = FALSE,
                searching = FALSE,
                info = FALSE,
                scrollX = TRUE,
                scrollY = "400px",
                columnDefs = list(
                    list(targets = 0:5, width = '150px', className = 'dt-left'),
                    list(targets = "_all", width = '200px', className = 'dt-left') 
                )
            )
        )
    })
    
    # Summary Statistics Preview
    output$summary_stats <- renderPrint({
        data <- property
        
        summary_stats <- data.frame(
            Mean = mean(data$Price_MYR, na.rm = TRUE),
            Median = median(data$Price_MYR, na.rm = TRUE),
            Std_Dev = sd(data$Price_MYR, na.rm = TRUE),
            Min = min(data$Price_MYR, na.rm = TRUE),
            Max = max(data$Price_MYR, na.rm = TRUE)
        )
        
        print(summary_stats)
    })
    
    # Proximity Factors Preview
    output$home_map_preview <- renderTmap({
        map <- tm_shape(study_area) + 
            tm_borders(col = "black", lwd = 1) +
            tm_text("shapeName", size = 1, col = "black", shadow = TRUE) +
            tm_view(bbox = study_area, set.zoom.limits = zoom_limits) + 
            tm_basemap("OpenStreetMap", alpha = 0.5)
            tm_layout(legend.show = FALSE, 
                title = "Map of Study Area", 
                title.size = 1.5, 
                title.position = c("left", "top"))
        map
        
    })

    # ==========================================================
    # PROPERTY DATA TABLE
    # ==========================================================
    
    filtered_data <- reactive({
        
        # Filter by price range
        filtered_price_data <- property %>%
            filter(Price_MYR >= input$price_range[1], Price_MYR <= input$price_range[2])  %>% 
            filter(`Land/Parcel Area` >= input$area_range[1], `Land/Parcel Area` <= input$area_range[2])
        
        # If property types are selected, filter by selected property types
        if (!is.null(input$property_types)) {
            filtered_price_data <- filtered_price_data %>%
                filter(`Property Type` %in% input$property_types)
        }
        
        return(filtered_price_data)
    })
    
    output$property_table <- DT::renderDataTable({
        DT::datatable(
            filtered_data(),
            options = list(
                orderClasses = TRUE,
                scrollX = TRUE,
                scrollY = "400px",
                lengthMenu = c(5, 30, 50),
                pageLength = 5
            )
        )
    })
    
    # Number of Properties
    output$num_properties <- renderText({
        nrow(filtered_data())
    })
    
    # Average Price
    output$avg_price <- renderText({
        mean(filtered_data()$Price_MYR, na.rm = TRUE) %>%
            scales::dollar(prefix = "MYR ")
    })
    
    # Minimum Price
    output$min_price <- renderText({
        min(filtered_data()$Price_MYR, na.rm = TRUE) %>%
            scales::dollar(prefix = "MYR ")
    })
    
    # Maximum Price
    output$max_price <- renderText({
        max(filtered_data()$Price_MYR, na.rm = TRUE) %>%
            scales::dollar(prefix = "MYR ")
    })
    
    output$median_price <- renderText({
        median(filtered_data()$Price_MYR, na.rm = TRUE) %>%
            scales::dollar(prefix = "MYR ")
    })

    output$sd_price <- renderText({
        sd(filtered_data()$Price_MYR, na.rm = TRUE) %>%
            scales::dollar(prefix = "MYR ")
    })
    
    # Histogram Plot
    output$price_histogram <- renderPlotly({
        ggplot_histogram <- ggplot(filtered_data(), aes(x = Price_MYR)) +
            geom_histogram(binwidth = input$histogram_bin_width, fill = "blue", color = "white", alpha = 0.5) +
            labs(title = input$histogram_title, x = "Price (MYR)", y = "Count") +
            scale_x_continuous(labels = scales::label_comma()) +
            theme_minimal()
        
      ggplotly(ggplot_histogram)
    })
    
    # Density Plot
    output$price_density <- renderPlotly({
        ggplot_density <- ggplot(filtered_data(), aes(x = Price_MYR)) +
            geom_density(fill = "green", alpha = 0.5) +
            labs(title = input$densityplot_title, x = "Price (MYR)", y = "Density") +
            scale_x_continuous(labels = scales::label_comma()) +
            theme_minimal()
        
        ggplotly(ggplot_density)
    })

    # Box Plot
    output$price_boxplot <- renderPlotly({
        ggplot_boxplot <- ggplot(filtered_data(), aes(x = factor(0), y = Price_MYR)) +
            geom_boxplot(fill = input$boxplot_color, alpha = 0.5, notch = input$boxplot_show_notches) +
            labs(title = input$boxplot_plot_title, x = "", y = "Price (MYR)") +
            theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
            scale_y_continuous(labels = scales::label_comma()) +
            theme_minimal()
        
        ggplotly(ggplot_boxplot)
    })
    
    # Scatter Plot
    output$price_scatter <- renderPlotly({
      
        req(input$scatterplot_land_variable)
      
        ggplot_scatter <- ggplot(
            filtered_data(), 
            aes_string(x = paste0("`", input$scatterplot_land_variable, "`"),
            y = "Price_MYR")) +
            geom_point(color = "purple", alpha = 0.6) +
            labs(title = input$scatterplot_title, 
                 x = gsub("_", " ", input$scatterplot_land_variable), 
                 y = "Price (MYR)") +
            scale_y_continuous(labels = scales::label_comma()) +
            theme_minimal()
        
        ggplotly(ggplot_scatter)
    })
    
    # Violin Plot
    output$price_violin <- renderPlotly({
      
        ggplot_violin <- ggplot(filtered_data(), aes(x = `Property Type`, y = Price_MYR)) +
            geom_violin(fill = "lightgreen", alpha = 0.7) +
            labs(
            title = "Price Distribution by Property Type", 
            x = "Property Type", 
            y = "Price (MYR)"
            ) +
            scale_y_continuous(labels = scales::label_comma()) +
            theme_minimal()
      
        ggplotly(ggplot_violin) %>%
            layout(
                xaxis = list(
                  tickangle = -45 # Rotate labels
                )
            )
    })

    # ==========================================================
    # PROXIMITY FACTORS MAPPING
    # ==========================================================

    input_prox_factor <- reactive({ 
        
        input_prox_factor <- c(input$gov_facilities_coastlines, 
                                input$education,
                                input$gov_buildings,
                                input$land_use_commercial_industrial,
                                input$recreation_nature,
                                input$religious_places,
                                input$commercial_services,
                                input$transport_infrastructure)

        selected_prox_factors <- prox_factor_list[input_prox_factor]
        return (selected_prox_factors)
    })


    output$amenity_map <- renderTmap({
        tmap_options(max.categories = 400)
      
        # Base map setup
        map <- tm_shape(study_area) + 
            tm_borders(col = "black", lwd = 1) +
            tm_text("shapeName", size = 1, col = "black", shadow = TRUE) +
            tm_view(bbox = study_area, set.zoom.limits = zoom_limits) + 
            tm_basemap("OpenStreetMap", alpha = 0.5)
            tm_layout(legend.show = FALSE, 
                title = "Map of Study Area", 
                title.size = 1.5, 
                title.position = c("left", "top"))
      
        selected_prox_factors <- input_prox_factor()
      
        # Loop through selected proximity factor to add to the map
        for (prox_factor_name in names(selected_prox_factors)) {
        
            prox_factor <- selected_prox_factors[[prox_factor_name]]
            
            if (inherits(prox_factor, "sf")) {
                geom_type <- unique(st_geometry_type(prox_factor))
                
                # Plot points
                if ("POINT" %in% geom_type && input$show_points) {
                map <- map + 
                    tm_shape(prox_factor) +
                    tm_dots(col = "blue", 
                            size = 0.1, 
                            cluster = input$cluster_points,  # Cluster based on user input
                            alpha = 0.8,
                            border.col = NA)
                }
                
                # Plot lines
                if (("LINESTRING" %in% geom_type || "MULTILINESTRING" %in% geom_type) && input$show_lines) {
                map <- map + 
                    tm_shape(prox_factor) + 
                    tm_lines(col = "green")
                }
                
                # Plot polygons
                if (("POLYGON" %in% geom_type || "MULTIPOLYGON" %in% geom_type) && input$show_polygons) {
                map <- map + tm_shape(prox_factor) +
                    tm_borders(col = "magenta") +
                    tm_fill("name", legend.show = FALSE)
                }
            }
        }
        map
    })
    
    # Render the data table with selected amenities
    output$proximity_factor_table <- DT::renderDataTable({

        selected_data <- do.call(rbind, lapply(input_prox_factor(), function(prox_factor) {
            if (inherits(prox_factor, "sf")) {
                
                # Add a column for geometry type
                prox_factor$`Geometry Type` <- sapply(st_geometry(prox_factor), function(geom) {
                    geom_type <- st_geometry_type(geom)
                    return(geom_type)
                })
                # convert the geometry to text (WKT) 
                prox_factor$Geometry <- st_as_text(st_geometry(prox_factor))
                
                # Replace NaN or NULL values in the 'name' column with a readable default
                prox_factor$name[is.na(prox_factor$name) | prox_factor$name == ""] <- "Not Available"
                colnames(prox_factor)[colnames(prox_factor) == "name"] <- "Name"
                
                df <- st_drop_geometry(prox_factor)
                return(df)
            }
        }))
        
        DT::datatable(
            selected_data,
            options = list(
                orderClasses = TRUE,
                scrollX = TRUE,
                scrollY = "400px",
                lengthMenu = c(5, 30, 50),
                pageLength = 5
            )
        )
    })

    # ==========================================================
    # CREATE BASE MAP
    # ==========================================================
    
    filter_properties <- function(property, keep_list) {
        print("To keep:")
        print(keep_list)
        return(property %>% filter(`Property Type` %in% keep_list))
    }

    # Reactive function to perform filtering for the base map
    base_map_filter <- reactive({
        # Filter data based on checkboxes
        keep_list <- input$property_plot_property_types
        return(filter_properties(property, keep_list))
    })
    
    # Output base map
    output$base_map_plot <- renderTmap({
        property_filtered <- base_map_filter()
        if (is.null(property_filtered)) {
        tmap_options(check.and.fix =  TRUE) +
            tm_shape(study_area) + 
            tm_borders(col = "black", lwd = 1) +
            tm_text("shapeName", size = 1, col = "black", shadow = TRUE) +
            tm_view(bbox = study_area, set.zoom.limits = zoom_limits) + 
            tm_basemap("OpenStreetMap", alpha = 0.5)
            tm_layout(legend.show = FALSE, 
                title = "Map of Study Area", 
                title.size = 1.5, 
                title.position = c("left", "top"))
        }

        print(paste("Creating base map plot for currency variable:", input$property_plot_currency))
        currency <- input$property_plot_currency
        colour <- "Purples"
        if (currency == "Price_SGD") {
        colour <- "Reds"
        } else if (currency == "Price_MYR") {
        colour <- "YlOrBr"
        }
        
        map <- tmap_options(check.and.fix = TRUE) +
            tm_shape(study_area) + 
            tm_borders(col = "black", lwd = 1) +
            tm_text("shapeName", size = 1, col = "black", shadow = TRUE)
            
        map <- map + tm_shape(property_filtered) +
            tm_dots(col = input$property_plot_currency, alpha = input$property_plot_transparency, 
                    style = input$property_plot_classification, 
                    palette = colour,
                    n = input$property_plot_num_classes,  
                    popup.vars = c("Scheme Name/Area", "Road Name", "Mukim", "District",
                                    "Month, Year of Transaction Date", "Land/Parcel Area", "Main Floor Area",
                                    "Price_MYR", "Price_SGD", "Price_USD")
            ) +
            tm_view(bbox = study_area, set.zoom.limits = zoom_limits) + 
            tm_basemap("OpenStreetMap", alpha = 0.5)
            tm_layout(legend.show = FALSE, 
                title = "Map of Study Area", 
                title.size = 1.5, 
                title.position = c("left", "top"))
        map
    })
    
    # ==========================================================
    # HEX GRID PLOT
    # ==========================================================
    
    create_hex_grid <- function(property_filtered, cellsize) {
        # Create grid and filter by study area
        jb_kulai_hex <- st_make_grid(study_area, cellsize = cellsize, square = FALSE) %>% st_sf()
        jb_kulai_grid <- st_intersection(jb_kulai_hex, st_union(study_area))
        
        # Compute intersections once
        intersections <- st_intersects(jb_kulai_grid, property_filtered)
        property_prices <- lapply(intersections, function(i) property_filtered$`Price_MYR`[i])
        
        # Add metrics
        jb_kulai_grid <- jb_kulai_grid %>% 
            mutate( density = lengths(intersections), prices = property_prices) %>%
            rowwise() %>%
            mutate(
                avg_price = mean(prices, na.rm = TRUE),
                median_price = median(prices, na.rm = TRUE),
                max_price = max(prices, na.rm = TRUE)
            ) %>%
            ungroup() %>%
            drop_na()

        return(jb_kulai_grid)
    }
    
    # Button reactive function to create the hex grid
    hex_update <- eventReactive(input$hexagonal_grid_update, {
        print("Creating hex grid plot")
        property_filtered <- filter_properties(property, input$hexagonal_grid_property_types)
        return(create_hex_grid(property_filtered, c(input$hexagonal_grid_cell_size, input$hexagonal_grid_cell_size)))
    })
    
    # Output hex grid
    output$hex_grid <- renderTmap({
        jb_kulai_grid <- hex_update()
        
        # Filter out polygons with 0 or NA values in the selected price variable
        filtered_grid <- jb_kulai_grid %>%
        filter(!is.na(.data[[input$hexagonal_grid_mapping_variable]]) & .data[[input$hexagonal_grid_mapping_variable]] > 0)

        # Check if the filtered grid is empty
        if (nrow(filtered_grid) == 0) {
            print("No polygons to display.")
            return(NULL)
        }
        
        print(paste("Creating map display for", input$hexagonal_grid_mapping_variable))
        map <- tmap_options(check.and.fix = TRUE) +
            tm_shape(filtered_grid) +
            tm_fill(input$hexagonal_grid_mapping_variable,
                    n = input$hexagonal_grid_num_classes,
                    style = input$hexagonal_grid_classification,
                    palette = input$hexagonal_grid_colour,
                    alpha = input$hexagonal_grid_transparency,
                    id = input$hexagonal_grid_mapping_variable
            ) +
            tm_borders(lwd = 0.1, alpha = 1) +
            tm_view(bbox = study_area, set.zoom.limits = zoom_limits) + 
            tm_basemap("OpenStreetMap", alpha = 0.5)

        map
    })

    # ==========================================================
    # Local Measures of Spatial AutoCorrelation
    # ==========================================================
    
    localMIResults <- eventReactive(input$MoranUpdate, {
        property_temp_filtered <- filter(property, Mukim == input$mukim_lm)
        property_filtered <- filter_properties(property_temp_filtered, input$types_to_keep_lmi)
        jb_kulai_grid <- create_hex_grid(property_filtered, c(750, 750))
        print("1 #######################")
        if (nrow(jb_kulai_grid) == 0) {
        return(NULL)
        } # Exit if no data

        input_variable <- input$variable
        measured_variable <- jb_kulai_grid$median_price

        if (input_variable == "avg_price") {
        measured_variable <- jb_kulai_grid$avg_price
        } else if (input_variable == "max_price") {
        measured_variable <- jb_kulai_grid$max_price
        }
        print("2 #######################")
        # Compute distance weights
        # Grids with null values are removed - need to account for "islands"
        jb_kulai_wgs <- jb_kulai_grid %>% st_transform(4326)
        longitude <- map_dbl(jb_kulai_wgs$geometry, ~st_centroid(.x)[[1]])
        latitude <- map_dbl(jb_kulai_wgs$geometry, ~st_centroid(.x)[[2]])
        coords <- cbind(longitude, latitude)
        print("3 #######################")

        # Determine the cutoff distance
        k1 <- knn2nb(knearneigh(coords))
        k1dists <- unlist(nbdists(k1, coords, longlat = TRUE))
        print(summary(k1dists))
        print("4 #######################")

        # Compute adaptive distance weight matrix
        max_distance <- max(k1dists)
        nb <- dnearneigh(coords, 0, max_distance, longlat=TRUE)
        distances <- nbdists(nb, coords)
        rswm_q <- nb2listw(nb, glist = distances, style = "W")
        print("5 #######################")

        # Compute local Moran's I
        print(paste("Deriving local Moran's I for", input_variable))
        print("before fips #######################")
        # fips <- order(jb_kulai_grid$index)
        print("after fips #######################")
        localMI <- localmoran(measured_variable, rswm_q)
        
        # Merge into dataset
        lisa <- cbind(jb_kulai_grid, localMI) %>%
        rename(
            "local moran(ii)" = "Ii",
            "expectation(eii)" = "E.Ii",
            "variance(var_ii)" = "Var.Ii",
            "std deviation(z_ii)" = "Z.Ii",
            "p_value" = "Pr.z....E.Ii.."
        )
        
        quadrant <- vector(mode = "numeric", length = nrow(localMI))
        lisa$lag <- lag.listw(rswm_q, measured_variable)
        DV <- lisa$lag - mean(measured_variable)
        
        print(paste("Lisa Class:", input$LisaClass))
        if (input$LisaClass == "median") {
        LM_I <- localMI[,1] - median(localMI[, 1])
        } else {
        LM_I <- localMI[,1] - mean(localMI[, 1])
        }
        
        signif <- input$MoranConf
        quadrant[DV <0 & LM_I>0] <- 1
        quadrant[DV >0 & LM_I<0] <- 2
        quadrant[DV <0 & LM_I<0] <- 3  
        quadrant[DV >0 & LM_I>0] <- 4
        quadrant[localMI[,5]>signif] <- 0
        lisa$quadrant_class <- quadrant
        return(lisa)
    })
    
    # Render local Moran I statistics
    output$LocalMoranMap <- renderTmap({
        df <- localMIResults()
        
        if (is.null(df) || nrow(df) == 0) {
        return()
        }
        
        # Map creation using tmap
        print("Creating local Moran's I map")
        localMI_map <- tm_shape(df) +
        tm_fill(
            col = input$localmoranstats,
            style = "pretty",
            palette = "RdBu",
            title = input$localmoranstats,
            alpha = input$opacity
        ) +
        tm_borders() +
        tm_view(bbox = study_area, set.zoom.limits = zoom_limits) 
        
        localMI_map
    })
    
    # Render LISA map
    output$LISA <- renderTmap({
        print("Creating LISA cluster map")
        df <- localMIResults()
        if (is.null(df)) {
        return()
        }
        print(names(df))
        colors <- c("#ffffff", "#2c7bb6", "#fdae61", "#abd9e9", "#d7191c")
        clusters <- c("insignificant", "low-low", "low-high", "high-low", "high-high")
        lisamap <- tm_shape(df) +
        tm_fill(
            col = "quadrant_class",
            style = "cat", 
            palette = colors[c(sort(unique(df$quadrant_class)))+1], 
            labels = clusters[c(sort(unique(df$quadrant_class)))+1],
            title = (paste("Quadrant")),
            alpha = input$opacity
        ) +
        tm_borders() +
        tm_view(bbox = study_area, set.zoom.limits = zoom_limits) 
        
        lisamap
    })

    # ==========================================================
    # Hot/Cold Spot Analysis
    # ==========================================================
    
    gi_statistics_results <- eventReactive(input$GiUpdate, {
        property_temp_filtered <- filter(property, Mukim == input$mukim_gi)
        property_filtered <- filter_properties(property_temp_filtered, input$types_to_keep_lgi)
        jb_kulai_grid <- create_hex_grid(property_filtered, c(750, 750))

        if (nrow(jb_kulai_grid) == 0) {
            print("GRID OUTPUT IS NULL.")
            return(NULL)
        }

        # Compute distance weights
        # Grids with null values are removed - need to account for "islands"
        jb_kulai_wgs <- jb_kulai_grid %>% st_transform(4326)
        longitude <- map_dbl(jb_kulai_wgs$geometry, ~st_centroid(.x)[[1]])
        latitude <- map_dbl(jb_kulai_wgs$geometry, ~st_centroid(.x)[[2]])
        coords <- cbind(longitude, latitude)
        print("Successfully created coordinates")

        k <- input$k
        nb_style <-input$GiWeights
        use_adaptive_bandwidth <- as.logical(input$BandwidthType)

        if (use_adaptive_bandwidth) {
            knn <- knn2nb(knearneigh(coords, k = k))
            lw <- nb2listw(knn, style = nb_style)
        } else {
            # Compute fixed distance weight matrix
            k1 <- knn2nb(knearneigh(coords))
            k1dists <- unlist(nbdists(k1, coords, longlat = TRUE))
            wm_d62 <- dnearneigh(coords, 0, max(k1dists), longlat = TRUE)
            lw <- nb2listw(wm_d62, style = nb_style)
        }

        input_variable <- input$variable
        measured_variable <- jb_kulai_grid$median_price
        if (input_variable == "avg_price") {
            measured_variable <- jb_kulai_grid$avg_price
        } else if (input_variable == "max_price") {
            measured_variable <- jb_kulai_grid$max_price
        }
        
        print(paste("Performing local Gi for", input_variable))
        gi <- localG(measured_variable, lw)
        jb_kulai_wgs <- jb_kulai_grid %>% st_transform(4326)
        jb_kulai.gi <- cbind(jb_kulai_wgs, as.matrix(gi)) %>% rename(gstat = as.matrix.gi.)
        return(jb_kulai.gi)
    })
    
    # Render hot/cold spot map
    output$Gi <- renderTmap({
        gi <- gi_statistics_results()
        if (is.null(gi)) {
        return()
        }
        print("Creating hot/cold spot map")
        gi_map <- tm_shape(gi) +
        tm_fill(
            col = "gstat",
            palette = "-RdBu",
            title = "Local Gi",
            breaks = seq(from = -10, to = 10, by = 2),
            alpha = input$opacity
        ) +
        tm_borders(alpha = 0.5) +
        tm_view(bbox = study_area, set.zoom.limits = zoom_limits) 
        gi_map
    })
}

# Run the application
shinyApp(ui = ui, server = server)
