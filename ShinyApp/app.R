#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

pacman::p_load(shiny, bslib, shinydashboard, shinythemes, rsconnect, olsrr, ggstatsplot, sf, tmap, tidyverse, gtsummary, performance, see, sfdep, spdep, tidygeocoder, RColorBrewer)

#load rds
study_area <- read_rds("data/study_area.rds")
amenities_list <- read_rds("data/amenities.rds")
property <- read_rds("data/property.rds")


zoom_limits <- c(11, 15)
boundary_box = c(103.4, 1.5, 104, 1.6)
default_num_classes <- 6

large_map_size <- 720
med_map_size <- 640
adj_map_size <- 560
default_alpha <- 0.9

property_type_names <- c(
  "1 - 1 1/2 Storey Semi-Detached",
  "1 - 1 1/2 Storey Terraced",
  "2 - 2 1/2 Storey Semi-Detached",
  "2 - 2 1/2 Storey Terraced",
  "Cluster House",
  "Condominium",
  "Detached",
  "Townhouse"
)

property_types <- c(
  "1 - 1 1/2 Storey Semi-Detached",
  "1 - 1 1/2 Storey Terraced",
  "2 - 2 1/2 Storey Semi-Detached",
  "2 - 2 1/2 Storey Terraced",
  "Cluster House",
  "Condominium/Apartment",
  "Detached",
  "Town House"
)

# ========================#
###### Shiny UI ######
# ========================#

ui <- navbarPage(
  title = "JB: The New Frontier?",
  fluid = TRUE,
  theme = shinytheme("flatly"),
  id = "navbarID",
  navbarMenu("Exploratory Spatial Data Analysis",
   tabPanel("Proximity Factors Mapping",
            sidebarLayout(
              sidebarPanel(
                h4("Control whether to show certain feature types."),
                HTML("<hr>"),
                checkboxInput("show_points", label = "Show Points", value = TRUE),
                checkboxInput("show_lines", label = "Show Lines", value = TRUE),
                checkboxInput("show_polygons", label = "Show Polygons", value = FALSE),
                checkboxInput("cluster_points", label = "Enable Point Clustering*", value = TRUE),
                "*once enabled, points that appear too close will be clustered together.",
                br(),
                br(),
                
                # Grouping amenities into categories
                h4("Select Proximity Factors"),
                br(),
                
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
                ),
                
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
                ),
                
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
              ),
              
              mainPanel(
                h2("Proximity Factors Mapping"),
                strong("Click on the checkboxes on the left to view key amenities or features"),
                br(),
                br(),
                tmapOutput("amenity_map", width = "100%", height = "1100px")
              )
            )
   )
  ),
  tabPanel(
    "Base Map",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          inputId = "types_to_keep_base",
          label = "Include: ",
          choiceNames = property_type_names,
          choiceValues = property_types,
          selected = property_types
        ),
        selectInput(
          inputId = "currency",
          label = "Currency",
          choices = list(
            "MYR" = "Price_MYR",
            "SGD" = "Price_SGD",
            "USD" = "Price_USD"
          ),
          selected = "Price_MYR"
        ),
        sliderInput(
          inputId = "classes",
          label = "Number of classes",
          min = 2,
          max = 20,
          value = c(default_num_classes)
        ),
        selectInput(
          inputId = "classification",
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
          inputId = "opacity",
          label = "Level of transparency",
          min = 0,
          max = 1,
          value = c(default_alpha)
        )
      ),
      mainPanel(
        p(
          "Map of all property transactions in JB and Kulai between 2023 and 2024. Hover over a point to reveal its details."
        ),
        tmapOutput("base_map_plot",
                   width = "100%",
                   height = large_map_size
        )
      )
    )
  ),
  tabPanel(
    "Hexagon Grid",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          inputId = "types_to_keep_hex",
          label = "Include: ",
          choiceNames = property_type_names,
          choiceValues = property_types,
          selected = property_types
        ),
        selectInput(
          inputId = "variable",
          label = "Mapping variable",
          choices = list(
            "Density" = "density",
            "Average Property Prices" = "avg_price",
            "Median Property Prices" = "median_price",
            "Maximum Property Prices" = "max_price"
          ),
          selected = "median_price"
        ),
        selectInput(
          inputId = "classification",
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
          inputId = "classes",
          label = "Number of classes",
          min = 2,
          max = 20,
          value = c(default_num_classes)
          # ),
          # selectInput(
          #     inputId = "colour",
          #     label = "Colour scheme:",
          #     choices = list(
          #         "blues" = "Blues",
          #         "reds" = "Reds",
          #         "greens" = "Greens",
          #         "Yellow-Orange-Red" = "YlOrRd",
          #         "Yellow-Orange-Brown" = "YlOrBr",
          #         "Yellow-Green" = "YlGn",
          #         "Orange-Red" = "OrRd"
          #     ),
          #     selected = "YlOrRd"
        ),
        sliderInput(
          inputId = "opacity",
          label = "Level of transparency",
          min = 0,
          max = 1,
          value = c(default_alpha)
        ),
        actionButton("HexUpdate", "Update Plot"),
        hr()
      ),
      mainPanel(
        p("Map of the hexagonal grid plotted over JB and Kulai. Select a price variable to plot or customise mapping parameters on the left panel, then click 'Update Plot' to begin."),
        tmapOutput("hex_grid",
                   width = "100%",
                   height = large_map_size
        )
      )
    )
  ),
  tabPanel(
    "Local Moran",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          inputId = "types_to_keep_lmi",
          label = "Include: ",
          choiceNames = property_type_names,
          choiceValues = property_types,
          selected = property_types
        ),
        selectInput(
          inputId = "mukim_lm",
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
        actionButton("MoranUpdate", "Update Plot"),
        hr(),
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
        selectInput("LisaClass", "Lisa Classification",
                    choices = c(
                      "mean" = "mean",
                      "median" = "median"
                    ),
                    selected = "mean"
        ),
        selectInput("localmoranstats", "Output variable",
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
          value = c(default_alpha)
        )
      ),
      mainPanel(
        p("The selected output variable map will be displayed on the left, and the LISA class map on the right. Customise the parameters in the left panel, then click 'Update Plot' to begin."),
        fluidRow(
          column(6, tmapOutput("LocalMoranMap")),
          column(6, tmapOutput("LISA"))
        ) # Maximum total width is 12
        # Use 6 and 6 to define equal distance
        # Can have a map with a statistical output
      )
    )
  ),
  tabPanel(
    "Local Gi",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          inputId = "types_to_keep_lgi",
          label = "Include: ",
          choiceNames = property_type_names,
          choiceValues = property_types,
          selected = property_types
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
        selectInput("GiWeights", "Spatial Weights Style",
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
        hr(),
        sliderInput(
          inputId = "opacity",
          label = "Level of transparency",
          min = 0,
          max = 1,
          value = c(default_alpha)
        )
      ),
      mainPanel(
        p("The hot/cold spot map will be displayed on the left, and the price map on the right. Customise the parameters in the left panel, then click 'Update Plot' to begin."),
        tmapOutput(
          "Gi",
          width = "100%",
          height = large_map_size
        ) # Maximum total width is 12
        # Use 6 and 6 to define equal distance
        # Can have a map with a statistical output
      )
    )
  )
)


# ========================#
###### Shiny Server ######
# ========================#

server <- function(input, output) {
  # Define a maximum limit for the checkboxes (e.g., 5)
  max_selected <- 5
  
  observe({
    
    # Loop through the selected amenities
    input_prox_factor <- selected_amenities <- c(input$gov_facilities_coastlines, 
                                                 input$education, 
                                                 input$gov_buildings, 
                                                 input$land_use_commercial_industrial, 
                                                 input$recreation_nature, 
                                                 input$religious_places, 
                                                 input$commercial_services, 
                                                 input$transport_infrastructure)
    
    # Check if the number of selected amenities exceeds the max limit
    if (length(input_prox_factor) > max_selected) {
      # If exceeded, show a warning and revert the last change
      showModal(modalDialog(
        title = "Limit Exceeded",
        paste("You can only select up to", max_selected, "proximity factors at once."),
        easyClose = TRUE,
        footer = NULL
      ))
      
      # Revert the selection to the previous valid set
      updateCheckboxGroupInput(session, "prox_factor", selected = head(input_prox_factor, max_selected))
    }
  })
  
  
  #==========================================================
  # Exploratory Spatial Data Analysis (Amenity Mapping)
  #==========================================================
  
  # Helper Function
  base_map <- tm_shape(study_area) + 
    tm_borders(col = "red", lwd = 1) +
    tm_text("shapeName", size = 1, col = "red", shadow = TRUE) +
    tm_view(bbox = boundary_box, set.zoom.limits = zoom_limits) +
    tm_layout(legend.show = FALSE, 
              title = "Map of Study Area", 
              title.size = 1.5, 
              title.position = c("left", "top"))
  
  output$amenity_map <- renderTmap({
    # req(input$prox_factor)
    
    tmap_options(max.categories = 400)
    
    # Initialize the base map
    map <- base_map
    
    # Loop through the selected amenities
    input_prox_factor <- selected_amenities <- c(input$gov_facilities_coastlines, 
                                                 input$education, 
                                                 input$gov_buildings, 
                                                 input$land_use_commercial_industrial, 
                                                 input$recreation_nature, 
                                                 input$religious_places, 
                                                 input$commercial_services, 
                                                 input$transport_infrastructure)
    
    selected_amenities <- amenities_list[input_prox_factor]
    
    # Handle visibility of points, lines, and polygons
    for (amenity_name in names(selected_amenities)) {
      
      tmap_options(max.categories = 400)
      
      Amenity <- selected_amenities[[amenity_name]]
      
      if (inherits(Amenity, "sf")) {
        geom_type <- unique(st_geometry_type(Amenity))
        
        # Plot points
        if ("POINT" %in% geom_type && input$show_points) {
          map <- map + 
            tm_shape(Amenity) +
            tm_dots(col = "blue", 
                    size = 0.2, 
                    cluster = input$cluster_points,  # Cluster based on user input
                    alpha = 0.5, 
                    group = "Markers")
        }
        
        # Plot lines
        if (("LINESTRING" %in% geom_type || "MULTILINESTRING" %in% geom_type) && input$show_lines) {
          map <- map + 
            tm_shape(Amenity) + 
            tm_lines(col = "purple", group = "Lines")
        }
        
        # Plot polygons
        if (("POLYGON" %in% geom_type || "MULTIPOLYGON" %in% geom_type) && input$show_polygons) {
          map <- map + tm_shape(Amenity) +
            tm_borders(col = "green") +
            tm_fill("name", legend.show = FALSE, group = "Polygons")
        }
      }
    }
    map
  })
  
  # ==========================================================
  # Filter properties not in the keep list
  # ==========================================================
  filter_properties <- function(property, keep_list) {
    print("To keep:")
    print(keep_list)
    return(property %>% filter(`Property Type` %in% keep_list))
  }
  
  # ==========================================================
  # Create base (points) map
  # ==========================================================
  
  # Reactive function to perform filtering for the base map
  base_map_filter <- reactive({
    # Filter data based on checkboxes
    keep_list <- input$types_to_keep_base
    return(filter_properties(property, keep_list))
  })
  
  # Output base map
  output$base_map_plot <- renderTmap({
    property_filtered <- base_map_filter()
    if (is.null(property_filtered)) {
      tmap_options(check.and.fix =  TRUE) +
        tm_shape(study_area) +
        tm_view(
          bbox = boundary_box, set.zoom.limits = zoom_limits
        ) 
      return()
    }
    print(paste("Creating base map plot for currency variable:", input$currency))
    currency <- input$currency
    colour <- "Purples"
    if (currency == "Price_SGD") {
      colour <- "Reds"
    } else if (currency == "Price_MYR") {
      colour <- "YlOrBr"
    }
    
    tmap_options(check.and.fix = TRUE) +
      tm_shape(study_area) +
      tm_polygons(alpha = 0.4) +
      tm_shape(property_filtered) +
      tm_dots(
        col = input$currency,
        alpha = 0.6,
        style = input$classification,
        palette = colour,
        n = input$classes,
        popup.vars = c(
          "Scheme Name/Area", "Road Name", "Mukim", "District",
          "Month, Year of Transaction Date", "Land/Parcel Area", "Main Floor Area",
          "Price_MYR", "Price_SGD", "Price_USD"
        )
      ) +
      tm_view(
        bbox = boundary_box, set.zoom.limits = zoom_limits
      ) 
  })
  
  # ==========================================================
  # Create hex grid
  # ==========================================================
  
  # Function to create the hex grid
  create_hex_grid <- function(property_filtered, cellsize) {
    jb_kulai_hex <- st_make_grid(study_area, cellsize = cellsize, what = "polygons", square = FALSE) %>%
      st_sf() %>%
      mutate(index = as.factor(row_number()))
    jb_kulai_border <- study_area %>% st_union()
    jb_kulai_grid <- st_intersection(jb_kulai_hex, jb_kulai_border)
    
    # Check if hex grid intersects any polygons using st_intersects
    # Returns a list of intersecting hexagons
    intersection_list = jb_kulai_hex$index[lengths(st_intersects(jb_kulai_hex, jb_kulai_grid)) > 0]
    
    # Filter for the intersecting hexagons
    jb_kulai_grid <- jb_kulai_hex %>%
      filter(index %in% intersection_list)
    
    # Associate with Mukims
    joined <- st_join(jb_kulai_hex, study_area, join = st_intersects, left = FALSE)
    aggregated <- joined %>%
      group_by(index) %>%
      summarise(`Mukim` = first(`shapeName`))
    
    jb_kulai_grid$Mukim <- aggregated$Mukim
    jb_kulai_grid <- jb_kulai_grid %>%
      mutate(index = as.factor(row_number()))
    
    jb_kulai_grid <- jb_kulai_grid[, c("Mukim", setdiff(names(jb_kulai_grid), "Mukim"))]
    
    # Save density
    intersections <- st_intersects(jb_kulai_grid, property_filtered)
    jb_kulai_grid$density <- lengths(intersections)
    
    # Extract aggregated property prices
    aggregate_price <- function(i, method = "median") {
      if (length(i) == 0 || all(is.na(property_filtered$`Price_USD`[i]))) return(NA)
      
      if (method == "mean") {
        return(mean(property_filtered$`Price_USD`[i], na.rm = TRUE))
      } else if (method == "max") {
        return(mean(property_filtered$`Price_USD`[i], na.rm = TRUE))
      } else if (method == "median") {
        return(median(property_filtered$`Price_USD`[i], na.rm = TRUE))
      } else {
        stop("Invalid method: Choose either 'mean' or 'max'.")
      }
    }
    
    avg_prices <- sapply(intersections, aggregate_price, method = "mean")
    jb_kulai_grid$avg_price <- avg_prices
    
    median_prices <- sapply(intersections, aggregate_price, method = "median")
    jb_kulai_grid$median_price <- median_prices
    
    max_prices <- sapply(intersections, aggregate_price, method = "max")
    jb_kulai_grid$max_price <- max_prices
    
    # Drop NA
    jb_kulai_grid <- jb_kulai_grid %>% drop_na()
    return(jb_kulai_grid)
  }
  
  # Button reactive function to create the hex grid
  hex_update <- eventReactive(input$HexUpdate, {
    print("Creating hex grid plot")
    property_filtered <- filter_properties(property, input$types_to_keep_hex)
    return(create_hex_grid(property_filtered, c(750, 750)))
  })
  
  # Output hex grid
  hex_grid <- renderTmap({
    jb_kulai_grid <- hex_update()
    if (is.null(jb_kulai_grid) || nrow(jb_kulai_grid) == 0) {
      print("Map is empty. Stopping")
      return()
    }
    
    colour <- "Greens"
    input_variable <- input$variable
    if (input_variable == "avg_price") {
      colour <- "Purples"
    } else if (input_variable == "max_price") {
      colour <- "YlOrRd"
    } else if (input_variable == "density") {
      colour <- "Greys"
    }
    
    print(paste("Creating map displaying", input_variable))
    tmap_options(check.and.fix = TRUE) +
      tm_shape(jb_kulai_grid) +
      tm_fill(input$variable,
              n = input$classes,
              style = input$classification,
              palette = colour,
              alpha = input$opacity
      ) +
      tm_borders(lwd = 0.1, alpha = 1) +
      tm_view(
        bbox = boundary_box, set.zoom.limits = zoom_limits
      ) 
  })
  
  output$hex_grid <- hex_grid
  output$hex_grid_2 <- hex_grid
  
  # ==========================================================
  # Local Measures of Spatial AutoCorrelation
  # ==========================================================
  
  localMIResults <- eventReactive(input$MoranUpdate, {
    property_temp_filtered <- filter(property, Mukim == input$mukim_lm)
    property_filtered <- filter_properties(property_temp_filtered, input$types_to_keep_lmi)
    jb_kulai_grid <- create_hex_grid(property_filtered, c(750, 750))
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
    
    # Compute distance weights
    # Grids with null values are removed - need to account for "islands"
    jb_kulai_wgs <- jb_kulai_grid %>% st_transform(4326)
    longitude <- map_dbl(jb_kulai_wgs$geometry, ~st_centroid(.x)[[1]])
    latitude <- map_dbl(jb_kulai_wgs$geometry, ~st_centroid(.x)[[2]])
    coords <- cbind(longitude, latitude)
    # Determine the cutoff distance
    k1 <- knn2nb(knearneigh(coords))
    k1dists <- unlist(nbdists(k1, coords, longlat = TRUE))
    print(summary(k1dists))
    
    # Compute adaptive distance weight matrix
    max_distance <- max(k1dists)
    nb <- dnearneigh(coords, 0, max_distance, longlat=TRUE)
    distances <- nbdists(nb, coords)
    rswm_q <- nb2listw(nb, glist = distances, style = "W")
    
    # Compute local Moran's I
    print(paste("Deriving local Moran's I for", input_variable))
    fips <- order(jb_kulai_grid$index)
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
    } # Exit if no data
    
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
      tm_view(bbox = boundary_box, set.zoom.limits = zoom_limits) 
    
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
      tm_view(bbox = boundary_box, set.zoom.limits = zoom_limits) 
    
    lisamap
  })
  
  # ==========================================================
  # Hot/Cold Spot Analysis
  # ==========================================================
  
  gi_statistics_results <- eventReactive(input$GiUpdate, {
    property_temp_filtered <- filter(property, Mukim == input$mukim_lm)
    property_filtered <- filter_properties(property_temp_filtered, input$types_to_keep_hex)
    jb_kulai_grid <- create_hex_grid(property_filtered, c(750, 750))
    if (nrow(jb_kulai_grid) == 0) {
      return(NULL)
    } # Exit if no data
    jb_kulai_wgs <- jb_kulai_grid %>% st_transform(4326)
    longitude <- map_dbl(jb_kulai_wgs$geometry, ~ st_centroid(.x)[[1]])
    latitude <- map_dbl(jb_kulai_wgs$geometry, ~ st_centroid(.x)[[2]])
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
      tm_view(bbox = boundary_box, set.zoom.limits = zoom_limits) 
    gi_map
  })
}

# Run the application
shinyApp(ui = ui, server = server)