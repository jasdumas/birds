library(shiny)
library(bslib)
library(htmltools)
library(leaflet)
library(bsicons)
library(readr)
library(dplyr)
library(purrr)
library(stringr)

source("curated_data_prep.R", local = TRUE)

# Wave header (inverted)
wave_header <- div(
  style = "height: 90px; overflow: hidden; transform: rotate(180deg);",
  
  tags$svg(
    viewBox = "0 0 1200 80",
    preserveAspectRatio = "none",
    style = "height: 100%; width: 200%; position: absolute; animation: waveSlide 8s linear infinite;",
    tags$path(
      d = "M0,30 C300,80 900,0 1200,50 L1200,80 L0,80 Z",
      fill = "#FFF9F0"
    )
  ),
  
  # Birds - multiple layers of flight
  tags$div(
    style = "position: absolute; top: 10px; left: 0; width: 100%; height: 100%; pointer-events: none;",
    # First bird
    tags$svg(
      viewBox = "0 0 24 24", fill = "none",
      style = "width: 20px; height: 20px; position: absolute; animation: birdFly 10s linear infinite; transform: rotate(180deg);",
      tags$path(d = "M2,12 Q6,2 12,12 Q18,2 22,12", stroke = "#6D6A75", fill = "none")
    ),
    # Second bird
    tags$svg(
      viewBox = "0 0 24 24", fill = "none",
      style = "width: 15px; height: 15px; position: absolute; animation: birdFly 12s linear infinite; animation-delay: 2s; top: 20px; transform: rotate(180deg);",
      tags$path(d = "M2,12 Q6,2 12,12 Q18,2 22,12", stroke = "#6D6A75", fill = "none")
    ),
    # Third bird
    tags$svg(
      viewBox = "0 0 24 24", fill = "none",
      style = "width: 18px; height: 18px; position: absolute; animation: birdFly 14s linear infinite; animation-delay: 4s; top: 5px; transform: rotate(180deg);",
      tags$path(d = "M2,12 Q6,2 12,12 Q18,2 22,12", stroke = "#6D6A75", fill = "none")
    )
  ),
  
  tags$style(HTML("
    @keyframes waveSlide {
      0% { transform: translateX(0); }
      100% { transform: translateX(-50%); }
    }
    @keyframes birdFly {
      0% { left: -30px; opacity: 0; }
      10% { opacity: 1; }
      90% { opacity: 1; }
      100% { left: 100%; opacity: 0; }
    }
  "))
)


# Card with inverted wave as header
full_width_with_wave_header <- div(
  style = "width: 100%; overflow: hidden; background-color: #AECBED;",  # Soft blue background
  wave_header,
  div(
    style = "padding: 10px 20px; background-color: #AECBED; text-align: center;",  # Soft blue background and centered text
    h2("Connecticut Priority Bird Species"),
    p("Conservation efforts focused on these high-priority bird species help 
       protect and enhance habitats that benefit other wildlife and people across Connecticut. 
       This is an demo Shiny app showcasing example priority bird species. 
      Illustrations & inspiration are from: ", a(
        href = "https://ct.audubon.org/priority-bird-species",  
        target = "_blank",
        "Audubon Priority Birds in Connecticut", 
        style = "text-decoration: none; text-align: center; "
      ) )
    
  ) 
  
)


# Sample bird data with added population counts
# birds <- list(
#   list(
#     name = "American Oystercatcher",
#     scientific_name = "Haematopus palliatus",
#     status = "Species of Concern",
#     habitat = "Coastal, Shoreline",
#     population = "11,500",
#     image = "https://nas-national-prod.s3.amazonaws.com/styles/api_bird_illustration_with_crop_193_200/public/birds/illustration/2820_Sibl_9780307957900_art_r1.jpg",
#     range_center = list(lat = 41.2, lng = -72.5),
#     range_radius = 50000, # meters
#     description = "Found along the Atlantic coast of Connecticut. These birds nest on beaches and feed on shellfish in tidal areas."
#   ),
#   list(
#     name = "Saltmarsh Sparrow",
#     scientific_name = "Ammospiza caudacuta",
#     status = "Endangered",
#     habitat = "Saltmarsh, Coastal",
#     population = "53,000",
#     image = "https://nas-national-prod.s3.amazonaws.com/styles/api_bird_illustration_with_crop_193_200/public/birds/illustration/6519_Sibl_9780307957900_art_r1.jpg",
#     range_center = list(lat = 41.3, lng = -72.3),
#     range_radius = 30000, # meters
#     description = "Limited to salt marshes along Connecticut's coast. Highly vulnerable to sea level rise and habitat loss."
#   ),
#   list(
#     name = "Piping Plover",
#     scientific_name = "Charadrius melodus",
#     status = "Threatened",
#     habitat = "Beach, Coastal",
#     population = "8,400",
#     image = "https://nas-national-prod.s3.amazonaws.com/styles/api_bird_illustration_with_crop_193_200/public/birds/illustration/1256_Sibl_9780307957900_art_r1.jpg",
#     range_center = list(lat = 41.25, lng = -72.8),
#     range_radius = 45000, # meters
#     description = "Nests on sandy beaches along Connecticut's shoreline. Protection efforts include fencing off nesting areas during breeding season."
#   ),
#   list(
#     name = "American Woodcock",
#     scientific_name = "Scolopax minor",
#     status = "Special Concern",
#     habitat = "Forest, Wetland",
#     population = "3,500,000",
#     image = "https://nas-national-prod.s3.amazonaws.com/styles/api_bird_illustration_with_crop_193_200/public/birds/illustration/5207_Sibl_9780307957900_art_r1.jpg",
#     range_center = list(lat = 41.6, lng = -72.7),
#     range_radius = 70000, # meters
#     description = "Found in young forests and shrublands throughout Connecticut. Known for its unique 'sky dance' courtship display at dusk."
#   ),
#   list(
#     name = "Common Tern",
#     scientific_name = "Sterna hirundo",
#     status = "Special Concern",
#     habitat = "Coastal, Islands",
#     population = "590,000",
#     image = "https://nas-national-prod.s3.amazonaws.com/styles/api_bird_illustration_with_crop_193_200/public/birds/illustration/6600_Sibl_9780307957900_art_r1.jpg",
#     range_center = list(lat = 41.3, lng = -72.1),
#     range_radius = 60000, # meters
#     description = "Nests on islands and coastal areas. Conservation efforts include creating and maintaining artificial nesting sites."
#   ),
#   list(
#     name = "Wood Thrush",
#     scientific_name = "Hylocichla mustelina",
#     status = "Vulnerable",
#     habitat = "Forest, Woodland",
#     population = "12,000,000",
#     image = "https://nas-national-prod.s3.amazonaws.com/styles/api_bird_illustration_with_crop_193_200/public/birds/illustration/4557_Sibl_9780307957900_art_r1.jpg",
#     range_center = list(lat = 41.7, lng = -72.5),
#     range_radius = 80000, # meters
#     description = "Found in mature forests throughout Connecticut. Known for its flute-like song that echoes through the woods."
#   )
# )

# Extract unique values for filters
extract_unique_values <- function(birds, field) {
  unique_values <- c()
  for (bird in birds) {
    # Split values by comma if there are multiple values
    values <- unlist(strsplit(bird[[field]], ",\\s*"))
    unique_values <- c(unique_values, values)
  }
  return(sort(unique(unique_values)))
}

unique_habitats <- extract_unique_values(birds, "habitat")
unique_statuses <- extract_unique_values(birds, "status")

# Create a card for each bird
create_bird_card <- function(bird, index) {  # Add index parameter
  card(
    height = 450,
    width = "100%",
    class = "bird-card",  # Add this class
    id = paste0("bird-card-", index),  # Add this ID
    card_header(h4(bird$name), class = "text-center"),
    card_image(file = bird$image, fill = TRUE, height = "200px", width = "100%"),
    card_body(
      h6(em(bird$scientific_name)),
      tags$p(
        tags$strong("Status: "), bird$status
      ),
      tags$p(
        tags$strong("Habitat: "), bird$habitat
      ),
      # tags$p(
      #   tags$strong("Est. Population: "), bird$population, " birds"
      # )
      tags$p(
        tags$strong("Est. Population: "), 
        format(bird$population, scientific = FALSE, big.mark = ","), 
        " birds"
      )
    ),
    # Add click handler for map interaction
    tags$script(HTML(sprintf(
      "$('#bird-card-%s').on('click', function() { Shiny.setInputValue('selected_bird', %s); });",
      index, index
    )))
  )
}

# Create a carousel with the bird cards
create_carousel <- function(birds) {
  carousel_id <- "birdCarousel"
  
  # Handle case when no birds match the filter
  if (length(birds) == 0) {
    return(
      div(
        class = "alert alert-info",
        "No birds match the selected filters. Please adjust your selection."
      )
    )
  }
  
  # Create carousel items
  carousel_items <- lapply(seq_along(birds), function(i) {
    bird <- birds[[i]]
    active_class <- if (i == 1) "active" else ""
    
    tags$div(
      class = paste("carousel-item", active_class),
      style = "padding: 0 10px;",
      div(
        class = "d-flex justify-content-center",
        create_bird_card(bird, i)  # Pass the index here
      )
    )
  })
  
  # Create indicators
  indicators <- lapply(seq_along(birds), function(i) {
    active_class <- if (i == 1) "active" else ""
    aria_current <- if (i == 1) "true" else "false"
    
    tags$button(
      type = "button",
      `data-bs-target` = paste0("#", carousel_id),
      `data-bs-slide-to` = i-1,
      class = active_class,
      `aria-current` = aria_current,
      `aria-label` = paste("Slide", i)
    )
  })
  
  # Assemble the carousel
  tags$div(
    class = "carousel slide",
    id = carousel_id,
    `data-bs-ride` = "false",
    
    # Indicators
    tags$div(
      class = "carousel-indicators",
      indicators
    ),
    
    # Items container
    tags$div(
      class = "carousel-inner",
      carousel_items
    ),
    
    # Controls
    tags$button(
      class = "carousel-control-prev",
      type = "button",
      `data-bs-target` = paste0("#", carousel_id),
      `data-bs-slide` = "prev",
      tags$span(class = "carousel-control-prev-icon", `aria-hidden` = "true"),
      tags$span(class = "visually-hidden", "Previous")
    ),
    tags$button(
      class = "carousel-control-next",
      type = "button",
      `data-bs-target` = paste0("#", carousel_id),
      `data-bs-slide` = "next",
      tags$span(class = "carousel-control-next-icon", `aria-hidden` = "true"),
      tags$span(class = "visually-hidden", "Next")
    )
  )
}

# UI with sidebar for filters
ui <- page_sidebar(
  #title = "🦉Connecticut Priority Bird Species",
  window_title = "🦉Connecticut Priority Bird Species",
  title = NULL,
  theme = bs_theme(brand = TRUE),
  fillable = FALSE,
  
  sidebar = sidebar(
    title = "Filter Birds",
    
    #card(
      card_image(
        src = "sunset.png",  # Path to the image inside www/ folder
        fill = TRUE,
        height = "auto", 
        style = "width: 50px; height: auto; display: block; margin: 0 auto;"  # Set a smaller width (adjust as needed)
      ),
      card_body(
        a(
          href = "https://www.dalyanalytics.com",  # Replace with actual URL
          target = "_blank",
          "Developed by Daly Analytics", 
          style = "text-decoration: none; text-align: center; "
        )
      ),

    # Habitat filter
    card(
      card_header("Habitat", class = "text-center"),
      checkboxGroupInput(
        "habitat_filter",
        label = NULL,
        choices = unique_habitats,
        selected = unique_habitats
      )
    ),
    
    # Status filter
    card(
      card_header("Conservation Status", class = "text-center"),
      checkboxGroupInput(
        "status_filter",
        label = NULL,
        choices = unique_statuses,
        selected = unique_statuses
      )
    ),
    
    # Reset button
    actionButton("reset_filters", "Reset Filters", class = "btn-danger w-100 mt-3")
  ),

  
  #card_with_wave_header, 
  full_width_with_wave_header,
  
  layout_columns(
    # Bird carousel (will be filled by server)
    uiOutput("filtered_carousel"),
    
    card(
      card_header(
        div(
          class = "d-flex align-items-center justify-content-center",
          h4("Bird Range & Habitat Map", class = "mb-0 me-2"),
          tooltip(
            bs_icon("info-circle"),
            "Click on a bird card to view its range on the map.",
            placement = "top"
          )
        )
      ),
      card_body(
        div(
          style = "height: 500px;",
          leafletOutput("range_map", height = "100%")
        ) #,
        # div(
        #   class = "mt-3",
        #   h4(textOutput("selected_bird_title")),
        #   p(textOutput("selected_bird_description"))
        # )
      )
    )
  ),
  
  
  # Custom CSS to make carousel arrows more visible
  tags$style(HTML("
    .carousel-control-prev-icon, .carousel-control-next-icon {
      background-color: rgba(0,0,0,0.3);
      border-radius: 50%;
      padding: 10px;
    }
    .carousel-control-prev {
      left: -25px;
    }
    .carousel-control-next {
      right: -25px;
    }
    .carousel-item {
      transition: transform 0.6s ease-in-out;
    }
    .carousel-indicators {
      bottom: -40px;
    }
    .carousel-indicators button {
      background-color: #888 !important;
    }
    .bird-card {
    cursor: pointer;
    transition: transform 0.2s, box-shadow 0.2s;
  }
  .bird-card:hover {
    transform: translateY(-5px);
    box-shadow: 0 10px 20px rgba(0,0,0,0.1);
  }
  
  
  "))

  
  
)

server <- function(input, output, session) {
  # Reactive expression for filtered birds
  filtered_birds <- reactive({
    # If no filters are selected, return all birds
    if (length(input$habitat_filter) == 0 && length(input$status_filter) == 0) {
      return(birds)
    }
    
    # Filter birds based on selected criteria
    filtered <- birds
    
    # Filter by habitat if any are selected
    if (length(input$habitat_filter) > 0) {
      filtered <- Filter(function(bird) {
        bird_habitats <- unlist(strsplit(bird$habitat, ",\\s*"))
        any(bird_habitats %in% input$habitat_filter)
      }, filtered)
    }
    
    # Filter by status if any are selected
    if (length(input$status_filter) > 0) {
      filtered <- Filter(function(bird) {
        bird$status %in% input$status_filter
      }, filtered)
    }
    
    return(filtered)
  })
  
  # Output the carousel with filtered birds
  output$filtered_carousel <- renderUI({
    div(
      style = "margin-top: 20px;",
      create_carousel(filtered_birds())
    )
  })
  
  # Reset filters when button is clicked
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "habitat_filter", selected = unique_habitats)
    updateCheckboxGroupInput(session, "status_filter", selected = unique_statuses)
  })
  
  # Handle bird selection and update map
  observeEvent(input$selected_bird, {
    index <- input$selected_bird
    if(!is.null(index) && index >= 1 && index <= length(filtered_birds())) {
      selected_bird <- filtered_birds()[[index]]
      
      # Update the map
      leafletProxy("range_map") %>%
        clearShapes() %>%
        setView(
          lng = selected_bird$range_center$lng, 
          lat = selected_bird$range_center$lat, 
          zoom = 7
        ) %>%
        addCircles(
          lng = selected_bird$range_center$lng,
          lat = selected_bird$range_center$lat,
          radius = selected_bird$range_radius,
          color = "#FF4500",
          fillColor = "#FF4500",
          fillOpacity = 0.2,
          weight = 2,
          popup = paste0(selected_bird$name, ": ", selected_bird$description)
        )
      
      # Update text info
      output$selected_bird_title <- renderText({
        paste(selected_bird$name, " (", selected_bird$scientific_name, ")", sep="")
      })
      
      output$selected_bird_description <- renderText({
        selected_bird$description
      })
    }
  })
  
  # Initialize the map
  output$range_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lng = -72.7, lat = 41.6, zoom = 8) %>%
      addControl(
        html = "<p class='text-center'>Click on a bird card to view its range</p>",
        position = "bottomleft"
      )
  })
}

shinyApp(ui = ui, server = server)
