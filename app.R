library(shiny)
library(shinythemes)
library(DT)
library(wordcloud2)
library(ggplot2)
library(stringr)
library(dplyr)
library(leaflet)
library(jsonlite)
library(bslib)
library(htmlwidgets)
library(httr)
library(zip)
library(shinyWidgets)
iris$Species <- NULL
# Define the directory where JSON files are stored
json_dir <- getwd()  # or set to your specific directory, e.g., "data/json"

# Function to fetch/load list of user games / games that user has access to their tracks
fetch_games_data_from_server <- function(url, token) {
  message("Fetched data, now processing...")
  # Adds a space between 'Bearer' and the token
  auth_header <- paste("Bearer", token)  
  
  # Make the request with Authorization header
  res <- GET(url, add_headers(Authorization = auth_header))
  
  # Handle the response
  if (status_code(res) == 200) {
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    if (length(data) == 0) {
      return(NULL)  # No content found
    }
    # return the content of the response
    message("Finished fetching data... m")
    return(data$content)
  } else {
    if (status_code(res) == 401) {
      warning("Unauthorized: Invalid or expired token.")
    } else {
      warning(paste("Failed to fetch games. Status code:", status_code(res)))
    }
    return(NULL)
  }
}

# commited out as there was an issue that it works fine locally but not on the server
# Get Git version and commit time of latest commit in main branch
# git_version <- tryCatch({
#   # Get commit date in dd.mm.yy format
#   date_part <- trimws(system("git log -n 1 main --format='%cd' --date=format:%d.%m.%y", intern = TRUE))
#   # Get commit time in hh:mm:ss format
#   time_part <- trimws(system("git log -n 1 main --format='%cd' --date=format:%H:%M:%S", intern = TRUE))
#   # Combine with custom separator
#   paste0("Version 1.5.2 - ", date_part, " ", time_part)
# }, error = function(e) {
#   # Fallback if Git is unavailable
#   format(Sys.time(), "%d.%m.%y %H:%M:%S")
# })

ui <- page_sidebar(
  title = div(
    style = "display: flex; align-items: center; gap: 20px;",
    tags$img(src = "https://geogami.ifgi.de/wp-content/uploads/2020/03/Unbenannt-7.png", height = "60px"),
    tags$div(
      tags$h1("Welcome to the dashboard!", style = "margin: 0; font-size: 24px;"),
      tags$a("app.geogami.ifgi.de", href = "https://app.geogami.ifgi.de/", style = "font-size: 14px; color: white;")
    )
  ),
  
  tags$head(
    tags$style(HTML("
    #selected_multiple_files-label + div div > .items {
      width: 80vw;
    }
    @media only screen and (min-width: 575px) {
      #selected_multiple_files-label + div div > .items {
        width: 20vw;
      }
    }
    @media only screen and (min-width: 750px) {
      #selected_multiple_files-label + div div > .items {
        width: 40vw;
      }
    }
    @media only screen and (min-width: 1100px) {
      #selected_multiple_files-label + div div > .items {
        width: 60vw;
      }
    }
    #selected_multiple_files-label + div div > .items {
      display: flex;
      flex-flow: row wrap;
      justify-content: flex-start;
      gap: 5px;
    }
    .item {
      word-wrap: anywhere;
    }
    pre {
      background-color: #F0F0F0 !important;
      color: #333 !important;
    }
   
    .bslib-page-sidebar .navbar {
      background: linear-gradient(90deg, rgb(7, 48, 59) 20%, #0CD1E8 100%);
      min-height: 80px;
      padding-top: 10px;
      padding-bottom: 10px;
      padding-left: 20px;
    }

    .bslib-page-sidebar .navbar-brand {
      display: flex;
      align-items: center;
      gap: 15px;
    }

    .bslib-page-sidebar h1 {
      margin: 0;
      font-size: 25px;
      color: white;
    }

    .bslib-page-sidebar a {
      color: black;
    }
    
    #inlineDiv {
      display: inline-block;
    }

    /* Default (desktop) sidebar: fixed width */
    .bslib-page-sidebar .sidebar {
      flex: 0 0 300px !important;
      max-width: 300px !important;
    }

    /* On tablets (portrait, e.g. iPad ≤ 1024px), sidebar smaller */
    @media (max-width: 1024px) {
      .bslib-page-sidebar .sidebar {
        flex: 0 0 220px !important;
        max-width: 220px !important;
      }
    }

    /* On very small screens (phones ≤ 768px), sidebar takes full width (collapses on top) */
    @media (max-width: 768px) {
      .bslib-page-sidebar .sidebar {
        flex: 0 0 100% !important;
        max-width: 100% !important;
      }
    }
    
    
    .selectize-control {
      width: 100% !important;
    }
    .selectize-input {
      width: 100% !important;
    }

    /* Hover effect on main tabs */
    .nav-tabs > li > a:hover {
      background-color: #27E7F5 !important;  /* teal blue on hover */
      color: #000 !important;
      border: 1px solid #ffc107;
    }

    .bootstrap-select,
    .bootstrap-select .dropdown-menu {
      width: 100% !important;
      max-width: 100% !important;
    }
    
    .dropdown-menu.inner {
      max-height: 300px !important;
      overflow-y: auto !important;
    }
  
  
  /* Make pickerInput dropdown items behave like selectInput */
  .bootstrap-select .dropdown-menu li a {
    white-space: normal !important;      /* allow multi-line wrapping */
    line-height: 1.3 !important;         /* adjust vertical spacing */
    padding-top: 6px !important;
    padding-bottom: 6px !important;
  }

  /* Align and restyle check marks */
  .bootstrap-select .dropdown-menu li a span.check-mark {
    position: absolute !important;
    right: 10px !important;
    top: 50% !important;
    transform: translateY(-50%);
    color: #198754;                      /* green tick */
    font-size: 14px;
  }


    /* Make pickerInput dropdown text darker and bolder like selectInput */
.bootstrap-select .dropdown-menu li a span.text {
  font-weight: 400 !important;   /* similar to selectInput weight */
  color: #000 !important;        /* pure black text for better contrast */
}

    /* Optional: make the selected text (on the button) also bold */
.bootstrap-select .filter-option-inner-inner {
  font-weight: 400 !important;
  color: #000 !important;
}


    /* Make selected/hovered pickerInput option lighter */
.bootstrap-select .dropdown-menu li.active a,
.bootstrap-select .dropdown-menu li:hover a {
  background-color: #27e7f5 !important; /* light teal-blue background */
  color: #000 !important;                /* ensure text stays readable */
    

  "))
  ),
  
  theme = bs_theme(),  # initially empty theme
  
  # Sidebar with collapsible toggle
  
  sidebar = sidebar(
    width = "300px",
    radioButtons("theme", "Choose Theme:",
                 choices = c("Light", "Dark"),
                 inline = TRUE,
                 selected = "Light"),
    # Upload JSON file section
    div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 5px; border-radius: 5px;",
        fileInput("uploaded_json_file", "Upload JSON file:", accept = ".json", multiple = FALSE),
    ),
    
    #filter 1 - game selection
    conditionalPanel(
      condition = "typeof window.location.search.match(/token=([^&]+)/) !== 'undefined' && window.location.search.match(/token=([^&]+)/) !== null",
      div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 15px; border-radius: 8px;",
          pickerInput(
            inputId = "selected_games",
            label = "Select your game:",
            choices = NULL,  # Leave it empty initially
            multiple = FALSE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = FALSE,
              `none-selected-text` = "Select a player",
              `width` = '100%',
              container = FALSE,
              size = 10
            )
          )
      )
    ),
    
    #filter 2 - JSON file selection
    conditionalPanel(
      condition = "typeof window.location.search.match(/token=([^&]+)/) !== 'undefined' && window.location.search.match(/token=([^&]+)/) !== null",
      div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 15px; border-radius: 8px;",
          pickerInput(
            inputId = "selected_files",
            label = "Select the players:",
            choices = NULL,  # Leave it empty initially
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = FALSE,
              `none-selected-text` = "Select a player",
              `width` = '100%',
              container = FALSE,
              size = 10
            )
          ),
          #actionButton("reset", "Reset", icon = icon("refresh"), style = "width:150px; margin-top: 10px; margin-bottom: 15px; margin-right: 15px"),
          textOutput("info_download"),
          downloadButton("download_json", "Download", icon = icon("download"), style = "width:150px; margin-top: 10px; margin-bottom: 15px;")
      )
    ),
    
    
    #filter 2 - ID - 2nd div
    # div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 15px; border-radius: 8px;",
    #     numericInput("num_value", "Enter a task number:", value = 1, min = 1, max = 1)
    # ),
    
    div(
      style = "text-align: left; color: #888; font-size: 12px;",
      
      # "Version 1.5.5 - 12:33 24.10.2025"
      HTML(paste0("Version 1.5.5 - " , format(Sys.time(), "%d.%m.%y %H:%M:%S")))      
    )
  ),
  
  # Main tabs
  tabsetPanel(
    tabPanel(
      'All tasks',
      div(
        style = "display: flex; justify-content: flex-start; gap: 40px; align-items: flex-start;
             margin-top: 20px; margin-bottom: 15px;
             border: 1px solid #ccc; padding: 10px;",
        # Task filter
        div(style = "min-width: 300px;", 
            uiOutput("task_id_selector")),
        
        # Player selector
        div(style = "min-width: 300px;", 
            uiOutput("file_selector_ui"))
      ),
      
      uiOutput("player_info_box"),
      DTOutput('iris_data'),
      div(
        style = "border: 0px solid #ccc; padding: 10px; margin-top: 15px; border-radius: 8px;",
        uiOutput('save_big_table')
      )
    ),
    
    tabPanel(
      'Map',
      div(
        style = "display: flex; justify-content: flex-start; gap: 40px; align-items: flex-start;
             margin-top: 20px; margin-bottom: 15px;
             border: 1px solid #ccc; padding: 10px;",
        div(style = "min-width: 300px;", pickerInput("num_value", "Selected Tasks:", choices = 1, selected = 1, multiple = FALSE)),
        div(style = "min-width: 300px;", uiOutput("file_selector_ui3"))
      ),
      textOutput("mapLegend"),
      div(id="map", leafletOutput("map"), style = "margin-top: 5px"),
      div(style = "border: 0px solid #ccc; padding: 10px; margin-top: 15px; border-radius: 8px;",
          downloadButton('downloadMap','Save the map'), full_screen = TRUE)
    ),
    tabPanel(
      'Pictures',
      div(
        style = "display: flex; justify-content: flex-start; gap: 40px; align-items: flex-start;
             margin-top: 20px; margin-bottom: 15px;
             border: 1px solid #ccc; padding: 10px;",
        div(style = "min-width: 300px;", pickerInput("num_value_pictures", "Selected Tasks:", choices = 1, selected = 1,multiple = FALSE)),
        div(style = "min-width: 300px;", uiOutput("file_selector_ui4"))
      ),
      card(uiOutput("photo_display"))
    ),
    tabPanel(
      'Compare Players',
      div(
        style = "display: flex; justify-content: flex-start; gap: 40px; align-items: flex-start;
           margin-top: 20px; margin-bottom: 15px;
           border: 1px solid #ccc; padding: 10px;",
        div(style = "min-width: 300px; max-width: 350px;", 
            pickerInput("num_value_comparison", "Selected Tasks:", choices = 1, selected = 1, multiple = FALSE)
        ),
        div(style = "flex: 1; max-width: 700px;", 
            uiOutput("file_selector_ui1")
        )
        
        #NOTE FOR SELECTIZE INPUT HANDLING THE MAXIMUM WIDTH OF CHOSEN PLAYERS IN 'COMPARISON' FROM THE ABOVE  - ADDED THE CODE WRITTEN BELOW IN THE CSS TAGS$HEAD$STYLE ABOVE,
        # .selectize-control {
        #   width: 100% !important;
        # }
        # .selectize-input {
        #   width: 100% !important;
        # }
      ),
      textOutput("tabLegend"),
      conditionalPanel(
        condition = "output.tabLegend == 'Task type: Navigation to flag' || output.tabLegend == 'Task type: Navigation with arrow' || output.tabLegend == 'Task type: Navigation via text' || output.tabLegend == 'Task type: Navigation via photo'",
        card(h4("Route length versus time"), tableOutput('cmp_table1'), downloadButton('save_table1', 'Save to csv'), style = "margin-top: 10px")
      ),
      conditionalPanel(
        condition = "output.tabLegend == 'Task type: Direction determination'",
        card(h4("Answer and error for direction task"), tableOutput('cmp_table2'), downloadButton('save_table2', 'Save to csv'), style = "margin-top: 10px")
      )
    ),
    tabPanel(
      'Statistics',
      div(
        style = "display: flex; justify-content: flex-start; gap: 40px; align-items: flex-start;
             margin-top: 20px; margin-bottom: 15px;
             border: 1px solid #ccc; padding: 10px;",
        div(style = "min-width: 300px;  max-width: 350px;", 
            pickerInput("num_value_Statistics", "Selected Tasks:", choices = 1, selected = 1, multiple = FALSE)
        ),
        div(style = "flex: 1; max-width: 700px;", 
            uiOutput("file_selector_ui2")
        )
      ), 
      # .selectize-control {
      #   width: 100% !important;
      # }
      # .selectize-input {
      #   width: 100% !important;
      # }
      
      textOutput("graphLegend"),
      #if the task category is navigation
      conditionalPanel(condition = "output.graphLegend == 'Task type: Navigation to flag' || output.graphLegend == 'Task type: Navigation with arrow' || output.graphLegend == 'Task type: Navigation via text' || output.graphLegend == 'Task type: Navigation via photo'",
                       pickerInput(
                         inputId = "graph_filter",
                         label = "Choose graphic to display:",
                         choices = c("Time VS Distance","Answer & Error"),
                         selected = c("Answer & Error")
                       ), conditionalPanel(
                         condition = "input.graph_filter == 'Answer & Error'",
                         card(card_header("Pie chart") , full_screen = TRUE, plotOutput('pie_chart'), 
                              div(style = "border: 0px solid #ccc; padding: 10px; margin-top: 15px; border-radius: 8px;",
                                  downloadButton('save_picture','Save to png')))
                       ), conditionalPanel(
                         condition = "input.graph_filter == 'Time VS Distance'",
                         card(card_header("Time vs distance scatter plot"), full_screen = TRUE, plotOutput('time_chart'), 
                              div(style = "border: 0px solid #ccc; padding: 10px; margin-top: 15px; border-radius: 8px;",
                                  downloadButton('save_time_chart','Save to png')))
                       )
      ),
      #else, for the other tasks
      conditionalPanel(condition = "output.graphLegend == 'Task type: Direction determination' || output.graphLegend == 'Task type: Free' || output.graphLegend == 'Task type: Self location' || output.graphLegend == 'Task type: Object location'",
                       pickerInput(
                         inputId = "graph_filter2",
                         label = "Choose graphic to display:",
                         choices = c("Answer & Error"),
                         selected = c("Answer & Error")
                       ), conditionalPanel(
                         condition = "input.graph_filter2 == 'Answer & Error'",
                         card(card_header("Time vs distance scatter plot"), full_screen = TRUE, plotOutput('pie_chart2'),
                              div(style = "border: 0px solid #ccc; padding: 10px; margin-top: 15px; border-radius: 8px;",
                                  downloadButton('save_picture2','Save to png')))
                       ),
      ),
    )
  )
)


server <- function(input, output, session) {
  
  #####---------TYPE CONVERSION HELPER FUNCTIONS FOR VR HELEN'S Tasks - Start--------######
  # num <- function(x) suppressWarnings(as.numeric(x))
  # 
  # get_correct_bearing <- function(j, evts) {
  #   b1 <- try(evts$task$question$initialAvatarPosition$bearing[j], silent = TRUE)
  #   if (!inherits(b1, "try-error") && length(b1) && !is.na(b1)) return(num(b1))
  #   b2 <- try(evts$task$question$direction$bearing[j], silent = TRUE)
  #   if (!inherits(b2, "try-error") && length(b2) && !is.na(b2)) return(num(b2))
  #   b3 <- try(evts$compassHeading[j], silent = TRUE)
  #   return(num(b3))
  # }
  # 
  # get_answer_bearing <- function(j, evts) {
  #   a1 <- try(evts$answer$clickDirection[j], silent = TRUE)
  #   if (!inherits(a1, "try-error") && length(a1) && !is.na(a1)) return(num(a1))
  #   a2 <- try(evts$answer$compassHeading[j], silent = TRUE)
  #   return(num(a2))
  # }
  #####---------TYPE CONVERSION HELPER FUNCTIONS FOR VR HELEN'S Tasks - end--------######
  
  # robust numeric parse (handles "74,66778")
  num <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_real_)
    if (is.list(x)) return(NA_real_)
    if (is.character(x)) x <- gsub(",", ".", x, fixed = TRUE)
    suppressWarnings(as.numeric(x))
  }
  
  angle_diff_deg <- function(a, b) {
    if (is.na(a) || is.na(b)) return(NA_real_)
    d <- (a - b + 180) %% 360 - 180
    abs(d)
  }
  
  get_correct_bearing <- function(j, evts) {
    eval <- try(evts$task$evaluate[j], silent = TRUE)
    eval <- if (inherits(eval, "try-error")) NA else eval
    
    # IMPORTANT: evalDirection should use question$direction$bearing first
    if (!is.na(eval) && eval == "evalDirection") {
      b <- try(evts$task$question$direction$bearing[j], silent = TRUE)
      if (!inherits(b, "try-error") && length(b) && !is.na(b)) return(num(b))
    }
    
    # evalMapDirection should use initialAvatarPosition$bearing
    if (!is.na(eval) && eval == "evalMapDirection") {
      b <- try(evts$task$question$initialAvatarPosition$bearing[j], silent = TRUE)
      if (!inherits(b, "try-error") && length(b) && !is.na(b)) return(num(b))
    }
    
    # fallback order
    b2 <- try(evts$task$question$direction$bearing[j], silent = TRUE)
    if (!inherits(b2, "try-error") && length(b2) && !is.na(b2)) return(num(b2))
    
    b1 <- try(evts$task$question$initialAvatarPosition$bearing[j], silent = TRUE)
    if (!inherits(b1, "try-error") && length(b1) && !is.na(b1)) return(num(b1))
    
    b3 <- try(evts$compassHeading[j], silent = TRUE)
    num(b3)
  }
  
  get_answer_bearing <- function(j, evts) {
    eval <- try(evts$task$evaluate[j], silent = TRUE)
    eval <- if (inherits(eval, "try-error")) NA else eval
    
    # For evalDirection, answer is compassHeading
    if (!is.na(eval) && eval == "evalDirection") {
      a <- try(evts$answer$compassHeading[j], silent = TRUE)
      return(num(a))
    }
    
    # For evalMapDirection, answer is clickDirection
    a1 <- try(evts$answer$clickDirection[j], silent = TRUE)
    if (!inherits(a1, "try-error") && length(a1) && !is.na(a1)) return(num(a1))
    
    # fallback
    a2 <- try(evts$answer$compassHeading[j], silent = TRUE)
    num(a2)
  }
  
  
  
  
  
  
  map_rv <- reactiveVal(NULL)
  
  
  ####R helper function ##################
  haversine_m <- function(lon1, lat1, lon2, lat2) {
    R <- 6371000
    toRad <- pi / 180
    dlat <- (lat2 - lat1) * toRad
    dlon <- (lon2 - lon1) * toRad
    a <- sin(dlat/2)^2 + cos(lat1*toRad) * cos(lat2*toRad) * sin(dlon/2)^2
    2 * R * asin(pmin(1, sqrt(a)))
  }
  
  fix_vr_distance_tasks <- function(evts) {
    # detect VR rows (virEnvType exists)
    is_vr <- !is.null(evts$task$virEnvType) & !is.na(evts$task$virEnvType)
    
    # only tasks evaluated by distance to a point
    is_dist_task <- !is.null(evts$task$evaluate) &
      evts$task$evaluate == "distanceToPoint"
    
    # only evaluate on OK clicks
    is_ok <- evts$type == "ON_OK_CLICKED"
    
    idx <- which(is_vr & is_dist_task & is_ok)
    
    if (!length(idx)) return(evts)
    
    for (j in idx) {
      # target is usually already in events$answer$target at the OK event
      targ <- try(evts$answer$target[[j]], silent = TRUE)
      if (inherits(targ, "try-error") || length(targ) != 2) next
      
      lon_t <- as.numeric(targ[1]); lat_t <- as.numeric(targ[2])
      
      lon_p <- suppressWarnings(as.numeric(evts$position$coords$longitude[j]))
      lat_p <- suppressWarnings(as.numeric(evts$position$coords$latitude[j]))
      if (is.na(lon_p) || is.na(lat_p)) next
      
      d <- haversine_m(lon_p, lat_p, lon_t, lat_t)
      
      acc <- suppressWarnings(as.numeric(evts$task$settings$accuracy[j]))
      if (is.na(acc)) acc <- 10  # fallback if missing
      
      evts$answer$distance[j] <- d
      evts$answer$correct[j]  <- (d <= acc)
      evts$correct[j]         <- (d <= acc)  # keep both consistent
    }
    
    evts
  }
  ################################################ 
  
  
  
  fix_direction_tasks <- function(evts) {
    is_ok <- evts$type == "ON_OK_CLICKED"
    eval <- evts$task$evaluate
    
    idx <- which(
      is_ok &
        !is.null(eval) &
        (eval %in% c("evalMapDirection", "evalDirection"))
    )
    
    if (!length(idx)) return(evts)
    
    for (j in idx) {
      acc <- num(evts$task$settings$accuracy[j])
      if (is.na(acc)) acc <- 10
      
      cor <- get_correct_bearing(j, evts)
      ans <- get_answer_bearing(j, evts)
      err <- angle_diff_deg(ans, cor)
      
      if (is.na(err)) next
      
      evts$answer$correct[j] <- (err <= acc)
      evts$correct[j]        <- (err <= acc)
    }
    
    evts
  }
  
  
  
  
  dest_point <- function(lon, lat, bearing_deg, dist_m) {
    R <- 6371000
    br <- bearing_deg * pi/180
    lat1 <- lat * pi/180
    lon1 <- lon * pi/180
    
    lat2 <- asin(sin(lat1) * cos(dist_m/R) + cos(lat1) * sin(dist_m/R) * cos(br))
    lon2 <- lon1 + atan2(sin(br) * sin(dist_m/R) * cos(lat1),
                         cos(dist_m/R) - sin(lat1) * sin(lat2))
    
    c(lon2 * 180/pi, lat2 * 180/pi)
  }
  
  arrow_lines <- function(lon, lat, bearing, len_m = 25, head_m = 7, head_ang = 25) {
    end <- dest_point(lon, lat, bearing, len_m)
    left  <- dest_point(end[1], end[2], bearing + 180 - head_ang, head_m)
    right <- dest_point(end[1], end[2], bearing + 180 + head_ang, head_m)
    
    list(
      main  = data.frame(lng = c(lon, end[1]),   lat = c(lat, end[2])),
      left  = data.frame(lng = c(end[1], left[1]),  lat = c(end[2], left[2])),
      right = data.frame(lng = c(end[1], right[1]), lat = c(end[2], right[2]))
    )
  }
  
  
  angle_diff_deg_vec <- function(a, b) {
    d <- (a - b + 180) %% 360 - 180
    abs(d)
  }
  
  
  
  # Store selected game track data reactively
  selected_game_tracks_rv <- reactiveVal()
  
  num_value_num <- reactive({
    suppressWarnings(as.integer(input$num_value))
  })
  
  
  games_choices_rv <- reactiveVal()   # store mapping of game_id -> game_name (CREATING THIS FOR zip file that gets downloaded, this is used for giving the right name to the zip file that gets downloaded)
  # Store access token reactively
  accessToken_rv <- reactiveVal()
  track_data_rv <- reactiveVal()
  
  choices_rv <- reactiveVal() #FOR ENSURING THAT RIGHT NAME IS REFLECTED IN SELECTIZEINPUT INSTEAD OF MONGO DB IDs
  
  
  apiURL_rv <- reactiveVal("https://api.geogami.uni-muenster.de")
  
  # Observe the URL query string for the token parameter
  observe({
    query <- parseQueryString(session$clientData$url_search)
    tokenParam <- query[["token"]]
    accessToken_rv(tokenParam)
  })
  # Theme options
  observe({
    if (input$theme == "Dark") {
      session$setCurrentTheme(bs_theme(bootswatch = "solar"))
    } else {
      session$setCurrentTheme(bs_theme(bootswatch = "flatly"))
    }
  })
  output$text <- renderText({
    paste("Current theme is:", input$theme)
  })
  
  #############---------------LOADING GAMES start (NEWEST ON TOP)-----################
  mongo_objectid_time <- function(id) {
    id <- as.character(id)
    ts_hex <- substr(id, 1, 8)
    as.POSIXct(strtoi(ts_hex, base = 16L), origin = "1970-01-01", tz = "UTC")
  }
  
  
  observe({
    req(accessToken_rv())
    req(apiURL_rv())
    
    apiUrl <- paste0(apiURL_rv(), "/game/usergames")
    games_data <- fetch_games_data_from_server(apiUrl, accessToken_rv())
    
    if (is.null(games_data) || NROW(games_data) == 0) {
      games_choices_rv(setNames(character(0), character(0)))
      updatePickerInput(session, "selected_games", choices = character(0), selected = character(0))
      output$info_download <- renderText({ "" })
      return()
    }
    
    games_df <- as.data.frame(games_data, stringsAsFactors = FALSE)
    
    # --- NEW: derive created time from Mongo _id and sort newest first ---
    if ("_id" %in% names(games_df)) {
      games_df$created_dt <- mongo_objectid_time(games_df[["_id"]])
      games_df <- games_df[order(games_df$created_dt, decreasing = TRUE), , drop = FALSE]
    }
    
    # Build picker choices (label=name, value=id) in this sorted order
    mapping <- setNames(games_df[["_id"]], games_df$name)
    games_choices_rv(mapping)
    
    # Optional: force newest as default if you want
    # selected_val <- games_df[["_id"]][1]
    # Otherwise: preserve current selection if still valid
    cur <- isolate(input$selected_games)
    selected_val <- if (!is.null(cur) && cur %in% mapping) cur else games_df[["_id"]][1]
    
    updatePickerInput(session, "selected_games", choices = mapping, selected = selected_val)
    
    # Debug print to confirm ordering
    message("=== Games (newest -> oldest) ===")
    print(games_df[, c("name", "_id", "created_dt")], row.names = FALSE)
    
    output$info_download <- renderText({ "" })
  })
  
  #############---------------LOADING GAMES END (NEWEST ON TOP)-----################
  
  

  
  observeEvent(input$selected_games, {
    game_id <- input$selected_games
    
    # update the API URL with the selected game ID
    apiUrl <- paste0(apiURL_rv(), "/track/gametracks/", game_id)
    
    # Fetch game's tracks data from API
    # Note: The token is used for authentication, ensure it is valid
    games_tracks <- fetch_games_data_from_server(apiUrl, accessToken_rv())
    
    # --- NEW: sort tracks by createdAt (newest first) ---
    if (!is.null(games_tracks) && !is.null(games_tracks$createdAt)) {
      ord <- order(games_tracks$createdAt, decreasing = TRUE)
      games_tracks <- games_tracks[ord, , drop = FALSE]
    }
    
    # Store in reactive value
    selected_game_tracks_rv(games_tracks)
  })
  
  
  ### 4. Update file selector when data changes
  # observe({
  #   tracks_data <- selected_game_tracks_rv()
  #   
  #   if (!is.null(tracks_data)) {
  #     # Use meaningful labels for the UI: e.g., "Player Name - Date"
  #     choices <- setNames(
  #       tracks_data[["_id"]],
  #       paste0(tracks_data$players, " - ", tracks_data$createdAt)
  #     )
  #     
  #     # saving this mapping for reuse in a reactive variable
  #     choices_rv(choices)
  #     
  #     
  #     updatePickerInput(session, "selected_files",
  #                       choices = choices)
  #     
  #     output$info_download <- renderText({
  #       ""
  #     })
  #   }
  # })
  
  
  observe({
    tracks_data <- selected_game_tracks_rv()
    
    if (!is.null(tracks_data)) {
      # (tracks_data is already sorted above, but this keeps it robust)
      if (!is.null(tracks_data$createdAt)) {
        ord <- order(tracks_data$createdAt, decreasing = TRUE)
        tracks_data <- tracks_data[ord, , drop = FALSE]
      }
      
      # Labels: "Player - createdAt"
      choices <- setNames(
        tracks_data[["_id"]],
        paste0(tracks_data$players, " - ", tracks_data$createdAt)
      )
      
      # Save mapping for reuse
      choices_rv(choices)
      
      updatePickerInput(
        session, "selected_files",
        choices = choices
      )
      
      output$info_download <- renderText({ "" })
    }
  })
  
  
  
  # --- DOWNLOAD JSON (single or multiple) ---
  output$download_json <- downloadHandler(
    filename = function() {
      req(input$selected_files)
      
      ############mapping of zip file i.e = name of the chosen game STARTS##############
      
      # --- looking up readable game name from the stored mapping (game_id -> game_name) ---
      games_map <- games_choices_rv()   # this was saved earlier
      # games_map is a named vector where names(games_map) == games_name and games_map == games_id
      game_name <- NA_character_
      if (!is.null(games_map) && length(games_map) > 0) {
        # finding here the name whose value equals the selected id
        idx <- which(games_map == input$selected_games)
        if (length(idx) > 0) {
          game_name <- names(games_map)[idx[1]]
        }
      }
      if (is.null(game_name) || length(game_name) == 0 || is.na(game_name)) {
        game_name <- "geogami_game"
      }
      # clearing here the whitespaces if any (replace whitespace or other problematic chars with underscore)
      game_name <- gsub("[^A-Za-z0-9_\\-]", "_", game_name)
      ############mapping of zip file i.e = name of the chosen game ENDS##############
      
      if (length(input$selected_files) > 1) {
        paste0(game_name, "_tracks_", Sys.Date(), ".zip")
      } else {
        selected_track_data <- track_data_rv()
        player_name <- gsub("\\s+", "_", selected_track_data$players)
        date_label <- substr(selected_track_data$createdAt, 1, 10)
        paste0(player_name, "_", date_label, ".json")
      }
    },
    
    content = function(file) {
      req(input$selected_files)
      
      # --- MULTIPLE FILES SELECTED ---
      if (length(input$selected_files) > 1) {
        # Creating a temporary directory to hold all the files
        tmpdir <- tempdir()
        json_files <- c()
        game_name <- gsub("\\s+", "_", input$selected_games)##########
        
        for (selected_id in input$selected_files) {
          url <- paste0(apiURL_rv(), "/track/", selected_id)
          track_data <- fetch_games_data_from_server(url, accessToken_rv())
          
          # Handling empty results
          if (is.null(track_data)) next
          
          # giving Clean filenames: PlayerName_Date.json
          player_name <- gsub("\\s+", "_", track_data$players)
          date_label <- substr(track_data$createdAt, 1, 10)
          fname <- file.path(tmpdir, paste0(player_name, "_", date_label, ".json"))
          
          # Preserving coordinate precision
          jsonlite::write_json(
            track_data,
            path = fname,
            pretty = TRUE,
            auto_unbox = TRUE,
            digits = NA   # keeping all coordinate decimals
          )
          
          json_files <- c(json_files, fname)
        }
        
        # Bundling all JSONs into ZIP
        zip::zipr(zipfile = file, files = json_files, recurse = FALSE)
        
      } else {
        # --- SINGLE FILE SELECTED ---
        selected_id <- input$selected_files[1]
        url <- paste0(apiURL_rv(), "/track/", selected_id)
        track_data <- fetch_games_data_from_server(url, accessToken_rv())
        
        # Preserving here all/ full coordinate precision for trajectory normalization
        jsonlite::write_json(
          track_data,
          path = file,
          pretty = TRUE,
          auto_unbox = TRUE,
          digits = NA   #  keeping all coordinate decimals
        )
      }
    }
  )
  
  
  
  ### 5. Reset file selector when reset button clicked
  # observeEvent(input$reset, {
  #   tracks_data <- selected_game_tracks_rv()
  #   
  #   if (!is.null(tracks_data)) {
  #     choices <- setNames(
  #       tracks_data[["_id"]],
  #       paste0(tracks_data$players, " - ", tracks_data$createdAt)
  #     )
  #     
  #     updatePickerInput(session, "selected_files",
  #                       choices = choices,
  #                       selected = NULL)
  #   }
  # })
  
  # 6. Select single file to view
  output$file_selector_ui <- renderUI({
    
    req(choices_rv())  # ensuring here that the choices are ready
    
    req(input$selected_files)
    
    pickerInput("selected_data_file",
                "Selected Players:",
                choices = choices_rv(),
                selected = input$selected_files[1],
                multiple = FALSE,
                options = list(
                  `actions-box` = FALSE,
                  `live-search` = FALSE,
                  `none-selected-text` = "Select a player",
                  `width` = '100%',
                  container = FALSE,
                  size = 10
                )
    )
  })
  
  ### 7. Reactive: load selected single file data
  # loaded_json <- reactive({
  #   req(input$selected_data_file)
  #   selected <- input$selected_data_file
  #   
  #   lapply(selected, function(file) {
  #     track_id <- input$selected_data_file
  #     # Construct the API URL
  #     url <- paste0(apiURL_rv(), "/track/", track_id)
  #     # Fetch and return the JSON data from the server
  #     track_data <- fetch_games_data_from_server(url, accessToken_rv())
  #     track_data_rv(track_data)  # Store the data in reactive value
  #     return(track_data)
  #   })
  # })
  
  loaded_json <- reactive({
    req(input$selected_data_file)
    req(accessToken_rv())
    req(apiURL_rv())
    
    track_id <- input$selected_data_file[1]
    url <- paste0(apiURL_rv(), "/track/", track_id)
    
    d <- fetch_games_data_from_server(url, accessToken_rv())
    
    shiny::validate(
      shiny::need(!is.null(d), "No data returned for selected track.")
    )
    
    # apply VR correction patch
    if (!is.null(d$events)) {
      d$events <- fix_vr_distance_tasks(d$events)
      d$events <- fix_direction_tasks(d$events)
    }
    
    track_data_rv(d)
    list(d)
  })
  
  
  
  
  
  
  
  #Get the uploaded json file
  uploaded_json <- reactive({
    req(input$uploaded_json_file)
    datapaths <- input$uploaded_json_file$datapath
    
    lapply(datapaths, function(path) {
      d <- jsonlite::fromJSON(path)
      if (!is.null(d$events)) {
        d$events <- fix_vr_distance_tasks(d$events)
        d$events <- fix_direction_tasks(d$events)
      }
      d
    })
  })
  
  
  
  ### 8. Reactive: load multiple json files for comparison
  load_multiple <- reactive({
    req(input$selected_multiple_files)
    
    # Taking all selected IDs
    sel <- input$selected_multiple_files
    
    # Fetching the data for each selected track
    data_list <- lapply(sel, function(track_id) {
      url <- paste0(apiURL_rv(), "/track/", track_id)
      d <- fetch_games_data_from_server(url, accessToken_rv())
      if (!is.null(d$events)) {
        d$events <- fix_vr_distance_tasks(d$events)
        d$events <- fix_direction_tasks(d$events)
      }
      d
    })
    
    
    # updating track_data_rv to hold a list of all
    track_data_rv(data_list)
    
    return(data_list)
  })
  
  
  
  
  ### 9. UI: multiple file selector for comparison (tables, graphics, maps, photos)
  # UI for Compare Players - with select/deselect buttons
  output$file_selector_ui1 <- renderUI({
    
    req(choices_rv())  # ensuring here that the choices are ready
    
    req(input$selected_files)
    
    tagList(
      pickerInput(
        "selected_multiple_files", 
        "Selected Players:", 
        choices = choices_rv(),
        selected = input$selected_files,
        multiple = TRUE,
        options = list(
          `actions-box` = FALSE,
          `live-search` = FALSE,
          `none-selected-text` = "Select a player",
          `width` = '100%',
          container = FALSE,
          size = 10
        )
      )
      # ,
      # # Add select/deselect buttons
      # actionButton("select_all_players", "Select All"),
      # actionButton("deselect_all_players", "Select None")
    )
  })
  
  
  ####-------------'select and deselect all' buttons logic for file_selector_ui 1 that is 'compare' tab------------
  # Select all players
  # observeEvent(input$select_all_players, {
  #   req(choices_rv())
  #   req(input$selected_files)
  #   updateSelectInput(
  #     session,
  #     "selected_multiple_files",
  #     selected = input$selected_files
  #   )
  # })
  # 
  # # Deselect all players
  # observeEvent(input$deselect_all_players, {
  #   req(choices_rv())
  #   updateSelectInput(
  #     session,
  #     "selected_multiple_files",
  #     selected = character(0)
  #   )
  # })
  ####-------------'select and deselect all' buttons logic for file_selector_ui 1 that is 'compare' tab ENDS------------
  
  
  
  ##### Filters for comparing Graphics starts
  output$file_selector_ui2 <- renderUI({
    
    req(choices_rv())  # ensuring here that the choices are ready
    
    req(input$selected_files)
    
    tagList(
      pickerInput(
        "selected_multiple_files",  
        "Selected Players:", 
        choices = choices_rv(),
        selected = input$selected_files,
        multiple = TRUE,
        options = list(
          `actions-box` = FALSE,
          `live-search` = FALSE,
          `none-selected-text` = "Select a player",
          `width` = '100%',
          container = FALSE,
          size = 10
        )
      )
    )
  })
  
  
  
  
  
  ##### Filter for maps
  output$file_selector_ui3 <- renderUI({
    
    req(choices_rv())  # ensuring here that the choices are ready
    
    req(input$selected_files)
    
    pickerInput("selected_data_file",
                "Selected Players: ",
                choices = choices_rv(),
                selected = input$selected_files[1],
                multiple = FALSE,
                options = list(
                  `actions-box` = FALSE,
                  `live-search` = FALSE,
                  `none-selected-text` = "Select a player",
                  `width` = '100%',
                  container = FALSE,
                  size = 10
                ))
  })
  
  ##### Filter for photos
  output$file_selector_ui4 <- renderUI({
    
    req(choices_rv())  # ensuring here that the choices are ready
    
    req(input$selected_files)
    
    pickerInput("selected_data_file",
                "Selected Players: ",
                choices = choices_rv(),
                selected = input$selected_files[1],
                multiple = FALSE,
                options = list(
                  `actions-box` = FALSE,
                  `live-search` = FALSE,
                  `none-selected-text` = "Select a player",
                  `width` = '100%',
                  container = FALSE,
                  size = 10
                ))
  })
  
  #####Big table code
  df_react <- reactiveVal()
  
  observeEvent(req(input$selected_data_file, num_value_num()), {
    req(num_value_num() != 0 && num_value_num() > 0)
    
    data <- loaded_json()  # load selected JSON
    
    if (is.null(data) || length(data) == 0) {
      showNotification("No data found for selected file.", type = "error")
      return()
    }
    
    # Defensive check: ensure expected structure exists
    if (!("events" %in% names(data[[1]])) || !("task" %in% names(data[[1]]$events))) {
      showNotification("Unexpected data format.", type = "error")
      return()
    }
    
    id <- data[[1]]$events$task[["_id"]]
    typ <- list()
    cons <- list()
    ans <- list()
    
    # Extract reusable fields
    csg <- data[[1]]$events$task$question$text   #assignment text 
    ev <- data[[1]]$events$type                    # is used later to count tries - event type (INIT_TASK, ON_OK_CLICKED, ...)
    pict_quest <- data[[1]]$events$task$question$photo
    ans_type <- data[[1]]$events$task$answer$type    #decides how to format the “Answer” column.
    
    for (j in seq_len(length(id) - 1)) {
      if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
        correct_flag <- data[[1]]$events$answer$correct[j]
        
        ans_value <- NA  # Default
        
        if (ans_type[j] == "TEXT") {
          ans_text <- data[[1]]$events$answer$text[j]
          if (!is.na(correct_flag)) {
            ans_value <- paste(ifelse(correct_flag == "TRUE", "Correct", "Incorrect"), ans_text)
          }
        } else if (ans_type[j] == "MULTIPLE_CHOICE_TEXT") {
          choice_val <- data[[1]]$events$answer$selectedChoice$value[j]
          if (!is.na(correct_flag)) {
            ans_value <- paste(ifelse(correct_flag == "TRUE", "Correct", "Incorrect"), choice_val)
          }
        } else if (ans_type[j] == "NUMBER") {
          num_val <- data[[1]]$events$answer$numberInput[j]
          if (!is.na(correct_flag)) {
            ans_value <- paste(ifelse(correct_flag == "TRUE", "Correct", "Incorrect"), num_val)
          }
        } else {
          # Fallback for unknown types
          if (!is.null(correct_flag)) {
            ans_value <- ifelse(correct_flag == "TRUE", "Correct", "Incorrect")
          }
        }
        
        ans <- append(ans, ans_value)
        typ <- append(typ, data[[1]]$events$task$type[j])
        cons <- append(cons, csg[j])
      }
    }
    
    # print(cons)
    # print(typ)
    #print(cbind(data[[1]]$events$task$type, data[[1]]$events$correct,data[[1]]$events$answer$correct))
    #print(ans)
    
    # Distance to the correct answer
    dist1_m   <- list()   # meters
    dist1_deg <- list()   # player's bearing (deg)
    dist2_deg <- list()   # correct bearing (deg)
    
    for (j in 1:(length(id) - 1)) {
      if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
        # distance in meters (as is)
        dist1_m <- append(dist1_m, data[[1]]$events$answer$distance[j])
        
        # NEW: 
        ans_bearing <- get_answer_bearing(j, data[[1]]$events)
        cor_bearing <- get_correct_bearing(j, data[[1]]$events)
        
        dist1_deg <- append(dist1_deg, ans_bearing)
        dist2_deg <- append(dist2_deg, cor_bearing)
      }
    }
    
    
    #num <- function(x) suppressWarnings(as.numeric(x))
    
    # d1m   <- num(unlist(dist1_m))
    # d1deg <- num(unlist(dist1_deg))
    # d2deg <- num(unlist(dist2_deg))
    # 
    # maxn <- max(length(d1m), length(d1deg), length(d2deg))
    # length(d1m)   <- maxn
    # length(d1deg) <- maxn
    # length(d2deg) <- maxn
    # 
    # rds <- cbind(d1m, dist_deg = angle_diff_deg_vec(d1deg, d2deg))
    # #print(rds)
    # 
    # #print(rds)
    # 
    # ####sometimes we don't need to merge the column
    # if (ncol(rds) == 2) {
    #   rds[is.na(rds)] <- 0
    #   dist <- c(rds[,1]+rds[,2])
    #   dist[dist == 0] <- NA
    # }
    # else {
    #   dist <- rds
    # }
    # 
    # if (length(dist) != 0) {
    #   dist <- round(dist,2)
    # }
    # 
    # #Add unities on the last column
    # for (i in 1:length(typ)) {
    #   if (!is.na(typ[[i]]) && !is.na(dist[[i]]) && typ[[i]] == "theme-direction"){
    #     dist[[i]] <- paste(dist[[i]], "°")
    #   }
    #   if (!is.na(typ[[i]]) && !is.na(dist[[i]]) && (typ[[i]] == "nav-flag" || typ[[i]] == "theme-loc")) {
    #     dist[[i]] <- paste(dist[[i]], "m")
    #   }
    # }
    # #print(dist)
    
    
    # after you build typ, and dist1_m/dist1_deg/dist2_deg
    
    task_type <- as.character(unlist(typ))
    n <- length(task_type)
    
    d1m   <- suppressWarnings(as.numeric(unlist(dist1_m)))
    d1deg <- num(unlist(dist1_deg))
    d2deg <- num(unlist(dist2_deg))
    
    length(d1m)   <- n
    length(d1deg) <- n
    length(d2deg) <- n
    
    deg_err <- angle_diff_deg_vec(d1deg, d2deg)
    
    # logical masks (TRUE/FALSE length n)
    mask_dir <- !is.na(task_type) & task_type == "theme-direction"
    mask_m   <- !is.na(task_type) & task_type %in% c("nav-flag", "theme-loc")
    
    dist_num <- rep(NA_real_, n)
    dist_num[mask_dir] <- deg_err[mask_dir]
    dist_num[mask_m]   <- d1m[mask_m]
    
    dist_num <- round(dist_num, 2)
    
    dist_txt <- rep(NA_character_, n)
    dist_txt[mask_dir & !is.na(dist_num)] <- paste0(dist_num[mask_dir & !is.na(dist_num)], " °")
    dist_txt[mask_m   & !is.na(dist_num)] <- paste0(dist_num[mask_m   & !is.na(dist_num)], " m")
    
    dist <- dist_txt
    
    
    
    
    
    
    
    
    #Computing time spent on a task
    tps <- data[[1]]$events$timestamp
    time1 <- as.POSIXct(tps[1], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
    tmp <- list()
    for (j in 1:(length(id) - 1)) {
      if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
        time2 <- as.POSIXct(tps[j], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
        tmp <- append(tmp, paste(floor(as.numeric(time2 - time1, units = "secs")),"s"))
        time1 <- as.POSIXct(tps[j+1], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
      }
    }
    
    #print(tmp)
    #Computing tries number for each task
    tries = 0
    try <- list()
    for (j in 1:(length(id) - 1)) {
      if (ev[j] == "ON_OK_CLICKED") { #Answer when the player clicks on OK
        tries <- tries + 1
      }
      if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
        try <- append(try, tries)
        tries = 0
      }
    }
    
    #print(try)
    
    
    #Compute for maps
    long <- list()
    lati <- list()
    cou <- 1 #counter
    mr <- FALSE #Print an empty map or not
    coor <- data[[1]]$events$task$answer$position$geometry$coordinates #Position answer nav tasks
    
    lng_targ <- list()
    lat_targ <- list()
    lng_true <- list()
    lat_true <- list()
    targ <- data[[1]]$events$answer$clickPosition #Position answer theme localisation task 
    coor_true <- data[[1]]$events$position$coords
    
    #Position for free task
    dr_point_lat <- list()
    dr_point_lng <- list()
    drawing_point_lat <- data[[1]]$events$clickPosition$latitude
    drawing_point_lng <- data[[1]]$events$clickPosition$longitude
    #print(drawing_point_lat)
    
    type_task <- data[[1]]$events$task$type
    cat_task <- data[[1]]$events$task$category
    accuracy_radius <- data[[1]]$events$task$settings$accuracy #For theme-loc or navigation tasks
    accuracy_rad <- 0
    ev <- data[[1]]$events$type #Name of the event
    t <- ""
    
    
    #Print Polygons
    sel_polygon <- data[[1]]$events$task$question$geometry$feature
    lng_poly <- list()
    lat_poly <- list()
    lng_ans_obj <- list()
    lat_ans_obj <- list()
    
    
    dir_ok_idx <- NA_integer_
    
    
    #Recovering answers position for the map
    for (i in 1:(length(id)-1)) {
      if (!is.na(cat_task[i])) {
        if ((cat_task[i] == "nav") && (cou == num_value_num())) {
          long <- append(long, coor[[i]][1])
          lati <- append(lati, coor[[i]][2])
          accuracy_rad <- accuracy_radius[[i]]
          t <- type_task[i]
        }
        if ((cat_task[i] == "info") && (cou == num_value_num())) {
          mr <- TRUE #Showing an empty map
          t <- cat_task[i]
        }
      }
      if (!is.na(type_task[i])) { #target point for theme task
        if ((type_task[i] == "theme-loc") && (ev[i] == "ON_OK_CLICKED") && (cou == num_value_num())) {
          lng_targ <- append(lng_targ, targ[[i]][1])
          lat_targ <- append(lat_targ, targ[[i]][2])
          lng_true <- append(lng_true, coor_true$longitude[[i]])
          lat_true <- append(lat_true, coor_true$latitude[[i]])
          accuracy_rad <- accuracy_radius[[i]]
          t <- type_task[i]
        }
        if ((type_task[i] == "theme-loc") && (cou == num_value_num())) { #Always having the task type shown for theme-localisation
          t <- type_task[i]
        }
        
        
        if (type_task[i] == "theme-direction" && ev[i] == "ON_OK_CLICKED" && cou == num_value_num()) {
          dir_ok_idx <- i
          t <- type_task[i]
        }
        
        
        # #########-DOMINIKA'S game issue solved ######################################
        # safe single values for this event i
        # safe access
        # safe access
        mode_vec <- data[[1]]$events$task$question$mode
        
        mode_i <- if (!is.null(mode_vec) && length(mode_vec) >= i) mode_vec[[i]] else NA_character_
        ans_i  <- if (!is.null(ans_type) && length(ans_type) >= i) ans_type[[i]] else NA_character_
        
        is_theme_direction <-
          !is.na(type_task[i]) && type_task[i] == "theme-direction"
        
        is_theme_object_photo <-
          !is.na(type_task[i]) && type_task[i] == "theme-object" &&
          !is.na(ans_i)        && ans_i == "PHOTO"
        
        is_theme_object_nofeature <-
          !is.na(type_task[i]) && type_task[i] == "theme-object" &&
          !is.na(mode_i)       && mode_i == "NO_FEATURE"
        
        if ((is_theme_object_photo || is_theme_object_nofeature) && cou == num_value_num()) {
          mr <- TRUE
          t  <- type_task[i]
        }
        
        if (is_theme_direction && cou == num_value_num()) {
          t <- type_task[i]
          # DO NOT set mr <- TRUE
        }
        
        
        # #########-DOMINIKA'S game issue solved ######################################
        
        
        
        # if ((type_task[i] == "theme-direction" || (type_task[i] == "theme-object" && ans_type[[i]] == "PHOTO") || (type_task[i] == "theme-object" && length(data[[1]]$events$task$question$mode) != 0 && data[[1]]$events$task$question$mode[[i]] == "NO_FEATURE")) && cou == num_value_num()) { #tasks that show nothing on the map
        #   mr <- TRUE
        #   t <- type_task[i]
        # }

        
        
        if (type_task[i] == "theme-object" && cou == num_value_num() && ans_type[[i]] == "MAP_POINT") { #tasks that show nothing on the map
          poly <- sel_polygon[[i]]$geometry$coordinates[[1]]
          for (n in 1:(length(poly)/2)) {
            lng_poly <- append(lng_poly, poly[n])
          }
          for (n in (length(poly)/2+1):length(poly)) {
            lat_poly <- append(lat_poly, poly[n])
          }
          t <- type_task[i]
          if (type_task[i] == "theme-object" && (cou == num_value_num())) {
            lng_ans_obj <- append(lng_ans_obj, targ[[i]][1])
            lat_ans_obj <- append(lat_ans_obj, targ[[i]][2])
          }
        }
        if ((type_task[i] == "free") && (cou == num_value_num()) && length(drawing_point_lat) != 0 && !is.na(drawing_point_lat[[i]]) && ans_type[[i]] == "DRAW") {
          dr_point_lat <- append(dr_point_lat, drawing_point_lat[[i]])
          dr_point_lng <- append(dr_point_lng, drawing_point_lng[[i]])
          t <- type_task[i]
        }
        if ((type_task[i] == "free") && (cou == num_value_num()) && (length(drawing_point_lat) == 0 || is.na(drawing_point_lat[[i]])) && ans_type[[i]] == "DRAW") { #correcting error to visualize draw
          mr <- TRUE
          t <- type_task[i]
        }
        if ((type_task[i] == "free") && (cou == num_value_num()) && ans_type[[i]] != "DRAW") {
          mr <- TRUE
          t <- type_task[i]
        }
        
      }
      if (is.na(type_task[i]) && (i != 1) && (cou == num_value_num())) { #na task
        mr <- TRUE
      }
      if ((!is.na(id[i]) && (i != 1) && (id[i] != id[i + 1])) || i == (length(id) - 1)) {
        cou <- cou + 1
      }
    }
    
    #Print trajectory on the map
    traj_lng <- list()
    traj_lat <- list()
    accuracy <- list()
    task_number <- data[[1]]$waypoints$taskNo #Task number
    for (i in 1:length(task_number)) {
      if (!is.null(task_number[i])) {
        if (task_number[i] == num_value_num()) {
          traj_lng <- append(traj_lng, data[[1]]$waypoints$position$coords$longitude[i])
          traj_lat <- append(traj_lat, data[[1]]$waypoints$position$coords$latitude[i])
          if (length(data[[1]]$waypoints$position$coords$accuracy) != 0) {
            accuracy <- append(accuracy, data[[1]]$waypoints$position$coords$accuracy[i]) #accuracy on coordinates
          }
          else {
            accuracy <- append(accuracy, 1)
          }
        }
      }
    }
    
    #Compute again time (with way points)
    time_waypoints <- list()
    tps_waypoints <- data[[1]]$waypoints$timestamp
    
    for (k in 1:length(tmp)) {
      if (tmp[[k]] == "0 s") {
        for (i in 1:length(task_number)) {
          if (task_number[i] == k) {
            time_waypoints <- append(time_waypoints, tps_waypoints[[i]])
          }
        }
        if (length(time_waypoints) != 0) {
          x <- as.POSIXct(time_waypoints[[1]], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
          y <- as.POSIXct(time_waypoints[[length(time_waypoints)]], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
          delta <- paste(floor(as.numeric(y - x, units = "secs")),"s")
          tmp[[k]] <- delta
          if (tmp[[k]] != "0 s" && grepl(pattern = "nav", typ[[k]])) {
            ans[[k]] <- "Target not reached"
          }
        }
      }
      time_waypoints <- list()
    }
    
    #print(tmp)
    
    #Compute distance traveled
    deg_to_rad <- pi/180
    R <- 6.378e6
    if (length(traj_lng) > 1) {
      for (k in 1:(length(traj_lng)-1)) {
        if (!is.na(traj_lng[[k]][1])) {
          lat_1 <- traj_lat[[k]][1]
          lng_1 <- traj_lng[[k]][1]
          lat_2 <- traj_lat[[k+1]][1]
          lng_2 <- traj_lng[[k+1]][1]
          d <- R * acos(sin(lat_1*deg_to_rad)*sin(lat_2*deg_to_rad) + cos(lat_1*deg_to_rad)*cos(lat_2*deg_to_rad)*cos((lng_2 - lng_1)*deg_to_rad))
          if (accuracy[[k]] >= 20) {
            traj_lat[k] <- traj_lat[k+1] #Filter GPS coordinate errors
            traj_lng[k] <- traj_lng[k+1]
          }
        }
      }
    }
    
    rg <- cbind(unlist(typ), unlist(cons), unlist(ans), unlist(tmp), unlist(try), unlist(dist))
    #print(rg)
    
    for (i in 1:(nrow(rg))) {
      if (((!is.na(rg[i,1]) && rg[i,1] == "nav-arrow") || (!is.na(rg[i,1]) && rg[i,1] == "nav-text")
           || (!is.na(rg[i,1]) && rg[i,1] == "nav-photo")) && rg[i,4] != "0 s" && is.na(rg[i,3])) {
        rg[i,3] <- "Correct"
        rg[i,5] <- 1
      }
      if (is.na(rg[i,1])) {
        rg[i,1] <- "information"
        rg[i,3] <- NA
        rg[i,5] <- 0
      }
    }
    
    #Delete the last column if it's empty 
    counter_dist <- 0
    for (k in 1:length(dist)) {
      if (is.na(dist[[k]])) {
        counter_dist <- counter_dist + 1
      }
    }
    
    #Build the main table
    if (counter_dist != length(dist)) {
      mat = matrix(rg, ncol = 6, nrow = length(ans))
      
      df <- data.frame(
        Type = mat[,1],
        Assignment = mat[,2],
        Answer = mat[,3],
        Time = mat[,4],
        Tries = mat[,5],
        Error = mat[,6]
      )
      colnames(df)[6] <- "Error in °/m"
    }
    else {
      mat = matrix(rg, ncol = 6, nrow = length(ans)) #Big table without the last column
      
      df <- data.frame(
        Type = mat[,1],
        Assignment = mat[,2],
        Answer = mat[,3],
        Time = mat[,4],
        Tries = mat[,5]
      )
    }
    
    #Name of the player
    #print(data[[1]]$players[1])
    output$player_name <- renderText({
      paste("Player: ", data[[1]]$players[1], sep = "")
    })
    #Overall score
    if (sum(grepl(pattern = "Incorrect", df$Answer)) != 0 || sum(grepl(pattern = "Target", df$Answer)) != 0) { #If incorrect values are in the table
      good <- sum(grepl(pattern = "Correct", df$Answer))
      total <- sum(grepl(pattern = "Incorrect", df$Answer)) + sum(grepl(pattern = "Correct", df$Answer)) + sum(grepl(pattern = "Target", df$Answer))
    }
    else { #if all is correct
      good <- sum(grepl(pattern = "Correct", df$Answer))
      total <- good
    }
    
    output$overall_score <- renderText({
      paste("Overall score: ", good, "/", total, sep = "")
    })
    
    output$player_info_box <- renderUI({
      req(data[[1]]$players[1])
      
      div(id = "inlineDiv",
          style = "margin-bottom: 20px; border: 1px solid #ccc; padding: 10px; border-radius: 5px; background-color: #f9f9f9;",
          h5(textOutput("player_name")),
          h5(textOutput("overall_score"))
      )
    })
    
    df_react(df)
    
    #PREVIOUSLY OBSERVE BUTTONS - PICKER INPUT 
    # observe({
    #   req(df_react())
    #   choices <- seq_len(nrow(df_react()))
    #   updatePickerInput(session, "num_value_comparison", choices = choices, selected = input$num_value_comparison)
    # })
    
    
    
    ###############----------Task IDs updation all 4 STARTS-------------################
    observeEvent(df_react(), {
      df <- df_react()
      if (is.null(df) || nrow(df) == 0) return()
      
      n <- nrow(df)
      
      # try to preserve current Map selection if valid; otherwise default to 1
      cur <- suppressWarnings(as.integer(input$num_value))
      if (is.na(cur) || cur < 1 || cur > n) cur <- 1
      
      # choices are strings for pickerInput
      choice_vec <- as.character(seq_len(n))
      sel <- as.character(cur)
      
      # Push the SAME choices + selected to all four pickers
      for (id in all_ids) {
        updatePickerInput(session, id, choices = choice_vec, selected = sel)
      }
    })
    ###############----------Task IDs updation all 4 STARTS-------------################
    
    
    #all_ids <- c("num_value", "num_value_map", "num_value_pictures", "num_value_comparison", "num_value_Statistics")
    #NOTE : num_value is the important variable, all of the other stored elements are triggered because of 'num_value'
    
    output$iris_data <- renderDT({
      df_react()
    })
    
    
    
    
    #-----------all tasks - id checkbox filter starts --------------------------------
    output$task_id_selector <- renderUI({
      req(df_react())
      df <- df_react()
      
      task_ids <- seq_len(nrow(df))   # use row numbers as task IDs
      
      tagList(
        pickerInput(
          "selected_task_ids",
          "Filter by Task ID:",
          choices = task_ids,
          selected = task_ids,   # initially all
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = FALSE,
            `none-selected-text` = "Filter by Task ID: ",
            `width` = '100%',
            container = FALSE,
            size = 10
          )
        )
      )
    })
    
    # Filtered data
    filtered_df <- reactive({
      req(df_react())
      df <- df_react()
      
      if (is.null(input$selected_task_ids) || length(input$selected_task_ids) == 0) {
        return(df)   # if none selected, show all
      }
      
      df[input$selected_task_ids, , drop = FALSE]   # subset by row numbers
    })
    
    # Show table
    output$iris_data <- renderDT({
      filtered_df()
    }, options = list(pageLength = 10))
    
    #---------logic for select/deselect all starts ----------------------
    observeEvent(input$select_all_tasks, {
      req(df_react())
      task_ids <- seq_len(nrow(df_react()))
      updateSelectInput(session, "selected_task_ids", selected = task_ids)
    })
    
    observeEvent(input$deselect_all_tasks, {
      updateSelectInput(session, "selected_task_ids", selected = character(0))
    })
    
    #---------logic for select/deselect all ends ----------------------
    
    #---------task filter id for all tasks ends------------------------------------
    
    
    # Download filtered big table
    output$save_data <- downloadHandler(
      filename = function(){
        paste("data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file){
        # use the filtered reactive df
        write.csv(filtered_df(), file, row.names = FALSE)
      }
    )
    
    output$save_big_table <- renderUI({
      req(filtered_df())
      
      if (nrow(filtered_df()) > 0) {
        downloadButton('save_data', 'Save to CSV')
      }
    })
    
    
    
    
    
    # create icons (from Jakub's code)
    loc_marker <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/origami-team/origami/master/src/assets/icons/marker-editor.png",
      iconWidth = 20, iconHeight = 20,
      iconAnchorX = 10, iconAnchorY = 20,
    )
    loc_marker_green <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/origami-team/origami/master/src/assets/icons/marker-editor-solution.png",
      iconWidth = 20, iconHeight = 20,
      iconAnchorX = 10, iconAnchorY = 20,
    )
    
    safe_coords <- function(x) {
      x <- unlist(x)
      x <- x[is.finite(x) & !is.na(x)]
      return(x)
    }
    
    long <- safe_coords(long)
    lati <- safe_coords(lati)
    traj_lng <- safe_coords(traj_lng)
    traj_lat <- safe_coords(traj_lat)
    lng_targ <- safe_coords(lng_targ)
    lat_targ <- safe_coords(lat_targ)
    lng_true <- safe_coords(lng_true)
    lat_true <- safe_coords(lat_true)
    lng_poly <- safe_coords(lng_poly)
    lat_poly <- safe_coords(lat_poly)
    lng_ans_obj <- safe_coords(lng_ans_obj)
    lat_ans_obj <- safe_coords(lat_ans_obj)
    dr_point_lng <- safe_coords(dr_point_lng)
    dr_point_lat <- safe_coords(dr_point_lat)
    
    
    if (!is.na(dir_ok_idx) && t == "theme-direction") {
      evts <- data[[1]]$events
      
      lon0 <- num(evts$position$coords$longitude[dir_ok_idx])
      lat0 <- num(evts$position$coords$latitude[dir_ok_idx])
      
      ans_b <- get_answer_bearing(dir_ok_idx, evts)
      cor_b <- get_correct_bearing(dir_ok_idx, evts)
      
      if (is.finite(lon0) && is.finite(lat0) && !is.na(ans_b)) {
        A <- arrow_lines(lon0, lat0, ans_b)
        
        map_shown <- leaflet() %>%
          addTiles() %>%
          addMarkers(lng = lon0, lat = lat0, icon = loc_marker) %>%
          addPolylines(lng = A$main$lng,  lat = A$main$lat) %>%
          addPolylines(lng = A$left$lng,  lat = A$left$lat) %>%
          addPolylines(lng = A$right$lng, lat = A$right$lat) %>%
          setView(lng = lon0, lat = lat0, zoom = 19)
        
        # optional: draw correct direction (dashed)
        if (!is.na(cor_b)) {
          C <- arrow_lines(lon0, lat0, cor_b)
          map_shown <- map_shown %>%
            addPolylines(lng = C$main$lng,  lat = C$main$lat,  opacity = 0.6, dashArray = "5,5") %>%
            addPolylines(lng = C$left$lng,  lat = C$left$lat,  opacity = 0.6, dashArray = "5,5") %>%
            addPolylines(lng = C$right$lng, lat = C$right$lat, opacity = 0.6, dashArray = "5,5")
        }
        
        mr <- FALSE
      }
    }
    
    
    
    
    
    #Print map
    if (mr == TRUE || length(ans) <= num_value_num() || (length(lng_targ) == 0 && length(lng_true) == 0 && t == "theme-loc")
        || (length(long) == 0 && length(traj_lat) == 0 && (t == "nav-flag" || t == "nav-text" || t == "nav-arrow" || t == "nav-photo"))) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          setView(lng = 7, lat = 51, zoom = 6)
      }
    }
    if (length(long) != 0 && length(traj_lat) == 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addMarkers(lng = unlist(long)[1], lat = unlist(lati)[1], icon = loc_marker_green) %>%
          addCircles(lng = unlist(long)[1], lat = unlist(lati)[1], radius = accuracy_rad, opacity = 0.5)
      }
    }
    if (length(long) != 0 && length(traj_lat) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addMarkers(lng = unlist(long)[1], lat = unlist(lati)[1], icon = loc_marker_green) %>%
          addCircles(lng = unlist(long)[1], lat = unlist(lati)[1], radius = accuracy_rad, opacity = 0.5) %>%
          addPolylines(lng = unlist(traj_lng), lat = unlist(traj_lat), color = "red", weight = 2, opacity = 1, stroke = TRUE)
      }
    }
    if (length(dr_point_lng) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addPolylines(lng = unlist(dr_point_lng), lat = unlist(dr_point_lat), color = "red", weight = 2, opacity = 1, stroke = TRUE)
      }
    }
    if (length(lng_targ) != 0 && length(lng_true) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>% #Displaying last answer of the player
          addMarkers(lng = unlist(lng_targ)[length(lng_targ)], lat = unlist(lat_targ)[length(lat_targ)], icon = loc_marker) %>%
          addMarkers(lng = unlist(lng_true)[length(lng_true)], lat = unlist(lat_true)[length(lat_true)], icon = loc_marker_green) %>%
          addCircles(lng = unlist(lng_true)[length(lng_true)], lat = unlist(lat_true)[length(lat_true)], radius = accuracy_rad)
      }
    }
    if (length(lng_targ) == 0 && length(lng_true) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>% #Displaying last answer of the player
          addMarkers(lng = unlist(lng_true)[length(lng_true)], lat = unlist(lat_true)[length(lat_true)], icon = loc_marker_green) %>%
          addCircles(lng = unlist(lng_true)[length(lng_true)], lat = unlist(lat_true)[length(lat_true)], radius = accuracy_rad)
      }
    }
    if (length(lng_poly) != 0 && length(lng_ans_obj) == 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addPolygons(lng = unlist(lng_poly), lat = unlist(lat_poly), color = "blue", fillColor = "grey", weight = 2, opacity = 1)
      }
    }
    if (length(lng_poly) != 0 && length(lng_ans_obj) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addMarkers(lng = unlist(lng_ans_obj)[length(lng_ans_obj)], lat = unlist(lat_ans_obj)[length(lat_ans_obj)], icon = loc_marker) %>%
          addPolygons(lng = unlist(lng_poly), lat = unlist(lat_poly), color = "blue", fillColor = "grey", weight = 2, opacity = 1)
      }
    }
    
    mr <- FALSE #Reinitialize variable
    
    
    ### Rendering map
    
    ## Extract virtual environment names which have INIT_TASK events, to exclude other events
    ## here having init-task twice for one task will cause an issue, this migth happen if user uses previous button
    # Get events where type = INIT_TASK 
    init_task_indices <- which(data[[1]]$events$type == "INIT_TASK") 
    message("----- init_task_indices: ")
    print(init_task_indices)
    
    if (is.null(data[[1]]$events$task$virEnvType[init_task_indices[num_value_num()]])) {
      # 1. Real-world map: just use map_shown as built above
      map_rv(map_shown)
    } else {
      
      # 2. To render virtual environment map
      # Extract virEnvName and floor if 3d building for those events 
      virEnvLayers <- character(0)      # for virtual environment layers / images names
      virEnvNames <- character(0)       # for virtual environemnt names
      sapply(
        init_task_indices, function(i) { 
          if (!is.null(data[[1]]$events$task$virEnvType[i])) {
            val =  data[[1]]$events$task$virEnvType[i]
            virEnvNames <<- c(virEnvNames, val)
            
            if (!is.null(data[[1]]$events$task$floor[i]) && !is.na(data[[1]]$events$task$floor[i])) {
              virEnvLayers <<- c(virEnvLayers, paste0(val , "_", data[[1]]$events$task$floor[i])) 
            } 
            else
              virEnvLayers <<- c(virEnvLayers, val) 
          } 
          else 
          { NA } 
        }
      )
      
      # Load virtual environment properties from JSON
      # virEnvsProperties <- fromJSON("www/virEnvsProperties.json");
      virEnvsProperties <- jsonlite::fromJSON(
        file.path(getwd(), "www", "virEnvsProperties.json")
      )
      
      # Build virtual environment map once
      map_virtual <- {
        # Default empty map
        map_shown <- leaflet() %>%
          addTiles() %>%
          setView(lng = 7, lat = 51, zoom = 20)
        
        # Conditions
        if (mr == TRUE ||
            length(ans) <= num_value_num() ||
            (length(lng_targ) == 0 && length(lng_true) == 0 && t == "theme-loc") ||
            (length(long) == 0 && length(traj_lat) == 0 &&
             (t %in% c("nav-flag", "nav-text", "nav-arrow", "nav-photo")))) {
          
          map_shown <- leaflet() %>%
            addTiles() %>%
            setView(lng = 7, lat = 51, zoom = 20)
        }
        
        if (length(long) != 0 && length(traj_lat) == 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addMarkers(
              lng = unlist(long)[1],
              lat = unlist(lati)[1],
              icon = loc_marker_green
            ) %>%
            addCircles(
              lng = unlist(long)[1],
              lat = unlist(lati)[1],
              radius = accuracy_rad,
              opacity = 0.5
            )
        }
        
        if (length(long) != 0 && length(traj_lat) != 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addMarkers(
              lng = unlist(long)[1],
              lat = unlist(lati)[1],
              icon = loc_marker_green
            ) %>%
            addCircles(
              lng = unlist(long)[1],
              lat = unlist(lati)[1],
              radius = accuracy_rad,
              opacity = 0.5
            ) %>%
            addPolylines(
              lng = unlist(traj_lng),
              lat = unlist(traj_lat),
              color = "red", weight = 2, opacity = 1, stroke = TRUE
            )
        }
        
        if (length(dr_point_lng) != 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addPolylines(
              lng = unlist(dr_point_lng),
              lat = unlist(dr_point_lat),
              color = "red", weight = 2, opacity = 1, stroke = TRUE
            )
        }
        
        if (length(lng_targ) != 0 && length(lng_true) != 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addMarkers(
              lng = tail(unlist(lng_targ), 1),
              lat = tail(unlist(lat_targ), 1),
              icon = loc_marker
            ) %>%
            addMarkers(
              lng = tail(unlist(lng_true), 1),
              lat = tail(unlist(lat_true), 1),
              icon = loc_marker_green
            ) %>%
            addCircles(
              lng = tail(unlist(lng_true), 1),
              lat = tail(unlist(lat_true), 1),
              radius = accuracy_rad
            )
        }
        
        if (length(lng_targ) == 0 && length(lng_true) != 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addMarkers(
              lng = tail(unlist(lng_true), 1),
              lat = tail(unlist(lat_true), 1),
              icon = loc_marker_green
            ) %>%
            addCircles(
              lng = tail(unlist(lng_true), 1),
              lat = tail(unlist(lat_true), 1),
              radius = accuracy_rad
            )
        }
        
        if (length(lng_poly) != 0 && length(lng_ans_obj) == 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addPolygons(
              lng = unlist(lng_poly),
              lat = unlist(lat_poly),
              color = "blue", fillColor = "grey", weight = 2, opacity = 1
            )
        }
        
        if (length(lng_poly) != 0 && length(lng_ans_obj) != 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addMarkers(
              lng = tail(unlist(lng_ans_obj), 1),
              lat = tail(unlist(lat_ans_obj), 1),
              icon = loc_marker
            ) %>%
            addPolygons(
              lng = unlist(lng_poly),
              lat = unlist(lat_poly),
              color = "blue", fillColor = "grey", weight = 2, opacity = 1
            )
        }
        
        
        if (!is.na(dir_ok_idx) && t == "theme-direction") {
          evts <- data[[1]]$events
          
          lon0 <- num(evts$position$coords$longitude[dir_ok_idx])
          lat0 <- num(evts$position$coords$latitude[dir_ok_idx])
          
          ans_b <- get_answer_bearing(dir_ok_idx, evts)   # player's FINAL answer
          cor_b <- get_correct_bearing(dir_ok_idx, evts)  # correct direction
          
          if (is.finite(lon0) && is.finite(lat0) && !is.na(ans_b)) {
            
            # Make arrow smaller if you want
            A <- arrow_lines(lon0, lat0, ans_b, len_m = 18, head_m = 5, head_ang = 25)
            
            map_shown <- map_shown %>%
              addMarkers(lng = lon0, lat = lat0, icon = loc_marker) %>%
              
              # Player FINAL answer arrow (BLUE)
              addPolylines(lng = A$main$lng,  lat = A$main$lat,  color = "blue",  weight = 4, opacity = 1) %>%
              addPolylines(lng = A$left$lng,  lat = A$left$lat,  color = "blue",  weight = 4, opacity = 1) %>%
              addPolylines(lng = A$right$lng, lat = A$right$lat, color = "blue",  weight = 4, opacity = 1)
            
            # Correct direction arrow (GREEN) - optional
            # if (!is.na(cor_b)) {
            #   C <- arrow_lines(lon0, lat0, cor_b, len_m = 18, head_m = 5, head_ang = 25)
            #   
            #   map_shown <- map_shown %>%
            #     addPolylines(lng = C$main$lng,  lat = C$main$lat,  color = "green", weight = 4, opacity = 0.9) %>%
            #     addPolylines(lng = C$left$lng,  lat = C$left$lat,  color = "green", weight = 4, opacity = 0.9) %>%
            #     addPolylines(lng = C$right$lng, lat = C$right$lat, color = "green", weight = 4, opacity = 0.9)
            # }
            
            mr <- FALSE
          }
        }
        
        
        
        
        
        
        # Add overlay with zIndex control
        map_shown %>%
          htmlwidgets::onRender("
              function(el, x, data) {
                var map = this;
                var task_number = data.task_number;
                var virEnvName = data.virEnvName;
                var virEnvLayer = data.virEnvLayer;

                // console.log('task_number from R:', task_number);
                // console.log('virEnvName from R:', virEnvName);
                // console.log('virEnvLayer from R:', virEnvLayer);

                // Set min and max zoom levels
                map.options.minZoom = 17;
                map.options.maxZoom = 20;
                map.on('zoomend', function() {
                  console.log('Current zoom level:', map.getZoom());
                });
                  
                // Define imageUrl variable
                var imageUrl;
                if (virEnvName !== null && virEnvName !== undefined && virEnvName !== 'NA') {
                  imageUrl = 'assets/vir_envs_layers/' + virEnvLayer + '.png';
                } else {
                  imageUrl = 'assets/vir_envs_layers/VirEnv_1.png';
                }

                // ########################
                // overlayCoords: 4 corners of where the image should appear
                var overlayCoords = data.virEnvsProperties[virEnvName].overlayCoords;

                // Compute SW/NE bounds from overlayCoords
                var lats = overlayCoords.map(c => c[0]);
                var lngs = overlayCoords.map(c => c[1]);
                var sw = [Math.min(...lats), Math.min(...lngs)];
                var ne = [Math.max(...lats), Math.max(...lngs)];

                // ########################
                // bounds: constrain map panning/zooming to these bounds
                var mapBounds = data.virEnvsProperties[virEnvName].bounds;

                // Add image overlay
                var overlay = L.imageOverlay(imageUrl, [sw, ne], { zIndex: 10 }).addTo(this);

                // Constrain map to bounds
                this.setMaxBounds(mapBounds);

                // Fit map view to overlay
                this.fitBounds([sw, ne]);
                }
            ", data = list(task_number = num_value_num(),
                           virEnvName = virEnvNames[num_value_num()],
                           virEnvLayer = virEnvLayers[num_value_num()],
                           virEnvsProperties = virEnvsProperties)
          )
      }
      
      # Store in reactive value
      map_rv(map_virtual)
    }
      # output$map <- renderLeaflet({
      #   req(map_rv())
      #   map_rv()
      # })
      
      # Convert abbreviation for type task
      if (!is.na(t)) {
        if (t == "nav-flag") {
          t <- "Navigation to flag"
        }
        if (t == "nav-arrow") {
          t <- "Navigation with arrow"
        }
        if (t == "nav-photo") {
          t <- "Navigation via photo"
        }
        if (t == "nav-text") {
          t <- "Navigation via text"
        }
        if (t == "theme-loc") {
          t <- "Self location"
        }
        if (t == "theme-object") {
          t <- "Object location"
        }
        if (t == "theme-direction") {
          t <- "Direction determination"
        }
        if (t == "free") {
          t <- "Free"
        }
        if (t == "info") {
          t <- "Information"
        }
        if (t == "") {
          t <- "No task exists with this number"
        }
      }
      
      output$mapLegend <- renderText({paste("Task type:",t)})
      
      #Download map
      # output$downloadMap <- downloadHandler(
      #   filename = function() {
      #     paste("map_", Sys.Date(), ".html", sep="")
      #   },
      #   content = function(file) {
      #     m <- saveWidget(map_shown, file = file, selfcontained = TRUE)
      #   }
      # )
      # 
      
      #photo code starts---------------------
      cou <- 1 #counter
      pict <- list()
      ans_photo <- list()
      
      for (i in 1:(length(id)-1)) {
        if ((!is.na(id[i]) && (i != 1) && (id[i] != id[i + 1])) || i == (length(id) - 1)) {
          cou <- cou + 1
          pict <- append(pict, unlist(data[[1]]$events$task$question$photo[[i]]))
          ans_photo <- append(ans_photo, unlist(data[[1]]$events$answer$photo[[i]]))
        }
      }
      
      if (length(pict) != 0) { #Photos in assignment
        if (num_value_num() <= length(pict) && !is.na(pict[[num_value_num()]]) && pict[[num_value_num()]] != "") {
          # Render photo display with download buttons
          output$photo_display <- renderUI({
            
            photo_url <- pict[[num_value_num()]]
            
            output[["download_image"]] <- downloadHandler(
              filename = function() {
                paste("image_", t, ".jpg", sep = "")
              },
              content = function(file) {
                download.file(photo_url, file, mode = "wb")
              }
            )
            
            # Create a flex container for images
            div(style = "display: flex; flex-wrap: wrap; gap: 20px;",
                tagList(
                  tags$div(style = "flex: 0 1 200px; display: inline-block; text-align: center;",
                           tags$h4(paste("Assignment for", t)),
                           tags$img(src = photo_url, height = "500px", style = "margin: 10px; border: 1px solid #ccc;"),
                           downloadButton("download_image"), label = "Download", class = "btn btn-primary", style = "margin-top: 10px;")
                )
            )
          })
        }
      }
      
      if (length(ans_photo) != 0) { #Photos in answer
        if (num_value_num() <= length(ans_photo) && !is.na(ans_photo[[num_value_num()]]) && ans_photo[[num_value_num()]] != "") {
          # Render photo display with download buttons
          output$photo_display <- renderUI({
            
            photo_ans_url <- ans_photo[[num_value_num()]]
            
            output[["download_image_2"]] <- downloadHandler(
              filename = function() {
                paste("image_", t, ".jpg", sep = "")
              },
              content = function(file) {
                download.file(photo_ans_url, file, mode = "wb")
              }
            )
            
            # Create a flex container for images
            div(style = "display: flex; flex-wrap: wrap; gap: 20px;", 
                tagList(
                  tags$div(style = "flex: 0 1 200px; display: inline-block; text-align: center;",
                           tags$h4(paste("Answer for", t)),
                           tags$img(src = photo_ans_url, height = "500px", style = "margin: 10px; border: 1px solid #ccc;"),
                           downloadButton("download_image_2"), label = "Download", class = "btn btn-primary", style = "margin-top: 10px; background-color: #0CD1E8 ")
                )
            )
          })
        }
      }
      
      if (length(ans_photo) == 0 && length(pict) == 0) {
        output$photo_display <- renderUI({
          "No photos for this game"
        })
      }
      
      if (length(pict) != 0 && num_value_num() > length(pict)) {
        output$photo_display <- renderUI({
          "No task exists with this number"
        })
      }
      
      if (length(pict) != 0 && length(ans_photo) != 0) {
        if (num_value_num() <= length(ans_photo) && (is.na(ans_photo[[num_value_num()]]) || ans_photo[[num_value_num()]] == "") && (is.na(pict[[num_value_num()]]) || pict[[num_value_num()]] == "")) {
          output$photo_display <- renderUI({
            "No photos for this task"
          })
        }
      }
      if (length(pict) == 0 && length(ans_photo) != 0) {
        if (num_value_num() <= length(ans_photo) && (is.na(ans_photo[[num_value_num()]]) || ans_photo[[num_value_num()]] == "")) {
          output$photo_display <- renderUI({
            "No photos for this task"
          })
        }
      }
      if (length(pict) != 0 && length(ans_photo) == 0) {
        if (num_value_num() <= length(pict) && (is.na(pict[[num_value_num()]]) || pict[[num_value_num()]] == "")) {
          output$photo_display <- renderUI({
            "No photos for this task"
          })
        }
      }
      
      #photo code ends here-----------------------------------
    
  })
  
  #####End of big table
  
  #####multiple files - (4th and 5th menus)
  observeEvent(req(input$selected_multiple_files, num_value_num()), {
    cores <- data.frame(Name = c(), Correct = c(), Answer = c(), Error = c())
    ngts <- data.frame(Name = c(), Correct = c(), Time = c(), Distance = c())
    
    sum_cor <- list()
    sum_incor <- list()
    
    data2 <- load_multiple() #load multiple json
    data <- loaded_json() #load one json
    
    id <- data[[1]]$events$task[["_id"]]
    type_task <- data[[1]]$events$task$type
    cat_task <- data[[1]]$events$task$category
    t <- "" #type task
    cou <- 1 #counter
    
    
    #Recovering type task
    for (i in 1:(length(id)-1)) {
      if (!is.na(cat_task[i])) {
        if ((cat_task[i] == "nav") && (cou == num_value_num())) {
          t <- type_task[i]
        }
        if ((cat_task[i] == "theme") && (cou == num_value_num())) {
          t <- type_task[i]
        }
        if ((cat_task[i] == "info") && (cou == num_value_num())) {
          t <- cat_task[i]
        }
      }
      if ((!is.na(id[i]) && (i != 1) && (id[i] != id[i + 1])) || i == (length(id) - 1)) {
        cou <- cou + 1
      }
    }
    
    for (i in 1:length(data2)) {
      
      id <- data2[[i]]$events$task[["_id"]]
      
      ans2 <- list()
      typ2 <- list()
      dist1_m_new <- list()
      dist1_deg_new <- list()
      dist2_deg_new <- list()
      
      tps <- data2[[i]]$events$timestamp
      time1 <- as.POSIXct(tps[1], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
      tmp2 <- list()
      
      ev <- data2[[i]]$events$type #Name of the event
      tries = 0
      try2 <- list()
      
      for (j in 1:(length(id) - 1)) {
        if (ev[j] == "ON_OK_CLICKED") {
          tries <- tries + 1
        }
        if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
          if (length(data2[[i]]$events$answer$correct[j]) != 0) {
            ans2 <- append(ans2, data2[[i]]$events$answer$correct[j])
          } else {
            ans2 <- append(ans2, data2[[i]]$events$correct[j])
          }
          typ2 <- append(typ2, data2[[i]]$events$task$type[j])
          
          # distance in meters
          dist1_m_new <- append(dist1_m_new, data2[[i]]$events$answer$distance[j])
          
          # bearings: use shared helpers
          ans_bearing <- get_answer_bearing(j, data2[[i]]$events)
          cor_bearing <- get_correct_bearing(j, data2[[i]]$events)
          
          dist1_deg_new <- append(dist1_deg_new, ans_bearing)
          dist2_deg_new <- append(dist2_deg_new, cor_bearing)
          
          time2 <- as.POSIXct(tps[j], format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
          tmp2 <- append(tmp2, floor(as.numeric(time2 - time1, units = "secs")))
          time1 <- as.POSIXct(tps[j+1], format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
          
          try2 <- append(try2, tries)
          tries = 0
        }
      }
      
      
      
      for (k in 1:length(ans2)) {
        if (!is.na(typ2[[k]][1]) && (typ2[[k]][1] == "nav-photo" || typ2[[k]][1] == "nav-arrow" || typ2[[k]][1] == "nav-text") && tmp2[[k]][1] != 0 && is.na(ans2[[k]][1])) {
          ans2[[k]][1] <- "Correct"
        }
        if (!is.na(ans2[[k]][1]) && ans2[[k]][1] == TRUE) {
          ans2[[k]][1] <- "Correct"
        }
        if (!is.na(ans2[[k]][1]) && ans2[[k]][1] == FALSE) {
          ans2[[k]][1] <- "Incorrect"
        }
      }
      
      #Taking the player trajectory
      traj_lng <- list()
      traj_lat <- list()
      accuracy <- list()
      task_number <- data2[[i]]$waypoints$taskNo #Task number
      for (k in 1:length(task_number)) {
        if ((task_number[k] == num_value_num())) {
          traj_lng <- append(traj_lng, data2[[i]]$waypoints$position$coords$longitude[k])
          traj_lat <- append(traj_lat, data2[[i]]$waypoints$position$coords$latitude[k])
          if (length(data2[[i]]$waypoints$position$coords$accuracy) != 0) {
            accuracy <- append(accuracy, data2[[i]]$waypoints$position$coords$accuracy[k]) #accuracy on coordinates
          }
          else {
            accuracy <- append(accuracy, 1)
          }
        }
      }
      
      #Computing distance traveled
      deg_to_rad <- pi/180
      R <- 6.378e6
      d_total <- 0 #counter
      if (length(traj_lng) > 1) {
        for (k in 1:(length(traj_lng)-1)) {
          if (!is.na(traj_lng[[k]][1])) {
            lat_1 <- traj_lat[[k]][1]
            lng_1 <- traj_lng[[k]][1]
            lat_2 <- traj_lat[[k+1]][1]
            lng_2 <- traj_lng[[k+1]][1]
            d <- R * acos(sin(lat_1*deg_to_rad)*sin(lat_2*deg_to_rad) + cos(lat_1*deg_to_rad)*cos(lat_2*deg_to_rad)*cos((lng_2 - lng_1)*deg_to_rad))
            if (d <= 10 && accuracy[[k]] <= 20) {
              d_total <- d_total + d
            }
          }
        }
      }
      #print(d_total)
      
      #Computing again time (with way points)
      time_waypoints_new <- list()
      tps_waypoints <- data2[[i]]$waypoints$timestamp
      for (k in 1:length(tmp2)) {
        if (tmp2[[k]] == 0) {
          for (n in 1:length(task_number)) {
            if (task_number[n] == k) {
              time_waypoints_new <- append(time_waypoints_new, tps_waypoints[[n]])
            }
          }
          if (length(time_waypoints_new) != 0) {
            x <- as.POSIXct(time_waypoints_new[[1]], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
            y <- as.POSIXct(time_waypoints_new[[length(time_waypoints_new)]], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
            delta <- floor(as.numeric(y - x, units = "secs"))
            tmp2[[k]] <- delta
          }
        }
        time_waypoints_new <- list()
      }
      
      #TIME VS DISTANCE
      if (length(ans2) >= num_value_num() && unlist(tmp2[[num_value_num()]][1]) != 0) {
        if (is.na(ans2[[num_value_num()]][1]) && grepl(pattern = "nav", t)) {
          ans2[[num_value_num()]][1] <- "Target not reached" #navigation task
        } 
        if (length(dist1_m_new) != 0 && !is.na(unlist(dist1_m_new)[[num_value_num()]][1])) { #Distance to the correct answer or not
          ngt <- cbind(Name = data2[[i]]$players[1], Correct = unlist(ans2[[num_value_num()]][1]), Time = paste(unlist(tmp2[[num_value_num()]][1]),"s"), Distance_travelled = paste(round(d_total), "m"), Distance_to_the_correct_answer = paste(round(unlist(dist1_m_new)[[num_value_num()]][1],3),"m"))
          ngts <- rbind(ngts, ngt)
        }
        else {
          ngt <- cbind(Name = data2[[i]]$players[1], Correct = unlist(ans2[[num_value_num()]][1]), Time = paste(unlist(tmp2[[num_value_num()]][1]),"s"), Distance_travelled = paste(round(d_total),"m"), Distance_to_the_correct_answer = NA)
          ngts <- rbind(ngts, ngt)
        }
      }
      
      # Coerce degree lists to numeric and align lengths
      d1deg_new <- num(unlist(dist1_deg_new))
      d2deg_new <- num(unlist(dist2_deg_new))
      
      maxn <- max(length(d1deg_new), length(d2deg_new))
      length(d1deg_new) <- maxn
      length(d2deg_new) <- maxn
      
      deg_error <- angle_diff_deg_vec(d1deg_new, d2deg_new)
      
      
      
      
      # CORRECT & ERRORS
      if (length(d1deg_new) >= num_value_num()) {
        idx <- num_value_num()
        
        if (!is.na(ans2[[idx]][1]) || !is.na(d1deg_new[idx])) {
          err_val   <- deg_error[idx]
          answer_deg <- d1deg_new[idx]
          
          core <- cbind(
            Name   = data2[[i]]$players[1],
            Correct= ans2[[idx]][1],
            Answer = paste(round(answer_deg, 3), "°"),
            Error  = paste(round(err_val,      3), "°")
          )
          
          cores <- rbind(cores, core)
        }
      }
      
      
      #Computing correct or incorrect tries
      if (length(ans2) >= num_value_num()) { #outside index
        if (!is.na(ans2[[num_value_num()]])) { #answer existence
          if (ans2[[num_value_num()]] == "Correct") { #correct answer
            sum_incor <- append(sum_incor, 0) 
            sum_cor <- append(sum_cor, 1)
          }
          else if (ans2[[num_value_num()]] == "Target not reached") { #Target no reached
            sum_incor <- append(sum_incor, 0) 
            sum_cor <- append(sum_cor, 0)
          }
          else { #incorrect answer
            sum_incor <- append(sum_incor, 1)
            sum_cor <- append(sum_cor, 0)
          }
        }
        else { #NA value: indeterminate answer
          sum_incor <- append(sum_incor, 0)
          sum_cor <- append(sum_cor, 0)
        }
      }
    }
    
    #Change the columns names in tables
    if (ncol(ngts) >= 4) {
      colnames(ngts)[4] <- "Distance travelled"
    }
    if (ncol(ngts) == 5) {
      colnames(ngts)[5] <- "Distance to the correct answer"
    }
    
    
    #Graphic on time
    if (length(ngts) != 0) {
      
      oral <- list()
      oral2 <- list()
      seconds <- strsplit(ngts[,3], " s")
      meters <- strsplit(ngts[,4], " m")
      for (k in 1:length(seconds)) {
        oral <- append(oral, as.numeric(seconds[[k]]))  #Convert in numeric format to show on graphic
        oral2 <- append(oral2, as.numeric(meters[[k]]))
      }
      
      df_player <- data.frame(time = unlist(oral), distance = unlist(oral2), players = ngts[,1])
      time_chart <- ggplot(df_player, aes(x = time, y = distance, fill = players)) +
        geom_point(size=4, shape=22) +
        labs(title = paste("Time chart of task:", t), x = "Time for this task in seconds", y = "Distance travelled in metres")
      
      for (i in 1:length(df_player$time)) {
        if (df_player$time[i] == 0) {
          sum_cor[i] <- 0
          sum_incor[i] <- 0
        }
      }
    }
    else {
      time_chart <- ggplot() +
        theme_void() +
        labs(title = "You didn't reply for this task")
    }
    
    
    output$time_chart <- renderPlot({
      time_chart
    })
    
    #Download time chart
    output$save_time_chart <- downloadHandler(
      filename = function(){
        paste("time_chart_", Sys.Date(), ".png", sep="")
      },
      content = function(file){
        png(file)
        print(time_chart)
        dev.off()
      }
    )
    
    
    corr <- sum(unlist(sum_cor)) #Number of correct answer for one task - only the last answer per player
    incorr <- sum(unlist(sum_incor)) #Number of incorrect answer for one task - only the last answer per player
    rel <- c(corr,incorr)
    answer_vect <- c("Correct","Incorrect")
    df_pie <- data.frame(Answers = answer_vect, value = rel)
    #print(df_pie)
    
    
    #pie_chart (Correct & Incorrect)
    if (corr == 0 && incorr == 0) {
      pie_chart <- ggplot() +
        theme_void() +
        labs(title = "You didn't reply for this task")
    }
    else {
      # Calculate percentages
      df_pie$percentage <- round(df_pie$value / sum(df_pie$value) * 100, 1)
      df_pie$label <- paste0(df_pie$Answers, ": ", df_pie$percentage, "%")
      
      # Count how many players were selected
      num_players <- length(input$selected_multiple_files)
      
      pie_chart <- ggplot(df_pie, aes(x = "", y = value, fill = Answers)) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        geom_text(aes(label = label), 
                  position = position_stack(vjust = 0.5), 
                  color = "white", size = 5, fontface = "bold") +
        scale_fill_manual(values = c("#3C8D53","#BE2A3E")) +
        theme_void() +
        labs(
          title = paste("Pie chart of task:", t),
          subtitle = paste("\n\nNo. of Players Selected:", num_players)
        ) +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5)
        )
    }
    
    
    #Two outputs because two conditions in UI
    output$pie_chart <- renderPlot({
      pie_chart
    })
    
    output$pie_chart2 <- renderPlot({
      pie_chart
    })
    
    #Download pie chart
    output$save_picture <- downloadHandler(
      filename = function(){
        paste("pie_chart_", Sys.Date(), ".png", sep="")
      },
      content = function(file){
        png(file)
        print(pie_chart)
        dev.off()
      }
    )
    
    output$save_picture2 <- downloadHandler(
      filename = function(){
        paste("pie_chart_", Sys.Date(), ".png", sep="")
      },
      content = function(file){
        png(file)
        print(pie_chart)
        dev.off()
      }
    )
    
    #Delete the last column if it's empty 
    counter <- 0
    if (length(ngts) != 0) {
      for (k in 1:length(ngts[,5])) { 
        if (is.na(ngts[k,5])) {
          counter <- counter + 1
        }
      }
      if (counter == length(ngts[,5])) {
        ngts <- select(ngts, -"Distance to the correct answer")
      }
    }
    
    
    #TIME VS DISTANCE Table
    output$cmp_table1 <- renderTable(
      ngts
    )
    
    #CORRECT & ERRORS Table
    output$cmp_table2 <- renderTable(
      cores
    )
    
    #Convert abbreviation for type task
    if (!is.na(t)) {
      if (t == "nav-flag") {
        t <- "Navigation to flag"
      }
      if (t == "nav-arrow") {
        t <- "Navigation with arrow"
      }
      if (t == "nav-photo") {
        t <- "Navigation via photo"
      }
      if (t == "nav-text") {
        t <- "Navigation via text"
      }
      if (t == "theme-loc") {
        t <- "Self location"
      }
      if (t == "theme-object") {
        t <- "Object location"
      }
      if (t == "theme-direction") {
        t <- "Direction determination"
      }
      if (t == "free") {
        t <- "Free"
      }
      if (t == "info") {
        t <- "Information"
      }
      if (t == "") {
        t <- "No task exists with this number"
      }
    }
    
    output$tabLegend <- renderText({paste("Task type:",t)})
    output$graphLegend <- renderText({paste("Task type:",t)})
    
    #Save analyse tables
    output$save_table1 <- downloadHandler(
      filename = function(){
        paste("time_dist_table_", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(ngts, file)
      }
    )
    
    output$save_table2 <- downloadHandler(
      filename = function(){
        paste("ans_err_table_", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(cores, file)
      }
    )
  })
  
  
  #Upload button reading - Loaded json
  observeEvent(req(input$uploaded_json_file, num_value_num()),{
    req(num_value_num() && num_value_num() != 0 && num_value_num() > 0)
    df_react <- reactiveVal()
    
    files <- list.files(json_dir, pattern = "\\.json$", full.names = FALSE)
    files <- grep(input$selected_games, files, ignore.case = TRUE, value = TRUE)
    
    choices <- c("All Files" = "ALL", files)
    
    updatePickerInput(session, "selected_files",
                      choices = choices,
                      selected = NULL)
    
    output$info_download <- renderText({
      ""
    })
    
    
    data <- uploaded_json() #load one json
    
    id <- data[[1]]$events$task[["_id"]]
    #print(id)
    
    #Building columns on types, answers and assignments
    typ <- list()   #type of task
    cons <- list()  #consignes -> english : instructions
    ans <-list()   #answers -> english -> english
    
    #csg -> instructions in english
    csg <- data[[1]]$events$task$question$text
    ev <- data[[1]]$events$type #Name of the event
    pict_quest <- data[[1]]$events$task$question$photo
    ans_type <- data[[1]]$events$task$answer$type #what is required as the answer type
    for (j in 1:(length(id) - 1)) {
      if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
        if (!is.na(id[j]) && ans_type[j] == "TEXT") {
          if (!is.na(data[[1]]$events$answer$correct[j]) && data[[1]]$events$answer$correct[j] == "TRUE" ) {
            ans <- append(ans, paste("Correct", data[[1]]$events$answer$text[j])) #add text in input in the answer column
          }
          if (!is.na(data[[1]]$events$answer$correct[j]) && data[[1]]$events$answer$correct[j] == "FALSE") {
            ans <- append(ans, paste("Incorrect", data[[1]]$events$answer$text[j])) #add text in input in the answer column
          }
          if (is.na(data[[1]]$events$answer$correct[j])) {
            ans <- append(ans, NA)
          }
        }
        else if (!is.na(id[j]) && ans_type[j] == "MULTIPLE_CHOICE_TEXT") {
          if (!is.na(data[[1]]$events$answer$correct[j]) && data[[1]]$events$answer$correct[j] == "TRUE") {
            ans <- append(ans, paste("Correct", data[[1]]$events$answer$selectedChoice$value[j])) #add the validated answer in the answer column
          }
          if (!is.na(data[[1]]$events$answer$correct[j]) && data[[1]]$events$answer$correct[j] == "FALSE") {
            ans <- append(ans, paste("Incorrect", data[[1]]$events$answer$selectedChoice$value[j])) #add the validated answer in the answer column
          }
          if (is.na(data[[1]]$events$answer$correct[j])) {
            ans <- append(ans, NA)
          }
        }
        else if (!is.na(id[j]) && ans_type[j] == "NUMBER") {
          if (!is.na(data[[1]]$events$answer$correct[j]) && data[[1]]$events$answer$correct[j] == "TRUE") {
            ans <- append(ans, paste("Correct", data[[1]]$events$answer$numberInput[j])) #add the validated answer in the answer column
          }
          if (!is.na(data[[1]]$events$answer$correct[j]) && data[[1]]$events$answer$correct[j] == "FALSE") {
            ans <- append(ans, paste("Incorrect", data[[1]]$events$answer$numberInput[j])) #add the validated answer in the answer column
          }
          if (is.na(data[[1]]$events$answer$correct[j])) {
            ans <- append(ans, NA)
          }
        }
        else {
          if (length(data[[1]]$events$answer$correct[j]) != 0) {
            if (!is.na(data[[1]]$events$answer$correct[j]) && data[[1]]$events$answer$correct[j] == "TRUE") {
              ans <- append(ans, "Correct")
            }
            if (!is.na(data[[1]]$events$answer$correct[j]) && data[[1]]$events$answer$correct[j] == "FALSE") {
              ans <- append(ans, "Incorrect")
            }
            if (is.na(data[[1]]$events$answer$correct[j])) {
              ans <- append(ans, NA)
            }
          }
          else {
            if (!is.na(data[[1]]$events$correct[j]) && data[[1]]$events$correct[j] == "TRUE") {
              ans <- append(ans, "Correct")
            }
            if (!is.na(data[[1]]$events$correct[j]) && data[[1]]$events$correct[j] == "FALSE") {
              ans <- append(ans, "Incorrect")
            }
            if (is.na(data[[1]]$events$correct[j])) {
              ans <- append(ans, NA)
            }
          }
        }
        typ <- append(typ, data[[1]]$events$task$type[j])
        cons <- append(cons, csg[j])
      }
    }
    # print(cons)
    # print(typ)
    #print(cbind(data[[1]]$events$task$type, data[[1]]$events$correct,data[[1]]$events$answer$correct))
    #print(ans)
    
    # Distance to the correct answer
    dist1_m   <- list()   # meters
    dist1_deg <- list()   # player's bearing (deg)
    dist2_deg <- list()   # correct bearing (deg)
    
    for (j in 1:(length(id) - 1)) {
      if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
        # distance in meters (as is)
        dist1_m <- append(dist1_m, data[[1]]$events$answer$distance[j])
        
        # NEW: 
        ans_bearing <- get_answer_bearing(j, data[[1]]$events)
        cor_bearing <- get_correct_bearing(j, data[[1]]$events)
        
        dist1_deg <- append(dist1_deg, ans_bearing)
        dist2_deg <- append(dist2_deg, cor_bearing)
      }
    }
    
    
    #num <- function(x) suppressWarnings(as.numeric(x))
    
    # d1m   <- num(unlist(dist1_m))
    # d1deg <- num(unlist(dist1_deg))
    # d2deg <- num(unlist(dist2_deg))
    # 
    # maxn <- max(length(d1m), length(d1deg), length(d2deg))
    # length(d1m)   <- maxn
    # length(d1deg) <- maxn
    # length(d2deg) <- maxn
    # 
    # rds <- cbind(d1m, dist_deg = angle_diff_deg_vec(d1deg, d2deg))
    # #print(rds)
    # 
    # 
    # 
    # ####sometimes we don't need to merge the column
    # if (ncol(rds) == 2) {
    #   rds[is.na(rds)] <- 0
    #   dist <- c(rds[,1]+rds[,2])
    #   dist[dist == 0] <- NA
    # }
    # else {
    #   dist <- rds
    # }
    # 
    # if (length(dist) != 0) {
    #   dist <- round(dist,2)
    # }
    # 
    # #Add unities on the last column
    # for (i in 1:length(typ)) {
    #   if (!is.na(typ[[i]]) && !is.na(dist[[i]]) && typ[[i]] == "theme-direction"){
    #     dist[[i]] <- paste(dist[[i]], "°")
    #   }
    #   if (!is.na(typ[[i]]) && !is.na(dist[[i]]) && (typ[[i]] == "nav-flag" || typ[[i]] == "theme-loc")) {
    #     dist[[i]] <- paste(dist[[i]], "m")
    #   }
    # }
    
    task_type <- as.character(unlist(typ))
    n <- length(task_type)
    
    d1m   <- suppressWarnings(as.numeric(unlist(dist1_m)))
    d1deg <- num(unlist(dist1_deg))
    d2deg <- num(unlist(dist2_deg))
    
    length(d1m)   <- n
    length(d1deg) <- n
    length(d2deg) <- n
    
    deg_err <- angle_diff_deg_vec(d1deg, d2deg)
    
    # logical masks (TRUE/FALSE length n)
    mask_dir <- !is.na(task_type) & task_type == "theme-direction"
    mask_m   <- !is.na(task_type) & task_type %in% c("nav-flag", "theme-loc")
    
    dist_num <- rep(NA_real_, n)
    dist_num[mask_dir] <- deg_err[mask_dir]
    dist_num[mask_m]   <- d1m[mask_m]
    
    dist_num <- round(dist_num, 2)
    
    dist_txt <- rep(NA_character_, n)
    dist_txt[mask_dir & !is.na(dist_num)] <- paste0(dist_num[mask_dir & !is.na(dist_num)], " °")
    dist_txt[mask_m   & !is.na(dist_num)] <- paste0(dist_num[mask_m   & !is.na(dist_num)], " m")
    
    dist <- dist_txt
    
    
    
    
    
    
    
    
    
    
    #print(dist)
    
    
    
    
    
    
    #Computing time spent on a task
    tps <- data[[1]]$events$timestamp
    time1 <- as.POSIXct(tps[1], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
    tmp <- list()
    for (j in 1:(length(id) - 1)) {
      if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
        time2 <- as.POSIXct(tps[j], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
        tmp <- append(tmp, paste(floor(as.numeric(time2 - time1, units = "secs")),"s"))
        time1 <- as.POSIXct(tps[j+1], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
      }
    }
    
    #print(tmp)
    #Computing tries number for each task
    tries = 0
    try <- list()
    for (j in 1:(length(id) - 1)) {
      if (ev[j] == "ON_OK_CLICKED") { #Answer when the player clicks on OK
        tries <- tries + 1
      }
      if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
        try <- append(try, tries)
        tries = 0
      }
    }
    
    #print(try)
    
    
    #Compute for maps
    long <- list()
    lati <- list()
    cou <- 1 #counter
    mr <- FALSE #Print an empty map or not
    coor <- data[[1]]$events$task$answer$position$geometry$coordinates #Position answer nav tasks
    
    lng_targ <- list()
    lat_targ <- list()
    lng_true <- list()
    lat_true <- list()
    targ <- data[[1]]$events$answer$clickPosition #Position answer theme localisation task 
    coor_true <- data[[1]]$events$position$coords
    
    #Position for free task
    dr_point_lat <- list()
    dr_point_lng <- list()
    drawing_point_lat <- data[[1]]$events$clickPosition$latitude
    drawing_point_lng <- data[[1]]$events$clickPosition$longitude
    #print(drawing_point_lat)
    
    type_task <- data[[1]]$events$task$type
    cat_task <- data[[1]]$events$task$category
    accuracy_radius <- data[[1]]$events$task$settings$accuracy #For theme-loc or navigation tasks
    accuracy_rad <- 0
    ev <- data[[1]]$events$type #Name of the event
    t <- ""
    
    
    #Print Polygons
    sel_polygon <- data[[1]]$events$task$question$geometry$feature
    lng_poly <- list()
    lat_poly <- list()
    lng_ans_obj <- list()
    lat_ans_obj <- list()
    
    #Recovering answers position for the map
    for (i in 1:(length(id)-1)) {
      if (!is.na(cat_task[i])) {
        if ((cat_task[i] == "nav") && (cou == num_value_num())) {
          long <- append(long, coor[[i]][1])
          lati <- append(lati, coor[[i]][2])
          accuracy_rad <- accuracy_radius[[i]]
          t <- type_task[i]
        }
        if ((cat_task[i] == "info") && (cou == num_value_num())) {
          mr <- TRUE #Showing an empty map
          t <- cat_task[i]
        }
      }
      if (!is.na(type_task[i])) { #target point for theme task
        if ((type_task[i] == "theme-loc") && (ev[i] == "ON_OK_CLICKED") && (cou == num_value_num())) {
          lng_targ <- append(lng_targ, targ[[i]][1])
          lat_targ <- append(lat_targ, targ[[i]][2])
          lng_true <- append(lng_true, coor_true$longitude[[i]])
          lat_true <- append(lat_true, coor_true$latitude[[i]])
          accuracy_rad <- accuracy_radius[[i]]
          t <- type_task[i]
        }
        if ((type_task[i] == "theme-loc") && (cou == num_value_num())) { #Always having the task type shown for theme-localisation
          t <- type_task[i]
        }
        
        if (type_task[i] == "theme-direction" && ev[i] == "ON_OK_CLICKED" && cou == num_value_num()) {
          dir_ok_idx <- i
          t <- type_task[i]
        }
        
        #########-DOMINIKA'S game issue solved ######################################
        # safe single values for this event i
        # safe access
        mode_vec <- data[[1]]$events$task$question$mode
        
        mode_i <- if (!is.null(mode_vec) && length(mode_vec) >= i) mode_vec[[i]] else NA_character_
        ans_i  <- if (!is.null(ans_type) && length(ans_type) >= i) ans_type[[i]] else NA_character_
        
        is_theme_direction <-
          !is.na(type_task[i]) && type_task[i] == "theme-direction"
        
        is_theme_object_photo <-
          !is.na(type_task[i]) && type_task[i] == "theme-object" &&
          !is.na(ans_i)        && ans_i == "PHOTO"
        
        is_theme_object_nofeature <-
          !is.na(type_task[i]) && type_task[i] == "theme-object" &&
          !is.na(mode_i)       && mode_i == "NO_FEATURE"
        
        if ((is_theme_object_photo || is_theme_object_nofeature) && cou == num_value_num()) {
          mr <- TRUE
          t  <- type_task[i]
        }
        
        if (is_theme_direction && cou == num_value_num()) {
          t <- type_task[i]
          # DO NOT set mr <- TRUE
        }
        
        
        
        #########-DOMINIKA'S game issue solved ######################################
        
        
        
        
        
        
        # if ((type_task[i] == "theme-direction" || (type_task[i] == "theme-object" && ans_type[[i]] == "PHOTO") || (type_task[i] == "theme-object" && length(data[[1]]$events$task$question$mode) != 0 && data[[1]]$events$task$question$mode[[i]] == "NO_FEATURE")) && cou == num_value_num()) { #tasks that show nothing on the map
        #   mr <- TRUE
        #   t <- type_task[i]
        # }

        
        
        if (type_task[i] == "theme-object" && cou == num_value_num() && ans_type[[i]] == "MAP_POINT") { #tasks that show nothing on the map
          poly <- sel_polygon[[i]]$geometry$coordinates[[1]]
          for (n in 1:(length(poly)/2)) {
            lng_poly <- append(lng_poly, poly[n])
          }
          for (n in (length(poly)/2+1):length(poly)) {
            lat_poly <- append(lat_poly, poly[n])
          }
          t <- type_task[i]
          if (type_task[i] == "theme-object" && (cou == num_value_num())) {
            lng_ans_obj <- append(lng_ans_obj, targ[[i]][1])
            lat_ans_obj <- append(lat_ans_obj, targ[[i]][2])
          }
        }
        if ((type_task[i] == "free") && (cou == num_value_num()) && length(drawing_point_lat) != 0 && !is.na(drawing_point_lat[[i]]) && ans_type[[i]] == "DRAW") {
          dr_point_lat <- append(dr_point_lat, drawing_point_lat[[i]])
          dr_point_lng <- append(dr_point_lng, drawing_point_lng[[i]])
          t <- type_task[i]
        }
        if ((type_task[i] == "free") && (cou == num_value_num()) && (length(drawing_point_lat) == 0 || is.na(drawing_point_lat[[i]])) && ans_type[[i]] == "DRAW") { #correcting error to visualize draw
          mr <- TRUE
          t <- type_task[i]
        }
        if ((type_task[i] == "free") && (cou == num_value_num()) && ans_type[[i]] != "DRAW") {
          mr <- TRUE
          t <- type_task[i]
        }
      }
      if (is.na(type_task[i]) && (i != 1) && (cou == num_value_num())) { #na task
        mr <- TRUE
      }
      if ((!is.na(id[i]) && (i != 1) && (id[i] != id[i + 1])) || i == (length(id) - 1)) {
        cou <- cou + 1
      }
    }
    
    #Print trajectory on the map
    traj_lng <- list()
    traj_lat <- list()
    accuracy <- list()
    task_number <- data[[1]]$waypoints$taskNo #Task number
    for (i in 1:length(task_number)) {
      if (task_number[i] == num_value_num()) {
        traj_lng <- append(traj_lng, data[[1]]$waypoints$position$coords$longitude[i])
        traj_lat <- append(traj_lat, data[[1]]$waypoints$position$coords$latitude[i])
        if (length(data[[1]]$waypoints$position$coords$accuracy) != 0) {
          accuracy <- append(accuracy, data[[1]]$waypoints$position$coords$accuracy[i]) #accuracy on coordinates
        }
        else {
          accuracy <- append(accuracy, 1)
        }
      }
    }
    
    #Compute again time (with way points)
    time_waypoints <- list()
    tps_waypoints <- data[[1]]$waypoints$timestamp
    
    for (k in 1:length(tmp)) {
      if (tmp[[k]] == "0 s") {
        for (i in 1:length(task_number)) {
          if (task_number[i] == k) {
            time_waypoints <- append(time_waypoints, tps_waypoints[[i]])
          }
        }
        if (length(time_waypoints) != 0) {
          x <- as.POSIXct(time_waypoints[[1]], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
          y <- as.POSIXct(time_waypoints[[length(time_waypoints)]], format = "%Y-%m-%dT%H:%M:%OS",tz = "UTC")
          delta <- paste(floor(as.numeric(y - x, units = "secs")),"s")
          tmp[[k]] <- delta
          if (tmp[[k]] != "0 s" && grepl(pattern = "nav", typ[[k]])) {
            ans[[k]] <- "Target not reached"
          }
        }
      }
      time_waypoints <- list()
    }
    
    #print(tmp)
    
    #Compute distance traveled
    deg_to_rad <- pi/180
    R <- 6.378e6
    if (length(traj_lng) > 1) {
      for (k in 1:(length(traj_lng)-1)) {
        if (!is.na(traj_lng[[k]][1])) {
          lat_1 <- traj_lat[[k]][1]
          lng_1 <- traj_lng[[k]][1]
          lat_2 <- traj_lat[[k+1]][1]
          lng_2 <- traj_lng[[k+1]][1]
          d <- R * acos(sin(lat_1*deg_to_rad)*sin(lat_2*deg_to_rad) + cos(lat_1*deg_to_rad)*cos(lat_2*deg_to_rad)*cos((lng_2 - lng_1)*deg_to_rad))
          if (accuracy[[k]] >= 20) {
            traj_lat[k] <- traj_lat[k+1] #Filter GPS coordinate errors
            traj_lng[k] <- traj_lng[k+1]
          }
        }
      }
    }
    
    rg <- cbind(unlist(typ), unlist(cons), unlist(ans), unlist(tmp), unlist(try), unlist(dist))
    #print(rg)
    
    for (i in 1:(nrow(rg))) {
      if (((!is.na(rg[i,1]) && rg[i,1] == "nav-arrow") || (!is.na(rg[i,1]) && rg[i,1] == "nav-text")
           || (!is.na(rg[i,1]) && rg[i,1] == "nav-photo")) && rg[i,4] != "0 s" && is.na(rg[i,3])) {
        rg[i,3] <- "Correct"
        rg[i,5] <- 1
      }
      if (is.na(rg[i,1])) {
        rg[i,1] <- "information"
        rg[i,3] <- NA
        rg[i,5] <- 0
      }
    }
    
    #Delete the last column if it's empty 
    counter_dist <- 0
    for (k in 1:length(dist)) {
      if (is.na(dist[[k]])) {
        counter_dist <- counter_dist + 1
      }
    }
    
    #Build the main table
    if (counter_dist != length(dist)) {
      mat = matrix(rg, ncol = 6, nrow = length(ans))
      
      df <- data.frame(
        Type = mat[,1],
        Assignment = mat[,2],
        Answer = mat[,3],
        Time = mat[,4],
        Tries = mat[,5],
        Error = mat[,6]
      )
      colnames(df)[6] <- "Error in °/m"
    }
    else {
      mat = matrix(rg, ncol = 6, nrow = length(ans)) #Big table without the last column
      
      df <- data.frame(
        Type = mat[,1],
        Assignment = mat[,2],
        Answer = mat[,3],
        Time = mat[,4],
        Tries = mat[,5]
      )
    }
    
    #Name of the player
    #print(data[[1]]$players[1])
    output$player_name <- renderText({
      paste("Player: ", data[[1]]$players[1], sep = "")
    })
    #Overall score
    if (sum(grepl(pattern = "Incorrect", df$Answer)) != 0 || sum(grepl(pattern = "Target", df$Answer)) != 0) { #If incorrect values are in the table
      good <- sum(grepl(pattern = "Correct", df$Answer))
      total <- sum(grepl(pattern = "Incorrect", df$Answer)) + sum(grepl(pattern = "Correct", df$Answer)) + sum(grepl(pattern = "Target", df$Answer))
    }
    else { #if all is correct
      good <- sum(grepl(pattern = "Correct", df$Answer))
      total <- good
    }
    
    output$overall_score <- renderText({
      paste("Overall score: ", good, "/", total, sep = "")
    })
    
    output$player_info_box <- renderUI({
      req(data[[1]]$players[1])
      
      div(id = "inlineDiv",
          style = "margin-bottom: 20px; border: 1px solid #ccc; padding: 10px; border-radius: 5px; background-color: #f9f9f9;",
          h5(textOutput("player_name")),
          h5(textOutput("overall_score"))
      )
    })
    
    df_react(df)
    
    
    
    #PREVIOUSLY OBSERVE BUTTONS - PICKER INPUT 
    # observe({
    #   req(df_react())
    #   choices <- seq_len(nrow(df_react()))
    #   updatePickerInput(session, "num_value_comparison", choices = choices, selected = input$num_value_comparison)
    # })
    
    
    ###############----------Task IDs updation all 4 STARTS-------------################
    observeEvent(df_react(), {
      df <- df_react()
      if (is.null(df) || nrow(df) == 0) return()
      
      n <- nrow(df)
      
      # preserving current Map selection if valid; otherwise default to 1
      cur <- suppressWarnings(as.integer(input$num_value))
      if (is.na(cur) || cur < 1 || cur > n) cur <- 1
      
      # choices are strings for pickerInput
      choice_vec <- as.character(seq_len(n))
      sel <- as.character(cur)
      
      # Push the SAME choices + selected to all four pickers
      for (id in all_ids) {
        updatePickerInput(session, id, choices = choice_vec, selected = sel)
      }
    })
    ###############----------Task IDs updation all 4 STARTS-------------################
    
    output$iris_data <- renderDT({
      df_react()
    })
    
    
    
    
    #-----------all tasks - id checkbox filter starts --------------------------------
    output$task_id_selector <- renderUI({
      req(df_react())
      df <- df_react()
      
      task_ids <- seq_len(nrow(df))   # use row numbers as task IDs
      
      tagList(
        pickerInput(
          "selected_task_ids",
          "Filter by Task ID:",
          choices = task_ids,
          selected = task_ids,   # initially all
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = FALSE,
            `none-selected-text` = "Filter by Task ID: ",
            `width` = '100%',
            container = FALSE,
            size = 10
          )
        )
      )
    })
    
    # Filtered data
    filtered_df <- reactive({
      req(df_react())
      df <- df_react()
      
      if (is.null(input$selected_task_ids) || length(input$selected_task_ids) == 0) {
        return(df)   # if none selected, show all
      }
      
      df[input$selected_task_ids, , drop = FALSE]   # subset by row numbers
    })
    
    # Show table
    output$iris_data <- renderDT({
      filtered_df()
    }, options = list(pageLength = 10))
    
    #---------logic for select/deselect all starts ----------------------
    observeEvent(input$select_all_tasks, {
      req(df_react())
      task_ids <- seq_len(nrow(df_react()))
      updateSelectInput(session, "selected_task_ids", selected = task_ids)
    })
    
    observeEvent(input$deselect_all_tasks, {
      updateSelectInput(session, "selected_task_ids", selected = character(0))
    })
    
    #---------logic for select/deselect all ends ----------------------
    
    #---------task filter id for all tasks ends------------------------------------
    
    
    #DOWNLOAD FILTERED BIG TABLE - FOR SINGLE FILE UPLOAD..STARTS-----------------------------
    output$save_data <- downloadHandler(
      filename = function(){
        paste("data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file){
        # use the filtered reactive df
        write.csv(filtered_df(), file, row.names = FALSE)
      }
    )
    
    output$save_big_table <- renderUI({
      req(filtered_df())
      
      if (nrow(filtered_df()) > 0) {
        downloadButton('save_data', 'Save to CSV')
      }
    })
    # DOWNLOAD FILTERED BIG TABLE - FOR SINGLE FILE UPLOAD.. ENDS--------------------------
    
    
    
    # create icons (from Jakub's code)
    loc_marker <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/origami-team/origami/master/src/assets/icons/marker-editor.png",
      iconWidth = 20, iconHeight = 20,
      iconAnchorX = 10, iconAnchorY = 20,
    )
    loc_marker_green <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/origami-team/origami/master/src/assets/icons/marker-editor-solution.png",
      iconWidth = 20, iconHeight = 20,
      iconAnchorX = 10, iconAnchorY = 20,
    )
    
    
    
    
    ###########-------------START : downloaded json files issue solved here : safely handled the empty df and NA values--- THIS SOLVED THE MAP RENDERING ISSUE FOR THEME OBJECTS##################
    # --- SAFE MAP RENDERING for upload button ---
    # Cleaning all coordinate vectors before plotting with leaflet.
    # Preventing the "invalid lat/lon" and "data.frame row names" errors.
    # we can use this function before any addMarkers(), addCircles(), or addPolylines() calls.
    # Cleaned all coordinate vectors before using them
    
    # --- SAFE MAP RENDERING ---
    # Helper: safely unlist and remove invalid coords
    safe_coords <- function(x) {
      x <- unlist(x)
      x <- x[is.finite(x) & !is.na(x)]
      return(x)
    }
    
    long <- safe_coords(long)
    lati <- safe_coords(lati)
    traj_lng <- safe_coords(traj_lng)
    traj_lat <- safe_coords(traj_lat)
    lng_targ <- safe_coords(lng_targ)
    lat_targ <- safe_coords(lat_targ)
    lng_true <- safe_coords(lng_true)
    lat_true <- safe_coords(lat_true)
    lng_poly <- safe_coords(lng_poly)
    lat_poly <- safe_coords(lat_poly)
    lng_ans_obj <- safe_coords(lng_ans_obj)
    lat_ans_obj <- safe_coords(lat_ans_obj)
    dr_point_lng <- safe_coords(dr_point_lng)
    dr_point_lat <- safe_coords(dr_point_lat)
    
    
    
    
    
    if (!is.na(dir_ok_idx) && t == "theme-direction") {
      evts <- data[[1]]$events
      lon0 <- num(evts$position$coords$longitude[dir_ok_idx])
      lat0 <- num(evts$position$coords$latitude[dir_ok_idx])
      
      ans_b <- get_answer_bearing(dir_ok_idx, evts)
      cor_b <- get_correct_bearing(dir_ok_idx, evts)
      
      if (is.finite(lon0) && is.finite(lat0) && !is.na(ans_b)) {
        A <- arrow_lines(lon0, lat0, ans_b)
        
        map_shown <- leaflet() %>%
          addTiles() %>%
          addMarkers(lng = lon0, lat = lat0, icon = loc_marker) %>%
          addPolylines(lng = A$main$lng,  lat = A$main$lat) %>%
          addPolylines(lng = A$left$lng,  lat = A$left$lat) %>%
          addPolylines(lng = A$right$lng, lat = A$right$lat) %>%
          setView(lng = lon0, lat = lat0, zoom = 19)
        
        if (!is.na(cor_b)) {
          C <- arrow_lines(lon0, lat0, cor_b)
          map_shown <- map_shown %>%
            addPolylines(lng = C$main$lng,  lat = C$main$lat,  opacity = 0.6, dashArray = "5,5") %>%
            addPolylines(lng = C$left$lng,  lat = C$left$lat,  opacity = 0.6, dashArray = "5,5") %>%
            addPolylines(lng = C$right$lng, lat = C$right$lat, opacity = 0.6, dashArray = "5,5")
        }
        
        mr <- FALSE  # ensure fallback doesn't blank it
      }
    }
    
    
    
    
    
    ###########-------------END : downloaded json files issue solved here : safely handled the empty df and NA values--- THIS SOLVED THE MAP RENDERING ISSUE FOR THEME OBJECTS##################
    
    #Print map
    if (mr == TRUE || length(ans) <= num_value_num() || (length(lng_targ) == 0 && length(lng_true) == 0 && t == "theme-loc")
        || (length(long) == 0 && length(traj_lat) == 0 && (t == "nav-flag" || t == "nav-text" || t == "nav-arrow" || t == "nav-photo"))) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          setView(lng = 7, lat = 51, zoom = 6)
      }
    }
    if (length(long) != 0 && length(traj_lat) == 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addMarkers(lng = unlist(long)[1], lat = unlist(lati)[1], icon = loc_marker_green) %>%
          addCircles(lng = unlist(long)[1], lat = unlist(lati)[1], radius = accuracy_rad, opacity = 0.5)
      }
    }
    if (length(long) != 0 && length(traj_lat) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addMarkers(lng = unlist(long)[1], lat = unlist(lati)[1], icon = loc_marker_green) %>%
          addCircles(lng = unlist(long)[1], lat = unlist(lati)[1], radius = accuracy_rad, opacity = 0.5) %>%
          addPolylines(lng = unlist(traj_lng), lat = unlist(traj_lat), color = "red", weight = 2, opacity = 1, stroke = TRUE)
      }
    }
    if (length(dr_point_lng) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addPolylines(lng = unlist(dr_point_lng), lat = unlist(dr_point_lat), color = "red", weight = 2, opacity = 1, stroke = TRUE)
      }
    }
    if (length(lng_targ) != 0 && length(lng_true) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>% #Displaying last answer of the player
          addMarkers(lng = unlist(lng_targ)[length(lng_targ)], lat = unlist(lat_targ)[length(lat_targ)], icon = loc_marker) %>%
          addMarkers(lng = unlist(lng_true)[length(lng_true)], lat = unlist(lat_true)[length(lat_true)], icon = loc_marker_green) %>%
          addCircles(lng = unlist(lng_true)[length(lng_true)], lat = unlist(lat_true)[length(lat_true)], radius = accuracy_rad)
      }
    }
    if (length(lng_targ) == 0 && length(lng_true) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>% #Displaying last answer of the player
          addMarkers(lng = unlist(lng_true)[length(lng_true)], lat = unlist(lat_true)[length(lat_true)], icon = loc_marker_green) %>%
          addCircles(lng = unlist(lng_true)[length(lng_true)], lat = unlist(lat_true)[length(lat_true)], radius = accuracy_rad)
      }
    }
    if (length(lng_poly) != 0 && length(lng_ans_obj) == 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addPolygons(lng = unlist(lng_poly), lat = unlist(lat_poly), color = "blue", fillColor = "grey", weight = 2, opacity = 1)
      }
    }
    if (length(lng_poly) != 0 && length(lng_ans_obj) != 0) {
      map_shown <- {
        leaflet() %>%
          addTiles() %>%
          addMarkers(lng = unlist(lng_ans_obj)[length(lng_ans_obj)], lat = unlist(lat_ans_obj)[length(lat_ans_obj)], icon = loc_marker) %>%
          addPolygons(lng = unlist(lng_poly), lat = unlist(lat_poly), color = "blue", fillColor = "grey", weight = 2, opacity = 1)
      }
    }
    
    mr <- FALSE #Reinitialize variable
    ### Rendering map
    
    ## Extract virtual environment names which have INIT_TASK events, to exclude other events
    ## here having init-task twice for one task will cause an issue, this migth happen if user uses previous button
    # Get events where type = INIT_TASK 
    init_task_indices <- which(data[[1]]$events$type == "INIT_TASK") 
    message("----- init_task_indices: ")
    print(init_task_indices)
    
    if (is.null(data[[1]]$events$task$virEnvType[init_task_indices[num_value_num()]])) {
      # 1. Real-world map: just use map_shown as built above
      map_rv(map_shown)
    } else {
      
      # 2. To render virtual environment map
      # Extract virEnvName and floor if 3d building for those events 
      virEnvLayers <- character(0)      # for virtual environment layers / images names
      virEnvNames <- character(0)       # for virtual environemnt names
      sapply(
        init_task_indices, function(i) { 
          if (!is.null(data[[1]]$events$task$virEnvType[i])) {
            val =  data[[1]]$events$task$virEnvType[i]
            virEnvNames <<- c(virEnvNames, val)
            
            if (!is.null(data[[1]]$events$task$floor[i]) && !is.na(data[[1]]$events$task$floor[i])) {
              virEnvLayers <<- c(virEnvLayers, paste0(val , "_", data[[1]]$events$task$floor[i])) 
            } 
            else
              virEnvLayers <<- c(virEnvLayers, val) 
          } 
          else 
          { NA } 
        }
      )
      
      # Load virtual environment properties from JSON
      # virEnvsProperties <- fromJSON("www/virEnvsProperties.json");
      virEnvsProperties <- jsonlite::fromJSON(
        file.path(getwd(), "www", "virEnvsProperties.json")
      )
      
      # Build virtual environment map once
      map_virtual <- {
        # Default empty map
        map_shown <- leaflet() %>%
          addTiles() %>%
          setView(lng = 7, lat = 51, zoom = 20)
        
        # Conditions
        if (mr == TRUE ||
            length(ans) <= num_value_num() ||
            (length(lng_targ) == 0 && length(lng_true) == 0 && t == "theme-loc") ||
            (length(long) == 0 && length(traj_lat) == 0 &&
             (t %in% c("nav-flag", "nav-text", "nav-arrow", "nav-photo")))) {
          
          map_shown <- leaflet() %>%
            addTiles() %>%
            setView(lng = 7, lat = 51, zoom = 20)
        }
        
        if (length(long) != 0 && length(traj_lat) == 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addMarkers(
              lng = unlist(long)[1],
              lat = unlist(lati)[1],
              icon = loc_marker_green
            ) %>%
            addCircles(
              lng = unlist(long)[1],
              lat = unlist(lati)[1],
              radius = accuracy_rad,
              opacity = 0.5
            )
        }
        
        if (length(long) != 0 && length(traj_lat) != 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addMarkers(
              lng = unlist(long)[1],
              lat = unlist(lati)[1],
              icon = loc_marker_green
            ) %>%
            addCircles(
              lng = unlist(long)[1],
              lat = unlist(lati)[1],
              radius = accuracy_rad,
              opacity = 0.5
            ) %>%
            addPolylines(
              lng = unlist(traj_lng),
              lat = unlist(traj_lat),
              color = "red", weight = 2, opacity = 1, stroke = TRUE
            )
        }
        
        if (length(dr_point_lng) != 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addPolylines(
              lng = unlist(dr_point_lng),
              lat = unlist(dr_point_lat),
              color = "red", weight = 2, opacity = 1, stroke = TRUE
            )
        }
        
        if (length(lng_targ) != 0 && length(lng_true) != 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addMarkers(
              lng = tail(unlist(lng_targ), 1),
              lat = tail(unlist(lat_targ), 1),
              icon = loc_marker
            ) %>%
            addMarkers(
              lng = tail(unlist(lng_true), 1),
              lat = tail(unlist(lat_true), 1),
              icon = loc_marker_green
            ) %>%
            addCircles(
              lng = tail(unlist(lng_true), 1),
              lat = tail(unlist(lat_true), 1),
              radius = accuracy_rad
            )
        }
        
        if (length(lng_targ) == 0 && length(lng_true) != 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addMarkers(
              lng = tail(unlist(lng_true), 1),
              lat = tail(unlist(lat_true), 1),
              icon = loc_marker_green
            ) %>%
            addCircles(
              lng = tail(unlist(lng_true), 1),
              lat = tail(unlist(lat_true), 1),
              radius = accuracy_rad
            )
        }
        
        if (length(lng_poly) != 0 && length(lng_ans_obj) == 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addPolygons(
              lng = unlist(lng_poly),
              lat = unlist(lat_poly),
              color = "blue", fillColor = "grey", weight = 2, opacity = 1
            )
        }
        
        if (length(lng_poly) != 0 && length(lng_ans_obj) != 0) {
          map_shown <- leaflet() %>%
            addTiles() %>%
            addMarkers(
              lng = tail(unlist(lng_ans_obj), 1),
              lat = tail(unlist(lat_ans_obj), 1),
              icon = loc_marker
            ) %>%
            addPolygons(
              lng = unlist(lng_poly),
              lat = unlist(lat_poly),
              color = "blue", fillColor = "grey", weight = 2, opacity = 1
            )
        }
        
        if (!is.na(dir_ok_idx) && t == "theme-direction") {
          evts <- data[[1]]$events
          
          lon0 <- num(evts$position$coords$longitude[dir_ok_idx])
          lat0 <- num(evts$position$coords$latitude[dir_ok_idx])
          
          ans_b <- get_answer_bearing(dir_ok_idx, evts)   # player's FINAL answer
          cor_b <- get_correct_bearing(dir_ok_idx, evts)  # correct direction
          
          if (is.finite(lon0) && is.finite(lat0) && !is.na(ans_b)) {
            
            # Make arrow smaller if you want
            A <- arrow_lines(lon0, lat0, ans_b, len_m = 18, head_m = 5, head_ang = 25)
            
            map_shown <- map_shown %>%
              addMarkers(lng = lon0, lat = lat0, icon = loc_marker) %>%
              
              # Player FINAL answer arrow (BLUE)
              addPolylines(lng = A$main$lng,  lat = A$main$lat,  color = "blue",  weight = 4, opacity = 1) %>%
              addPolylines(lng = A$left$lng,  lat = A$left$lat,  color = "blue",  weight = 4, opacity = 1) %>%
              addPolylines(lng = A$right$lng, lat = A$right$lat, color = "blue",  weight = 4, opacity = 1)
            
            # Correct direction arrow (GREEN) - optional
            # if (!is.na(cor_b)) {
            #   C <- arrow_lines(lon0, lat0, cor_b, len_m = 18, head_m = 5, head_ang = 25)
            #   
            #   map_shown <- map_shown %>%
            #     addPolylines(lng = C$main$lng,  lat = C$main$lat,  color = "green", weight = 4, opacity = 0.9) %>%
            #     addPolylines(lng = C$left$lng,  lat = C$left$lat,  color = "green", weight = 4, opacity = 0.9) %>%
            #     addPolylines(lng = C$right$lng, lat = C$right$lat, color = "green", weight = 4, opacity = 0.9)
            # }
            
            mr <- FALSE
          }
        }
        
        
        
        
        
        # Add overlay with zIndex control
        map_shown %>%
          htmlwidgets::onRender("
              function(el, x, data) {
                var map = this;
                var task_number = data.task_number;
                var virEnvName = data.virEnvName;
                var virEnvLayer = data.virEnvLayer;

                // console.log('task_number from R:', task_number);
                // console.log('virEnvName from R:', virEnvName);
                // console.log('virEnvLayer from R:', virEnvLayer);

                // Set min and max zoom levels
                map.options.minZoom = 17;
                map.options.maxZoom = 20;
                map.on('zoomend', function() {
                  console.log('Current zoom level:', map.getZoom());
                });
                  
                // Define imageUrl variable
                var imageUrl;
                if (virEnvName !== null && virEnvName !== undefined && virEnvName !== 'NA') {
                  imageUrl = 'assets/vir_envs_layers/' + virEnvLayer + '.png';
                } else {
                  imageUrl = 'assets/vir_envs_layers/VirEnv_1.png';
                }

                // ########################
                // overlayCoords: 4 corners of where the image should appear
                var overlayCoords = data.virEnvsProperties[virEnvName].overlayCoords;

                // Compute SW/NE bounds from overlayCoords
                var lats = overlayCoords.map(c => c[0]);
                var lngs = overlayCoords.map(c => c[1]);
                var sw = [Math.min(...lats), Math.min(...lngs)];
                var ne = [Math.max(...lats), Math.max(...lngs)];

                // ########################
                // bounds: constrain map panning/zooming to these bounds
                var mapBounds = data.virEnvsProperties[virEnvName].bounds;

                // Add image overlay
                var overlay = L.imageOverlay(imageUrl, [sw, ne], { zIndex: 10 }).addTo(this);

                // Constrain map to bounds
                this.setMaxBounds(mapBounds);

                // Fit map view to overlay
                this.fitBounds([sw, ne]);
                }
            ", data = list(task_number = num_value_num(),
                           virEnvName = virEnvNames[num_value_num()],
                           virEnvLayer = virEnvLayers[num_value_num()],
                           virEnvsProperties = virEnvsProperties)
          )
      }
      
      # Store in reactive value
      map_rv(map_virtual)
      
      # output$map <- renderLeaflet({
      #   req(map_rv())
      #   map_rv()
      # })
      
      # Convert abbreviation for type task
      if (!is.na(t)) {
        if (t == "nav-flag") {
          t <- "Navigation to flag"
        }
        if (t == "nav-arrow") {
          t <- "Navigation with arrow"
        }
        if (t == "nav-photo") {
          t <- "Navigation via photo"
        }
        if (t == "nav-text") {
          t <- "Navigation via text"
        }
        if (t == "theme-loc") {
          t <- "Self location"
        }
        if (t == "theme-object") {
          t <- "Object location"
        }
        if (t == "theme-direction") {
          t <- "Direction determination"
        }
        if (t == "free") {
          t <- "Free"
        }
        if (t == "info") {
          t <- "Information"
        }
        if (t == "") {
          t <- "No task exists with this number"
        }
      }
      
      output$mapLegend <- renderText({paste("Task type:",t)})
    }
      #Download map
      # output$downloadMap <- downloadHandler(
      #   filename = function() {
      #     paste("map_", Sys.Date(), ".html", sep="")
      #   },
      #   content = function(file) {
      #     m <- saveWidget(map_shown, file = file, selfcontained = TRUE)
      #   }
      # )
      # 
      
      #photo code starts---------------------
    cou <- 1 #counter
    pict <- list()
    ans_photo <- list()
    
    for (i in 1:(length(id)-1)) {
      if ((!is.na(id[i]) && (i != 1) && (id[i] != id[i + 1])) || i == (length(id) - 1)) {
        cou <- cou + 1
        pict <- append(pict, unlist(data[[1]]$events$task$question$photo[[i]]))
        ans_photo <- append(ans_photo, unlist(data[[1]]$events$answer$photo[[i]]))
      }
    }
    
    if (length(pict) != 0) { #Photos in assignment
      if (num_value_num() <= length(pict) && !is.na(pict[[num_value_num()]]) && pict[[num_value_num()]] != "") {
        # Render photo display with download buttons
        output$photo_display <- renderUI({
          
          photo_url <- pict[[num_value_num()]]
          
          output[["download_image"]] <- downloadHandler(
            filename = function() {
              paste("image_", t, ".jpg", sep = "")
            },
            content = function(file) {
              download.file(photo_url, file, mode = "wb")
            }
          )
          
          # Create a flex container for images
          div(style = "display: flex; flex-wrap: wrap; gap: 20px;",
              tagList(
                tags$div(style = "flex: 0 1 200px; display: inline-block; text-align: center;",
                         tags$h4(paste("Assignment for", t)),
                         tags$img(src = photo_url, height = "500px", style = "margin: 10px; border: 1px solid #ccc;"),
                         downloadButton("download_image"), label = "Download", class = "btn btn-primary", style = "margin-top: 10px;")
              )
          )
        })
      }
    }
    
    if (length(ans_photo) != 0) { #Photos in answer
      if (num_value_num() <= length(ans_photo) && !is.na(ans_photo[[num_value_num()]]) && ans_photo[[num_value_num()]] != "") {
        # Render photo display with download buttons
        output$photo_display <- renderUI({
          
          photo_ans_url <- ans_photo[[num_value_num()]]
          
          output[["download_image_2"]] <- downloadHandler(
            filename = function() {
              paste("image_", t, ".jpg", sep = "")
            },
            content = function(file) {
              download.file(photo_ans_url, file, mode = "wb")
            }
          )
          
          # Create a flex container for images
          div(style = "display: flex; flex-wrap: wrap; gap: 20px;", 
              tagList(
                tags$div(style = "flex: 0 1 200px; display: inline-block; text-align: center;",
                         tags$h4(paste("Answer for", t)),
                         tags$img(src = photo_ans_url, height = "500px", style = "margin: 10px; border: 1px solid #ccc;"),
                         downloadButton("download_image_2"), label = "Download", class = "btn btn-primary", style = "margin-top: 10px; background-color: #0CD1E8 ")
              )
          )
        })
      }
    }
    
    if (length(ans_photo) == 0 && length(pict) == 0) {
      output$photo_display <- renderUI({
        "No photos for this game"
      })
    }
    
    if (length(pict) != 0 && num_value_num() > length(pict)) {
      output$photo_display <- renderUI({
        "No task exists with this number"
      })
    }
    
    if (length(pict) != 0 && length(ans_photo) != 0) {
      if (num_value_num() <= length(ans_photo) && (is.na(ans_photo[[num_value_num()]]) || ans_photo[[num_value_num()]] == "") && (is.na(pict[[num_value_num()]]) || pict[[num_value_num()]] == "")) {
        output$photo_display <- renderUI({
          "No photos for this task"
        })
      }
    }
    if (length(pict) == 0 && length(ans_photo) != 0) {
      if (num_value_num() <= length(ans_photo) && (is.na(ans_photo[[num_value_num()]]) || ans_photo[[num_value_num()]] == "")) {
        output$photo_display <- renderUI({
          "No photos for this task"
        })
      }
    }
    if (length(pict) != 0 && length(ans_photo) == 0) {
      if (num_value_num() <= length(pict) && (is.na(pict[[num_value_num()]]) || pict[[num_value_num()]] == "")) {
        output$photo_display <- renderUI({
          "No photos for this task"
        })
      }
    }
    
    #No multiple analysis
    cores <- data.frame(Name = c(), Correct = c(), Answer = c(), Error = c())
    ngts <- data.frame(Name = c(), Correct = c(), Time = c(), Distance = c())
    
    #TIME VS DISTANCE Table
    output$cmp_table1 <- renderTable(
      ngts
    )
    
    #CORRECT & ERRORS Table
    output$cmp_table2 <- renderTable(
      cores
    )
    
    output$tabLegend <- renderText({paste("Task type:",t)})
    output$graphLegend <- renderText({paste("Task type:",t)})
    
    pie_chart <- ggplot() +
      theme_void() +
      labs(title = "You can't compare your file with another file.")
    
    #Two outputs because two conditions in UI
    output$pie_chart <- renderPlot({
      pie_chart
    })
    
    output$pie_chart2 <- renderPlot({
      pie_chart
    })
    
    time_chart <- ggplot() +
      theme_void() +
      labs(title = "You can't compare your file with another file.")
    
    output$time_chart <- renderPlot({
      time_chart
    })
    
  })
  #End of upload json
  
  
  
  
  
  
  
  #have defined the four pickers once
  all_ids <- c("num_value", "num_value_pictures", "num_value_comparison", "num_value_Statistics")
  
  # Helper to read Map picker as number wherever we compare
  num_value_num <- reactive({
    suppressWarnings(as.integer(input$num_value))
  })
  
  # Preventing recursive updates
  sync_lock <- FALSE
  
  # Set up sync observers once
  observe({
    for (id in all_ids) {
      local({
        this_id <- id
        observeEvent(input[[this_id]], {
          
          if (is.null(input[[this_id]])) return()
          if (sync_lock) return()
          
          val <- input[[this_id]]
          
          sync_lock <<- TRUE
          for (other in setdiff(all_ids, this_id)) {
            if (!identical(input[[other]], val)) {
              updatePickerInput(session, other, selected = val)
            }
          }
          sync_lock <<- FALSE
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    }
  })
  
  output$map <- renderLeaflet({
    req(map_rv())
    map_rv()
  })
  
  # Single download handler for maps (works for real + virtual env)
  # output$downloadMap <- downloadHandler(
  #   filename = function() {
  #     paste0("map_", Sys.Date(), ".html")
  #   },
  #   content = function(file) {
  #     req(map_rv())
  #     htmlwidgets::saveWidget(map_rv(), file = file, selfcontained = TRUE)
  #   }
  # )
  
  output$downloadMap <- downloadHandler(
    filename = function() {
      paste0("map_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(map_rv())

      # Temp dir for export
      tmpdir <- tempfile("map_export_")
      dir.create(tmpdir)

      # have Saved widget
      htmlfile <- file.path(tmpdir, "map.html")
      htmlwidgets::saveWidget(
        widget = map_rv(),
        file   = htmlfile,
        selfcontained = FALSE
      )
      # At this point tmpdir contains:
      #   - map.html
      #   - map_files/   (Leaflet JS/CSS etc.)

      #Copied virtual-environment images into assets/vir_envs_layers
      from_dir <- file.path(getwd(), "www", "assets", "vir_envs_layers")
      to_dir   <- file.path(tmpdir, "assets", "vir_envs_layers")
      dir.create(to_dir, recursive = TRUE, showWarnings = FALSE)

      if (dir.exists(from_dir)) {
        file.copy(
          from = list.files(from_dir, full.names = TRUE),
          to   = to_dir,
          recursive = TRUE
        )
      }

      # Zipped EVERYTHING in tmpdir (html + *_files + assets)
      oldwd <- getwd()
      setwd(tmpdir)
      zip::zipr(
        zipfile = file,
        files   = list.files()  # "map.html", "map_files", "assets", …
      )
      setwd(oldwd)
    }
  )
  
  
}

shinyApp(ui, server)