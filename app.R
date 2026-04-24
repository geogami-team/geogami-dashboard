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

# ── API helpers for share/unshare feature ────────────────────────────
# The existing fetch_games_data_from_server() only supports GET.
# These two helpers add POST/DELETE and a generic GET that returns both
# status code and parsed body, so the share UI can react to errors.

# POST or DELETE JSON to an API endpoint.
# Used by the share modal to add/remove emails from game.sharedWith.
api_request <- function(url, token, body = list(), method = "POST") {
  auth_header <- paste("Bearer", token)
  if (method == "DELETE") {
    res <- httr::DELETE(url, add_headers(Authorization = auth_header),
                        body = body, encode = "json",
                        content_type_json())
  } else {
    res <- httr::POST(url, add_headers(Authorization = auth_header),
                      body = body, encode = "json",
                      content_type_json())
  }
  # Return status + parsed body so callers can check for errors and
  # update the UI with the server's response (e.g. updated sharedWith list).
  list(
    status = status_code(res),
    data = tryCatch(fromJSON(content(res, "text", encoding = "UTF-8")),
                    error = function(e) list())
  )
}

# GET JSON from an API endpoint, returning status + body.
# Used to fetch the current sharedWith list when the modal opens.
api_get <- function(url, token) {
  auth_header <- paste("Bearer", token)
  res <- httr::GET(url, add_headers(Authorization = auth_header))
  list(
    status = status_code(res),
    data = tryCatch(fromJSON(content(res, "text", encoding = "UTF-8")),
                    error = function(e) list())
  )
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
    
    # Share button — appears once a game is selected
    conditionalPanel(
      condition = "typeof window.location.search.match(/token=([^&]+)/) !== 'undefined' && window.location.search.match(/token=([^&]+)/) !== null",
      uiOutput("share_button_ui")
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
  
  
  
  # ---- DEFAULT accuracy fallbacks (used only if task$settings$accuracy is missing/NA) ----
  DEFAULT_NAV_ACC_M   <- 15   # navigation / distance-based tasks
  DEFAULT_DIR_ACC_DEG <- 45   # direction tasks
  
  get_task_accuracy <- function(evts, idx, kind = c("nav", "dir")) {
    kind <- match.arg(kind)
    a <- try(evts$task$settings$accuracy[idx], silent = TRUE)
    a <- if (inherits(a, "try-error")) NA_real_ else num(a)
    
    if (!is.finite(a) || is.na(a)) {
      if (kind == "nav") DEFAULT_NAV_ACC_M else DEFAULT_DIR_ACC_DEG
    } else a
  }
  
  ###########ends : Default fallback accuracy of Navigation and direction tasks #########
  
  
  
  
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
      
      acc <- get_task_accuracy(evts, j, kind = "nav")   # 15m fallback
      
      evts$answer$distance[j] <- d
      evts$answer$correct[j]  <- (d <= acc)
      evts$correct[j]         <- (d <= acc)  # keep both consistent
    }
    
    evts
  }
  ################################################ 
  
  
  ###########starts : Default fallback accuracy of Navigation and direction tasks #########
  
  
  ########################CALCULATING CORRECT DIRECTION ERROR FIXING starts#################
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
      acc <- get_task_accuracy(evts, j, kind = "dir")   # 45° fallback
      
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
  
  
  
  #showing arrow lines for direction based tasks
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
  
  normalize_ring_coords <- function(coords) {
    if (is.null(coords)) return(NULL)
    
    if (is.data.frame(coords) || is.matrix(coords)) {
      m <- as.matrix(coords)
      if (ncol(m) >= 2) {
        m <- cbind(num(m[, 1]), num(m[, 2]))
        m <- m[is.finite(m[, 1]) & is.finite(m[, 2]), , drop = FALSE]
        if (nrow(m) >= 3) return(m)
      }
    }
    
    v <- unlist(coords, use.names = FALSE)
    v <- num(v)
    v <- v[is.finite(v)]
    
    if (length(v) < 6 || (length(v) %% 2) != 0) return(NULL)
    
    half <- length(v) / 2
    cand1 <- cbind(v[1:half], v[(half + 1):length(v)])
    cand2 <- matrix(v, ncol = 2, byrow = TRUE)
    
    is_plausible <- function(m) {
      is.matrix(m) &&
        ncol(m) == 2 &&
        all(is.finite(m)) &&
        all(m[, 1] >= -180 & m[, 1] <= 180) &&
        all(m[, 2] >= -90 & m[, 2] <= 90)
    }
    
    closure_score <- function(m) {
      if (!is_plausible(m) || nrow(m) < 2) return(Inf)
      sum((m[1, ] - m[nrow(m), ])^2)
    }
    
    s1 <- closure_score(cand1)
    s2 <- closure_score(cand2)
    
    out <- if (s1 <= s2) cand1 else cand2
    out <- out[is.finite(out[, 1]) & is.finite(out[, 2]), , drop = FALSE]
    
    if (nrow(out) < 3) return(NULL)
    out
  }
  
  get_polygon_ring <- function(evts, idx) {
    coords <- NULL
    
    feat <- try(evts$task$question$geometry$features[[idx]], silent = TRUE)
    if (!inherits(feat, "try-error") && !is.null(feat)) {
      coords <- try(feat$geometry$coordinates[[1]], silent = TRUE)
      if (inherits(coords, "try-error")) coords <- NULL
    }
    
    if (is.null(coords)) {
      feat <- try(evts$task$question$geometry$feature[[idx]], silent = TRUE)
      if (!inherits(feat, "try-error") && !is.null(feat)) {
        coords <- try(feat$geometry$coordinates[[1]], silent = TRUE)
        if (inherits(coords, "try-error")) coords <- NULL
      }
    }
    
    normalize_ring_coords(coords)
  }
  
  get_click_lonlat <- function(evts, idx) {
    cp <- try(evts$answer$clickPosition[[idx]], silent = TRUE)
    if (!inherits(cp, "try-error") && length(cp) == 2) {
      lon <- num(cp[1]); lat <- num(cp[2])
      if (is.finite(lon) && is.finite(lat)) return(c(lon, lat))
    }
    
    lon <- try(evts$clickPosition$longitude[idx], silent = TRUE)
    lat <- try(evts$clickPosition$latitude[idx],  silent = TRUE)
    
    lon <- if (inherits(lon, "try-error")) NA_real_ else num(lon)
    lat <- if (inherits(lat, "try-error")) NA_real_ else num(lat)
    
    c(lon, lat)
  }
  
  point_in_ring <- function(lon, lat, ring) {
    ring <- as.matrix(ring)
    ring <- ring[is.finite(ring[, 1]) & is.finite(ring[, 2]), , drop = FALSE]
    if (nrow(ring) < 3) return(FALSE)
    
    if (!all(ring[1, ] == ring[nrow(ring), ])) {
      ring <- rbind(ring, ring[1, ])
    }
    
    inside <- FALSE
    j <- nrow(ring)
    
    for (i in seq_len(nrow(ring))) {
      xi <- ring[i, 1]; yi <- ring[i, 2]
      xj <- ring[j, 1]; yj <- ring[j, 2]
      
      hit <- ((yi > lat) != (yj > lat)) &&
        (lon < (xj - xi) * (lat - yi) / ((yj - yi) + 1e-12) + xi)
      
      if (isTRUE(hit)) inside <- !inside
      j <- i
    }
    
    inside
  }
  
  point_to_segment_distance_m <- function(lon, lat, lon1, lat1, lon2, lat2) {
    scale_x <- 111320 * cos(lat * pi / 180)
    scale_y <- 110540
    
    px <- 0
    py <- 0
    
    x1 <- (lon1 - lon) * scale_x
    y1 <- (lat1 - lat) * scale_y
    x2 <- (lon2 - lon) * scale_x
    y2 <- (lat2 - lat) * scale_y
    
    dx <- x2 - x1
    dy <- y2 - y1
    denom <- dx * dx + dy * dy
    
    if (!is.finite(denom) || denom == 0) {
      return(sqrt((px - x1)^2 + (py - y1)^2))
    }
    
    t <- ((px - x1) * dx + (py - y1) * dy) / denom
    t <- max(0, min(1, t))
    
    projx <- x1 + t * dx
    projy <- y1 + t * dy
    
    sqrt((px - projx)^2 + (py - projy)^2)
  }
  
  point_to_ring_distance_m <- function(lon, lat, ring) {
    ring <- as.matrix(ring)
    ring <- ring[is.finite(ring[, 1]) & is.finite(ring[, 2]), , drop = FALSE]
    if (nrow(ring) < 2) return(NA_real_)
    
    if (!all(ring[1, ] == ring[nrow(ring), ])) {
      ring <- rbind(ring, ring[1, ])
    }
    
    if (point_in_ring(lon, lat, ring)) return(0)
    
    d <- vapply(seq_len(nrow(ring) - 1), function(i) {
      point_to_segment_distance_m(
        lon, lat,
        ring[i, 1], ring[i, 2],
        ring[i + 1, 1], ring[i + 1, 2]
      )
    }, numeric(1))
    
    d <- d[is.finite(d)]
    if (!length(d)) return(NA_real_)
    min(d)
  }
  
  get_task_error_m <- function(evts, idx) {
    task_type <- try(evts$task$type[idx], silent = TRUE)
    task_type <- if (inherits(task_type, "try-error")) NA_character_ else as.character(task_type)
    
    eval_type <- try(evts$task$evaluate[idx], silent = TRUE)
    eval_type <- if (inherits(eval_type, "try-error")) NA_character_ else as.character(eval_type)
    
    d0 <- try(evts$answer$distance[idx], silent = TRUE)
    d0 <- if (inherits(d0, "try-error")) NA_real_ else num(d0)
    if (is.finite(d0)) return(d0)
    
    if ((!is.na(task_type) && task_type == "theme-loc") ||
        (!is.na(eval_type) && eval_type %in% c("evalDistanceToPoint", "distanceToPoint"))) {
      
      cp <- get_click_lonlat(evts, idx)
      lon_true <- num(evts$position$coords$longitude[idx])
      lat_true <- num(evts$position$coords$latitude[idx])
      
      if (all(is.finite(c(cp, lon_true, lat_true)))) {
        return(haversine_m(cp[1], cp[2], lon_true, lat_true))
      }
    }
    
    if (!is.na(task_type) && task_type == "nav-flag") {
      targ <- try(evts$answer$target[[idx]], silent = TRUE)
      lon_p <- num(evts$position$coords$longitude[idx])
      lat_p <- num(evts$position$coords$latitude[idx])
      
      if (!inherits(targ, "try-error") && length(targ) == 2) {
        lon_t <- num(targ[1])
        lat_t <- num(targ[2])
        
        if (all(is.finite(c(lon_t, lat_t, lon_p, lat_p)))) {
          return(haversine_m(lon_p, lat_p, lon_t, lat_t))
        }
      }
    }
    
    if ((!is.na(task_type) && task_type == "theme-object") ||
        (!is.na(eval_type) && eval_type == "evalPointInPolygon")) {
      
      cp <- get_click_lonlat(evts, idx)
      ring <- get_polygon_ring(evts, idx)
      
      if (all(is.finite(cp)) && !is.null(ring)) {
        return(point_to_ring_distance_m(cp[1], cp[2], ring))
      }
    }
    
    NA_real_
  }
  
  format_task_error <- function(evts, idx) {
    task_type <- try(evts$task$type[idx], silent = TRUE)
    task_type <- if (inherits(task_type, "try-error")) NA_character_ else as.character(task_type)
    
    eval_type <- try(evts$task$evaluate[idx], silent = TRUE)
    eval_type <- if (inherits(eval_type, "try-error")) NA_character_ else as.character(eval_type)
    
    correct_flag <- try(evts$answer$correct[idx], silent = TRUE)
    if (inherits(correct_flag, "try-error") || is.null(correct_flag) || length(correct_flag) == 0 || is.na(correct_flag)) {
      correct_flag <- try(evts$correct[idx], silent = TRUE)
    }
    correct_flag <- if (inherits(correct_flag, "try-error")) NA else truthy(correct_flag)
    
    is_dir <- (!is.na(task_type) && task_type == "theme-direction") ||
      (!is.na(eval_type) && eval_type %in% c("evalMapDirection", "evalDirection"))
    
    is_tol_dist <- (!is.na(task_type) && task_type %in% c("theme-loc", "nav-flag")) ||
      (!is.na(eval_type) && eval_type %in% c("evalDistanceToPoint", "distanceToPoint"))
    
    if (is_dir) {
      err_deg <- angle_diff_deg(
        get_answer_bearing(idx, evts),
        get_correct_bearing(idx, evts)
      )
      if (is.finite(err_deg)) return(paste0(round(err_deg, 2), " °"))
      return(NA_character_)
    }
    
    err_m <- get_task_error_m(evts, idx)
    if (!is.finite(err_m)) return(NA_character_)
    
    if (is_tol_dist) {
      acc_m <- get_task_accuracy(evts, idx, kind = "nav")
      shown_m <- if (!is.na(correct_flag) && !isTRUE(correct_flag)) {
        pmax(err_m - acc_m, 0)
      } else {
        err_m
      }
      return(paste0(round(shown_m, 2), " m"))
    }
    
    paste0(round(err_m, 2), " m")
  }
  
  
  ########################CALCULATING CORRECT DIRECTION ERROR FIXING ENDS#################
  
  
  
  ######## STARTS - PHoto code error - subscript out of bounds CORRECTION #########
  safe_photo_at <- function(x, i) {
    if (is.null(x)) return(NA_character_)
    
    # If x is a data.frame (happens when JSON has {} / mixed types)
    if (is.data.frame(x)) {
      if (nrow(x) < i || ncol(x) == 0) return(NA_character_)
      v <- unlist(x[i, , drop = TRUE], use.names = FALSE)
      v <- v[!is.na(v) & nzchar(v)]
      if (!length(v)) return(NA_character_)
      return(as.character(v[1]))
    }
    
    # If x is a list (typical case)
    if (is.list(x)) {
      if (length(x) < i) return(NA_character_)
      v <- unlist(x[[i]], use.names = FALSE)
      v <- v[!is.na(v) & nzchar(v)]
      if (!length(v)) return(NA_character_)
      return(as.character(v[1]))
    }
    
    # If x is an atomic vector (character, etc.)
    if (length(x) < i) return(NA_character_)
    v <- as.character(x[i])
    if (is.na(v) || !nzchar(v)) return(NA_character_)
    v
  }
  ######## ENDS- PHoto code error - subscript out of bounds CORRECTION #########
  
  
  ########### MAP AND PICTURES - TASKS SELECTION - APPEAR GREY OUT WHEN NOT AVAILABLE - START #########
  has_real_photo <- function(x) {
    if (is.null(x) || length(x) == 0) return(FALSE)
    x <- as.character(x[1])
    !is.na(x) && nzchar(trimws(x))
  }
  
  task_choice_styles <- function(track, n_tasks = NULL) {
    if (is.null(track) || is.null(track$events)) {
      return(list(
        pic_grey = logical(0),
        map_grey = logical(0),
        cmp_grey = logical(0),
        stat_grey = logical(0)
      ))
    }
    
    evts <- track$events
    sl <- task_slices_by_init_safe(evts)
    if (!nrow(sl)) {
      return(list(
        pic_grey = logical(0),
        map_grey = logical(0),
        cmp_grey = logical(0),
        stat_grey = logical(0)
      ))
    }
    
    pic_grey  <- rep(FALSE, nrow(sl))
    map_grey  <- rep(FALSE, nrow(sl))
    cmp_grey  <- rep(FALSE, nrow(sl))
    stat_grey <- rep(FALSE, nrow(sl))
    
    for (k in seq_len(nrow(sl))) {
      block <- sl$start[k]:sl$end[k]
      s <- sl$start[k]
      
      # ----- Pictures tab: grey if NO assignment photo and NO answer photo -----
      q_has <- any(vapply(block, function(i) {
        has_real_photo(safe_photo_at(evts$task$question$photo, i))
      }, logical(1)))
      
      a_has <- any(vapply(block, function(i) {
        has_real_photo(safe_photo_at(evts$answer$photo, i))
      }, logical(1)))
      
      pic_grey[k] <- !(q_has || a_has)
      
      # task category
      task_cat <- try(evts$task$category[s], silent = TRUE)
      task_cat <- if (inherits(task_cat, "try-error") || is.null(task_cat) || length(task_cat) == 0) {
        NA_character_
      } else {
        as.character(task_cat[1])
      }
      
      # task type
      task_type <- try(evts$task$type[s], silent = TRUE)
      task_type <- if (inherits(task_type, "try-error") || is.null(task_type) || length(task_type) == 0) {
        NA_character_
      } else {
        as.character(task_type[1])
      }
      
      # ----- Map tab: grey if information task OR free task -----
      map_grey[k] <- (!is.na(task_cat) && task_cat == "info") ||
        (!is.na(task_type) && task_type == "free")
      
      # ----- Compare Players / Statistics tabs: grey where no meaningful output is produced -----
      is_info_task <- (!is.na(task_cat) && task_cat == "info") ||
        (!is.na(task_type) && task_type == "info")
      
      cmp_grey[k]  <- is_info_task
      stat_grey[k] <- is_info_task
    }
    
    if (!is.null(n_tasks)) {
      length(pic_grey)  <- n_tasks
      length(map_grey)  <- n_tasks
      length(cmp_grey)  <- n_tasks
      length(stat_grey) <- n_tasks
      
      pic_grey[is.na(pic_grey)]   <- FALSE
      map_grey[is.na(map_grey)]   <- FALSE
      cmp_grey[is.na(cmp_grey)]   <- FALSE
      stat_grey[is.na(stat_grey)] <- FALSE
    }
    
    list(
      pic_grey = pic_grey,
      map_grey = map_grey,
      cmp_grey = cmp_grey,
      stat_grey = stat_grey
    )
  }
  
  update_main_task_pickers <- function(session, track, n_tasks, selected) {
    if (is.null(n_tasks) || n_tasks <= 0) return()
    
    choice_vec <- as.character(seq_len(n_tasks))
    selected <- as.character(selected)
    
    grey_css <- "color: #9aa0a6 !important; background-color: #f3f4f6 !important;"
    
    styles <- task_choice_styles(track, n_tasks)
    
    # Map picker: grey only info/free tasks
    updatePickerInput(
      session = session,
      inputId = "num_value",
      choices = choice_vec,
      selected = selected,
      choicesOpt = list(
        style = ifelse(styles$map_grey, grey_css, "")
      )
    )
    
    # Pictures picker: grey tasks with no photos
    updatePickerInput(
      session = session,
      inputId = "num_value_pictures",
      choices = choice_vec,
      selected = selected,
      choicesOpt = list(
        style = ifelse(styles$pic_grey, grey_css, "")
      )
    )
    
    # Compare Players picker: grey tasks with no useful compare/stat output
    updatePickerInput(
      session = session,
      inputId = "num_value_comparison",
      choices = choice_vec,
      selected = selected,
      choicesOpt = list(
        style = ifelse(styles$cmp_grey, grey_css, "")
      )
    )
    
    # Statistics picker: grey tasks with no useful compare/stat output
    updatePickerInput(
      session = session,
      inputId = "num_value_Statistics",
      choices = choice_vec,
      selected = selected,
      choicesOpt = list(
        style = ifelse(styles$stat_grey, grey_css, "")
      )
    )
  }
  ########### MAP AND PICTURES - TASKS SELECTION - APPEAR GREY OUT WHEN NOT AVAILABLE - END #########
  
  
  
  
  
  ##########starting - CORRECTING COMPARE TASKS TAB ---MAKING HELPER FUNCTIONS FOR THAT############
  task_slices_by_init <- function(evts) {
    init <- which(evts$type == "INIT_TASK")
    if (!length(init)) return(data.frame(taskNo=integer(0), start=integer(0), end=integer(0)))
    
    ends <- c(init[-1] - 1L, nrow(evts))
    data.frame(taskNo = seq_along(init), start = init, end = ends)
  }
  
  task_id_for_taskNo <- function(evts, taskNo) {
    sl <- task_slices_by_init(evts)
    if (nrow(sl) < taskNo) return(NA_character_)
    s <- sl$start[taskNo]
    evts$task$`_id`[s]
  }
  ##########ENDING - CORRECTING COMPARE TASKS TAB ---MAKING HELPER FUNCTIONS FOR THAT############
  
  
  
  
  
  
  
  
  # -------------------- CANONICAL TASK SUMMARY (ONE truth source) --------------------
  
  parse_ts <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(as.POSIXct(NA))
    }
    
    # handle list-columns safely (can happen with jsonlite)
    x <- unlist(x, use.names = FALSE)
    
    if (!length(x)) return(as.POSIXct(NA))
    
    # keep as character, remove trailing Z if present
    x_chr <- as.character(x)
    x_chr <- sub("Z$", "", x_chr)
    
    suppressWarnings(as.POSIXct(x_chr, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  
  
  truthy <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    
    # if something comes in as length>1, take the first (we need a scalar)
    if (length(x) > 1) x <- x[1]
    
    if (is.na(x)) return(NA)
    
    if (is.logical(x)) return(isTRUE(x))
    if (is.character(x)) {
      if (x == "TRUE")  return(TRUE)
      if (x == "FALSE") return(FALSE)
    }
    NA
  }
  
  
  format_answer_value <- function(evts, idx) {
    # idx should be the chosen "final" event index for the task block
    atype <- try(evts$task$answer$type[idx], silent = TRUE)
    if (inherits(atype, "try-error") || is.null(atype) || length(atype) == 0) return(NA_character_)
    
    if (atype == "TEXT") {
      v <- try(evts$answer$text[idx], silent = TRUE)
      v <- if (inherits(v, "try-error")) NA_character_ else as.character(v)
      return(v)
    }
    if (atype == "MULTIPLE_CHOICE_TEXT") {
      v <- try(evts$answer$selectedChoice$value[idx], silent = TRUE)
      v <- if (inherits(v, "try-error")) NA_character_ else as.character(v)
      return(v)
    }
    if (atype == "NUMBER") {
      v <- try(evts$answer$numberInput[idx], silent = TRUE)
      v <- if (inherits(v, "try-error")) NA_character_ else as.character(v)
      return(v)
    }
    
    # PHOTO / MAP_POINT / DRAW / etc. => no stable scalar to show in big table
    NA_character_
  }
  
  distance_travelled_between <- function(track, t_start, t_end, speed_cap = 10) {
    wps <- track$waypoints
    if (is.null(wps) || is.null(wps$timestamp)) return(NA_real_)
    
    ts <- parse_ts(wps$timestamp)
    if (all(is.na(ts))) return(NA_real_)
    
    keep <- which(ts >= t_start & ts <= t_end)
    if (length(keep) < 2) return(NA_real_)
    
    lon <- num(wps$position$coords$longitude[keep])
    lat <- num(wps$position$coords$latitude[keep])
    ts  <- ts[keep]
    
    o <- order(ts)
    lon <- lon[o]; lat <- lat[o]; ts <- ts[o]
    
    # optional accuracy gate if available
    acc <- NULL
    if (!is.null(wps$position$coords$accuracy)) {
      acc <- suppressWarnings(as.numeric(wps$position$coords$accuracy[keep][o]))
    }
    
    seg_d  <- haversine_m(lon[-length(lon)], lat[-length(lat)], lon[-1], lat[-1])
    seg_dt <- pmax(as.numeric(diff(ts), units = "secs"), 0.001)
    speed  <- seg_d / seg_dt
    
    good <- is.finite(seg_d) & is.finite(speed) & speed <= speed_cap
    if (!is.null(acc)) {
      # accept NA accuracy, but drop very noisy points where accuracy is huge
      good_acc <- (is.na(acc[-length(acc)]) | acc[-length(acc)] <= 20) &
        (is.na(acc[-1])          | acc[-1]          <= 20)
      good <- good & good_acc
    }
    
    sum(seg_d[good], na.rm = TRUE)
  }
  
  task_slices_by_init_safe <- function(evts) {
    init <- which(evts$type == "INIT_TASK")
    if (!length(init)) return(data.frame(taskNo=integer(0), start=integer(0), end=integer(0)))
    ends <- c(init[-1] - 1L, nrow(evts))
    data.frame(taskNo = seq_along(init), start = init, end = ends)
  }
  
  task_summary <- function(track) {
    if (is.null(track) || is.null(track$events)) {
      return(data.frame())
    }
    evts <- track$events
    sl <- task_slices_by_init_safe(evts)
    if (!nrow(sl)) return(data.frame())
    
    out <- vector("list", nrow(sl))
    
    for (k in seq_len(nrow(sl))) {
      s <- sl$start[k]
      e <- sl$end[k]
      block <- s:e
      
      # final event preference: ON_OK_CLICKED, else last event in block
      ok <- block[evts$type[block] == "ON_OK_CLICKED"]
      final_idx <- if (length(ok)) tail(ok, 1) else e
      
      task_id <- try(evts$task$`_id`[s], silent = TRUE)
      task_id <- if (inherits(task_id, "try-error")) NA_character_ else as.character(task_id)
      
      task_type <- try(evts$task$type[s], silent = TRUE)
      task_type <- if (inherits(task_type, "try-error")) NA_character_ else as.character(task_type)
      
      task_cat <- try(evts$task$category[s], silent = TRUE)
      task_cat <- if (inherits(task_cat, "try-error")) NA_character_ else as.character(task_cat)
      
      assignment <- try(evts$task$question$text[s], silent = TRUE)
      assignment <- if (inherits(assignment, "try-error")) NA_character_ else as.character(assignment)
      
      t_start <- parse_ts(evts$timestamp[s])
      t_end   <- parse_ts(evts$timestamp[final_idx])
      time_s  <- if (is.na(t_start) || is.na(t_end)) NA_integer_ else as.integer(floor(difftime(t_end, t_start, units="secs")))
      
      tries <- sum(evts$type[block] == "ON_OK_CLICKED", na.rm = TRUE)
      
      # correctness (prefer answer$correct, fallback to correct)
      cr <- try(evts$answer$correct[final_idx], silent = TRUE)
      cr <- if (inherits(cr, "try-error") || is.null(cr) || length(cr) == 0 || is.na(cr)) {
        try(evts$correct[final_idx], silent = TRUE)
      } else cr
      cr <- if (inherits(cr, "try-error")) NA else cr
      correct <- truthy(cr)
      
      nav_reached <- (!is.na(task_cat) && task_cat == "nav" &&
                        any(evts$type[block] == "WAYPOINT_REACHED", na.rm = TRUE))
      
      has_ok_click <- any(evts$type[block] == "ON_OK_CLICKED", na.rm = TRUE)
      
      status <- NA_character_
      
      if (!is.na(correct)) {
        status <- if (isTRUE(correct)) "Correct" else "Incorrect"
        
      } else if (!is.na(task_cat) && task_cat == "nav") {
        
        if (isTRUE(nav_reached)) {
          status <- "Correct"
          correct <- TRUE
          
        } else {
          is_nav_guided <- !is.na(task_type) && task_type %in% c("nav-arrow", "nav-text", "nav-photo")
          
          if (is_nav_guided && !is.na(time_s) && time_s > 0) {
            status  <- "Correct"
            correct <- TRUE
          } else {
            status  <- "Target not reached"
            correct <- FALSE
          }
        }
        
      } else if (is.na(status) && !is.na(task_cat) && task_cat == "info") {
        status  <- "Correct"
        correct <- TRUE
        
      } else if (is.na(status) && !has_ok_click) {
        status  <- "Not submitted"
        correct <- FALSE
      }
      
      # answer display string (status + optional value)
      ans_val <- format_answer_value(evts, final_idx)
      answer_txt <- if (!is.na(status) && !is.na(ans_val) && nzchar(ans_val)) {
        paste(status, ans_val)
      } else {
        status
      }
      
      # direction metrics
      ans_b <- NA_real_
      cor_b <- NA_real_
      err_deg <- NA_real_
      
      is_dir <- (!is.na(task_type) && task_type == "theme-direction") ||
        (!is.null(evts$task$evaluate) && !is.na(evts$task$evaluate[final_idx]) &&
           evts$task$evaluate[final_idx] %in% c("evalMapDirection","evalDirection"))
      
      if (is_dir) {
        ans_b <- get_answer_bearing(final_idx, evts)
        cor_b <- get_correct_bearing(final_idx, evts)
        err_deg <- angle_diff_deg(ans_b, cor_b)
      }
      
      dist_to_target_m <- get_task_error_m(evts, final_idx)
      error_txt <- format_task_error(evts, final_idx)
      # --- END PATCH 6 ---
      
      # distance travelled for this task attempt (from waypoints within [start,end])
      dist_travel_m <- distance_travelled_between(track, t_start, t_end)
      
      out[[k]] <- data.frame(
        taskNo = sl$taskNo[k],
        task_id = task_id,
        task_type = task_type,
        assignment = assignment,
        category = task_cat,
        start_idx = s,
        end_idx = e,
        final_idx = final_idx,
        start_time = t_start,
        end_time = t_end,
        time_s = time_s,
        tries = tries,
        status = status,
        answer_txt = answer_txt,
        ans_bearing = ans_b,
        cor_bearing = cor_b,
        err_deg = err_deg,
        dist_to_target_m = dist_to_target_m,
        dist_travel_m = dist_travel_m,
        error_txt = error_txt,
        stringsAsFactors = FALSE
      )
    }
    
    do.call(rbind, out)
  }
  
  # -------------------- END CANONICAL TASK SUMMARY --------------------
  
  #######HELPER FUNCTION FOR CLEANLY EXPORTING AND DOWNLOADING THE CSV (USED FOR THE FUNCTION BELOW THAT IS 'BUILD_BIG_TABLE_EXPORT') - START ######
  clean_export_text <- function(x) {
    x <- as.character(x)
    x <- gsub("[\r\n]+", " ", x)
    x <- gsub("\\s{2,}", " ", x)
    trimws(x)
  }
  #######HELPER FUNCTION FOR CLEANLY EXPORTING AND DOWNLOADING THE CSV (USED FOR THE FUNCTION BELOW THAT IS 'BUILD_BIG_TABLE_EXPORT') - END ######
  
  
  
  ######## MAKING HELPER FUNCTION FOR ADDING THE NEW DOWNLOAD BUTTON IN BIG TABLE 'SAVE ALL TO CSV' START #############
  pretty_task_type <- function(x) {
    x <- as.character(x)
    
    if (length(x) == 0 || is.na(x) || x == "") return("information")
    
    switch(
      x,
      "nav-flag" = "Navigation to flag",
      "nav-arrow" = "Navigation with arrow",
      "nav-photo" = "Navigation via photo",
      "nav-text" = "Navigation via text",
      "theme-loc" = "Self location",
      "theme-object" = "Object location",
      "theme-direction" = "Direction determination",
      "free" = "Free",
      "info" = "Information",
      x
    )
  }
  
  build_big_table_export_df <- function(track) {
    sm <- task_summary(track)
    
    if (is.null(sm) || nrow(sm) == 0) {
      return(data.frame())
    }
    
    df <- data.frame(
      `Task ID` = sm$taskNo,
      Type = vapply(sm$task_type, pretty_task_type, character(1)),
      Assignment = clean_export_text(sm$assignment),
      Answer = clean_export_text(sm$answer_txt),
      Time = ifelse(is.na(sm$time_s), NA_character_, paste0(sm$time_s, " s")),
      Tries = sm$tries,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    df[["Error in °/m"]] <- sm$error_txt
    
    info_mask <- tolower(trimws(df$Type)) == "information"
    df$Answer[info_mask] <- NA_character_
    df$Tries[info_mask] <- 0
    df[["Error in °/m"]][info_mask] <- NA_character_
    
    df
  }
  
  apply_task_id_filter_export <- function(df, selected_task_ids) {
    if (is.null(df) || nrow(df) == 0) return(df)
    
    if (is.null(selected_task_ids) || length(selected_task_ids) == 0) {
      return(df)
    }
    
    df[as.character(df[["Task ID"]]) %in% as.character(selected_task_ids), , drop = FALSE]
  }
  ######## MAKING HELPER FUNCTION FOR ADDING THE NEW DOWNLOAD BUTTON IN BIG TABLE 'SAVE ALL TO CSV' END #############
  
  
  
  ############ SAVED CSVS ENCODING ISSUE -- GERMAN CHARACTERS --- STARTS #####################
  
  write_csv_excel_utf8 <- function(df, file, na = "NA") {
    if (is.null(df)) df <- data.frame()
    
    names(df) <- enc2utf8(names(df))
    char_cols <- vapply(df, is.character, logical(1))
    if (any(char_cols)) {
      df[char_cols] <- lapply(df[char_cols], enc2utf8)
    }
    
    con <- file(file, open = "wb")
    writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
    close(con)
    
    utils::write.table(
      df,
      file = file,
      sep = ",",
      row.names = FALSE,
      col.names = TRUE,
      na = na,
      quote = TRUE,
      qmethod = "double",
      append = TRUE,
      fileEncoding = "UTF-8",
      eol = "\r\n"
    )
  }
  ############ SAVED CSVS ENCODING ISSUE -- GERMAN CHARACTERS --- ENDS  #####################
  
  
  ####### starts - MAKING HELPER FUNCTION TO CORRECT THE COMPARE PLAYERS TAB - CSV FILE DOWNLOAD#########
  get_selected_game_name <- function() {
    games_map <- games_choices_rv()
    game_name <- NA_character_
    
    if (!is.null(games_map) && length(games_map) > 0 && !is.null(input$selected_games)) {
      idx <- which(games_map == input$selected_games)
      if (length(idx) > 0) {
        game_name <- names(games_map)[idx[1]]
      }
    }
    
    if (is.null(game_name) || length(game_name) == 0 || is.na(game_name)) {
      game_name <- "Unknown Game"
    }
    
    game_name
  }
  
  sanitize_filename <- function(x) {
    x <- as.character(x)
    x <- gsub("[^A-Za-z0-9_\\-]", "_", x)
    x
  }
  ####### ends  - MAKING HELPER FUNCTION TO CORRECT THE COMPARE PLAYERS TAB - CSV FILE DOWNLOAD#########
  
  
  
  
  
  
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
  
  
  #### only players selected on left should appear on right *starts) #####
  
  filtered_choices_r <- reactive({
    req(choices_rv())
    req(input$selected_files)
    
    all_choices <- choices_rv()
    sel <- input$selected_files
    
    # keep only players currently selected in the left sidebar
    all_choices[all_choices %in% sel]
  })
  #### only players selected on left should appear on right *ends) #####
  
  
  
  # apiURL_rv <- reactiveVal("http://localhost:3000")
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
    req(filtered_choices_r())
    
    choices_now <- filtered_choices_r()
    
    cur <- isolate(input$selected_data_file)
    selected_now <- if (!is.null(cur) && cur %in% choices_now) cur else choices_now[1]
    
    pickerInput(
      "selected_data_file",
      "Selected Players:",
      choices = choices_now,
      selected = selected_now,
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
    req(filtered_choices_r())
    
    choices_now <- filtered_choices_r()
    
    cur <- isolate(input$selected_multiple_files)
    selected_now <- intersect(cur, choices_now)
    if (length(selected_now) == 0) selected_now <- choices_now
    
    tagList(
      pickerInput(
        "selected_multiple_files",
        "Selected Players:",
        choices = choices_now,
        selected = selected_now,
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
    req(filtered_choices_r())
    
    choices_now <- filtered_choices_r()
    
    cur <- isolate(input$selected_multiple_files)
    selected_now <- intersect(cur, choices_now)
    if (length(selected_now) == 0) selected_now <- choices_now
    
    tagList(
      pickerInput(
        "selected_multiple_files",
        "Selected Players:",
        choices = choices_now,
        selected = selected_now,
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
    req(filtered_choices_r())
    
    choices_now <- filtered_choices_r()
    
    cur <- isolate(input$selected_data_file)
    selected_now <- if (!is.null(cur) && cur %in% choices_now) cur else choices_now[1]
    
    pickerInput(
      "selected_data_file",
      "Selected Players: ",
      choices = choices_now,
      selected = selected_now,
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
  
  ##### Filter for photos
  output$file_selector_ui4 <- renderUI({
    req(filtered_choices_r())
    
    choices_now <- filtered_choices_r()
    
    cur <- isolate(input$selected_data_file)
    selected_now <- if (!is.null(cur) && cur %in% choices_now) cur else choices_now[1]
    
    pickerInput(
      "selected_data_file",
      "Selected Players: ",
      choices = choices_now,
      selected = selected_now,
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
    
    dist <- list()
    
    for (j in 1:(length(id) - 1)) {
      if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
        dist <- append(dist, format_task_error(data[[1]]$events, j))
      }
    }
    
    
    
    
    
    
    
    
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
    
    # Build the main table from the same canonical source used by "Save all to CSV"
    sm <- task_summary(data[[1]])
    
    df <- data.frame(
      Type = vapply(sm$task_type, pretty_task_type, character(1)),
      Assignment = sm$assignment,
      Answer = sm$answer_txt,
      Time = ifelse(is.na(sm$time_s), NA_character_, paste0(sm$time_s, " s")),
      Tries = sm$tries,
      `Error in °/m` = sm$error_txt,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    info_mask <- tolower(trimws(df$Type)) == "information"
    df$Answer[info_mask] <- NA_character_
    df$Tries[info_mask] <- 0
    df[["Error in °/m"]][info_mask] <- NA_character_
    
    
    
    
    
    #Name of the player
    #print(data[[1]]$players[1])
    output$player_name <- renderText({
      paste("Player: ", data[[1]]$players[1], sep = "")
    })
    
    
    #Overall score
    good <- sum(sm$status == "Correct", na.rm = TRUE)
    total <- nrow(sm)
    
    
    
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
      cur <- suppressWarnings(as.integer(input$num_value))
      if (is.na(cur) || cur < 1 || cur > n) cur <- 1
      
      update_main_task_pickers(
        session = session,
        track = data[[1]],
        n_tasks = n,
        selected = cur
      )
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
      
      task_ids <- as.character(seq_len(nrow(df)))   # row numbers used in All tasks tab
      
      # preserve previous selection when switching player
      prev_selected <- isolate(input$selected_task_ids)
      prev_selected <- as.character(prev_selected)
      
      # keep only those that still exist for the newly selected player
      selected_ids <- intersect(prev_selected, task_ids)
      
      # if nothing valid remains, fall back to all
      if (length(selected_ids) == 0) {
        selected_ids <- task_ids
      }
      
      tagList(
        pickerInput(
          "selected_task_ids",
          "Filter by Task ID:",
          choices = task_ids,
          selected = selected_ids,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = FALSE,
            `none-selected-text` = "Filter by Task ID:",
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
      filename = function() {
        player_name <- "player"
        
        tr <- track_data_rv()
        if (!is.null(tr) && !is.list(tr[[1]]) && !is.null(tr$players) && length(tr$players) > 0) {
          player_name <- as.character(tr$players[1])
        } else if (!is.null(tr) && !is.null(tr$players) && length(tr$players) > 0) {
          player_name <- as.character(tr$players[1])
        }
        
        player_name <- gsub("[^A-Za-z0-9_\\-]", "_", player_name)
        paste0(player_name, "_", Sys.Date(), ".csv")
      },
      
      content = function(file) {
        df_out <- filtered_df()
        
        if (!is.null(df_out) && nrow(df_out) > 0) {
          if ("Assignment" %in% names(df_out)) {
            df_out$Assignment <- clean_export_text(df_out$Assignment)
          }
          if ("Answer" %in% names(df_out)) {
            df_out$Answer <- clean_export_text(df_out$Answer)
          }
        }
        
        # get game name
        games_map <- games_choices_rv()
        game_name <- NA_character_
        
        if (!is.null(games_map) && length(games_map) > 0 && !is.null(input$selected_games)) {
          idx <- which(games_map == input$selected_games)
          if (length(idx) > 0) {
            game_name <- names(games_map)[idx[1]]
          }
        }
        
        if (is.null(game_name) || length(game_name) == 0 || is.na(game_name)) {
          game_name <- "Unknown Game"
        }
        
        # get player name
        player_name <- NA_character_
        tr <- track_data_rv()
        if (!is.null(tr) && !is.null(tr$players) && length(tr$players) > 0) {
          player_name <- as.character(tr$players[1])
        }
        
        # prepend columns
        if (!is.null(df_out) && nrow(df_out) > 0) {
          df_out <- data.frame(
            Game = game_name,
            Player = player_name,
            df_out,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        }
        
        write_csv_excel_utf8(df_out, file, na = "NA")
      }
    )
    
    ########NEW DOWNLOAD BUTTON FOR ALL TASKS BIG TABLE - 'SAVE ALL TO CSV' starts########
    output$save_all_data <- downloadHandler(
      filename = function() {
        games_map <- games_choices_rv()
        game_name <- "geogami_game"
        
        if (!is.null(games_map) && length(games_map) > 0 && !is.null(input$selected_games)) {
          idx <- which(games_map == input$selected_games)
          if (length(idx) > 0) {
            game_name <- names(games_map)[idx[1]]
          }
        }
        
        game_name <- gsub("[^A-Za-z0-9_\\-]", "_", game_name)
        paste0(game_name, "_all_tasks_", Sys.Date(), ".csv")
      },
      
      content = function(file) {
        req(input$selected_files)
        req(accessToken_rv())
        req(apiURL_rv())
        
        selected_task_ids_now <- input$selected_task_ids
        player_blocks <- list()
        
        # get readable game name once
        games_map <- games_choices_rv()
        game_name <- NA_character_
        
        if (!is.null(games_map) && length(games_map) > 0 && !is.null(input$selected_games)) {
          idx <- which(games_map == input$selected_games)
          if (length(idx) > 0) {
            game_name <- names(games_map)[idx[1]]
          }
        }
        
        if (is.null(game_name) || length(game_name) == 0 || is.na(game_name)) {
          game_name <- "Unknown Game"
        }
        
        for (track_id in input$selected_files) {
          url <- paste0(apiURL_rv(), "/track/", track_id)
          tr <- fetch_games_data_from_server(url, accessToken_rv())
          
          if (is.null(tr)) next
          
          if (!is.null(tr$events)) {
            tr$events <- fix_vr_distance_tasks(tr$events)
            tr$events <- fix_direction_tasks(tr$events)
          }
          
          df_one <- build_big_table_export_df(tr)
          if (nrow(df_one) == 0) next
          
          df_one <- apply_task_id_filter_export(df_one, selected_task_ids_now)
          if (nrow(df_one) == 0) next
          
          player_name <- if (!is.null(tr$players) && length(tr$players) > 0) {
            as.character(tr$players[1])
          } else {
            NA_character_
          }
          
          created_at <- if (!is.null(tr$createdAt) && length(tr$createdAt) > 0) {
            as.character(tr$createdAt[1])
          } else {
            NA_character_
          }
          
          df_one <- data.frame(
            Game = game_name,
            Player = player_name,
            `Created At` = created_at,
            df_one,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          
          player_blocks[[length(player_blocks) + 1]] <- df_one
        }
        
        if (length(player_blocks) == 0) {
          write_csv_excel_utf8(data.frame(), file, na = "NA")
          return()
        }
        
        # Write UTF-8 BOM once
        con <- file(file, open = "wb")
        writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
        close(con)
        
        for (i in seq_along(player_blocks)) {
          # Add exactly 2 blank lines BETWEEN players
          if (i > 1) {
            cat("\r\n\r\n", file = file, append = TRUE)
          }
          
          utils::write.table(
            player_blocks[[i]],
            file = file,
            sep = ",",
            row.names = FALSE,
            col.names = (i == 1),
            na = "NA",
            quote = TRUE,
            qmethod = "double",
            append = TRUE,
            fileEncoding = "UTF-8",
            eol = "\r\n"
          )
        }
      }
    )
    ########NEW DOWNLOAD BUTTON FOR ALL TASKS BIG TABLE - 'SAVE ALL TO CSV' ENDS ########
    
    
    
    
    
    output$save_big_table <- renderUI({
      req(filtered_df())
      
      if (nrow(filtered_df()) > 0) {
        tagList(
          div(
            style = "display: flex; gap: 10px; align-items: center; flex-wrap: wrap;",
            downloadButton('save_data', 'Save to CSV'),
            if (!is.null(input$selected_files) && length(input$selected_files) > 0) {
              downloadButton('save_all_data', 'Save all to CSV')
            }
          )
        )
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
    
    
    # if (!is.na(dir_ok_idx) && t == "theme-direction") {
    #   evts <- data[[1]]$events
    #   
    #   lon0 <- num(evts$position$coords$longitude[dir_ok_idx])
    #   lat0 <- num(evts$position$coords$latitude[dir_ok_idx])
    #   
    #   ans_b <- get_answer_bearing(dir_ok_idx, evts)
    #   cor_b <- get_correct_bearing(dir_ok_idx, evts)
    #   
    #   if (is.finite(lon0) && is.finite(lat0) && !is.na(ans_b)) {
    #     A <- arrow_lines(lon0, lat0, ans_b)
    #     
    #     map_shown <- leaflet() %>%
    #       addTiles() %>%
    #       addMarkers(lng = lon0, lat = lat0, icon = loc_marker) %>%
    #       addPolylines(lng = A$main$lng,  lat = A$main$lat) %>%
    #       addPolylines(lng = A$left$lng,  lat = A$left$lat) %>%
    #       addPolylines(lng = A$right$lng, lat = A$right$lat) %>%
    #       setView(lng = lon0, lat = lat0, zoom = 19)
    #     
    #     # optional: draw correct direction (dashed)
    #     if (!is.na(cor_b)) {
    #       C <- arrow_lines(lon0, lat0, cor_b)
    #       map_shown <- map_shown %>%
    #         addPolylines(lng = C$main$lng,  lat = C$main$lat,  opacity = 0.6, dashArray = "5,5") %>%
    #         addPolylines(lng = C$left$lng,  lat = C$left$lat,  opacity = 0.6, dashArray = "5,5") %>%
    #         addPolylines(lng = C$right$lng, lat = C$right$lat, opacity = 0.6, dashArray = "5,5")
    #     }
    #     
    #     mr <- FALSE
    #   }
    # }
    
    
    
    
    if (!is.na(dir_ok_idx) && t == "theme-direction") {
      evts <- data[[1]]$events

      lon0 <- num(evts$position$coords$longitude[dir_ok_idx])
      lat0 <- num(evts$position$coords$latitude[dir_ok_idx])

      ans_b <- get_answer_bearing(dir_ok_idx, evts)
      cor_b <- get_correct_bearing(dir_ok_idx, evts)

      if (is.finite(lon0) && is.finite(lat0) && !is.na(ans_b)) {
        A <- arrow_lines(lon0, lat0, ans_b, len_m = 18, head_m = 5, head_ang = 25)

        map_shown <- leaflet() %>%
          addTiles() %>%
          addMarkers(lng = lon0, lat = lat0, icon = loc_marker) %>%
          
          
          # Player FINAL answer arrow (BLUE)
          addPolylines(lng = A$main$lng,  lat = A$main$lat,  color = "blue",  weight = 4, opacity = 1) %>%
          addPolylines(lng = A$left$lng,  lat = A$left$lat,  color = "blue",  weight = 4, opacity = 1) %>%
          addPolylines(lng = A$right$lng, lat = A$right$lat, color = "blue",  weight = 4, opacity = 1) %>%
          setView(lng = lon0, lat = lat0, zoom = 19)

        # Correct direction arrow (GREEN) - optional
        if (!is.na(cor_b)) {
          C <- arrow_lines(lon0, lat0, cor_b, len_m = 18, head_m = 5, head_ang = 25)
          map_shown <- map_shown %>%
                addPolylines(lng = C$main$lng,  lat = C$main$lat,  color = "green", weight = 4, opacity = 0.9) %>%
                addPolylines(lng = C$left$lng,  lat = C$left$lat,  color = "green", weight = 4, opacity = 0.9) %>%
                addPolylines(lng = C$right$lng, lat = C$right$lat, color = "green", weight = 4, opacity = 0.9)
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
      cou <- 1
      pict <- list()
      ans_photo <- list()
      
      for (i in 1:(length(id) - 1)) {
        if ((!is.na(id[i]) && (i != 1) && (id[i] != id[i + 1])) || i == (length(id) - 1)) {
          cou <- cou + 1
          
          pict <- append(pict, safe_photo_at(data[[1]]$events$task$question$photo, i))
          ans_photo <- append(ans_photo, safe_photo_at(data[[1]]$events$answer$photo, i))
        }
      }
      
      
      if (length(pict) != 0) { #Photos in assignment
        if (num_value_num() <= length(pict) && !is.na(pict[[num_value_num()]]) && pict[[num_value_num()]] != "") {
          # Render photo display with download buttons
          output$photo_display <- renderUI({
            
            photo_url <- trimws(pict[[num_value_num()]])
            
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
    data_ref  <- loaded_json()
    data_list <- load_multiple()
    
    ref_track <- data_ref[[1]]
    ref_sum   <- task_summary(ref_track)
    
    if (is.null(ref_sum) || nrow(ref_sum) < num_value_num()) {
      output$cmp_table1 <- renderTable(data.frame())
      output$cmp_table2 <- renderTable(data.frame())
      output$tabLegend  <- renderText("Task type: Information")
      output$graphLegend<- renderText("Task type: Information")
      return()
    }
    
    ref_task_id   <- ref_sum$task_id[num_value_num()]
    ref_task_type <- ref_sum$task_type[num_value_num()]
    
    # Tables
    ngts <- data.frame(
      Name = character(0),
      Correct = character(0),
      Time = character(0),
      `Distance travelled` = character(0),
      `Error to target` = character(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    cores <- data.frame(
      Name = character(0),
      Correct = character(0),
      Answer = character(0),
      Error = character(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    # Build per-player row by matching task_id (never by taskNo)
    for (i in seq_along(data_list)) {
      tr <- data_list[[i]]
      nm <- if (!is.null(tr$players) && length(tr$players)) tr$players[1] else paste0("Player_", i)
      
      sm <- task_summary(tr)
      hit <- sm[sm$task_id == ref_task_id, , drop = FALSE]
      
      if (!nrow(hit)) {
        ngts <- rbind(ngts, data.frame(
          Name = nm,
          Correct = "Task not played",
          Time = NA_character_,
          `Distance travelled` = NA_character_,
          `Error to target` = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
        next
      }
      
      # if the same task_id appears multiple times (back button / replay), use the LAST attempt
      row <- hit[nrow(hit), , drop = FALSE]
      
      correct_txt <- row$status
      time_txt <- if (is.na(row$time_s)) NA_character_ else paste0(row$time_s, " s")
      dist_txt <- if (is.na(row$dist_travel_m)) NA_character_ else paste0(round(row$dist_travel_m), " m")
      
      # Big-table style:
      # - blank if Correct
      # - else (distance - accuracy) with 15m fallback
      dist_to_target_txt <- row$error_txt
      
      # Navigation-style table (also used in Statistics time-vs-distance)
      ngts <- rbind(ngts, data.frame(
        Name = nm,
        Correct = ifelse(is.na(correct_txt), NA_character_, correct_txt),
        Time = time_txt,
        `Distance travelled` = dist_txt,
        `Error to target` = dist_to_target_txt,
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
      
      # Direction table only for theme-direction (Answer/Error in degrees)
      if (!is.na(ref_task_type) && ref_task_type == "theme-direction") {
        ans_deg <- row$ans_bearing
        err_deg <- row$err_deg
        
        cores <- rbind(cores, data.frame(
          Name = nm,
          Correct = ifelse(is.na(correct_txt), NA_character_, correct_txt),
          Answer = ifelse(is.na(ans_deg), NA_character_, paste0(round(ans_deg, 3), " °")),
          Error  = ifelse(is.na(err_deg), NA_character_, paste0(round(err_deg, 3), " °")),
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Render tables
    output$cmp_table1 <- renderTable(ngts)
    output$cmp_table2 <- renderTable(cores)
    
    # Legends
    t <- ref_task_type
    if (!is.na(t)) {
      if (t == "nav-flag") t <- "Navigation to flag"
      if (t == "nav-arrow") t <- "Navigation with arrow"
      if (t == "nav-photo") t <- "Navigation via photo"
      if (t == "nav-text") t <- "Navigation via text"
      if (t == "theme-loc") t <- "Self location"
      if (t == "theme-object") t <- "Object location"
      if (t == "theme-direction") t <- "Direction determination"
      if (t == "free") t <- "Free"
      if (t == "info") t <- "Information"
    } else {
      t <- "Information"
    }
    
    output$tabLegend   <- renderText({ paste("Task type:", t) })
    output$graphLegend <- renderText({ paste("Task type:", t) })
    
    # ---------- Statistics outputs (pie + time chart) derived from SAME ngts ----------
    # Pie (Correct vs Incorrect)
    valid <- ngts$Correct %in% c("Correct", "Incorrect")
    corr  <- sum(ngts$Correct[valid] == "Correct", na.rm = TRUE)
    incorr<- sum(ngts$Correct[valid] == "Incorrect", na.rm = TRUE)
    
    if (corr == 0 && incorr == 0) {
      pie_chart <- ggplot() + theme_void() + labs(title = "No comparable answers for this task")
    } else {
      df_pie <- data.frame(
        Answers = c("Correct", "Incorrect"),
        value   = c(corr, incorr),
        stringsAsFactors = FALSE
      )
      df_pie$percentage <- round(df_pie$value / sum(df_pie$value) * 100, 1)
      df_pie$label <- paste0(df_pie$Answers, ": ", df_pie$percentage, "%")
      
      num_players <- length(input$selected_multiple_files)
      
      pie_chart <- ggplot(df_pie, aes(x = "", y = value, fill = Answers)) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        geom_text(aes(label = label), position = position_stack(vjust = 0.5),
                  color = "BLACK", size = 5, fontface = "bold") +
        scale_fill_manual(values = c("Correct" = "green", "Incorrect" = "red")) +
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
    
    output$pie_chart  <- renderPlot({ pie_chart })
    output$pie_chart2 <- renderPlot({ pie_chart })
    
    # Time vs distance scatter (uses same ngts table)
    time_num <- suppressWarnings(as.numeric(sub(" s$", "", ngts$Time)))
    dist_num <- suppressWarnings(as.numeric(sub(" m$", "", ngts$`Distance travelled`)))
    
    df_player <- data.frame(
      time = time_num,
      distance = dist_num,
      players = ngts$Name,
      stringsAsFactors = FALSE
    )
    df_player <- df_player[is.finite(df_player$time) & is.finite(df_player$distance), , drop = FALSE]
    
    if (!nrow(df_player)) {
      time_chart <- ggplot() + theme_void() + labs(title = "No comparable trajectory data for this task")
    } else {
      time_chart <- ggplot(df_player, aes(x = time, y = distance, fill = players)) +
        geom_point(size = 4, shape = 22) +
        labs(title = paste("Time chart of task:", t),
             x = "Time for this task in seconds",
             y = "Distance travelled in metres")
    }
    
    output$time_chart <- renderPlot({ time_chart })
    
    # Save charts
    output$save_picture <- downloadHandler(
      filename = function(){ paste0("pie_chart_", Sys.Date(), ".png") },
      content  = function(file){ png(file); print(pie_chart); dev.off() }
    )
    output$save_picture2 <- downloadHandler(
      filename = function(){ paste0("pie_chart_", Sys.Date(), ".png") },
      content  = function(file){ png(file); print(pie_chart); dev.off() }
    )
    
    output$save_time_chart <- downloadHandler(
      filename = function(){ paste0("time_chart_", Sys.Date(), ".png") },
      content  = function(file){ png(file); print(time_chart); dev.off() }
    )
    
    # Save CSVs
    output$save_table1 <- downloadHandler(
      filename = function() {
        game_name_safe <- sanitize_filename(get_selected_game_name())
        paste0("Compare_", game_name_safe, "_route_length_vs_time_", Sys.Date(), ".csv")
      },
      content = function(file) {
        game_name <- get_selected_game_name()
        
        df_out <- ngts
        
        if (!is.null(df_out) && nrow(df_out) > 0) {
          df_out <- data.frame(
            Game = game_name,
            Player = df_out$Name,
            Correct = df_out$Correct,
            Time = df_out$Time,
            `Distance travelled` = df_out$`Distance travelled`,
            `Error to target` = df_out$`Error to target`,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        }
        
        write_csv_excel_utf8(df_out, file, na = "NA")
      }
    )
    
    output$save_table2 <- downloadHandler(
      filename = function() {
        game_name_safe <- sanitize_filename(get_selected_game_name())
        paste0("Compare_", game_name_safe, "_answer_error_", Sys.Date(), ".csv")
      },
      content = function(file) {
        game_name <- get_selected_game_name()
        
        df_out <- cores
        
        if (!is.null(df_out) && nrow(df_out) > 0) {
          df_out <- data.frame(
            Game = game_name,
            Player = df_out$Name,
            Correct = df_out$Correct,
            Answer = df_out$Answer,
            Error = df_out$Error,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        }
        
        write_csv_excel_utf8(df_out, file, na = "NA")
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
    dist <- list()
    
    for (j in 1:(length(id) - 1)) {
      if ((!is.na(id[j]) && (id[j] != id[j + 1])) || j == (length(id) - 1)) {
        dist <- append(dist, format_task_error(data[[1]]$events, j))
      }
    }
    
    
    
    
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
    
    # Build the main table from the same canonical source used by "Save all to CSV"
    sm <- task_summary(data[[1]])
    
    df <- data.frame(
      Type = vapply(sm$task_type, pretty_task_type, character(1)),
      Assignment = sm$assignment,
      Answer = sm$answer_txt,
      Time = ifelse(is.na(sm$time_s), NA_character_, paste0(sm$time_s, " s")),
      Tries = sm$tries,
      `Error in °/m` = sm$error_txt,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    info_mask <- tolower(trimws(df$Type)) == "information"
    df$Answer[info_mask] <- NA_character_
    df$Tries[info_mask] <- 0
    df[["Error in °/m"]][info_mask] <- NA_character_
    
    
    
    
    
    #Name of the player
    #print(data[[1]]$players[1])
    output$player_name <- renderText({
      paste("Player: ", data[[1]]$players[1], sep = "")
    })
    
    
    #Overall score
    good <- sum(sm$status == "Correct", na.rm = TRUE)
    total <- nrow(sm)
    
    
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
      cur <- suppressWarnings(as.integer(input$num_value))
      if (is.na(cur) || cur < 1 || cur > n) cur <- 1
      
      update_main_task_pickers(
        session = session,
        track = data[[1]],
        n_tasks = n,
        selected = cur
      )
    })
    ###############----------Task IDs updation all 4 STARTS-------------################
    
    output$iris_data <- renderDT({
      df_react()
    })
    
    
    
    
    #-----------all tasks - id checkbox filter starts --------------------------------
    output$task_id_selector <- renderUI({
      req(df_react())
      df <- df_react()
      
      task_ids <- as.character(seq_len(nrow(df)))   # row numbers used in All tasks tab
      
      # preserve previous selection when switching player
      prev_selected <- isolate(input$selected_task_ids)
      prev_selected <- as.character(prev_selected)
      
      # keep only those that still exist for the newly selected player
      selected_ids <- intersect(prev_selected, task_ids)
      
      # if nothing valid remains, fall back to all
      if (length(selected_ids) == 0) {
        selected_ids <- task_ids
      }
      
      tagList(
        pickerInput(
          "selected_task_ids",
          "Filter by Task ID:",
          choices = task_ids,
          selected = selected_ids,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = FALSE,
            `none-selected-text` = "Filter by Task ID:",
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
      filename = function() {
        player_name <- "player"
        
        data_now <- uploaded_json()
        if (!is.null(data_now) && length(data_now) > 0 && !is.null(data_now[[1]]$players) && length(data_now[[1]]$players) > 0) {
          player_name <- as.character(data_now[[1]]$players[1])
        }
        
        player_name <- gsub("[^A-Za-z0-9_\\-]", "_", player_name)
        paste0(player_name, "_", Sys.Date(), ".csv")
      },
      
      content = function(file) {
        df_out <- filtered_df()
        
        if (!is.null(df_out) && nrow(df_out) > 0) {
          if ("Assignment" %in% names(df_out)) {
            df_out$Assignment <- clean_export_text(df_out$Assignment)
          }
          if ("Answer" %in% names(df_out)) {
            df_out$Answer <- clean_export_text(df_out$Answer)
          }
        }
        
        data_now <- uploaded_json()
        
        game_name <- if (!is.null(input$selected_games) && nzchar(input$selected_games)) {
          as.character(input$selected_games)
        } else {
          "Uploaded JSON"
        }
        
        player_name <- NA_character_
        if (!is.null(data_now) && length(data_now) > 0 && !is.null(data_now[[1]]$players) && length(data_now[[1]]$players) > 0) {
          player_name <- as.character(data_now[[1]]$players[1])
        }
        
        if (!is.null(df_out) && nrow(df_out) > 0) {
          df_out <- data.frame(
            Game = game_name,
            Player = player_name,
            df_out,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        }
        
        write_csv_excel_utf8(df_out, file, na = "NA")
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
    
    
    
    
    
    # if (!is.na(dir_ok_idx) && t == "theme-direction") {
    #   evts <- data[[1]]$events
    #   lon0 <- num(evts$position$coords$longitude[dir_ok_idx])
    #   lat0 <- num(evts$position$coords$latitude[dir_ok_idx])
    #   
    #   ans_b <- get_answer_bearing(dir_ok_idx, evts)
    #   cor_b <- get_correct_bearing(dir_ok_idx, evts)
    #   
    #   if (is.finite(lon0) && is.finite(lat0) && !is.na(ans_b)) {
    #     A <- arrow_lines(lon0, lat0, ans_b)
    #     
    #     map_shown <- leaflet() %>%
    #       addTiles() %>%
    #       addMarkers(lng = lon0, lat = lat0, icon = loc_marker) %>%
    #       addPolylines(lng = A$main$lng,  lat = A$main$lat) %>%
    #       addPolylines(lng = A$left$lng,  lat = A$left$lat) %>%
    #       addPolylines(lng = A$right$lng, lat = A$right$lat) %>%
    #       setView(lng = lon0, lat = lat0, zoom = 19)
    #     
    #     if (!is.na(cor_b)) {
    #       C <- arrow_lines(lon0, lat0, cor_b)
    #       map_shown <- map_shown %>%
    #         addPolylines(lng = C$main$lng,  lat = C$main$lat,  opacity = 0.6, dashArray = "5,5") %>%
    #         addPolylines(lng = C$left$lng,  lat = C$left$lat,  opacity = 0.6, dashArray = "5,5") %>%
    #         addPolylines(lng = C$right$lng, lat = C$right$lat, opacity = 0.6, dashArray = "5,5")
    #     }
    #     
    #     mr <- FALSE  # ensure fallback doesn't blank it
    #   }
    # }
    
    
    
    
    if (!is.na(dir_ok_idx) && t == "theme-direction") {
      evts <- data[[1]]$events
      
      lon0 <- num(evts$position$coords$longitude[dir_ok_idx])
      lat0 <- num(evts$position$coords$latitude[dir_ok_idx])
      
      ans_b <- get_answer_bearing(dir_ok_idx, evts)
      cor_b <- get_correct_bearing(dir_ok_idx, evts)
      
      if (is.finite(lon0) && is.finite(lat0) && !is.na(ans_b)) {
        A <- arrow_lines(lon0, lat0, ans_b, len_m = 18, head_m = 5, head_ang = 25)
        
        map_shown <- leaflet() %>%
          addTiles() %>%
          addMarkers(lng = lon0, lat = lat0, icon = loc_marker) %>%
          
          
          # Player FINAL answer arrow (BLUE)
          addPolylines(lng = A$main$lng,  lat = A$main$lat,  color = "blue",  weight = 4, opacity = 1) %>%
          addPolylines(lng = A$left$lng,  lat = A$left$lat,  color = "blue",  weight = 4, opacity = 1) %>%
          addPolylines(lng = A$right$lng, lat = A$right$lat, color = "blue",  weight = 4, opacity = 1) %>%
          setView(lng = lon0, lat = lat0, zoom = 19)
        
        # Correct direction arrow (GREEN) - optional
        if (!is.na(cor_b)) {
          C <- arrow_lines(lon0, lat0, cor_b, len_m = 18, head_m = 5, head_ang = 25)
          map_shown <- map_shown %>%
            addPolylines(lng = C$main$lng,  lat = C$main$lat,  color = "green", weight = 4, opacity = 0.9) %>%
            addPolylines(lng = C$left$lng,  lat = C$left$lat,  color = "green", weight = 4, opacity = 0.9) %>%
            addPolylines(lng = C$right$lng, lat = C$right$lat, color = "green", weight = 4, opacity = 0.9)
        }
        
        mr <- FALSE
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
    cou <- 1
    pict <- list()
    ans_photo <- list()
    
    for (i in 1:(length(id) - 1)) {
      if ((!is.na(id[i]) && (i != 1) && (id[i] != id[i + 1])) || i == (length(id) - 1)) {
        cou <- cou + 1
        
        pict <- append(pict, safe_photo_at(data[[1]]$events$task$question$photo, i))
        ans_photo <- append(ans_photo, safe_photo_at(data[[1]]$events$answer$photo, i))
      }
    }
    
    
    if (length(pict) != 0) { #Photos in assignment
      if (num_value_num() <= length(pict) && !is.na(pict[[num_value_num()]]) && pict[[num_value_num()]] != "") {
        # Render photo display with download buttons
        output$photo_display <- renderUI({
          
          photo_url <- trimws(pict[[num_value_num()]])
          
          
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


  # ── Share game tracks ──────────────────────────────────────────────
  # This section lets the game creator grant other GeoGami users access
  # to view the tracks of a specific game via the dashboard.
  #
  # How it works:
  # - The creator selects a game in the sidebar and clicks "Share game tracks".
  # - A modal opens showing an email input and the list of currently-shared users.
  # - Adding an email calls POST /game/:id/share on the server, which pushes
  #   the email into the game's sharedWith array.
  # - Removing an email calls DELETE /game/:id/share to revoke access.
  # - The recipient sees the shared game in their own dashboard the next time
  #   they open it — getUserGames now includes games where sharedWith contains
  #   the authenticated user's email.

  # Reactive value holding the current shared-with emails for the selected game.
  shared_emails_rv <- reactiveVal(character(0))

  # Show the share button only when a game is selected in the sidebar.
  output$share_button_ui <- renderUI({
    req(input$selected_games)
    div(
      style = "margin-bottom: 10px;",
      actionButton("open_share_modal", "Share game tracks",
                   icon = icon("share-alt"),
                   style = "width: 100%;")
    )
  })

  # When the share button is clicked: fetch the current shared list from the
  # server and open a modal dialog.
  observeEvent(input$open_share_modal, {
    req(input$selected_games, accessToken_rv(), apiURL_rv())

    # Fetch who this game is already shared with.
    game_id <- input$selected_games
    url <- paste0(apiURL_rv(), "/game/", game_id, "/share")
    result <- api_get(url, accessToken_rv())

    if (result$status == 200 && !is.null(result$data$sharedWith)) {
      shared_emails_rv(result$data$sharedWith)
    } else {
      shared_emails_rv(character(0))
    }

    showModal(modalDialog(
      title = "Share game tracks",
      size = "m",
      easyClose = TRUE,

      p("Grant other GeoGami users access to view this game's tracks by entering their email."),

      # Email input + share button in a horizontal row.
      div(
        style = "display: flex; gap: 8px; align-items: flex-end;",
        div(style = "flex: 1;",
            textInput("share_email_input", "Email address:", placeholder = "user@example.com")
        ),
        actionButton("share_add_btn", "Share", icon = icon("plus"),
                     class = "btn-primary", style = "margin-bottom: 15px;")
      ),

      # Dynamic list of currently-shared emails (rendered below).
      uiOutput("shared_list_ui"),

      footer = modalButton("Close")
    ))
  })

  # Render the list of emails the game is currently shared with.
  # Each email gets a small "X" button to revoke access.
  output$shared_list_ui <- renderUI({
    emails <- shared_emails_rv()
    if (length(emails) == 0) {
      return(p(style = "color: #888;", "Not shared with anyone yet."))
    }

    tags$div(
      tags$h6(paste0("Shared with (", length(emails), "):")),
      tags$ul(
        style = "list-style: none; padding-left: 0;",
        lapply(seq_along(emails), function(i) {
          tags$li(
            style = "display: flex; align-items: center; justify-content: space-between; padding: 4px 0; border-bottom: 1px solid #eee;",
            tags$span(emails[i]),
            actionButton(
              inputId = paste0("remove_share_", i),
              label = NULL,
              icon = icon("times"),
              class = "btn-sm btn-outline-danger",
              style = "padding: 2px 8px;"
            )
          )
        })
      )
    )
  })

  # Handle the "Share" button click: validate email, POST to server,
  # update the reactive list, and clear the input.
  observeEvent(input$share_add_btn, {
    req(input$selected_games, accessToken_rv(), apiURL_rv())

    email <- trimws(tolower(input$share_email_input))
    if (!grepl("@", email)) {
      showNotification("Please enter a valid email address.", type = "warning")
      return()
    }

    game_id <- input$selected_games
    url <- paste0(apiURL_rv(), "/game/", game_id, "/share")

    # Debug: log the request details to the R console
    message("=== SHARE DEBUG ===")
    message("URL: ", url)
    message("Email: ", email)
    message("Token (first 20 chars): ", substr(accessToken_rv(), 1, 20), "...")

    result <- api_request(url, accessToken_rv(), body = list(emails = list(email)), method = "POST")

    # Debug: log the response
    message("Status: ", result$status)
    message("Response: ", toJSON(result$data, auto_unbox = TRUE))
    message("===================")

    if (result$status == 200) {
      # Server returns the full updated sharedWith array — use it directly
      # so the UI always reflects the server's state.
      shared_emails_rv(result$data$sharedWith)
      updateTextInput(session, "share_email_input", value = "")
      showNotification(paste0("Shared with ", email), type = "message")
    } else {
      msg <- if (!is.null(result$data$message)) result$data$message else "Could not share."
      showNotification(msg, type = "error")
    }
  })

  # Dynamically observe the per-email "X" remove buttons.
  # Each button's ID is "remove_share_N" where N is the position in the list.
  # once = TRUE prevents duplicate observer registration on re-render.
  observe({
    emails <- shared_emails_rv()
    lapply(seq_along(emails), function(i) {
      btn_id <- paste0("remove_share_", i)
      observeEvent(input[[btn_id]], {
        req(input$selected_games, accessToken_rv(), apiURL_rv())

        email_to_remove <- emails[i]
        game_id <- input$selected_games
        url <- paste0(apiURL_rv(), "/game/", game_id, "/share")
        result <- api_request(url, accessToken_rv(),
                              body = list(emails = list(email_to_remove)),
                              method = "DELETE")

        if (result$status == 200) {
          shared_emails_rv(result$data$sharedWith)
          showNotification(paste0("Removed ", email_to_remove), type = "message")
        } else {
          showNotification("Could not remove user.", type = "error")
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })


}

shinyApp(ui, server)