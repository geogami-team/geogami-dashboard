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

# Build the footer string shown in the sidebar: the latest GitHub release
# version plus the date/time of the last commit on the main branch. Computed
# once at app startup; falls back gracefully if GitHub is unreachable.
build_version_footer <- function(repo = "origami-team/geogami-dashboard") {
  github_get <- function(path) {
    resp <- httr::GET(
      paste0("https://api.github.com/repos/", repo, path),
      httr::user_agent("geogami-dashboard"),
      httr::add_headers(Accept = "application/vnd.github+json")
    )
    httr::stop_for_status(resp)
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  }

  tryCatch({
    # Latest release tag, e.g. "v2.0.0" -> "2.0.0"
    version <- sub("^v", "", github_get("/releases/latest")$tag_name)

    # Timestamp of the last commit on main (committer date, ISO-8601 UTC)
    commit_date <- github_get("/commits/main")$commit$committer$date
    commit_time <- as.POSIXct(commit_date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    paste0("Version ", version, " - ",
           format(commit_time, "%d.%m.%y %H:%M:%S", tz = "Europe/Berlin"))
  }, error = function(e) {
    message("Could not fetch version info from GitHub: ", conditionMessage(e))
    "Version unavailable"
  })
}

version_footer <- build_version_footer()

# Render a stored free-DRAW FeatureCollection onto a leaflet map, dispatching by
# geometry type. `feats` is events$answer$drawing$features[[okIdx]] as parsed by
# jsonlite (a data.frame with $type and a nested $geometry frame). Point coords
# parse as c(lng,lat); LineString as an N x 2 matrix; Polygon as a 1 x N x 2
# array (only the exterior ring is drawn).
add_drawing_features <- function(map, feats) {
  if (is.null(feats) || is.null(feats$geometry) || !length(feats$geometry$type)) {
    return(map)
  }
  for (i in seq_along(feats$geometry$type)) {
    gtype  <- feats$geometry$type[i]
    coords <- feats$geometry$coordinates[[i]]
    if (is.null(coords)) next

    if (gtype == "Point") {
      map <- map %>% addCircleMarkers(
        lng = coords[1], lat = coords[2],
        radius = 5, color = "red", fillColor = "red",
        fillOpacity = 1, opacity = 1, weight = 2
      )
    } else if (gtype == "LineString") {
      cm <- matrix(coords, ncol = 2)
      map <- map %>% addPolylines(
        lng = cm[, 1], lat = cm[, 2],
        color = "red", weight = 2, opacity = 1, stroke = TRUE
      )
    } else if (gtype == "Polygon") {
      # exterior ring; coords is a [ring x point x lng/lat] array
      lng <- coords[1, , 1]
      lat <- coords[1, , 2]
      map <- map %>% addPolygons(
        lng = lng, lat = lat,
        color = "red", fillColor = "red", fillOpacity = 0.2,
        weight = 2, opacity = 1
      )
    }
  }
  map
}

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
    /* RW / VE badges in the games picker + legend. Styled via a class (not
       inline style) because bootstrap-select's HTML sanitizer keeps class=
       but strips style=. */
    .geo-badge {
      display: inline-block;
      width: 26px;
      height: 16px;
      line-height: 16px;
      text-align: center;
      padding: 0;
      box-sizing: border-box;
      border-radius: 3px;
      font-size: 9px;
      font-weight: 600;
      color: #fff !important;
      margin-right: 6px;
      vertical-align: middle;
    }
    .geo-badge-ve { background: #6f42c1 !important; }
    .geo-badge-rw { background: #198754 !important; }
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
}

/* Keep long information assignment text to max 2 visible lines */
.assignment-two-lines {
  display: -webkit-box;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: normal !important;
  line-height: 1.25em;
  max-height: 2.5em;
  cursor: help;
}

/* Better vertical alignment for multi-line table cells */
#iris_data table.dataTable tbody td {
  vertical-align: top;
}


/* Stable layout for All tasks DT table */
#iris_data table.dataTable {
  table-layout: fixed !important;
  width: 100% !important;
}

/* Row-number column added by DT */
#iris_data table.dataTable thead th:nth-child(1),
#iris_data table.dataTable tbody td:nth-child(1) {
  width: 45px !important;
  min-width: 45px !important;
  max-width: 45px !important;
  white-space: nowrap !important;
}

/* Type column */
#iris_data table.dataTable thead th:nth-child(2),
#iris_data table.dataTable tbody td:nth-child(2) {
  width: 150px !important;
  min-width: 150px !important;
  max-width: 150px !important;
  white-space: normal !important;
}

/* Assignment column - fixed */
#iris_data table.dataTable thead th:nth-child(3),
#iris_data table.dataTable tbody td:nth-child(3) {
  width: 380px !important;
  min-width: 380px !important;
  max-width: 380px !important;
  white-space: normal !important;
}

/* Numeric / direction columns */
#iris_data table.dataTable thead th:nth-child(n+5),
#iris_data table.dataTable tbody td:nth-child(n+5) {
  text-align: center !important;
  white-space: normal !important;
}

/* Compact header */
#iris_data table.dataTable thead th {
  font-size: 12px !important;
  padding: 6px 5px !important;
  vertical-align: middle !important;
}

/* Compact body */
#iris_data table.dataTable tbody td {
  font-size: 13px !important;
  padding: 6px 5px !important;
  vertical-align: top !important;
}



  "),
               
               
               ),
    
    
    tags$script(HTML('
(function registerGeoGamiTrajectoryAnimation() {
  if (typeof Shiny === "undefined" || typeof HTMLWidgets === "undefined" || typeof L === "undefined") {
    setTimeout(registerGeoGamiTrajectoryAnimation, 100);
    return;
  }

  window.geogamiTrajectoryAnimations = window.geogamiTrajectoryAnimations || {};

  function esc(value) {
    return String(value == null ? "" : value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  Shiny.addCustomMessageHandler("animateTrajectories", function(message) {
    var mapId = message.mapId || "map";
    var widget = HTMLWidgets.find("#" + mapId);

    if (!widget || typeof widget.getMap !== "function") {
      console.warn("Leaflet map not found for animation:", mapId);
      return;
    }

    var map = widget.getMap();

    var old = window.geogamiTrajectoryAnimations[mapId];
    if (old) {
      if (old.timer) clearInterval(old.timer);
      if (old.group) map.removeLayer(old.group);
      if (old.legend) map.removeControl(old.legend);
    }

    var group = L.layerGroup().addTo(map);
    var trajectories = message.trajectories || [];
    var duration = Number(message.duration || 6000);
    var steps = Number(message.steps || 160);
    var lineWeight = Number(message.lineWeight || 4);

    var animated = [];
    var allPoints = [];

    trajectories.forEach(function(traj) {
      var pts = [];
      var lat = traj.lat || [];
      var lng = traj.lng || [];
      var n = Math.min(lat.length, lng.length);

      for (var i = 0; i < n; i++) {
        var la = Number(lat[i]);
        var ln = Number(lng[i]);
        if (isFinite(la) && isFinite(ln)) {
          pts.push([la, ln]);
          allPoints.push([la, ln]);
        }
      }

      if (pts.length < 2) return;

      var color = traj.color || "#d62728";

      L.circleMarker(pts[0], {
        radius: 3,
        color: color,
        fillColor: color,
        fillOpacity: 1,
        weight: 1
      }).addTo(group);

      var movingMarker = L.circleMarker(pts[0], {
        radius: 4,
        color: color,
        fillColor: color,
        fillOpacity: 1,
        weight: 1
      }).addTo(group);

      var line = L.polyline([pts[0]], {
        color: color,
        weight: lineWeight,
        opacity: 1
      }).addTo(group);

      animated.push({
        pts: pts,
        line: line,
        movingMarker: movingMarker,
        color: color,
        label: traj.label || ""
      });
    });

        if (animated.length === 0) return;

    // Animation legend: visible only on the map during animation.
    // This is not connected to any CSV download.
    var legend = L.control({ position: "topright" });

    legend.onAdd = function() {
      var div = L.DomUtil.create("div", "geogami-trajectory-animation-legend");

      var html = `
        <div style="
          background: white;
          padding: 8px 10px;
          border-radius: 6px;
          box-shadow: 0 1px 5px rgba(0,0,0,0.35);
          font-size: 13px;
          line-height: 1.3;
          max-width: 260px;
        ">
          <div style="font-weight: bold; margin-bottom: 5px;">
            Animated trajectories
          </div>
      `;

      animated.forEach(function(a) {
        html += `
          <div style="display: flex; align-items: center; gap: 6px; margin: 2px 0;">
            <span style="
              display: inline-block;
              width: 14px;
              height: 14px;
              background: ${a.color};
              border: 1px solid #333;
            "></span>
            <span>${esc(a.label)}</span>
          </div>
        `;
      });

      html += `</div>`;
      div.innerHTML = html;

      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);

      return div;
    };

    legend.addTo(map);

    if (allPoints.length > 1) {
      map.fitBounds(L.latLngBounds(allPoints), { padding: [30, 30] });
    }

    var currentStep = 1;
    var intervalMs = Math.max(15, duration / steps);

    var timer = setInterval(function() {
      currentStep++;
      var fraction = Math.min(1, currentStep / steps);

      animated.forEach(function(a) {
        var count = Math.max(1, Math.ceil(fraction * a.pts.length));
        var shown = a.pts.slice(0, count);
        a.line.setLatLngs(shown);
        a.movingMarker.setLatLng(shown[shown.length - 1]);
      });

      if (fraction >= 1) {
        clearInterval(timer);
      }
    }, intervalMs);

        window.geogamiTrajectoryAnimations[mapId] = {
      group: group,
      legend: legend,
      timer: timer
    };
  });
})();
'))
  ),
  
  theme = bs_theme(),  # initially empty theme
  
  # Sidebar with collapsible toggle
  
  sidebar = sidebar(
    width = "300px",
    # Upload JSON file section
    div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 5px; border-radius: 5px;",
        fileInput("uploaded_json_file", "Upload JSON file:", accept = ".json", multiple = FALSE),
    ),
    
    #filter 0 - event selection (above game selection)
    conditionalPanel(
      condition = "typeof window.location.search.match(/token=([^&]+)/) !== 'undefined' && window.location.search.match(/token=([^&]+)/) !== null",
      div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 15px; border-radius: 8px;",
          pickerInput(
            inputId = "selected_event",
            label = "Select event:",
            choices = NULL,  # populated from /event/userevents
            multiple = FALSE,
            options = list(
              `live-search` = FALSE,
              `none-selected-text` = "No events",
              `width` = '100%',
              container = FALSE,
              size = 10
            )
          ),
          # Active event-filter banner + "show all" reset. Only visible while an
          # event is selected; reframes the narrowed game/player lists as an
          # intentional filter rather than missing data.
          uiOutput("event_filter_banner")
      )
    ),

    # Share event button — appears once an event is selected (owner-only on the server)
    conditionalPanel(
      condition = "typeof window.location.search.match(/token=([^&]+)/) !== 'undefined' && window.location.search.match(/token=([^&]+)/) !== null",
      uiOutput("share_event_button_ui")
    ),

    #filter 1 - game selection
    conditionalPanel(
      condition = "typeof window.location.search.match(/token=([^&]+)/) !== 'undefined' && window.location.search.match(/token=([^&]+)/) !== null",
      div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 15px; border-radius: 8px;",
          pickerInput(
            inputId = "selected_games",
            label = "Select a game:",
            choices = NULL,  # Leave it empty initially
            multiple = FALSE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = FALSE,
              `none-selected-text` = "Select a player",
              `width` = '100%',
              container = FALSE,
              size = 10,
              sanitize = FALSE  # allow the colored RW/VE badge HTML in options
            )
          ),
          # Legend: what the RW / VE badges mean. A wrapping flex row so each
          # badge+label pair stays together but drops to the next line on
          # narrow (responsive) widths.
          div(
            style = "margin-top: 6px; font-size: 11px; color: #555; display: flex; flex-wrap: wrap; gap: 2px 10px;",
            tags$span(
              style = "white-space: nowrap;",
              tags$span(class = "geo-badge geo-badge-rw", "RW"), "Real world"
            ),
            tags$span(
              style = "white-space: nowrap;",
              tags$span(class = "geo-badge geo-badge-ve", "VE"), "Virtual env."
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
          downloadButton("download_json", "Download", icon = icon("download"), style = "width:150px; margin-top: 10px; margin-bottom: 15px;"),
          # Per-track share button — appears when exactly one track is selected.
          uiOutput("share_track_button_ui")
      )
    ),

    #filter 2 - ID - 2nd div
    # div(style = "border: 1px solid #ccc; padding: 10px; margin-bottom: 15px; border-radius: 8px;",
    #     numericInput("num_value", "Enter a task number:", value = 1, min = 1, max = 1)
    # ),

    radioButtons("theme", "Choose Theme:",
                 choices = c("Light", "Dark"),
                 inline = TRUE,
                 selected = "Light"),

    div(
      style = "text-align: left; color: #888; font-size: 12px;",

      # Latest GitHub release version + last commit time on main (see build_version_footer)
      HTML(version_footer)
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
      
      div(
        style = "display: flex; justify-content: space-between; align-items: flex-end; gap: 20px; margin-bottom: 10px;",
        
        uiOutput("player_info_box"),
        
        div(
          style = "min-width: 280px; text-align: right;",
          switchInput(
            inputId = "advanced_direction_analysis_all",
            label = "Advanced Direction Task Analysis",
            value = FALSE,
            onLabel = "On",
            offLabel = "Off",
            size = "small"
          )
        )
      ),
      
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
        div(style = "flex: 1; min-width: 900px;", uiOutput("file_selector_ui3"))
      ),
      textOutput("mapLegend"),
      div(id = "map_container", leafletOutput("map"), style = "margin-top: 5px"),
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
        condition = "
          output.tabLegend == 'Task type: Navigation to flag' ||
          output.tabLegend == 'Task type: Navigation with arrow' ||
          output.tabLegend == 'Task type: Navigation via text' ||
          output.tabLegend == 'Task type: Navigation via photo' ||
          output.tabLegend == 'Task type: Self location' ||
          output.tabLegend == 'Task type: Object location' ||
          output.tabLegend == 'Task type: Free'
        ",
        card(
          h4(textOutput("cmp_table1_title", inline = TRUE)),
          tableOutput('cmp_table1'),
          downloadButton('save_table1', 'Save to csv'),
          style = "margin-top: 10px"
        )
      ),
      conditionalPanel(
        condition = "output.tabLegend == 'Task type: Direction determination'",
        card(
          div(
            style = "display: flex; justify-content: space-between; align-items: center; gap: 20px; margin-bottom: 10px;",
            
            h4("Direction Task Comparison", style = "margin: 0;"),
            
            div(
              style = "min-width: 280px; text-align: right;",
              switchInput(
                inputId = "advanced_direction_analysis_compare",
                label = "Advanced Direction Task Analysis",
                value = FALSE,
                onLabel = "On",
                offLabel = "Off",
                size = "small"
              )
            )
          ),
          
          tableOutput('cmp_table2'),
          downloadButton('save_table2', 'Save to csv'),
          style = "margin-top: 10px"
        )
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
                         card(card_header("Pie chart"), full_screen = TRUE, plotOutput('pie_chart2'),
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
  
  # ---- Direction column helpers for All tasks table STARTS  ----
  # These helpers ONLY expose values in the table/export.
  # They do NOT change correctness, score, map rendering, or existing error logic.
  
  safe_scalar <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    
    if (is.list(x) || is.data.frame(x)) {
      x <- unlist(x, use.names = FALSE)
    }
    
    if (length(x) == 0) return(NA)
    x[1]
  }
  
  normalize_deg <- function(x) {
    x <- num(x)
    ifelse(is.finite(x), ((x %% 360) + 360) %% 360, NA_real_)
  }
  
  format_numeric_clean <- function(x, digits = 2) {
    x <- num(x)
    out <- rep(NA_character_, length(x))
    ok <- is.finite(x)
    out[ok] <- as.character(round(x[ok], digits))
    out
  }
  
  format_bearing_deg <- function(x, digits = 2) {
    x <- normalize_deg(x)
    format_numeric_clean(x, digits)
  }
  
  format_angle_deg <- function(x, digits = 2) {
    format_numeric_clean(x, digits)
  }
  
  get_initial_viewing_bearing <- function(j, evts) {
    b <- try(safe_scalar(evts$task$question$initialAvatarPosition$bearing[j]), silent = TRUE)
    b <- if (inherits(b, "try-error")) NA_real_ else num(b)
    
    if (is.finite(b)) return(b)
    NA_real_
  }
  
  get_final_viewing_bearing <- function(j, evts) {
    # Best case: final submitted heading saved inside answer
    a <- try(safe_scalar(evts$answer$compassHeading[j]), silent = TRUE)
    a <- if (inherits(a, "try-error")) NA_real_ else num(a)
    if (is.finite(a)) return(a)
    
    # Fallback: heading saved directly on the event row
    c <- try(safe_scalar(evts$compassHeading[j]), silent = TRUE)
    c <- if (inherits(c, "try-error")) NA_real_ else num(c)
    if (is.finite(c)) return(c)
    
    NA_real_
  }
  
  get_rotation_angle <- function(j, evts) {
    initial_b <- get_initial_viewing_bearing(j, evts)
    final_b   <- get_final_viewing_bearing(j, evts)
    
    if (!is.finite(initial_b) || !is.finite(final_b)) {
      return(NA_real_)
    }
    
    angle_diff_deg(final_b, initial_b)
  }
  
  ## HELPER FUNCTION FOR ROTATION ANGLE COLUMN - ENDS ######
  
  
  get_direction_components <- function(j, evts) {
    task_type <- try(safe_scalar(evts$task$type[j]), silent = TRUE)
    task_type <- if (inherits(task_type, "try-error")) NA_character_ else as.character(task_type)
    
    eval_type <- try(safe_scalar(evts$task$evaluate[j]), silent = TRUE)
    eval_type <- if (inherits(eval_type, "try-error")) NA_character_ else as.character(eval_type)
    
    is_dir <- (!is.na(task_type) && task_type == "theme-direction") ||
      (!is.na(eval_type) && eval_type %in% c("evalMapDirection", "evalDirection"))
    
    if (!is_dir) {
      return(list(
        viewing_direction = NA_real_,
        final_viewing_direction = NA_real_,
        pointing_direction = NA_real_,
        direction_error = NA_real_
      ))
    }
    
    initial_avatar_bearing <- num(tryCatch(
      safe_scalar(evts$task$question$initialAvatarPosition$bearing[j]),
      error = function(e) NA
    ))
    
    target_direction_bearing <- num(tryCatch(
      safe_scalar(evts$task$question$direction$bearing[j]),
      error = function(e) NA
    ))
    
    click_direction <- num(tryCatch(
      safe_scalar(evts$answer$clickDirection[j]),
      error = function(e) NA
    ))
    
    answer_compass_heading <- num(tryCatch(
      safe_scalar(evts$answer$compassHeading[j]),
      error = function(e) NA
    ))
    
    event_compass_heading <- num(tryCatch(
      safe_scalar(evts$compassHeading[j]),
      error = function(e) NA
    ))
    
    # Initial direction at the start of the task
    viewing_direction <- initial_avatar_bearing
    
    # Final direction after rotating / submitting
    final_viewing_direction <- answer_compass_heading
    if (!is.finite(final_viewing_direction)) {
      final_viewing_direction <- event_compass_heading
    }
    
    pointing_direction <- NA_real_
    direction_error <- NA_real_
    
    if (!is.na(eval_type) && eval_type == "evalMapDirection") {
      # Player answers by clicking/marking a direction on the map.
      # Viewing direction = initial avatar/view direction.
      # Pointing direction = clicked map direction.
      pointing_direction <- click_direction
      
      correct_reference <- initial_avatar_bearing
      if (!is.finite(correct_reference)) {
        correct_reference <- target_direction_bearing
      }
      
      direction_error <- angle_diff_deg(pointing_direction, correct_reference)
      
    } else if (!is.na(eval_type) && eval_type == "evalDirection") {
      # Player answers by rotating/looking.
      # Viewing direction = initial direction.
      # Final viewing direction = submitted compass/head direction.
      # Pointing direction = correct/target direction.
      pointing_direction <- target_direction_bearing
      if (!is.finite(pointing_direction)) {
        pointing_direction <- get_correct_bearing(j, evts)
      }
      
      direction_error <- angle_diff_deg(final_viewing_direction, pointing_direction)
      
    } else {
      # Safe fallback for older/irregular direction tasks.
      if (is.finite(click_direction)) {
        pointing_direction <- click_direction
        
        correct_reference <- initial_avatar_bearing
        if (!is.finite(correct_reference)) {
          correct_reference <- target_direction_bearing
        }
        
        direction_error <- angle_diff_deg(pointing_direction, correct_reference)
        
      } else {
        pointing_direction <- target_direction_bearing
        if (!is.finite(pointing_direction)) {
          pointing_direction <- get_correct_bearing(j, evts)
        }
        
        direction_error <- angle_diff_deg(final_viewing_direction, pointing_direction)
      }
    }
    
    list(
      viewing_direction = viewing_direction,
      final_viewing_direction = final_viewing_direction,
      pointing_direction = pointing_direction,
      direction_error = direction_error
    )
  }
  
  
  ###### STARTING - helper function for another column of all tasks main tab panel i.e Final Answer for direction determination####
  
  get_final_answer_direction <- function(j, evts) {
    task_type <- try(safe_scalar(evts$task$type[j]), silent = TRUE)
    task_type <- if (inherits(task_type, "try-error")) NA_character_ else as.character(task_type)
    
    eval_type <- try(safe_scalar(evts$task$evaluate[j]), silent = TRUE)
    eval_type <- if (inherits(eval_type, "try-error")) NA_character_ else as.character(eval_type)
    
    is_dir <- (!is.na(task_type) && task_type == "theme-direction") ||
      (!is.na(eval_type) && eval_type %in% c("evalMapDirection", "evalDirection"))
    
    if (!is_dir) {
      return(NA_real_)
    }
    
    dir_parts <- get_direction_components(j, evts)
    
    # evalDirection:
    # player rotates/faces a direction.
    # submitted answer = answer$compassHeading = Final viewing direction
    if (!is.na(eval_type) && eval_type == "evalDirection") {
      return(dir_parts$final_viewing_direction)
    }
    
    # evalMapDirection:
    # player marks/clicks a direction on the map.
    # submitted answer = answer$clickDirection = Pointing direction
    if (!is.na(eval_type) && eval_type == "evalMapDirection") {
      return(dir_parts$pointing_direction)
    }
    
    # Safe fallback for older/irregular direction tasks:
    # if clickDirection exists, treat pointing direction as final answer;
    # otherwise use final viewing direction.
    click_direction <- num(tryCatch(
      safe_scalar(evts$answer$clickDirection[j]),
      error = function(e) NA
    ))
    
    if (is.finite(click_direction)) {
      return(dir_parts$pointing_direction)
    }
    
    dir_parts$final_viewing_direction
  }
  
  ###### ENDING - helper function for another column of all tasks main tab panel i.e Final Answer for direction determination####
  
  
  
  
  
  
  # ---- Direction column helpers for All tasks table ENDS ----
  
  
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
  
  
  
  ######renaming compare players tab columns with units - starts - helper function#######
  rename_unit_headers <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    
    names(df)[names(df) == "Time"] <- "Time (s)"
    names(df)[names(df) == "Distance travelled"] <- "Distance travelled (m)"
    names(df)[names(df) == "Error to target"] <- "Error to target (m)"
    names(df)[names(df) == "Error"] <- "Error (°)"
    
    names(df)[names(df) == "Viewing direction"] <- "Viewing dir. (°)"
    names(df)[names(df) == "Final viewing direction"] <- "Final view dir. (°)"
    names(df)[names(df) == "Pointing direction"] <- "Pointing dir. (°)"
    names(df)[names(df) == "Rotation angle"] <- "Rotation (°)"
    names(df)[names(df) == "Final Answer"] <- "Final answer (°)"
    
    df
  }
  ######renaming compare players tab columns with units - ENDS - helper function#######
  
  
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
      dir_parts <- get_direction_components(idx, evts)
      err_deg <- dir_parts$direction_error
      
      if (is.finite(err_deg)) return(as.character(round(err_deg, 2)))
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
      return(as.character(round(shown_m, 2)))
    }
    
    as.character(round(err_m, 2))
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
  
  to_numeric_vector_safe <- function(x) {
    if (is.null(x) || length(x) == 0) return(numeric(0))
    x <- unlist(x, use.names = FALSE)
    if (!length(x)) return(numeric(0))
    if (is.character(x)) x <- gsub(",", ".", x, fixed = TRUE)
    suppressWarnings(as.numeric(x))
  }
  
  has_task_waypoints <- function(track, task_no) {
    if (is.null(track) || is.null(track$waypoints)) return(FALSE)
    
    wps <- track$waypoints
    
    if (
      is.null(wps$taskNo) ||
      is.null(wps$position$coords$longitude) ||
      is.null(wps$position$coords$latitude)
    ) {
      return(FALSE)
    }
    
    task_no_all <- suppressWarnings(as.integer(unlist(wps$taskNo, use.names = FALSE)))
    keep <- which(task_no_all == as.integer(task_no))
    
    if (!length(keep)) return(FALSE)
    
    lng <- to_numeric_vector_safe(wps$position$coords$longitude[keep])
    lat <- to_numeric_vector_safe(wps$position$coords$latitude[keep])
    
    n <- min(length(lng), length(lat))
    if (n < 2) return(FALSE)
    
    sum(is.finite(lng[seq_len(n)]) & is.finite(lat[seq_len(n)])) >= 2
  }
  
  has_free_drawing_points <- function(evts, block) {
    if (
      is.null(evts$clickPosition$longitude) ||
      is.null(evts$clickPosition$latitude)
    ) {
      return(FALSE)
    }
    
    lng <- to_numeric_vector_safe(evts$clickPosition$longitude[block])
    lat <- to_numeric_vector_safe(evts$clickPosition$latitude[block])
    
    n <- min(length(lng), length(lat))
    if (n < 2) return(FALSE)
    
    sum(is.finite(lng[seq_len(n)]) & is.finite(lat[seq_len(n)])) >= 2
  }
  
  has_object_map_geometry <- function(evts, block) {
    any(vapply(block, function(i) {
      task_type_i <- try(evts$task$type[i], silent = TRUE)
      task_type_i <- if (inherits(task_type_i, "try-error") || is.null(task_type_i) || length(task_type_i) == 0) {
        NA_character_
      } else {
        as.character(task_type_i[1])
      }
      
      if (is.na(task_type_i) || task_type_i != "theme-object") {
        return(FALSE)
      }
      
      ring <- get_polygon_ring(evts, i)
      !is.null(ring) && nrow(ring) >= 3
    }, logical(1)))
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
      
      # ----- Map tab: grey only if there is no useful map content -----
      is_info_task <- (!is.na(task_cat) && task_cat == "info") ||
        (!is.na(task_type) && task_type == "info")
      
      is_free_task <- !is.na(task_type) && task_type == "free"
      is_object_task <- !is.na(task_type) && task_type == "theme-object"
      
      has_free_map_content <- FALSE
      has_object_map_content <- FALSE
      
      if (is_free_task) {
        has_free_map_content <- has_free_drawing_points(evts, block) ||
          has_task_waypoints(track, k)
      }
      
      if (is_object_task) {
        has_object_map_content <- has_object_map_geometry(evts, block)
      }
      
      map_grey[k] <- is_info_task ||
        (is_free_task && !has_free_map_content) ||
        (is_object_task && !has_object_map_content)
      
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
      
      # tries / attempts
      # Some real-world navigation tasks do not create ON_OK_CLICKED.
      # They finish automatically with WAYPOINT_REACHED.
      ok_count <- sum(evts$type[block] == "ON_OK_CLICKED", na.rm = TRUE)
      waypoint_count <- sum(evts$type[block] == "WAYPOINT_REACHED", na.rm = TRUE)
      map_click_count <- sum(evts$type[block] == "ON_MAP_CLICKED", na.rm = TRUE)
      choice_count <- sum(evts$type[block] == "MULTIPLE_CHOICE_SELECTED", na.rm = TRUE)
      
      is_info_task <- !is.na(task_cat) && task_cat == "info"
      is_nav_task  <- !is.na(task_cat) && task_cat == "nav"
      is_theme_task <- !is.na(task_cat) && task_cat == "theme"
      
      tries <- ok_count
      
      # Information tasks should stay 0, even if they have an OK click.
      if (is_info_task) {
        tries <- 0L
        
        # Real-world nav-photo / nav-arrow / nav-text often complete by WAYPOINT_REACHED.
      } else if (tries == 0L && is_nav_task && waypoint_count > 0L) {
        tries <- waypoint_count
        
        # Extra fallback: if it is a navigation task with time spent but no OK event,
        # count it as one played attempt.
      } else if (tries == 0L && is_nav_task && !is.na(time_s) && time_s > 0L) {
        tries <- 1L
        
        # Extra safety for theme/free tasks if a player interacted but no OK was stored.
      } else if (tries == 0L && is_theme_task && (map_click_count > 0L || choice_count > 0L)) {
        tries <- 1L
      }
      
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
      
      # new explicit direction columns for All tasks table/export
      viewing_direction <- NA_real_
      final_viewing_direction <- NA_real_
      pointing_direction <- NA_real_
      rotation_angle <- NA_real_
      final_answer_direction <- NA_real_
      
      
      
      is_dir <- (!is.na(task_type) && task_type == "theme-direction") ||
        (!is.null(evts$task$evaluate) && !is.na(evts$task$evaluate[final_idx]) &&
           evts$task$evaluate[final_idx] %in% c("evalMapDirection","evalDirection"))
      
      if (is_dir) {
        # Keep these for existing Compare Players "Answer" column / map logic
        ans_b <- get_answer_bearing(final_idx, evts)
        cor_b <- get_correct_bearing(final_idx, evts)
        
        # Existing table/export direction values
        dir_parts <- get_direction_components(final_idx, evts)
        viewing_direction <- dir_parts$viewing_direction
        pointing_direction <- dir_parts$pointing_direction
        
        # Final head/device direction after rotating
        final_viewing_direction <- dir_parts$final_viewing_direction
        
        # Rotation from initial viewing direction to final viewing direction
        rotation_angle <- get_rotation_angle(final_idx, evts)
        
        # Final submitted answer direction:
        # evalDirection    -> final viewing direction
        # evalMapDirection -> pointing direction
        # If the task was not submitted, keep this blank.
        if (has_ok_click) {
          final_answer_direction <- get_final_answer_direction(final_idx, evts)
        }
        
        # Make Compare Players error consistent with All tasks error
        err_deg <- dir_parts$direction_error
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
        viewing_direction = viewing_direction,
        final_viewing_direction = final_viewing_direction,
        pointing_direction = pointing_direction,
        rotation_angle = rotation_angle,
        final_answer_direction = final_answer_direction,
        dist_to_target_m = dist_to_target_m,
        dist_travel_m = dist_travel_m,
        error_txt = error_txt,
        stringsAsFactors = FALSE
      )
    }
    
    res <- do.call(rbind, out)
    
    # Create a unique key per repeated task_id occurrence.
    # This prevents task 1 from accidentally matching task 3, 5, 7, etc.
    id_key <- as.character(res$task_id)
    bad_id <- is.na(id_key) | id_key == "" | id_key == "NA"
    id_key[bad_id] <- paste0("taskNo_", res$taskNo[bad_id])
    
    res$task_occurrence <- ave(seq_len(nrow(res)), id_key, FUN = seq_along)
    res$task_key <- paste0(id_key, "__occ_", res$task_occurrence)
    
    res
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
  
  
  ###########STARTS - HELPER FUNCTION FOR MAKING INFORMATION TEXT SHORTER THAT IS UPTO 2 LINES ONLY ON THE ALL TASKS MAIN TAB###########
  INFO_ASSIGNMENT_MAX_CHARS <- 150
  
  two_line_visible_text <- function(x, max_chars = INFO_ASSIGNMENT_MAX_CHARS) {
    x <- clean_export_text(x)
    
    too_long <- !is.na(x) & nchar(x, type = "chars") > max_chars
    x[too_long] <- paste0(substr(x[too_long], 1, max_chars - 3), "...")
    
    x
  }
  
  assignment_two_line_html <- function(x, max_chars = INFO_ASSIGNMENT_MAX_CHARS) {
    full_text <- clean_export_text(x)
    visible_text <- two_line_visible_text(full_text, max_chars)
    
    mapply(function(full, visible) {
      if (is.na(full)) return(NA_character_)
      
      paste0(
        '<span class="assignment-two-lines" title="',
        htmltools::htmlEscape(full),
        '">',
        htmltools::htmlEscape(visible),
        '</span>'
      )
    }, full_text, visible_text, USE.NAMES = FALSE)
  }
  
  html_visible_text <- function(x) {
    x <- as.character(x)
    x <- gsub("<[^>]+>", "", x)
    x <- gsub("&nbsp;", " ", x, fixed = TRUE)
    x <- gsub("&amp;", "&", x, fixed = TRUE)
    x <- gsub("&lt;", "<", x, fixed = TRUE)
    x <- gsub("&gt;", ">", x, fixed = TRUE)
    x <- gsub("&quot;", "\"", x, fixed = TRUE)
    x <- gsub("&#39;", "'", x, fixed = TRUE)
    clean_export_text(x)
  }
  
  apply_info_assignment_limit <- function(df, html = FALSE) {
    if (is.null(df) || nrow(df) == 0) return(df)
    if (!("Type" %in% names(df)) || !("Assignment" %in% names(df))) return(df)
    
    info_mask <- tolower(trimws(df$Type)) == "information"
    
    if (html) {
      df$Assignment[info_mask] <- assignment_two_line_html(df$Assignment[info_mask])
    } else {
      df$Assignment[info_mask] <- two_line_visible_text(df$Assignment[info_mask])
    }
    
    df
  }
  
  ###########ENDS - HELPER FUNCTION FOR MAKING INFORMATION TEXT SHORTER THAT IS UPTO 2 LINES ONLY ON THE ALL TASKS MAIN TAB###########
  
  
  ####helper starts - applying assignment column limit######
  apply_assignment_display_limit <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    if (!("Assignment" %in% names(df))) return(df)
    
    # Apply 2-line display limit to ALL assignment texts in dashboard only
    df$Assignment <- assignment_two_line_html(df$Assignment, max_chars = 120)
    
    df
  }
  ####helper ends - applying assignment column limit######
  
  
  
  
  
  
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
      `Time (s)` = sm$time_s,
      Tries = sm$tries,
      `Viewing dir. (°)` = format_bearing_deg(sm$viewing_direction),
      `Final view dir. (°)` = format_bearing_deg(sm$final_viewing_direction),
      `Pointing dir. (°)` = format_bearing_deg(sm$pointing_direction),
      `Rotation (°)` = format_angle_deg(sm$rotation_angle),
      `Final answer (°)` = format_bearing_deg(sm$final_answer_direction),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    df[["Error (°/m)"]] <- sm$error_txt
    
    info_mask <- tolower(trimws(df$Type)) == "information"
    df$Answer[info_mask] <- NA_character_
    df$Tries[info_mask] <- 0
    df[["Viewing dir. (°)"]][info_mask] <- NA_character_
    df[["Final view dir. (°)"]][info_mask] <- NA_character_
    df[["Pointing dir. (°)"]][info_mask] <- NA_character_
    df[["Rotation (°)"]][info_mask] <- NA_character_
    df[["Final answer (°)"]][info_mask] <- NA_character_
    df[["Error (°/m)"]][info_mask] <- NA_character_
    
    # For Save All Players CSV: save only the visible shortened information text
    df <- apply_info_assignment_limit(df, html = FALSE)
    
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
  
  
  ######Starts - Helper function for advanced direction analysis toggle buttons#####
  # ---- Advanced Direction Task Analysis toggle helpers ----
  advanced_direction_cols <- c(
    # old internal names, still used by Compare Players before renaming
    "Viewing direction",
    "Final viewing direction",
    "Pointing direction",
    "Rotation angle",
    "Final Answer",
    
    # new display/export names for All tasks
    "Viewing dir. (°)",
    "Final view dir. (°)",
    "Pointing dir. (°)",
    "Rotation (°)",
    "Final answer (°)"
  )
  
  drop_advanced_direction_cols <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df[, setdiff(names(df), advanced_direction_cols), drop = FALSE]
  }
  
  advanced_direction_enabled <- reactive({
    isTRUE(input$advanced_direction_analysis_all) ||
      isTRUE(input$advanced_direction_analysis_compare)
  })
  ######ENDS - Helper function for advanced direction task toggle buttons#####
  
  
  
  
  
  # Store selected game track data reactively
  selected_game_tracks_rv <- reactiveVal()
  
  num_value_num <- reactive({
    suppressWarnings(as.integer(input$num_value))
  })
  
  
  games_choices_rv <- reactiveVal()   # store mapping of game_id -> game_name (CREATING THIS FOR zip file that gets downloaded, this is used for giving the right name to the zip file that gets downloaded)
  # Maps game id -> display label with a "[RW]"/"[VE]" prefix. Kept separate
  # from games_choices_rv (which must stay clean for filenames/headers) and
  # used to relabel the games picker wherever its choices are (re)built.
  game_labels_rv <- reactiveVal(setNames(character(0), character(0)))

  # Relabel a picker choice vector (values = game ids) so each option shows its
  # "[RW]"/"[VE]" prefix. Falls back to the original label when a game id has no
  # known environment (e.g. a shared game not in the user's own list).
  # This text prefix is also the fallback if the colored badge HTML is stripped.
  relabel_games <- function(choice_vec) {
    if (is.null(choice_vec) || length(choice_vec) == 0) return(choice_vec)
    labels <- game_labels_rv()
    ids <- unname(choice_vec)
    new_names <- unname(labels[ids])
    missing <- is.na(new_names)
    new_names[missing] <- names(choice_vec)[missing]
    setNames(choice_vec, new_names)
  }

  # Maps game id -> HTML option content with a colored RW/VE badge. Used as
  # choicesOpt$content so the badge color shows wherever the picker is built.
  game_content_rv <- reactiveVal(setNames(character(0), character(0)))

  # Build the choicesOpt$content vector aligned to a picker choice vector
  # (values = game ids). Falls back to the (escaped, already [RW]/[VE]-prefixed)
  # option label when a game id has no known badge.
  game_contents <- function(choice_vec) {
    if (is.null(choice_vec) || length(choice_vec) == 0) return(NULL)
    contents <- game_content_rv()
    ids <- unname(choice_vec)
    out <- unname(contents[ids])
    missing <- is.na(out)
    out[missing] <- htmltools::htmlEscape(names(choice_vec)[missing])
    out
  }

  # Store access token reactively
  accessToken_rv <- reactiveVal()
  track_data_rv <- reactiveVal()
  
  choices_rv <- reactiveVal() #FOR ENSURING THAT RIGHT NAME IS REFLECTED IN SELECTIZEINPUT INSTEAD OF MONGO DB IDs

  # ── Events (experimental studies / school excursions) ────────────────
  # Picker choices for the event selector: event name -> event id, always with
  # a leading "Not selected" option (value = EVENT_NONE) when events exist.
  events_choices_rv <- reactiveVal(setNames(character(0), character(0)))
  # Per-event game mapping: a list keyed by event id whose value is a named
  # char vector (game name -> game id). Used to narrow the game picker.
  event_games_rv <- reactiveVal(list())
  # Emails the currently-selected event is shared with (for the share modal).
  event_shared_emails_rv <- reactiveVal(character(0))
  # Sentinel for the "Not selected" option (no event filter active). Empty
  # string is avoided because pickerInput treats it inconsistently.
  EVENT_NONE <- "__none__"
  # TRUE when a real event is selected (filter active).
  event_is_active <- reactive({
    ev <- input$selected_event
    !is.null(ev) && nzchar(ev) && ev != EVENT_NONE
  })


  #### only players selected on left should appear on right *starts) #####
  
  filtered_choices_r <- reactive({
    req(choices_rv())
    req(input$selected_files)
    
    all_choices <- choices_rv()
    sel <- input$selected_files
    
    # keep only players currently selected in the left sidebar
    all_choices[all_choices %in% sel]
  })
  
  
  #### Sync selected single player across All tasks / Map / Pictures START ####
  
  selected_single_player_rv <- reactiveVal(NULL)
  
  current_single_player <- reactive({
    choices_now <- filtered_choices_r()
    cur <- selected_single_player_rv()
    
    if (!is.null(cur) && length(cur) > 0 && cur[1] %in% choices_now) {
      cur[1]
    } else {
      choices_now[1]
    }
  })
  
  sync_single_player_pickers <- function(selected = NULL) {
    choices_now <- filtered_choices_r()
    
    if (is.null(selected) || length(selected) == 0 || !(selected[1] %in% choices_now)) {
      selected <- choices_now[1]
    } else {
      selected <- selected[1]
    }
    
    updatePickerInput(
      session,
      "selected_data_file_all",
      choices = choices_now,
      selected = selected
    )
    
    updatePickerInput(
      session,
      "selected_data_file_map",
      choices = choices_now,
      selected = selected
    )
    
    updatePickerInput(
      session,
      "selected_data_file_pictures",
      choices = choices_now,
      selected = selected
    )
  }
  
  # When the left sidebar selected players change, keep the selected player valid
  observeEvent(filtered_choices_r(), {
    choices_now <- filtered_choices_r()
    cur <- selected_single_player_rv()
    
    selected_now <- if (!is.null(cur) && length(cur) > 0 && cur[1] %in% choices_now) {
      cur[1]
    } else {
      choices_now[1]
    }
    
    selected_single_player_rv(selected_now)
    sync_single_player_pickers(selected_now)
  }, ignoreInit = FALSE)
  
  # If changed in All tasks tab
  observeEvent(input$selected_data_file_all, {
    req(input$selected_data_file_all)
    
    if (!identical(input$selected_data_file_all[1], selected_single_player_rv())) {
      selected_single_player_rv(input$selected_data_file_all[1])
    }
  }, ignoreInit = TRUE)
  
  # If changed in Map tab
  observeEvent(input$selected_data_file_map, {
    req(input$selected_data_file_map)
    
    if (!identical(input$selected_data_file_map[1], selected_single_player_rv())) {
      selected_single_player_rv(input$selected_data_file_map[1])
    }
  }, ignoreInit = TRUE)
  
  # If changed in Pictures tab
  observeEvent(input$selected_data_file_pictures, {
    req(input$selected_data_file_pictures)
    
    if (!identical(input$selected_data_file_pictures[1], selected_single_player_rv())) {
      selected_single_player_rv(input$selected_data_file_pictures[1])
    }
  }, ignoreInit = TRUE)
  
  # Whenever the central selected player changes, update all three picker UIs
  observeEvent(selected_single_player_rv(), {
    req(selected_single_player_rv())
    sync_single_player_pickers(selected_single_player_rv())
  }, ignoreInit = TRUE)
  
  # When the reference player changes, reset the trajectory selector
  # to the new reference player instead of keeping the old reference player.
  observeEvent(current_single_player(), {
    req(current_single_player())
    req(filtered_choices_r())
    
    choices_now <- filtered_choices_r()
    selected_ref <- current_single_player()
    
    if (!(selected_ref %in% choices_now)) return()
    
    updatePickerInput(
      session,
      "selected_map_files",
      choices = choices_now,
      selected = selected_ref
    )
  }, ignoreInit = TRUE)
  
  
  #### Sync selected single player across All tasks / Map / Pictures END ####
  
  
  #### only players selected on left should appear on right *ends) #####
  
  
  
  # apiURL_rv <- reactiveVal("http://localhost:3000")
  apiURL_rv <- reactiveVal("https://api.geogami.uni-muenster.de")
  
  # ---------- Multi-player trajectories on Map tab START ----------
  
  MAP_TRAJ_GROUP <- "multi_player_trajectories"
  
  # Default red trajectory drawn by the base map for the reference player.
  # We put it in its own group so we can hide it before animation starts.
  REFERENCE_BASE_TRAJ_GROUP <- "reference_base_trajectory"
  
  # Keep overlay trajectories visually consistent with the original red reference trajectory
  MAP_TRAJ_WEIGHT <- 2
  MAP_TRAJ_OPACITY <- 1
  REFERENCE_TRAJ_COLOR <- "red"
  
  MAP_TRAJ_COLORS <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
    "#003f5c", "#ffa600", "#58508d", "#ff6361", "#2f4b7c"
  )
  
  # Small cache so we do not refetch the same track every time the map selector changes.
  map_track_cache <- new.env(parent = emptyenv())
  
  observeEvent(input$selected_games, {
    rm(list = ls(map_track_cache), envir = map_track_cache)
  }, ignoreInit = TRUE)
  
  num_vec <- function(x) {
    if (is.null(x) || length(x) == 0) return(numeric(0))
    x <- unlist(x, use.names = FALSE)
    if (is.character(x)) x <- gsub(",", ".", x, fixed = TRUE)
    suppressWarnings(as.numeric(x))
  }
  
  get_track_for_map <- function(track_id, api_url, token) {
    key <- as.character(track_id)
    
    if (exists(key, envir = map_track_cache, inherits = FALSE)) {
      return(get(key, envir = map_track_cache, inherits = FALSE))
    }
    
    if (is.null(api_url) || is.na(api_url) || !nzchar(api_url) ||
        is.null(token) || is.na(token) || !nzchar(token)) {
      return(NULL)
    }
    
    url <- paste0(api_url, "/track/", key)
    tr <- fetch_games_data_from_server(url, token)
    
    if (!is.null(tr) && !is.null(tr$events)) {
      tr$events <- fix_vr_distance_tasks(tr$events)
      tr$events <- fix_direction_tasks(tr$events)
    }
    
    assign(key, tr, envir = map_track_cache)
    tr
  }
  
  get_map_track_label <- function(track_id, tr = NULL, choices_now = NULL) {
    label <- NA_character_
    
    if (!is.null(choices_now) && length(choices_now) > 0) {
      idx <- which(choices_now == track_id)
      if (length(idx) > 0) {
        label <- names(choices_now)[idx[1]]
      }
    }
    
    if ((is.null(label) || is.na(label) || !nzchar(label)) &&
        !is.null(tr) && !is.null(tr$players) && length(tr$players) > 0) {
      label <- as.character(tr$players[1])
    }
    
    if (is.null(label) || is.na(label) || !nzchar(label)) {
      label <- as.character(track_id)
    }
    
    label
  }
  
  get_task_trajectory_df <- function(track, task_no) {
    if (is.null(track) || is.null(track$waypoints)) {
      return(data.frame(lng = numeric(0), lat = numeric(0)))
    }
    
    wps <- track$waypoints
    
    if (is.null(wps$taskNo) ||
        is.null(wps$position$coords$longitude) ||
        is.null(wps$position$coords$latitude)) {
      return(data.frame(lng = numeric(0), lat = numeric(0)))
    }
    
    task_no_all <- suppressWarnings(as.integer(unlist(wps$taskNo, use.names = FALSE)))
    keep <- which(task_no_all == as.integer(task_no))
    
    if (!length(keep)) {
      return(data.frame(lng = numeric(0), lat = numeric(0)))
    }
    
    lng <- num_vec(wps$position$coords$longitude[keep])
    lat <- num_vec(wps$position$coords$latitude[keep])
    
    ts <- if (!is.null(wps$timestamp)) {
      parse_ts(wps$timestamp[keep])
    } else {
      rep(as.POSIXct(NA), length(keep))
    }
    
    df <- data.frame(
      lng = lng,
      lat = lat,
      ts = ts,
      stringsAsFactors = FALSE
    )
    
    df <- df[is.finite(df$lng) & is.finite(df$lat), , drop = FALSE]
    
    if (!nrow(df)) {
      return(df)
    }
    
    # Optional GPS accuracy filter, same idea as your existing map logic.
    if (!is.null(wps$position$coords$accuracy)) {
      acc <- num_vec(wps$position$coords$accuracy[keep])
      acc <- acc[seq_len(min(length(acc), nrow(df)))]
      
      if (length(acc) == nrow(df)) {
        df <- df[is.na(acc) | acc < 20, , drop = FALSE]
      }
    }
    
    if (nrow(df) > 1 && any(!is.na(df$ts))) {
      df <- df[order(df$ts), , drop = FALSE]
    }
    
    df
  }
  
  # ---------- Multi-player trajectories on Map tab END ----------
  
  
  
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

    # Progress bar shown while the games list loads; auto-closes when the
    # block finishes (i.e. once the picker is populated).
    withProgress(message = "Loading games…", value = 0.2, {
      apiUrl <- paste0(apiURL_rv(), "/game/usergames")
      games_data <- fetch_games_data_from_server(apiUrl, accessToken_rv())
      setProgress(0.7)

      if (is.null(games_data) || NROW(games_data) == 0) {
        games_choices_rv(setNames(character(0), character(0)))
        updatePickerInput(session, "selected_games", choices = character(0), selected = character(0))
        output$info_download <- renderText({ "" })
      } else {
        games_df <- as.data.frame(games_data, stringsAsFactors = FALSE)

        # --- derive created time from Mongo _id and sort newest first ---
        if ("_id" %in% names(games_df)) {
          games_df$created_dt <- mongo_objectid_time(games_df[["_id"]])
          games_df <- games_df[order(games_df$created_dt, decreasing = TRUE), , drop = FALSE]
        }

        # Build picker choices (label=name, value=id) in this sorted order.
        # Keep this mapping CLEAN (name only) — it is reused for the download
        # zip filename and the selected-game header.
        mapping <- setNames(games_df[["_id"]], games_df$name)
        games_choices_rv(mapping)

        # Prefix each picker label with a plain-text [RW] / [VE] marker so the
        # environment is visible in the dropdown. isVRWorld == TRUE -> virtual
        # environment (VE); anything else (FALSE / missing) -> real world (RW).
        is_ve <- if ("isVRWorld" %in% names(games_df)) {
          !is.na(games_df$isVRWorld) & as.logical(games_df$isVRWorld)
        } else {
          rep(FALSE, nrow(games_df))
        }
        picker_labels <- paste0("[", ifelse(is_ve, "VE", "RW"), "] ", games_df$name)
        # id -> labeled name, used to relabel the picker wherever it is rebuilt
        # (e.g. when the event filter restores or narrows the games list).
        game_labels_rv(setNames(picker_labels, games_df[["_id"]]))
        picker_choices <- setNames(games_df[["_id"]], picker_labels)

        # id -> HTML content with the colored RW/VE badge (name is escaped).
        game_badges <- ifelse(
          is_ve,
          "<span class='geo-badge geo-badge-ve'>VE</span>",
          "<span class='geo-badge geo-badge-rw'>RW</span>"
        )
        game_content_rv(setNames(
          paste0(game_badges, htmltools::htmlEscape(games_df$name)),
          games_df[["_id"]]
        ))

        # Preserve current selection if still valid, else default to newest
        cur <- isolate(input$selected_games)
        selected_val <- if (!is.null(cur) && cur %in% mapping) cur else games_df[["_id"]][1]

        updatePickerInput(
          session, "selected_games",
          choices = picker_choices, selected = selected_val,
          choicesOpt = list(content = game_contents(picker_choices))
        )

        # Debug print to confirm ordering
        message("=== Games (newest -> oldest) ===")
        print(games_df[, c("name", "_id", "created_dt")], row.names = FALSE)

        output$info_download <- renderText({ "" })
      }
      setProgress(1)
    })
  })
  
  #############---------------LOADING GAMES END (NEWEST ON TOP)-----################


  #############---------------LOADING EVENTS start-----################
  # Load the user's events (owned + shared) and populate the event picker. The
  # first option is always "Not selected" so the dashboard starts unfiltered.
  observe({
    req(accessToken_rv())
    req(apiURL_rv())

    apiUrl <- paste0(apiURL_rv(), "/event/userevents")
    events_data <- fetch_games_data_from_server(apiUrl, accessToken_rv())

    if (is.null(events_data) || NROW(events_data) == 0) {
      # No events: disable the picker and show a clear empty message.
      events_choices_rv(setNames(character(0), character(0)))
      event_games_rv(list())
      # No events: the picker holds only a single "No events" option, so there
      # is nothing to filter by (effectively a disabled state without needing a
      # client-side JS dependency).
      updatePickerInput(
        session, "selected_event",
        choices = c("No events" = EVENT_NONE),
        selected = EVENT_NONE
      )
      return()
    }

    events_df <- as.data.frame(events_data, stringsAsFactors = FALSE)

    # Build the event-id -> games mapping. `games` is a list-column where each
    # element is that event's populated games (a data.frame with _id + name).
    gmap <- list()
    for (i in seq_len(NROW(events_df))) {
      ev_id <- events_df[["_id"]][i]
      g <- events_data$games[[i]]
      if (!is.null(g) && NROW(g) > 0) {
        gdf <- as.data.frame(g, stringsAsFactors = FALSE)
        gmap[[ev_id]] <- setNames(gdf[["_id"]], gdf[["name"]])
      } else {
        gmap[[ev_id]] <- setNames(character(0), character(0))
      }
    }
    event_games_rv(gmap)

    # Picker choices: "Not selected" first, then each event by name.
    ev_choices <- c(
      setNames(EVENT_NONE, "Not selected"),
      setNames(events_df[["_id"]], events_df[["name"]])
    )
    events_choices_rv(ev_choices)

    cur <- isolate(input$selected_event)
    selected_val <- if (!is.null(cur) && cur %in% ev_choices) cur else EVENT_NONE
    updatePickerInput(
      session, "selected_event",
      choices = ev_choices, selected = selected_val
    )
  })

  # When the event selection changes, narrow the game picker to the event's
  # games (or restore the full list when "Not selected").
  observeEvent(input$selected_event, {
    if (event_is_active()) {
      gmap <- event_games_rv()[[input$selected_event]]
      if (is.null(gmap)) gmap <- setNames(character(0), character(0))
      cur <- isolate(input$selected_games)
      selected_val <- if (!is.null(cur) && cur %in% gmap) cur
                      else if (length(gmap) > 0) unname(gmap[1]) else character(0)
      updatePickerInput(session, "selected_games",
                        choices = relabel_games(gmap), selected = selected_val,
                        choicesOpt = list(content = game_contents(gmap)))
    } else {
      # Restore the full games list (the unfiltered view).
      gmap <- games_choices_rv()
      if (is.null(gmap)) gmap <- setNames(character(0), character(0))
      cur <- isolate(input$selected_games)
      selected_val <- if (!is.null(cur) && cur %in% gmap) cur
                      else if (length(gmap) > 0) unname(gmap[1]) else character(0)
      updatePickerInput(session, "selected_games",
                        choices = relabel_games(gmap), selected = selected_val,
                        choicesOpt = list(content = game_contents(gmap)))
    }
  }, ignoreNULL = FALSE)

  # Active-filter banner: shown only while an event is selected. States the
  # event name + how many players are in the filtered view, and offers a
  # one-click "Show all players" reset. An empty event shows a gentle hint.
  output$event_filter_banner <- renderUI({
    if (!event_is_active()) return(NULL)

    ev_name <- names(events_choices_rv())[match(input$selected_event, events_choices_rv())]
    if (is.na(ev_name)) ev_name <- "event"

    tracks <- selected_game_tracks_rv()
    n <- if (is.null(tracks) || NROW(tracks) == 0) 0 else NROW(tracks)
    game_selected <- !is.null(input$selected_games) && nzchar(input$selected_games)

    # The count is scoped to the SELECTED GAME within the event (not the whole
    # event), so the wording says "for this game" to avoid implying an
    # event-wide total.
    body <- if (!game_selected) {
      paste0("Filtered to event “", ev_name, "”. Select a game to see its plays.")
    } else if (n == 0) {
      paste0("No plays for this game in event “", ev_name,
             "” yet. Share the event's QR PDF to start collecting.")
    } else {
      paste0("Showing ", n, " player", if (n == 1) "" else "s",
             " for this game in event “", ev_name, "”.")
    }

    div(
      style = "background:#e7f5fb; border:1px solid #b6e2f2; border-radius:6px; padding:8px 10px; margin-top:8px; font-size:13px;",
      tags$span(style = "margin-right:6px;", "\U0001F50E"),
      tags$span(body),
      actionLink("clear_event_filter", "Show all players ✕",
                 style = "display:block; margin-top:6px; font-weight:600;")
    )
  })

  # "Show all players" reset → clears the event filter.
  observeEvent(input$clear_event_filter, {
    updatePickerInput(session, "selected_event", selected = EVENT_NONE)
  })

  #############---------------LOADING EVENTS END-----################



  
  # Re-fetch tracks when either the game OR the event filter changes. When an
  # event is active the request is event-scoped (?eventId=), so only plays
  # collected for that study are returned; otherwise the normal access-based
  # view is returned.
  observeEvent(list(input$selected_games, input$selected_event), {
    game_id <- input$selected_games
    req(game_id)

    # update the API URL with the selected game ID
    apiUrl <- paste0(apiURL_rv(), "/track/gametracks/", game_id)
    if (event_is_active()) {
      apiUrl <- paste0(apiUrl, "?eventId=", input$selected_event)
    }

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

    # No tracks for this selection (e.g. an event with no plays yet, or a game
    # with none): clear the player picker. Without this guard, building choices
    # from a missing `_id` column calls setNames(NULL, ...) and errors.
    if (is.null(tracks_data) || NROW(tracks_data) == 0 ||
        is.null(tracks_data[["_id"]])) {
      choices_rv(setNames(character(0), character(0)))
      updatePickerInput(session, "selected_files",
                        choices = character(0), selected = character(0))
      output$info_download <- renderText({ "" })
      return()
    }

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
    selected_now <- current_single_player()
    
    pickerInput(
      "selected_data_file_all",
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
    req(current_single_player())
    req(accessToken_rv())
    req(apiURL_rv())
    
    track_id <- current_single_player()
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
    
    # Reference player
    selected_ref <- current_single_player()
    
    # Multi-player trajectory selector
    cur_map <- isolate(input$selected_map_files)
    
    # First time opening the Map tab: default to the reference player.
    # But if the user clicked "Deselect all", keep it empty.
    if (is.null(cur_map)) {
      selected_map <- selected_ref
    } else {
      selected_map <- intersect(cur_map, choices_now)
    }
    
    div(
      style = "display: flex; gap: 18px; align-items: flex-end; flex-wrap: wrap;",
      
      div(
        style = "width: 300px;",
        pickerInput(
          "selected_data_file_map",
          "Reference player / map details:",
          choices = choices_now,
          selected = selected_ref,
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
      ),
      
      div(
        style = "width: 330px;",
        pickerInput(
          "selected_map_files",
          "Players shown as trajectories:",
          choices = choices_now,
          selected = selected_map,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = FALSE,
            `none-selected-text` = "Select players for trajectories",
            `select-all-text` = "Select all",
            `deselect-all-text` = "Deselect all",
            `width` = '100%',
            container = FALSE,
            size = 10
          )
        )
      ),
      
      div(
        style = "padding-bottom: 15px;",
        actionButton(
          "visualize_trajectories_btn",
          "Visualize Trajectories",
          icon = icon("play"),
          style = "height: 38px; white-space: nowrap; background-color: #0CD1E8; border-color: #0CD1E8; color: #000;"
        )
      )
    )
  })
  
  ##### Filter for photos
  output$file_selector_ui4 <- renderUI({
    req(filtered_choices_r())
    
    choices_now <- filtered_choices_r()
    selected_now <- current_single_player()
    
    pickerInput(
      "selected_data_file_pictures",
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
  
  observeEvent(list(current_single_player(), num_value_num()), {
    req(current_single_player())
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

    # Final stored drawing geometry (GeoJSON FeatureCollection persisted on the
    # ON_OK_CLICKED answer for free DRAW tasks). Preferred over the raw click
    # stream when present; falls back to clicks for older tracks.
    drawing_feats <- NULL
    has_drawing_col <- !is.null(data[[1]]$events$answer) &&
      "drawing" %in% names(data[[1]]$events$answer)

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

        
        
        if (!is.na(type_task[i]) && type_task[i] == "theme-object" && cou == num_value_num()) {
          
          # Always set task type for object-location tasks.
          # This prevents "No task exists with this number" for MULTIPLE_CHOICE object tasks.
          t <- type_task[i]
          
          ans_i <- if (!is.null(ans_type) && length(ans_type) >= i) {
            as.character(ans_type[[i]])
          } else {
            NA_character_
          }
          
          # Object-location tasks can have map geometry even when the answer is MULTIPLE_CHOICE.
          # Example: "Wähle das passende Foto für den markierten Ort."
          ring <- get_polygon_ring(data[[1]]$events, i)
          
          if (!is.null(ring) && nrow(ring) >= 3) {
            lng_poly <- as.list(ring[, 1])
            lat_poly <- as.list(ring[, 2])
            mr <- FALSE
          } else if (!is.na(ans_i) && ans_i %in% c("PHOTO", "MULTIPLE_CHOICE")) {
            # If an object-photo / object-multiple-choice task has no geometry,
            # then there is genuinely nothing useful to draw on the map.
            mr <- TRUE
          }
          
          # Only MAP_POINT object tasks have a clicked answer marker.
          # MULTIPLE_CHOICE object tasks should show the marked polygon only.
          if (!is.na(ans_i) && ans_i == "MAP_POINT") {
            cp <- get_click_lonlat(data[[1]]$events, i)
            
            if (length(cp) == 2 && all(is.finite(cp))) {
              lng_ans_obj <- append(lng_ans_obj, cp[1])
              lat_ans_obj <- append(lat_ans_obj, cp[2])
            }
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
        # Capture the final stored drawing geometry for the selected DRAW task.
        if (has_drawing_col && (type_task[i] == "free") && (cou == num_value_num()) &&
            ans_type[[i]] == "DRAW" && ev[i] == "ON_OK_CLICKED") {
          feats_i <- try(data[[1]]$events$answer$drawing$features[[i]], silent = TRUE)
          if (!inherits(feats_i, "try-error") && is.data.frame(feats_i) && nrow(feats_i) > 0) {
            drawing_feats <- feats_i
          }
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
      `Time (s)` = sm$time_s,
      Tries = sm$tries,
      `Viewing dir. (°)` = format_bearing_deg(sm$viewing_direction),
      `Final view dir. (°)` = format_bearing_deg(sm$final_viewing_direction),
      `Pointing dir. (°)` = format_bearing_deg(sm$pointing_direction),
      `Rotation (°)` = format_angle_deg(sm$rotation_angle),
      `Final answer (°)` = format_bearing_deg(sm$final_answer_direction),
      `Error (°/m)` = sm$error_txt,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    info_mask <- tolower(trimws(df$Type)) == "information"
    df$Answer[info_mask] <- NA_character_
    df$Tries[info_mask] <- 0
    df[["Viewing dir. (°)"]][info_mask] <- NA_character_
    df[["Final view dir. (°)"]][info_mask] <- NA_character_
    df[["Pointing dir. (°)"]][info_mask] <- NA_character_
    df[["Rotation (°)"]][info_mask] <- NA_character_
    df[["Final answer (°)"]][info_mask] <- NA_character_
    df[["Error (°/m)"]][info_mask] <- NA_character_
    # For dashboard display: show max 2-line information text, full text on hover
    df <- apply_assignment_display_limit(df)
    
    
    
    
    
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
      df_show <- filtered_df()
      
      if (!advanced_direction_enabled()) {
        df_show <- drop_advanced_direction_cols(df_show)
      }
      
      DT::datatable(
        df_show,
        escape = setdiff(names(df_show), "Assignment"),
        class = "compact stripe hover",
        options = list(
          pageLength = 10,
          ordering = FALSE,
          autoWidth = FALSE
        )
      )
    })
    
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
        
        if (!advanced_direction_enabled()) {
          df_out <- drop_advanced_direction_cols(df_out)
        }
        
        if (!is.null(df_out) && nrow(df_out) > 0) {
          if ("Assignment" %in% names(df_out)) {
            df_out$Assignment <- html_visible_text(df_out$Assignment)
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
          
          if (!advanced_direction_enabled()) {
            df_one <- drop_advanced_direction_cols(df_one)
          }
          
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
          # Add exactly 1 blank line BETWEEN players
          if (i > 1) {
            cat("\r\n", file = file, append = TRUE)
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
              downloadButton('save_all_data', 'Save All Players')
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

      if (is.finite(lon0) && is.finite(lat0) && is.finite(ans_b)) {
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
        if (is.finite(cor_b)) {
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
          addPolylines(
            lng = unlist(traj_lng),
            lat = unlist(traj_lat),
            color = "red",
            weight = 2,
            opacity = 1,
            stroke = TRUE,
            group = REFERENCE_BASE_TRAJ_GROUP
          )
      }
    }
    if (length(dr_point_lng) != 0 || !is.null(drawing_feats)) {
      map_shown <- {
        m <- leaflet() %>% addTiles()
        if (!is.null(drawing_feats)) {
          m <- add_drawing_features(m, drawing_feats)
        } else {
          m <- m %>% addPolylines(lng = unlist(dr_point_lng), lat = unlist(dr_point_lat), color = "red", weight = 2, opacity = 1, stroke = TRUE)
        }
        m
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
              color = "red",
              weight = 2,
              opacity = 1,
              stroke = TRUE,
              group = REFERENCE_BASE_TRAJ_GROUP
            )
        }
        
        if (length(dr_point_lng) != 0 || !is.null(drawing_feats)) {
          map_shown <- leaflet() %>% addTiles()
          if (!is.null(drawing_feats)) {
            map_shown <- add_drawing_features(map_shown, drawing_feats)
          } else {
            map_shown <- map_shown %>%
              addPolylines(
                lng = unlist(dr_point_lng),
                lat = unlist(dr_point_lat),
                color = "red",
                weight = 2,
                opacity = 1,
                stroke = TRUE
              )
          }
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
          
          if (is.finite(lon0) && is.finite(lat0) && is.finite(ans_b)) {
            
            # Make arrow smaller if you want
            A <- arrow_lines(lon0, lat0, ans_b, len_m = 18, head_m = 5, head_ang = 25)
            
            map_shown <- map_shown %>%
              addMarkers(lng = lon0, lat = lat0, icon = loc_marker) %>%
              
              # Player FINAL answer arrow (BLUE)
              addPolylines(lng = A$main$lng,  lat = A$main$lat,  color = "blue",  weight = 4, opacity = 1) %>%
              addPolylines(lng = A$left$lng,  lat = A$left$lat,  color = "blue",  weight = 4, opacity = 1) %>%
              addPolylines(lng = A$right$lng, lat = A$right$lat, color = "blue",  weight = 4, opacity = 1)
            
            # Correct direction arrow (GREEN) - optional
            if (is.finite(cor_b)) {
              C <- arrow_lines(lon0, lat0, cor_b, len_m = 18, head_m = 5, head_ang = 25)

              map_shown <- map_shown %>%
                addPolylines(lng = C$main$lng,  lat = C$main$lat,  color = "green", weight = 4, opacity = 0.9) %>%
                addPolylines(lng = C$left$lng,  lat = C$left$lat,  color = "green", weight = 4, opacity = 0.9) %>%
                addPolylines(lng = C$right$lng, lat = C$right$lat, color = "green", weight = 4, opacity = 0.9)
            }
            
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
    
    ref_task_key  <- ref_sum$task_key[num_value_num()]
    ref_task_type <- ref_sum$task_type[num_value_num()]
    
    # Tables
    ngts <- data.frame(
      Name = character(0),
      Answer = character(0),
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
      Time = character(0),
      `Viewing direction` = character(0),
      `Final viewing direction` = character(0),
      `Pointing direction` = character(0),
      `Rotation angle` = character(0),
      `Final Answer` = character(0),
      Error = character(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    # Build per-player row by matching task_id (never by taskNo)
    for (i in seq_along(data_list)) {
      tr <- data_list[[i]]
      nm <- if (!is.null(tr$players) && length(tr$players)) tr$players[1] else paste0("Player_", i)
      
      sm <- task_summary(tr)
      hit <- sm[sm$task_key == ref_task_key, , drop = FALSE]
      
      if (!nrow(hit)) {
        ngts <- rbind(ngts, data.frame(
          Name = nm,
          Answer = "Task not played",
          Correct = "Task not played",
          Time = NA_character_,
          `Distance travelled` = NA_character_,
          `Error to target` = NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
        
        if (!is.na(ref_task_type) && ref_task_type == "theme-direction") {
          cores <- rbind(cores, data.frame(
            Name = nm,
            Correct = "Task not played",
            Time = NA_character_,
            `Viewing direction` = NA_character_,
            `Final viewing direction` = NA_character_,
            `Pointing direction` = NA_character_,
            `Rotation angle` = NA_character_,
            `Final Answer` = NA_character_,
            Error = NA_character_,
            check.names = FALSE,
            stringsAsFactors = FALSE
          ))
        }
        
        next
      }
      
      # if the same task_id appears multiple times (back button / replay), use the LAST attempt
      row <- hit[1, , drop = FALSE]
      
      correct_txt <- row$status
      time_txt <- if (is.na(row$time_s)) NA_character_ else as.character(row$time_s)
      dist_txt <- if (is.na(row$dist_travel_m)) NA_character_ else as.character(round(row$dist_travel_m))
      
      # Big-table style:
      # - blank if Correct
      # - else (distance - accuracy) with 15m fallback
      dist_to_target_txt <- row$error_txt
      
      
      # STARTS : For free tasks, use the same Answer text as the All tasks big table
      # Example: "Correct 700 m"
      answer_txt <- row$answer_txt
      
      if (
        is.null(answer_txt) ||
        length(answer_txt) == 0 ||
        is.na(answer_txt) ||
        !nzchar(trimws(as.character(answer_txt)))
      ) {
        answer_txt <- correct_txt
      }
      
      answer_txt <- as.character(answer_txt)
      # ENDS : For free tasks, use the same Answer text as the All tasks big table
      
      # Navigation-style table (also used in Statistics time-vs-distance)
      ngts <- rbind(ngts, data.frame(
        Name = nm,
        Answer = answer_txt,
        Correct = ifelse(is.na(correct_txt), NA_character_, correct_txt),
        Time = time_txt,
        `Distance travelled` = dist_txt,
        `Error to target` = dist_to_target_txt,
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
      
      # Direction table only for theme-direction (Answer/Error in degrees)
      if (!is.na(ref_task_type) && ref_task_type == "theme-direction") {
        view_deg       <- row$viewing_direction
        final_view_deg <- row$final_viewing_direction
        point_deg      <- row$pointing_direction
        rotation_deg   <- row$rotation_angle
        final_answer_deg <- row$final_answer_direction
        
        cores <- rbind(cores, data.frame(
          Name = nm,
          Correct = ifelse(is.na(correct_txt), NA_character_, correct_txt),
          Time = time_txt,
          `Viewing direction` = format_bearing_deg(view_deg),
          `Final viewing direction` = format_bearing_deg(final_view_deg),
          `Pointing direction` = format_bearing_deg(point_deg),
          `Rotation angle` = format_angle_deg(rotation_deg),
          `Final Answer` = format_bearing_deg(final_answer_deg),
          Error = row$error_txt,
          check.names = FALSE,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Render tables
    # Free tasks should only show: Name, Answer, Time
    # Other route/navigation tasks keep the old columns
    is_free_task <- isTRUE(!is.na(ref_task_type) && ref_task_type == "free")
    
    ngts_display <- if (is_free_task) {
      ngts[, c("Name", "Answer", "Time"), drop = FALSE]
    } else {
      ngts[, c("Name", "Correct", "Time", "Distance travelled", "Error to target"), drop = FALSE]
    }
    
    output$cmp_table1 <- renderTable({
      rename_unit_headers(ngts_display)
    })
    
    output$cmp_table2 <- renderTable({
      df_show <- cores
      
      if (!advanced_direction_enabled()) {
        df_show <- drop_advanced_direction_cols(df_show)
      }
      
      rename_unit_headers(df_show)
    })
    
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
    
    output$cmp_table1_title <- renderText({
      if (!is.na(ref_task_type) && ref_task_type == "free") {
        "Free task comparison"
      } else if (!is.na(ref_task_type) && ref_task_type %in% c("theme-loc", "theme-object")) {
        "Location task comparison"
      } else {
        "Route length versus time"
      }
    })
    
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
    time_num <- suppressWarnings(as.numeric(ngts$Time))
    dist_num <- suppressWarnings(as.numeric(ngts$`Distance travelled`))
    
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
    
    # ---------- Filename part for selected comparison task --START ----------
    selected_task_no <- ref_sum$taskNo[num_value_num()]
    
    selected_task_type_label <- t
    if (is.null(selected_task_type_label) || is.na(selected_task_type_label) || !nzchar(selected_task_type_label)) {
      selected_task_type_label <- "Unknown_Task_Type"
    }
    
    selected_task_file_part <- sanitize_filename(
      paste0("Task_", selected_task_no, "_", selected_task_type_label)
    )
    # ---------- Filename part for selected comparison task -- END ----------
    
    # Save CSVs
    output$save_table1 <- downloadHandler(
      filename = function() {
        game_name_safe <- sanitize_filename(get_selected_game_name())
        
        file_suffix <- if (is_free_task) {
          "free_task_comparison"
        } else if (!is.na(ref_task_type) && ref_task_type %in% c("theme-loc", "theme-object")) {
          "location_task_comparison"
        } else {
          "route_length_vs_time"
        }
        
        paste0(
          "Compare_",
          game_name_safe,
          "_",
          selected_task_file_part,
          "_",
          file_suffix,
          "_",
          Sys.Date(),
          ".csv"
        )
      },
      
      content = function(file) {
        game_name <- get_selected_game_name()
        
        if (is_free_task) {
          # Free task CSV should contain only these 3 columns
          df_out <- ngts[, c("Name", "Answer", "Time"), drop = FALSE]
          
        } else {
          # Normal route/navigation CSV keeps the existing format
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
        }
        
        df_out <- rename_unit_headers(df_out)
        write_csv_excel_utf8(df_out, file, na = "NA")
      }
    )
    
    output$save_table2 <- downloadHandler(
      filename = function() {
        game_name_safe <- sanitize_filename(get_selected_game_name())
        paste0("Compare_", game_name_safe, "_", selected_task_file_part, "_direction_error_", Sys.Date(), ".csv")
      },
      content = function(file) {
        game_name <- get_selected_game_name()
        
        df_out <- cores
        
        if (!advanced_direction_enabled()) {
          df_out <- drop_advanced_direction_cols(df_out)
        }
        
        if (!is.null(df_out) && nrow(df_out) > 0) {
          df_out <- data.frame(
            Game = game_name,
            Player = df_out$Name,
            df_out[, setdiff(names(df_out), "Name"), drop = FALSE],
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        }
        
        df_out <- rename_unit_headers(df_out)
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

    # Final stored drawing geometry (GeoJSON FeatureCollection persisted on the
    # ON_OK_CLICKED answer for free DRAW tasks). Preferred over the raw click
    # stream when present; falls back to clicks for older tracks.
    drawing_feats <- NULL
    has_drawing_col <- !is.null(data[[1]]$events$answer) &&
      "drawing" %in% names(data[[1]]$events$answer)

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

        
        
        if (!is.na(type_task[i]) && type_task[i] == "theme-object" && cou == num_value_num()) {
          
          # Always set task type for object-location tasks.
          # This prevents "No task exists with this number" for MULTIPLE_CHOICE object tasks.
          t <- type_task[i]
          
          ans_i <- if (!is.null(ans_type) && length(ans_type) >= i) {
            as.character(ans_type[[i]])
          } else {
            NA_character_
          }
          
          # Object-location tasks can have map geometry even when the answer is MULTIPLE_CHOICE.
          # Example: "Wähle das passende Foto für den markierten Ort."
          ring <- get_polygon_ring(data[[1]]$events, i)
          
          if (!is.null(ring) && nrow(ring) >= 3) {
            lng_poly <- as.list(ring[, 1])
            lat_poly <- as.list(ring[, 2])
            mr <- FALSE
          } else if (!is.na(ans_i) && ans_i %in% c("PHOTO", "MULTIPLE_CHOICE")) {
            # If an object-photo / object-multiple-choice task has no geometry,
            # then there is genuinely nothing useful to draw on the map.
            mr <- TRUE
          }
          
          # Only MAP_POINT object tasks have a clicked answer marker.
          # MULTIPLE_CHOICE object tasks should show the marked polygon only.
          if (!is.na(ans_i) && ans_i == "MAP_POINT") {
            cp <- get_click_lonlat(data[[1]]$events, i)
            
            if (length(cp) == 2 && all(is.finite(cp))) {
              lng_ans_obj <- append(lng_ans_obj, cp[1])
              lat_ans_obj <- append(lat_ans_obj, cp[2])
            }
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
      `Time (s)` = sm$time_s,
      Tries = sm$tries,
      `Viewing dir. (°)` = format_bearing_deg(sm$viewing_direction),
      `Final view dir. (°)` = format_bearing_deg(sm$final_viewing_direction),
      `Pointing dir. (°)` = format_bearing_deg(sm$pointing_direction),
      `Rotation (°)` = format_angle_deg(sm$rotation_angle),
      `Final answer (°)` = format_bearing_deg(sm$final_answer_direction),
      `Error (°/m)` = sm$error_txt,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    info_mask <- tolower(trimws(df$Type)) == "information"
    df$Answer[info_mask] <- NA_character_
    df$Tries[info_mask] <- 0
    df[["Viewing dir. (°)"]][info_mask] <- NA_character_
    df[["Final view dir. (°)"]][info_mask] <- NA_character_
    df[["Pointing dir. (°)"]][info_mask] <- NA_character_
    df[["Rotation (°)"]][info_mask] <- NA_character_
    df[["Final answer (°)"]][info_mask] <- NA_character_
    df[["Error (°/m)"]][info_mask] <- NA_character_
    
    # For dashboard display: show max 2-line information text, full text on hover
    df <- apply_assignment_display_limit(df)
    
    
    
    
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
      df_show <- filtered_df()
      
      if (!advanced_direction_enabled()) {
        df_show <- drop_advanced_direction_cols(df_show)
      }
      
      DT::datatable(
        df_show,
        escape = setdiff(names(df_show), "Assignment"),
        class = "compact stripe hover",
        options = list(
          pageLength = 10,
          ordering = FALSE,
          autoWidth = FALSE
        )
      )
    })
    
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
        
        if (!advanced_direction_enabled()) {
          df_out <- drop_advanced_direction_cols(df_out)
        }
        
        if (!is.null(df_out) && nrow(df_out) > 0) {
          if ("Assignment" %in% names(df_out)) {
            df_out$Assignment <- html_visible_text(df_out$Assignment)
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
      
      if (is.finite(lon0) && is.finite(lat0) && is.finite(ans_b)) {
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
        if (is.finite(cor_b)) {
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
          addPolylines(
            lng = unlist(traj_lng),
            lat = unlist(traj_lat),
            color = "red",
            weight = 2,
            opacity = 1,
            stroke = TRUE,
            group = REFERENCE_BASE_TRAJ_GROUP
          )
      }
    }
    if (length(dr_point_lng) != 0 || !is.null(drawing_feats)) {
      map_shown <- {
        m <- leaflet() %>% addTiles()
        if (!is.null(drawing_feats)) {
          m <- add_drawing_features(m, drawing_feats)
        } else {
          m <- m %>% addPolylines(lng = unlist(dr_point_lng), lat = unlist(dr_point_lat), color = "red", weight = 2, opacity = 1, stroke = TRUE)
        }
        m
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
              color = "red",
              weight = 2,
              opacity = 1,
              stroke = TRUE,
              group = REFERENCE_BASE_TRAJ_GROUP
            )
        }
        
        if (length(dr_point_lng) != 0 || !is.null(drawing_feats)) {
          map_shown <- leaflet() %>% addTiles()
          if (!is.null(drawing_feats)) {
            map_shown <- add_drawing_features(map_shown, drawing_feats)
          } else {
            map_shown <- map_shown %>%
              addPolylines(
                lng = unlist(dr_point_lng),
                lat = unlist(dr_point_lat),
                color = "red",
                weight = 2,
                opacity = 1,
                stroke = TRUE
              )
          }
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
          
          if (is.finite(lon0) && is.finite(lat0) && is.finite(ans_b)) {
            
            # Make arrow smaller if you want
            A <- arrow_lines(lon0, lat0, ans_b, len_m = 18, head_m = 5, head_ang = 25)
            
            map_shown <- map_shown %>%
              addMarkers(lng = lon0, lat = lat0, icon = loc_marker) %>%
              
              # Player FINAL answer arrow (BLUE)
              addPolylines(lng = A$main$lng,  lat = A$main$lat,  color = "blue",  weight = 4, opacity = 1) %>%
              addPolylines(lng = A$left$lng,  lat = A$left$lat,  color = "blue",  weight = 4, opacity = 1) %>%
              addPolylines(lng = A$right$lng, lat = A$right$lat, color = "blue",  weight = 4, opacity = 1)
            
            # Correct direction arrow (GREEN) - optional
            if (is.finite(cor_b)) {
              C <- arrow_lines(lon0, lat0, cor_b, len_m = 18, head_m = 5, head_ang = 25)

              map_shown <- map_shown %>%
                addPolylines(lng = C$main$lng,  lat = C$main$lat,  color = "green", weight = 4, opacity = 0.9) %>%
                addPolylines(lng = C$left$lng,  lat = C$left$lat,  color = "green", weight = 4, opacity = 0.9) %>%
                addPolylines(lng = C$right$lng, lat = C$right$lat, color = "green", weight = 4, opacity = 0.9)
            }
            
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
    output$cmp_table2 <- renderTable({
      df_show <- cores
      
      if (!advanced_direction_enabled()) {
        df_show <- drop_advanced_direction_cols(df_show)
      }
      
      df_show
    })
    
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
  
  #### STARTS - toggle buttons sync lock####
  
  # Sync the two Advanced Direction Task Analysis toggles
  adv_dir_lock <- FALSE
  
  observeEvent(input$advanced_direction_analysis_all, {
    if (adv_dir_lock) return()
    
    adv_dir_lock <<- TRUE
    updateSwitchInput(
      session,
      inputId = "advanced_direction_analysis_compare",
      value = isTRUE(input$advanced_direction_analysis_all)
    )
    adv_dir_lock <<- FALSE
  }, ignoreInit = TRUE)
  
  observeEvent(input$advanced_direction_analysis_compare, {
    if (adv_dir_lock) return()
    
    adv_dir_lock <<- TRUE
    updateSwitchInput(
      session,
      inputId = "advanced_direction_analysis_all",
      value = isTRUE(input$advanced_direction_analysis_compare)
    )
    adv_dir_lock <<- FALSE
  }, ignoreInit = TRUE)
  
  #### ENDS - toggle buttons sync lock####
  
  
  
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
  
  
  # Overlay trajectories of multiple selected players on the Map tab.
  # Overlay trajectories of multiple selected players on the Map tab.
  observeEvent(
    list(input$selected_map_files, input$num_value, current_single_player(), map_rv()),
    {
      req(input$num_value)
      req(map_rv())
      req(apiURL_rv())
      req(accessToken_rv())
      
      selected_ids <- input$selected_map_files
      if (is.null(selected_ids)) {
        selected_ids <- character(0)
      }
      
      task_no <- suppressWarnings(as.integer(input$num_value))
      
      req(current_single_player())
      ref_id <- as.character(current_single_player())
      
      # If no player is selected in "Players shown as trajectories",
      # clear the old overlay and old legend from the map.
      if (is.na(task_no) || task_no <= 0 || length(selected_ids) == 0) {
        session$onFlushed(function() {
          leafletProxy(
            mapId = "map",
            session = session,
            deferUntilFlush = FALSE
          ) %>%
            clearGroup(MAP_TRAJ_GROUP) %>%
            clearGroup(REFERENCE_BASE_TRAJ_GROUP) %>%
            clearControls()
          
          # Also clear any animated trajectory layer if it exists.
          session$sendCustomMessage(
            "animateTrajectories",
            list(
              mapId = "map",
              trajectories = list()
            )
          )
        }, once = TRUE)
        
        return()
      }
      
      # IMPORTANT:
      # Capture reactive values here, inside the observer.
      # Do not call apiURL_rv(), accessToken_rv(), or choices_rv()
      # inside session$onFlushed().
      api_url_now <- apiURL_rv()
      token_now <- accessToken_rv()
      choices_now <- choices_rv()
      
      session$onFlushed(function() {
        
        proxy <- leafletProxy(
          mapId = "map",
          session = session,
          deferUntilFlush = FALSE
        ) %>%
          clearGroup(MAP_TRAJ_GROUP) %>%
          clearGroup(REFERENCE_BASE_TRAJ_GROUP) %>%
          clearControls()
        
        legend_labels <- character(0)
        legend_colors <- character(0)
        all_lng <- numeric(0)
        all_lat <- numeric(0)
        
        colors <- rep(MAP_TRAJ_COLORS, length.out = length(selected_ids))
        
        for (i in seq_along(selected_ids)) {
          track_id <- selected_ids[i]
          
          tr <- get_track_for_map(
            track_id = track_id,
            api_url = api_url_now,
            token = token_now
          )
          
          if (is.null(tr)) next
          
          traj <- get_task_trajectory_df(tr, task_no)
          if (is.null(traj) || nrow(traj) < 2) next
          
          player_label <- get_map_track_label(
            track_id = track_id,
            tr = tr,
            choices_now = choices_now
          )
          
          is_reference_player <- !is.na(ref_id) && as.character(track_id) == ref_id
          
          # The reference player's trajectory is already drawn in the base map.
          # Do not draw it again, otherwise it becomes visually thicker.
          this_color <- if (is_reference_player) REFERENCE_TRAJ_COLOR else colors[i]
          
          # The reference player's trajectory is already drawn in the base map.
          # Do not draw it again, otherwise it can look thicker.
          # Draw every player selected in "Players shown as trajectories",
          # including the reference player if it is selected there.
          proxy <- proxy %>%
            addPolylines(
              lng = traj$lng,
              lat = traj$lat,
              color = this_color,
              weight = MAP_TRAJ_WEIGHT,
              opacity = MAP_TRAJ_OPACITY,
              group = MAP_TRAJ_GROUP,
              label = player_label,
              popup = player_label
            )
          
          # Still include the reference player in the legend and map bounds
          legend_labels <- c(legend_labels, player_label)
          legend_colors <- c(legend_colors, this_color)
          all_lng <- c(all_lng, traj$lng)
          all_lat <- c(all_lat, traj$lat)
        }
        
        if (length(legend_labels) > 0) {
          proxy <- proxy %>%
            addLegend(
              position = "topright",
              colors = legend_colors,
              labels = legend_labels,
              opacity = 1,
              title = paste("Task", task_no, "trajectories")
            )
        }
        
        if (length(all_lng) > 1 && length(all_lat) > 1) {
          lng_range <- range(all_lng, na.rm = TRUE)
          lat_range <- range(all_lat, na.rm = TRUE)
          
          if (is.finite(lng_range[1]) && is.finite(lng_range[2]) &&
              is.finite(lat_range[1]) && is.finite(lat_range[2])) {
            
            if (lng_range[1] == lng_range[2] && lat_range[1] == lat_range[2]) {
              proxy <- proxy %>%
                setView(lng = lng_range[1], lat = lat_range[1], zoom = 19)
            } else {
              proxy <- proxy %>%
                fitBounds(
                  lng1 = lng_range[1],
                  lat1 = lat_range[1],
                  lng2 = lng_range[2],
                  lat2 = lat_range[2]
                )
            }
          }
        }
        
      }, once = TRUE)
      
    },
    ignoreInit = FALSE,
    priority = -100
  )
  
  
  observeEvent(input$visualize_trajectories_btn, {
    req(input$num_value)
    req(map_rv())
    req(apiURL_rv())
    req(accessToken_rv())
    
    selected_ids <- input$selected_map_files
    if (is.null(selected_ids)) {
      selected_ids <- character(0)
    }
    
    task_no <- suppressWarnings(as.integer(input$num_value))
    
    if (is.na(task_no) || task_no <= 0 || length(selected_ids) == 0) {
      leafletProxy(
        mapId = "map",
        session = session,
        deferUntilFlush = FALSE
      ) %>%
        clearGroup(MAP_TRAJ_GROUP) %>%
        clearGroup(REFERENCE_BASE_TRAJ_GROUP) %>%
        clearControls()
      
      session$sendCustomMessage(
        "animateTrajectories",
        list(
          mapId = "map",
          trajectories = list()
        )
      )
      
      showNotification("No trajectory player selected.", type = "warning")
      return()
    }
    
    req(current_single_player())
    ref_id <- as.character(current_single_player())
    
    api_url_now <- apiURL_rv()
    token_now <- accessToken_rv()
    choices_now <- choices_rv()
    
    colors <- rep(MAP_TRAJ_COLORS, length.out = length(selected_ids))
    trajectory_payload <- list()
    
    for (i in seq_along(selected_ids)) {
      track_id <- selected_ids[i]
      
      tr <- get_track_for_map(
        track_id = track_id,
        api_url = api_url_now,
        token = token_now
      )
      
      if (is.null(tr)) next
      
      traj <- get_task_trajectory_df(tr, task_no)
      if (is.null(traj) || nrow(traj) < 2) next
      
      player_label <- get_map_track_label(
        track_id = track_id,
        tr = tr,
        choices_now = choices_now
      )
      
      is_reference_player <- !is.na(ref_id) && as.character(track_id) == ref_id
      this_color <- if (is_reference_player) REFERENCE_TRAJ_COLOR else colors[i]
      
      trajectory_payload[[length(trajectory_payload) + 1]] <- list(
        label = player_label,
        color = this_color,
        lat = unname(as.list(traj$lat)),
        lng = unname(as.list(traj$lng))
      )
    }
    
    if (length(trajectory_payload) == 0) {
      showNotification("No trajectory points found for the selected task.", type = "warning")
      return()
    }
    
    # Remove the static multi-player overlay first, so the animation is clearly visible.
    leafletProxy(
      mapId = "map",
      session = session,
      deferUntilFlush = FALSE
    ) %>%
      clearGroup(MAP_TRAJ_GROUP) %>%
      clearGroup(REFERENCE_BASE_TRAJ_GROUP) %>%
      clearControls()
    
    session$onFlushed(function() {
      session$sendCustomMessage(
        "animateTrajectories",
        list(
          mapId = "map",
          trajectories = trajectory_payload,
          duration = 6500,
          steps = 170,
          lineWeight = MAP_TRAJ_WEIGHT
        )
      )
    }, once = TRUE)
    
  }, ignoreInit = TRUE)
  
  
  
  
  build_current_map_for_download <- function() {
    req(map_rv())
    
    m <- map_rv()
    
    selected_ids <- input$selected_map_files
    
    ref_id <- as.character(current_single_player())
    
    # fallback: if no multi-player selection exists, save the reference player map
    if (is.null(selected_ids) || length(selected_ids) == 0) {
      selected_ids <- current_single_player()
    }
    
    task_no <- suppressWarnings(as.integer(input$num_value))
    
    if (is.na(task_no) || task_no <= 0 || is.null(selected_ids) || length(selected_ids) == 0) {
      return(m)
    }
    
    api_url_now <- apiURL_rv()
    token_now   <- accessToken_rv()
    choices_now <- choices_rv()
    
    legend_labels <- character(0)
    legend_colors <- character(0)
    all_lng <- numeric(0)
    all_lat <- numeric(0)
    
    colors <- rep(MAP_TRAJ_COLORS, length.out = length(selected_ids))
    
    for (i in seq_along(selected_ids)) {
      track_id <- selected_ids[i]
      
      tr <- get_track_for_map(
        track_id = track_id,
        api_url  = api_url_now,
        token    = token_now
      )
      
      if (is.null(tr)) next
      
      traj <- get_task_trajectory_df(tr, task_no)
      if (is.null(traj) || nrow(traj) < 2) next
      
      player_label <- get_map_track_label(
        track_id = track_id,
        tr = tr,
        choices_now = choices_now
      )
      
      is_reference_player <- !is.na(ref_id) && as.character(track_id) == ref_id
      
      # The reference player's trajectory is already inside map_rv().
      # Do not add it again to the exported map.
      this_color <- if (is_reference_player) REFERENCE_TRAJ_COLOR else colors[i]
      
      # The reference player's trajectory is already inside map_rv().
      # Do not add it again to the exported map.
      if (!is_reference_player) {
        m <- m %>%
          addPolylines(
            lng = traj$lng,
            lat = traj$lat,
            color = this_color,
            weight = MAP_TRAJ_WEIGHT,
            opacity = MAP_TRAJ_OPACITY,
            group = MAP_TRAJ_GROUP,
            label = player_label,
            popup = player_label
          )
      }
      
      # Still include the reference player in the legend and bounds
      legend_labels <- c(legend_labels, player_label)
      legend_colors <- c(legend_colors, this_color)
      all_lng <- c(all_lng, traj$lng)
      all_lat <- c(all_lat, traj$lat)
    }
    
    if (length(legend_labels) > 0) {
      m <- m %>%
        addLegend(
          position = "topright",
          colors = legend_colors,
          labels = legend_labels,
          opacity = 1,
          title = paste("Task", task_no, "trajectories")
        )
    }
    
    if (length(all_lng) > 1 && length(all_lat) > 1) {
      lng_range <- range(all_lng, na.rm = TRUE)
      lat_range <- range(all_lat, na.rm = TRUE)
      
      if (is.finite(lng_range[1]) && is.finite(lng_range[2]) &&
          is.finite(lat_range[1]) && is.finite(lat_range[2])) {
        
        if (lng_range[1] == lng_range[2] && lat_range[1] == lat_range[2]) {
          m <- m %>%
            setView(lng = lng_range[1], lat = lat_range[1], zoom = 19)
        } else {
          m <- m %>%
            fitBounds(
              lng1 = lng_range[1],
              lat1 = lat_range[1],
              lng2 = lng_range[2],
              lat2 = lat_range[2]
            )
        }
      }
    }
    
    m
  }
  
  output$downloadMap <- downloadHandler(
    filename = function() {
      task_no <- suppressWarnings(as.integer(input$num_value))
      
      if (is.na(task_no)) {
        paste0("map_", Sys.Date(), ".zip")
      } else {
        paste0("map_task_", task_no, "_", Sys.Date(), ".zip")
      }
    },
    
    content = function(file) {
      req(map_rv())
      
      # Build export map with the currently selected trajectories
      export_map <- build_current_map_for_download()
      
      # Temp dir for export
      tmpdir <- tempfile("map_export_")
      dir.create(tmpdir)
      
      # Save widget
      htmlfile <- file.path(tmpdir, "map.html")
      
      htmlwidgets::saveWidget(
        widget = export_map,
        file   = htmlfile,
        selfcontained = FALSE
      )
      
      # Copy virtual-environment images into assets/vir_envs_layers
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
      
      # Zip everything in tmpdir
      oldwd <- getwd()
      on.exit(setwd(oldwd), add = TRUE)
      
      setwd(tmpdir)
      
      zip::zipr(
        zipfile = file,
        files   = list.files(),
        recurse = TRUE
      )
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

    # Use the server's message verbatim (it explains exactly which emails
    # were added, skipped because they're the owner, or rejected because
    # no account exists).
    server_msg <- if (!is.null(result$data$message)) result$data$message else ""

    if (result$status == 200) {
      shared_emails_rv(result$data$sharedWith)
      updateTextInput(session, "share_email_input", value = "")
      msg <- 
        if (nzchar(server_msg)) server_msg 
        else paste0("Shared with ", email)
      showNotification(msg, type = "message", duration = 6)
    } else if (result$status == 400 && nzchar(server_msg)) {
      # 400 = nothing was actually shared (e.g. owner email or no account).
      # Show the server's explanation as a warning, not a hard error.
      showNotification(server_msg, type = "warning", duration = 8)
    } else {
      msg <- 
        if (nzchar(server_msg)) server_msg 
        else "Could not share."
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


  # ── Share an event ──────────────────────────────────────────────────
  # Mirrors the game-share flow, but targets /event/:id/share. Sharing an event
  # makes the recipient a co-editor (can add/remove games) and lets them view
  # the event's tracks. Owner-only on the server (delete/share stay owner-only).

  # Show the share-event button only when a real event is selected.
  output$share_event_button_ui <- renderUI({
    req(event_is_active())
    div(
      style = "margin-bottom: 10px;",
      actionButton("open_share_event_modal", "Share event",
                   icon = icon("share-alt"),
                   style = "width: 100%;")
    )
  })

  # Open the share-event modal and load the current shared list.
  observeEvent(input$open_share_event_modal, {
    req(event_is_active(), accessToken_rv(), apiURL_rv())

    event_id <- input$selected_event
    url <- paste0(apiURL_rv(), "/event/", event_id, "/share")
    result <- api_get(url, accessToken_rv())

    if (result$status == 200 && !is.null(result$data$sharedWith)) {
      event_shared_emails_rv(result$data$sharedWith)
    } else {
      event_shared_emails_rv(character(0))
    }

    showModal(modalDialog(
      title = "Share event",
      size = "m",
      easyClose = TRUE,
      p("Share this event with other GeoGami users. They become co-editors (can add or remove games) and can view the event's tracks."),
      div(
        style = "display: flex; gap: 8px; align-items: flex-end;",
        div(style = "flex: 1;",
            textInput("share_event_email_input", "Email address:", placeholder = "user@example.com")
        ),
        actionButton("share_event_add_btn", "Share", icon = icon("plus"),
                     class = "btn-primary", style = "margin-bottom: 15px;")
      ),
      uiOutput("shared_event_list_ui"),
      footer = modalButton("Close")
    ))
  })

  # Render the current shared-with list with per-email revoke buttons.
  output$shared_event_list_ui <- renderUI({
    emails <- event_shared_emails_rv()
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
              inputId = paste0("remove_event_share_", i),
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

  # Add an email to the event's sharedWith.
  observeEvent(input$share_event_add_btn, {
    req(event_is_active(), accessToken_rv(), apiURL_rv())

    email <- trimws(tolower(input$share_event_email_input))
    if (!grepl("@", email)) {
      showNotification("Please enter a valid email address.", type = "warning")
      return()
    }

    event_id <- input$selected_event
    url <- paste0(apiURL_rv(), "/event/", event_id, "/share")
    result <- api_request(url, accessToken_rv(), body = list(emails = list(email)), method = "POST")

    server_msg <- if (!is.null(result$data$message)) result$data$message else ""
    if (result$status == 200) {
      event_shared_emails_rv(result$data$sharedWith)
      updateTextInput(session, "share_event_email_input", value = "")
      msg <- if (nzchar(server_msg)) server_msg else paste0("Shared with ", email)
      showNotification(msg, type = "message", duration = 6)
    } else if (result$status == 400 && nzchar(server_msg)) {
      showNotification(server_msg, type = "warning", duration = 8)
    } else {
      msg <- if (nzchar(server_msg)) server_msg else "Could not share."
      showNotification(msg, type = "error")
    }
  })

  # Per-email "X" revoke buttons: DELETE /event/:id/share.
  observe({
    emails <- event_shared_emails_rv()
    lapply(seq_along(emails), function(i) {
      btn_id <- paste0("remove_event_share_", i)
      observeEvent(input[[btn_id]], {
        req(event_is_active(), accessToken_rv(), apiURL_rv())

        email_to_remove <- emails[i]
        event_id <- input$selected_event
        url <- paste0(apiURL_rv(), "/event/", event_id, "/share")
        result <- api_request(url, accessToken_rv(),
                              body = list(emails = list(email_to_remove)),
                              method = "DELETE")

        if (result$status == 200) {
          event_shared_emails_rv(result$data$sharedWith)
          showNotification(paste0("Removed ", email_to_remove), type = "message")
        } else {
          showNotification("Could not remove user.", type = "error")
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })


  # ── Share a single track ───────────────────────────────────────────
  # Mirrors the game-share flow above, but targets one specific track via
  # /track/:id/share. The server resolves the track owner (instructor for
  # class plays, otherwise the game creator); only the owner or an admin can
  # manage sharing. selected_files is a multi-select picker, so the button is
  # only offered when exactly one track is selected.

  # Current shared-with emails for the selected track.
  track_shared_emails_rv <- reactiveVal(character(0))

  # Show the button only when exactly one track is selected.
  output$share_track_button_ui <- renderUI({
    req(input$selected_files)
    if (length(input$selected_files) > 1) {
      return(p(style = "color: #888; margin-top: 10px;",
               "Select a single track to share it."))
    }
    div(
      style = "margin-top: 10px;",
      actionButton("open_share_track_modal", "Share this track",
                   icon = icon("share-alt"),
                   style = "width: 150px;")
    )
  })

  # Open the modal: fetch who this track is already shared with.
  observeEvent(input$open_share_track_modal, {
    req(input$selected_files, accessToken_rv(), apiURL_rv())

    track_id <- input$selected_files[1]
    url <- paste0(apiURL_rv(), "/track/", track_id, "/share")
    result <- api_get(url, accessToken_rv())

    if (result$status == 200 && !is.null(result$data$sharedWith)) {
      track_shared_emails_rv(result$data$sharedWith)
    } else {
      track_shared_emails_rv(character(0))
    }

    showModal(modalDialog(
      title = "Share this track",
      size = "m",
      easyClose = TRUE,

      p("Grant other GeoGami users access to view this single track by entering their email."),

      div(
        style = "display: flex; gap: 8px; align-items: flex-end;",
        div(style = "flex: 1;",
            textInput("share_track_email_input", "Email address:", placeholder = "user@example.com")
        ),
        actionButton("share_track_add_btn", "Share", icon = icon("plus"),
                     class = "btn-primary", style = "margin-bottom: 15px;")
      ),

      uiOutput("shared_track_list_ui"),

      footer = modalButton("Close")
    ))
  })

  # Render the list of emails this track is currently shared with.
  output$shared_track_list_ui <- renderUI({
    emails <- track_shared_emails_rv()
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
              inputId = paste0("remove_track_share_", i),
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

  # Handle the "Share" button click: validate email, POST to server.
  observeEvent(input$share_track_add_btn, {
    req(input$selected_files, accessToken_rv(), apiURL_rv())

    email <- trimws(tolower(input$share_track_email_input))
    if (!grepl("@", email)) {
      showNotification("Please enter a valid email address.", type = "warning")
      return()
    }

    track_id <- input$selected_files[1]
    url <- paste0(apiURL_rv(), "/track/", track_id, "/share")

    result <- api_request(url, accessToken_rv(), body = list(emails = list(email)), method = "POST")

    # Use the server's message verbatim (it explains which emails were added,
    # skipped because they're the owner, or rejected because no account exists).
    server_msg <- if (!is.null(result$data$message)) result$data$message else ""

    if (result$status == 200) {
      track_shared_emails_rv(result$data$sharedWith)
      updateTextInput(session, "share_track_email_input", value = "")
      msg <- if (nzchar(server_msg)) server_msg else paste0("Shared with ", email)
      showNotification(msg, type = "message", duration = 6)
    } else if (result$status == 400 && nzchar(server_msg)) {
      showNotification(server_msg, type = "warning", duration = 8)
    } else {
      msg <- if (nzchar(server_msg)) server_msg else "Could not share."
      showNotification(msg, type = "error")
    }
  })

  # Per-email "X" revoke buttons: DELETE /track/:id/share.
  observe({
    emails <- track_shared_emails_rv()
    lapply(seq_along(emails), function(i) {
      btn_id <- paste0("remove_track_share_", i)
      observeEvent(input[[btn_id]], {
        req(input$selected_files, accessToken_rv(), apiURL_rv())

        email_to_remove <- emails[i]
        track_id <- input$selected_files[1]
        url <- paste0(apiURL_rv(), "/track/", track_id, "/share")
        result <- api_request(url, accessToken_rv(),
                              body = list(emails = list(email_to_remove)),
                              method = "DELETE")

        if (result$status == 200) {
          track_shared_emails_rv(result$data$sharedWith)
          showNotification(paste0("Removed ", email_to_remove), type = "message")
        } else {
          showNotification("Could not remove user.", type = "error")
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })


}

shinyApp(ui, server)