# Run with: Rscript tests/regression.R
# Load definitions without constructing the UI or fetching the GitHub footer.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
app_path <- normalizePath(file.path(dirname(script), "..", "app.R"))
app_dir <- dirname(app_path)
setwd(app_dir)
parsed <- as.list(parse(app_path))
app <- new.env(parent = globalenv())
is_assignment <- function(e, name = NULL) {
  is.call(e) && identical(e[[1]], as.name("<-")) &&
    (is.null(name) || identical(e[[2]], as.name(name)))
}
for (e in parsed) {
  if (is.call(e) && identical(e[[1]], as.name("library"))) {
    suppressPackageStartupMessages(eval(e, app))
  } else if (is_assignment(e) && is.call(e[[3]]) &&
             identical(e[[3]][[1]], as.name("function"))) {
    eval(e, app)
  }
}
app$json_dir <- tempdir()
server_expression <- Filter(function(e) is_assignment(e, "server"), parsed)[[1]]
server_body <- as.list(server_expression[[3]][[3]])[-1]
helper_expressions <- Filter(function(e) is_assignment(e) && is.call(e[[3]]) &&
                              identical(e[[3]][[1]], as.name("function")), server_body)
for (e in helper_expressions) eval(e, app)

checks <- 0L
check <- function(name, run) {
  run()
  checks <<- checks + 1L
  cat("PASS:", name, "\n")
}

make_track <- function(n = 3L, change = function(x) x, waypoints = TRUE) {
  task <- list(`_id` = "task1", type = "free", category = "theme",
               question = list(text = "Sample"), answer = list(type = "TEXT"),
               settings = list(accuracy = 15), evaluate = "none")
  events <- lapply(seq_len(n), function(i) list(
    type = if (i == 1L) "INIT_TASK" else if (i == 2L) "ON_OK_CLICKED" else "FINISHED_GAME",
    timestamp = paste0("2026-10-14T10:00:0", i, "Z"), task = task,
    answer = list(correct = if (i == 1L) NA else TRUE, text = "example")
  ))
  points <- if (waypoints) lapply(1:3, function(i) list(
    taskNo = 1L, timestamp = paste0("2026-10-14T10:00:0", i, "Z"),
    position = list(coords = list(longitude = 7 + i / 100000,
                                  latitude = 51 + i / 100000, accuracy = 10))
  )) else list()
  raw <- change(list(`_id` = "track1", players = list("Example"),
                     createdAt = "2026-10-14T10:00:00Z", events = events, waypoints = points))
  json <- jsonlite::toJSON(raw, auto_unbox = TRUE, na = "null", null = "null")
  list(json = json, data = jsonlite::fromJSON(json))
}

cases <- list(
  normal = make_track(),
  single_init = make_track(1L),
  empty_events = make_track(0L, waypoints = FALSE),
  missing_answer_type = make_track(change = function(x) {
    x$events <- lapply(x$events, function(e) { e$task$answer <- NULL; e }); x
  }),
  missing_correctness = make_track(change = function(x) {
    x$events <- lapply(x$events, function(e) { e$answer$correct <- NULL; e }); x
  }),
  missing_task_id = make_track(change = function(x) { x$events[[2]]$task$`_id` <- NA; x }),
  information = make_track(change = function(x) {
    x$events <- lapply(x$events, function(e) {
      e$task$type <- NULL; e$task$category <- "info"; e$task$answer <- NULL; e$answer <- NULL; e
    }); x
  }),
  missing_task_number = make_track(change = function(x) { x$waypoints[[1]]$taskNo <- NA; x }),
  missing_accuracy = make_track(change = function(x) { x$waypoints[[1]]$position$coords$accuracy <- NA; x }),
  missing_coordinates = make_track(change = function(x) { x$waypoints[[1]]$position$coords <- NULL; x }),
  no_waypoints = make_track(2L, waypoints = FALSE),
  navigation_without_target = make_track(change = function(x) {
    x$events <- lapply(x$events, function(e) {
      e$task$type <- "nav-arrow"; e$task$category <- "nav"; e$task$answer$type <- "POSITION"; e
    }); x
  }),
  navigation_with_target = make_track(change = function(x) {
    x$events <- lapply(x$events, function(e) {
      e$task$type <- "nav-arrow"; e$task$category <- "nav"
      e$task$answer <- list(type = "POSITION", position = list(geometry = list(coordinates = c(7.001, 51.001))))
      e
    }); x
  }),
  virtual_direction = make_track(change = function(x) {
    x$events <- lapply(x$events, function(e) {
      e$task$type <- "theme-direction"; e$task$evaluate <- "evalDirection"
      e$task$virEnvType <- "VirEnv_1"; e$task$settings$accuracy <- 45
      e$task$question$direction <- list(bearing = 90)
      e$task$question$initialAvatarPosition <- list(bearing = 0)
      e$task$answer$type <- "MAP_POINT"; e$answer$compassHeading <- 90
      e$compassHeading <- 90; e$position <- list(coords = list(longitude = 0.001, latitude = 0.002))
      e
    }); x
  }),
  two_tasks = make_track(change = function(x) {
    second <- lapply(seq_along(x$events), function(i) {
      e <- x$events[[i]]; e$task$`_id` <- "task2"
      e$timestamp <- paste0("2026-10-14T10:00:0", i + 3L, "Z"); e
    })
    x$events <- c(x$events, second)
    x
  })
)

for (name in names(cases)) local({
  fixture <- cases[[name]]
  check(paste("API track observer:", name), function() {
    env <- new.env(parent = app)
    env$fetch_games_data_from_server <- function(url, token, ...) {
      if (grepl("/game/usergames$", url)) {
        return(data.frame(`_id` = "507f1f77bcf86cd799439011", name = "Example", isVRWorld = FALSE,
                          check.names = FALSE))
      }
      if (grepl("/event/userevents$", url)) return(NULL)
      if (grepl("/track/gametracks/", url)) {
        return(data.frame(`_id` = "track1", players = "Example", createdAt = "2026-10-14",
                          check.names = FALSE))
      }
      if (grepl("/track/track1$", url)) return(fixture$data)
      stop("Unexpected API request in test")
    }
    eval(server_expression, env)
    shiny::testServer(env$server, {
      session$setInputs(theme = "Light", num_value = 1L)
      accessToken_rv("dummy")
      session$setInputs(selected_games = "507f1f77bcf86cd799439011", selected_event = "__none__",
                        selected_files = "track1", selected_data_file_all = "track1")
      stopifnot(!session$isClosed(), is.data.frame(df_react()),
                nrow(df_react()) == sum(fixture$data$events$type == "INIT_TASK"))
      stopifnot(nzchar(output$dashboard_context$html), nzchar(output$dashboard_summaries$html))
      if (nrow(df_react())) {
        stopifnot(nzchar(output$iris_data), nzchar(output$dashboard_task_detail$html))
      }
      if (name == "normal") {
        stopifnot(df_react()$Answer == "Correct example", df_react()$Tries == 1L)
      }
      if (name == "navigation_with_target") {
        stopifnot(any(vapply(map_rv()$x$calls, function(call) call$method == "addMarkers", logical(1))))
      }
      if (name == "virtual_direction") {
        stopifnot(df_react()[["Final view dir. (°)"]] == "90", df_react()[["Rotation (°)"]] == "90")
      }
    })
  })
  check(paste("Standalone JSON upload:", name), function() {
    file <- tempfile(fileext = ".json")
    on.exit(unlink(file))
    writeLines(fixture$json, file)
    env <- new.env(parent = app)
    env$fetch_games_data_from_server <- function(...) stop("Uploads must not fetch API data")
    eval(server_expression, env)
    shiny::testServer(env$server, {
      session$setInputs(theme = "Light", num_value = 1L,
                        uploaded_json_file = data.frame(name = "track.json", datapath = file,
                                                        size = file.info(file)$size,
                                                        type = "application/json"))
      stopifnot(!session$isClosed(), !is.null(map_rv()))
      stopifnot(nrow(app$task_summary(uploaded_json()[[1]])) == sum(fixture$data$events$type == "INIT_TASK"))
      stopifnot(identical(dashboard_source(), "upload"), nzchar(output$dashboard_context$html),
                nzchar(output$dashboard_summaries$html))
      if (nrow(df_react())) {
        stopifnot(nzchar(output$iris_data), nzchar(output$dashboard_task_detail$html))
      }
    })
  })
})

check("Invalid uploaded JSON leaves the session open", function() {
  file <- tempfile(fileext = ".json")
  on.exit(unlink(file))
  writeLines("not JSON", file)
  env <- new.env(parent = app)
  env$fetch_games_data_from_server <- function(...) stop("Uploads must not fetch API data")
  eval(server_expression, env)
  shiny::testServer(env$server, {
    session$setInputs(theme = "Light", num_value = 1L,
                      uploaded_json_file = data.frame(name = "invalid.json", datapath = file,
                                                      size = file.info(file)$size,
                                                      type = "application/json"))
    stopifnot(!session$isClosed())
  })
})

response <- function(status, text) structure(list(
  status_code = status, headers = list(`content-type` = "application/json"),
  content = charToRaw(text), url = "https://example.invalid"
), class = "response")
for (method in c("GET", "POST", "DELETE")) local({
  check(paste("Handled API transport failure:", method), function() {
    count <- 0L
    fail <- function(...) { count <<- count + 1L; stop("Simulated connection failure") }
    result <- app$api_call("https://example.invalid", "dummy", method = method, request = fail)
    stopifnot(result$status == 0L, nzchar(result$data$message), count == 1L)
  })
  check(paste("Handled invalid API JSON:", method), function() {
    result <- app$api_call("https://example.invalid", "dummy", method = method,
                           request = function(...) response(200L, "<html>proxy error</html>"))
    stopifnot(result$status == 502L, nzchar(result$data$message))
  })
})
check("API envelopes and HTTP errors preserve their contract", function() {
  ok <- function(...) response(200L, '{"content":[{"_id":"track1"}]}')
  stopifnot(app$fetch_games_data_from_server("https://example.invalid", "dummy", request = ok)$`_id` == "track1")
  bad <- function(...) response(403L, '{"message":"Not allowed"}')
  stopifnot(app$api_get("https://example.invalid", "dummy", request = bad)$status == 403L)
  stopifnot(is.null(app$fetch_games_data_from_server("https://example.invalid", "dummy", request = bad)))
})
check("API mutations keep JSON, authentication, and a bounded timeout", function() {
  request <- function(method, url, ...) {
    args <- list(...)
    configs <- Filter(function(x) inherits(x, "request"), args)
    stopifnot(method == "DELETE", args$encode == "json",
              identical(args$body$emails, list("C@example.invalid")),
              any(vapply(configs, function(x) identical(unname(x$headers["Authorization"]), "Bearer dummy"), logical(1))),
              any(vapply(configs, function(x) identical(x$options$timeout_ms, 30000), logical(1))))
    response(200L, '{"sharedWith":[]}')
  }
  result <- app$api_request("https://example.invalid", "dummy",
                            body = list(emails = list("C@example.invalid")),
                            method = "DELETE", request = request)
  stopifnot(result$status == 200L, length(result$data$sharedWith) == 0L)
})

sharing_expression <- Filter(function(e) is_assignment(e, "observe_share_removals"), server_body)[[1]]
for (kind in c("game", "event", "track")) local({
  prefix <- switch(kind, game = "remove_share_", event = "remove_event_share_", track = "remove_track_share_")
  requests <- list()
  fail <- FALSE
  mini <- function(input, output, session) {
    emails <- shiny::reactiveVal(character(0))
    target <- shiny::reactiveVal(NULL)
    accessToken_rv <- function() "dummy"
    apiURL_rv <- function() "https://example.invalid"
    showNotification <- function(...) NULL
    api_request <- function(url, token, body, method) {
      requests[[length(requests) + 1L]] <<- list(url = url, email = body$emails[[1]])
      if (fail) return(list(status = 0L, data = list()))
      list(status = 200L, data = list(sharedWith = setdiff(shiny::isolate(emails()), body$emails[[1]])))
    }
    eval(sharing_expression)
    observe_share_removals(emails, target, function() input$item, prefix, paste0("/", kind, "/"))
  }
  click <- function(session, count) do.call(session$setInputs, setNames(list(count), paste0(prefix, 1)))
  check(paste("Sharing observers replace stale recipients and allow retries:", kind), function() {
    shiny::testServer(mini, {
      session$setInputs(item = "first")
      target("first"); emails(c("A@example.invalid", "B@example.invalid")); session$flushReact()
      session$setInputs(item = "second")
      click(session, 1L)
      stopifnot(length(requests) == 0L)
      target("second"); emails(c("C@example.invalid", "A@example.invalid")); session$flushReact()
      fail <<- TRUE
      click(session, 2L)
      stopifnot(length(requests) == 1L, requests[[1]]$email == "C@example.invalid")
      fail <<- FALSE
      click(session, 3L)
      stopifnot(length(requests) == 2L, requests[[2]]$email == "C@example.invalid",
                requests[[2]]$url == paste0("https://example.invalid/", kind, "/second/share"))
      click(session, 4L)
      stopifnot(length(requests) == 3L, requests[[3]]$email == "A@example.invalid", !session$isClosed())
    })
  })
})

check("Session cards distinguish missing recordings from measured zero", function() {
  absent <- app$dashboard_session_metrics(cases$no_waypoints$data)
  normal <- app$dashboard_session_metrics(cases$normal$data)
  unfinished <- app$dashboard_session_metrics(cases$single_init$data)
  stopifnot(is.na(absent$distance), normal$tasks == 1L, normal$correct == 1L,
            normal$duration == 2, is.finite(normal$distance), normal$distance >= 0,
            is.na(unfinished$duration))
})

check("Task filters preserve original identity and reject invalid task numbers", function() {
  df <- data.frame(Type = c("Free", "Information", "Free", "Navigation with arrow"))
  stopifnot(identical(app$dashboard_task_rows(df, c("3", "1", "invalid", "999"), "Free"), c(1L, 3L)),
            identical(app$dashboard_task_rows(df, character(), "Information"), 2L),
            length(app$dashboard_task_rows(df, "999", "__all__")) == 0L)
})

dashboard_test_env <- function() {
  env <- new.env(parent = app)
  env$fetch_games_data_from_server <- function(url, token, ...) {
    if (grepl("/game/usergames$", url)) return(data.frame(`_id` = "507f1f77bcf86cd799439011",
      name = "Example", isVRWorld = FALSE, check.names = FALSE))
    if (grepl("/event/userevents$", url)) return(NULL)
    if (grepl("/track/gametracks/", url)) return(data.frame(`_id` = c("track1", "track2"),
      players = c("Example", "Other"), createdAt = c("2026-10-14", "2026-10-13"), check.names = FALSE))
    if (grepl("/track/track1$", url)) return(cases$two_tasks$data)
    if (grepl("/track/track2$", url)) {
      tr <- cases$normal$data; tr$players <- "Other"; tr$`_id` <- "track2"; return(tr)
    }
    stop("Unexpected API request in redesign test")
  }
  eval(server_expression, env)
  env
}

check("Filtered task clicks use task keys and reject stale sessions", function() {
  env <- dashboard_test_env()
  updates <- list()
  env$updatePickerInput <- function(session, inputId, choices = NULL, selected = NULL, ...) {
    updates[[length(updates) + 1L]] <<- list(id = inputId, selected = selected)
    shinyWidgets::updatePickerInput(session, inputId, choices = choices, selected = selected, ...)
  }
  shiny::testServer(env$server, {
    session$setInputs(theme = "Light", num_value = 1L)
    accessToken_rv("dummy")
    session$setInputs(selected_games = "507f1f77bcf86cd799439011", selected_event = "__none__",
                      selected_files = c("track1", "track2"), selected_data_file_all = "track1", selected_task_ids = "2")
    stopifnot(identical(filtered_task_rows(), 2L), nrow(filtered_df()) == 1L)
    sm <- dashboard_summary(); generation <- dashboard_generation()
    session$setInputs(dashboard_task_clicked = list(task = 2L, source = generation, key = sm$task_key[2]))
    # A valid click emits updates for the original task number, despite the filtered row being first.
    stopifnot(any(vapply(updates, function(msg) identical(msg$id, "num_value") &&
                          identical(msg$selected, "2"), logical(1))))
    session$setInputs(num_value = 2L)
    session$setInputs(selected_data_file_all = "track2")
    stopifnot(dashboard_generation() > generation, dashboard_player_label(dashboard_track()) == "Other",
              num_value_num() == 1L, !is.null(map_rv()))
    before <- length(updates)
    session$setInputs(dashboard_task_clicked = list(task = 2L, source = generation, key = sm$task_key[2]))
    stopifnot(nrow(dashboard_summary()) == 1L, length(updates) == before, !session$isClosed())
  })
})

check("Upload, account return and deselection refresh summaries and CSV metadata", function() {
  env <- dashboard_test_env()
  file <- tempfile(fileext = ".json")
  on.exit(unlink(file))
  raw <- cases$normal$data; raw$players <- "Imported Player"
  jsonlite::write_json(raw, file, auto_unbox = TRUE)
  shiny::testServer(env$server, {
    session$setInputs(theme = "Light", num_value = 1L)
    accessToken_rv("dummy")
    session$setInputs(selected_games = "507f1f77bcf86cd799439011", selected_event = "__none__",
                      selected_files = "track1", selected_data_file_all = "track1")
    stopifnot(nrow(df_react()) == 2L)
    session$setInputs(num_value = 2L)
    session$setInputs(uploaded_json_file = data.frame(name = "uploaded.json", datapath = file,
      size = file.info(file)$size, type = "application/json"))
    stopifnot(dashboard_source() == "upload", nrow(df_react()) == 1L, num_value_num() == 1L,
              dashboard_player_label(dashboard_track()) == "Imported Player")
    csv <- readLines(output$save_data, warn = FALSE)
    stopifnot(any(grepl("Imported JSON", csv)), any(grepl("Imported Player", csv)))
    session$setInputs(dashboard_use_account = 1L)
    stopifnot(dashboard_source() == "server", nrow(df_react()) == 2L)
    session$setInputs(selected_files = character())
    stopifnot(is.null(dashboard_track()), nrow(df_react()) == 0L, identical(output$dashboard_has_track, "FALSE"))
  })
})

check("Photo selection follows task occurrences and retains assignment and answer images", function() {
  track <- cases$two_tasks$data
  track$events$task$`_id`[] <- "repeated"
  track$events$task$question$photo <- c(rep("https://example.invalid/first.png", 3), rep("https://example.invalid/second.png", 3))
  track$events$answer$photo <- c(NA, "https://example.invalid/answer1.jpg", NA,
                                NA, "https://example.invalid/answer2.jpg", NA)
  summary <- app$task_summary(track)
  first <- app$dashboard_photo_urls(track, summary[1, , drop = FALSE])
  second <- app$dashboard_photo_urls(track, summary[2, , drop = FALSE])
  stopifnot(first$assignment == "https://example.invalid/first.png", first$answer == "https://example.invalid/answer1.jpg",
            second$assignment == "https://example.invalid/second.png", second$answer == "https://example.invalid/answer2.jpg",
            summary$task_key[1] != summary$task_key[2])
})

check("Group tables and dark charts render while reference exports retain their metadata", function() {
  env <- dashboard_test_env()
  shiny::testServer(env$server, {
    session$setInputs(theme = "Dark", num_value = 1L)
    accessToken_rv("dummy")
    session$setInputs(selected_games = "507f1f77bcf86cd799439011", selected_event = "__none__",
      selected_files = c("track1", "track2"), selected_data_file_all = "track1",
      selected_multiple_files = c("track1", "track2"))
    stopifnot(grepl("Other", output$cmp_table1), nzchar(output$pie_chart$src),
              nzchar(output$time_chart$src), identical(track_data_rv()$players, "Example"))
    chart <- output$save_picture
    stopifnot(identical(readBin(chart, "raw", 8L), as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))))
    session$setInputs(selected_files = "track1")
    json <- jsonlite::fromJSON(output$download_json)
    stopifnot(identical(json$players, "Example"), json$`_id` == "track1", !session$isClosed())
  })
})

check("Sidebar session rows select a valid reference and follow deselection", function() {
  env <- dashboard_test_env()
  shiny::testServer(env$server, {
    session$setInputs(theme = "Light", num_value = 1L)
    accessToken_rv("dummy")
    session$setInputs(selected_games = "507f1f77bcf86cd799439011", selected_event = "__none__",
                      selected_files = c("track1", "track2"))
    session$setInputs(dashboard_reference_session = "track2")
    stopifnot(unname(current_single_player()) == "track2", dashboard_player_label(dashboard_track()) == "Other",
              grepl('aria-pressed="true"', output$dashboard_session_list$html),
              grepl('<strong>Other</strong>', output$dashboard_session_list$html))
    session$setInputs(dashboard_reference_session = "outside-game")
    stopifnot(unname(current_single_player()) == "track2")
    session$setInputs(selected_files = "track1")
    session$setInputs(dashboard_reference_session = "track2")
    stopifnot(unname(current_single_player()) == "track1", grepl("1 session selected", output$dashboard_session_count$html))
    session$setInputs(selected_files = character())
    stopifnot(is.null(current_single_player()), !grepl('aria-pressed="true"', output$dashboard_session_list$html),
              identical(output$dashboard_has_track, "FALSE"))
  })
})

check("Sidebar session metadata supports group names and missing dates", function() {
  records <- app$dashboard_session_records(data.frame(`_id` = c("one", "two", "three"),
    players = I(list(c("A", "B"), character(), "Other")),
    createdAt = c("2026-10-14T10:00:00Z", NA, "2026-99-99"), check.names = FALSE))
  stopifnot(records$player[1] == "A, B", grepl("12:00", records$date[1]),
            records$player[2] == "Unnamed session", records$date[2] == "Date not recorded",
            records$date[3] == "2026-99-99")
})

check("Dashboard UI has one binding per input across all five tabs", function() {
  app$version_footer <- "Test version"
  expression <- Filter(function(e) is_assignment(e, "ui"), parsed)[[1]]
  eval(expression, app)
  rendered <- htmltools::renderTags(app$ui)
  html <- paste(rendered$head, rendered$html)
  ids <- regmatches(html, gregexpr(' id="[^"]+"', html))[[1]]
  stopifnot(!anyDuplicated(ids), grepl('dashboard.css', html), grepl('dashboard.js', html),
            grepl('Compare Players', html), grepl('Statistics', html))
})
cat(checks, "regression checks passed.\n")
