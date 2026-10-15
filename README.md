<p align="center">
  <img src=https://github.com/origami-team/geogami/blob/master/src/assets/icons/icon.png width="100" alt="GeoGami logo"/>
</p>

<h1 align="center">GeoGami Dashboard</h1>

<p align="center">
  Interactive analytics dashboard for inspecting and evaluating <strong>GeoGami</strong> game tracks.
</p>

<p align="center">
  <img src="https://img.shields.io/badge/R-Shiny-1A6FB5?logo=R" alt="R Shiny"/>
  <img src="https://img.shields.io/badge/Leaflet-maps-199900?logo=leaflet" alt="Leaflet"/>
  <img src="https://img.shields.io/badge/Docker-ready-2496ED?logo=docker" alt="Docker"/>
</p>

The GeoGami Dashboard is an **R Shiny** application used by game creators, scholars, and admins to:

- Browse the games they have access to (their own, games **shared** with them, games they instruct via a class QR, or games containing a track shared with them)
- View track sessions for each game and inspect tasks, answers, photos, and a Leaflet map of the route
- Export evaluation data and track maps
- Grant or revoke access to a game's tracks for other GeoGami users (by email)
- Share — or revoke — a **single track** with another user, independently of the whole game (by email)

> Companion projects:
> - **Front-end UI**: [`../geogami-ui`](https://github.com/geogami-team/geogami)
> - **Backend API**: [`../geogami-server`](https://github.com/geogami-team/origami-backend)
> - **Virtual environment**: [`../geogami-virtual-environment-dev`](https://github.com/geogami-team/geogami-virtual-environment-dev)
> - **Platform documentation**: [`geogami-docs`](https://github.com/geogami-team/geogami-docs) — including the [Track Data Reference](https://github.com/geogami-team/geogami-docs/blob/HEAD/docs/TRACK_DATA_REFERENCE.md) describing the JSON files this dashboard works with

---

## Table of contents

- [Tech stack](#tech-stack)
- [Prerequisites](#prerequisites)
- [Quick start (Docker)](#quick-start-docker)
- [Local development without Docker](#local-development-without-docker)
- [How authentication works](#how-authentication-works)
- [User guide](#user-guide)
- [Project layout](#project-layout)
- [Deployment](#deployment)

---

## Tech stack

| Area | Technology |
|---|---|
| Framework | R 4.x + Shiny |
| UI | `bslib` themes, `shinyWidgets` |
| Maps | `leaflet`, `htmlwidgets` |
| Tables | `DT` |
| HTTP | `httr` (calls the GeoGami API) |
| Server | `rocker/shiny` (Docker) on port `3838` |

## Prerequisites

- **Docker** (recommended), or a local R 4.x install
- A reachable **GeoGami server** (REST API)
- A valid GeoGami **JWT** for any user with track-evaluation access

## Quick start (Docker)

```bash
docker compose up -d        # Shiny on http://localhost:3838
```

`docker-compose.yml` mounts `app.R` and `www/` from the host so local code,
styles, scripts and virtual-map assets stay in sync. The default service uses
the published image; update it with:

```bash
docker compose pull
docker compose up -d --force-recreate
```

To build locally, enable the `build` block in the compose file and run
`docker compose up -d --build`.

## Local development without Docker

```r
# inside the geogami-dashboard directory
install.packages(c(
  "shiny", "shinythemes", "DT", "wordcloud2", "ggplot2",
  "stringr", "dplyr", "leaflet", "bslib", "htmlwidgets",
  "httr", "jsonlite", "zip", "shinyWidgets"
))
shiny::runApp("app.R", port = 3838)
```

## How authentication works

The dashboard does **not** present its own login screen. Users open the dashboard from the GeoGami front-end, which appends their JWT to the URL:

```
https://dashboard.example.com/?token=<JWT>
```

The Shiny app reads the `token` query parameter and uses it for all subsequent API calls (`Authorization: Bearer <token>`). Without a token the sidebar selectors stay hidden.

The default API URL is hardcoded near the top of `app.R`:

```r
apiURL_rv <- reactiveVal("https://api.geogami.uni-muenster.de")
```

Change this when running against a local server during development.

## User guide

The interface has a compact action header, searchable event/game/session
selectors, a shared reference session, light/dark themes and responsive panels.
The All tasks table supports sorting, task/type filters, search, column
visibility and a task preview with a Leaflet map. Map, Pictures, Compare Players
and Statistics use the same reference session and synchronized task selection.

The summary cards always describe the complete reference session, regardless
of table filters or the active tab:

- **Tasks recorded:** the number of `INIT_TASK` blocks, including repeated tasks.
- **Score:** canonical correct-task count; information tasks count as correct
  and appear as **Viewed** in the interface.
- **Session time:** elapsed time between the first and last recorded event,
  including time between tasks. A lone or missing timestamp shows **Not recorded**.
- **Distance:** the full recorded waypoint route using the existing GPS
  accuracy and speed filters. Missing usable coordinates show **Not recorded**.

Importing JSON switches the active analysis source to the imported session.
Authenticated users can choose **Back to account data** to restore their account
selection. Sharing and account exports apply only to account data.

End-user walkthrough of the sidebar and the five main-panel tabs (All tasks, Map, Pictures, Compare Players, Statistics) — including the **share game tracks** flow and the per-track **"Share this track"** button (shown when a single track is selected) — lives in [docs/USER_GUIDE.md](docs/USER_GUIDE.md).

Analyzing the exported track JSON files yourself (R, Python, …)? The complete field-by-field format documentation is the [Track Data Reference](https://github.com/geogami-team/geogami-docs/blob/HEAD/docs/TRACK_DATA_REFERENCE.md) in `geogami-docs`.

## Project layout

```
.
├── app.R                  # Single-file Shiny app (UI + server)
├── www/                   # dashboard.css, dashboard.js and virtual-map assets
├── shiny-server.conf      # Configuration for the bundled shiny-server
├── Dockerfile             # rocker/shiny image with required system + R packages
└── docker-compose.yml     # Local stack
```

`app.R` is intentionally a single file to keep the deployment surface small. Helpers for HTTP calls (`fetch_games_data_from_server`, `api_request`, `api_get`) sit at the top, followed by the UI definition (`page_sidebar(...)`) and a `server <- function(input, output, session)` block.

Run the regression checks from this directory with `Rscript tests/regression.R`.
They exercise Shiny sessions with synthetic tracks and mocked API requests,
including incomplete tracks, missing GPS data, standalone uploads, API failures,
sharing changes, session metrics, stable task selection and source switching.
They do not contact the live API or modify production data.
Check the client script with `node --check www/dashboard.js` and run the
playback checks with `node tests/dashboard-client.test.js`.

## Deployment

The project deploys cleanly to anything that can run a Docker container with port 3838 exposed. Recommended setup:

- Put a TLS-terminating reverse proxy in front (Caddy, nginx, Traefik).
- Pass through `?token=` query parameters untouched.
- Run alongside the GeoGami server so both share the same domain root (cookies aren't used; CORS is not an issue for token-based auth).

Deploy `app.R` together with `www/dashboard.css`, `www/dashboard.js` and the
existing virtual-map assets. The Dockerfile copies all of them. With the supplied
compose mounts, update the host checkout and recreate the container; pulling a
new image alone cannot replace the mounted files. No API or database migration
is required for the dashboard redesign.

## License

MIT — see the parent project for citation information.
