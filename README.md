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

- Browse the games they have access to (their own + games **shared** with them)
- View track sessions for each game and inspect tasks, answers, photos, and a Leaflet map of the route
- Export evaluation data and track maps
- Grant or revoke access to a game's tracks for other GeoGami users (by email)

> Companion projects:
> - **Front-end UI**: [`../geogami-ui`](https://github.com/geogami-team/geogami)
> - **Backend API**: [`../geogami-server`](https://github.com/geogami-team/origami-backend)
> - **Virtual environment**: [`../geogami-virtual-environment-dev`](https://github.com/geogami-team/geogami-virtual-environment-dev)(../geogami-virtual-environment-dev)

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

`docker-compose.yml` mounts `app.R` from the host so iterative changes don't require a rebuild. To force a fresh build:

```bash
docker compose build --no-cache
docker compose up
```

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

End-user walkthrough of the sidebar and the five main-panel tabs (All tasks, Map, Pictures, Compare Players, Statistics) — including the **share game tracks** flow — lives in [docs/USER_GUIDE.md](docs/USER_GUIDE.md).

## Project layout

```
.
├── app.R                  # Single-file Shiny app (UI + server)
├── www/                   # Static assets served by Shiny
├── shiny-server.conf      # Configuration for the bundled shiny-server
├── Dockerfile             # rocker/shiny image with required system + R packages
└── docker-compose.yml     # Local stack
```

`app.R` is intentionally a single file to keep the deployment surface small. Helpers for HTTP calls (`fetch_games_data_from_server`, `api_request`, `api_get`) sit at the top, followed by the UI definition (`page_sidebar(...)`) and a `server <- function(input, output, session)` block.

## Deployment

The project deploys cleanly to anything that can run a Docker container with port 3838 exposed. Recommended setup:

- Put a TLS-terminating reverse proxy in front (Caddy, nginx, Traefik).
- Pass through `?token=` query parameters untouched.
- Run alongside the GeoGami server so both share the same domain root (cookies aren't used; CORS is not an issue for token-based auth).

## License

MIT — see the parent project for citation information.
