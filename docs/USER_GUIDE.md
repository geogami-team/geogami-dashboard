<p align="center">
  <img src="https://github.com/origami-team/geogami/blob/master/src/assets/icons/icon.png" width="90" alt="GeoGami logo"/>
</p>

<h1 align="center">GeoGami Dashboard — User Guide</h1>

<p align="center">
  A walkthrough of the dashboard from an end-user perspective.<br/>
  For installation, deployment, and developer setup, see the <a href="../README.md">main README</a>.
</p>

---

## 🗺️ Layout at a glance

The dashboard has an action header, a selection sidebar and an analysis area:

| Area | What lives there |
|---|---|
| **Header** | Import JSON, Share and Export data |
| **Sidebar** (left) | Event, game, selected sessions, reference session and appearance |
| **Main panel** (right) | Session summary cards and five analysis tabs |

The summary cards show the **complete reference session**, even when the table
is filtered. Score includes viewed information tasks. Session time runs from the
first to the last recorded event; distance uses the recorded route with GPS
quality filtering. Missing recordings display **Not recorded**.

---

## 🎛️ Sidebar

### 1. Choose Theme
Switch between **Light** and **Dark**.

### 2. Upload JSON file *(optional)*
Choose **Import JSON** in the header, or browse for a file in the sidebar, to
inspect one track directly from disk. Importing switches the dashboard to that
session. Choose **Back to account data** to return to your account selection.

### 3. Select your game
*Visible when the dashboard is opened with a `?token=` from the GeoGami front-end.*

- Lists every game you have access to — games you created plus any games **shared** with you.
- Pick one to load its tracks into the dashboard.

### 4. Share game tracks
*Available under **Share** in the header once a game is selected.*

- Opens a dialog where you can grant or revoke access to this game's tracks for other GeoGami users by email.
- Add one or more addresses; the server validates each — it rejects the owner's own email and addresses that don't belong to a registered GeoGami account.
- Recipients see the shared game in their own dashboard the next time they open it.
- Backing endpoints: `POST /game/:id/share`, `DELETE /game/:id/share`, `GET /game/:id/share`.

### 5. Select the players
After picking a game, **Player sessions** lists each recorded run with a bold
player name and a smaller date/time underneath. Search the list by player name
or date, or use **Select all** / **Clear**.

- Choose one or more sessions to load their data into the main-panel tabs.
- Until you load at least one session, the right-hand tabs stay empty.
- Use the checkboxes to select sessions for comparison and export. Click a player
  name to inspect that session; the highlighted row shows **Viewing**. Clicking
  an unselected name also selects its session. Comparison task numbers follow
  the session being viewed.
- **Export data → Selected sessions JSON** downloads one session as JSON or
  several as a ZIP. The format is documented in the [Track Data Reference](https://github.com/geogami-team/geogami-docs/blob/HEAD/docs/TRACK_DATA_REFERENCE.md).

Task selection is synchronized across Map, Pictures, Compare Players and
Statistics. Selecting a row in All tasks selects that same task for these tabs.

### 6. Share a single track
*Available under **Share** once **exactly one** account session is selected.*

- Opens a dialog to grant or revoke access to **that one track** for other GeoGami users by email — independent of the whole game.
- Same validation as game sharing: the server rejects the track owner's own email and addresses with no GeoGami account.
- Recipients see the game (with just that track) in their own dashboard the next time they open it.
- The track's owner is its **instructor** for class plays, otherwise the game creator; only the owner or an admin can manage the share.
- Backing endpoints: `POST /track/:id/share`, `DELETE /track/:id/share`, `GET /track/:id/share`.

---

## 📊 Main panel

The main panel has five tabs. The first three analyse loaded sessions individually; the last two compare across sessions.

### 1. All tasks
- **Tasks** and **Task type** narrow the table. An empty Tasks selection shows all tasks.
- Search task text, sort columns and use **Columns** to choose visible table fields.
- Result badges distinguish correct, incorrect, viewed and unfinished tasks.
- Select a row to view its full assignment, timing, attempts, error and map preview.
  Rows can also be selected with Enter or Space when focused.
- **Open full map** opens the selected task in Map.
- **Export data → Reference session CSV** exports the current task/type filters
  with full assignment text. **Selected sessions CSV** exports these filters for
  all sessions selected in the sidebar. Table search, sorting and column visibility
  affect the table presentation; CSV retains its analysis columns.

### 2. Map
- **Task** picker and trajectory-session selector at the top.
- Shows the player's trajectory and answer locations on a Leaflet map for the chosen task.
- Map legend updates based on the task type (navigation, direction, etc.).
- **Visualize Trajectories** plays the selected recorded routes. Pause/resume,
  seek through the playback and choose its speed using the playback controls.
- **Export map** downloads the map package with the selected trajectories.

### 3. Pictures
- **Task** picker at the top; the reference session comes from the sidebar.
- For picture-based tasks, displays the photo the player took, plus the assignment image where applicable.
- Each image has its own **Download** button.
- Tasks that don't produce a photo are greyed out in the task picker.

### 4. Compare Players
- **Task** picker and a shared comparison-session selector to choose sessions.
- Displays a comparison table that adapts to the task type:
  - Navigation tasks → **Route length versus time**
  - Direction-determination tasks → **Answer and error for direction task**
- **Export comparison CSV** exports the comparison table.

### 5. Statistics
- **Task** picker and the same comparison-session selection as Compare Players.
- Displays an answer/error donut chart, plus a time/distance scatter plot for
  navigation tasks. Charts adapt to the chosen appearance.
- **Export chart PNG** saves the visualization.
- Imported JSON contains one session, so group comparison and statistics require
  account sessions.

---

## 💡 Tips

- The sidebar collapses on tablet/phone widths — tap the toggle in the header to reopen it.
- All tabs share the same loaded sessions, so a session picked in **Select the players** is available everywhere.
- The dashboard reads its JWT from the `?token=` URL parameter. If you opened the page directly without a token, the game/session selectors stay hidden and only the **Upload JSON file** path is available.
