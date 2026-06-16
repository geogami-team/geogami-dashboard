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

When you open the dashboard you see two areas:

| Area | What lives there |
|---|---|
| **Sidebar** (left) | Theme, game picker, player picker, share controls |
| **Main panel** (right) | Five tabs for analysis & comparison |

---

## 🎛️ Sidebar

### 1. Choose Theme
Switch between **Light** and **Dark**.

### 2. Upload JSON file *(optional)*
Use this to inspect a single track JSON file directly from disk, without going through the API. Useful for offline analysis or debugging.

### 3. Select your game
*Visible when the dashboard is opened with a `?token=` from the GeoGami front-end.*

- Lists every game you have access to — games you created plus any games **shared** with you.
- Pick one to load its tracks into the dashboard.

### 4. Share game tracks
*Button appears once a game is selected.*

- Opens a dialog where you can grant or revoke access to this game's tracks for other GeoGami users by email.
- Add one or more addresses; the server validates each — it rejects the owner's own email and addresses that don't belong to a registered GeoGami account.
- Recipients see the shared game in their own dashboard the next time they open it.
- Backing endpoints: `POST /game/:id/share`, `DELETE /game/:id/share`, `GET /game/:id/share`.

### 5. Select the players
After picking a game, this dropdown lists every track session (one per player run).

- Choose one or more sessions to load their data into the main-panel tabs.
- Until you load at least one session, the right-hand tabs stay empty.
- **Download** button: exports the selected JSON track files as a zip. The structure of these files is documented field by field in the [Track Data Reference](https://github.com/geogami-team/geogami-docs/blob/HEAD/TRACK_DATA_REFERENCE.md).

> ℹ️ **Note about task numbers:** unlike older versions, there is no longer a single "task number" field in the sidebar. Each tab that needs a task selection has its own **Selected Tasks** picker at the top, so you can compare different tasks across tabs without losing your place.

### 6. Share a single track
*Button appears once **exactly one** track is selected in **Select the players**.*

- Opens a dialog to grant or revoke access to **that one track** for other GeoGami users by email — independent of the whole game.
- Same validation as game sharing: the server rejects the track owner's own email and addresses with no GeoGami account.
- Recipients see the game (with just that track) in their own dashboard the next time they open it.
- The track's owner is its **instructor** for class plays, otherwise the game creator; only the owner or an admin can manage the share.
- Backing endpoints: `POST /track/:id/share`, `DELETE /track/:id/share`, `GET /track/:id/share`.

---

## 📊 Main panel

The main panel has five tabs. The first three analyse loaded sessions individually; the last two compare across sessions.

### 1. All tasks
- **Selected Tasks** picker — narrow the table to one task, or leave it on the default to see every task.
- **File selector** — pick which loaded session to inspect.
- Shows a player-info box (overall score, summary stats) followed by the **Big table** with full per-task details.
- **Save all to csv** under the table exports the full per-player data.

### 2. Map
- **Selected Tasks** picker + **file selector** at the top.
- Shows the player's trajectory and answer locations on a Leaflet map for the chosen task.
- Map legend updates based on the task type (navigation, direction, etc.).
- **Save the map** downloads the current view as an HTML file.

### 3. Pictures
- **Selected Tasks** picker + **file selector** at the top.
- For picture-based tasks, displays the photo the player took, plus the assignment image where applicable.
- Each image has its own **Download** button.
- Tasks that don't produce a photo are greyed out in the task picker.

### 4. Compare Players
- **Selected Tasks** picker + multi-select **file selector** to choose the sessions you want to compare.
- Displays a comparison table that adapts to the task type:
  - Navigation tasks → **Route length versus time**
  - Direction-determination tasks → **Answer and error for direction task**
- **Save to csv** exports the comparison table.

### 5. Statistics
- **Selected Tasks** picker + multi-select **file selector**.
- **Choose graphic to display** — switches between available chart types for the selected task type:
  - Navigation: **Time VS Distance** (scatter) or **Answer & Error** (pie chart)
  - Direction / Free / Self-location / Object-location: **Answer & Error** plot
- **Save to png** under each chart exports the visualization.

---

## 💡 Tips

- The sidebar collapses on tablet/phone widths — tap the toggle in the header to reopen it.
- All tabs share the same loaded sessions, so a session picked in **Select the players** is available everywhere.
- The dashboard reads its JWT from the `?token=` URL parameter. If you opened the page directly without a token, the game/session selectors stay hidden and only the **Upload JSON file** path is available.
