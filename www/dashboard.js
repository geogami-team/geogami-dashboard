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
      if (old.group) (old.map || map).removeLayer(old.group);
      if (old.legend) (old.map || map).removeControl(old.legend);
      delete window.geogamiTrajectoryAnimations[mapId];
    }
    var pauseButton = document.getElementById('dashboard_playback_pause');
    var seek = document.getElementById('dashboard_playback_seek');
    var speed = document.getElementById('dashboard_playback_speed');
    if (pauseButton) { pauseButton.disabled = true; pauseButton.textContent = 'Pause'; }
    if (seek) { seek.disabled = true; seek.value = '0'; }

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

    if (animated.length === 0) { map.removeLayer(group); return; }

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

    var state = { map: map, group: group, legend: legend, fraction: 0, paused: false, lastTick: performance.now() };
    state.draw = function(fraction) {
      state.fraction = Math.max(0, Math.min(1, fraction));
      animated.forEach(function(a) {
        var count = Math.max(1, Math.ceil(state.fraction * a.pts.length));
        var shown = a.pts.slice(0, count);
        a.line.setLatLngs(shown);
        a.movingMarker.setLatLng(shown[shown.length - 1]);
      });
      if (seek) seek.value = String(Math.round(state.fraction * 100));
    };
    state.draw(0);
    if (seek) seek.disabled = false;
    if (pauseButton) { pauseButton.disabled = false; pauseButton.textContent = 'Pause'; }
    state.play = function() {
      clearInterval(state.timer);
      state.paused = false;
      state.lastTick = performance.now();
      state.timer = setInterval(function() {
        var now = performance.now();
        var elapsed = now - state.lastTick;
        state.lastTick = now;
        state.draw(state.fraction + elapsed * Number(speed ? speed.value : 1) / duration);
        if (state.fraction >= 1) {
          state.paused = true;
          clearInterval(state.timer);
          if (pauseButton) pauseButton.textContent = 'Replay';
        }
      }, Math.max(15, duration / steps));
    };
    state.play();
    window.geogamiTrajectoryAnimations[mapId] = state;
  });
})();

// Local presentation behavior; all data and mutations remain in Shiny.
(function () {
  function selectedSessionIds() {
    var select = document.getElementById('selected_files');
    return select ? Array.from(select.selectedOptions, function(option) { return option.value; }) : [];
  }
  function setSelectedSessionIds(ids) {
    var select = document.getElementById('selected_files');
    if (!select) return;
    if (window.jQuery && $.fn && $.fn.selectpicker) {
      $(select).selectpicker('val', ids).trigger('change');
    } else {
      Array.from(select.options).forEach(function(option) { option.selected = ids.includes(option.value); });
      select.dispatchEvent(new Event('change', { bubbles: true }));
    }
  }
  function filterSessionRows() {
    var input = document.getElementById('dashboard_session_search');
    var query = input ? input.value.trim().toLowerCase() : '';
    var rows = document.querySelectorAll('#dashboard_session_list .geo-session-row');
    var matches = 0;
    rows.forEach(function(row) {
      row.hidden = !row.dataset.sessionSearch.toLowerCase().includes(query);
      if (!row.hidden) matches++;
    });
    var message = document.getElementById('dashboard_session_no_results');
    if (message) message.hidden = !rows.length || matches > 0;
  }
  function register() {
    if (!window.Shiny) { window.setTimeout(register, 100); return; }
    Shiny.addCustomMessageHandler('dashboardTheme', function (theme) {
      document.documentElement.dataset.geoTheme = theme;
      window.dispatchEvent(new Event('resize'));
    });
  }
  register();
  document.addEventListener('click', function (event) {
    var importButton = event.target.closest('#dashboard_import');
    if (importButton) {
      var input = document.getElementById('uploaded_json_file');
      if (input) {
        var section = input.closest('.geo-import-details');
        if (section) section.open = true;
        input.click();
      }
    }
    var reference = event.target.closest('.geo-session-name[data-session-id]');
    if (reference) {
      var id = reference.dataset.sessionId;
      var selected = selectedSessionIds();
      if (!selected.includes(id)) setSelectedSessionIds(selected.concat(id));
      Shiny.setInputValue('dashboard_reference_session', id, { priority: 'event' });
    }
    var sessionAction = event.target.closest('[data-session-action]');
    if (sessionAction) {
      var select = document.getElementById('selected_files');
      if (select) setSelectedSessionIds(sessionAction.dataset.sessionAction === 'all'
        ? Array.from(select.options, function(option) { return option.value; }) : []);
    }
    var menu = event.target.closest('.geo-menu');
    document.querySelectorAll('.geo-menu[open]').forEach(function (item) {
      if (item !== menu || (menu && event.target.closest('a, button'))) item.open = false;
    });
  });
  document.addEventListener('change', function(event) {
    if (!event.target.matches('.geo-session-checkbox')) return;
    var id = event.target.value;
    var selected = selectedSessionIds().filter(function(value) { return value !== id; });
    if (event.target.checked) selected.push(id);
    setSelectedSessionIds(selected);
  });
  document.addEventListener('input', function(event) {
    if (event.target.id === 'dashboard_session_search') { filterSessionRows(); return; }
    if (event.target.id !== 'dashboard_playback_seek') return;
    var state = window.geogamiTrajectoryAnimations && window.geogamiTrajectoryAnimations.map;
    if (state && state.draw) {
      state.draw(Number(event.target.value) / 100);
      state.lastTick = performance.now();
      var button = document.getElementById('dashboard_playback_pause');
      if (button && state.paused) button.textContent = state.fraction >= 1 ? 'Replay' : 'Resume';
    }
  });
  if (window.jQuery) $(document).on('shiny:value', function(event) {
    if (event.name === 'dashboard_session_list') setTimeout(filterSessionRows, 0);
  });
  document.addEventListener('click', function(event) {
    if (!event.target.closest('#dashboard_playback_pause')) return;
    var state = window.geogamiTrajectoryAnimations && window.geogamiTrajectoryAnimations.map;
    if (!state) return;
    if (state.fraction >= 1) { state.draw(0); state.play(); }
    else if (state.paused) state.play();
    else { state.paused = true; clearInterval(state.timer); }
    event.target.closest('button').textContent = state.paused ? 'Resume' : 'Pause';
  });
  if (window.jQuery) $(document).on('shiny:value', function(event) {
    if (event.name !== 'map') return;
    var state = window.geogamiTrajectoryAnimations && window.geogamiTrajectoryAnimations.map;
    if (state) {
      clearInterval(state.timer);
      state.map.removeLayer(state.group);
      state.map.removeControl(state.legend);
      delete window.geogamiTrajectoryAnimations.map;
    }
    var pause = document.getElementById('dashboard_playback_pause');
    var seek = document.getElementById('dashboard_playback_seek');
    if (pause) { pause.disabled = true; pause.textContent = 'Pause'; }
    if (seek) { seek.disabled = true; seek.value = '0'; }
  });
  document.addEventListener('keydown', function (event) {
    if (event.key === 'Escape') document.querySelectorAll('.geo-menu[open]').forEach(function (menu) { menu.open = false; });
  });
})();
