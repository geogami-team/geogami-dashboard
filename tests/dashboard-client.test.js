// Run with: node tests/dashboard-client.test.js
// Exercise playback state without a browser or external services.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

const handlers = {};
const listeners = {};
const timers = new Map();
let now = 0;
let timerId = 0;
let resizeEvents = 0;
let imported = false;
const inputValues = {};
let legend;
const controls = {};
for (const id of ['dashboard_playback_pause', 'dashboard_playback_seek', 'dashboard_playback_speed', 'dashboard_import', 'uploaded_json_file']) {
  controls[id] = {
    id, disabled: true, value: id === 'dashboard_playback_speed' ? '1' : '0',
    closest(selector) { return selector === '#' + id || selector === 'button' ? this : null; },
    click() { imported = true; }
  };
}
const map = {
  layers: new Set(), controls: new Set(),
  removeLayer(layer) { this.layers.delete(layer); },
  removeControl(control) { this.controls.delete(control); },
  fitBounds() {}
};
const layer = () => ({
  addTo(group) { group.items.push(this); return this; },
  setLatLng(value) { this.point = value; },
  setLatLngs(value) { this.points = value; }
});
const document = {
  documentElement: { dataset: {} },
  getElementById(id) { return controls[id]; },
  querySelectorAll(selector) { return selector === '#dashboard_session_list .geo-session-row' ? sessionRows : []; },
  addEventListener(type, fn) { (listeners[type] ||= []).push(fn); }
};
const context = vm.createContext({
  document, performance: { now: () => now }, console, Event: class { constructor(type) { this.type = type; } },
  setTimeout() { throw new Error('Client dependencies should be ready'); },
  setInterval(fn) { timers.set(++timerId, fn); return timerId; },
  clearInterval(id) { timers.delete(id); },
  Shiny: { addCustomMessageHandler(name, fn) { handlers[name] = fn; }, setInputValue(name, value) { inputValues[name] = value; } },
  HTMLWidgets: { find: () => ({ getMap: () => map }) },
  L: {
    layerGroup() { return { items: [], addTo(target) { target.layers.add(this); return this; } }; },
    circleMarker: layer, polyline: layer, latLngBounds: value => value,
    control() {
      const control = { addTo(target) { target.controls.add(this); legend = this.onAdd(); } };
      return control;
    },
    DomUtil: { create: () => ({}) },
    DomEvent: { disableClickPropagation() {}, disableScrollPropagation() {} }
  },
  $: () => ({ on(type, fn) { (listeners[type] ||= []).push(fn); } })
});
context.window = context;
context.jQuery = true;
context.dispatchEvent = () => { resizeEvents++; };
const sessionRows = [
  { dataset: { sessionSearch: 'Example 14 Oct 2026 track1' }, hidden: false },
  { dataset: { sessionSearch: 'Other 14 Oct 2026 track2' }, hidden: false }
];
controls.selected_files = {
  options: [{ value: 'track1', selected: true }, { value: 'track2', selected: false }],
  get selectedOptions() { return this.options.filter(option => option.selected); },
  dispatchEvent() {}
};
controls.dashboard_session_search = { id: 'dashboard_session_search', value: '' };
controls.dashboard_session_no_results = { hidden: true };
vm.runInContext(fs.readFileSync(path.join(__dirname, '../www/dashboard.js'), 'utf8'), context);

const emit = (type, target, extra = {}) => listeners[type].forEach(fn => fn({ target, ...extra }));
const tick = ms => { now += ms; [...timers.values()].forEach(fn => fn()); };
const start = () => handlers.animateTrajectories({ duration: 1000, steps: 10, trajectories: [
  { lat: [51, 51.001, 51.002], lng: [7, 7.001, 7.002], color: '#006f77', label: '<img src=x onerror=alert(1)>' }
] });

start();
let state = context.geogamiTrajectoryAnimations.map;
tick(500);
assert.equal(state.fraction, 0.5);
assert.equal(controls.dashboard_playback_seek.disabled, false);
assert.equal(controls.dashboard_playback_pause.disabled, false);
assert.ok(legend.innerHTML.includes('&lt;img'));
assert.ok(!legend.innerHTML.includes('<img'));
console.log('PASS: Playback advances through recorded points and escapes session labels');

emit('click', controls.dashboard_playback_pause);
tick(2000);
assert.equal(state.fraction, 0.5);
assert.equal(state.paused, true);
assert.equal(timers.size, 0);
controls.dashboard_playback_seek.value = '75';
emit('input', controls.dashboard_playback_seek);
assert.equal(state.fraction, 0.75);
controls.dashboard_playback_speed.value = '2';
emit('click', controls.dashboard_playback_pause);
tick(100);
assert.ok(Math.abs(state.fraction - 0.95) < 1e-8);
tick(100);
assert.equal(state.fraction, 1);
assert.equal(timers.size, 0);
assert.equal(controls.dashboard_playback_pause.textContent, 'Replay');
emit('click', controls.dashboard_playback_pause);
assert.equal(state.fraction, 0);
assert.equal(state.paused, false);
console.log('PASS: Pause, seek, speed, completion and replay work without idle timers');

handlers.animateTrajectories({ trajectories: [] });
assert.equal(context.geogamiTrajectoryAnimations.map, undefined);
assert.equal(timers.size, 0);
assert.equal(map.layers.size, 0);
assert.equal(map.controls.size, 0);
assert.equal(controls.dashboard_playback_seek.disabled, true);
console.log('PASS: Clearing trajectories removes playback and disables controls');

handlers.animateTrajectories({ trajectories: [{ lat: [51, 'invalid'], lng: [7, 7.001] }] });
assert.equal(context.geogamiTrajectoryAnimations.map, undefined);
assert.equal(map.layers.size, 0);
console.log('PASS: Incomplete trajectories leave no playback layers');

start();
emit('shiny:value', null, { name: 'map' });
assert.equal(context.geogamiTrajectoryAnimations.map, undefined);
assert.equal(timers.size, 0);
assert.equal(map.layers.size, 0);
assert.equal(map.controls.size, 0);
console.log('PASS: Changing the map disposes the previous animation');

handlers.dashboardTheme('dark');
assert.equal(document.documentElement.dataset.geoTheme, 'dark');
assert.equal(resizeEvents, 1);
emit('click', controls.dashboard_import);
assert.equal(imported, true);
console.log('PASS: Theme and import controls update the actual page controls');

const referenceButton = {
  dataset: { sessionId: 'track2' },
  closest(selector) { return selector === '.geo-session-name[data-session-id]' ? this : null; }
};
emit('click', referenceButton);
assert.deepEqual(controls.selected_files.selectedOptions.map(option => option.value), ['track1', 'track2']);
assert.equal(inputValues.dashboard_reference_session, 'track2');
emit('change', { value: 'track1', checked: false, matches: selector => selector === '.geo-session-checkbox' });
assert.deepEqual(controls.selected_files.selectedOptions.map(option => option.value), ['track2']);
for (const [action, expected] of [['all', ['track1', 'track2']], ['none', []]]) {
  emit('click', { dataset: { sessionAction: action }, closest(selector) { return selector === '[data-session-action]' ? this : null; } });
  assert.deepEqual(controls.selected_files.selectedOptions.map(option => option.value), expected);
}
console.log('PASS: Session names, checkboxes and bulk controls retain the existing selection binding');

controls.dashboard_session_search.value = ' OTHER ';
emit('input', controls.dashboard_session_search);
assert.equal(sessionRows[0].hidden, true);
assert.equal(sessionRows[1].hidden, false);
assert.equal(controls.dashboard_session_no_results.hidden, true);
controls.dashboard_session_search.value = 'missing';
emit('input', controls.dashboard_session_search);
assert.equal(controls.dashboard_session_no_results.hidden, false);
assert.deepEqual(controls.selected_files.selectedOptions.map(option => option.value), []);
console.log('PASS: Session search filters rows without changing the selected sessions');
console.log('8 client checks passed.');
