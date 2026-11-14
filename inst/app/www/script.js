// What the graph panel needs in the browser: fitting the layout to the panel,
// zoom, the graph and table views, and a sortable table.
window.legviz = (function () {
  function networkIn(panel) {
    var el = panel.find('.visNetwork')[0];
    var widget = el && window.HTMLWidgets && HTMLWidgets.find('#' + el.id);
    return widget && widget.network;
  }

  // The layout comes out about as tall as it is wide. Turn its long side to the
  // panel's, and stretch the short side toward the panel's shape, so the graph
  // fills the panel instead of a narrow strip of it.
  function arrange(network) {
    var edges = network.body.data.edges.get();
    var linked = {};
    edges.forEach(function (e) { linked[e.from] = linked[e.to] = true; });
    var ids = network.body.data.nodes.getIds();
    var core = ids.filter(function (id) { return linked[id]; });
    if (core.length < 3) return;

    var pos = network.getPositions();
    var mx = 0, my = 0;
    core.forEach(function (id) { mx += pos[id].x; my += pos[id].y; });
    mx /= core.length; my /= core.length;

    var sxx = 0, sxy = 0, syy = 0;
    core.forEach(function (id) {
      var dx = pos[id].x - mx, dy = pos[id].y - my;
      sxx += dx * dx; sxy += dx * dy; syy += dy * dy;
    });
    var angle = 0.5 * Math.atan2(2 * sxy, sxx - syy);

    var w = network.body.container.clientWidth, h = network.body.container.clientHeight;
    var turn = (w >= h ? 0 : Math.PI / 2) - angle;
    var cos = Math.cos(turn), sin = Math.sin(turn);
    function rotated(id) {
      var dx = pos[id].x - mx, dy = pos[id].y - my;
      return { x: dx * cos - dy * sin, y: dx * sin + dy * cos };
    }

    var minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
    core.forEach(function (id) {
      var p = rotated(id);
      minX = Math.min(minX, p.x); maxX = Math.max(maxX, p.x);
      minY = Math.min(minY, p.y); maxY = Math.max(maxY, p.y);
    });
    var want = w / h, have = (maxX - minX) / (maxY - minY), fx = 1, fy = 1;
    if (have < want) fx = Math.min(want / have, 1.6); else fy = Math.min(have / want, 1.6);

    var updates = core.map(function (id) {
      var p = rotated(id);
      return { id: id, x: p.x * fx, y: p.y * fy };
    });

    // Anyone with no line goes in a row under the rest, in reach of the view
    var alone = ids.filter(function (id) { return !linked[id]; });
    var gap = (maxX - minX) * fx / Math.max(alone.length + 1, 6);
    alone.forEach(function (id, i) {
      updates.push({
        id: id,
        x: ((minX + maxX) / 2) * fx + (i - (alone.length - 1) / 2) * gap,
        y: maxY * fy + 1.4 * gap
      });
    });
    // moveNode changes only the position; updating the data set would apply the
    // group's colors over the party colors
    updates.forEach(function (u) { network.moveNode(u.id, u.x, u.y); });
  }

  return { arrange: arrange, networkIn: networkIn };
})();

$(document).on('click', '.graph-zoom button', function () {
  var panel = $(this).closest('.legviz-graph');
  var network = legviz.networkIn(panel);
  if (!network) return;
  var action = $(this).data('zoom');
  var animation = { duration: 200, easingFunction: 'easeInOutQuad' };
  if (action === 'fit') network.fit({ animation: animation });
  else network.moveTo({ scale: network.getScale() * (action === 'in' ? 1.4 : 1 / 1.4), animation: animation });
});

$(document).on('click', '.view-toggle button', function () {
  var panel = $(this).closest('.legviz-graph');
  var view = $(this).data('view');
  panel.attr('data-view', view);
  panel.find('.view-toggle button').attr('aria-pressed', function () {
    return $(this).data('view') === view ? 'true' : 'false';
  });
  if (view === 'graph') {
    // The canvas had no size while hidden
    setTimeout(function () {
      var network = legviz.networkIn(panel);
      if (network) { network.redraw(); network.fit(); }
    }, 50);
  }
});

$(document).on('click', '.member-table th button', function () {
  var th = $(this).closest('th');
  var table = th.closest('table');
  var column = th.index();
  var ascending = th.attr('aria-sort') !== 'ascending';
  table.find('thead th').attr('aria-sort', 'none');
  th.attr('aria-sort', ascending ? 'ascending' : 'descending');

  function key(row) {
    var value = $(row).children().eq(column).attr('data-sort');
    var number = parseFloat(value);
    return value === '' || isNaN(number) ? value : number;
  }
  var rows = table.find('tbody tr').get().sort(function (a, b) {
    var x = key(a), y = key(b);
    var order = x < y ? -1 : x > y ? 1 : 0;
    return ascending ? order : -order;
  });
  table.children('tbody').append(rows);
});

$(document).on('click', '.member-link', function () {
  var input = $(this).closest('table').data('input');
  if (input) Shiny.setInputValue(input, $(this).data('id'), { priority: 'event' });
});

// On a narrow screen the card is below the graph, so bring it into view
$(document).on('shiny:value', function (event) {
  if (!/legislator_card$/.test(event.name)) return;
  if (!window.matchMedia('(max-width: 991px)').matches) return;
  setTimeout(function () {
    // The output's own element has no box of its own, so measure the card in it
    var el = document.getElementById(event.name);
    var card = el && el.querySelector('.legislator-card');
    if (card) window.scrollTo({ top: card.getBoundingClientRect().top + window.scrollY - 8 });
  }, 100);
});
