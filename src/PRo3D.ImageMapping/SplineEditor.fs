namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

module SplineEditor =

    open System.Globalization

    let private fmt (v : float) =
        v.ToString("0.#####", CultureInfo.InvariantCulture)

    let private serialize (spline : TransferSpline) =
        let pts =
            spline.points
            |> List.map (fun p -> sprintf "[%s,%s,%s]" (fmt p.px) (fmt p.py) (fmt p.slope))
            |> String.concat ","

        let samples = 128

        let curve =
            [ 0 .. samples - 1 ]
            |> List.map (fun i ->
                let x = float i / float (samples - 1)
                sprintf "[%s,%s]" (fmt x) (fmt (TransferSpline.evaluate spline x)))
            |> String.concat ","

        sprintf "{\"pts\":[%s],\"curve\":[%s]}" pts curve

    let private num (s : string) =
        Double.Parse(s.Trim('"'), CultureInfo.InvariantCulture)

    // kind: 0 move point, 1 set tangent slope, 2 add point, 3 remove point, 4 flatten tangent
    let private messageOfEvent (args : list<string>) : Message =
        try
            match args with
            | [ kind; index; a; b ] ->
                let i = int (num index)

                match int (num kind) with
                | 0 -> SplineMovePoint (i, num a, num b)
                | 1 -> SplineSetTangent (i, num a)
                | 2 -> SplineAddPoint (num a, num b)
                | 3 -> SplineRemovePoint i
                | 4 -> SplineResetTangent i
                | _ -> Nop
            | _ -> Nop
        with _ ->
            Nop

    let private script =
        """
var root = $('#__ID__')[0];
var cv = document.createElement('canvas');
cv.width = 360; cv.height = 240;
cv.style.width = '100%'; cv.style.display = 'block';
cv.style.touchAction = 'none'; cv.style.cursor = 'crosshair';
root.appendChild(cv);
var ctx = cv.getContext('2d');
var W = cv.width, H = cv.height, PAD = 26, HANDLE = 40, PR = 6, HR = 5;
var state = { pts: [], curve: [] };
var drag = null, hover = null, lastSent = 0;

function toPx(x, y) { return [PAD + x * (W - 2 * PAD), H - PAD - y * (H - 2 * PAD)]; }
function toData(px, py) { return [(px - PAD) / (W - 2 * PAD), (H - PAD - py) / (H - 2 * PAD)]; }

function handles(p) {
  var c = toPx(p[0], p[1]);
  var dx = W - 2 * PAD, dy = -p[2] * (H - 2 * PAD);
  var l = Math.sqrt(dx * dx + dy * dy);
  var ux = dx / l, uy = dy / l;
  return { c: c, r: [c[0] + HANDLE * ux, c[1] + HANDLE * uy], l: [c[0] - HANDLE * ux, c[1] - HANDLE * uy] };
}

function draw() {
  ctx.fillStyle = '#161616'; ctx.fillRect(0, 0, W, H);
  ctx.strokeStyle = 'rgba(255,255,255,0.12)'; ctx.lineWidth = 1;
  ctx.fillStyle = '#888'; ctx.font = '10px sans-serif';
  for (var i = 0; i <= 4; i++) {
    var v = i / 4;
    var a = toPx(v, 0), b = toPx(v, 1), c = toPx(0, v), d = toPx(1, v);
    ctx.beginPath(); ctx.moveTo(a[0], a[1]); ctx.lineTo(b[0], b[1]); ctx.stroke();
    ctx.beginPath(); ctx.moveTo(c[0], c[1]); ctx.lineTo(d[0], d[1]); ctx.stroke();
    ctx.fillText(v.toFixed(2), a[0] - 10, H - PAD + 14);
    ctx.fillText(v.toFixed(2), 2, c[1] + 3);
  }

  ctx.strokeStyle = '#4fc3f7'; ctx.lineWidth = 2; ctx.beginPath();
  for (var k = 0; k < state.curve.length; k++) {
    var q = toPx(state.curve[k][0], state.curve[k][1]);
    if (k == 0) ctx.moveTo(q[0], q[1]); else ctx.lineTo(q[0], q[1]);
  }
  ctx.stroke();

  for (var j = 0; j < state.pts.length; j++) {
    var h = handles(state.pts[j]);
    var active = (drag && drag.idx == j) || (hover && hover.idx == j);
    ctx.strokeStyle = active ? '#ffb74d' : '#9e9e9e'; ctx.lineWidth = 1;
    ctx.beginPath(); ctx.moveTo(h.l[0], h.l[1]); ctx.lineTo(h.r[0], h.r[1]); ctx.stroke();
    ctx.fillStyle = '#ffb74d';
    [h.l, h.r].forEach(function (e) { ctx.beginPath(); ctx.arc(e[0], e[1], HR, 0, 2 * Math.PI); ctx.fill(); });
    ctx.fillStyle = active ? '#ffffff' : '#e0e0e0';
    ctx.beginPath(); ctx.arc(h.c[0], h.c[1], PR, 0, 2 * Math.PI); ctx.fill();
  }
}

function pos(e) {
  var r = cv.getBoundingClientRect();
  return [(e.clientX - r.left) * W / r.width, (e.clientY - r.top) * H / r.height];
}

function hit(p) {
  var best = null, bd = 1e9;
  for (var i = 0; i < state.pts.length; i++) {
    var h = handles(state.pts[i]);
    var cands = [['p', h.c, PR + 4], ['r', h.r, HR + 4], ['l', h.l, HR + 4]];
    for (var k = 0; k < cands.length; k++) {
      var d = Math.sqrt(Math.pow(p[0] - cands[k][1][0], 2) + Math.pow(p[1] - cands[k][1][1], 2));
      if (d <= cands[k][2] && d < bd) { bd = d; best = { idx: i, part: cands[k][0] }; }
    }
  }
  return best;
}

function send(kind, idx, a, b) { aardvark.processEvent('__ID__', 'onsplineedit', kind, idx, a, b); }

function slopeFrom(part, p, d) {
  var dx = part == 'r' ? d[0] - p[0] : p[0] - d[0];
  var dy = part == 'r' ? d[1] - p[1] : p[1] - d[1];
  return dy / Math.max(dx, 0.005);
}

function sendDrag(p) {
  var d = toData(p[0], p[1]);
  if (drag.part == 'p') send(0, drag.idx, d[0], d[1]);
  else { var pt = state.pts[drag.idx]; if (pt) send(1, drag.idx, slopeFrom(drag.part, pt, d), 0); }
}

cv.addEventListener('pointerdown', function (e) {
  if (e.button !== 0) return;
  var h = hit(pos(e));
  if (h) { drag = h; cv.setPointerCapture(e.pointerId); e.preventDefault(); draw(); }
});

cv.addEventListener('pointermove', function (e) {
  var p = pos(e);
  if (!drag) {
    var h = hit(p);
    var changed = (h ? h.idx : -1) != (hover ? hover.idx : -1);
    hover = h;
    cv.style.cursor = h ? 'pointer' : 'crosshair';
    if (changed) draw();
    return;
  }
  var now = Date.now();
  if (now - lastSent < 30) return;
  lastSent = now;
  sendDrag(p);
});

cv.addEventListener('pointerup', function (e) {
  if (drag) { sendDrag(pos(e)); drag = null; draw(); }
});
cv.addEventListener('pointercancel', function () { drag = null; draw(); });

cv.addEventListener('dblclick', function (e) {
  var p = pos(e), h = hit(p);
  if (h && h.part != 'p') send(4, h.idx, 0, 0);
  else if (!h) {
    var d = toData(p[0], p[1]);
    if (d[0] >= 0 && d[0] <= 1 && d[1] >= 0 && d[1] <= 1) send(2, 0, d[0], d[1]);
  }
});

cv.addEventListener('contextmenu', function (e) {
  e.preventDefault();
  var h = hit(pos(e));
  if (h && h.part == 'p') send(3, h.idx, 0, 0);
});

function load() {
  var s = root.getAttribute('data-spline');
  if (!s) return;
  try { state = JSON.parse(s); } catch (err) { return; }
  draw();
}
new MutationObserver(load).observe(root, { attributes: true, attributeFilter: ['data-spline'] });
load();
draw();
"""

    let view (spline : aval<TransferSpline>) : DomNode<Message> =
        onBoot script (
            Incremental.div
                (AttributeMap.ofAMap (amap {
                    yield style "width: 100%; margin-top: 10px;"
                    yield onEvent "onsplineedit" [] messageOfEvent
                    let! s = spline
                    yield attribute "data-spline" (serialize s)
                }))
                AList.empty
        )