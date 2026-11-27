//! Figures: 2D curves and 3D surfaces/curves, rendered as Plotly JSON
//! (interactive in JupyterLab), SVG (static) and text (terminal).

use std::fmt::Write;

pub const SAMPLES: usize = 400;
pub const CURVE3D_SAMPLES: usize = 600;
pub const GRID: usize = 41;

const PALETTE: [&str; 8] = [
    "#2563eb", "#dc2626", "#16a34a", "#9333ea", "#ea580c", "#0891b2", "#be185d", "#4d7c0f",
];

// ---------- JSON helpers ----------

pub fn jstr(s: &str) -> String {
    let mut o = String::with_capacity(s.len() + 2);
    o.push('"');
    for c in s.chars() {
        match c {
            '"' => o.push_str("\\\""),
            '\\' => o.push_str("\\\\"),
            '\n' => o.push_str("\\n"),
            '\r' => o.push_str("\\r"),
            '\t' => o.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                let _ = write!(o, "\\u{:04x}", c as u32);
            }
            c => o.push(c),
        }
    }
    o.push('"');
    o
}

fn jnum(d: f64) -> String {
    if d.is_finite() {
        // compact but exact enough for plotting
        let s = format!("{:.6e}", d);
        let v: f64 = s.parse().unwrap_or(d);
        let plain = format!("{}", v);
        if plain.len() <= s.len() { plain } else { s }
    } else {
        "null".into()
    }
}

fn jnums<'a>(it: impl IntoIterator<Item = &'a f64>) -> String {
    let v: Vec<String> = it.into_iter().map(|d| jnum(*d)).collect();
    format!("[{}]", v.join(","))
}

/// Numbers for large point clouds, to 5 significant digits (keeps notebooks small)
fn short_nums(v: &[f64], short: bool) -> String {
    if !short {
        return jnums(v);
    }
    let parts: Vec<String> = v
        .iter()
        .map(|d| {
            if d.is_finite() {
                format!("{}", format!("{:.4e}", d).parse::<f64>().unwrap_or(*d))
            } else {
                "null".into()
            }
        })
        .collect();
    format!("[{}]", parts.join(","))
}

fn obj(fields: &[(&str, String)]) -> String {
    let v: Vec<String> = fields
        .iter()
        .map(|(k, v)| format!("{}:{}", jstr(k), v))
        .collect();
    format!("{{{}}}", v.join(","))
}

fn esc(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

fn finite(d: f64) -> bool {
    d.is_finite()
}

/// Number for axis ticks
fn fmt_tick(d: f64) -> String {
    if d == 0.0 {
        "0".into()
    } else if d.abs() >= 1e5 || d.abs() < 1e-3 {
        format!("{:.2e}", d)
    } else {
        let s = format!("{:.4}", d);
        let s = s.trim_end_matches('0');
        s.trim_end_matches('.').to_string()
    }
}

/// "Nice" tick positions (1, 2, 5 x 10^k)
pub(crate) fn ticks(lo: f64, hi: f64, target: usize) -> Vec<f64> {
    let raw = (hi - lo) / target as f64;
    if raw <= 0.0 || !raw.is_finite() {
        return vec![];
    }
    let mag = 10f64.powf(raw.log10().floor());
    let step = [1.0, 2.0, 5.0, 10.0]
        .iter()
        .map(|m| m * mag)
        .find(|s| *s >= raw)
        .unwrap_or(10.0 * mag);
    let mut t = (lo / step).ceil() * step;
    let mut out = vec![];
    while t <= hi + step * 1e-9 && out.len() < 100 {
        out.push(if t.abs() < step * 1e-9 { 0.0 } else { t });
        t += step;
    }
    out
}

// ---------- 2D ----------

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Series {
    pub label: String,
    pub points: Vec<(f64, f64)>,
    /// draw points (data) instead of a line
    pub markers: bool,
    /// vertical error bars, one per point (or none)
    pub errors: Vec<f64>,
    /// a fixed color instead of the palette's (contour levels)
    pub color: Option<String>,
    /// thin lines (contours, arrows)
    pub thin: bool,
    /// fill each closed run (histogram and bar chart bars)
    pub fill: bool,
}

impl Series {
    fn col(&self, i: usize) -> &str {
        self.color.as_deref().unwrap_or(PALETTE[i % PALETTE.len()])
    }
    fn width(&self) -> &'static str {
        if self.thin { "1.3" } else { "2" }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Figure {
    pub series: Vec<Series>,
    pub x_label: String,
    pub y_label: String,
    pub title: String,
    pub equal_aspect: bool,
    /// fixed view [x0, x1] x [y0, y1], set by window(p, ...)
    pub window: Option<(f64, f64, f64, f64)>,
    /// a picture under the curves (domain coloring)
    pub image: Option<Raster>,
    /// an animation: frame k gives new points for some series (the series
    /// themselves hold the first frame)
    pub frames: Vec<Frame>,
}

/// One frame of an animation: its label (t = 0.5) and (series index, points)
#[derive(Clone, Debug, PartialEq)]
pub struct Frame {
    pub label: String,
    pub series: Vec<(usize, Vec<(f64, f64)>)>,
}

/// RGB pixels covering [x0, x1] x [y0, y1], top row first
#[derive(Clone, Debug, PartialEq)]
pub struct Raster {
    pub w: usize,
    pub h: usize,
    pub rgb: Vec<u8>,
    pub x0: f64,
    pub x1: f64,
    pub y0: f64,
    pub y1: f64,
}

impl Raster {
    /// a data: URI with the pixels as a PNG
    pub fn png_uri(&self) -> String {
        format!("data:image/png;base64,{}", base64(&png_bytes(self.w, self.h, &self.rgb)))
    }
}

/// RGB pixels (top row first) as a PNG file
pub fn png_bytes(w: usize, h: usize, rgb: &[u8]) -> Vec<u8> {
        fn crc32(data: &[u8]) -> u32 {
            let mut c = 0xffff_ffffu32;
            for &b in data {
                c ^= b as u32;
                for _ in 0..8 {
                    c = if c & 1 == 1 { 0xedb8_8320 ^ (c >> 1) } else { c >> 1 };
                }
            }
            !c
        }
        fn chunk(out: &mut Vec<u8>, kind: &[u8], data: &[u8]) {
            out.extend_from_slice(&(data.len() as u32).to_be_bytes());
            let mut body = kind.to_vec();
            body.extend_from_slice(data);
            out.extend_from_slice(&body);
            out.extend_from_slice(&crc32(&body).to_be_bytes());
        }
        // filter each row by its difference from the row above (smooth pictures give small values)
        let stride = w * 3;
        let mut raw = Vec::with_capacity(h * (stride + 1));
        for (j, row) in rgb.chunks(stride).enumerate() {
            if j == 0 {
                raw.push(0);
                raw.extend_from_slice(row);
            } else {
                raw.push(2);
                let up = &rgb[(j - 1) * stride..j * stride];
                raw.extend(row.iter().zip(up).map(|(a, b)| a.wrapping_sub(*b)));
            }
        }
        let mut z = vec![0x78, 0x01];
        z.extend(deflate_fixed(&raw));
        let (mut a, mut bb) = (1u32, 0u32);
        for &c in &raw {
            a = (a + c as u32) % 65521;
            bb = (bb + a) % 65521;
        }
        z.extend_from_slice(&((bb << 16) | a).to_be_bytes());
        let mut png = vec![0x89, b'P', b'N', b'G', 0x0d, 0x0a, 0x1a, 0x0a];
        let mut ihdr = vec![];
        ihdr.extend_from_slice(&(w as u32).to_be_bytes());
        ihdr.extend_from_slice(&(h as u32).to_be_bytes());
        ihdr.extend_from_slice(&[8, 2, 0, 0, 0]);
        chunk(&mut png, b"IHDR", &ihdr);
        chunk(&mut png, b"IDAT", &z);
        chunk(&mut png, b"IEND", &[]);
        png
}

/// deflate with fixed Huffman codes and greedy LZ77 matching
fn deflate_fixed(data: &[u8]) -> Vec<u8> {
    struct Bits {
        out: Vec<u8>,
        acc: u64,
        n: u32,
    }
    impl Bits {
        fn put(&mut self, v: u32, n: u32) {
            self.acc |= (v as u64) << self.n;
            self.n += n;
            while self.n >= 8 {
                self.out.push(self.acc as u8);
                self.acc >>= 8;
                self.n -= 8;
            }
        }
        /// a Huffman code, most significant bit first
        fn code(&mut self, c: u32, len: u32) {
            let mut r = 0;
            for i in 0..len {
                r |= ((c >> i) & 1) << (len - 1 - i);
            }
            self.put(r, len);
        }
        fn lit(&mut self, v: u32) {
            match v {
                0..=143 => self.code(0x30 + v, 8),
                144..=255 => self.code(0x190 + v - 144, 9),
                256..=279 => self.code(v - 256, 7),
                _ => self.code(0xc0 + v - 280, 8),
            }
        }
    }
    const LB: [u32; 29] = [3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258];
    const LE: [u32; 29] = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0];
    const DB: [u32; 30] = [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577];
    const DE: [u32; 30] = [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13];
    let mut b = Bits { out: vec![], acc: 0, n: 0 };
    b.put(1, 1);
    b.put(1, 2);
    let n = data.len();
    let mut head = vec![usize::MAX; 1 << 15];
    let mut prev = vec![usize::MAX; n];
    let hash = |i: usize| ((data[i] as usize) << 10 ^ (data[i + 1] as usize) << 5 ^ data[i + 2] as usize) & 0x7fff;
    let mut i = 0;
    while i < n {
        let (mut best, mut dist) = (0usize, 0usize);
        if i + 2 < n {
            let h = hash(i);
            let mut j = head[h];
            let mut tries = 0;
            while j != usize::MAX && i - j <= 32768 && tries < 48 {
                let mut l = 0;
                while l < 258 && i + l < n && data[j + l] == data[i + l] {
                    l += 1;
                }
                if l > best {
                    best = l;
                    dist = i - j;
                }
                j = prev[j];
                tries += 1;
            }
        }
        let step = if best >= 3 {
            let k = LB.iter().rposition(|&x| x as usize <= best).unwrap();
            b.lit(257 + k as u32);
            b.put(best as u32 - LB[k], LE[k]);
            let d = DB.iter().rposition(|&x| x as usize <= dist).unwrap();
            b.code(d as u32, 5);
            b.put(dist as u32 - DB[d], DE[d]);
            best
        } else {
            b.lit(data[i] as u32);
            1
        };
        for p in i..i + step {
            if p + 2 < n {
                let h = hash(p);
                prev[p] = head[h];
                head[h] = p;
            }
        }
        i += step;
    }
    b.lit(256);
    if b.n > 0 {
        b.out.push(b.acc as u8);
    }
    b.out
}

pub(crate) fn base64(data: &[u8]) -> String {
    const T: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = String::with_capacity(data.len() * 4 / 3 + 4);
    for c in data.chunks(3) {
        let n = (c[0] as u32) << 16 | (*c.get(1).unwrap_or(&0) as u32) << 8 | *c.get(2).unwrap_or(&0) as u32;
        out.push(T[(n >> 18) as usize & 63] as char);
        out.push(T[(n >> 12) as usize & 63] as char);
        out.push(if c.len() > 1 { T[(n >> 6) as usize & 63] as char } else { '=' });
        out.push(if c.len() > 2 { T[n as usize & 63] as char } else { '=' });
    }
    out
}

struct Bounds {
    x0: f64,
    x1: f64,
    y0: f64,
    y1: f64,
}

impl Figure {
    pub fn new(series: Vec<Series>, x_label: &str, y_label: &str, equal_aspect: bool) -> Figure {
        Figure {
            series,
            x_label: x_label.into(),
            y_label: y_label.into(),
            title: String::new(),
            equal_aspect,
            window: None,
            image: None,
            frames: vec![],
        }
    }

    pub fn overlay(&self, o: &Figure) -> Figure {
        let pick = |a: &String, b: &String| if a.is_empty() { b.clone() } else { a.clone() };
        Figure {
            series: self.series.iter().chain(&o.series).cloned().collect(),
            x_label: pick(&self.x_label, &o.x_label),
            y_label: pick(&self.y_label, &o.y_label),
            title: pick(&self.title, &o.title),
            equal_aspect: self.equal_aspect || o.equal_aspect,
            window: self.window.or(o.window),
            image: self.image.clone().or_else(|| o.image.clone()),
            frames: {
                // frames of both, side by side; a shorter animation holds its last frame
                let n = self.frames.len().max(o.frames.len());
                let off = self.series.len();
                (0..n)
                    .map(|k| {
                        let a = self.frames.get(k.min(self.frames.len().saturating_sub(1)));
                        let b = o.frames.get(k.min(o.frames.len().saturating_sub(1)));
                        let mut series = a.map(|f| f.series.clone()).unwrap_or_default();
                        if let Some(f) = b {
                            series.extend(f.series.iter().map(|(i, p)| (i + off, p.clone())));
                        }
                        let label = self
                            .frames
                            .get(k)
                            .or(o.frames.get(k))
                            .map(|f| f.label.clone())
                            .unwrap_or_default();
                        Frame { label, series }
                    })
                    .collect()
            },
        }
    }

    /// Data bounds, ignoring poles: when a few points blow up (tan, 1/x), clip to the bulk
    fn bounds(&self) -> Bounds {
        if let Some((x0, x1, y0, y1)) = self.window {
            return Bounds { x0, x1, y0, y1 };
        }
        let mut pts: Vec<(f64, f64)> = self
            .series
            .iter()
            .flat_map(|s| s.points.iter().copied())
            .filter(|p| finite(p.0) && finite(p.1))
            .collect();
        if let Some(r) = &self.image {
            pts.extend([(r.x0, r.y0), (r.x1, r.y1)]);
        }
        for f in &self.frames {
            for (_, p) in &f.series {
                pts.extend(p.iter().copied().filter(|p| finite(p.0) && finite(p.1)));
            }
        }
        for s in &self.series {
            for (p, e) in s.points.iter().zip(&s.errors) {
                if finite(p.0) && finite(p.1) && finite(*e) {
                    pts.push((p.0, p.1 + e));
                    pts.push((p.0, p.1 - e));
                }
            }
        }
        if pts.is_empty() {
            return Bounds {
                x0: -1.0,
                x1: 1.0,
                y0: -1.0,
                y1: 1.0,
            };
        }
        let mut ys: Vec<f64> = pts.iter().map(|p| p.1).collect();
        ys.sort_by(|a, b| a.total_cmp(b));
        let q = |f: f64| ys[((f * (ys.len() - 1) as f64).round() as usize).min(ys.len() - 1)];
        let (lo, hi) = (ys[0], ys[ys.len() - 1]);
        let (q1, q9) = (q(0.02), q(0.98));
        let core = q9 - q1;
        let (mut y0, mut y1) = if core > 0.0 && hi - lo > 20.0 * core {
            (q1 - core * 0.5, q9 + core * 0.5)
        } else {
            (lo, hi)
        };
        // clipping is for curves running off to a pole; data points always stay visible
        for s in self.series.iter().filter(|s| s.markers) {
            for &(x, y) in &s.points {
                if finite(x) && finite(y) {
                    y0 = y0.min(y);
                    y1 = y1.max(y);
                }
            }
        }
        let (ya, yb) = if y1 - y0 < 1e-12 * y0.abs().max(1.0) {
            (y0 - 1.0, y1 + 1.0)
        } else {
            (y0, y1)
        };
        let pad = (yb - ya) * 0.05;
        let xa = pts.iter().map(|p| p.0).fold(f64::INFINITY, f64::min);
        let xb = pts.iter().map(|p| p.0).fold(f64::NEG_INFINITY, f64::max);
        let (xa, xb) = if xb - xa < 1e-12 * xa.abs().max(1.0) {
            (xa - 1.0, xb + 1.0)
        } else {
            (xa, xb)
        };
        if !self.equal_aspect {
            return Bounds {
                x0: xa,
                x1: xb,
                y0: ya - pad,
                y1: yb + pad,
            };
        }
        // same units per pixel on both axes (the SVG plot area is 560 x 336)
        let ratio = 336.0 / 560.0;
        let xp = (xb - xa) * 0.05;
        let (w, h) = (xb - xa + 2.0 * xp, yb - ya + 2.0 * pad);
        let (cx, cy) = ((xa + xb) / 2.0, (ya + yb) / 2.0);
        if h / w > ratio {
            let w2 = h / ratio;
            Bounds {
                x0: cx - w2 / 2.0,
                x1: cx + w2 / 2.0,
                y0: cy - h / 2.0,
                y1: cy + h / 2.0,
            }
        } else {
            let h2 = w * ratio;
            Bounds {
                x0: cx - w / 2.0,
                x1: cx + w / 2.0,
                y0: cy - h2 / 2.0,
                y1: cy + h2 / 2.0,
            }
        }
    }

    /// Drawable runs: breaks at NaN/infinity and at poles crossing the visible range
    fn runs(&self, s: &Series, b: &Bounds) -> Vec<Vec<(f64, f64)>> {
        let mut out = vec![];
        let mut cur: Vec<(f64, f64)> = vec![];
        let mut prev: Option<f64> = None;
        for &(x, y) in &s.points {
            let ok = finite(x) && finite(y);
            let jump =
                prev.is_some_and(|p| ok && ((p > b.y1 && y < b.y0) || (p < b.y0 && y > b.y1)));
            if (!ok || jump) && !cur.is_empty() {
                out.push(std::mem::take(&mut cur));
            }
            if ok {
                cur.push((x, y));
                prev = Some(y);
            } else {
                prev = None;
            }
        }
        if !cur.is_empty() {
            out.push(cur);
        }
        out
    }

    pub fn plotly(&self) -> String {
        let b = self.bounds();
        let traces: Vec<String> = self
            .series
            .iter()
            .enumerate()
            .map(|(i, s)| {
                let rs = self.runs(s, &b);
                let mut xs = vec![];
                let mut ys = vec![];
                for r in rs {
                    xs.extend(r.iter().map(|p| p.0));
                    ys.extend(r.iter().map(|p| p.1));
                    xs.push(f64::NAN);
                    ys.push(f64::NAN);
                }
                if s.markers {
                    let pts: Vec<&(f64, f64)> = s.points.iter().collect();
                    let mut fields = vec![
                        ("type", jstr("scatter")),
                        ("mode", jstr("markers")),
                        ("name", jstr(&s.label)),
                        (
                            "x",
                            short_nums(
                                &pts.iter().map(|p| p.0).collect::<Vec<_>>(),
                                pts.len() > 1500,
                            ),
                        ),
                        (
                            "y",
                            short_nums(
                                &pts.iter().map(|p| p.1).collect::<Vec<_>>(),
                                pts.len() > 1500,
                            ),
                        ),
                        (
                            "marker",
                            obj(&[
                                ("color", jstr(s.col(i))),
                                ("size", if pts.len() > 1500 { "2" } else { "8" }.into()),
                            ]),
                        ),
                    ];
                    if !s.errors.is_empty() {
                        fields.push((
                            "error_y",
                            obj(&[
                                ("type", jstr("data")),
                                ("array", jnums(&s.errors)),
                                ("visible", "true".into()),
                            ]),
                        ));
                    }
                    return obj(&fields);
                }
                if s.fill {
                    return obj(&[
                        ("type", jstr("scatter")),
                        ("mode", jstr("lines")),
                        ("fill", jstr("toself")),
                        ("name", jstr(&s.label)),
                        ("x", jnums(&xs)),
                        ("y", jnums(&ys)),
                        ("line", obj(&[("color", jstr(s.col(i))), ("width", "1.5".into())])),
                    ]);
                }
                obj(&[
                    ("type", jstr("scatter")),
                    ("mode", jstr("lines")),
                    ("name", jstr(&s.label)),
                    ("x", jnums(&xs)),
                    ("y", jnums(&ys)),
                    (
                        "line",
                        obj(&[
                            ("color", jstr(s.col(i))),
                            ("width", s.width().into()),
                        ]),
                    ),
                ])
            })
            .collect();
        let mut yaxis = vec![
            ("range", jnums(&[b.y0, b.y1])),
            ("zeroline", "true".into()),
            ("title", obj(&[("text", jstr(&self.y_label))])),
        ];
        if self.equal_aspect {
            yaxis.push(("scaleanchor", jstr("x")));
            yaxis.push(("scaleratio", "1".into()));
        }
        let grid = obj(&[("gridcolor", jstr("#e5e7eb"))]);
        let layout = obj(&[
            ("title", obj(&[("text", jstr(&self.title))])),
            (
                "xaxis",
                obj(&[
                    ("title", obj(&[("text", jstr(&self.x_label))])),
                    ("zeroline", "true".into()),
                ]),
            ),
            ("yaxis", obj(&yaxis)),
            ("showlegend", "true".into()),
            ("legend", {
                let (w, h) = self.legend_size();
                let corner = self.legend_corner(&b, (w + 8.0) / 560.0, (h + 8.0) / 336.0);
                let (x, xa) = if corner % 2 == 0 {
                    ("0.01", "left")
                } else {
                    ("0.99", "right")
                };
                let (y, ya) = if corner < 2 {
                    ("0.99", "top")
                } else {
                    ("0.01", "bottom")
                };
                obj(&[
                    ("x", x.into()),
                    ("y", y.into()),
                    ("xanchor", jstr(xa)),
                    ("yanchor", jstr(ya)),
                    ("bgcolor", jstr("rgba(255,255,255,0.8)")),
                ])
            }),
            (
                "margin",
                obj(&[
                    ("l", "60".into()),
                    ("r", "20".into()),
                    ("t", if self.title.is_empty() { "20" } else { "50" }.into()),
                    ("b", if self.frames.is_empty() { "50" } else { "140" }.into()),
                ]),
            ),
            ("width", "700".into()),
            ("height", "440".into()),
            (
                "template",
                obj(&[(
                    "layout",
                    obj(&[
                        ("plot_bgcolor", jstr("white")),
                        ("xaxis", grid.clone()),
                        ("yaxis", grid),
                    ]),
                )]),
            ),
        ]);
        let layout = match &self.image {
            Some(r) => {
                let img = obj(&[
                    ("source", jstr(&r.png_uri())),
                    ("xref", jstr("x")),
                    ("yref", jstr("y")),
                    ("x", jnum(r.x0)),
                    ("y", jnum(r.y1)),
                    ("sizex", jnum(r.x1 - r.x0)),
                    ("sizey", jnum(r.y1 - r.y0)),
                    ("sizing", jstr("stretch")),
                    ("layer", jstr("below")),
                ]);
                format!("{},\"images\":[{}]}}", &layout[..layout.len() - 1], img)
            }
            None => layout,
        };
        if self.frames.is_empty() {
            return obj(&[
                ("data", format!("[{}]", traces.join(","))),
                ("layout", layout),
            ]);
        }
        // an animation: fixed axes, a play button and a slider
        let frames: Vec<String> = self
            .frames
            .iter()
            .enumerate()
            .map(|(k, f)| {
                let data: Vec<String> = f
                    .series
                    .iter()
                    .map(|(i, p)| {
                        let mut xs = vec![];
                        let mut ys = vec![];
                        match self.series.get(*i) {
                            Some(ser) if !ser.markers => {
                                let ghost = Series { points: p.clone(), ..ser.clone() };
                                for r in self.runs(&ghost, &b) {
                                    xs.extend(r.iter().map(|q| q.0));
                                    ys.extend(r.iter().map(|q| q.1));
                                    xs.push(f64::NAN);
                                    ys.push(f64::NAN);
                                }
                            }
                            _ => {
                                xs.extend(p.iter().map(|q| q.0));
                                ys.extend(p.iter().map(|q| q.1));
                            }
                        }
                        obj(&[("x", jnums(&xs)), ("y", jnums(&ys))])
                    })
                    .collect();
                let idx: Vec<String> = f.series.iter().map(|(i, _)| i.to_string()).collect();
                obj(&[
                    ("name", jstr(&k.to_string())),
                    ("data", format!("[{}]", data.join(","))),
                    ("traces", format!("[{}]", idx.join(","))),
                ])
            })
            .collect();
        let quick = obj(&[
            ("mode", jstr("immediate")),
            ("frame", obj(&[("duration", "0".into()), ("redraw", "false".into())])),
            ("transition", obj(&[("duration", "0".into())])),
        ]);
        let play = obj(&[
            ("fromcurrent", "true".into()),
            ("frame", obj(&[("duration", "40".into()), ("redraw", "false".into())])),
            ("transition", obj(&[("duration", "0".into())])),
        ]);
        let buttons = format!(
            "[{}]",
            [
                obj(&[("label", jstr("Play")), ("method", jstr("animate")), ("args", format!("[null,{}]", play))]),
                obj(&[("label", jstr("Pause")), ("method", jstr("animate")), ("args", format!("[[null],{}]", quick))]),
            ]
            .join(",")
        );
        let steps: Vec<String> = self
            .frames
            .iter()
            .enumerate()
            .map(|(k, f)| {
                obj(&[
                    ("label", jstr(&f.label)),
                    ("method", jstr("animate")),
                    ("args", format!("[[{}],{}]", jstr(&k.to_string()), quick)),
                ])
            })
            .collect();
        let extra = format!(
            ",\"xaxis\":{},\"updatemenus\":[{}],\"sliders\":[{}]",
            obj(&[
                ("range", jnums(&[b.x0, b.x1])),
                ("autorange", "false".into()),
                ("title", obj(&[("text", jstr(&self.x_label))])),
                ("zeroline", "true".into()),
            ]),
            obj(&[
                ("type", jstr("buttons")),
                ("showactive", "false".into()),
                ("x", "0".into()),
                ("y", "-0.18".into()),
                ("xanchor", jstr("left")),
                ("yanchor", jstr("top")),
                ("direction", jstr("left")),
                ("buttons", buttons),
            ]),
            obj(&[
                ("x", "0.18".into()),
                ("len", "0.82".into()),
                ("y", "-0.12".into()),
                // many steps: only the current value is written out
                ("font", obj(&[("color", jstr("rgba(0,0,0,0)"))])),
                ("currentvalue", obj(&[("visible", "true".into()), ("font", obj(&[("color", jstr("#374151"))]))])),
                ("steps", format!("[{}]", steps.join(","))),
            ])
        );
        // the layout's own xaxis is replaced: JSON keeps the last key
        let layout = format!("{}{}}}", &layout[..layout.len() - 1], extra);
        let layout = layout.replacen("\"height\":440", "\"height\":520", 1);
        obj(&[
            ("data", format!("[{}]", traces.join(","))),
            ("layout", layout),
            ("frames", format!("[{}]", frames.join(","))),
        ])
    }

    /// Which corner of the plot area the legend covers least data in:
    /// 0 top left, 1 top right, 2 bottom left, 3 bottom right. The box size is
    /// given as a fraction of the plot area.
    fn legend_corner(&self, b: &Bounds, fw: f64, fh: f64) -> usize {
        let mut count = [0usize; 4];
        for ser in &self.series {
            for &(x, y) in &ser.points {
                if !(finite(x) && finite(y)) {
                    continue;
                }
                let u = (x - b.x0) / (b.x1 - b.x0);
                let v = (y - b.y0) / (b.y1 - b.y0);
                let (left, right, top, bottom) = (u <= fw, u >= 1.0 - fw, v >= 1.0 - fh, v <= fh);
                count[0] += (left && top) as usize;
                count[1] += (right && top) as usize;
                count[2] += (left && bottom) as usize;
                count[3] += (right && bottom) as usize;
            }
        }
        // prefer the top left unless another corner is clearly emptier
        let mut best = 0;
        for k in 1..4 {
            if count[k] < count[best] && count[k] + 2 < count[0] {
                best = k;
            }
        }
        best
    }

    /// The legend box in the SVG: width and height in pixels
    fn legend_size(&self) -> (f64, f64) {
        let w = 46
            + 7 * self
                .series
                .iter()
                .map(|s| s.label.chars().count())
                .max()
                .unwrap_or(0);
        (w as f64, (18 * self.series.len() + 2) as f64)
    }

    pub fn svg(&self) -> String {
        let (width, height) = (640.0, 400.0);
        let b = self.bounds();
        let (ml, mr, mt, mb) = (
            60.0,
            20.0,
            if self.title.is_empty() { 20.0 } else { 40.0 },
            44.0,
        );
        let (pw, ph) = (width - ml - mr, height - mt - mb);
        let sx = |x: f64| ml + (x - b.x0) / (b.x1 - b.x0) * pw;
        let sy = |y: f64| mt + (1.0 - (y - b.y0) / (b.y1 - b.y0)) * ph;
        let mut s = String::new();
        let _ = write!(
            s,
            r#"<svg xmlns="http://www.w3.org/2000/svg" width="640" height="400" viewBox="0 0 640 400" font-family="sans-serif" font-size="12"><rect width="640" height="400" fill="white"/><clipPath id="plotarea"><rect x="{:.2}" y="{:.2}" width="{:.2}" height="{:.2}"/></clipPath>"#,
            ml, mt, pw, ph
        );
        for t in ticks(b.x0, b.x1, 6) {
            let x = sx(t);
            let _ = write!(
                s,
                r##"<line x1="{x:.2}" y1="{:.2}" x2="{x:.2}" y2="{:.2}" stroke="#e5e7eb"/><text x="{x:.2}" y="{:.2}" text-anchor="middle" fill="#374151">{}</text>"##,
                mt,
                mt + ph,
                mt + ph + 16.0,
                fmt_tick(t)
            );
        }
        for t in ticks(b.y0, b.y1, 6) {
            let y = sy(t);
            let _ = write!(
                s,
                r##"<line x1="{:.2}" y1="{y:.2}" x2="{:.2}" y2="{y:.2}" stroke="#e5e7eb"/><text x="{:.2}" y="{y:.2}" dy="4" text-anchor="end" fill="#374151">{}</text>"##,
                ml,
                ml + pw,
                ml - 6.0,
                fmt_tick(t)
            );
        }
        if b.x0 < 0.0 && b.x1 > 0.0 {
            let _ = write!(
                s,
                r##"<line x1="{0:.2}" y1="{1:.2}" x2="{0:.2}" y2="{2:.2}" stroke="#9ca3af"/>"##,
                sx(0.0),
                mt,
                mt + ph
            );
        }
        if b.y0 < 0.0 && b.y1 > 0.0 {
            let _ = write!(
                s,
                r##"<line x1="{1:.2}" y1="{0:.2}" x2="{2:.2}" y2="{0:.2}" stroke="#9ca3af"/>"##,
                sy(0.0),
                ml,
                ml + pw
            );
        }
        let _ = write!(
            s,
            r##"<rect x="{:.2}" y="{:.2}" width="{:.2}" height="{:.2}" fill="none" stroke="#6b7280"/>"##,
            ml, mt, pw, ph
        );
        if let Some(r) = &self.image {
            let _ = write!(
                s,
                r#"<image clip-path="url(#plotarea)" x="{:.2}" y="{:.2}" width="{:.2}" height="{:.2}" preserveAspectRatio="none" href="{}"/>"#,
                sx(r.x0),
                sy(r.y1),
                sx(r.x1) - sx(r.x0),
                sy(r.y0) - sy(r.y1),
                r.png_uri()
            );
        }
        s.push_str(r#"<g clip-path="url(#plotarea)" fill="none" stroke-width="2" stroke-linejoin="round">"#);
        let span = b.y1 - b.y0;
        // an animation on paper: later frames drawn faintly under the first
        let n = self.frames.len();
        if n > 3 {
            for k in [n / 3, 2 * n / 3, n - 1] {
                for (i, pts) in &self.frames[k].series {
                    let Some(ser) = self.series.get(*i) else { continue };
                    let c = ser.col(*i);
                    if ser.markers {
                        for &(x, y) in pts.iter().filter(|p| finite(p.0) && finite(p.1)) {
                            let _ = write!(
                                s,
                                r#"<circle cx="{:.2}" cy="{:.2}" r="4" fill="{c}" fill-opacity="0.35" stroke="none"/>"#,
                                sx(x),
                                sy(y)
                            );
                        }
                    } else {
                        let ghost = Series { points: pts.clone(), ..ser.clone() };
                        let mut d = String::new();
                        for run in self.runs(&ghost, &b) {
                            for (k, (x, y)) in run.iter().enumerate() {
                                let yy = y.clamp(b.y0 - span, b.y1 + span);
                                let _ = write!(d, "{}{:.2},{:.2}", if k == 0 { "M" } else { "L" }, sx(*x), sy(yy));
                            }
                        }
                        let _ = write!(s, r#"<path d="{d}" stroke="{c}" stroke-opacity="0.3" stroke-width="{}"/>"#, ser.width());
                    }
                }
            }
        }
        for (i, ser) in self.series.iter().enumerate() {
            if ser.markers && ser.points.len() > 1500 {
                // many points (a fractal, a bifurcation diagram): tiny squares in one path
                let c = ser.col(i);
                let mut d = String::new();
                for &(x, y) in &ser.points {
                    if finite(x) && finite(y) {
                        let _ = write!(d, "M{:.1},{:.1}h2v2h-2z", sx(x) - 1.0, sy(y) - 1.0);
                    }
                }
                let _ = write!(s, r#"<path d="{d}" fill="{c}" stroke="none"/>"#);
                continue;
            }
            if ser.markers {
                let c = ser.col(i);
                for (k, &(x, y)) in ser.points.iter().enumerate() {
                    if !(finite(x) && finite(y)) {
                        continue;
                    }
                    if let Some(e) = ser.errors.get(k).filter(|e| finite(**e) && **e > 0.0) {
                        let (xx, y0, y1) = (sx(x), sy(y - e), sy(y + e));
                        let _ = write!(
                            s,
                            r#"<path d="M{xx:.2},{y0:.2}L{xx:.2},{y1:.2}M{:.2},{y0:.2}L{:.2},{y0:.2}M{:.2},{y1:.2}L{:.2},{y1:.2}" stroke="{c}" stroke-width="1.5"/>"#,
                            xx - 4.0,
                            xx + 4.0,
                            xx - 4.0,
                            xx + 4.0
                        );
                    }
                    let _ = write!(
                        s,
                        r#"<circle cx="{:.2}" cy="{:.2}" r="4" fill="{c}" stroke="white" stroke-width="1"/>"#,
                        sx(x),
                        sy(y)
                    );
                }
                continue;
            }
            if ser.fill {
                let mut d = String::new();
                for run in self.runs(ser, &b) {
                    for (k, (x, y)) in run.iter().enumerate() {
                        let yy = y.clamp(b.y0 - span, b.y1 + span);
                        let _ = write!(d, "{}{:.2},{:.2}", if k == 0 { "M" } else { "L" }, sx(*x), sy(yy));
                    }
                    d.push('Z');
                }
                let c = ser.col(i);
                let _ = write!(s, r#"<path d="{d}" fill="{c}" fill-opacity="0.35" stroke="{c}" stroke-width="1.5"/>"#);
                continue;
            }
            let mut d = String::new();
            for run in self.runs(ser, &b) {
                for (k, (x, y)) in run.iter().enumerate() {
                    let yy = y.clamp(b.y0 - span, b.y1 + span);
                    let _ = write!(
                        d,
                        "{}{:.2},{:.2}",
                        if k == 0 { "M" } else { "L" },
                        sx(*x),
                        sy(yy)
                    );
                }
            }
            let _ = write!(
                s,
                r#"<path d="{}" stroke="{}" stroke-width="{}"/>"#,
                d,
                ser.col(i),
                ser.width()
            );
        }
        s.push_str("</g>");
        if !self.title.is_empty() {
            let _ = write!(
                s,
                r##"<text x="{:.2}" y="24" text-anchor="middle" font-size="14" fill="#111827">{}</text>"##,
                ml + pw / 2.0,
                esc(&self.title)
            );
        }
        if !self.x_label.is_empty() {
            let _ = write!(
                s,
                r##"<text x="{:.2}" y="{:.2}" text-anchor="middle" fill="#111827">{}</text>"##,
                ml + pw / 2.0,
                height - 8.0,
                esc(&self.x_label)
            );
        }
        if !self.y_label.is_empty() {
            let y = mt + ph / 2.0;
            let _ = write!(
                s,
                r##"<text x="14" y="{y:.2}" text-anchor="middle" fill="#111827" transform="rotate(-90 14 {y:.2})">{}</text>"##,
                esc(&self.y_label)
            );
        }
        if !self.series.is_empty() {
            let (w, h) = self.legend_size();
            let corner = self.legend_corner(&b, (w + 8.0) / pw, (h + 8.0) / ph);
            let lx = if corner % 2 == 0 {
                ml + 4.0
            } else {
                ml + pw - 4.0 - w
            };
            let ly = if corner < 2 {
                mt + 4.0
            } else {
                mt + ph - 4.0 - h
            };
            let _ = write!(
                s,
                r##"<rect x="{:.2}" y="{:.2}" width="{}" height="{}" fill="white" fill-opacity="0.85" stroke="#e5e7eb"/>"##,
                lx, ly, w, h
            );
            for (i, ser) in self.series.iter().enumerate() {
                let y = ly + 10.0 + i as f64 * 18.0;
                let c = ser.col(i);
                if ser.markers {
                    let _ = write!(
                        s,
                        r#"<circle cx="{:.2}" cy="{y:.2}" r="4" fill="{c}"/>"#,
                        lx + 16.0
                    );
                } else {
                    let _ = write!(
                        s,
                        r#"<line x1="{:.2}" y1="{y:.2}" x2="{:.2}" y2="{y:.2}" stroke="{c}" stroke-width="2"/>"#,
                        lx + 6.0,
                        lx + 26.0
                    );
                }
                let _ = write!(
                    s,
                    r##"<text x="{:.2}" y="{y:.2}" dy="4" fill="#111827" paint-order="stroke" stroke="white" stroke-width="3">{}</text>"##,
                    lx + 32.0,
                    esc(&ser.label)
                );
            }
        }
        s.push_str("</svg>");
        s
    }

    /// A small text rendering for terminals
    pub fn ascii(&self) -> String {
        let (width, height) = (64usize, 18usize);
        let marks: Vec<char> = "*+ox#@%&".chars().collect();
        let b = self.bounds();
        let mut grid = vec![vec![' '; width]; height];
        let col = |x: f64| ((x - b.x0) / (b.x1 - b.x0) * (width - 1) as f64).round() as i64;
        let row =
            |y: f64| ((1.0 - (y - b.y0) / (b.y1 - b.y0)) * (height - 1) as f64).round() as i64;
        if b.y0 < 0.0 && b.y1 > 0.0 {
            let r0 = row(0.0) as usize;
            grid[r0].iter_mut().for_each(|c| *c = '-');
        }
        if b.x0 < 0.0 && b.x1 > 0.0 {
            let c0 = col(0.0) as usize;
            for r in grid.iter_mut() {
                r[c0] = if r[c0] == '-' { '+' } else { '|' };
            }
        }
        for (i, s) in self.series.iter().enumerate() {
            for &(x, y) in s.points.iter().filter(|p| finite(p.0) && finite(p.1)) {
                let (c, r) = (col(x), row(y));
                if c >= 0 && (c as usize) < width && r >= 0 && (r as usize) < height {
                    grid[r as usize][c as usize] = marks[i % marks.len()];
                }
            }
        }
        let (top, bot) = (fmt_tick(b.y1), fmt_tick(b.y0));
        let yl = top.len().max(bot.len());
        let mut lines: Vec<String> = grid
            .iter()
            .enumerate()
            .map(|(i, line)| {
                let label = if i == 0 {
                    top.as_str()
                } else if i == height - 1 {
                    bot.as_str()
                } else {
                    ""
                };
                format!("{:>yl$} |{}", label, line.iter().collect::<String>())
            })
            .collect();
        lines.push(format!("{} +{}", " ".repeat(yl), "-".repeat(width)));
        let (xl, xr) = (fmt_tick(b.x0), fmt_tick(b.x1));
        lines.push(format!(
            "{}{}{}{}",
            " ".repeat(yl + 2),
            xl,
            " ".repeat(width.saturating_sub(xl.len() + xr.len()).max(1)),
            xr
        ));
        for (i, s) in self.series.iter().enumerate() {
            lines.push(format!("  {} {}", marks[i % marks.len()], s.label));
        }
        lines.join("\n")
    }
}

// ---------- 3D ----------

type P3 = (f64, f64, f64);

#[derive(Clone, Debug, PartialEq)]
pub struct Surface {
    pub label: String,
    pub x: Vec<Vec<f64>>,
    pub y: Vec<Vec<f64>>,
    pub z: Vec<Vec<f64>>,
}

impl Surface {
    fn rows(&self) -> usize {
        self.x.len()
    }
    fn cols(&self) -> usize {
        self.x.first().map_or(0, |r| r.len())
    }
    fn at(&self, i: usize, j: usize) -> P3 {
        (self.x[i][j], self.y[i][j], self.z[i][j])
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Curve {
    pub label: String,
    pub points: Vec<P3>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Figure3D {
    pub surfaces: Vec<Surface>,
    pub curves: Vec<Curve>,
    pub title: String,
    pub labels: [String; 3],
    pub equal_aspect: bool,
}

const SURFACE_SCALES: [&str; 5] = ["Viridis", "Reds", "Blues", "Greens", "Purples"];
const CURVE_COLORS: [&str; 6] = [
    "#dc2626", "#f59e0b", "#2563eb", "#16a34a", "#9333ea", "#0f172a",
];
const SOLID: [P3; 4] = [
    (220.0, 38.0, 38.0),
    (37.0, 99.0, 235.0),
    (22.0, 163.0, 74.0),
    (147.0, 51.0, 234.0),
];

fn finite3(p: &P3) -> bool {
    finite(p.0) && finite(p.1) && finite(p.2)
}

fn viridis(t: f64) -> P3 {
    let stops = [
        (68.0, 1.0, 84.0),
        (59.0, 82.0, 139.0),
        (33.0, 145.0, 140.0),
        (94.0, 201.0, 98.0),
        (253.0, 231.0, 37.0),
    ];
    let t = if t.is_finite() {
        t.clamp(0.0, 1.0)
    } else {
        0.5
    } * (stops.len() - 1) as f64;
    let i = (t as usize).min(stops.len() - 2);
    let f = t - i as f64;
    let (a, b) = (stops[i], stops[i + 1]);
    (
        a.0 + (b.0 - a.0) * f,
        a.1 + (b.1 - a.1) * f,
        a.2 + (b.2 - a.2) * f,
    )
}

fn rgb(c: P3, k: f64) -> String {
    let ch = |v: f64| ((v * k).round()).clamp(0.0, 255.0) as u8;
    format!("#{:02x}{:02x}{:02x}", ch(c.0), ch(c.1), ch(c.2))
}

fn sub3(a: P3, b: P3) -> P3 {
    (a.0 - b.0, a.1 - b.1, a.2 - b.2)
}
fn dot3(a: P3, b: P3) -> f64 {
    a.0 * b.0 + a.1 * b.1 + a.2 * b.2
}
fn cross3(a: P3, b: P3) -> P3 {
    (
        a.1 * b.2 - a.2 * b.1,
        a.2 * b.0 - a.0 * b.2,
        a.0 * b.1 - a.1 * b.0,
    )
}

impl Figure3D {
    pub fn new(
        surfaces: Vec<Surface>,
        curves: Vec<Curve>,
        labels: [&str; 3],
        equal_aspect: bool,
    ) -> Figure3D {
        Figure3D {
            surfaces,
            curves,
            title: String::new(),
            labels: labels.map(|s| s.to_string()),
            equal_aspect,
        }
    }

    pub fn overlay(&self, o: &Figure3D) -> Figure3D {
        Figure3D {
            surfaces: self.surfaces.iter().chain(&o.surfaces).cloned().collect(),
            curves: self.curves.iter().chain(&o.curves).cloned().collect(),
            title: if self.title.is_empty() {
                o.title.clone()
            } else {
                self.title.clone()
            },
            labels: self.labels.clone(),
            equal_aspect: self.equal_aspect || o.equal_aspect,
        }
    }

    pub fn text(&self) -> String {
        let mut parts: Vec<String> = self
            .surfaces
            .iter()
            .map(|s| format!("surface {} ({}x{} grid)", s.label, s.rows(), s.cols()))
            .collect();
        parts.extend(
            self.curves
                .iter()
                .map(|c| format!("curve {} ({} points)", c.label, c.points.len())),
        );
        format!(
            "3D figure: {}\n(shown as a 3D graphic in Jupyter notebooks)",
            parts.join("; ")
        )
    }

    fn points(&self) -> Vec<P3> {
        let mut v = vec![];
        for s in &self.surfaces {
            for i in 0..s.rows() {
                for j in 0..s.cols() {
                    v.push(s.at(i, j));
                }
            }
        }
        for c in &self.curves {
            v.extend(c.points.iter().copied());
        }
        v.retain(finite3);
        v
    }

    fn bounds(&self) -> [(f64, f64); 3] {
        let pts = self.points();
        let b = |f: &dyn Fn(&P3) -> f64| {
            if pts.is_empty() {
                return (-1.0, 1.0);
            }
            let lo = pts.iter().map(f).fold(f64::INFINITY, f64::min);
            let hi = pts.iter().map(f).fold(f64::NEG_INFINITY, f64::max);
            if hi - lo < 1e-12 {
                (lo - 1.0, hi + 1.0)
            } else {
                (lo, hi)
            }
        };
        [b(&|p| p.0), b(&|p| p.1), b(&|p| p.2)]
    }

    pub fn plotly(&self) -> String {
        let grid =
            |g: &Vec<Vec<f64>>| format!("[{}]", g.iter().map(jnums).collect::<Vec<_>>().join(","));
        let mut traces: Vec<String> = self
            .surfaces
            .iter()
            .enumerate()
            .map(|(i, s)| {
                obj(&[
                    ("type", jstr("surface")),
                    ("name", jstr(&s.label)),
                    ("x", grid(&s.x)),
                    ("y", grid(&s.y)),
                    ("z", grid(&s.z)),
                    ("colorscale", jstr(SURFACE_SCALES[i % SURFACE_SCALES.len()])),
                    ("showscale", "false".into()),
                    (
                        "opacity",
                        if self.curves.is_empty() { "1" } else { "0.85" }.into(),
                    ),
                    ("showlegend", "true".into()),
                ])
            })
            .collect();
        traces.extend(self.curves.iter().enumerate().map(|(i, c)| {
            obj(&[
                ("type", jstr("scatter3d")),
                ("mode", jstr("lines")),
                ("name", jstr(&c.label)),
                (
                    "x",
                    jnums(&c.points.iter().map(|p| p.0).collect::<Vec<_>>()),
                ),
                (
                    "y",
                    jnums(&c.points.iter().map(|p| p.1).collect::<Vec<_>>()),
                ),
                (
                    "z",
                    jnums(&c.points.iter().map(|p| p.2).collect::<Vec<_>>()),
                ),
                (
                    "line",
                    obj(&[
                        ("color", jstr(CURVE_COLORS[i % CURVE_COLORS.len()])),
                        ("width", "6".into()),
                    ]),
                ),
            ])
        }));
        let axis = |t: &str| {
            obj(&[
                ("title", obj(&[("text", jstr(t))])),
                ("backgroundcolor", jstr("#f8fafc")),
                ("gridcolor", jstr("#e2e8f0")),
                ("showbackground", "true".into()),
            ])
        };
        let layout = obj(&[
            ("title", obj(&[("text", jstr(&self.title))])),
            (
                "scene",
                obj(&[
                    ("xaxis", axis(&self.labels[0])),
                    ("yaxis", axis(&self.labels[1])),
                    ("zaxis", axis(&self.labels[2])),
                    (
                        "aspectmode",
                        jstr(if self.equal_aspect { "data" } else { "cube" }),
                    ),
                    (
                        "camera",
                        obj(&[(
                            "eye",
                            obj(&[
                                ("x", "1.6".into()),
                                ("y", "-1.6".into()),
                                ("z", "1.0".into()),
                            ]),
                        )]),
                    ),
                ]),
            ),
            ("showlegend", "true".into()),
            ("legend", obj(&[("x", "0.01".into()), ("y", "0.99".into())])),
            (
                "margin",
                obj(&[
                    ("l", "0".into()),
                    ("r", "0".into()),
                    ("t", if self.title.is_empty() { "10" } else { "50" }.into()),
                    ("b", "0".into()),
                ]),
            ),
            ("width", "700".into()),
            ("height", "560".into()),
        ]);
        obj(&[
            ("data", format!("[{}]", traces.join(","))),
            ("layout", layout),
        ])
    }

    /// Static rendering: a shaded, depth-sorted projection
    pub fn svg(&self) -> String {
        let (width, height) = (640.0f64, 520.0f64);
        let bounds = self.bounds();
        let halves = bounds.map(|(a, b)| (b - a) / 2.0);
        let centers = bounds.map(|(a, b)| (a + b) / 2.0);
        let hmax = halves.iter().cloned().fold(0.0, f64::max);
        let scales = if self.equal_aspect { [hmax; 3] } else { halves };
        let norm = |p: P3| {
            (
                (p.0 - centers[0]) / scales[0],
                (p.1 - centers[1]) / scales[1],
                (p.2 - centers[2]) / scales[2],
            )
        };
        let (ex, ey, ez) = (
            halves[0] / scales[0],
            halves[1] / scales[1],
            halves[2] / scales[2],
        );
        let (az, el) = (35f64.to_radians(), 25f64.to_radians());
        let project = |p: P3| -> P3 {
            let x1 = p.0 * az.cos() - p.1 * az.sin();
            let y1 = p.0 * az.sin() + p.1 * az.cos();
            (
                x1,
                p.2 * el.cos() + y1 * el.sin(),
                y1 * el.cos() - p.2 * el.sin(),
            )
        };
        let mut corners = vec![];
        for sx in [-1.0, 1.0] {
            for sy in [-1.0, 1.0] {
                for sz in [-1.0, 1.0] {
                    corners.push((sx * ex, sy * ey, sz * ez));
                }
            }
        }
        let pc: Vec<P3> = corners.iter().map(|c| project(*c)).collect();
        let min_x = pc.iter().map(|p| p.0).fold(f64::INFINITY, f64::min);
        let max_x = pc.iter().map(|p| p.0).fold(f64::NEG_INFINITY, f64::max);
        let min_y = pc.iter().map(|p| p.1).fold(f64::INFINITY, f64::min);
        let max_y = pc.iter().map(|p| p.1).fold(f64::NEG_INFINITY, f64::max);
        let top = if self.title.is_empty() { 20.0 } else { 44.0 };
        let s = ((width - 120.0) / (max_x - min_x)).min((height - top - 50.0) / (max_y - min_y));
        let ox = width / 2.0 - s * (min_x + max_x) / 2.0;
        let oy = top + (height - top - 30.0) / 2.0 + s * (min_y + max_y) / 2.0;
        let scr = |p: P3| {
            let q = project(p);
            (ox + s * q.0, oy - s * q.1)
        };
        let mut out = String::new();
        let _ = write!(
            out,
            r#"<svg xmlns="http://www.w3.org/2000/svg" width="640" height="520" viewBox="0 0 640 520" font-family="sans-serif" font-size="12"><rect width="640" height="520" fill="white"/>"#
        );
        for (i, a) in corners.iter().enumerate() {
            for b in corners.iter().skip(i + 1) {
                let diff = [a.0 != b.0, a.1 != b.1, a.2 != b.2]
                    .iter()
                    .filter(|x| **x)
                    .count();
                if diff == 1 {
                    let (p, q) = (scr(*a), scr(*b));
                    let _ = write!(
                        out,
                        r##"<line x1="{:.1}" y1="{:.1}" x2="{:.1}" y2="{:.1}" stroke="#cbd5e1"/>"##,
                        p.0, p.1, q.0, q.1
                    );
                }
            }
        }
        let mut prims: Vec<(f64, String)> = vec![];
        let l = (-0.4f64, -0.6f64, 0.7f64);
        let ln = dot3(l, l).sqrt();
        let light = (l.0 / ln, l.1 / ln, l.2 / ln);
        let (z0, z1) = bounds[2];
        for (si, surf) in self.surfaces.iter().enumerate() {
            for i in 0..surf.rows().saturating_sub(1) {
                for j in 0..surf.cols().saturating_sub(1) {
                    let raw = [
                        surf.at(i, j),
                        surf.at(i + 1, j),
                        surf.at(i + 1, j + 1),
                        surf.at(i, j + 1),
                    ];
                    if !raw.iter().all(finite3) {
                        continue;
                    }
                    let q = raw.map(norm);
                    let depth = q.iter().map(|p| project(*p).2).sum::<f64>() / 4.0;
                    let nrm = cross3(sub3(q[2], q[0]), sub3(q[3], q[1]));
                    let len = dot3(nrm, nrm).sqrt();
                    let shade = if len < 1e-12 {
                        0.8
                    } else {
                        0.55 + 0.45 * (dot3(nrm, light) / len).abs()
                    };
                    let zmean = raw.iter().map(|p| p.2).sum::<f64>() / 4.0;
                    let base = if si == 0 {
                        viridis((zmean - z0) / (z1 - z0))
                    } else {
                        SOLID[(si - 1) % SOLID.len()]
                    };
                    let fill = rgb(base, shade);
                    let pts: Vec<String> = q
                        .iter()
                        .map(|p| {
                            let (a, b) = scr(*p);
                            format!("{:.1},{:.1}", a, b)
                        })
                        .collect();
                    prims.push((
                        depth,
                        format!(r#"<polygon points="{}" fill="{fill}" stroke="{fill}" stroke-width="0.6"/>"#, pts.join(" ")),
                    ));
                }
            }
        }
        for (ci, c) in self.curves.iter().enumerate() {
            let color = CURVE_COLORS[ci % CURVE_COLORS.len()];
            for w in c.points.windows(2) {
                if !(finite3(&w[0]) && finite3(&w[1])) {
                    continue;
                }
                let (na, nb) = (norm(w[0]), norm(w[1]));
                let depth = (project(na).2 + project(nb).2) / 2.0 - 0.06;
                let (p, q) = (scr(na), scr(nb));
                prims.push((
                    depth,
                    format!(
                        r#"<line x1="{:.1}" y1="{:.1}" x2="{:.1}" y2="{:.1}" stroke="{color}" stroke-width="2.5" stroke-linecap="round"/>"#,
                        p.0, p.1, q.0, q.1
                    ),
                ));
            }
        }
        prims.sort_by(|a, b| b.0.total_cmp(&a.0));
        for p in prims {
            out.push_str(&p.1);
        }
        // axis labels
        let fmt = |d: f64| {
            let v = if d.abs() < 1e-9 { 0.0 } else { d };
            if v.abs() >= 1e4 || (v != 0.0 && v.abs() < 1e-2) {
                format!("{:.1e}", v)
            } else {
                let s = format!("{:.2}", v);
                s.trim_end_matches('0').trim_end_matches('.').to_string()
            }
        };
        let mut label = |p: P3, text: &str, anchor: &str, dx: f64, dy: f64, bold: bool| {
            let (a, b) = scr(p);
            let _ = write!(
                out,
                r#"<text x="{:.1}" y="{:.1}" text-anchor="{anchor}" fill="{}"{}>{}</text>"#,
                a + dx,
                b + dy,
                if bold { "#111827" } else { "#475569" },
                if bold { r#" font-weight="bold""# } else { "" },
                esc(text)
            );
        };
        let bottom: Vec<P3> = corners.iter().copied().filter(|c| c.2 < 0.0).collect();
        let near = *bottom
            .iter()
            .min_by(|a, b| project(**a).2.total_cmp(&project(**b).2))
            .unwrap();
        let side = |p: P3| {
            if scr(p).0 < scr(near).0 {
                "end"
            } else {
                "start"
            }
        };
        let dxs = |a: &str| if a == "end" { -4.0 } else { 4.0 };
        let [(bx0, bx1), (by0, by1), (bz0, bz1)] = bounds;
        let (xa, xb) = ((-ex, near.1, -ez), (ex, near.1, -ez));
        label(xa, &fmt(bx0), side(xa), dxs(side(xa)), 14.0, false);
        label(xb, &fmt(bx1), side(xb), dxs(side(xb)), 14.0, false);
        label(
            (0.0, near.1, -ez),
            &self.labels[0],
            "middle",
            0.0,
            24.0,
            true,
        );
        let (ya, yb) = ((near.0, -ey, -ez), (near.0, ey, -ez));
        label(ya, &fmt(by0), side(ya), dxs(side(ya)), 14.0, false);
        label(yb, &fmt(by1), side(yb), dxs(side(yb)), 14.0, false);
        label(
            (near.0, 0.0, -ez),
            &self.labels[1],
            "middle",
            0.0,
            24.0,
            true,
        );
        let left = *bottom
            .iter()
            .min_by(|a, b| scr(**a).0.total_cmp(&scr(**b).0))
            .unwrap();
        label((left.0, left.1, -ez), &fmt(bz0), "end", -8.0, 0.0, false);
        label((left.0, left.1, ez), &fmt(bz1), "end", -8.0, 8.0, false);
        label(
            (left.0, left.1, 0.0),
            &self.labels[2],
            "end",
            -10.0,
            4.0,
            true,
        );
        let mut entries: Vec<(String, String)> = self
            .surfaces
            .iter()
            .enumerate()
            .map(|(i, su)| {
                (
                    su.label.clone(),
                    if i == 0 {
                        rgb(viridis(0.5), 1.0)
                    } else {
                        rgb(SOLID[(i - 1) % SOLID.len()], 1.0)
                    },
                )
            })
            .collect();
        entries.extend(self.curves.iter().enumerate().map(|(i, c)| {
            (
                c.label.clone(),
                CURVE_COLORS[i % CURVE_COLORS.len()].to_string(),
            )
        }));
        for (i, (text, color)) in entries.iter().enumerate() {
            let y = 16 + i * 18;
            let _ = write!(
                out,
                r##"<rect x="10" y="{}" width="18" height="10" fill="{color}"/><text x="34" y="{}" fill="#111827">{}</text>"##,
                y - 6,
                y + 3,
                esc(text)
            );
        }
        if !self.title.is_empty() {
            let _ = write!(
                out,
                r##"<text x="320" y="24" text-anchor="middle" font-size="14" fill="#111827">{}</text>"##,
                esc(&self.title)
            );
        }
        out.push_str("</svg>");
        out
    }
}

pub fn steps(a: f64, b: f64, n: usize) -> Vec<f64> {
    (0..=n).map(|i| a + (b - a) * i as f64 / n as f64).collect()
}
