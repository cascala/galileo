//! Plots as PNG and EPS files, made from the SVG that Galileo draws.
//!
//! Galileo draws every picture (plots, 3D surfaces, graphs) as SVG. This module
//! reads back the small part of SVG that those pictures use (rect, line, circle,
//! path with M/L/H/V/Z, polygon, text, image, groups with clip paths, arrow
//! markers), turns it into filled shapes, and writes them out as
//!
//! * EPS: PostScript shapes, so the picture stays sharp at any size (text is
//!   drawn as outlines, so no fonts are needed to print it; see-through colors
//!   are mixed with the white background, since PostScript has no transparency);
//! * PNG: pixels, drawn here with anti-aliasing at a chosen scale.
//!
//! Text uses the outlines of Inter (SIL Open Font License), in `font_data.rs`.

use crate::font_data::GLYPHS;
use crate::plot::Raster;
use std::collections::HashMap;
use std::fmt::Write;

type R<T> = Result<T, String>;
type Pt = (f64, f64);
type Rgb = (f64, f64, f64);

/// One thing to paint, in SVG pixel coordinates (y down)
enum Item {
    /// filled contours (nonzero winding)
    Fill { contours: Vec<Vec<Pt>>, color: Rgb, alpha: f64, clip: Option<[f64; 4]> },
    /// stroked lines; `closed` joins the last point to the first
    Stroke {
        lines: Vec<(Vec<Pt>, bool)>,
        width: f64,
        color: Rgb,
        alpha: f64,
        round: bool,
        clip: Option<[f64; 4]>,
    },
    Image { x: f64, y: f64, w: f64, h: f64, pic: Option<Raster>, clip: Option<[f64; 4]> },
}

struct Scene {
    w: f64,
    h: f64,
    items: Vec<Item>,
}

// ---- reading the SVG ----

#[derive(Clone)]
struct Style {
    fill: Option<Rgb>,
    stroke: Option<Rgb>,
    stroke_width: f64,
    fill_opacity: f64,
    stroke_opacity: f64,
    font_size: f64,
    anchor: String,
    clip: Option<[f64; 4]>,
    round: bool,
}

fn color(s: &str) -> Option<Rgb> {
    let s = s.trim();
    let hex = |h: &str| u8::from_str_radix(h, 16).ok().map(|v| v as f64 / 255.0);
    if let Some(h) = s.strip_prefix('#') {
        return match h.len() {
            6 => Some((hex(&h[0..2])?, hex(&h[2..4])?, hex(&h[4..6])?)),
            3 => {
                let d = |i: usize| hex(&h[i..i + 1].repeat(2));
                Some((d(0)?, d(1)?, d(2)?))
            }
            _ => None,
        };
    }
    if let Some(inner) = s.strip_prefix("rgb(").and_then(|r| r.strip_suffix(')')) {
        let v: Vec<f64> = inner.split(',').filter_map(|x| x.trim().parse().ok()).collect();
        if v.len() == 3 {
            return Some((v[0] / 255.0, v[1] / 255.0, v[2] / 255.0));
        }
    }
    let named = match s {
        "none" | "transparent" => return None,
        "white" => "#ffffff",
        "black" => "#000000",
        "red" => "#ff0000",
        "green" => "#008000",
        "blue" => "#0000ff",
        "gray" | "grey" => "#808080",
        "orange" => "#ffa500",
        "purple" => "#800080",
        "yellow" => "#ffff00",
        "cyan" => "#00ffff",
        "magenta" => "#ff00ff",
        "brown" => "#a52a2a",
        "pink" => "#ffc0cb",
        _ => "#000000",
    };
    color(named)
}

fn unescape(s: &str) -> String {
    let mut out = String::new();
    let mut rest = s;
    while let Some(i) = rest.find('&') {
        out.push_str(&rest[..i]);
        let tail = &rest[i..];
        let Some(j) = tail.find(';') else {
            out.push_str(tail);
            return out;
        };
        let ent = &tail[1..j];
        match ent {
            "amp" => out.push('&'),
            "lt" => out.push('<'),
            "gt" => out.push('>'),
            "quot" => out.push('"'),
            "apos" => out.push('\''),
            _ => {
                let code = ent
                    .strip_prefix("#x")
                    .and_then(|h| u32::from_str_radix(h, 16).ok())
                    .or_else(|| ent.strip_prefix('#').and_then(|d| d.parse().ok()));
                match code.and_then(char::from_u32) {
                    Some(c) => out.push(c),
                    None => out.push_str(&tail[..=j]),
                }
            }
        }
        rest = &tail[j + 1..];
    }
    out.push_str(rest);
    out
}

/// The attributes of a start tag: `name="value"` pairs
fn attrs(tag: &str) -> HashMap<String, String> {
    let mut m = HashMap::new();
    let mut rest = tag;
    while let Some(eq) = rest.find("=\"") {
        let name = rest[..eq].trim().rsplit(char::is_whitespace).next().unwrap_or("").to_string();
        let after = &rest[eq + 2..];
        let Some(end) = after.find('"') else { break };
        m.insert(name, unescape(&after[..end]));
        rest = &after[end + 1..];
    }
    m
}

fn num(a: &HashMap<String, String>, k: &str, default: f64) -> f64 {
    a.get(k)
        .and_then(|v| v.trim().trim_end_matches("px").parse().ok())
        .unwrap_or(default)
}

/// The numbers of a path or points list
fn numbers(s: &str) -> Vec<f64> {
    let mut out = vec![];
    let mut cur = String::new();
    let flush = |cur: &mut String, out: &mut Vec<f64>| {
        if let Ok(v) = cur.parse() {
            out.push(v);
        }
        cur.clear();
    };
    let mut prev = ' ';
    for c in s.chars() {
        if c.is_ascii_digit() || c == '.' || c == 'e' || c == 'E' || ((c == '-' || c == '+') && (prev == 'e' || prev == 'E')) {
            if c == '.' && cur.contains('.') && !cur.contains(['e', 'E']) {
                flush(&mut cur, &mut out);
            }
            cur.push(c);
        } else if c == '-' || c == '+' {
            flush(&mut cur, &mut out);
            cur.push(c);
        } else {
            flush(&mut cur, &mut out);
        }
        prev = c;
    }
    flush(&mut cur, &mut out);
    out
}

/// A path's subpaths: (points, closed). Handles M L H V Z in either case.
fn path(d: &str) -> Vec<(Vec<Pt>, bool)> {
    let mut subs: Vec<(Vec<Pt>, bool)> = vec![];
    let mut cur: Vec<Pt> = vec![];
    let (mut x, mut y) = (0.0, 0.0);
    let mut start = (0.0, 0.0);
    // split into (command, numbers)
    let mut cmds: Vec<(char, Vec<f64>)> = vec![];
    let mut buf = String::new();
    let mut cmd: Option<char> = None;
    for c in d.chars() {
        if "MLHVZmlhvz".contains(c) {
            if let Some(k) = cmd {
                cmds.push((k, numbers(&buf)));
            }
            buf.clear();
            cmd = Some(c);
        } else {
            buf.push(c);
        }
    }
    if let Some(k) = cmd {
        cmds.push((k, numbers(&buf)));
    }
    let end_sub = |cur: &mut Vec<Pt>, subs: &mut Vec<(Vec<Pt>, bool)>, closed: bool| {
        if cur.len() > 1 {
            subs.push((std::mem::take(cur), closed));
        }
        cur.clear();
    };
    for (k, v) in cmds {
        let rel = k.is_ascii_lowercase();
        if cur.is_empty() && matches!(k.to_ascii_uppercase(), 'L' | 'H' | 'V') {
            cur.push((x, y));
        }
        match k.to_ascii_uppercase() {
            'M' => {
                end_sub(&mut cur, &mut subs, false);
                for (i, p) in v.chunks_exact(2).enumerate() {
                    if rel {
                        x += p[0];
                        y += p[1];
                    } else {
                        x = p[0];
                        y = p[1];
                    }
                    if i == 0 {
                        start = (x, y);
                    }
                    cur.push((x, y));
                }
            }
            'L' => {
                for p in v.chunks_exact(2) {
                    if rel {
                        x += p[0];
                        y += p[1];
                    } else {
                        x = p[0];
                        y = p[1];
                    }
                    cur.push((x, y));
                }
            }
            'H' => {
                for p in v {
                    x = if rel { x + p } else { p };
                    cur.push((x, y));
                }
            }
            'V' => {
                for p in v {
                    y = if rel { y + p } else { p };
                    cur.push((x, y));
                }
            }
            'Z' => {
                end_sub(&mut cur, &mut subs, true);
                x = start.0;
                y = start.1;
            }
            _ => {}
        }
    }
    end_sub(&mut cur, &mut subs, false);
    subs
}

fn circle(cx: f64, cy: f64, r: f64) -> Vec<Pt> {
    let n = ((r * 4.0).ceil() as usize).clamp(12, 64);
    (0..n)
        .map(|i| {
            let t = -(i as f64) / n as f64 * std::f64::consts::TAU;
            (cx + r * t.cos(), cy + r * t.sin())
        })
        .collect()
}

fn rounded_rect(x: f64, y: f64, w: f64, h: f64, r: f64) -> Vec<Pt> {
    let r = r.min(w / 2.0).min(h / 2.0);
    if r <= 0.0 {
        return vec![(x, y), (x + w, y), (x + w, y + h), (x, y + h)];
    }
    let mut pts = vec![];
    let corners = [(x + w - r, y + r, -90.0), (x + w - r, y + h - r, 0.0), (x + r, y + h - r, 90.0), (x + r, y + r, 180.0)];
    for (cx, cy, a0) in corners {
        for k in 0..=6 {
            let t = (a0 + 15.0 * k as f64).to_radians();
            pts.push((cx + r * t.cos(), cy + r * t.sin()));
        }
    }
    pts
}

/// Text as glyph outlines: (contours, width) at font size `fs`, left end of the baseline at (x, y)
fn text_outline(s: &str, x: f64, y: f64, fs: f64) -> (Vec<Vec<Pt>>, f64) {
    let k = fs / 1000.0;
    let mut pen = 0.0;
    let mut out = vec![];
    for c in s.chars() {
        let g = GLYPHS
            .iter()
            .find(|g| g.0 == c)
            .or_else(|| GLYPHS.iter().find(|g| g.0 == '?'));
        let Some((_, adv, contours)) = g else { continue };
        for ct in contours.iter() {
            out.push(
                ct.chunks_exact(2)
                    .map(|p| (x + (pen + p[0] as f64) * k, y - p[1] as f64 * k))
                    .collect(),
            );
        }
        pen += *adv as f64;
    }
    (out, pen * k)
}

fn parse(svg: &str, raster: Option<&Raster>) -> R<Scene> {
    let mut scene = Scene { w: 640.0, h: 400.0, items: vec![] };
    let base = Style {
        fill: Some((0.0, 0.0, 0.0)),
        stroke: None,
        stroke_width: 1.0,
        fill_opacity: 1.0,
        stroke_opacity: 1.0,
        font_size: 16.0,
        anchor: "start".into(),
        clip: None,
        round: false,
    };
    let mut stack: Vec<(String, Style)> = vec![];
    let mut clips: HashMap<String, [f64; 4]> = HashMap::new();
    // markers: id -> (fill, size in stroke widths)
    let mut markers: HashMap<String, (Rgb, f64)> = HashMap::new();
    let mut in_clip: Option<String> = None;
    let mut in_marker: Option<(String, f64)> = None;
    let mut pending_text: Option<(HashMap<String, String>, Style)> = None;
    let mut rest = svg;
    while let Some(lt) = rest.find('<') {
        let text_before = &rest[..lt];
        let Some(gt) = rest[lt..].find('>') else { break };
        let tag = &rest[lt + 1..lt + gt];
        rest = &rest[lt + gt + 1..];
        if let Some((a, st)) = pending_text.take() {
            if tag.starts_with('/') {
                draw_text(&mut scene, &a, &st, &unescape(text_before));
                continue;
            }
            pending_text = Some((a, st));
        }
        if tag.starts_with('/') {
            let name = tag[1..].trim();
            if name == "clipPath" {
                in_clip = None;
            }
            if name == "marker" {
                in_marker = None;
            }
            if let Some(i) = stack.iter().rposition(|(n, _)| n == name) {
                stack.truncate(i);
            }
            continue;
        }
        if tag.starts_with('?') || tag.starts_with('!') {
            continue;
        }
        let self_closing = tag.ends_with('/');
        let tag = tag.trim_end_matches('/');
        let name = tag.split_whitespace().next().unwrap_or("").to_string();
        let a = attrs(&tag[name.len()..]);
        let parent = stack.last().map(|s| s.1.clone()).unwrap_or_else(|| base.clone());
        let mut st = parent.clone();
        if let Some(v) = a.get("fill") {
            st.fill = color(v);
        }
        if let Some(v) = a.get("stroke") {
            st.stroke = color(v);
        }
        st.stroke_width = num(&a, "stroke-width", st.stroke_width);
        st.fill_opacity = num(&a, "fill-opacity", st.fill_opacity) * num(&a, "opacity", 1.0);
        st.stroke_opacity = num(&a, "stroke-opacity", st.stroke_opacity) * num(&a, "opacity", 1.0);
        st.font_size = num(&a, "font-size", st.font_size);
        if let Some(v) = a.get("text-anchor") {
            st.anchor = v.clone();
        }
        if a.get("stroke-linejoin").is_some_and(|v| v == "round") || a.get("stroke-linecap").is_some_and(|v| v == "round") {
            st.round = true;
        }
        if let Some(c) = a.get("clip-path") {
            let id = c.trim_start_matches("url(#").trim_end_matches(')');
            st.clip = clips.get(id).copied();
        }
        match name.as_str() {
            "svg" => {
                if let Some(vb) = a.get("viewBox") {
                    let v = numbers(vb);
                    if v.len() == 4 {
                        scene.w = v[2];
                        scene.h = v[3];
                    }
                } else {
                    scene.w = num(&a, "width", scene.w);
                    scene.h = num(&a, "height", scene.h);
                }
            }
            "clipPath" => in_clip = a.get("id").cloned(),
            "marker" => {
                in_marker = a.get("id").map(|id| (id.clone(), num(&a, "markerWidth", 3.0)));
            }
            _ if in_clip.is_some() => {
                if name == "rect" {
                    let id = in_clip.clone().unwrap();
                    let (x, y) = (num(&a, "x", 0.0), num(&a, "y", 0.0));
                    clips.insert(id, [x, y, x + num(&a, "width", 0.0), y + num(&a, "height", 0.0)]);
                }
            }
            _ if in_marker.is_some() => {
                let (id, size) = in_marker.clone().unwrap();
                markers.insert(id, (st.fill.unwrap_or((0.0, 0.0, 0.0)), size));
            }
            "rect" => {
                let pct = |k: &str, full: f64| match a.get(k) {
                    Some(v) if v.ends_with('%') => v.trim_end_matches('%').parse::<f64>().unwrap_or(100.0) / 100.0 * full,
                    _ => num(&a, k, 0.0),
                };
                let (x, y) = (num(&a, "x", 0.0), num(&a, "y", 0.0));
                let (w, h) = (pct("width", scene.w), pct("height", scene.h));
                let pts = rounded_rect(x, y, w, h, num(&a, "rx", 0.0));
                shape(&mut scene, vec![(pts, true)], &st);
            }
            "circle" => {
                let pts = circle(num(&a, "cx", 0.0), num(&a, "cy", 0.0), num(&a, "r", 0.0));
                shape(&mut scene, vec![(pts, true)], &st);
            }
            "line" => {
                let p0 = (num(&a, "x1", 0.0), num(&a, "y1", 0.0));
                let p1 = (num(&a, "x2", 0.0), num(&a, "y2", 0.0));
                let mut s2 = st.clone();
                s2.fill = None;
                let mut end = p1;
                let arrow = a
                    .get("marker-end")
                    .map(|m| m.trim_start_matches("url(#").trim_end_matches(')').to_string())
                    .and_then(|id| markers.get(&id).copied());
                if let Some((col, size)) = arrow {
                    // an arrowhead whose tip is at the end of the line
                    let len = size * st.stroke_width;
                    let (dx, dy) = (p1.0 - p0.0, p1.1 - p0.1);
                    let d = (dx * dx + dy * dy).sqrt().max(1e-9);
                    let (ux, uy) = (dx / d, dy / d);
                    let back = (p1.0 - ux * len, p1.1 - uy * len);
                    let half = len / 2.0;
                    let head = vec![p1, (back.0 - uy * half, back.1 + ux * half), (back.0 + uy * half, back.1 - ux * half)];
                    end = (p1.0 - ux * len * 0.8, p1.1 - uy * len * 0.8);
                    let hs = Style { fill: Some(col), stroke: None, fill_opacity: 1.0, ..st.clone() };
                    shape(&mut scene, vec![(vec![p0, end], false)], &s2);
                    shape(&mut scene, vec![(head, true)], &hs);
                } else {
                    shape(&mut scene, vec![(vec![p0, end], false)], &s2);
                }
            }
            "path" => {
                if let Some(d) = a.get("d") {
                    shape(&mut scene, path(d), &st);
                }
            }
            "polygon" | "polyline" => {
                let v = numbers(a.get("points").map_or("", |s| s));
                let pts: Vec<Pt> = v.chunks_exact(2).map(|p| (p[0], p[1])).collect();
                shape(&mut scene, vec![(pts, name == "polygon")], &st);
            }
            "image" => {
                let pic = match a.get("href").or_else(|| a.get("xlink:href")) {
                    Some(h) if h.starts_with("data:image/png;base64,") => {
                        unbase64(&h["data:image/png;base64,".len()..]).and_then(|b| png_pixels(&b))
                    }
                    _ => None,
                }
                .or_else(|| raster.cloned());
                scene.items.push(Item::Image {
                    pic,
                    x: num(&a, "x", 0.0),
                    y: num(&a, "y", 0.0),
                    w: num(&a, "width", 0.0),
                    h: num(&a, "height", 0.0),
                    clip: st.clip,
                });
            }
            "text" => {
                if !self_closing {
                    pending_text = Some((a.clone(), st.clone()));
                }
                continue;
            }
            _ => {}
        }
        if !self_closing && !matches!(name.as_str(), "rect" | "circle" | "line" | "path" | "polygon" | "polyline" | "image") {
            stack.push((name, st));
        }
    }
    Ok(scene)
}

/// fill, then stroke, as SVG paints a shape
fn shape(scene: &mut Scene, subs: Vec<(Vec<Pt>, bool)>, st: &Style) {
    if let Some(c) = st.fill {
        let contours: Vec<Vec<Pt>> = subs.iter().filter(|s| s.0.len() > 2).map(|s| s.0.clone()).collect();
        if !contours.is_empty() && st.fill_opacity > 0.0 {
            scene.items.push(Item::Fill { contours, color: c, alpha: st.fill_opacity, clip: st.clip });
        }
    }
    if let Some(c) = st.stroke {
        if st.stroke_width > 0.0 && st.stroke_opacity > 0.0 {
            scene.items.push(Item::Stroke {
                lines: subs,
                width: st.stroke_width,
                color: c,
                alpha: st.stroke_opacity,
                round: st.round,
                clip: st.clip,
            });
        }
    }
}

fn draw_text(scene: &mut Scene, a: &HashMap<String, String>, st: &Style, text: &str) {
    if text.trim().is_empty() {
        return;
    }
    let fs = st.font_size;
    let (x, y) = (num(a, "x", 0.0), num(a, "y", 0.0) + num(a, "dy", 0.0));
    let (_, width) = text_outline(text, 0.0, 0.0, fs);
    let x0 = match st.anchor.as_str() {
        "middle" => x - width / 2.0,
        "end" => x - width,
        _ => x,
    };
    let (mut contours, _) = text_outline(text, x0, y, fs);
    // transform="rotate(angle cx cy)"
    if let Some(t) = a.get("transform").and_then(|t| t.strip_prefix("rotate(")) {
        let v = numbers(t);
        let ang = v.first().copied().unwrap_or(0.0).to_radians();
        let (cx, cy) = (v.get(1).copied().unwrap_or(0.0), v.get(2).copied().unwrap_or(0.0));
        let (s, c) = ang.sin_cos();
        for ct in contours.iter_mut() {
            for p in ct.iter_mut() {
                let (dx, dy) = (p.0 - cx, p.1 - cy);
                *p = (cx + dx * c - dy * s, cy + dx * s + dy * c);
            }
        }
    }
    // paint-order="stroke": a halo under the letters
    if let Some(c) = st.stroke {
        scene.items.push(Item::Stroke {
            lines: contours.iter().map(|c| (c.clone(), true)).collect(),
            width: st.stroke_width,
            color: c,
            alpha: st.stroke_opacity,
            round: true,
            clip: st.clip,
        });
    }
    if let Some(c) = st.fill {
        scene.items.push(Item::Fill { contours, color: c, alpha: st.fill_opacity, clip: st.clip });
    }
}

// ---- reading back an embedded PNG ----

fn unbase64(s: &str) -> Option<Vec<u8>> {
    let val = |c: u8| -> Option<u32> {
        Some(match c {
            b'A'..=b'Z' => c - b'A',
            b'a'..=b'z' => c - b'a' + 26,
            b'0'..=b'9' => c - b'0' + 52,
            b'+' | b'-' => 62,
            b'/' | b'_' => 63,
            _ => return None,
        } as u32)
    };
    let clean: Vec<u8> = s.bytes().filter(|c| !c.is_ascii_whitespace() && *c != b'=').collect();
    let mut out = Vec::with_capacity(clean.len() * 3 / 4);
    for ch in clean.chunks(4) {
        let mut n = 0u32;
        for (i, &c) in ch.iter().enumerate() {
            n |= val(c)? << (18 - 6 * i);
        }
        let bytes = [(n >> 16) as u8, (n >> 8) as u8, n as u8];
        out.extend_from_slice(&bytes[..ch.len().saturating_sub(1)]);
    }
    Some(out)
}

/// Bits of a deflate stream, least significant first
struct Bits<'a> {
    d: &'a [u8],
    pos: usize,
    bit: u32,
}

impl Bits<'_> {
    fn get(&mut self, n: u32) -> Option<u32> {
        let mut v = 0;
        for i in 0..n {
            let byte = *self.d.get(self.pos)?;
            v |= (((byte >> self.bit) & 1) as u32) << i;
            self.bit += 1;
            if self.bit == 8 {
                self.bit = 0;
                self.pos += 1;
            }
        }
        Some(v)
    }
}

/// A canonical Huffman code: (counts per length, symbols in order)
struct Huff {
    counts: [u16; 16],
    syms: Vec<u16>,
}

impl Huff {
    fn new(lens: &[u8]) -> Huff {
        let mut counts = [0u16; 16];
        for &l in lens {
            counts[l as usize] += 1;
        }
        counts[0] = 0;
        let mut offs = [0u16; 16];
        for i in 1..16 {
            offs[i] = offs[i - 1] + counts[i - 1];
        }
        let mut syms = vec![0u16; lens.len()];
        for (s, &l) in lens.iter().enumerate() {
            if l != 0 {
                syms[(offs[l as usize] - counts[0]) as usize] = s as u16;
                offs[l as usize] += 1;
            }
        }
        Huff { counts, syms }
    }

    fn decode(&self, b: &mut Bits) -> Option<u16> {
        let (mut code, mut first, mut index) = (0i32, 0i32, 0i32);
        for len in 1..16 {
            code |= b.get(1)? as i32;
            let count = self.counts[len] as i32;
            if code - count < first {
                return self.syms.get((index + (code - first)) as usize).copied();
            }
            index += count;
            first += count;
            first <<= 1;
            code <<= 1;
        }
        None
    }
}

/// zlib data, inflated
fn inflate(z: &[u8]) -> Option<Vec<u8>> {
    const LBASE: [u16; 29] = [3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258];
    const LEXT: [u8; 29] = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0];
    const DBASE: [u16; 30] = [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577];
    const DEXT: [u8; 30] = [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13];
    let mut b = Bits { d: z.get(2..)?, pos: 0, bit: 0 };
    let mut out: Vec<u8> = vec![];
    loop {
        let last = b.get(1)?;
        let kind = b.get(2)?;
        if kind == 0 {
            if b.bit != 0 {
                b.bit = 0;
                b.pos += 1;
            }
            let len = u16::from_le_bytes([*b.d.get(b.pos)?, *b.d.get(b.pos + 1)?]) as usize;
            b.pos += 4;
            out.extend_from_slice(b.d.get(b.pos..b.pos + len)?);
            b.pos += len;
        } else {
            let (lit, dist) = if kind == 1 {
                let mut l = [8u8; 288];
                l[144..256].fill(9);
                l[256..280].fill(7);
                (Huff::new(&l), Huff::new(&[5u8; 30]))
            } else if kind == 2 {
                let hlit = b.get(5)? as usize + 257;
                let hdist = b.get(5)? as usize + 1;
                let hclen = b.get(4)? as usize + 4;
                const ORDER: [usize; 19] = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15];
                let mut cl = [0u8; 19];
                for &o in ORDER.iter().take(hclen) {
                    cl[o] = b.get(3)? as u8;
                }
                let ch = Huff::new(&cl);
                let mut lens = vec![];
                while lens.len() < hlit + hdist {
                    let s = ch.decode(&mut b)?;
                    match s {
                        0..=15 => lens.push(s as u8),
                        16 => {
                            let p = *lens.last()?;
                            for _ in 0..3 + b.get(2)? {
                                lens.push(p);
                            }
                        }
                        17 => lens.extend(std::iter::repeat_n(0, 3 + b.get(3)? as usize)),
                        _ => lens.extend(std::iter::repeat_n(0, 11 + b.get(7)? as usize)),
                    }
                }
                (Huff::new(&lens[..hlit]), Huff::new(&lens[hlit..hlit + hdist]))
            } else {
                return None;
            };
            loop {
                let s = lit.decode(&mut b)? as usize;
                if s < 256 {
                    out.push(s as u8);
                } else if s == 256 {
                    break;
                } else {
                    let i = s - 257;
                    let len = *LBASE.get(i)? as usize + b.get(*LEXT.get(i)? as u32)? as usize;
                    let di = dist.decode(&mut b)? as usize;
                    let d = *DBASE.get(di)? as usize + b.get(*DEXT.get(di)? as u32)? as usize;
                    if d > out.len() {
                        return None;
                    }
                    let start = out.len() - d;
                    for k in 0..len {
                        out.push(out[start + k]);
                    }
                }
            }
        }
        if last == 1 {
            return Some(out);
        }
    }
}

/// The pixels of an 8-bit RGB or RGBA PNG (as Galileo writes them)
fn png_pixels(png: &[u8]) -> Option<Raster> {
    if png.get(..8)? != b"\x89PNG\r\n\x1a\n" {
        return None;
    }
    let (mut w, mut h, mut ct) = (0usize, 0usize, 0u8);
    let mut idat = vec![];
    let mut p = 8;
    while p + 8 <= png.len() {
        let len = u32::from_be_bytes(png[p..p + 4].try_into().ok()?) as usize;
        let kind = &png[p + 4..p + 8];
        let data = png.get(p + 8..p + 8 + len)?;
        match kind {
            b"IHDR" => {
                w = u32::from_be_bytes(data[0..4].try_into().ok()?) as usize;
                h = u32::from_be_bytes(data[4..8].try_into().ok()?) as usize;
                if data[8] != 8 || data[12] != 0 {
                    return None;
                }
                ct = data[9];
            }
            b"IDAT" => idat.extend_from_slice(data),
            _ => {}
        }
        p += 12 + len;
    }
    let bpp = match ct {
        2 => 3,
        6 => 4,
        _ => return None,
    };
    if w == 0 || h == 0 || w * h > 50_000_000 {
        return None;
    }
    let raw = inflate(&idat)?;
    let stride = w * bpp;
    if raw.len() < h * (stride + 1) {
        return None;
    }
    let mut img = vec![0u8; h * stride];
    for y in 0..h {
        let f = raw[y * (stride + 1)];
        let src = &raw[y * (stride + 1) + 1..(y + 1) * (stride + 1)];
        for x in 0..stride {
            let a = if x >= bpp { img[y * stride + x - bpp] as i32 } else { 0 };
            let up = if y > 0 { img[(y - 1) * stride + x] as i32 } else { 0 };
            let c = if x >= bpp && y > 0 { img[(y - 1) * stride + x - bpp] as i32 } else { 0 };
            let pred = match f {
                0 => 0,
                1 => a,
                2 => up,
                3 => (a + up) / 2,
                4 => {
                    let pp = a + up - c;
                    let (pa, pb, pc) = ((pp - a).abs(), (pp - up).abs(), (pp - c).abs());
                    if pa <= pb && pa <= pc { a } else if pb <= pc { up } else { c }
                }
                _ => return None,
            };
            img[y * stride + x] = (src[x] as i32 + pred) as u8;
        }
    }
    let rgb = if bpp == 3 {
        img
    } else {
        // over white
        img.chunks(4)
            .flat_map(|px| {
                let a = px[3] as u32;
                [0, 1, 2].map(|i| ((px[i] as u32 * a + 255 * (255 - a)) / 255) as u8)
            })
            .collect()
    };
    Some(Raster { w, h, rgb, x0: 0.0, x1: 1.0, y0: 0.0, y1: 1.0 })
}

// ---- EPS ----

/// A picture as Encapsulated PostScript (1 SVG pixel = 0.75 point)
pub fn eps(svg: &str, raster: Option<&Raster>) -> R<String> {
    let scene = parse(svg, raster)?;
    let k = 0.75;
    let (w, h) = (scene.w * k, scene.h * k);
    let mut s = String::new();
    let _ = write!(
        s,
        "%!PS-Adobe-3.0 EPSF-3.0\n%%BoundingBox: 0 0 {} {}\n%%HiResBoundingBox: 0 0 {:.2} {:.2}\n%%Creator: Galileo\n%%Pages: 1\n%%EndComments\n",
        w.ceil() as i64,
        h.ceil() as i64,
        w,
        h
    );
    s.push_str("/m {moveto} bind def /l {lineto} bind def /c {closepath} bind def\n");
    let _ = writeln!(s, "gsave 0 {:.2} translate {k} -{k} scale", scene.h * k);
    let over_white = |c: Rgb, a: f64| (1.0 - a + a * c.0, 1.0 - a + a * c.1, 1.0 - a + a * c.2);
    let pt = |s: &mut String, p: Pt, first: bool| {
        let _ = write!(s, "{:.2} {:.2} {} ", p.0, p.1, if first { "m" } else { "l" });
    };
    let clip_open = |s: &mut String, clip: &Option<[f64; 4]>| {
        s.push_str("gsave ");
        if let Some([x0, y0, x1, y1]) = clip {
            let _ = write!(s, "newpath {x0:.2} {y0:.2} m {x1:.2} {y0:.2} l {x1:.2} {y1:.2} l {x0:.2} {y1:.2} l c clip ");
        }
    };
    for it in &scene.items {
        match it {
            Item::Fill { contours, color, alpha, clip } => {
                clip_open(&mut s, clip);
                s.push_str("newpath ");
                for ct in contours {
                    for (i, p) in ct.iter().enumerate() {
                        pt(&mut s, *p, i == 0);
                    }
                    s.push_str("c ");
                }
                let c = over_white(*color, *alpha);
                let _ = writeln!(s, "{:.3} {:.3} {:.3} setrgbcolor fill grestore", c.0, c.1, c.2);
            }
            Item::Stroke { lines, width, color, alpha, round, clip } => {
                clip_open(&mut s, clip);
                s.push_str("newpath ");
                for (ln, closed) in lines {
                    for (i, p) in ln.iter().enumerate() {
                        pt(&mut s, *p, i == 0);
                    }
                    if *closed {
                        s.push_str("c ");
                    }
                }
                let c = over_white(*color, *alpha);
                let j = if *round { 1 } else { 0 };
                let _ = writeln!(
                    s,
                    "{:.3} {:.3} {:.3} setrgbcolor {:.2} setlinewidth {j} setlinejoin {j} setlinecap stroke grestore",
                    c.0, c.1, c.2, width
                );
            }
            Item::Image { x, y, w, h, pic, clip } => {
                let Some(r) = pic else { continue };
                clip_open(&mut s, clip);
                let _ = writeln!(
                    s,
                    "{x:.2} {y:.2} translate {w:.2} {h:.2} scale\n/pix {} string def\n{} {} 8 [{} 0 0 {} 0 0] {{currentfile pix readhexstring pop}} false 3 colorimage",
                    r.w * 3,
                    r.w,
                    r.h,
                    r.w,
                    r.h
                );
                for row in r.rgb.chunks(r.w * 3) {
                    for chunk in row.chunks(36) {
                        for b in chunk {
                            let _ = write!(s, "{:02x}", b);
                        }
                        s.push('\n');
                    }
                }
                s.push_str("grestore\n");
            }
        }
    }
    s.push_str("grestore\nshowpage\n%%EOF\n");
    Ok(s)
}

// ---- PNG ----

/// Coverage of filled contours, accumulated as signed areas (after font-rs):
/// one f32 per pixel of a w x h window
struct Acc {
    w: usize,
    h: usize,
    a: Vec<f32>,
}

impl Acc {
    fn new(w: usize, h: usize) -> Acc {
        Acc { w, h, a: vec![0.0; w * h + 4] }
    }

    fn line(&mut self, p0: Pt, p1: Pt) {
        let (w, h) = (self.w as f64, self.h as f64);
        let clampx = |x: f64| x.clamp(0.0, w - 1e-6);
        let (p0, p1) = ((clampx(p0.0), p0.1), (clampx(p1.0), p1.1));
        if p0.1 == p1.1 {
            return;
        }
        let (dir, p0, p1) = if p0.1 < p1.1 { (1.0, p0, p1) } else { (-1.0, p1, p0) };
        if p1.1 <= 0.0 || p0.1 >= h {
            return;
        }
        let dxdy = (p1.0 - p0.0) / (p1.1 - p0.1);
        let mut x = p0.0;
        if p0.1 < 0.0 {
            x -= p0.1 * dxdy;
        }
        let y0 = p0.1.max(0.0) as usize;
        let y1 = (p1.1.min(h)).ceil() as usize;
        for y in y0..y1 {
            let line = y * self.w;
            let yf = y as f64;
            let dy = (yf + 1.0).min(p1.1) - yf.max(p0.1);
            let xnext = x + dxdy * dy;
            let d = (dy * dir) as f32;
            let (xa, xb) = if x < xnext { (x, xnext) } else { (xnext, x) };
            let xa_floor = xa.floor();
            let xai = xa_floor as usize;
            let xb_ceil = xb.ceil();
            let xbi = xb_ceil as usize;
            if xbi <= xai + 1 {
                let xmf = (0.5 * (x + xnext) - xa_floor) as f32;
                self.a[line + xai] += d - d * xmf;
                self.a[line + xai + 1] += d * xmf;
            } else {
                let s = (1.0 / (xb - xa)) as f32;
                let xaf = (xa - xa_floor) as f32;
                let a0 = 0.5 * s * (1.0 - xaf) * (1.0 - xaf);
                let xbf = (xb - xb_ceil + 1.0) as f32;
                let am = 0.5 * s * xbf * xbf;
                self.a[line + xai] += d * a0;
                if xbi == xai + 2 {
                    self.a[line + xai + 1] += d * (1.0 - a0 - am);
                } else {
                    let a1 = s * (1.5 - xaf);
                    self.a[line + xai + 1] += d * (a1 - a0);
                    for xi in xai + 2..xbi - 1 {
                        self.a[line + xi] += d * s;
                    }
                    let a2 = a1 + (xbi - xai - 3) as f32 * s;
                    self.a[line + xbi - 1] += d * (1.0 - a2 - am);
                }
                self.a[line + xbi] += d * am;
            }
            x = xnext;
        }
    }

    fn polygon(&mut self, pts: &[Pt]) {
        for i in 0..pts.len() {
            self.line(pts[i], pts[(i + 1) % pts.len()]);
        }
    }
}

/// The outline of a stroked line as polygons, all turning the same way:
/// a rectangle per segment and a disc at each joint (and round ends)
fn stroke_polygons(ln: &[Pt], closed: bool, width: f64, round: bool) -> Vec<Vec<Pt>> {
    let hw = width / 2.0;
    let mut out = vec![];
    let n = ln.len();
    if n < 2 {
        return out;
    }
    let segs = if closed { n } else { n - 1 };
    let ccw = |mut p: Vec<Pt>| {
        let area: f64 = (0..p.len())
            .map(|i| {
                let (a, b) = (p[i], p[(i + 1) % p.len()]);
                a.0 * b.1 - b.0 * a.1
            })
            .sum();
        if area < 0.0 {
            p.reverse();
        }
        p
    };
    for i in 0..segs {
        let (a, b) = (ln[i], ln[(i + 1) % n]);
        let (dx, dy) = (b.0 - a.0, b.1 - a.1);
        let d = (dx * dx + dy * dy).sqrt();
        if d < 1e-9 {
            continue;
        }
        let (nx, ny) = (-dy / d * hw, dx / d * hw);
        out.push(ccw(vec![(a.0 + nx, a.1 + ny), (b.0 + nx, b.1 + ny), (b.0 - nx, b.1 - ny), (a.0 - nx, a.1 - ny)]));
    }
    let disc = |p: Pt| {
        let m = ((hw * 3.0).ceil() as usize).clamp(8, 24);
        ccw((0..m)
            .map(|k| {
                let t = k as f64 / m as f64 * std::f64::consts::TAU;
                (p.0 + hw * t.cos(), p.1 + hw * t.sin())
            })
            .collect())
    };
    // joints: discs keep the turns closed (thin lines only need them where they bend)
    for i in 0..n {
        let end = !closed && (i == 0 || i == n - 1);
        if end && !round {
            continue;
        }
        if hw < 0.75 && !end {
            continue;
        }
        out.push(disc(ln[i]));
    }
    out
}

/// A picture as PNG bytes, `scale` pixels per SVG pixel
pub fn png(svg: &str, raster: Option<&Raster>, scale: f64) -> R<Vec<u8>> {
    let scene = parse(svg, raster)?;
    let (w, h) = ((scene.w * scale).round() as usize, (scene.h * scale).round() as usize);
    if w == 0 || h == 0 || w * h > 40_000_000 {
        return Err("the picture is too large".into());
    }
    let mut px = vec![1.0f32; w * h * 3];
    for it in &scene.items {
        let (polys, color, alpha, clip): (Vec<Vec<Pt>>, Rgb, f64, &Option<[f64; 4]>) = match it {
            Item::Fill { contours, color, alpha, clip } => (contours.clone(), *color, *alpha, clip),
            Item::Stroke { lines, width, color, alpha, round, clip } => {
                // a stroke thinner than a pixel is drawn one pixel wide, fainter
                let wpx = width * scale;
                let (wd, a) = if wpx < 1.0 { (1.0 / scale, alpha * wpx) } else { (*width, *alpha) };
                let mut v = vec![];
                for (ln, closed) in lines {
                    v.extend(stroke_polygons(ln, *closed, wd, *round));
                }
                (v, *color, a, clip)
            }
            Item::Image { x, y, w: iw, h: ih, pic, clip } => {
                if let Some(r) = pic {
                    paint_image(&mut px, w, h, scale, r, (*x, *y, *iw, *ih), clip);
                }
                continue;
            }
        };
        // the window this shape covers
        let (mut x0, mut y0, mut x1, mut y1) = (f64::MAX, f64::MAX, f64::MIN, f64::MIN);
        for p in polys.iter().flatten() {
            x0 = x0.min(p.0);
            y0 = y0.min(p.1);
            x1 = x1.max(p.0);
            y1 = y1.max(p.1);
        }
        if let Some([cx0, cy0, cx1, cy1]) = clip {
            x0 = x0.max(*cx0);
            y0 = y0.max(*cy0);
            x1 = x1.min(*cx1);
            y1 = y1.min(*cy1);
        }
        let bx0 = ((x0 * scale).floor().max(0.0)) as usize;
        let by0 = ((y0 * scale).floor().max(0.0)) as usize;
        let bx1 = ((x1 * scale).ceil().min(w as f64)).max(0.0) as usize;
        let by1 = ((y1 * scale).ceil().min(h as f64)).max(0.0) as usize;
        if bx1 <= bx0 || by1 <= by0 {
            continue;
        }
        let (aw, ah) = (bx1 - bx0 + 2, by1 - by0);
        let mut acc = Acc::new(aw, ah);
        for p in &polys {
            let q: Vec<Pt> = p
                .iter()
                .map(|&(x, y)| (x * scale - bx0 as f64, y * scale - by0 as f64))
                .collect();
            acc.polygon(&q);
        }
        let clip_px = clip.map(|[a, b, c, d]| [a * scale, b * scale, c * scale, d * scale]);
        let mut sum = 0.0f32;
        for j in 0..ah {
            for i in 0..aw {
                sum += acc.a[j * aw + i];
                let (gx, gy) = (bx0 + i, by0 + j);
                if gx >= w {
                    continue;
                }
                let mut cov = sum.abs().min(1.0) as f64;
                if cov <= 0.002 {
                    continue;
                }
                if let Some([cx0, cy0, cx1, cy1]) = clip_px {
                    let fx = ((gx as f64 + 1.0).min(cx1) - (gx as f64).max(cx0)).clamp(0.0, 1.0);
                    let fy = ((gy as f64 + 1.0).min(cy1) - (gy as f64).max(cy0)).clamp(0.0, 1.0);
                    cov *= fx * fy;
                }
                let k = (cov * alpha) as f32;
                let o = (gy * w + gx) * 3;
                px[o] += (color.0 as f32 - px[o]) * k;
                px[o + 1] += (color.1 as f32 - px[o + 1]) * k;
                px[o + 2] += (color.2 as f32 - px[o + 2]) * k;
            }
        }
    }
    let rgb: Vec<u8> = px.iter().map(|v| (v.clamp(0.0, 1.0) * 255.0).round() as u8).collect();
    Ok(crate::plot::png_bytes(w, h, &rgb))
}

fn paint_image(px: &mut [f32], w: usize, h: usize, scale: f64, r: &Raster, (x, y, iw, ih): (f64, f64, f64, f64), clip: &Option<[f64; 4]>) {
    let [cx0, cy0, cx1, cy1] = clip.unwrap_or([0.0, 0.0, w as f64 / scale, h as f64 / scale]);
    let (x0, y0) = (x.max(cx0) * scale, y.max(cy0) * scale);
    let (x1, y1) = ((x + iw).min(cx1) * scale, (y + ih).min(cy1) * scale);
    for gy in (y0.floor().max(0.0) as usize)..(y1.ceil().min(h as f64) as usize) {
        for gx in (x0.floor().max(0.0) as usize)..(x1.ceil().min(w as f64) as usize) {
            let u = ((gx as f64 + 0.5) / scale - x) / iw * r.w as f64;
            let v = ((gy as f64 + 0.5) / scale - y) / ih * r.h as f64;
            if u < 0.0 || v < 0.0 {
                continue;
            }
            let (ui, vi) = ((u as usize).min(r.w - 1), (v as usize).min(r.h - 1));
            let s = (vi * r.w + ui) * 3;
            let o = (gy * w + gx) * 3;
            for c in 0..3 {
                px[o + c] = r.rgb[s + c] as f32 / 255.0;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SVG: &str = r##"<svg xmlns="http://www.w3.org/2000/svg" width="100" height="50" viewBox="0 0 100 50" font-size="12"><rect width="100" height="50" fill="white"/><path d="M10,40L90,10" fill="none" stroke="#ff0000" stroke-width="2"/><text x="50" y="30" text-anchor="middle" fill="#111827">a &lt; b</text></svg>"##;

    #[test]
    fn png_and_eps() {
        let p = png(SVG, None, 1.0).unwrap();
        assert_eq!(&p[1..4], b"PNG");
        let e = eps(SVG, None).unwrap();
        assert!(e.starts_with("%!PS-Adobe-3.0 EPSF-3.0\n%%BoundingBox: 0 0 75 38"));
        assert!(e.contains("1.000 0.000 0.000 setrgbcolor 2.00 setlinewidth"));
        assert!(e.trim_end().ends_with("%%EOF"));
    }

    #[test]
    fn coverage() {
        // a 2 x 2 square at (1.5, 1.5) covers 4 pixels by a quarter and 1 fully... in a 4 x 4 window
        let mut a = Acc::new(6, 4);
        a.polygon(&[(1.5, 1.5), (3.5, 1.5), (3.5, 3.5), (1.5, 3.5)]);
        let mut sum = 0.0;
        let mut total = 0.0;
        for v in &a.a[..24] {
            sum += v;
            total += sum.abs().min(1.0);
        }
        assert!((total - 4.0).abs() < 1e-4, "{}", total);
    }

    #[test]
    fn embedded_png() {
        let rgb: Vec<u8> = (0..6 * 4 * 3).map(|i| (i * 7 % 256) as u8).collect();
        let png = crate::plot::png_bytes(6, 4, &rgb);
        let r = png_pixels(&png).unwrap();
        assert_eq!((r.w, r.h), (6, 4));
        assert_eq!(r.rgb, rgb);
        let b64 = crate::plot::base64(&png);
        assert_eq!(unbase64(&b64).unwrap(), png);
    }

    #[test]
    fn paths() {
        let p = path("M0,0L10,0L10,10ZM1,1h2v2h-2z");
        assert_eq!(p.len(), 2);
        assert!(p[0].1 && p[1].1);
        assert_eq!(p[1].0, vec![(1.0, 1.0), (3.0, 1.0), (3.0, 3.0), (1.0, 3.0)]);
        assert_eq!(numbers("1.5-2e-3,4 .5.5"), vec![1.5, -2e-3, 4.0, 0.5, 0.5]);
    }
}
