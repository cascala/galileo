# Writes src/font_data.rs: the outlines of Inter Regular (OFL, see licenses/Inter-OFL.txt) for
# the characters Galileo's pictures use, curves flattened, in 1/1000 em. Needs fontTools.
#   python3 tools/font_data.py > /dev/null  (run from the repository root; prints point count)
import math, sys
from fontTools.ttLib import TTFont
from fontTools.pens.basePen import BasePen
f = TTFont('/usr/share/fonts/opentype/inter/Inter-Regular.otf')
upm = f['head'].unitsPerEm
cmap = f.getBestCmap(); gs = f.getGlyphSet(); hmtx = f['hmtx']
S = 1000/upm
chars = [chr(c) for c in range(32,127)] + [chr(c) for c in range(0xA0,0x100)]
chars += [chr(c) for c in range(0x391,0x3AA) if c!=0x3A2] + [chr(c) for c in range(0x3B1,0x3CA)]
chars += list("ϕϑϵ−±×÷·°²³¹⁴⁵⁶⁷⁸⁹⁰₀₁₂₃₄₅₆₇₈₉∞≈≠≤≥→←↔⇒∂∫√∑∏∇∈∉⊂∪∩∅∀∃′″…–—‘’“”•ℝℂℕℤℚħÅµ")
class P(BasePen):
    def __init__(s, gs): super().__init__(gs); s.cs=[]; s.c=None
    def _moveTo(s,p): s.c=[p]; s.cs.append(s.c)
    def _lineTo(s,p): s.c.append(p)
    def _curveToOne(s,p1,p2,p3):
        p0=s.c[-1]; L=math.dist(p0,p1)+math.dist(p1,p2)+math.dist(p2,p3)
        n=max(2,min(12,math.ceil(L*S/45)))
        for i in range(1,n+1):
            t=i/n; u=1-t
            s.c.append((u**3*p0[0]+3*u*u*t*p1[0]+3*u*t*t*p2[0]+t**3*p3[0], u**3*p0[1]+3*u*u*t*p1[1]+3*u*t*t*p2[1]+t**3*p3[1]))
    def _qCurveToOne(s,p1,p2):
        p0=s.c[-1]; L=math.dist(p0,p1)+math.dist(p1,p2); n=max(2,min(10,math.ceil(L*S/45)))
        for i in range(1,n+1):
            t=i/n;u=1-t
            s.c.append((u*u*p0[0]+2*u*t*p1[0]+t*t*p2[0], u*u*p0[1]+2*u*t*p1[1]+t*t*p2[1]))
    def _closePath(s): pass
    def _endPath(s): pass
out=["// Generated from Inter Regular (SIL Open Font License 1.1, https://rsms.me/inter):","// glyph outlines in 1/1000 em, y up, curves flattened. Made by tools/font_data.py; see export.rs.","","/// (char, advance width, contours as x0, y0, x1, y1, ...)","pub(crate) const GLYPHS: &[(char, u16, &[&[i16]])] = &["]
tot=0;missing=[]
for ch in chars:
    g=cmap.get(ord(ch))
    if g is None: missing.append(ch); continue
    p=P(gs); gs[g].draw(p)
    adv=round(hmtx[g][0]*S)
    cs=[]
    for c in p.cs:
        pts=[]
        for x,y in c:
            q=(round(x*S),round(y*S))
            if not pts or pts[-1]!=q: pts.append(q)
        if len(pts)>1 and pts[0]==pts[-1]: pts.pop()
        if len(pts)>=3:
            cs.append("&["+",".join(f"{x},{y}" for x,y in pts)+"]"); tot+=len(pts)
    out.append(f"    ('\\u{{{ord(ch):x}}}', {adv}, &[{', '.join(cs)}]),")
out.append("];")
open('src/font_data.rs','w').write("\n".join(out)+"\n")
print(tot, missing, file=sys.stderr)
