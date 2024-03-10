import std/[bitops, math, os, sequtils, strutils, tables, unicode]

type
  BdfParsingError* = object of ValueError

  BdfFontSize = tuple
    pointSize: int
    xres: int
    yres: int

  BdfFontBoundingBox = tuple
    fbbx: int
    fbby: int
    xoff: int
    yoff: int

  BdfProperty = tuple
    name: string
    value: string

  BdfGlyph = tuple
    name: string
    encoding: (int, int)
    swidth: tuple[swx0: int, swy0: int]
    dwidth: tuple[dwx0: int, dwy0: int]
    swidth1: tuple[swx1: int, swy1: int]
    dwidth1: tuple[dwx1: int, dwy1: int]
    vvector: tuple[xoff: int, yoff: int]
    bbx: tuple[
      bbw: int,
      bbh: int,
      bbxoff0x: int,
      bbyoff0y: int
    ]
    bitmap: seq[uint32]

  GlyphMap = Table[int, BdfGlyph]

  BdfFont = tuple
    bdfVersion: string
    fontComment: string
    fontVersion: int
    fontName: string
    size: BdfFontSize
    boundingBox: BdfFontBoundingBox
    metricsSet: range[0..2]
    swidth: tuple[swx0: int, swy0: int]
    dwidth: tuple[dwx0: int, dwy0: int]
    swidth1: tuple[swx1: int, swy1: int]
    dwidth1: tuple[dwx1: int, dwy1: int]
    vvector: tuple[xoff: int, yoff: int]
    properties: seq[BdfProperty]
    nGlyphs: int
    glyphs: GlyphMap

  TextBitmap = tuple
    width: int
    height: int
    data: seq[bool]

proc parseGlyph(lines: seq[string], startLine: int): (int, BdfGlyph) =
  var i = startLine
  var glyph: BdfGlyph
  while true:
    let
      line = lines[i]
      lineData =  line.split(' ', 1)
      keyword = lineData[0]

    var value: string
    if lineData.len > 1:
      value = lineData[1]
    else:
      value = ""

    case keyword
    of "STARTCHAR":
      glyph.name = value
    of "ENCODING":
      let values = value.split(' ')
      let val0 = values[0].parseInt()
      if val0 == -1:
        glyph.encoding = (val0, values[1].parseInt())
      else:
        glyph.encoding[0] = val0
    of "SWIDTH":
      let values = value.split(' ')
      glyph.swidth = (swx0: values[0].parseInt(), swy0: values[1].parseInt())
    of "DWIDTH":
      let values = value.split(' ')
      glyph.dwidth = (dwx0: values[0].parseInt(), dwy0: values[1].parseInt())
    of "SWIDTH1":
      let values = value.split(' ')
      glyph.swidth1 = (swx1: values[0].parseInt(), swy1: values[1].parseInt())
    of "DWIDTH1":
      let values = value.split(' ')
      glyph.dwidth1 = (dwx1: values[0].parseInt(), dwy1: values[1].parseInt())
    of "VVECTOR":
      let values = value.split(' ')
      glyph.vvector = (xoff: values[0].parseInt(), yoff: values[1].parseInt())
    of "BBX":
      let values = value.split(' ')
      glyph.bbx = (
        bbw: values[0].parseInt(),
        bbh: values[1].parseInt(),
        bbxoff0x: values[2].parseInt(),
        bbyoff0y: values[3].parseInt()
      )
    of "BITMAP":
      let height = glyph.bbx.bbh
      for j in 1..height:
        i += 1
        let raster = fromHex[uint32](lines[i])
        glyph.bitmap.add(raster)
      i += 1
      if lines[i] != "ENDCHAR":
        raise newException(BdfParsingError, "Error on line {i}: expected \"ENDCHAR\".")
      break

    i += 1
  result = (i, glyph)

proc parseGlyphs(lines: seq[string], startLine: int, nGlyphs: int): (int, GlyphMap) =
  var i = startLine
  var glyphs: GlyphMap
  for j in 0..<nGlyphs:
    var glyph: BdfGlyph
    (i, glyph) = lines.parseGlyph(i)
    glyphs[glyph.encoding[0]] = glyph
  result[0] = i
  result[1] = glyphs

proc parse*(data: string): BdfFont =
  let lines = data.splitLines()
  var i = 0
  while i < lines.len:
    let
      line = lines[i]
      lineData = line.split(' ', 1)
      keyword = lineData[0]

    var value: string
    if lineData.len > 1:
      value = lineData[1]
    else:
      value = ""

    case keyword
    of "STARTFONT":
      result.bdfVersion = value
    of "COMMENT":
      result.fontComment = value
    of "CONTENTVERSION":
      result.fontVersion = value.parseInt()
    of "FONT":
      result.fontName = value
    of "SIZE":
      let values = value.split(' ')
      result.size = (
        pointSize: values[0].parseInt(),
        xres: values[1].parseInt(),
        yres: values[2].parseInt()
      )
    of "FONTBOUNDINGBOX":
      let values = value.split(' ')
      result.boundingBox = (
        fbbx: values[0].parseInt(),
        fbby: values[1].parseInt(),
        xoff: values[2].parseInt(),
        yoff: values[3].parseInt()
      )
    of "METRICSET":
      result.metricsSet = value.parseInt()
    of "SWIDTH":
      let values = value.split(' ')
      result.swidth = (swx0: values[0].parseInt(), swy0: values[1].parseInt())
    of "DWIDTH":
      let values = value.split(' ')
      result.dwidth = (dwx0: values[0].parseInt(), dwy0: values[1].parseInt())
    of "SWIDTH1":
      let values = value.split(' ')
      result.swidth1 = (swx1: values[0].parseInt(), swy1: values[1].parseInt())
    of "DWIDTH1":
      let values = value.split(' ')
      result.dwidth1 = (dwx1: values[0].parseInt(), dwy1: values[1].parseInt())
    of "VVECTOR":
      let values = value.split(' ')
      result.vvector = (xoff: values[0].parseInt(), yoff: values[1].parseInt())
    of "STARTPROPERTIES":
      let n = value.parseInt()
      var properties: seq[BdfProperty]
      for j in 1..n:
        i += 1
        let
          propertyLine = lines[i]
          propertyData = propertyLine.split(' ', 1)
          propertyName = propertyData[0]
          propertyValue = propertyData[1]
          property = (
            name: propertyName,
            value: propertyValue
          )
        properties.add(property)
      i += 1
      if lines[i] != "ENDPROPERTIES":
        raise newException(BdfParsingError, "Error on line {i}: expected \"ENDPROPERTIES\".")
    of "CHARS":
      result.nGlyphs = value.parseInt()
      (i, result.glyphs) = lines.parseGlyphs(i + 1, result.nGlyphs)
    of "ENDFONT":
      break
    else:
      raise newException(BdfParsingError, "Error on line {i}: invalid keyword \"{keyword}\".")

    i += 1

func toGlyphs(s: string, font: BdfFont): seq[BdfGlyph] =
  let runes = toRunes(s)
  for r in runes:
    let
      codePoint = (int32)r
      g = font.glyphs[codePoint]
    result.add(g)

func calculateTextWidth(glyphs: seq[BdfGlyph]): int =
  for g in glyphs:
    result += g.dwidth.dwx0

func getIdx(bmp: TextBitmap; x, y: int): int = bmp.width * y + x

func render*(font: BdfFont, s: string): TextBitmap =
  let glyphs = s.toGlyphs(font)

  result.width = calculateTextWidth(glyphs)
  result.height = font.boundingBox.fbby
  result.data = newSeq[bool](result.width * result.height)

  var p = 0
  for glyph in glyphs:
    let
      # the nearest multiple of 8
      paddedWidth = - floorDiv(glyph.bbx.bbw, -8) * 8
      yOffset = font.boundingBox.fbby + font.boundingBox.yoff - glyph.bbx.bbh - glyph.bbx.bbyoff0y

    for y in 0..<glyph.bbx.bbh:
      let row = glyph.bitmap[y]
      for x in 0..<paddedWidth:
        if row.testBit(x):
          let
            reversed = paddedWidth - x
            glyphPos = reversed + glyph.bbx.bbxoff0x - 1
            idx = result.getIdx(p + glyphPos, y + yOffset)
          result.data[idx] = true
    p += glyph.dwidth.dwx0

when isMainModule:
  let
    fontData = stdin.readAll()
    font = fontdata.parse()

  let
    rendered = font.render(paramStr(1))
    text = rendered.data.mapIt(if it: "@" else: "-")

  for x in 0..<rendered.height:
    let w = rendered.width
    echo text[x*w..<x*w+w].join()
