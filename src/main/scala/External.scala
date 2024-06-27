package org.sufrin.glyph

/**
 *  Rendering on external media
 */
object External {
  import GlyphTypes.Scale

  import io.github.humbleui.skija.Image

  def renderBitmap(glyph: Glyph, path: String, kind: String = "png", scale: Scale = 1.0f): Unit =
       renderBitmap(glyph, java.nio.file.Path.of(path), kind, scale)

  def renderBitmap(glyph: Glyph, path: java.nio.file.Path, kind: String, scale: Scale): Unit = {
     import io.github.humbleui.skija.{EncoderJPEG, EncoderPNG, Image, ImageInfo, Surface => SKSurface}
     import io.github.humbleui.types.IRect
     val w = glyph.w.toInt
     val h = glyph.h.toInt
     val imageInfo = ImageInfo.makeN32Premul(w, h)
     val surface = SKSurface.makeRaster(imageInfo)
     val canvas  = surface.getCanvas
     val drawing = Surface(canvas, 1.0f)
     drawing.clear(0xFFFFFFFF)
     glyph.draw(drawing)
     val snapshot: Image       = surface.makeImageSnapshot(new IRect(0, 0, w, h))
     val bytes:    Array[Byte] = kind match {
       case "png" => EncoderPNG.encode(snapshot).getBytes
       case "jpeg"
       |    "jpg" => EncoderJPEG.encode(snapshot).getBytes
     }
     java.nio.file.Files.write(path, bytes)
   }

  /** DOES NOT WORK */
  @deprecated("malfunctioning") def glyph2SVG(glyph: Glyph, path: java.nio.file.Path): Unit = {
    import Glyphs.Label

    import io.github.humbleui.skija.OutputWStream
    import io.github.humbleui.skija.svg.SVGCanvas
    import io.github.humbleui.types.Rect
    val svgStream = new OutputWStream(new java.io.FileOutputStream(path.toFile))
    val bounds = new Rect(0f, 0f, glyph.w, glyph.h)
    val canvas = SVGCanvas.make(bounds, svgStream, true, true)
    val drawing = Surface(canvas, 1f)
    drawing.clear(0XF0F0F0F0)
    Label("FOOBAZ").draw(drawing)
    glyph.draw(drawing)
    canvas.close()
    svgStream.close()
  }

  def glyph2Image(glyph: Glyph): Image = {
    import io.github.humbleui.skija.{EncoderJPEG, EncoderPNG, Image, ImageInfo, Surface => SKSurface}
    import io.github.humbleui.types.IRect
    val w = glyph.w.toInt
    val h = glyph.h.toInt
    val imageInfo = ImageInfo.makeN32Premul(w, h)
    val surface = SKSurface.makeRaster(imageInfo)
    val canvas = surface.getCanvas
    val drawing = Surface(canvas, 1.0f)
    drawing.clear(0xFFFFFFFF)
    glyph.draw(drawing)
    surface.makeImageSnapshot(new IRect(0, 0, w, h))
  }
}
