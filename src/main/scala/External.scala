package org.sufrin
package glyph

import java.nio.file.Path

/**
 *  Rendering on external media
 */
object External {

  trait Base64Icon {
    val base64: String
    lazy val icon: Glyph = External.Image.readBase64(base64)
  }

  import GlyphTypes.{Image, Scale}

  def writeGlyph(glyph: Glyph, path: String, kind: String = "png", scale: Scale = 1.0f): Unit =
       writeGlyph(glyph, java.nio.file.Path.of(path), kind, scale)

  def writeGlyph(glyph: Glyph, path: java.nio.file.Path, kind: String, scale: Scale): Unit = {
     import io.github.humbleui.skija.{EncoderJPEG, EncoderPNG, ImageInfo, Surface => SKSurface}
     import io.github.humbleui.types.IRect
     val w = glyph.w.toInt
     val h = glyph.h.toInt
     val surface = SKSurface.makeRaster(ImageInfo.makeN32Premul(w, h))
     val canvas  = surface.getCanvas
     val drawing = Surface(canvas, scale)
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

  def toByteArray(glyph: Glyph, kind: String): Array[Byte] = {
    import io.github.humbleui.skija.{EncoderJPEG, EncoderPNG}
    val snapshot: Image       = toImage(glyph)
    val bytes:    Array[Byte] = kind match {
      case "png"          => EncoderPNG.encode(snapshot).getBytes
      case "jpeg" | "jpg" => EncoderJPEG.encode(snapshot).getBytes
    }
    bytes
  }

  def toImage(glyph: Glyph): Image = {
    import io.github.humbleui.skija.{ImageInfo, Surface => SKSurface}
    import io.github.humbleui.types.IRect
    val w = glyph.w.toInt
    val h = glyph.h.toInt
    val surface  = SKSurface.makeRaster(ImageInfo.makeN32Premul(w, h))
    val canvas   = surface.getCanvas
    val drawing  = Surface(canvas, 1f)
    drawing.clear(0xFFFFFFFF)
    glyph.draw(drawing)
    surface.makeImageSnapshot(new IRect(0, 0, w, h))
  }

  /** DOES NOT WORK */
  @deprecated("malfunctioning") def glyph2SVG(glyph: Glyph, path: java.nio.file.Path): Unit = {
    import io.github.humbleui.skija.OutputWStream
    import io.github.humbleui.skija.svg.SVGCanvas
    import io.github.humbleui.types.Rect
    import unstyled.static.Label
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

  object Image {
    import io.github.humbleui.skija.{ColorAlphaType, ColorType, Image, ImageInfo}

    import java.awt.image.BufferedImage
    import java.io.File
    import java.nio.ByteBuffer
    import javax.imageio.ImageIO

    def read(path: Path): Glyph = {
      try {
        val buffered: BufferedImage = ImageIO.read(path.toFile)
        new ExternalImage(buffered)
      }
      catch {
        case ex: Exception => unstyled.Label("NO SUCH\nIMAGE", font=FontFamily("Courier", "bold", 50), fg=Brushes.black, bg=Brushes.red)
      }
    }

    def read(path: String): Glyph = read(Path.of(path))

    def readBytes(bytes: Array[Byte]): Glyph = {
      import java.awt.image.BufferedImage
      import java.io.ByteArrayInputStream
      import javax.imageio.ImageIO
      val bais = new ByteArrayInputStream(bytes)
      val buffered: BufferedImage = ImageIO.read(bais)
      new ExternalImage(buffered)
    }

    def readBase64(base64: String): Glyph = {
      val bytes: Array[Byte] = java.util.Base64.getDecoder.decode(base64.replaceAll("\\s", ""))
      External.Image.readBytes(bytes)
    }

    def readResource(id: String): Glyph = {
      val is = getClass.getResourceAsStream(id)
      val buffered: BufferedImage = ImageIO.read(is)
      new ExternalImage(buffered)
    }

    private def toSkijaImage(buffered: BufferedImage): Image = {
      val width  = buffered.getWidth
      val height = buffered.getHeight

      // Grab raw pixels (ARGB)
      val pixels = buffered.getRGB(0, 0, width, height, null, 0, width)

      // Copy into ByteBuffer as Skija expects (RGBA int32)
      val byteBuffer = ByteBuffer.allocate(pixels.length * 4)
      pixels.foreach { argb =>
        val a = (argb >>> 24) & 0xFF
        val r = (argb >>> 16) & 0xFF
        val g = (argb >>> 8) & 0xFF
        val b = argb & 0xFF

        // Premultiply into BGRA order (Skia default on macOS)
        val rp = (r * a + 127) / 255
        val gp = (g * a + 127) / 255
        val bp = (b * a + 127) / 255

        // Store as little-endian BGRA (lowest byte is blue)
        byteBuffer.put((bp & 0xFF).toByte)
        byteBuffer.put((gp & 0xFF).toByte)
        byteBuffer.put((rp & 0xFF).toByte)
        byteBuffer.put((a  & 0xFF).toByte)
      }
      byteBuffer.rewind()

      val info: ImageInfo = new ImageInfo(width, height, ColorType.BGRA_8888, ColorAlphaType.PREMUL)
      io.github.humbleui.skija.Image.makeRasterFromBytes(info, byteBuffer.array(), width * 4)
    }

    class ExternalImage(theBufferedImage: BufferedImage, override val fg: Brush=Brushes.transparent, override val bg: Brush=Brushes.transparent) extends Glyph {
      override def toString: String = s"ExternalImage($diagonal)"
      val theImage = toSkijaImage(theBufferedImage)

      /**
       * Draw the glyph on the surface at its given size (as if at the origin).
       */
      def draw(surface: Surface): Unit = {
        surface.drawImage(theImage)
      }

      /**
       * The diagonal size of the glyph
       */
      val diagonal: Vec = Vec(theBufferedImage.getWidth, theBufferedImage.getHeight)

      /** A copy of this glyph; perhaps with different foreground/background */
      def copy(fg: Brush=fg, bg: Brush=bg): Glyph = new ExternalImage(theBufferedImage, fg, bg)

    }

  }


}
