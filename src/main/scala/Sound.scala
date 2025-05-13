package org.sufrin.glyph

object Sound {

  import javax.sound.sampled._
  import java.io.File
  import java.io.IOException

  /** A re-playable (shortish) sound clip  */
  case class Clip(file: File) {
    val canPlay: Boolean = file.exists()
    lazy val audioIn = AudioSystem.getAudioInputStream(file)
    lazy val clip = AudioSystem.getClip()
    lazy val msLength = clip.getMicrosecondLength / 1000
    locally {
      if (canPlay) {
        if (audioIn.markSupported()) {
          audioIn.mark(file.length.toInt)
          clip.addLineListener {
            event =>
              if (event.getType() == LineEvent.Type.STOP) {
                clip.close()
                audioIn.reset()
              }
          }
        }
      }
    }

    /** Play the clip once  */
    def play(synchronous: Boolean = false): Unit = if (canPlay) {
      clip.open(audioIn)
      clip.start()
      if (synchronous) Thread.sleep(msLength)
    }
  }

}

