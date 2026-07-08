package org.sufrin.glyph

object Sound {

  import javax.sound.sampled._
  import java.io.File
  import java.io.IOException

  /**
   * A re-playable (shortish) sound clip.
   *
   */
  case class Clip(file: File) {
    override def toString: String = if (canPlay) s"Clip($file)" else s"UnplayableClip($file)"
    val canPlay: Boolean = file.exists()
    lazy val audioIn = AudioSystem.getAudioInputStream(file)
    lazy val clip = AudioSystem.getClip()
    locally {
      if (canPlay) {
        clip.open(audioIn)
        clip.addLineListener {
          // Reset the clip to the start of the audio
          event => if (event.getType() == LineEvent.Type.STOP) clip.setFramePosition(0)
        }
      }
    }

    /** Start playing the clip; or restart it if it is already playing.  */
    def play(): Unit = if (canPlay) {
      if (clip.isActive) clip.stop()
      clip.start()
    }
  }

  def Clip(filePath: String): Clip = Clip(new File(filePath))

}

