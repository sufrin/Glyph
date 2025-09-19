package org.sufrin.glyph

import java.awt.datatransfer.{DataFlavor, FlavorEvent}

object Clipboard {

  
  class ClipboardWatcher (action: () => Unit) {
    import java.awt.{datatransfer, _}
    val sysClipboard: java.awt.datatransfer.Clipboard = Toolkit.getDefaultToolkit.getSystemClipboard

    locally {
      sysClipboard.addFlavorListener(
        new java.awt.datatransfer.FlavorListener()
        {
          def flavorsChanged(event: FlavorEvent): Unit = {
            if (sysClipboard.isDataFlavorAvailable(DataFlavor.javaFileListFlavor))
              if (sysClipboard.isDataFlavorAvailable(DataFlavor.stringFlavor))
          }
        })
    }

  }


  def onString(action: String => Unit): ClipboardWatcher = new ClipboardWatcher(()=>action)

  }

}
