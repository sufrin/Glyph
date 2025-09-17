import java.awt.*;
import java.awt.datatransfer.*;
import java.awt.image.BufferedImage;
import java.io.*;
import java.util.List;

/**
 * Simple cross-type clipboard helper.
 */
public final class ClipboardHelper {

    private ClipboardHelper() {} // no instances

    /** Put plain text onto the clipboard. */
    public static void putText(String text) {
        StringSelection selection = new StringSelection(text);
        getClipboard().setContents(selection, null);
    }

    /** Put an image onto the clipboard. */
    public static void putImage(final Image image) {
        Transferable t = new Transferable() {
            @Override
            public DataFlavor[] getTransferDataFlavors() {
                return new DataFlavor[]{DataFlavor.imageFlavor};
            }
            @Override
            public boolean isDataFlavorSupported(DataFlavor flavor) {
                return DataFlavor.imageFlavor.equals(flavor);
            }
            @Override
            public Object getTransferData(DataFlavor flavor) {
                if (isDataFlavorSupported(flavor)) return image;
                throw new UnsupportedFlavorException(flavor);
            }
        };
        getClipboard().setContents(t, null);
    }

    /** Put a list of files onto the clipboard (copy semantics). */
    public static void putFiles(List<File> files) {
        Transferable t = new Transferable() {
            @Override
            public DataFlavor[] getTransferDataFlavors() {
                return new DataFlavor[]{DataFlavor.javaFileListFlavor};
            }
            @Override
            public boolean isDataFlavorSupported(DataFlavor flavor) {
                return DataFlavor.javaFileListFlavor.equals(flavor);
            }
            @Override
            public Object getTransferData(DataFlavor flavor) {
                if (isDataFlavorSupported(flavor)) return files;
                throw new UnsupportedFlavorException(flavor);
            }
        };
        getClipboard().setContents(t, null);
    }

    /** Put arbitrary binary data (as bytes) onto the clipboard. */
    public static void putBytes(byte[] bytes) {
        try {
            DataFlavor binaryFlavor =
                new DataFlavor("application/octet-stream;class=java.io.InputStream", "Binary Data");
            Transferable t = new Transferable() {
                @Override
                public DataFlavor[] getTransferDataFlavors() {
                    return new DataFlavor[]{binaryFlavor};
                }
                @Override
                public boolean isDataFlavorSupported(DataFlavor flavor) {
                    return binaryFlavor.equals(flavor);
                }
                @Override
                public Object getTransferData(DataFlavor flavor) {
                    if (isDataFlavorSupported(flavor))
                        return new ByteArrayInputStream(bytes);
                    throw new UnsupportedFlavorException(flavor);
                }
            };
            getClipboard().setContents(t, null);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException("Clipboard binary flavor setup failed", e);
        }
    }

    /** Convenience: get system clipboard. */
    private static Clipboard getClipboard() {
        return Toolkit.getDefaultToolkit().getSystemClipboard();
    }

    // --- Example main for quick testing ---
    public static void main(String[] args) {
        putText("Hello clipboard!");
        // putImage(new BufferedImage(50, 50, BufferedImage.TYPE_INT_RGB));
        // putFiles(List.of(new File("/path/to/file.txt")));
        // putBytes(new byte[]{1,2,3,4,5});
    }
}

