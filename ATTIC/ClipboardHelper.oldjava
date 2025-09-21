package org.sufrin.glyph;

import java.awt.*;
import java.awt.datatransfer.*;
import java.awt.image.BufferedImage;
import java.io.*;
import java.util.List;

public final class ClipboardHelper {

    private ClipboardHelper() {} // no instances

    // -------------------
    // TEXT
    // -------------------
    public static void putText(String text) {
        StringSelection selection = new StringSelection(text);
        getClipboard().setContents(selection, null);
    }

    public static String getText() {
        try {
            if (getClipboard().isDataFlavorAvailable(DataFlavor.stringFlavor)) {
                return (String) getClipboard().getData(DataFlavor.stringFlavor);
            }
        } catch (UnsupportedFlavorException | IOException e) {
            // ignore
        }
        return null;
    }

    // -------------------
    // IMAGE
    // -------------------
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
            public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException {
                if (isDataFlavorSupported(flavor)) return image;
                throw new UnsupportedFlavorException(flavor);
            }
        };
        getClipboard().setContents(t, null);
    }

    public static Image getImage() {
        try {
            if (getClipboard().isDataFlavorAvailable(DataFlavor.imageFlavor)) {
                return (Image) getClipboard().getData(DataFlavor.imageFlavor);
            }
        } catch (UnsupportedFlavorException | IOException e) {
            // ignore
        }
        return null;
    }

    // -------------------
    // FILES
    // -------------------
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
            public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException {
                if (isDataFlavorSupported(flavor)) return files;
                throw new UnsupportedFlavorException(flavor);
            }
        };
        getClipboard().setContents(t, null);
    }

    @SuppressWarnings("unchecked")
    public static List<File> getFiles() {
        try {
            if (getClipboard().isDataFlavorAvailable(DataFlavor.javaFileListFlavor)) {
                return (List<File>) getClipboard().getData(DataFlavor.javaFileListFlavor);
            }
        } catch (UnsupportedFlavorException | IOException e) {
            // ignore
        }
        return null;
    }

    // -------------------
    // BYTES
    // -------------------
    /*
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

    public static byte[] getBytes() {
        try {
            DataFlavor binaryFlavor =
                    new DataFlavor("application/octet-stream;class=java.io.InputStream", "Binary Data");
            if (getClipboard().isDataFlavorAvailable(binaryFlavor)) {
                try (InputStream in = (InputStream) getClipboard().getData(binaryFlavor);
                     ByteArrayOutputStream out = new ByteArrayOutputStream()) {
                    in.transferTo(out);
                    return out.toByteArray();
                }
            }
        } catch (UnsupportedFlavorException | IOException | ClassNotFoundException e) {
            // ignore
        }
        return null;
    }
*/
    // -------------------
    // INTERNAL
    // -------------------
    private static Clipboard getClipboard() {
        return Toolkit.getDefaultToolkit().getSystemClipboard();
    }

    // -------------------
    // TEST MAIN
    // -------------------
    public static void main(String[] args) {
        // Text
        putText("Hello clipboard!");
        System.out.println("Text: " + getText());

        // Image
        BufferedImage img = new BufferedImage(50, 50, BufferedImage.TYPE_INT_RGB);
        putImage(img);
        System.out.println("Image: " + getImage());

        // Files
        putFiles(List.of(new File("/tmp/test.txt")));
        System.out.println("Files: " + getFiles());

//        // Bytes
//        putBytes(new byte[]{1, 2, 3, 4});
//        byte[] bytes = getBytes();
//        System.out.println("Bytes: " + (bytes == null ? null : bytes.length + " bytes"));
    }
}