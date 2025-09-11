/**
        Mac reports of free space on volume(s)

        The "opportunistic"  and "important" answers differ by the
        size of the purgeable space.
*/

public class MacFreeSpace {
    static {
        System.loadLibrary("macfree"); // loads libmacfree.dylib
    }

    // Finder-style "important usage" space (what Finder usually shows)
    public static native long getImportantAvailableSpace(String path);

    // "opportunistic usage" space (excludes what macOS could reclaim aggressively)
    public static native long getOpportunisticAvailableSpace(String path);

    public static void show(String path) {
        long gb = 1000*1000*1000;

        long important = getImportantAvailableSpace(path);
        long opportunistic = getOpportunisticAvailableSpace(path);

        System.out.print(path);
        System.out.print(" I: " + important/gb + " gb");
        System.out.println(" O: " + opportunistic/gb + " gb");
    }


    public static void main(String[] args) {
        if (args.length==0) { show("/"); } else {
           for (int i=0; i<args.length; i++) { show(args[i]); }
        }
        
    }
}