(ns scrapperplugin.clj.nature)

;;use declarative programming to guess the required natures

;;http://sdqweb.ipd.kit.edu/wiki/JDT_Tutorial:_Creating_Eclipse_Java_Projects_Programmatically
(defn add-javanature [eclipseproject source-dirs lib-dirs bin-dirs]
  (let [description (.getDescription eclipseproject)
        natures (.getNatureIds description)
        new-natures (into-array (conj (seq natures) (.. org.eclipse.jdt.core.JavaCore NATURE_ID)))]
    (.setNatureIds description new-natures)
    (.setDescription eclipseproject description nil)))


(defn create-javaproject [eclipseproject source-dirs lib-dirs bin-dirs]
  (let [javaproject (.. org.eclipse.jdt.core.JavaCore create eclipseproject)
        vm-install (.. org.eclipse.jdt.launching.JavaRuntime getDefaultVMInstall)
        locations (seq (.. org.eclipse.jdt.launching.JavaRuntime getLibraryLocations vmInstall))
        entries (map 
                  (fn [location]
                    (.. org.eclipse.jdt.core.JavaCore newLibraryEntry 
                      (.getSystemLibraryPath location nil nil)))
                  locations)]
    ;;set bin dirs
    (map (fn [x] 
           (.setOutputLocation javaProject (.getFullPath x) nil))
         bin-dirs)
    ;;set libs
    (.setRawClasspath javaproject
    (map (fn [entry]
           (
    
    
    
          List<IClasspathEntry> entries = new ArrayList<IClasspathEntry>();
IVMInstall vmInstall = JavaRuntime.getDefaultVMInstall();
LibraryLocation[] locations = JavaRuntime.getLibraryLocations(vmInstall);
for (LibraryLocation element : locations) {
 entries.add(JavaCore.newLibraryEntry(element.getSystemLibraryPath(), null, null));
}
//add libs to project class path
javaProject.setRawClasspath(entries.toArray(new IClasspathEntry[entries.size()]), null);