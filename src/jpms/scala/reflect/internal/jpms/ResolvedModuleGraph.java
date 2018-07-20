package scala.reflect.internal.jpms;

import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import java.io.IOException;
import java.lang.module.Configuration;
import java.lang.module.ModuleDescriptor;
import java.lang.module.ResolvedModule;
import java.nio.file.Path;
import java.util.*;

import static scala.reflect.internal.jpms.JpmsClasspathSupport.UNNAMED_MODULE_NAME;

public class ResolvedModuleGraph {
    private String currentModule;
    private final Configuration configuration;
    private StandardJavaFileManager fileManager;
    private Map<String, Set<JavaFileManager.Location>> modulePatchLocations = new HashMap<>();

    public ResolvedModuleGraph(String currentModule, Configuration configuration, StandardJavaFileManager fileManager) throws IOException {
        this.currentModule = currentModule;
        this.configuration = configuration;
        this.fileManager = fileManager;

        if (fileManager.hasLocation(StandardLocation.PATCH_MODULE_PATH)) {
            Iterable<Set<JavaFileManager.Location>> sets = fileManager.listLocationsForModules(StandardLocation.PATCH_MODULE_PATH);
            for (Set<JavaFileManager.Location> set: sets) {
                for (JavaFileManager.Location location: set) {
                    String moduleName = fileManager.inferModuleName(location);
                    Set<JavaFileManager.Location> locations = modulePatchLocations.computeIfAbsent(moduleName, (key) -> new HashSet<>());
                    locations.add(location);
                }
            }
        }
    }

    public Map<String, Set<String>> accessibleModulePackages(String siteModuleName) {
        if (siteModuleName.equals("")) siteModuleName = UNNAMED_MODULE_NAME;

        HashMap<String, Set<String>> result = new HashMap<>();
        String finalSiteModuleName = siteModuleName;
        Optional<ResolvedModule> siteModule = configuration.findModule(siteModuleName);
        if (!siteModule.isPresent()) return result;

        Set<ResolvedModule> readModules = siteModuleName.equals(UNNAMED_MODULE_NAME) ? configuration.modules() : siteModule.get().reads();
        for (ResolvedModule readModule: readModules) {
            HashSet<String> packages = new HashSet<>();
            result.put(readModule.name(), packages);
            ModuleDescriptor descriptor = readModule.reference().descriptor();
            if (descriptor.isAutomatic()) {
                packages.add("*");
            } else {
                for (ModuleDescriptor.Exports exports: descriptor.exports()) {
                    if (!exports.isQualified() || exports.targets().contains(finalSiteModuleName)) {
                        packages.add(exports.source());
                    }
                }
            }
        }
        return result;
    }

    public String moduleForSourceFile(Path path) throws IOException {
        String result = currentModule;
        if (path == null) return result;

        if (fileManager.hasLocation(StandardLocation.PATCH_MODULE_PATH)) {
            JavaFileObject sourceFile = fileManager.getJavaFileObjects(path).iterator().next();
            Iterable<Set<JavaFileManager.Location>> sets = fileManager.listLocationsForModules(StandardLocation.PATCH_MODULE_PATH);
            for (Set<JavaFileManager.Location> set: sets) {
                for (JavaFileManager.Location location: set) {
                    if (fileManager.contains(location, sourceFile)) {
                        return fileManager.inferModuleName(location);
                    }
                }
            }
        }
        return result;
    }
}
