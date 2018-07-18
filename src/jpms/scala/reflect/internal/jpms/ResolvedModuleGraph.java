package scala.reflect.internal.jpms;

import java.lang.module.Configuration;
import java.lang.module.ModuleDescriptor;
import java.lang.module.ModuleReference;
import java.lang.module.ResolvedModule;
import java.util.*;

import static scala.reflect.internal.jpms.JpmsClasspathImpl.UNNAMED_MODULE_NAME;

public class ResolvedModuleGraph {
    private String defaultModule;
    private final Configuration configuration;

    public ResolvedModuleGraph(String defaultModule, Configuration configuration) {
        this.defaultModule = defaultModule;
        this.configuration = configuration;
    }

    public boolean hasModule(String moduleName) {
        return configuration.findModule(moduleName).isPresent();
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

    public String getDefaultModule() {
        return defaultModule;
    }
}
