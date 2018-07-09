package scala.reflect.internal.jpms;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.Processor;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.Name;
import javax.lang.model.element.TypeElement;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Set;

public class ModuleInfoParser {

    public static void main(String[] args) {
        ModuleElementHolder holder = parseModuleInfo(Paths.get("/tmp/module-info.java"));
        if (holder != null) {
            System.out.println("module " + holder.qualifiedName);
            for (ModuleElement.Directive directive : holder.directives) {
                directive.accept(new ModuleElement.DirectiveVisitor<Void, Void>() {

                    @Override
                    public Void visitRequires(ModuleElement.RequiresDirective d, Void aVoid) {
                        System.out.print("requires ");
                        if (d.isStatic()) System.out.println("static ");
                        if (d.isTransitive()) System.out.println("transitive ");
                        System.out.println(d.getDependency().getQualifiedName());
                        return null;
                    }

                    @Override
                    public Void visitExports(ModuleElement.ExportsDirective d, Void aVoid) {
                        System.out.print("exports " + d.getPackage());
                        d.getPackage();
                        List<? extends ModuleElement> targetModules = d.getTargetModules();
                        if (targetModules != null) {
                            for (ModuleElement targetModule : targetModules) {
                                System.out.print(" to " + targetModule.getQualifiedName());
                            }
                        }
                        System.out.println();
                        return null;
                    }

                    @Override
                    public Void visitOpens(ModuleElement.OpensDirective d, Void aVoid) {
                        return null;
                    }

                    @Override
                    public Void visitUses(ModuleElement.UsesDirective d, Void aVoid) {
                        return null;
                    }

                    @Override
                    public Void visitProvides(ModuleElement.ProvidesDirective d, Void aVoid) {
                        return null;
                    }
                }, null);
            }
        }

    }

    public static ModuleElementHolder parseModuleInfo(Path path) {
        JavaCompiler systemJavaCompiler = ToolProvider.getSystemJavaCompiler();
        StandardJavaFileManager fileManager = systemJavaCompiler.getStandardFileManager(new NullDiagnosticListener<>(), Locale.getDefault(), Charset.defaultCharset());
        Iterable<? extends JavaFileObject> files = fileManager.getJavaFileObjects(path);
        JavaCompiler.CompilationTask task = systemJavaCompiler.getTask(new PrintWriter(System.out), fileManager, new NullDiagnosticListener<>(), Collections.emptyList(), Collections.emptyList(), files);
        final ModuleElementHolder[] moduleElementHolder = new ModuleElementHolder[1];
        Processor processor = new AbstractProcessor() {
            @Override
            public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
                return false;
            }

            @Override
            public synchronized void init(ProcessingEnvironment processingEnv) {
                Set<? extends ModuleElement> allModuleElements = processingEnv.getElementUtils().getAllModuleElements();
                for (ModuleElement moduleElement : allModuleElements) {
                    Name qualifiedName = moduleElement.getQualifiedName();
                    if (!(qualifiedName.contentEquals("java.base") || qualifiedName.contentEquals(""))) {
                        List<? extends ModuleElement.Directive> directives = moduleElement.getDirectives();
                        moduleElementHolder[0] = new ModuleElementHolder(qualifiedName, directives);
                    }
                }
                super.init(processingEnv);
            }
        };
        task.setProcessors(Collections.singleton(processor));
        task.call();
        return moduleElementHolder[0];
    }

    public static class ModuleElementHolder {
        private final Name qualifiedName;
        private final List<? extends ModuleElement.Directive> directives;

        public ModuleElementHolder(Name qualifiedName, List<? extends ModuleElement.Directive> directives) {
            this.qualifiedName = qualifiedName;
            this.directives = directives;
        }

        public Name getQualifiedName() {
            return qualifiedName;
        }

        public List<? extends ModuleElement.Directive> getDirectives() {
            return directives;
        }
    }
}
