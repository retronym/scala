import java.io.*;
import java.lang.reflect.Method;
import java.nio.file.*;
import java.util.*;
import java.util.regex.*;

/**
 * In-process, warmed-up timing of the pattern matching phase, as reported by the compiler itself:
 *   Scala 2: `-Vstatistics:patmat`, the `patmat` phase line;
 *   Scala 3: `-Yprofile-enabled`, `runNs` of the MegaPhase containing `patternMatcher`.
 * Each data point is the min over reps of one compile stopping after that phase.
 * Usage: Bench <label> <s2|s3> <shapes> <Ns> <srcDir> <outCsv> <extra scalac args...>
 */
public class Bench {
  static Method process;
  static String flavour;
  static String[] extra;
  static String outDir;
  static ByteArrayOutputStream captured = new ByteArrayOutputStream();
  static volatile long compileStart = 0; // 0 when not compiling a measured point
  static volatile String current = "";
  static final Pattern S2Phase = Pattern.compile("^\\s*patmat\\s*:\\s*\\d+ spans, \\(\\)([0-9.]+)ms", Pattern.MULTILINE);

  /** returns {phase ms, total ms} */
  static double[] compile(String file) throws Exception {
    List<String> a = new ArrayList<>(List.of(extra));
    a.addAll(List.of("-nowarn", "-d", outDir));
    Path prof = Path.of(outDir, "prof.csv");
    if (flavour.equals("s2")) a.addAll(List.of("-Ystop-after:patmat", "-Vstatistics:patmat"));
    else { Files.deleteIfExists(prof); a.addAll(List.of("-Ystop-after:patternMatcher", "-Yprofile-enabled", "-Yprofile-destination", prof.toString())); }
    a.add(file);
    captured.reset();
    long t0 = System.nanoTime();
    Object r = process.invoke(null, (Object) a.toArray(new String[0]));
    double total = (System.nanoTime() - t0) / 1e6;
    boolean failed = flavour.equals("s2") ? !((Boolean) r) : (Boolean) r.getClass().getMethod("hasErrors").invoke(r);
    if (failed) throw new RuntimeException("compilation failed: " + file + "\n" + captured);
    double phase;
    if (flavour.equals("s2")) {
      Matcher m = S2Phase.matcher(captured.toString());
      if (!m.find()) throw new RuntimeException("no patmat timing in output:\n" + captured);
      phase = Double.parseDouble(m.group(1));
    } else {
      phase = -1;
      for (String line : Files.readAllLines(prof)) {
        String[] f = line.split(",");
        if (f[0].equals("MAIN") && f[5].contains("patternMatcher")) phase = Long.parseLong(f[10]) / 1e6;
      }
      if (phase < 0) throw new RuntimeException("no patternMatcher row in " + prof);
    }
    return new double[] { phase, total };
  }

  public static void main(String[] args) throws Exception {
    // before any Scala code runs, so that scala.Console captures this stream
    PrintStream realOut = System.out;
    PrintStream tee = new PrintStream(captured, true);
    System.setErr(tee); System.setOut(tee);

    String label = args[0]; flavour = args[1];
    String[] shapes = args[2].split(","); String[] ns = args[3].split(",");
    Path srcDir = Path.of(args[4]); Path out = Path.of(args[5]);
    extra = Arrays.copyOfRange(args, 6, args.length);
    outDir = Files.createTempDirectory("bench-out").toString();
    process = Class.forName(flavour.equals("s2") ? "scala.tools.nsc.Main" : "dotty.tools.dotc.Main").getMethod("process", String[].class);
    double budgetMs = Double.parseDouble(System.getProperty("bench.budgetMs", "20000"));
    double predictMs = Double.parseDouble(System.getProperty("bench.predictMs", "90000"));
    double hardMs = Double.parseDouble(System.getProperty("bench.hardMs", "120000"));
    String[] allArgs = args;

    long warmEnd = System.nanoTime() + (long) (Double.parseDouble(System.getProperty("bench.warmupSecs", "40")) * 1e9);
    int warm = 0;
    while (System.nanoTime() < warmEnd) for (String sh : shapes) { compile(srcDir.resolve(sh + "64.scala").toString()); warm++; }
    realOut.println(label + ": warmed up with " + warm + " compiles");

    boolean header = !Files.exists(out);
    try (var w = Files.newBufferedWriter(out, StandardOpenOption.CREATE, StandardOpenOption.APPEND)) {
      if (header) w.write("compiler,shape,n,phase_ms,total_ms,reps\n");
      // Watchdog: a compile can't be interrupted, so on a hard timeout record it, write the shapes
      // still to do to -Dbench.restFile, and halt with exit code 3; the driver script starts a fresh JVM for them.
      final var wFinal = w;
      Thread dog = new Thread(() -> {
        while (true) {
          try { Thread.sleep(500); } catch (InterruptedException e) { return; }
          long st = compileStart;
          if (st != 0 && (System.nanoTime() - st) / 1e6 > hardMs) {
            try {
              String[] cur = current.split(",");
              String line = String.format(Locale.ROOT, "%s,%s,%s,timeout,%.0f,0", label, cur[0], cur[1], hardMs);
              realOut.println(line); wFinal.write(line + "\n"); wFinal.flush();
              List<String> rest = new ArrayList<>();
              boolean after = false;
              for (String sh : shapes) { if (after) rest.add(sh); if (sh.equals(cur[0])) after = true; }
              Files.writeString(Path.of(System.getProperty("bench.restFile", outDir + "/rest")), String.join(",", rest));
              realOut.println(label + ": timed out on " + current + ", remaining " + rest);
            } catch (Exception e) { e.printStackTrace(realOut); }
            Runtime.getRuntime().halt(3);
          }
        }
      });
      dog.setDaemon(true); dog.start();
      for (String sh : shapes) {
        double prevTotal = -1;
        for (String nS : ns) {
          String f = srcDir.resolve(sh + nS + ".scala").toString();
          double phase = Double.MAX_VALUE, total = Double.MAX_VALUE;
          long spent = 0; int reps = 0;
          current = sh + "," + nS;
          while (reps < 2 || (reps < 9 && spent < 3000)) {
            long t0 = System.nanoTime();
            compileStart = t0;
            double[] r = compile(f);
            compileStart = 0;
            phase = Math.min(phase, r[0]); total = Math.min(total, r[1]);
            spent += (System.nanoTime() - t0) / 1_000_000; reps++;
            if (total > budgetMs) break; // one rep is enough for a long compile
          }
          String line = String.format(Locale.ROOT, "%s,%s,%s,%.2f,%.2f,%d", label, sh, nS, phase, total, reps);
          realOut.println(line); w.write(line + "\n"); w.flush();
          if (total > budgetMs) break;
          // skip the next size if the last doubling predicts it would take too long
          double growth = prevTotal > 0 ? Math.max(total / prevTotal, 2) : 8;
          if (total * growth > predictMs) { realOut.println(label + ": " + sh + " stops at N=" + nS + ", next predicted " + Math.round(total * growth / 1000) + " s"); break; }
          prevTotal = total;
        }
      }
    }
  }
}
