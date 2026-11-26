#!/usr/bin/env python3
"""JaCoCo exec -> XML -> lines covered per scope file and the largest unreached methods.

usage: cov.py <exec> <xml-out> [build-pack]
"""
import os, subprocess, sys, json
import xml.etree.ElementTree as ET

HOME = os.path.expanduser('~')
CLI = HOME + '/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jacoco/org.jacoco.cli/0.8.13/org.jacoco.cli-0.8.13-nodeps.jar'
JAVA = HOME + '/.sdkman/candidates/java/21.0.12+1.1-tem/bin/java'
SCOPE = {
    'scala/tools/nsc/transform': ['CleanUp.scala', 'Erasure.scala', 'UnCurry.scala', 'Constructors.scala', 'Fields.scala'],
    'scala/tools/nsc/typechecker': ['RefChecks.scala', 'ConstantFolder.scala'],
    'scala/tools/nsc/backend/jvm': ['BCodeBodyBuilder.scala', 'BCodeSkelBuilder.scala'],
    'scala/tools/nsc/backend': ['ScalaPrimitives.scala'],
}


def main():
    exe, xml = sys.argv[1], sys.argv[2]
    pack = sys.argv[3] if len(sys.argv) > 3 else '/Users/jzaugg-virtus/code/.worktrees/scala/w4-int/build/pack'
    subprocess.run([JAVA, '-jar', CLI, 'report', exe, '--classfiles', pack + '/lib/scala-compiler.jar',
                    '--classfiles', pack + '/lib/scala-reflect.jar', '--xml', xml], check=True, capture_output=True)
    root = ET.parse(xml).getroot()
    cov, meths = {}, []
    for pkg in root.iter('package'):
        files = SCOPE.get(pkg.get('name'))
        if not files:
            continue
        for sf in pkg.findall('sourcefile'):
            if sf.get('name') in files:
                for c in sf.findall('counter'):
                    if c.get('type') == 'LINE':
                        mi, co = int(c.get('missed')), int(c.get('covered'))
                        cov[sf.get('name').replace('.scala', '')] = [co, mi + co]
        for cls in pkg.findall('class'):
            if cls.get('sourcefilename') not in files:
                continue
            for m in cls.findall('method'):
                for c in m.findall('counter'):
                    if c.get('type') == 'INSTRUCTION' and int(c.get('covered')) == 0:
                        meths.append((int(c.get('missed')), cls.get('name').rsplit('/', 1)[-1] + '.' + m.get('name')))
    meths.sort(reverse=True)
    print(json.dumps({'coverage': cov, 'largest_unreached': [m for _, m in meths[:25]]}, indent=1))


if __name__ == '__main__':
    main()
