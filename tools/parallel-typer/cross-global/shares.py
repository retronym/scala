#!/usr/bin/env python3
"""Inclusive shares of a collapsed profile: a sample counts for a bucket if any frame in its stack matches."""
import sys,re,collections
B=[('symbol loading: all (SymbolLoader.complete)',r'SymbolLoaders\$SymbolLoader\.complete'),
('  classfile loader',r'SymbolLoaders\$ClassfileLoader\.doComplete'),
('  package loader',r'SymbolLoaders\$PackageLoader\.doComplete'),
('unpickler: all, eager and lazy (UnPickler)',r'pickling/UnPickler'),
('classfile parser',r'classfile/ClassfileParser\.'),
('classpath listing',r'nsc/classpath/|nsc/util/ClassPath'),
('Definitions.init',r'Definitions\$DefinitionsClass\.init'),
('new Global (constructor)',r'nsc/Global\.<init>'),
('baseTypeSeq',r'baseTypeSeq|BaseTypeSeq'),
('baseClasses',r'baseClasses'),
('findMember',r'FindMember|findMember'),
('asSeenFrom',r'asSeenFrom|AsSeenFromMap'),
('adaptToNewRunMap',r'adaptToNewRunMap'),
('implicit search',r'Implicits\$ImplicitSearch|inferImplicit'),
('parser',r'parser/Parsers|SyntaxAnalyzer'),
('typer phase',r'Analyzer\$typerFactory'),
('backend (jvm phase)',r'backend/jvm/'),
]
tot=0;main=0;c=collections.Counter()
rx=[(n,re.compile(p)) for n,p in B]
for l in open(sys.argv[1]):
    s,_,n=l.rstrip().rpartition(' '); n=int(n); tot+=n
    if 'scala/tools/nsc/Global' not in s: continue
    main+=n
    for name,r in rx:
        if r.search(s): c[name]+=n
print('samples',tot,'in the compiler',main)
for name,_ in B: print('%-48s %6.1f%%'%(name,100*c[name]/max(1,main)))
