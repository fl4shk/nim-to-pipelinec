#!/bin/bash
#nim cc --compileOnly --nimcache:./outdir --genScript:on main.nim
#MY_NIM_OPTS="cpp -c --genScript:on --nimcache:./outdir"
#MY_NIM_OPTS="cpp --genScript:on --nimcache:./outdir"
#MY_NIM_OPTS="cpp --nimcache:./outdir"
MY_NIM_OPTS="$MY_NIM_OPTS c"
MY_NIM_OPTS="$MY_NIM_OPTS --nimcache:./outdir"
#MY_NIM_OPTS="$MY_NIM_OPTS --noMain:on"
#MY_NIM_OPTS="$MY_NIM_OPTS --os:standalone "
#MY_NIM_OPTS="$MY_NIM_OPTS --mm:none -d:useMalloc"
#MY_NIM_OPTS="$MY_NIM_OPTS -d:useMalloc"
#MY_NIM_OPTS="$MY_NIM_OPTS --threads:off"
MY_NIM_OPTS="$MY_NIM_OPTS -d:release"
MY_NIM_OPTS="$MY_NIM_OPTS --objChecks:off --fieldChecks:off"
MY_NIM_OPTS="$MY_NIM_OPTS --rangeChecks:off --boundChecks:off"
MY_NIM_OPTS="$MY_NIM_OPTS --overflowChecks:off"
MY_NIM_OPTS="$MY_NIM_OPTS --floatChecks:off --nanChecks:off --infChecks:off"

#MY_NIM_OPTS="$MY_NIM_OPTS --nilChecks:off"

#MY_NIM_OPTS="$MY_NIM_OPTS --checks:off"
#MY_NIM_OPTS="$MY_NIM_OPTS --cpu:arm"
#MY_NIM_OPTS="$MY_NIM_OPTS --tlsEmulation:off"
#MY_NIM_OPTS="$MY_NIM_OPTS --compileOnly"
#MY_NIM_OPTS="$MY_NIM_OPTS --gcc.exe:snowhousecpu-unknown-elf-gcc"
#MY_NIM_OPTS="$MY_NIM_OPTS --gcc.linkerexe:snowhousecpu-unknown-elf-gcc"
#MY_NIM_OPTS="$MY_NIM_OPTS --gcc.cpp.exe:snowhousecpu-unknown-elf-g++"
#MY_NIM_OPTS="$MY_NIM_OPTS --gcc.cpp.linkerexe:snowhousecpu-unknown-elf-g++"
#MY_NIM_OPTS="$MY_NIM_OPTS --l:-Wl,--relax"
MY_NIM_OPTS="$MY_NIM_OPTS --parallelBuild:0"
MY_NIM_OPTS="$MY_NIM_OPTS --experimental:caseStmtMacros"
#MY_NIM_OPTS="$MY_NIM_OPTS --run"

#echo nim $MY_NIM_OPTS src/Main.nim #panicoverride.nim
nim $MY_NIM_OPTS src/Main.nim #panicoverride.nim
mv src/Main ./Main
./Main
#nim --run Main
