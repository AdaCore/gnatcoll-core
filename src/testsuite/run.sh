#!/bin/sh

gnatmake -q -P default

expect() {
  cmd="$1"
  expected="$2"

  $cmd > out.$$ 2>&1
  if diff $expected out.$$ >/dev/null ; then
     :
  else
     echo "======= $cmd $expected"
     diff $expected out.$$
  fi
  rm -f out.$$
}

expect "./traces1" "traces1.out"

expect "./traces2" "traces2.out"
diff outfile.out outfile
rm -f outfile
