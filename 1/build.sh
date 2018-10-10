#! /bin/bash
if [ "$1" = "clean" ]; then
  rm -f Main *.o *.hi
else
  ghc -O Main.hs
fi
