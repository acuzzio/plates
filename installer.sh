rm -r dist
cabal install
cp dist/build/Plates/Plates ~/bin/
cp dist/build/Plates/Plates ${PATH%%:*}
echo 'Installed !!!!'
