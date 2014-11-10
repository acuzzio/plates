cd ..
rm -r dist
cabal install
cp dist/build/Plates/Plates ~/bin/
echo 'Installed !!!!'
cd src
