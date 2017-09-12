rm -rf html
zim --export --index-page index -r --template ZeroFiveEight -o html --overwrite ./
find . -name '*.html' -exec gzip -k {} \;
cp .htaccess html/
rsync -av --delete html/ daveops:daveops.net/
