#!/bin/bash

set +x
set -e


if ! git diff-index --quiet HEAD --; then
  echo "Can't deploy - repository is in dirty state"
  exit 1
fi

git_hash=$(git rev-parse HEAD)
s3_path=s3://rednosedreindeer/$git_hash/

rm -rf www/

brunch build --env production

tmp=$(mktemp -d)
for i in $(find www -type f \( -name '*.html' -o -name '*.css' -o -name '*.woff' -o -name '*.ttf' -o -name '*.eot' -o -name '*.js' \))
do
	file $i
	mkdir -p $tmp/$(dirname $i)
	gzip -c $i > $tmp/$i
done

s3cmd sync --progress --acl-public --reduced-redundancy --verbose --add-header 'Cache-Control: max-age=604800' \
  --add-header 'Content-Encoding: gzip' --add-header 'Cache-Control: max-age=604800' $tmp/www/* $s3_path       \
  --exclude '*.*' --include '*.html' --include '*.js'  --include '*.css'  --include '*.woff' --include '*.ttf' --include '*.eot'
s3cmd sync --progress --acl-public --reduced-redundancy --verbose --add-header 'Cache-Control: max-age=604800' \
                                        --add-header 'Cache-Control: max-age=604800'  www/*     $s3_path       \
  --exclude '*.html' --exclude '*.js'   --exclude '*.css' --exclude '*.woff' --exclude '*.ttf' --exclude '*.eot'

rm -rf $tmp
