#!/usr/bin/env bash

set -e
set -v

# NOTES:
# "toc"                     -> treated just like another page, its location can be changed
# "--window-status loaded"  -> when window.status is set to "loaded", wkhtmltopdf knows js is loaded

THIS_SCRIPT_DIR=$(dirname $0)
ROOT_DIR=$THIS_SCRIPT_DIR/..
SPEC_SRC_DIR=$ROOT_DIR/spec
SPEC_BUILD_DIR=$ROOT_DIR/build/spec
SPEC_PDF_BUILD_DIR=$ROOT_DIR/build/spec-pdf
PDF=$SPEC_BUILD_DIR/spec.pdf

mkdir -p $SPEC_PDF_BUILD_DIR

# arrays, so that an argument can contain a space
WKHTML_OPTS=(--print-media-type --window-status loaded --javascript-delay 1000 --load-error-handling ignore --enable-local-file-access --footer-center '[page]' --footer-font-name 'Luxi Sans')
WKHTML_TOC=(toc --xsl-style-sheet $SPEC_SRC_DIR/spec-toc.xslt)

# exclude index.html, prepend SPEC_PDF_BUILD_DIR path
HTML_FILES=$(ls $SPEC_PDF_BUILD_DIR | grep -vx 'index.html' | grep '\.html$' | while read line; do echo "$SPEC_PDF_BUILD_DIR/$line"; done)

echo "Making spec.pdf with HTML files: "
echo $SPEC_PDF_BUILD_DIR/index.html $HTML_FILES

# first goes index.html, then TOC, then rest
rm -f $PDF
# The exit status of wkhtmltopdf is not trusted: it exits with 1 when a subresource fails to load,
# such as a script the spec layouts load from a CDN, even with `--load-error-handling ignore`,
# and still writes the PDF. Check for the PDF instead.
wkhtmltopdf "${WKHTML_OPTS[@]}" $SPEC_PDF_BUILD_DIR/index.html "${WKHTML_TOC[@]}" $HTML_FILES $PDF || true

# fail if we didn't get a PDF file out, or an empty one
if [ ! -s $PDF ] ; then echo "wkhtmltopdf produced no PDF, or an empty one: $PDF" >&2 ; exit 1 ; fi
