IFS=$'\n'
set -f
for f in $(find ../ -name '*.log' -or -name '*.out' -or -name '*.synctex.gz' -or -name '*.fls' -or -name '*.fdb_latexmk' -or -name '*.aux'); do rm "$f"; done
unset IFS
set +f
