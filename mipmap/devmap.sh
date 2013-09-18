#!/bin/sh
mipmapdir=$(dirname $0)
schematics=$(grep '^schematics' < $1 | sed 's/^schematics\s*//')
devmaps_dir=$(grep '^devmaps-dir' < $1 | sed 's/^devmaps-dir\s*//')
for sch in ${schematics}; do
    cp ${sch} DEVMAP.${sch}
    gschlas -e DEVMAP.${sch}
    gaf shell -L ${mipmapdir} -s ${mipmapdir}/devmap.scm DEVMAP.${sch} -L ${devmaps_dir}
done
sed '/^schematics/ s/\(\s\+\)\([^\s]\+\.sch\)/\1DEVMAP.\2/g' < $1 > DEVMAP.$1

