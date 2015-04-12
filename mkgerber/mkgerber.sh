#!/bin/bash
set -e

fab_specs="$(cat <<EOT
-                   elecrow     oshpark
.fab.gbr            :ignore     :ignore
.outline.gbr        .GML        .GKO
.top.gbr            .GTL        .GTL
.topmask.gbr        .GTS        .GTS
.topsilk.gbr        .GTO        .GTO
.toppaste.gbr       :ignore     :ignore
.bottom.gbr         .GBL        .GBL
.bottommask.gbr     .GBS        .GBS
.bottomsilk.gbr     .GBO        .GBO
.bottompaste.gbr    :ignore     :ignore
.group1.gbr         .GL2        .G2L
.group2.gbr         .GL3        .G3L
.group3.gbr         .GL4        .G4L
.group4.gbr         .GL5        .G5L
.group5.gbr         .GL6        .G6L
.group6.gbr         .GL7        .G7L
.group7.gbr         .GL8        .G8L
.group8.gbr         .GL9        .G9L
.plated-drill.cnc   -PTH.TXT    :ignore
.unplated-drill.cnc -NPTH.TXT   :ignore
.merged-drill.cnc   :ignore     .XLN
README.TXT          :postproc   :ignore
.zip                :zip        :zip
EOT
)"

function postproc-elecrow
{
    readme="${outdir}/README.TXT"
    echo "Outline is in ${outbase}.GML" > "${readme}"
    if [ -e "${output}-NPTH.TXT" ]; then
        echo "Contains non-plated holes in ${outbase}-NPTH.TXT" >> "${readme}"
    fi
    files="${files[@]} ${readme}"
}

function usage
{
    echo "Usage: $(basename $0) [-f|--fab  <fab>] [-o|--output <output basename>] [-c|--clean] <input.pcb>"
    echo "    -f|--fab    The fab for which to prepare the gerber files"
    echo "                Supported fabs:"
    echo "${fab_specs}" | head -n1 | (
        read -a fabs
        fab_idx=1; while [ ${fab_idx} -lt ${#fabs[@]} ]; do
            echo "                    ${fabs[${fab_idx}]}"
            fab_idx=$(( ${fab_idx} + 1 ))
        done
    )
    echo "                Default: elecrow"
    echo
    echo "    -o|--output The basename of for the output files."
    echo "                Example: \"./mydir/mypcb\" will generate files named "mypcb.\*""
    echo "                         in the directory ./mydir"
    echo "                Default: \"./<input>-<fab>/<input>.\*\""
    echo
    echo "    -c|--clean  Deletes the output directory before generating the output files"
}

function generate
{
    # find the requested fab
    read -a fabs
    fab_idx=1; while [ ${fab_idx} -lt ${#fabs[@]} -a "${fabs[$fab_idx]}" != "${fab}" ]; do
        fab_idx=$(( ${fab_idx} + 1 ))
    done
    if [ ${fab_idx} -ge ${#fabs[@]} ]; then echo "Unknown fab: ${fab}"; exit 1; fi

    # generate gerbers
    pcb -x gerber --gerberfile ${output} ${input}

    # merge plated and unplated drill files
    merged="${output}.merged-drill.cnc"
    plated="${output}.plated-drill.cnc"
    unplated="${output}.unplated-drill.cnc"
    if [ -e "${plated}" -a -e "${unplated}" ]; then
        gerbv -x drill -o ${merged} ${plated} ${unplated}
    elif [ -e ${plated} ]; then
        cp ${plated} ${merged}
    elif [ -e ${unplated} ]; then
        cp ${unplated} ${merged}
    fi

    # rename gerber files to match fab format
    while read -a xform; do
        from="${output}${xform[0]}"
        to="${xform[${fab_idx}]}"
        if [ "${to#:}" != "${to}" ]; then
            case "${to}" in
                :ignore)    ;;
                :zip)       zip -qj ${output}.zip ${files[@]};;
                :postproc)  eval "postproc-${fab}";;
                *)          echo "Unknown special command: ${to}"; exit 1;;
            esac
        elif [ -e ${from} ]; then
            to="${output}${to}"
            cp "${from}" "${to}"
            files="${files[@]} ${to}"
        fi
    done
}

eval set -- `getopt -o f:o:c --long fab:,output:,clean -n "$0" -- "$@"`

while [ "$1" != "--" ]; do
    case "$1" in
        -f|--fab)
            fab="$2"; shift 2;;
        -o|--output)
            output="$2"; shift 2;;
        -c|--clean)
            clean=1; shift;;
        *) echo "Unknown option: $1"; exit 1;;
    esac
done
shift
if [ -z "$1" ]; then usage; exit 1; fi
input="$1"; shift
if [ -n "$1" ]; then usage; exit 1; fi

fab="${fab:-"elecrow"}"
output="${output:-"$(basename "${input%.pcb}")-${fab}/$(basename ${input%.pcb})"}"
outdir=$(dirname ${output})
outbase="$(basename ${output})"
clean="${clean:-0}"
if [ ${clean} -eq 1 ]; then
    if [ "${outdir}" != "." ]; then
        rm -rf "${outdir}"
    fi
fi
mkdir -p ${outdir}

echo "${fab_specs}" | generate
