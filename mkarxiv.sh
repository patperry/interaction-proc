#!/bin/bash 

ARXIV=arXiv

TITLE=iproc

FILES=( \
    iproc.tex \
    iproc_supplementary.pdf \
    iproc.bbl \
    iproc-macros.sty \
    statsoc.cls \
    figures/recipient-counts.png \
    figures/boot-resid-main.pdf \
    figures/nobs-by-nexp-main.png \
    figures/resid-by-nexp-main.png \
    figures/dyad-main.pdf \
    figures/triad-main.pdf \
    figures/multicast-error.pdf \
    tables/group-dynamic-main.tex \
    tables/group-static-main.tex
)

DISTDIR=${ARXIV}/${TITLE}


rm -rf "${ARXIV}"

mkdir -p "${DISTDIR}/figures"
mkdir -p "${DISTDIR}/tables"

for FILE in "${FILES[@]}"
do
    cp "${FILE}" "${DISTDIR}/${FILE}"
done 


pushd ${ARXIV} > /dev/null
rm -f ${TITLE}.tar.gz
tar czvf "${TITLE}.tar.gz" "${TITLE}" > /dev/null
popd > /dev/null
