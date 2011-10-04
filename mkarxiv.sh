#!/bin/bash 

ARXIV=arXiv

TITLE=iproc

FILES=( \
    iproc.tex \
    iproc.bbl \
    iproc-macros.sty \
    statsoc.cls \
    figures/recipient-counts.png \
    figures/boot-resid.pdf \
    figures/nobs-by-nexp.png \
    figures/resid-by-nexp.png \
    figures/dyad.pdf \
    figures/triad.pdf \
    tables/group-dynamic.tex \
    tables/group-static.tex
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
