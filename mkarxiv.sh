#!/bin/bash 

ARXIV=arXiv

TITLE=iproc

FILES=( \
    iproc.tex \
    iproc.bbl \
    iproc-macros.sty \
    statsoc.cls \
    figures/boot-resid.pdf \
    figures/recipient-counts.png \
    figures/reciprocation-bc.pdf \
    tables/deviance.tex \
    tables/employee-summary.tex \
    tables/group-effects-bc.tex
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
