#!/bin/sh

REPO_PATH="/Users/avramaral/Documents/Colombia Orphanhood/orphanhood_municipalities_colombia"
INITIAL=185
FINAL=300  

cd "$REPO_PATH"

for i in $(seq $INITIAL $FINAL); do
    Rscript R/orphanhood.R $i "Municipality"
    Rscript R/orphanhood.R $i "Department"
done
