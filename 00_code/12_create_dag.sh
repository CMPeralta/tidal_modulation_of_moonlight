#!/usr/bin/env bash 

snakemake --dag | dot -Tpng > 02_visuals/dag.png


