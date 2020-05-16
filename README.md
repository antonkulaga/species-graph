# species-graph
Repository to write sparql queries to the species graph

It is a helper repository
It contains classes to deal with Samples, Expression and Orthology Tables.

Note: the code is quite ugly as it is mostly used for internal scripting

Usage
-----
```
Usage:
    species-graph orthologs
    species-graph expressions
    species-graph genes_statistics

Species GRAPH Client

Subcommands:
    orthologs
        Generate orthology tables
    expressions
        Generate expression tables
```

Application is also paked into docker container.

Indexes:
--------
getting index of samples
```
docker run -v /data/species:/data/species quay.io/comp-bio-aging/species-graph run samples_index --output /data/species/samples.tsv
```

getting index of species (only species for which we have samples considered)
```
docker run -v /data/species:/data/species quay.io/comp-bio-aging/species-graph run species_index --output /data/species/species.tsv
```

Expressions examples
--------------------

extracting genes from several samples (note: feel free to change /data/species folder/volume to whatever you like)
```
docker run -v /data/species:/data/species quay.io/comp-bio-aging/species-graph expressions --path /data/species/tests/igf1_mtor.tsv --samples "SRR1521445;SRR3715877;SRR5115668;SRR1287653;SRR1287654" --genes "ENSG00000198793;ENSG00000017427" --server http://10.40.3.21:7200  --rewrite 
```
same genes+samples but with --verbose command which provides additional information of orthologs which are taken
```
docker run -v /data/species:/data/species quay.io/comp-bio-aging/species-graph expressions --path /data/species/tests/igf1_mtor.tsv --samples "SRR1521445;SRR3715877;SRR5115668;SRR1287653;SRR1287654" --genes "ENSG00000198793;ENSG00000017427" --server http://10.40.3.21:7200  --rewrite 
```
adding one2many genes which are provided with ";" as separator
```
docker run -v /data/species:/data/species quay.io/comp-bio-aging/species-graph expressions --path /data/species/tests/igf1_mtor.tsv --samples "SRR1521445;SRR3715877;SRR5115668;SRR1287653;SRR1287654" --genes "ENSG00000198793;ENSG00000017427" --one2many separator --server http://10.40.3.21:7200  --rewrite 
```
adding one2many genes which are summed up
```
docker run -v /data/species:/data/species quay.io/comp-bio-aging/species-graph expressions --path /data/species/tests/igf1_mtor.tsv --samples "SRR1521445;SRR3715877;SRR5115668;SRR1287653;SRR1287654" --genes "ENSG00000198793;ENSG00000017427" --one2many sum --server http://10.40.3.21:7200  --rewrite 
```

same genes+samples but with sumation of orthologs which belong to the same gene
```
docker run -v /data/species:/data/species quay.io/comp-bio-aging/species-graph expressions --path /data/species/tests/igf1_mtor.tsv --samples "SRR1521445;SRR3715877;SRR5115668;SRR1287653;SRR1287654" --genes "ENSG00000198793;ENSG00000017427" --server http://10.40.3.21:7200  --rewrite 
```
extracting all expressions for selected genes
```
docker run -v /data/species:/data/species quay.io/comp-bio-aging/species-graph expressions --path /data/species/tests/all.tsv --genes "ENSG00000198793;ENSG00000017427" --server http://10.40.3.21:7200  --rewrite 
```
extracting all expressions for selected genes and sorting them by class and tissue
```
docker run -v /data/species:/data/species quay.io/comp-bio-aging/species-graph expressions --split by_class_and_tissue --path /data/species/tests/by_class --genes "ENSG00000198793;ENSG00000017427" --server http://10.40.3.21:7200  --rewrite 
```
extracting all expressions of all human genes and sorting them by class and tissue
```
docker run -v /data/species:/data/species quay.io/comp-bio-aging/species-graph expressions --split by_class_and_tissue --one2many separator --path /data/species/tests/by_class --server http://10.40.3.21:7200  --rewrite 
```


KNOWN ISSUES
------------

In very rare cases there might be GraphDB memory and query errors