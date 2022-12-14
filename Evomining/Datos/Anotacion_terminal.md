Anotacion terminal
================

## En la terminal de bash

## Download Genomes of Rhodobacteraceae family from NCBI/Genbank/referencesonly

<https://www.ncbi.nlm.nih.gov/data-hub/genome/?taxon=31989&reference_only=true&typical_only=true>

Unpackage the genomes and put it in the same directory:

    ls */*/*/*fna | wc -l
    mv */*/*/*fna .
    gunzip *gz
    rm -r genbank
    rm -r refseq

Then we need to change the names of the downloaded genomes

### To check the new name:

    ls *fna |while read line; do fullname=$(head -1 $line);namecut=$(echo $fullname|cut -d ' ' -f2,3); string=${namecut// /_}; accesion=$(echo $line|cut -d'_' -f1,2,3|cut -d'.' -f1);newname=$(echo $string\_$accesion.fna);echo "${line}" "${newname}"; done

### Change names with mv

    ls *fna |while read line; do fullname=$(head -1 $line);namecut=$(echo $fullname|cut -d ' ' -f2,3); string=${namecut// /_}; accesion=$(echo $line|cut -d'_' -f1,2,3|cut -d'.' -f1);newname=$(echo $string\_$accesion.fna);mv "${line}" "${newname}"; done

### Example:

``` output
Yoonia_maritima__GCA_003003285.fna
Paracoccus_pantotrophus__GCA_008824185.fna            Yoonia_sediminilitoris__GCA_003058085.fna
Paracoccus_ravus__GCA_004522175.fna                   Youngimonas_vesicularis__GCA_004799325.fna
Paracoccus_salipaludis__GCA_002287065.fna             Zongyanglinia_huanghaiensis__GCA_009753675.fna
Paracoccus_salsus__GCA_021556615.fna                  Zongyanglinia_marina__GCA_005771405.fna
```

## Anottating genomes with prokka

### For visualization

    ls *fna | while read line; do specie=$(echo "$line"|cut -d'_' -f2); genera=$(echo "$line"|cut -d '_' -f1); isolate=$(echo $line|cut -d '_' -f3,4,5|cut -d '.' -f1);echo prokka $line --kingdom Bacteria -genus "${genera}" -species "${specie}" --addgenes --usegenus --prefix ${genera}\_${specie}\_"${isolate}"\.prokka --outdir "${genera}"\_"${specie}"\_"${isolate}"\_prokka; done

### For run:

    ls *fna | while read line; do specie=$(echo "$line"|cut -d'_' -f2); genera=$(echo "$line"|cut -d '_' -f1); isolate=$(echo $line|cut -d '_' -f3,4,5|cut -d '.' -f1); prokka $line --kingdom Bacteria -genus "${genera}" -species "${specie}" --addgenes --usegenus --prefix ${genera}\_${specie}\_"${isolate}"\.prokka --outdir "${genera}"\_"${specie}"\_"${isolate}"\_prokka; done

#### not use this part

``` note
ls *fasta | while read line; do specie=$(echo "$line"|cut -d'_' -f2); genera=$(echo "$line"|cut -d '_' -f1); isolate=$(echo $line|cut -d '_' -f3,4|cut -d '.' -f1);echo prokka $line --kingdom Bacteria -genus "${genera}" -species "${specie}" --addgenes --usegenus --prefix ${genera}\_${specie}\_"${isolate}"\.prokka --outdir "${genera}"\_"${specie}"\_"${isolate}"\_prokka; done
```

### Run evomining with the genomes

    docker run --rm -i -t -v $(pwd):/var/www/html/EvoMining/exchange -p 8000:80 nselem/evomining:latest /bin/bash
