---
title: "Natural products"
output: html_document
date: "`r Sys.Date()`"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Asi es como corrí el archivo desde el Docker: 

```{r eval=FALSE}
docker run --rm -i -t -v $(pwd):/var/www/html/EvoMining/exchange -p 8005:80 nselem/evomining:latest /bin/bash
```

ya dentro del Docker:

```{r eval=FALSE}
perl startEvoMining.pl -c test_centralDB -g 05.Alcanivoracaceae_workingcopy -r Alcanivoracaceae_bins_merged.IDs -n ALKB_especializado.txt
```

en la carpeta 

/home/andres/NPDB
