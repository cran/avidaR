# avidaR: an R library for performing complex queries on digital organisms stored in avidaDB.


<!-- badges: start -->

[![pipeline status](https://gitlab.com/fortunalab/avidaR/badges/main/pipeline.svg)](https://gitlab.com/fortunalab/avidaR/-/commits/main) [![coverage report](https://gitlab.com/fortunalab/avidaR/badges/main/coverage.svg)](https://gitlab.com/fortunalab/avidaR/-/commits/main) [![Version on
CRAN](https://www.r-pkg.org/badges/version/avidaR?color=brightgreen)](https://cran.r-project.org/package=avidaR) [![Total downloads on
CRAN](https://cranlogs.r-pkg.org/badges/grand-total/avidaR?color=brightgreen)](https://cran.r-project.org/package=avidaR) 

<!-- badges: end -->

## Introduction.

avidaR was born as part of a project focused on providing semantics to data resulting from experiments carried out in an Artificial Life software platform ([Avida](https://avida.devosoft.org)) for studying the evolution of self-replicating computer programs, known as digital organisms. This package provides users of the R programming language with an easy-to-use tools for performing complex queries on avidaDB, a semantic database that stores genomes and transcriptomes of more than a million digital organisms. The avidaR library was developed by the [Computational Biology Lab](https://fortunalab.org) of the Doñana Biological Station (EBD), a research institute of the Spanish National Research Council (CSIC) based in Seville (Spain).

## Installation.

avidaR depends on the following packages:
- [base64enc](https://cran.r-project.org/package=base64enc)
- [xml2](https://cran.r-project.org/package=xml2)
- [httr](https://cran.r-project.org/package=httr)
- [dplyr](https://cran.r-project.org/package=dplyr)
- [readr](https://cran.r-project.org/package=readr)
- [tidyr](https://cran.r-project.org/package=tidyr)
- [tibble](https://cran.r-project.org/package=tibble)
- [circlize](https://cran.r-project.org/package=circlize)
- [RColorBrewer](https://cran.r-project.org/package=RColorBrewer)
- [R6](https://cran.r-project.org/package=R6)
- [devtools](https://cran.r-project.org/package=devtools): needed for the GitLab installation.

Please, first install those packages with `install.packages`.
Then, install avidaR from GitLab using the following command:

`devtools::install_gitlab("fortunalab/avidaR@main")`

## Usage.

avidaR is loaded as follows:

``` r
library(avidaR)
```

## Connect to avidaDB.

avidaDB is a semantic database (or triple-store) on genomes and transcriptomes of more a million digital organisms stored as RDF data. It allows querying data using the [SPARQL query language](https://www.w3.org/TR/rdf-sparql-query/). The library avidaR can connect to triple-stores that support the [RDF4J server REST API](https://rdf4j.org/documentation/reference/rest-api/) such as [GraphDB](https://graphdb.ontotext.com/). Since avidaDB is implemented in GraphDB, a basic connection (requiring no password or requiring basic HTTP user-pass authentication) or a connection secured with an API access token can be established.

avidaR provides a `triplestore_access` class to manage access options and retrieve data through the database server API. In order to get access to the entire database, you should first create the `triplestore` object and run the `set_access_options()` method as follows:

``` r
# create object of class triplestore_access
triplestore <- triplestore_access$new()

# set access options do avidaDB
triplestore$set_access_options(
    url = "https://graphdb.fortunalab.org",
    user = "public_avida",
    password = "public_avida",
    repository = "avidaDB"
  )
```
## Get data from avidaDB.

The following function can be used to get the genome sequence of a single genome (e.g., genome_id = 1):

```  r
get_genome_seq_from_genome_id(genome_id = 1, triplestore = triplestore)
```

or to get the genome sequences of multiple genomes at once:

```  r
get_genome_seq_from_genome_id(genome_id = c(1, 2, 3), triplestore = triplestore)
```

Please, use the R help command to get more details about any specific function by writing the name of the function preceded by the symbol `?`:

``` r
?get_genome_seq_from_genome_id
```

## List of available functions grouped by the target entity:

### Get the genome of a digital organism:

- `get_genome_id_from_logic_operation()`
- `get_genome_id_from_phenotype_id()`
- `get_genome_id_from_transcriptome_id()`
- `get_genome_id_from_genome_seq()`
- `get_genome_seq_from_genome_id()`

### Get the phenotype encoded by the genome of a digital organism:

- `get_phenotype_id_from_logic_operation()`
- `get_phenotype_id_from_genome_id()`
- `get_phenotype_id_from_genome_seq()`
- `get_phenotype_id_from_transcriptome_id()`

### Get the logic operations (i.e., traits) defining the phenotype of a digital organism:

- `get_logic_operations_from_phenotype_id()`

### Get the transcriptome executed by a digital organism:

- `get_transcriptome_id_from_logic_operation()`
- `get_transcriptome_id_from_genome_id()`
- `get_transcriptome_id_from_genome_seq()`
- `get_transcriptome_id_from_phenotype_id()`
- `get_transcriptome_seq_from_transcriptome_id()`

### Get the tandem repeat contained in the transcriptome of a digital organism:

- `get_tandem_id_from_logic_operation()`
- `get_tandem_id_from_genome_id()`
- `get_tandem_id_from_genome_seq()`
- `get_tandem_id_from_phenotype_id()`
- `get_tandem_seq_from_tandem_id()`


### Miscellaneous functions:

- `get_db_summary()`
- `get_genome_id_of_wild_type_organisms()`
- `get_mutant_at_pos()`
- `convert_seq_into_org()`
- `plot_transcriptome()`

## Source code

[avidaR](https://gitlab.com/fortunalab/avidaR) was developed by [Raúl Ortega](https://gitlab.com/raul.ortega).
