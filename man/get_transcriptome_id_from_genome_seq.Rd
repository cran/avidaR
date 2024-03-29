% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_transcriptome_id_from_genome_seq.R
\name{get_transcriptome_id_from_genome_seq}
\alias{get_transcriptome_id_from_genome_seq}
\title{Get transcriptome from genome sequence}
\usage{
get_transcriptome_id_from_genome_seq(
  genome_seq,
  seed_id = FALSE,
  transcriptome_seq = FALSE,
  transcriptome_pos = FALSE,
  genome_id = FALSE,
  triplestore
)
}
\arguments{
\item{genome_seq}{String of letters or a list of strings.}

\item{seed_id}{Integer (from 1 to 1000), a vector of integer
values, or a logical value. This integer is used for starting the
pseudo-random number generator that represents the environment experiencing a
digital organism. If a logical value is used, TRUE returns data found in all
environments and FALSE (by default) returns only distinct data regardless of
the seed.}

\item{transcriptome_seq}{Logical value (TRUE/FALSE) to show/hide this column
(FALSE by default).}

\item{transcriptome_pos}{Logical value (TRUE/FALSE) to show/hide this column
(FALSE by default).}

\item{genome_id}{Logical value (TRUE/FALSE) to show/hide this column
(FALSE by default).}

\item{triplestore}{Object of class triplestore_access which manages database
access.}
}
\value{
Data frame. Columns: "seed_id" (optional), "genome_seq",
"transcriptome_id", "transcriptome_seq" (optional), "transcriptome_pos"
(optional).
}
\description{
Get the transcriptome of a digital organism having a specific
linear string of letters representing the instruction codes that make up its
genome for a list of seeds used for starting the pseudo-random
number generator (i.e., a set of environments).
}
\examples{

# Create triplestore object
avidaDB <- triplestore_access$new()

# Set access options
avidaDB$set_access_options(
  url = "https://graphdb.fortunalab.org",
  user = "public_avida",
  password = "public_avida",
  repository = "avidaDB_test"
)

# Get sequences for genome_1 and genome_2
sequence1 <- get_genome_seq_from_genome_id(
  genome_id = 1,
  triplestore = avidaDB
)$genome_seq[1]

sequence2 <- get_genome_seq_from_genome_id(
  genome_id = 2,
  triplestore = avidaDB
)$genome_seq[1]

# Single genome
get_transcriptome_id_from_genome_seq(
  genome_seq = sequence1,
  triplestore = avidaDB
)

# More than one genome
get_transcriptome_id_from_genome_seq(
  genome_seq = c(sequence1, sequence2),
  transcriptome_seq = TRUE,
  triplestore = avidaDB
)

# At seed_1 and seed_2
get_transcriptome_id_from_genome_seq(
  genome_seq = sequence2,
  seed_id = c(1,2),
  transcriptome_seq = TRUE,
  transcriptome_pos = TRUE,
  triplestore = avidaDB
)


}
