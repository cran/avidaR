# avidaR 1.1.2

**Improvements**

- If seed_id is not specified, a **randomly-chosen seed_id value is used to speed up the execution** of the following functions:

    - get_genome_id_from_logic_operation
    - get_genome_id_from_phenotype_id
    - get_transcriptome_id_from_logic_operation
    - get_transcriptome_id_from_phenotype_id
    - get_transcriptome_id_from_logic_operation
    - get_transcriptome_id_from_phenotype_id

- The function **get_mutant_at_pos** has been expanded:

    - The function parameter **genome_id has been added**, and the previous parameters (**inst_replaced , inst_replaced_by, and pos**) **can be now omitted** by the user.
    - If **genome_id is not specified**, a randomly chosen genome_id (between those provided by the function get_genome_id_of_wild_type_organisms) will be used.
    - If **inst_replaced** and/or **inst_replaced_by** is/are not specified, all mutations at the specified position on the genome are shown.
    - If the parameter **pos** is not specified, all mutations at all positions on the genome are shown (depending on the values of inst_replaced and inst_replaced_by).

**New features**

- Added function **get_db_summary** that will provide a summary of the database.
- Added **get_genome_id_of_wild_type_organisms** to identify the genomes of the organisms that were used as wild-type organisms.

**Bug fixes**

- Removed a line from the clause WHERE that **was slowing down the execution of the function get_transcriptome_id_from_phenotype_id**.
- The use of **exponential notation was prevented** because it provided empty results when requesting data by identifiers like genome_10000000.

# avidaR 1.0.1

- Initial release
