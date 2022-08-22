# avidaR 1.1.3

**Improvements**

* More **friendly error messages** for helping in solving issues caused by service availability, not valid user requests, unauthorized access, and others.

**New features**

* Added **timeout** parameter in method **set\_access\_options()** to limit the amount of time waiting data from the service.

**Bug fixes**

* Removed top-level code which creates `triplestore` object and set default access options.
  Read more about how to properly [connect to 'avidaDB'](https://gitlab.com/raul.ortega/avidaR/-/blob/main/README.md#connect-to-avidadb).

# avidaR 1.1.2

**Improvements**

* If seed\_id is not specified, a **randomly-chosen seed\_id value is used to speed up the execution** of the following functions:
  * get\_genome\_id\_from\_logic\_operation
  * get\_genome\_id\_from\_phenotype\_id
  * get\_transcriptome\_id\_from\_logic\_operation
  * get\_transcriptome\_id\_from\_phenotype\_id
  * get\_transcriptome\_id\_from\_logic\_operation
  * get\_transcriptome\_id\_from\_phenotype\_id
* The function **get\_mutant\_at\_pos** has been expanded:
  * The function parameter **genome\_id has been added**, and the previous parameters (**inst\_replaced , inst\_replaced\_by, and pos**) **can be now omitted** by the user.
  * If **genome\_id is not specified**, a randomly chosen genome\_id (between those provided by the function get\_genome\_id\_of\_wild\_type\_organisms) will be used.
  * If **inst\_replaced** and/or **inst\_replaced\_by** is/are not specified, all mutations at the specified position on the genome are shown.
  * If the parameter **pos** is not specified, all mutations at all positions on the genome are shown (depending on the values of inst\_replaced and inst\_replaced\_by).

**New features**

* Added function **get\_db\_summary** that will provide a summary of the database.
* Added **get\_genome\_id\_of\_wild\_type\_organisms** to identify the genomes of the organisms that were used as wild-type organisms.

**Bug fixes**

* Removed a line from the clause WHERE that **was slowing down the execution of the function get\_transcriptome\_id\_from\_phenotype\_id**.
* The use of **exponential notation was prevented** because it provided empty results when requesting data by identifiers like genome\_10000000.

# avidaR 1.0.1

* Initial release