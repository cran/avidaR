#' @import dplyr
#' @import tibble
#' @import circlize
#' @import RColorBrewer
#'
#'
# define prefixes
ontoavida_prefix <- "http://purl.obolibrary.org/obo/ONTOAVIDA_"
`rdf_prefix` <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

# default logic operations
logic_operation_default <- c("equals", "exclusive or", "not-or", "and-not", "or", "orn-not", "and", "not-and", "not")

#' Get the list of logic operations that a digital organism can compute
#' 
#' @description List of the logic operations that a digital organism can
#' execute.
#' 
#' @return Vector of character.
#' 
#' @export
#' 
logic_operation <- function() {
  return(logic_operation_default)
}

# default instruction set
instructions <- data.frame(
  instruction = c(
    "nop-A", "nop-B", "nop-C", "if-n-equ", "if-less", "if-label", "mov-head", "jmp-head", "get-head", "set-flow", "shift-r", "shift-l",
    "inc", "dec", "push", "pop", "swap-stk", "swap", "add", "sub", "nand", "h-copy", "h-alloc", "h-divide", "IO", "h-search"
  ),
  letter = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"),
  color = c(
    "#E5CF6C", "#E5CF6C", "#E5CF6C", "#5983B4", "#5983B4", "#5983B4", "#5983B4", "#5983B4", "#5983B4", "#5983B4", "#E77B6F", "#E77B6F",
    "#E77B6F", "#E77B6F", "#E77B6F", "#E77B6F", "#E77B6F", "#E77B6F", "#CB6686", "#CB6686", "#CB6686", "#91BD64", "#91BD64", "#91BD64", "#5CBE95", "#5CBE95"
  )
)

#' Get the genetic language of Avida
#' 
#' @description List of the instruction codes comprising the genetic language of
#' digital organisms in Avida.
#' 
#' @return Data frame. Columns: "instruction", "letter", "color"
#' 
#' @export
#' 

instruction_set <- function() {
  return(instructions)
}

.onAttach <- function(libname, pkgname) {
  # define message
  avidaDB_msg <- paste0("Please, change the name of the repository to ",
                        "access the entire avidaDB database.\nBy ",
                        "default, avidaR points to a small subset for ",
                        "testing (avidaDB_test).\nIn order to do it, ",
                        "you should run the following:\n\n",
                        "triplestore$set_access_options(\n", 
                        "  url = \"https://graphdb.fortunalab.org\",\n",
                        "  user = \"public_avida\",\n",
                        "  password = \"public_avida\",\n",
                        "  repository = \"avidaDB\"\n",
                        ")"
  )
  # show message at startup
  packageStartupMessage(avidaDB_msg)
}
