#' @import dplyr
#' @import tibble
#' @import circlize
#' @import RColorBrewer
#'
#'
# define prefixes
ontoavida_prefix <- function() {
  return ("http://purl.obolibrary.org/obo/ONTOAVIDA_")
}

rdf_prefix <- function() {
  return ("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
}

ro_prefix <- function() {
  return ("http://purl.obolibrary.org/obo/RO_")
}

# default logic operations
logic_operation_default <- function() {
  return (c("equals", "exclusive or", "not-or", "and-not", "or", "orn-not", "and", "not-and", "not"))
}

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
  return(logic_operation_default())
}

# default instruction set
instruction_set_heads_cfg <- function() {
  return (data.frame(
      instruction = c(
        "nop-A",
        "nop-B",
        "nop-C",
        "if-n-equ",
        "if-less",
        "if-label",
        "mov-head",
        "jmp-head",
        "get-head",
        "set-flow",
        "shift-r",
        "shift-l",
        "inc",
        "dec",
        "push",
        "pop",
        "swap-stk",
        "swap",
        "add",
        "sub",
        "nand",
        "h-copy",
        "h-alloc",
        "h-divide",
        "IO",
        "h-search"
      ),
      letter = c("a",
                 "b",
                 "c",
                 "d",
                 "e",
                 "f",
                 "g",
                 "h",
                 "i",
                 "j",
                 "k",
                 "l",
                 "m",
                 "n",
                 "o",
                 "p",
                 "q",
                 "r",
                 "s",
                 "t",
                 "u",
                 "v",
                 "w",
                 "x",
                 "y",
                 "z"
      ),
      color = c("#E5CF6C",
                "#E5CF6C",
                "#E5CF6C",
                "#5983B4",
                "#5983B4",
                "#5983B4",
                "#5983B4",
                "#5983B4",
                "#5983B4",
                "#5983B4",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#91BD64",
                "#91BD64",
                "#91BD64",
                "#5CBE95",
                "#91BD64"
      )
    )
  )
}

# instruction set from file instset-heads-sex.cfg
instruction_set_heads_sex_cfg <- function() {
  return (data.frame(
      instruction = c(
        "nop-A",
        "nop-B",
        "nop-C",
        "if-n-equ",
        "if-less",
        "if-label",
        "mov-head",
        "jmp-head",
        "get-head",
        "set-flow",
        "shift-r",
        "shift-l",
        "inc",
        "dec",
        "push",
        "pop",
        "swap-stk",
        "swap",
        "add",
        "sub",
        "nand",
        "h-copy",
        "h-alloc",
        "divide-sex",
        "IO",
        "h-search"
      ),
      letter = c("a",
                 "b",
                 "c",
                 "d",
                 "e",
                 "f",
                 "g",
                 "h",
                 "i",
                 "j",
                 "k",
                 "l",
                 "m",
                 "n",
                 "o",
                 "p",
                 "q",
                 "r",
                 "s",
                 "t",
                 "u",
                 "v",
                 "w",
                 "x",
                 "y",
                 "z"
      ),
      color = c("#E5CF6C",
                "#E5CF6C",
                "#E5CF6C",
                "#5983B4",
                "#5983B4",
                "#5983B4",
                "#5983B4",
                "#5983B4",
                "#5983B4",
                "#5983B4",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#E77B6F",
                "#91BD64",
                "#91BD64",
                "#91BD64",
                "#5CBE95",
                "#91BD64"
      )
    )
  )
}

# instruction set from file instset-transsmt.cfg
instruction_set_transsmt_cfg <- function() {
  return (
    data.frame(
      instruction = c(
        "Nop-A",
        "Nop-B",
        "Nop-C",
        "Nop-D",
        "Val-Shifht-R",
        "Val-Shif-L",
        "Val-Nand",
        "Val-Add",
        "Val-Sub",
        "Val-Mult",
        "Val-Div",
        "Val-Mod",
        "Val-Inc",
        "Val-Dec",
        "SetMemory",
        "Inst-Read",
        "Inst-Write",
        "If-Equal",
        "If-Not-Equal",
        "If-Less",
        "If-Greater",
        "Head-Push",
        "Head-Pop",
        "Head-Move",
        "Search",
        "Push-Next",
        "Push-Prev",
        "Push-Comp",
        "Val-Delete",
        "Val-Copy",
        "IO",
        "Inject",
        "Divide-Erase"
      ),
      letter = c(
        "a",
        "b",
        "c",
        "d",
        "e",
        "f",
        "g",
        "h",
        "i",
        "j",
        "k",
        "l",
        "m",
        "n",
        "o",
        "p",
        "q",
        "r",
        "s",
        "t",
        "u",
        "v",
        "w",
        "x",
        "y",
        "z",
        "A",
        "B",
        "C",
        "D",
        "E",
        "F",
        "G"
      ),
      color = c(
        "#E5CF6C",
        "#E5CF6C",
        "#E5CF6C",
        "#E5CF6C",
        "#E77B6F",
        "#E77B6F",
        "#CB6686",
        "#CB6686",
        "#CB6686",
        "#CB6686",
        "#CB6686",
        "#CB6686",
        "#E77B6F",
        "#E77B6F",
        "#91BD64",
        "#91BD64",
        "#91BD64",
        "#5983B4",
        "#5983B4",
        "#5983B4",
        "#5983B4",
        "#5983B4",
        "#5983B4",
        "#5983B4",
        "#5CBE95",
        "#E77B6F",
        "#E77B6F",
        "#E77B6F",
        "#E77B6F",
        "#E77B6F",
        "#5CBE95",
        "#91BD64",
        "#91BD64"
      )
    )
  )
}

#' Get the genetic language of Avida
#'
#' @description List of the instruction codes comprising the genetic language of
#' digital organisms in Avida.
#'
#' @param inst_set Name of the instruction set. It must be one of the
#' following: "heads" (default), "heads-sex", or `transsmt`. The names
#' correspond to the instruction set configuration files (e.g.,
#' `instset-heads.cfg` for "heads").
#'
#' @return Data frame. Columns: "instruction", "letter", "color"
#'
#' @export
#'

instruction_set <- function(inst_set = "heads") {
  instructions <- switch(inst_set,
                     "heads" = {
                       instructions <- instruction_set_heads_cfg()
                     },
                     "heads-sex" = {
                       instructions <- instruction_set_heads_sex_cfg()
                     },
                     "transsmt" = {
                       instructions <- instruction_set_transsmt_cfg()
                     },{
                       stop(paste0("Instruction set ", inst_set, " does not exist."))
                     })
  return(instructions)
}
