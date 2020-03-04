kappa <- function(misc) {

  po <- (misc[1] + misc[4]) / sum(misc)
  pc <- crossprod(colSums(misc) / sum(misc), rowSums(misc) / sum(misc))[1]

  (po - pc) / (1 - pc)

}
