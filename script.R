setwd('C:/Users/lenda/Desktop/VUT_Ing/1_semestr/MPA-PRG/exercises/MPA_PRG_exercise_08')

library('Biostrings')

FindSorted <- function(perm_of_integers){
  for (i in 2:(length(perm_of_integers)-1)){
    if (!(perm_of_integers[i] == perm_of_integers[i-1]+1)){
      return(i)
    }
  }
}

IndicateAscending <- function(perm_of_integers){
  asc_vector <- rep(0, length(perm_of_integers))
  asc_vector[1] <- 1
  asc_vector[length(perm_of_integers)] <- 1

  for (i in 2:(length(perm_of_integers)-1)){
    if (perm_of_integers[i] == perm_of_integers[i-1]+1){
      asc_vector[i-1] <- 1
      asc_vector[i] <- 1
    }
  }
  return(asc_vector)
}

BreakPointSort <- function(perm_of_integers){
  perm_of_integers <- c(0, perm_of_integers, max(perm_of_integers)+1)

  while (TRUE){
    pos <- FindSorted(perm_of_integers)
    if (is.null(pos)){
      return(perm_of_integers[2:(length(perm_of_integers)-1)])
    }

    asc_parts <- IndicateAscending(perm_of_integers)

    if ((sum(asc_parts) == length(asc_parts)) || (sum(asc_parts) == (length(asc_parts))-1)){
      for (i in (length(perm_of_integers)):2){
        if (!(perm_of_integers[i] == perm_of_integers[i-1]+1)){
          pos_min <- i-1
          break
        }
      }
    }else{
      # desc_parts <- 1 - asc_parts
      # desc_permutations <- perm_of_integers * desc_parts
      # minimal_desc_part <- min(desc_permutations[desc_permutations != 0])

      minimal_desc_part <- min(perm_of_integers[asc_parts != 1])
      pos_min <- which(perm_of_integers == minimal_desc_part)

    }

    perm_of_integers <- c(perm_of_integers[1:(pos-1)],
                       rev(perm_of_integers[pos:(pos_min)]),
                       perm_of_integers[(pos_min+1):length(perm_of_integers)])

  }
}

perm_of_integers <- c(5, 1, 4, 3, 7, 8, 9, 2, 6)
# perm_of_integers <- c(4, 5, 3, 2, 1, 6, 7)
# perm_of_integers <- c(1, 2, 3,4,8,5, 6, 7)


# FindSorted(c(0, 1, 2, 3, 6, 7, 4, 5, 8))
# IndicateAscending(c(0, 4, 5, 3, 2, 1, 6, 7, 8))
BreakPointSort(perm_of_integers)