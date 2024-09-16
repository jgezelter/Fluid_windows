library(tidyverse)


read_FTIR_IS_film <- function(path){
  out <- read_csv(path, 
                  col_names = F, 
                  id = "path", 
                  show_col_types = F) %>% 
    mutate(wavelength_nm = (1 / X1) * (10 ^ 7), 
           value = X2, 
           .keep = "unused")
  
  return(out)
}