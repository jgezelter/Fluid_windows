library(tidyverse)

read_FTIR_IS_fluid <- function(path){
  out <- read_csv(path, 
                  col_names = F, 
                  id = "path", 
                  show_col_types = F) %>% 
    mutate(wavelength_nm = (1 / X1) * (10 ^ 7), 
           value = X2, 
           date = basename(dirname(path)), 
           meta = basename(path),
           .keep = "none") %>% 
    separate(meta, 
             into = c("machine",
                      "attachment", 
                      "mode", 
                      "solvent", 
                      "dye",
                      "concentration", 
                      "measurement_num"), 
             sep = "_") %>% 
    mutate(measurement_num = str_extract(measurement_num, "[:alnum:]+"))
  
  return(out)
}

read_FTIR_IS_film <- function(path){
  out <- read_csv(path, 
                  col_names = F, 
                  id = "path", 
                  show_col_types = F) %>% 
    mutate(wavelength_nm = (1 / X1) * (10 ^ 7), 
           value = X2, 
           date = basename(dirname(path)), 
           meta = basename(path),
           .keep = "none") %>% 
    separate(meta, 
             into = c("film",
                      "power", 
                      "time", 
                      "attachment", 
                      "mode"), 
             sep = "_") 
  
  return(out)
}