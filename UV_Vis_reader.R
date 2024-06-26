library(tidyverse)
library(janitor)

read_uv_vis <- function(file){
  data <- read_csv(file, col_names = T, skip = 1) %>% 
    clean_names()
  
  path <- file
  
  headers <- read_csv(file, col_names = T) %>% slice(c(1))
  
  cols <- ncol(data)
  rows <- nrow(data)
  out <- data.frame()
  
  for(i in 1:(cols / 2)){
    sample <- names(headers)[[(2 * i) - 1]]
    
    data_cur <- data[,((2 * i) - 1):((2 * i))] 
    data_cur <- data_cur %>% 
      mutate(measurement_type = str_extract(names(data_cur)[2], ".+(?=[:digit:])")) %>% 
      rename_with(.fn = function(x){case_when(str_detect(x, "wavelength_nm") ~ "wavelength_nm", 
                                              str_detect(x, "measurement_type") ~ "measurement_type",
                                              .default = "value")}) %>% 
      mutate(sample = sample, 
             id = basename(path), 
             date = basename(dirname(path))) %>% 
      separate(id, into = c("machine", "attachment", "mode"), sep = "_") %>% 
      separate(sample, 
               into = c("solvent", "dye", "concentration", "measurement_num"), 
               sep = "_", 
               remove = F) %>% 
      mutate(mode = str_extract(mode, ".+(?=\\.csv)"))
    if(!str_detect(sample, "Baseline")){
      out <- out %>% rbind(data_cur)
    }
    
  }
  return(out)
}