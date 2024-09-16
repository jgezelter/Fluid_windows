read_uv_vis_is <- function(path){
  #reads the data and makes a dataframe from it, with numbered column headers
  data <- read_csv(path, 
                   col_names = T, 
                   skip = 1,
                   show_col_types = F, 
                   name_repair = "unique_quiet") %>% 
    clean_names()
  
  #reads the data and makes a dataframe containing only the column headers
  headers <- read_csv(path, 
                      col_names = T, 
                      show_col_types = F, 
                      name_repair = "unique_quiet") %>% 
    slice(c(1))
  
  #some helper data for later
  cols <- ncol(data)
  rows <- nrow(data)
  out <- data.frame()
  
  #grabs pairs of columns from the data dataframe and the matching headers from the headers dataframe
  #pastes them together
  #then "lengthens" the data by pasting the pairs of columns top to bottom 
  #includes wavelength, intensity data, data path, and sample name
  for(i in 1:(cols / 2)){
    sample <- names(headers)[[(2 * i) - 1]]
    
    data_cur <- data[,((2 * i) - 1):((2 * i))] 
    data_cur <- data_cur %>% 
      rename_with(.fn = function(x){case_when(str_detect(x, "wavelength_nm") ~ "wavelength_nm", 
                                              .default = "value")}) %>% 
      mutate(sample_id = sample, 
             path = path) %>% 
      filter(!str_detect(wavelength_nm, "[^[:digit:]]")) %>% 
      mutate(wavelength_nm = as.numeric(wavelength_nm), 
             value = as.numeric(value))
    
    if(!str_detect(sample, "Baseline")){
      out <- out %>% rbind(data_cur)
    }
    

    
  }
  
  
  return(out)
}
