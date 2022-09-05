outlierValue <- function(x){ return( 3 * IQR(x) )}


reLabel <- function(cluster){
  
  cluster = ifelse(cluster==1, "prefer open",
                   ifelse(cluster==2, "weak preference",
                          ifelse(cluster==3, "prefer forest",
                                 ifelse(cluster==4, "prefer intermediate",cluster))))
  
  cluster = factor(cluster, 
                   levels=c("prefer forest","prefer intermediate","weak preference",
                            "prefer open"))
  
  return(cluster)
  
}


makeCapital <- function(string){
  
  sapply(string, function(x){
    paste0(toupper(substr(x,1,1)), tolower(substr(x,2,nchar(x))))
  })
  
}

getModels <- function(modelFolder,modeltype = "simple_linear"){ 
  
  #translate
  if(modeltype=="simple_linear"){
    fileName <- "gamOutput_glm_subset_random_"
    
  } else if(modeltype=="simple_shape"){
    fileName <- "gamOutput_gam_shape_subset_random_"
    
  }else if(modeltype=="mixed_linear"){
    fileName <- "gamOutput_gamm_subset_random_"
    
  }else if(modeltype=="mixed_shape"){
    fileName <- "gamOutput_gamm_shape_subset_random_"
  }
  
  temp <- list.files(modelFolder,full.names=TRUE) %>%
    str_subset(fileName) %>%
    set_names() %>%
    map_dfr(readRDS, .id="source") %>%
    group_by(source) %>%
    mutate(Taxa = strsplit(source, fileName)[[1]][2]) %>%
    mutate(Taxa = gsub("conif_","",Taxa)) %>%
    mutate(Taxa = gsub("decid_","",Taxa)) %>%
    mutate(Taxa = strsplit(Taxa,"_")[[1]][1])%>%
    ungroup() %>%
    filter(Taxa %in% selectTaxa) %>%
    mutate(Taxa = case_when(Taxa=="E&D" ~ "Empidids",
                            TRUE ~ as.character(Taxa)))  %>%
    add_column(modeltype = modeltype)
  
  #fix names
  
  if("Species" %in% names(temp)){
    temp <- temp %>% rename(Species = species)
  }
  
  if("decid_forest" %in% names(temp)){
    temp <- temp %>% rename(decidForest = decid_forest)
    
  }
  
  return(temp)
}