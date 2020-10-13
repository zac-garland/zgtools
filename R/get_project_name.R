get_project_name <- function(){
  list.files(pattern = "proj") %>% 
    str_replace("\\.Rproj","")
  
}