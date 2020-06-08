##' @title

##' @return
##' @author Aaron Conway
##' @importFrom dplyr tibble
##' @importFrom stringr str_remove_all

##' @export

create_H_values_ptds7_table <- function(H_values_ptds7){

  tibble(Item = c(
  "My mouth is dry",
  "My lips are dry",
  "My tongue is thick",
  "My saliva is thick",
  "My throat is dry",
  "I have a bad taste in my mouth",
  "I want to drink water"
), Hi = c(H_values_ptds7$Hi[1],
          H_values_ptds7$Hi[2],
          H_values_ptds7$Hi[3],
          H_values_ptds7$Hi[4],
          H_values_ptds7$Hi[5],
          H_values_ptds7$Hi[6],
          H_values_ptds7$Hi[7]),
    SE = c(str_remove_all(H_values_ptds7$Hi[8], "[()]"),
           str_remove_all(H_values_ptds7$Hi[9], "[()]"),
                          str_remove_all(H_values_ptds7$Hi[10], "[()]"),
                                         str_remove_all(H_values_ptds7$Hi[11], "[()]"),
                                                        str_remove_all(H_values_ptds7$Hi[12], "[()]"),
                                                                       str_remove_all(H_values_ptds7$Hi[13], "[()]"),
                                                                                      str_remove_all(H_values_ptds7$Hi[14], "[()]")))
}
