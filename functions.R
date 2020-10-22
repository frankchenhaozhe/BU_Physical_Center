library(tidyverse)
library(magrittr)

## functions for data wrangling 
check_na_df <- function(df,plot = T,fontsize = 12, angle_text = 30){
  check_na <- is.na(df)%>%
    as.data.frame()%>%
    lapply(X = .,FUN = sum)%>%
    as.data.frame.list()%>%
    t()%>%
    as.data.frame()%>%
    rownames_to_column(var = "entry")
  colnames(check_na) <- c("entry","num_of_na")
  n <- nrow(df)
  if(plot){
    ggplot(check_na%>%filter(num_of_na>0))+
      aes(x = entry, y = num_of_na/n)+
      geom_col()+
      geom_label(aes(label = as.character(num_of_na)))+
      labs(title = paste0("Missing data on ",as.character(n)," observations"),x = "",y = "Percentage missing")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = angle_text,size = fontsize),
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(face = "italic"))
  }else{
    return(check_na)
  }
}

## ploting
## 
check_comp_df <- function(df,plot = T,fontsize = 12){
  check_na <- is.na(df)%>%
    as.data.frame()%>%
    lapply(X = .,FUN = sum)%>%
    as.data.frame.list()%>%
    t()%>%
    as.data.frame()%>%
    rownames_to_column(var = "entry")
  colnames(check_na) <- c("entry","num_of_na")
  n <- nrow(df)
  if(plot){
    ggplot(check_na%>%filter(num_of_na>0))+
      aes(x = entry, y = 1 - num_of_na/n)+
      geom_col()+
      geom_label(aes(label = as.character(n - num_of_na)))+
      labs(title = paste0("Completeness on ",as.character(n)," observations"),x = "",y = "Percentage")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 30,size = fontsize),
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(face = "italic"))
  }else{
    return(check_na)
  }
}

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-2*sd(x)
  ymax <- m+2*sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}