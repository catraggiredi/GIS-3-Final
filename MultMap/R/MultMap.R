#' @title Making Multiple Maps
#'
#' @param db spatial database
#' @param var string representing the name of the variable column name
#'
#' @return a series of heatmaps with the value as the filler
#' @export
#'
library(tmap)
library(devtools)
library(tidyverse)
library(sf)

makemap<- function(db,var){
  tmquant<- tm_shape(db) + tm_fill(alpha = 0.5,palette = "BuPu", var, style="quantile")+
    tm_shape(world) +  tm_borders()

  tmjenks<- tm_shape(db) + tm_fill(alpha = 0.5, palette = "BuPu",var, style="jenks")+
    tm_shape(world) +  tm_borders()

  tmsd<- tm_shape(db) + tm_fill(alpha = 0.5,palette = "BuPu", var, style="sd")+
    tm_shape(world) +  tm_borders()

  tmeq<- tm_shape(db) + tm_fill(alpha = 0.5,palette = "BuPu", var, style="equal")+
    tm_shape(world) +  tm_borders()

  list(tmquant, tmjenks, tmsd, tmeq)

}
