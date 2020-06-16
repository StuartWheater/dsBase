#' @title coxPHMDS is the serviceside function for Cox Proposional Hazard Model
#' @description creates Cox Proposional Hazard Model 
#' @details this serverside function returns the 
#'
#' @param data.name is a character string contained the name of the data variable
#' @param event.variable is a character string contains the name of the event field in the data
#' @param term.variables is a vector of character string contains the names of the term fields in the data
#' @param strata.variables is a vector of character string contains the names of the terms fields in the data, which are strata
#' @param start.variable is a character string contains the name of the start field in the data
#' @param stop.variable is a character string contains the name of the stop field in the data
#' @param period.variable is a character string contains the name of the period field in the data
#' @return the results of the Cox Proposional Hazard Model
#'
#' @author Stuart Wheater for DataSHIELD Development Team, Jun 2020
#' @export

coxPHMDS<-function(data.name, event.variable, term.variables, strata.variables, start.variable = NULL, stop.variable = NULL, period.variable = NULL)
{
    
}
#AGGREGATE FUNCTION
# coxPHMDS
