#' Citations and Cited Instances
#'
#' @inheritParams h_index
#' @param pub_years A numeric vector containing publication years
#' @return return year-wise total instances (tp), cited instances and total citations (tc)
#' @export
#' @examples
#'citation_info(c(1,3,0,4,2,3,1,0),c(2012,2012,2012,2013,2012,2011,2014,2014))
citation_info<-function(citations, pub_years){
  if(!is.numeric(citations))stop('citations must be numeric')
  if(length(citations)==0)stop('citations must be of minimum length 1')

  if(length(citations)!=length(pub_years))stop('citations and pub_years must be of same length.')
  if(!is.numeric(pub_years))stop('pub_years must be a numeric vector')

  citations[is.na(citations)]<-0

  yr<-sort(unique(pub_years))
  tp<-sapply(yr,FUN = function(x){sum(pub_years==x)})
  tc<-sapply(yr,FUN = function(x){sum(citations[pub_years==x])})
  cited<-sapply(yr,FUN = function(x){sum(citations[pub_years==x]>0)})

  list(years=yr,total_instances=tp,cited_instances=cited,total_citations=tc)
}
