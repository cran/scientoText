#' h index
#'
#'Find h index for a given set of documents
#' @param citations A numeric vector containing citations
#'
#' @return return the h index for the given citations
#' @export
#' @examples
#' h_index(c(1,2,5,0,3,11))
#' @seealso \code{\link{g_index}}
#' @references Hirsch, J. E. (2005). An index to quantify an individual's
#' scientific research output. Proceedings of the National academy of Sciences
#'  of the United States of America, 102(46), 16569-16572.
h_index<-function(citations)
{
  if(!is.numeric(citations))stop('citations must be numeric')
  if(length(citations)==0)stop('citations must be of minimum length 1')
  h<-0
  citations<-c(citations,0)
  ordered.citations<-sort(citations,decreasing = T)

  for(i in 1: length(ordered.citations))
  {
    if(ordered.citations[i]<i)
      break
  }
  h<-i-1
  h
}


#' g index
#'
#' @inheritParams h_index
#'
#' @return return the g index for the given citations
#' @export
#' @examples
#' g_index(c(1,2,5,0,3,11))
#' @seealso \code{\link[=h_index]{h index}}
g_index<-function(citations)
{
  if(!is.numeric(citations))stop('citations must be numeric')
  if(length(citations)==0)stop('citations must be of minimum length 1')
  g<-0
  citations<-c(citations,0)
  ordered.citations<-sort(citations,decreasing = T)
  s<-0
  for(i in 1: length(ordered.citations))
  {
    s<-s+ordered.citations[i]
    if(s<i**2)break
  }
  g<-i-1
  g
}

