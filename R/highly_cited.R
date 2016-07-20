#' Highly Cited Instances
#'
#' It finds the number of highly cited instances year-wise.
#'
#' @inheritParams citation_info
#' @param ref_citations The citations of reference instances
#' @param ref_pub_years The publication years of reference instances
#' @param top An integer which defines top percent highly cited instances
#' @param year_lim A list conating years and year-wise citation threshold. If not mentioned
#' these values are calculated from ref_citations, ref_pub_years & top.
#'
#' @return Returns a list containing number of top highly cited instances with other details
#' @export
#' @examples
#' citations<-c(2,0,12,3,1,1,4,5,8,2)
#' pub_years<-c(2011,2011,2012,2011,2013,2011,2011,2012,2011,2013)
#' ref_citations<-c(3,0,12,3,1,1,41,5,8,2,2,0,12,30,1,1,4,5,8,12)
#' ref_pub_years<-c(2012,2011,2012,2013,2013,2011,2011,2012,
#' 2011,2013,2011,2011,2012,2011,2013,2011,2011,2012,2011,2013)
#' highly_cited(citations,pub_years,ref_citations,ref_pub_years,10)
#' highly_cited(citations,pub_years,year_lim = list(c(2011, 2012, 2013), c(41, 12, 12)))
highly_cited<-function(citations, pub_years, ref_citations=NULL, ref_pub_years=NULL, top=NULL, year_lim=list()){
  # citations, pub_years
  if(!is.numeric(citations))stop('citations must be numeric')
  if(length(citations)==0)stop('citations must be of minimum length 1')

  if(length(citations)!=length(pub_years))stop('citations and pub_years must be of same length.')
  if(!is.numeric(pub_years))stop('pub_years must be a numeric vector')

  citations[is.na(citations)]<-0
  yr<-lim<-NULL
  # year_lim=list()
  if(length(year_lim)==2){
    if(length(year_lim[[1]])!=length(year_lim[[2]])){
      stop('year_lim list must have two vectors of equal length containing years and citation thresholds.')
    }
    if(!is.numeric(year_lim[[1]]) || !is.numeric(year_lim[[2]])){
      stop('both vectors in year_lim must be numeric.')
    }

    #  catch the year-wise limit
    yr<-year_lim[[1]]
    lim<-year_lim[[2]]
  }

  if(is.null(yr) && is.null(lim)){
    if(is.null(ref_citations) || is.null(ref_pub_years)|| is.null(top)){
      stop('ref_citations, ref_pub_years or top has NULL value')
    }
    if(length(ref_citations) != length(ref_pub_years)){
      stop('ref_citations and ref_pub_years must be of same length.')
    }
    if(!is.numeric(ref_citations) || !is.numeric(ref_pub_years)){
      stop('ref_citations and ref_pub_years must be numeric.')
    }
    if(is.integer(top)){
      stop('top must be an integer')
    }
    # catch the year-wise limit using ref_citations, ref_pub_years & top
    yr<-sort(unique(intersect(pub_years,ref_pub_years)))
    lim<-numeric(length(yr))

    for(i in seq_len(length(yr))){
      y_ind<-which(ref_pub_years==yr[i])
      ct<-sort(ref_citations[y_ind],decreasing = T)
      t<-ceiling(length(y_ind)*(top/100))
      lim[i]<-ct[t]
    }
  }


  tp<-hicp<-numeric(length(yr))

  for(i in seq_len(length(yr))){
    y_ind<-which(pub_years==yr[i])
    hicp[i]<-sum(citations[y_ind]>=lim[i])
    tp[i]<-length(y_ind)
  }

  list(years=yr, citation_thresholds=lim, total_instances=tp, highly_cited_instances=hicp)
}
