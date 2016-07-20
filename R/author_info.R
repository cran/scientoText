#' Authors' Information
#'
#' It finds top author names and their different performance indicators
#'
#' @param authors A character vector containing author names
#' @inheritParams h_index
#' @param sep A character pattern separating author names
#' @param top The number of top authors
#' @param only_first_author Logical. If to find the author list by the first authors
#'
#' @return A list consisting of author names, total instances, total citations, h index, g index, i10 index, max citation
#' @importFrom  stringr str_replace_all str_detect str_trim str_split
#' @export
#' @examples
#' authors<-c("Wolf W.R., Lele S.K.",
#' "Shin D., Yeh X., Khatib O.",
#' "Aukes D., Heyneman B., Duchaine V., Cutkosky M.R.")
#' author_info(authors,c(3,4,1),',')
#' @seealso \code{\link[=g_index]{g index}} \code{\link[=h_index]{h index}}

author_info<-function(authors, citations, sep,top=10,only_first_author=F){
  if(length(authors)!=length(citations))stop('authors and citations must be of same length.')
  if(!is.numeric(citations))stop('citations must be a numeric vector')
  citations[is.na(citations)]<-0

  unique_authors<-sort(table(str_trim(unlist(str_split(authors, sep)))), decreasing=T)
  if(only_first_author){
    auth<-str_split(authors,sep)
    unique_authors<-sort(table(str_trim(sapply(auth,function(x)x[1]))),decreasing = T)
  }

  top<-min(top, length(unique_authors))
  top_authors<-names(unique_authors)[1:top]
  pat<-str_replace_all(top_authors,'\\.','\\\\.')

  tp<-tc<-h<-g<-i10<-max_cit<-numeric(top)
  for(i in seq_len(top)){
    ind<- which(str_detect(string = authors,pattern = sprintf('%s%s|%s$',pat[i],sep,pat[i])))
    tp[i]<- length(ind)
    tc[i]<- sum(citations[ind])
    h[i]<-h_index(citations[ind])
    g[i]<-g_index(citations[ind])

    i10[i]<-sum(citations[ind]>=10)
    max_cit[i]<-max(citations[ind])
  }
  list(author_names=top_authors, total_instances=tp, total_citations=tc, h_index=h, g_index=g, i10_index=i10, max_citation=max_cit)
}
