#' Co-authorship Matrix and Average co-authorship
#'
#' It finds year-wise co-authorship matrix and average co-authorship values
#'
#' @param authors A character vector containing author names
#' @param pub_years A numeric vector containing publication years
#' @param sep A character pattern separating author names
#'
#' @return A list with co-authorship matrix and average co-authorship values
#' @export
#' @examples
#' authors<-c("Wolf W.R., Lele S.K.",
#' "Shin D., Yeh X., Khatib O.",
#' "Aukes D., Heyneman B., Duchaine V., Cutkosky M.R.")
#' years<-c(2011,2012,2012)
#' authorship_pattern(authors,years,',')
authorship_pattern<-function(authors, pub_years, sep){
  if(length(authors)!=length(pub_years))stop('authors and pub_years must be of same length.')
  if(!is.numeric(pub_years))stop('pub_years must be a numeric vector')

  no.of.authors<-sapply(X=str_split(authors,sep),FUN=length)
  max.authors<-max(no.of.authors)

  yr<-sort(unique(pub_years))
  coauth<-numeric(length(yr))
  m<-matrix(data=0,nrow=length(yr), ncol = max.authors, dimnames = list(yr,1:max.authors))

  for(i in seq_len(length(yr))){
    ind<-which(pub_years==yr[i])
    coauth[i]<-round(sum(no.of.authors[ind])/length(ind),2)
    m[i,]<-table(c(no.of.authors[ind],1:max.authors))-1
  }
  m<-m[,colSums(m)!=0]

  list(years=yr,authorship_matrix=m,avg_co_authorship=coauth)
}
