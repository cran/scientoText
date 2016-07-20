#' Term Frequency
#'
#' @param text A character vector
#' @inheritParams international_col
#' @param sep A character value whcih separates the terms (optional)
#' @param top The number of terms to return
#' @return Term frequencey vector or matrix (for year-wise)
#' @importFrom tm DocumentTermMatrix VCorpus VectorSource removePunctuation removeWords stopwords stripWhitespace tm_map
#' @export

term_freq<-function(text,pub_years=NULL,sep=NULL, top=NULL){
  if(!is.character(text))stop('text must be a character vector')
  if(!is.null(pub_years))pub_years<-pub_years[text!=""]
  text<-tolower(text[text!=""])
  if(length(text)==0)stop('text has zero non-empty element')

  if(!is.null(pub_years) && is.null(sep)){
    if(length(text)!=length(pub_years))stop('text and pub_years must be of same length')
    if(!is.numeric(pub_years))stop('pub_years must be numeric')

    corp<-VCorpus(VectorSource(x = text))
    corp <- tm_map(corp, removePunctuation)
    corp <- tm_map(corp, stripWhitespace)
    corp <- tm_map(corp, removeWords, stopwords("english"))

    dtm <- DocumentTermMatrix(corp)
    ord<-order(colSums(as.matrix(dtm)),decreasing = T)
    dtm<-dtm[,ord]

    yr<-sort(unique(pub_years))
    m<-matrix(data = 0,nrow = length(yr),
              ncol = dtm$ncol,dimnames = list(yr,dtm$dimnames$Terms))

    for(i in 1: length(yr)){
      tdtm<-dtm[pub_years %in% yr[i],]
      m[i,]<-colSums(x = as.matrix(tdtm))
    }
    ord<-order(colSums(m),decreasing = T)
    if(!is.null(top)){
      top<-min(top,ncol(m))
      m<-m[,ord[1:top]]
    }
    if(is.null(top))m<-m[,ord]
    return (list(terms=colnames(m), freqency=colSums(m), term_freq_matrix=m))
  }

  if(is.null(pub_years) && is.null(sep)){

    corp<-VCorpus(VectorSource(x = text))
    corp <- tm_map(corp, removePunctuation)
    corp <- tm_map(corp, stripWhitespace)
    corp <- tm_map(corp, removeWords, stopwords("english"))

    dtm <- DocumentTermMatrix(corp)
    ord<-order(colSums(as.matrix(dtm)),decreasing = T)
    dtm<-dtm[,ord]

    m<-as.matrix(dtm)
    ord<-order(colSums(m),decreasing = T)
    if(!is.null(top)){
      top<-min(top,ncol(m))
      m<-m[,ord[1:top]]
    }
    if(is.null(top))m<-m[,ord]
    return (list(terms=colnames(m), freqency=colSums(m)))
  }

  if(!is.null(pub_years) && !is.null(sep)){
    if(length(text)!=length(pub_years))stop('text and pub_years must be of same length')
    if(!is.numeric(pub_years))stop('pub_years must be numeric')
    if(length(sep)>1 || !is.character(sep))stop('sep must be a character value')

    yr<-sort(unique(pub_years))
    kw.list<-unlist(str_split(string = text,pattern = sep))
    kw.list<-str_trim(kw.list)
    kw.list<-unique(kw.list[kw.list!=""])

    m<-matrix(data = 0,nrow = length(yr),
              ncol = length(kw.list),dimnames = list(yr,kw.list))

    for(i in seq_len(length(yr))){
      y_ind<-which(pub_years == yr[i])
      for(j in y_ind){
        kw<-str_trim(unlist(str_split(string = text[j],pattern = sep)))
        ind<-which(kw.list %in% kw)
        if(length(ind)>0)m[i,ind]<-m[i,ind]+1
      }
    }
    ord<-order(colSums(m),decreasing = T)
    if(!is.null(top)){
      top<-min(top,ncol(m))
      m<-m[,ord[1:top]]
    }
    if(is.null(top))m<-m[,ord]
    return (list(terms=colnames(m), freqency=colSums(m), term_freq_matrix=m))
  }

  if(is.null(pub_years) && !is.null(sep)){
    if(length(sep)>1 || !is.character(sep))stop('sep must be a character value')

    kw.list<-unlist(str_split(string = text,pattern = sep))
    kw.list<-str_trim(kw.list)
    kw.list<-sort(table(kw.list[kw.list!=""]),decreasing = T)
    if(!is.null(top)){
      top<-min(top,length(kw.list))
      kw.list<-kw.list[1:top]
    }
    return (list(terms=names(kw.list), freqency=kw.list))
  }
}
