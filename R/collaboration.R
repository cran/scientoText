
#' International Collaboration
#'
#' Calculate the number of Internationally Collaborated Papers
#'
#' @param affiliations A text vector containing affiliation (country) information
#' @param pub_years A numeric vector containing publication years
#' @param countries A list of countries (optional)
#' @details It finds if there is any International Collaboration so affiliation fields must have country information
#' @return Collaboration count or a list  (collaboration counts year-wise)
#' @importFrom stringr str_replace_all fixed str_detect
#' @export
#' @examples
#' affiliations<-c("Stanford University, Stanford, CA, United States; Montreal, QC, Canada",
#' "Stanford University, United States; Google Inc., United States",
#' "University of Michigan, Ann Arbor, MI 48109-2122, United States;
#' Tsinghua University, Beijing 100084, China",
#' "Imperial College London, London, SW7 2BZ, United Kingdom;
#' ENSTA, Ecole Polytechnique, Palaiseau, 91761, France")
#'
#' pub_years<-c(2012,2012,2013,2014)
#'
#' international_col( affiliations, pub_years)
#' international_col(affiliations)

international_col<-function(affiliations,pub_years=NULL,countries=NULL){
  if(length(affiliations)==0)stop("affiliations is of length 0.")
  if(!is.null(pub_years)){
    if(length(affiliations)!=length(pub_years))stop('affiliations and pub_years must be of same length.')
    if(!is.numeric(pub_years))stop('pub_years must be a numeric vector')
  }


  C1<-affiliations
  C1<-str_replace_all(string = C1,pattern = "USA",replacement = "United States")
  C1<-str_replace_all(string = C1,pattern = "UK",replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = fixed("England",ignore_case = T),replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = fixed("Indiana",ignore_case = T),replacement = "")
  C1<-str_replace_all(string = C1,pattern = fixed("Scotland",ignore_case = T),replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = "Wales",replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = fixed("Northern Ireland",ignore_case = T),replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = "UAE",replacement = "United Arab Emirates")

  c.list<-c("United States", "China", "United Kingdom", "Germany", "Japan", "France", "Canada",
            "Italy", "Spain", "India", "Australia", "Russia", "South Korea", "Netherlands", "Brazil",
            "Taiwan", "Switzerland", "Sweden", "Poland", "Turkey", "Belgium", "Israel", "Austria",
            "Denmark", "Iran", "Finland", "Greece", "Mexico", "Czech Republic", "Norway",
            "Singapore", "Portugal", "New Zealand", "South Africa", "Argentina", "Hungary", "Ukraine",
            "Ireland", "Malaysia", "Romania", "Egypt", "Thailand", "Chile", "Saudi Arabia", "Pakistan",
            "Croatia", "Slovakia", "Slovenia", "Bulgaria", "Nigeria", "Tunisia", "Colombia", "Serbia",
            "Morocco", "Venezuela", "Algeria", "Belarus", "Lithuania",  "Cuba", "Indonesia",  "Jordan",
            "Bangladesh",  "Estonia", "United Arab Emirates",  "Kenya", "Viet Nam", "Kuwait", "Lebanon",
            "Philippines", "Iceland", "Cyprus", "Latvia", "Uruguay", "Armenia", "Peru", "Sri Lanka",
            "Oman", "Ethiopia", "Tanzania", "Uzbekistan", "Cameroon", "Azerbaijan", "Uganda", "Ghana",
            "Luxembourg", "Costa Rica", "Nepal", "Iraq", "Qatar", "Macedonia", "Kazakhstan", "Zimbabwe",
            "Senegal", "Ecuador", "Moldova", "Bosnia and Herzegovina", "Sudan", "Syria",
            "Trinidad and Tobago", "Ivory Coast", "Panama", "Jamaica", "Botswana", "Burkina Faso",
            "Malawi", "Bahrain",  "Palestine", "Libya", "Zambia", "Bolivia", "Benin", "Malta",
            "Madagascar", "North Korea", "Mongolia", "Congo", "Mali", "Papua New Guinea", "Yemen",
            "Cambodia", "Albania", "Guatemala", "Fiji", "Gambia", "Mozambique", "Gabon", "Namibia",
            "Mauritius", "Brunei",  "Barbados", "Monaco", "Niger", "Laos",  "Montenegro", "Myanmar",
            "Guadeloupe", "Togo", "Kyrgyzstan", "Nicaragua", "Paraguay", "Rwanda", "Tajikistan",
            "El Salvador", "Dominican Republic", "Swaziland", "Honduras", "Guam", "Grenada",
            "Afghanistan", "Bermuda", "Martinique", "Angola", "Haiti", "Guyana", "Central African Republic",
            "Guinea", "Eritrea", "Mauritania", "Sierra Leone", "Faroe Islands", "Seychelles",
            "Guinea-Bissau", "Lesotho", "Bhutan", "Burundi", "Chad", "Bahamas", "Belize", "Solomon Islands",
            "Turkmenistan", "Vanuatu", "Suriname", "Samoa", "Dominica", "Federated States of Micronesia",
            "Maldives", "Djibouti", "Saint Kitts and Nevis", "Liberia", "San Marino", "Palau", "Andorra",
            "Cape Verde", "Tonga",  "Antigua and Barbuda", "Marshall Islands", "Mayotte", "Gibraltar",
            "Somalia", "Comoros", "Saint Lucia", "Timor-Leste", "Aruba", "Sao Tome and Principe",
            "Cook Islands", "Turks and Caicos Islands","Saint Vincent and The Grenadines", "Norfolk Island",
            "Tuvalu", "Nauru", "Anguilla", "Vatican City",  "Kiribati", "Niue")


  if(!is.null(countries)){
    c.list<-countries
  }

  if(is.null(pub_years)){
    count<-0
    for(j in seq_len(length(C1))){
      ind<-which(str_detect(string = C1[j],pattern = fixed(c.list,ignore_case = T)))
      if(length(ind)>1)count<-count+1
    }
    return (list(total_instances=length(C1),ic_instances=count))
  }

  yr<-sort(unique(pub_years))
  total<-numeric(length(yr))
  ic<-numeric(length(yr))

  for(i in seq_len(length(yr))){
    y_ind<-which(pub_years==yr[i])
    total[i]<-length(y_ind)
    for(j in y_ind){
      ind<-which(str_detect(string = C1[j],pattern = fixed(c.list,ignore_case = T)))
      if(length(ind)>1)ic[i]<-ic[i]+1
    }
  }
  return (list(years=yr,total_instances=total,ic_instances=ic))
}



#' International Collaboration Matrix
#'
#' Calculate Internationally Collaborated Matrix(es)
#'
#' @inheritParams international_col
#' @details It finds the collaboration network at internationla level in terms of
#' adjacent matrix so affiliation fields must have country information
#' @return Collaboration adjacent matrix(es)
#' @importFrom stringr str_replace_all fixed str_detect
#' @importFrom utils combn
#' @export
#' @examples
#' affiliations<-c("Stanford University, Stanford, CA, United States; Montreal, QC, Canada",
#' "Stanford University, United States; Google Inc., United States",
#' "University of Michigan, Ann Arbor, MI 48109-2122, United States;
#' Tsinghua University, Beijing 100084, China",
#' "Imperial College London, London, SW7 2BZ, United Kingdom;
#' ENSTA, Ecole Polytechnique, Palaiseau, 91761, France")
#'
#' pub_years<-c(2012,2012,2013,2014)
#'
#' international_colmat( affiliations, pub_years)
#' international_colmat(affiliations)

international_colmat<-function(affiliations,pub_years=NULL,countries=NULL){
  if(length(affiliations)==0)stop("affiliations is of length 0.")
  if(!is.null(pub_years)){
    if(length(affiliations)!=length(pub_years))stop('affiliations and pub_years must be of same length.')
    if(!is.numeric(pub_years))stop('pub_years must be a numeric vector')
  }


  C1<-affiliations
  C1<-str_replace_all(string = C1,pattern = "USA",replacement = "United States")
  C1<-str_replace_all(string = C1,pattern = "UK",replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = fixed("England",ignore_case = T),replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = fixed("Indiana",ignore_case = T),replacement = "")
  C1<-str_replace_all(string = C1,pattern = fixed("Scotland",ignore_case = T),replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = "Wales",replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = fixed("Northern Ireland",ignore_case = T),replacement = "United Kingdom")
  C1<-str_replace_all(string = C1,pattern = "UAE",replacement = "United Arab Emirates")

  c.list<-c("United States", "China", "United Kingdom", "Germany", "Japan", "France", "Canada",
            "Italy", "Spain", "India", "Australia", "Russia", "South Korea", "Netherlands", "Brazil",
            "Taiwan", "Switzerland", "Sweden", "Poland", "Turkey", "Belgium", "Israel", "Austria",
            "Denmark", "Iran", "Finland", "Greece", "Mexico", "Czech Republic", "Norway",
            "Singapore", "Portugal", "New Zealand", "South Africa", "Argentina", "Hungary", "Ukraine",
            "Ireland", "Malaysia", "Romania", "Egypt", "Thailand", "Chile", "Saudi Arabia", "Pakistan",
            "Croatia", "Slovakia", "Slovenia", "Bulgaria", "Nigeria", "Tunisia", "Colombia", "Serbia",
            "Morocco", "Venezuela", "Algeria", "Belarus", "Lithuania",  "Cuba", "Indonesia",  "Jordan",
            "Bangladesh",  "Estonia", "United Arab Emirates",  "Kenya", "Viet Nam", "Kuwait", "Lebanon",
            "Philippines", "Iceland", "Cyprus", "Latvia", "Uruguay", "Armenia", "Peru", "Sri Lanka",
            "Oman", "Ethiopia", "Tanzania", "Uzbekistan", "Cameroon", "Azerbaijan", "Uganda", "Ghana",
            "Luxembourg", "Costa Rica", "Nepal", "Iraq", "Qatar", "Macedonia", "Kazakhstan", "Zimbabwe",
            "Senegal", "Ecuador", "Moldova", "Bosnia and Herzegovina", "Sudan", "Syria",
            "Trinidad and Tobago", "Ivory Coast", "Panama", "Jamaica", "Botswana", "Burkina Faso",
            "Malawi", "Bahrain",  "Palestine", "Libya", "Zambia", "Bolivia", "Benin", "Malta",
            "Madagascar", "North Korea", "Mongolia", "Congo", "Mali", "Papua New Guinea", "Yemen",
            "Cambodia", "Albania", "Guatemala", "Fiji", "Gambia", "Mozambique", "Gabon", "Namibia",
            "Mauritius", "Brunei",  "Barbados", "Monaco", "Niger", "Laos",  "Montenegro", "Myanmar",
            "Guadeloupe", "Togo", "Kyrgyzstan", "Nicaragua", "Paraguay", "Rwanda", "Tajikistan",
            "El Salvador", "Dominican Republic", "Swaziland", "Honduras", "Guam", "Grenada",
            "Afghanistan", "Bermuda", "Martinique", "Angola", "Haiti", "Guyana", "Central African Republic",
            "Guinea", "Eritrea", "Mauritania", "Sierra Leone", "Faroe Islands", "Seychelles",
            "Guinea-Bissau", "Lesotho", "Bhutan", "Burundi", "Chad", "Bahamas", "Belize", "Solomon Islands",
            "Turkmenistan", "Vanuatu", "Suriname", "Samoa", "Dominica", "Federated States of Micronesia",
            "Maldives", "Djibouti", "Saint Kitts and Nevis", "Liberia", "San Marino", "Palau", "Andorra",
            "Cape Verde", "Tonga",  "Antigua and Barbuda", "Marshall Islands", "Mayotte", "Gibraltar",
            "Somalia", "Comoros", "Saint Lucia", "Timor-Leste", "Aruba", "Sao Tome and Principe",
            "Cook Islands", "Turks and Caicos Islands","Saint Vincent and The Grenadines", "Norfolk Island",
            "Tuvalu", "Nauru", "Anguilla", "Vatican City",  "Kiribati", "Niue")


  if(!is.null(countries)){
    c.list<-countries
  }

  if(is.null(pub_years)){
    m<-matrix(data=0,nrow=length(c.list), ncol = length(c.list),dimnames=list(c.list,c.list))
    #pb <- txtProgressBar(min = 1, max = length(C1), style = 3)
    for(j in 1:length(C1)){
      ind<-which(str_detect(string = C1[j],pattern = fixed(c.list,ignore_case = T)))
      if(length(ind)>1){
        rc<-t(combn(x = ind,m = 2))
        m[rc]<-m[rc]+1
      }
      #setTxtProgressBar(pb, j)
    }
    m<-m+t(m)
    r<-which(rowSums(m)==0)
    m<-m[-r,-r]
    return (list(total_instances=length(C1),ic_adjacent_matrix=m))
  }

  yr<-sort(unique(pub_years))
  total<-numeric(length(yr))
  M<-NULL

  for(i in seq_len(length(yr))){
    y_ind<-which(pub_years==yr[i])
    total[i]<-length(y_ind)
    m<-matrix(data=0,nrow=length(c.list), ncol = length(c.list),dimnames=list(c.list,c.list))
    #pb <- txtProgressBar(min = 1, max = length(C1), style = 3)
    for(j in y_ind){
      ind<-which(str_detect(string = C1[j],pattern = fixed(c.list,ignore_case = T)))
      if(length(ind)>1){
        rc<-t(combn(x = ind,m = 2))
        m[rc]<-m[rc]+1
      }
      #setTxtProgressBar(pb, j)
    }
    m<-m+t(m)
    r<-which(rowSums(m)==0)
    M[[i]]<-m[-r,-r]
  }
  return (list(years=yr,total_instances=total,ic_adjacent_matrix=M))
}
