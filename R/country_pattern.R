#' Country Instances
#'
#' Country-wise and year-wise output for a defined period.
#'
#' @inheritParams international_col
#' @param only_first_author Logical. If to find the author list by the first authors
#'
#' @return A list containing country output and other details.
#' @details The function returns year and country-wise output matrix if the publication years
#' are provided. If only affliation data is provided the country-wise output is returned as
#' a single vector instead of a matrix.
#' @export
#' @importFrom stringr str_replace_all str_detect
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
#' country_pattern( affiliations, pub_years)
#' country_pattern(affiliations)

country_pattern<-function(affiliations,pub_years=NULL,countries=NULL,only_first_author=F){
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
    tp<-numeric(length(c.list))
    for(j in seq_len(length(C1))){
      ind<-which(str_detect(string = C1[j],pattern = fixed(c.list,ignore_case = T)))
      if(length(ind)>0){
        if(only_first_author)ind<-ind[1]
        tp[ind]<-tp[ind]+1
      }
    }
    ord<-order(tp,decreasing = T)
    tp<-tp[ord]
    countries<-c.list[ord]
    countries<-countries[tp>0]
    tp<-tp[tp>0]

    return (list(countries=countries,total_instances=tp))
  }

  yr<-sort(unique(pub_years))
  m<-matrix(data=0,nrow=length(c.list), ncol = length(yr), dimnames = list(c.list,yr))
  for(i in seq_len(length(yr))){
    y_ind<-which(pub_years==yr[i])
    for(j in y_ind){
      ind<-which(str_detect(string = C1[j],pattern = fixed(c.list,ignore_case = T)))
      if(length(ind)>0){
        if(only_first_author)ind<-ind[1]
        m[ind,i]<-m[ind,i]+1
      }
    }
  }
  m<-m[rowSums(m)>0,]
  m<-m[order(rowSums(m),decreasing = T),]
  tp<-rowSums(m)

  return (list(years=yr, countries=row.names(m), country_matrix=m, total_instances=tp))
}


