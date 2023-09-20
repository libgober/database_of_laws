require(lubridate)

assign_regime <- function(date,congress_number){
  # first handle an error case where we cannot determine the reginme
  if (missing(date) & missing(congress_number)){
    stop("No way to figure out citation regime from data provided")
  }
  if (!missing(date)){
    #next do the date approach
    if (!is.date(date)){
      stop("Date provided has wrong type")
    }
    regime1 = case_when(date >= ymd("1959-01-07") ~ 'Late',
              date <= ymd("1901-03-03") ~ 'Early',
              T ~ 'Middle')
    
  }
  if (!missing(congress_number)){
    congress_number=as.integer(congress_number)
    regime2 = case_when(congress_number >= 86 ~ 'Late',
                        congress_number <= 56 ~ 'Early',
                        T ~ 'Middle')    

  }
  if (missing(date)){
    return(regime2)
  }
  if (missing(congress_number)){
    return(regime1)
  }
  
  regime = ifelse(regime1==regime2,regime1,"Inconsistent")
  return(regime)
}

assign_regime(bdl_laws$date_of_passage[1],bdl_laws$congress_number[1])
assign_regime(bdl_laws$date_of_passage[1],100)

merge_late_laws <- function(x,y,x.names,x.law_no,y.congress,y.law_no){
  
}


assign_regime_with_named_arguments <- function(x,x.vars){
  has_congress = 'congress_number' %in% names(x.vars)
  has_date = 'date_of_passage' %in% names(x.vars)
  if (has_congress & has_date){
    assign_regime(date=x[[x.vars$date_of_passage]],
                  congress_number=x[[x.vars$congress_number]])
  } else if (has_congress & !has_date){
    assign_regime(congress_number=x[[x.vars$congress_number]])
  } else if (!has_congress & has_date){
    assign_regime(date=x[[x.vars$date_of_passage]])   
  } else {
    stop("Not enough data to determine appropriate citation regime")
  }
}

x.vars = list("date_of_passage"='date_of_passage',
              "congress_number" = "congress_number")

assign_regime_with_named_arguments(bdl_laws,list("congress_number" = "congress_number"))


merge_late_regime_laws <- function(x.late,y.late,
                                   x.vars,
                                   y.vars,
                                   all.x,
                                   all.y){
  
  "Assumes that the pl_number column is validated"
  merged.late = merge(x.late,
        y.late,
        by.x=x.vars[["pl_no"]],
        by.y=y.vars[["pl_no"]],
        all.x=all.x,
        all.y=all.y)
  
  return(merged.late)
}


merge_early_regime_laws <- function(x.early,y.early,
                                   x.vars,
                                   y.vars,
                                   all.x,
                                   all.y){
  
  "Assumes that the chapter,congress, session columns are validated"
  merged.early = merge(x.early,
                      y.early,
                      by.x=x.vars[["pl_no"]],
                      by.y=y.vars[["pl_no"]],
                      all.x=all.x,
                      all.y=all.y)
  
  return(merged.early)
}

merge_middle_regime_laws <- function(x.middle,y.middle,
                                     x.vars,
                                     y.vars,
                                     all.x,
                                     all.y){
  
  "Assumes that the pl_number column is validated"
  merged.middle = merge(x.middle,
                        y.middle,
                        by.x=x.vars[["pl_no"]],
                        by.y=y.vars[["pl_no"]],
                        all.x=all.x,
                        all.y=all.y)
  
  return(merged.middle)
}

merge_law_databases <- function(x,y,x.vars,y.vars,all.x,all.y){

  #first cut up the datasets using the named arguments
  # x cuts
  x.regimes = assign_regime_with_named_arguments(x,x.vars)
  x.early = x[x.regimes=='Early',]
  x.middle = x[x.regimes=='Middle',]
  x.late = x[x.regimes=='Late',]
  
  #y cuts
  y.regimes = assign_regime_with_named_arguments(y,y.vars)
  y.early = y[y.regimes=='Early',]
  y.middle = y[y.regimes=='Middle',]
  y.late = y[y.regimes=='Late',]
  
  # now merge the parts
  merged.late = merge_late_regime_laws(x.late,y.late,
    x.vars,
    y.vars,
    all.x,
    all.y) %>%
    as_tibble()
  
  merged.early = merge_early_regime_laws(x.early,y.early,
                                       x.vars,
                                       y.vars,
                                       all.x,
                                       all.y) %>%
    as_tibble()
  
  merged.middle = merge_middle_regime_laws(x.middle,y.middle,
                                         x.vars,
                                         y.vars,
                                         all.x,
                                         all.y) %>%
    as_tibble()
  
  merged = bind_rows(merged.early,merged.middle,merged.late)
  
  return(merged)
}

lookup_congress_and_session <- function(date){
  
  
}


x = bdl_laws
x.vars = list("date_of_passage"='date_of_passage',
              "congress_number" = "congress_number",
              "pl_no" = 'pl_no',
              "chapter" = "chapter",
              "session_number" = "session_number")

y = popnames %>%
  mutate(date=ymd(date))

y.vars = list("date_of_passage"='date',
              "pl_no" = 'public_law',
              "chapter" = "chapter",
              'session_number' = "session_number",
              "congress_number" = "congress_number")




