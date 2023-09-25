require(lubridate)
require(assertthat)

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

#assign_regime(bdl_laws$date_of_passage[1],bdl_laws$congress_number[1])
#assign_regime(bdl_laws$date_of_passage[1],100)


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
                      by.x=c(x.vars[["congress_number"]],
                             x.vars[["session"]],
                             x.vars[["chapter"]]),
                      by.y=c(y.vars[["congress_number"]],
                             y.vars[["session"]],
                             y.vars[["chapter"]]),
                      all.x=all.x,
                      all.y=all.y)
  
  return(merged.early)
}

merge_middle_regime_laws <- function(x.middle,y.middle,
                                     x.vars,
                                     y.vars,
                                     all.x,
                                     all.y){
  "Assumes that the congres,session, and chapter are clean.
  The merge will default to adding .y to the right hand dataset."
  x.middle$x.ids = 1:nrow(x.middle)
  y.middle$y.ids = 1:nrow(y.middle)
  y.middle.pl_nos = y.middle[[y.vars[["pl_no"]]]]
  y.middle.congress_number  = y.middle[[y.vars[["congress_number"]]]]
  y.middle.session =  y.middle[[y.vars[["session"]]]]
  y.middle.chapter = y.middle[[y.vars[['chapter']]]]
  
 result <- tibble()
 for (i in 1:nrow(x.middle)){
    x.i.pl_no = x.middle[[(x.vars[["pl_no"]])]][i]
    x.i.congress_number = x.middle[[x.vars[["congress_number"]]]][i]
    x.i.session = x.middle[[x.vars[["session"]]]][i]
    x.i.chapter = x.middle[[x.vars[["chapter"]]]][i]
    
    pl_matches = x.i.pl_no==y.middle.pl_nos
    congress_number_matches = x.i.congress_number==y.middle.congress_number
    session_matches = x.i.session==y.middle.session
    chapter_matches = x.i.chapter == y.middle.chapter
    
    selector = congress_number_matches & session_matches & (pl_matches | chapter_matches)
    selector[is.na(selector)] <- F
    x.row = x.middle[i,]
    matching.y.rows = y.middle[selector,]
    
    if (nrow(matching.y.rows)>0){
      #find the problem columns
      overlapping_col_names = intersect(names(x.row),names(matching.y.rows))
      #make a lookup vector a la instructions in ?rename
      names(overlapping_col_names) = paste0(overlapping_col_names,".y")
      # rename
      matching.y.rows.safe = rename(matching.y.rows,
                                    all_of(overlapping_col_names))
      result = bind_rows(result, bind_cols(x.row,matching.y.rows.safe))
 
    } else if (all.x) {result = bind_rows(result,x.row)}
    else {}
 } #end for
    
  
  if (all.y){
    toadd = y.middle[!(y.middle$y.ids %in% result$y.ids),]
    #setup the names for the new stuff
    overlapping_col_names = intersect(names(x.middle),names(toadd))
    selector.for.columns.in.y = names(toadd) %in% overlapping_col_names
    new.names = paste0(overlapping_col_names,".y")
    names(toadd)[selector.for.columns.in.y] <- new.names
    
    result = bind_rows(result, toadd)
    
  }

  result$x.ids <- NULL
  result$y.ids <- NULL
  return(result)
}

merge_law_databases <- function(x,y,x.vars,y.vars,all.x,all.y){
  "
  Requires x.vars and y.vars as lists that have the names mapping values
  
  pl_no
  congress_number
  session
  chapter
  
  
  "
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
  
  merged = bind_rows(merged.early ,merged.middle,merged.late)
  
  return(merged)
}

lookup_session_by_date <- function(dates){
  "Attempts to lookup the session of a law based on its enactment date.
  Note that for some laws, e.g. those enacted on December 1, 1913, this will fail.
  As a session begins and ends on the same day."

  congress_date_reference = read_csv("build/congress_dates.csv")
  congress_date_reference$end[1] = ymd("2100-01-01")
    
  dates.assigned.tentatively = data.frame(dates) %>%
    unique() %>%
    cross_join(congress_date_reference) %>%
    filter(dates>=begin & dates<=end)

  #it's possible that a law is enacted on a day that starts and ends a session
  #e.g. December 1, 1913.
  # In that case, we really don't know what happened with it, 
  # so we will assign nothing
  multiply_assigned_dates = dates.assigned.tentatively %>%
    arrange(end) %>%
    group_by(dates) %>%
    count() %>%
    filter(n>1)

  appends = dates.assigned.tentatively %>%
    anti_join(multiply_assigned_dates) %>%
    select(dates,congress,session)
  
  data.frame(dates=dates) %>%
    left_join(appends) %>%
    pull(session)
  
}

lookup_congress_by_date <-  function(dates){
  "Attempts to lookup the session of a law based on its enactment date.
  Note that for some laws, e.g. those enacted on December 1, 1913, this will fail.
  As a session begins and ends on the same day."

  congress_session_date_reference = read_csv("build/congress_dates.csv") 
  congress_session_date_reference$end[1] = ymd("2100-01-01")
  
  congress_date_reference = congress_session_date_reference %>% 
    group_by(congress) %>%
    summarise(begin=min(begin),end=max(end))
    
  dates.assigned.tentatively = data.frame(dates) %>%
    unique() %>%
    cross_join(congress_date_reference) %>%
    filter(!is.na(dates)) %>%
    filter(dates>=begin & dates<=end)

  #it's possible that a law is enacted on a day that starts and ends a session
  #e.g. December 1, 1913.
  # In that case, we really don't know what happened with it, 
  # so we will assign nothing
  multiply_assigned_dates = dates.assigned.tentatively %>%
    arrange(end) %>%
    group_by(dates) %>%
    count() %>%
    filter(n>1)

  appends = dates.assigned.tentatively %>%
    anti_join(multiply_assigned_dates,by = join_by(dates)) %>%
    select(dates,congress)
  
  data.frame(dates=dates) %>%
    left_join(appends,by = join_by(dates)) %>%
    pull(congress)
  
}

lookup_congress_by_volume <- function(volumes){
  #first load the set of laws to reference
  master_laws = read_csv(here("build","master.csv"))
  
  # then extract a set of combinations of volumes and congresses
  potential_volume_congress_lookups = master_laws %>% 
    select(sal_volume,congress_number) %>% 
    distinct()
  
  # early volumes don't uniquely match to congresses, ignore those
  unique_volume_congress_lookups = potential_volume_congress_lookups  %>%
    group_by(sal_volume) %>%
    count() %>%
    filter(n==1) 

  #now extract the valid ones
  valid_volume_congress_lookups = potential_volume_congress_lookups %>%
    filter(sal_volume %in% unique_volume_congress_lookups$sal_volume)
  
  data.frame(sal_volume=as.numeric(volumes)) %>%
    left_join(valid_volume_congress_lookups) %>%
    pull(congress_number)
  
}

lookup_session_by_volume <- function(volumes){
  #first load the set of laws to reference
  master_laws = read_csv(here("build","master.csv"))
  
  # then extract a set of combinations of volumes and congresses
  potential_volume_congress_lookups = master_laws %>% 
    select(sal_volume,congress_number,session_number) %>% 
    distinct()
  
  # early volumes don't uniquely match to congresses, ignore those
  unique_volume_congress_lookups = potential_volume_congress_lookups  %>%
    group_by(sal_volume) %>%
    count() %>%
    filter(n==1) 
  
  #now extract the valid ones
  valid_volume_congress_lookups = potential_volume_congress_lookups %>%
    filter(sal_volume %in% unique_volume_congress_lookups$sal_volume)
  
  data.frame(sal_volume=as.numeric(volumes)) %>%
    left_join(valid_volume_congress_lookups) %>%
    pull(session_number)
  
}

lookup_session_by_pl <- function(pl_nos){
  "Assumes a conforming source, i.e. \\d+"
  
  master_laws = read_csv("build/master.csv")
  
  potential_links = master_laws %>% 
    filter(!is.na(pl_no)) %>%
    arrange(date_of_passage) %>%
    select(congress_number,session_number,pl_no) %>%
    separate(pl_no,into=c("cong_number_from_pl","law_no")) %>%
    group_by(congress_number,session_number) %>% 
    summarise(first=min(as.numeric(law_no)),last=max(as.numeric(law_no))) 

  # if public laws are recycled, this will detect congresses where that happens
  failed_links = potential_links %>%
    group_by(congress_number,first) %>%
    count() %>%
    filter(n>1) %>%
    pull(congress_number)
  
  links = potential_links %>%
    filter(!(congress_number %in% failed_links))
  
  possible_appends = data.frame(pl_nos) %>%
    separate(sep="-",pl_nos,into=c("congress_number","l"),remove = F) %>%
    mutate(congress_number=as.numeric(congress_number)) %>% 
    left_join(links,relationship='many-to-many') %>%
    filter(!is.na(session_number)) %>%
    filter(l>=first & l<=last)  %>%
    select(pl_nos,congress_number,session_number) 
  
  #now we don't want any NAs or multiples messing it up, so just drop
  
  rejected_pl_nos = possible_appends %>%
    group_by(pl_nos) %>%
    count() %>%
    filter(n>1) %>%
    pull(pl_nos)
  
  appends = possible_appends %>%
    filter(!(pl_nos %in% rejected_pl_nos)) %>%
    filter(!is.na(session_number))
  
  data.frame(pl_nos) %>%
    left_join(appends) %>%
    pull(session_number)
  
  
  
}

lookup_congress_by_pl <- function(pl_nos){
  "This is easier"
  str_extract(pl_nos,"(\\d+)\\-\\d+",group=1) %>% as.numeric()
}


lookup_congress <- function(dates,pl_nos,volumes){
  "An opinionated look up function"
  sources = data.frame(dates,pl_nos,volumes)
  
  appends = sources %>%
    mutate(congress_volume=lookup_congress_by_volume(volumes),
           congress_pl=lookup_congress_by_pl(pl_nos),
           congress_dates=lookup_congress_by_date(dates),
           congress=coalesce(congress_pl,congress_volume,congress_dates)) %>%
    pull(congress) 
  
}

lookup_session <- function(dates,pl_nos,volumes){
  "An opinionated look up function"
  sources = data.frame(dates,pl_nos,volumes)
  
  sources %>%
    mutate(session_volume=as.character(lookup_session_by_volume(volumes)),
           session_pl=as.character(lookup_session_by_pl(pl_nos)),
           session_dates=as.character(lookup_session_by_date(dates)),
           session=coalesce(session_pl,session_volume,session_dates))%>%
    pull(session) 
  
  
  
}






if (F){
  #data 
  x = read_csv(here("build","master.csv"))
  y = read_csv(here('build',"popnames.csv")) %>%
    select(-`...1`)
  
  
  x.vars = list(pl_no="pl_no",
                     congress_number="congress_number",
                     session="session_number",
                     chapter="chapter")
  
  y.vars = list(pl_no="pl_no",
                       congress_number="congress",
                       session="session",
                       chapter="chapter")


}


