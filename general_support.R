


loadPkg("xtable")
loadPkg("kableExtra")
loadPkg("stringi")

xkabledply = function(modelsmmrytable, title="Table", digits = 4, pos="left", bso="striped", wide=FALSE) { 
  #' Combining base::summary, xtable, and kableExtra, to easily display model summary. 
  #' wrapper for the base::summary function on model objects
  #' Can also use as head for better display
  #' ELo 202004 GWU DATS
  #' version 1.2
  #' @param modelsmmrytable This can be a generic table, a model object such as lm(), or the summary of a model object summary(lm()) 
  #' @param title Title of table. 
  #' @param digits Number of digits to display
  #' @param pos Position of table, c("left","center","right") 
  #' @param bso bootstrap_options = c("basic", "striped", "bordered", "hover", "condensed", "responsive")
  #' @param wide print table in long (FALSE) format or wide (TRUE) format
  #' @return HTML table for display
  #' @examples
  #' library("xtable")
  #' library("kableExtra")
  #' xkabledply( df, title="Table testing", pos="left", bso="hover" )
  #' xkabledply( ISLR::Hitters[1:5,] )
  if (wide) { modelsmmrytable <- t(modelsmmrytable) }
  modelsmmrytable %>%
    xtable() %>% 
    kable(caption = title, digits = digits) %>%
    kable_styling(bootstrap_options = bso, full_width = FALSE, position = pos)
}

xkabledplyhead = function(df, rows=5, title="Head", digits = 4, pos="left", bso="striped") { 
  xkabledply(df[1:rows, ], title, digits, pos, bso, wide=FALSE)
}

xkabledplytail = function(df, rows=5, title="Tail", digits = 4, pos="left", bso="striped") { 
  trows = nrow(df)
  xkabledply(df[ (trows-rows+1) : trows, ], title, digits, pos, bso, wide=FALSE)
}

xkablesummary = function(df, title="Table: Statistics summary.", digits = 4, pos="left", bso="striped") { 
  #' Combining base::summary, xtable, and kableExtra, to easily display numeric variable summary of dataframes. 
  #' ELo 202004 GWU DATS
  #' version 1.2
  #' @param df The dataframe.
  #' @param title Title of table. 
  #' @param digits Number of digits to display
  #' @param pos Position of table, c("left","center","right") 
  #' @param bso bootstrap_options = c("basic", "striped", "bordered", "hover", "condensed", "responsive")
  #' @return The HTML summary table for display, or for knitr to process into other formats 
  #' @examples
  #' xkablesummary( faraway::ozone )
  #' xkablesummary( ISLR::Hitters, title="Five number summary", pos="left", bso="hover"  )
  
  s = summary(df) %>%
    apply( 2, function(x) stringr::str_remove_all(x,c("Min.\\s*:\\s*","1st Qu.\\s*:\\s*","Median\\s*:\\s*","Mean\\s*:\\s*","3rd Qu.\\s*:\\s*","Max.\\s*:\\s*")) ) %>% # replace all leading words
    apply( 2, function(x) stringr::str_trim(x, "right")) # trim trailing spaces left
  
  colnames(s) <- stringr::str_trim(colnames(s))
  
  if ( dim(s)[1] ==6 ) { rownames(s) <- c('Min','Q1','Median','Mean','Q3','Max') 
  } else if ( dim(s)[1] ==7 ) { rownames(s) <- c('Min','Q1','Median','Mean','Q3','Max','NA') }
  
  xkabledply(s, title=title, digits = digits, pos=pos, bso=bso )
}


xkablevif = function(model, title="VIFs of the model", digits = 3, pos="left", bso="striped", wide=TRUE) { 
  #' Combining faraway::vif, xtable, and kableExtra, to easily display numeric summary of VIFs for a model. 
  #' ELo 202004 GWU DATS
  #' version 1.2
  #' @param model The lm or compatible model object.
  #' @param title Title of table. 
  #' @param digits Number of digits to display
  #' @param pos Position of table, c("left","center","right") 
  #' @param bso bootstrap_options = c("basic", "striped", "bordered", "hover", "condensed", "responsive")
  #' @param wide print table in long (FALSE) format or wide (TRUE) format
  #' @return The HTML summary table of the VIFs for a model for display, or for knitr to process into other formats 
  #' @examples
  #' xkablevif( lm(Salary~Hits+RBI, data=ISLR::Hitters, wide=T ) )
  
  df <- data.frame(vif(model))                            
  names(df) <- "VIF"
  
  if (wide) { df <- t(df) }
  xkabledply(df, title=title, digits = digits, pos=pos, bso=bso )    
}

unloadPkg(xtable)
unloadPkg(kableExtra)
unloadPkg(stringi)