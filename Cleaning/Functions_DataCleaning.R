############# Data Cleaning functions ###########



#out <- find_names(a, test, T)

find_names <- function(patterns, listnames, caseSensitiveTorF){
  listwrong <-NULL
  
  for (i in patterns){
    listwrong_temp <- listnames[grepl(i, listnames, ignore.case = caseSensitiveTorF)]
    listnames <- listnames[!grepl(i, listnames, ignore.case = caseSensitiveTorF)]
    listwrong <- c(listwrong_temp, listwrong)
  }
  listwrong <- unique(listwrong)
  return
  return(listwrong)
}


# 1)) ##### small but handy functions for cleaning data: ########
                                                                          

# 1a)  Function for deleting completely empty rows
# useful if somehow empty rows have been stored into the csv by excel

deleting_EmptyRows <- function(df){
  df <- df[rowSums(is.na(df)) < ncol(df), ] # only keep those rows where at least one value other than NA has been entered in any of the columns
  return(df)
}


# 1b) replacing wrongly entered things
replaceStuff <- function(list_wrong_strings, replacement, df, col_name){
  for (i in list_wrong_strings)
    df[[col_name]] <- gsub(i, replacement, df[[col_name]])
  return(df)
}


#1c assign treatment
assign.treatment <- function(df, 
                             Plot, 
                             Treatment
){
  
  df$Treatment <- NA 
  df$Treatment[which(df$Plot %in% c(1,6,11,13,14,15))] <- "T0" # control
  df$Treatment[which(df$Plot %in% c(2,7,9))] <- "T1" #T1
  df$Treatment[which(df$Plot %in% c(3,5,10))] <- "T2" #T2
  df$Treatment[which(df$Plot %in% c(4,8,12))] <- "T3" #
  return(df)
}

get_colnames <- function(dt, listStrings){
  #dt <- as.data.table(dt)
  #browser()
  mycolnames <- NULL
  for(i in listStrings){
    list <- names(dt[grepl(i, names(dt), ignore.case = T)])
    list <- unique(list)
    mycolnames <- c(mycolnames, list) # try with bind rows 
  }
  return(mycolnames)
}



##################### extract corresponding weightmeasurements FUNCTION
# works only thing is that the last updating of columns should work in one step with ':=' but it doesn't - so I did it in two steps

extract_corresponding_WeightMeas <- function(df, column, list_correspondingColumns, ending){
  require(data.table)
  df <- setDT(df) # turn it into a datatable
  dfnew <- data.table::copy(df) # Important: copy the original datatable otherwise it will be overwritten in the function
  
  #if(list_correspondingColumns != "none"){
  
  if(!is.null(list_correspondingColumns)) {
    
    for (i in list_correspondingColumns){ # here the function goes through the list of the corresponding columns that need to be updated
      
      dfnew <- dfnew[is.na(get(column)),  # the function only updates rows where there is missing information in the reference column
                     c(i) := get(paste0(i, ending)) # it updates it with the information of the column that is named the same with only the difference in ending
      ]
    }
  }
  
  #dfnew <-  dfnew[, c(Extracted.info) := 0L]
  
  # after updating the corresponding columns it now enteres a code that this information had been extracted
  dfnew <- dfnew[is.na(get(column)), 
                 c(paste0("Extracted_", column)) := 1
  ]
  # and updates the reference column in the end 
  dfnew <- dfnew[is.na(get(column)), 
                 c(column) := get(paste0(column, ending)) 
  ]
  
  #Extracted.info <- paste0("Extracted_", column)
  # should work in one go with this, but it doesnt:
  #dfnew <- dfnew[is.na(get(column)),  
  #                      `:=`( c(paste0("Extracted_", column)) = 1, 
  #                             c(column) = get(paste0(column, ending)) 
  # )]
  
  #     `:=`( c(Extracted.info) = 1L, c(column) = get(paste0(column, ending)) )]
  
  
  dfnew <- as_tibble(dfnew) # transforms it into a tibble for dplyr again - this could be changed once I switch completely to data.table
  return(dfnew) 
  
}




############### Renaming functions

## rename columnnames

# RWC - doesn't work

rename_columnsRWC <- function(df, # not in ""
                              RWCnr, # all other variable names in "variable_name1" etc... 
                              RWCnr_bigbag,
                              RWCnr_original,
                              resampled,
                              Comment_RWCnr,
                              
                              ### Info on individual
                              idTree,
                              specCode,
                              Plot,
                              SubPlot,
                              TreeFieldNum,
                              
                              ### Weight measurements
                              # Weight measurements for entire leaf
                              BW_E.g,
                              FW.BW_E.g,
                              SW_E.g,
                              DW_E.g,
                              
                              # Weight measurements for Discs
                              BW_D.g,
                              FW.BW_D.g,
                              SW_D.g,
                              DW_D.g,
                              
                              # addtional information on discs
                              DiscDiam.mm,
                              DiscNrLeaves,
                              DiscNr_FW,
                              DiscNr_DW,
                              
                              # Dates measurement
                              Date_BW,
                              Date_Field,
                              Date_FW,
                              Date_SW,
                              Date_DW,
                              
                              # Time measurments
                              Time_FW,
                              Time_SW,
                              Time_DW,
                              
                              # Info on operator and balance used
                              Name_BW,
                              Name_FW,
                              Name_SW,
                              Name_DW,
                              Name_Balance,
                              
                              # Comments
                              Comment_BW,
                              Comment_FW,
                              Comment_SW,
                              Comment_Lab,
                              Comment_Field, 
                              
                              # Project
                              
                              Name_Project,
                              Name_Site,
                              Name_Fieldseason
){
  
  require("dplyr") 
  require("rlang")
  
  is.defined <- function(sym) {
    sym <- deparse(substitute(sym))
    env <- parent.frame()
    exists(sym, env)
  }

  
  df <- df %>% dplyr::transmute(RWCnr = !! rlang::sym(RWCnr),
                                
                                RWCnr_bigbag = ifelse(is.defined(RWCnr_bigbag) == TRUE, !! rlang::sym(RWCnr_bigbag),  NA),
                                #RWCnr_bigbag = ifelse(!! RWCnr_bigbag == "NA", NA, !! rlang::sym(RWCnr_bigbag)),
                                #RWCnr_bigbag = ifelse(!! rlang::sym(RWCnr_bigbag) == "skipped", NA, !! rlang::sym(RWCnr_bigbag)),
                                RWCnr_original = ifelse(!! RWCnr_original == "skipped", NA, !! rlang::sym(RWCnr_original)),
                                resampled = ifelse(!! resampled == "NA", NA, !! rlang::sym(resampled)),
                                
                                Comment_RWCnr = ifelse(!! Comment_RWCnr == "NA", NA, !! rlang::sym(Comment_RWCnr)),
                                
                                ### Info on individual
                                idTree = !! rlang::sym(idTree),
                                specCode = !! rlang::sym(specCode),
                                Plot = !! rlang::sym(Plot),
                                SubPlot = !! rlang::sym(SubPlot),
                                TreeFieldNum = !! rlang::sym(TreeFieldNum),
                                
                                ### Weight measurements
                                # Weight measurements for entire leaf
                                BW_E.g = !! rlang::sym(BW_E.g),
                                FW.BW_E.g = !! rlang::sym(FW.BW_E.g),
                                SW_E.g = !! rlang::sym(SW_E.g),
                                DW_E.g = !! rlang::sym(DW_E.g),
                                
                                # Weight measurements for Discs
                                BW_D.g = ifelse(!! BW_D.g == "NA", NA, !! rlang::sym(BW_D.g)),
                                FW.BW_D.g = ifelse(!! FW.BW_D.g == "NA", NA, !! rlang::sym(FW.BW_D.g)),
                                SW_D.g = ifelse(!! SW_D.g == "NA", NA, !! rlang::sym(SW_D.g)),
                                DW_D.g = ifelse(!! DW_D.g == "NA", NA, !! rlang::sym(DW_D.g)),
                                
                                #addtional information on discs
                                DiscDiam.mm = ifelse(!! DiscDiam.mm == "NA", NA, !! rlang::sym(DiscDiam.mm)),
                                DiscNrLeaves = ifelse(!! DiscNrLeaves == "NA", NA, !! rlang::sym(DiscNrLeaves)),
                                DiscNr_FW = ifelse(!! DiscNr_FW == "NA", NA, !! rlang::sym(DiscNr_FW)),
                                DiscNr_DW = ifelse(!! DiscNr_DW == "NA", NA, !! rlang::sym(DiscNr_DW)),
                                
                                # Dates measurement
                                Date_BW = ifelse(!! Date_BW == "NA", NA, !! rlang::sym(Date_BW)),
                                Date_Field = ifelse(!! Date_Field == "NA", NA, !! rlang::sym(Date_Field)),
                                Date_FW = ifelse(!! Date_FW == "NA", NA, !! rlang::sym(Date_FW)),
                                Date_SW = ifelse(!! Date_SW == "NA", NA, !! rlang::sym(Date_SW)),
                                Date_DW = ifelse(!! Date_DW == "NA", NA, !! rlang::sym(Date_DW)),
                                
                                # Time measurments
                                Time_FW = ifelse(!! Time_FW == "NA", NA, !! rlang::sym(Time_FW)),
                                Time_SW = ifelse(!! Time_SW == "NA", NA, !! rlang::sym(Time_SW)),
                                Time_DW = ifelse(!! Time_DW == "NA", NA, !! rlang::sym(Time_DW)),
                                
                                # Info on operator and balance used
                                Name_BW = ifelse(!! Name_BW == "NA", NA, !! rlang::sym(Name_BW)),
                                Name_FW = ifelse(!! Name_FW == "NA", NA, !! rlang::sym(Name_FW)),
                                Name_SW = ifelse(!! Name_SW == "NA", NA, !! rlang::sym(Name_SW)),
                                Name_DW = ifelse(!! Name_DW == "NA", NA, !! rlang::sym(Name_DW)),
                                Name_Balance = !! Name_Balance,
                                
                                # Comments
                                Comment_BW = ifelse(!! Comment_BW == "NA", NA, !! rlang::sym(Comment_BW)),
                                Comment_FW = ifelse(!! Comment_FW == "NA", NA, !! rlang::sym(Comment_FW)),
                                Comment_SW = ifelse(!! Comment_SW == "NA", NA, !! rlang::sym(Comment_SW)),
                                Comment_Lab = ifelse(!! Comment_Lab == "NA", NA, !! rlang::sym(Comment_Lab)),
                                Comment_Field = ifelse(!! Comment_Field == "NA", NA, !! rlang::sym(Comment_Field)),
                                
                                # Project
                                Name_Project = !! Name_Project,
                                Name_Site = !! Name_Site, 
                                Name_Fieldseason = !! Name_Fieldseason
  )
  print(is.defined(RWCnr_bigbag))
  print(!! rlang::sym(resampled))
  
  return(df)
  
}






