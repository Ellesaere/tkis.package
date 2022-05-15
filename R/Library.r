
options(scipen = 999) # options(scipen = 0)
# options(digits=2)

shut_up = function(expr) {
  #temp file
  f = file()

  #write output to that file
  sink(file = f)

  #evaluate expr in original environment
  y = eval(expr, envir = parent.frame())

  #close sink
  sink()

  #get rid of file
  close(f)

  y
}
##########################################################################################################################################################################################################################
cleanfunction <- function(dataframe) {
  dataframe <- as.data.frame(dataframe)
  ## get mode of all vars
  var_mode <- sapply(dataframe, mode)
  ## produce error if complex or raw is found
  if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
  ## get class of all vars
  var_class <- sapply(dataframe, class)
  ## produce error if an "AsIs" object has "logical" or "character" mode
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
      stop("matrix variables with 'AsIs' class must be 'numeric'")
      }
  ## identify columns that needs be coerced to factors
  ind1 <- which(var_mode %in% c("logical", "character"))
  ## coerce logical / character to factor with `as.factor`
  dataframe[ind1] <- lapply(dataframe[ind1], as.factor)
  return(dataframe)
}

cleanfunction_DT <- function(dataframe) {
  setDT(dataframe)
  ## get mode of all vars
  var_mode <- sapply(dataframe, mode)
  ## produce error if complex or raw is found
  if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
  ## get class of all vars
  var_class <- sapply(dataframe, class)
  ## produce error if an "AsIs" object has "logical" or "character" mode
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
      stop("matrix variables with 'AsIs' class must be 'numeric'")
      }
  ## identify columns that needs be coerced to factors
  ind1 <- which(var_mode %in% c("logical", "character"))
  ## coerce logical / character to factor with `as.factor`
  if (length(ind1)) dataframe[, c(ind1) := lapply(.SD, as.factor), .SDcols = ind1]
  return(dataframe)
}

overview <- function(df) {
  df <- as.data.frame(df)
  labels <- setNames(stack(lapply(df, label))[2:1], c("Varcode", "Description")) # Get labels
  # classes <- as.data.frame(sapply(df, class))
  classes <- sapply(df, function(x) paste(class(x), collapse = '-'))
  classes <- as.data.frame(classes)
  modes <- as.data.frame(sapply(df, mode))
  #attributes  <- as.data.frame(sapply(df, attributes))
  obs <- as.data.frame(colSums(!is.na(df))) # Get observations
  labels <- setDT(labels, keep.rownames = FALSE)[]
  classes <- setDT(classes, keep.rownames = TRUE)[]
  modes <- setDT(modes, keep.rownames = TRUE)[]
  #attributes <- setDT(attributes, keep.rownames = TRUE)[]
  obs <- setDT(obs, keep.rownames = TRUE)[]
  obs <- merge(obs, classes,  by= "rn", all.x = TRUE, allow.cartesian=TRUE)
  obs <- merge(obs, modes,  by= "rn", all.x = TRUE, allow.cartesian=TRUE)
  #obs <- merge(obs, attributes,  by= "rn", all.x = TRUE, allow.cartesian=TRUE)
  obs <- obs[labels, on=c(rn = "Varcode")] # https://stackoverflow.com/questions/34644707/left-outer-join-with-data-table-with-different-names-for-key-variables 
  colnames(obs)[1]<-"var"
  colnames(obs)[2]<-"Observations"
  colnames(obs)[3] <- "varclass" 
  colnames(obs)[4] <- "varmode" 
  obs <- as.data.table(obs)
  return(obs)
}


overview2 <- function(DT, corvar=NULL) { # , Group=NULL
  # Melt - Take differences - Cast                                        # https://stackoverflow.com/questions/57406654/speeding-up-a-function/57407959#57407959
  DT <- as.data.table(DT)                                                 # Make sure it is a data.table 
  DT [, uniqueID := .I]                                                   # Add a unique ID
  labels <- setNames(stack(lapply(DT, label))[2:1], c("Varcode", "Variables")) # Get labels
  cols = sapply(DT, is.numeric)                                           # Check numerical columns
  mean <- DT[,lapply(Filter(is.numeric,.SD),mean, na.rm=TRUE)]
  mean <- as.data.frame(t(mean))
  variance <- DT[,lapply(Filter(is.numeric,.SD),var, na.rm=TRUE)]
  variance <- as.data.frame(t(variance))
  # Correlation
  numcols <- names(Filter(is.numeric,DT))
  corr_y <- DT[, data.table(var=numcols, cor(.SD[, mget(numcols)], .SD[, mget(corvar)],use= "pairwise.complete.obs", method= "pearson")   )] #, by=NULL]
  corr_y_sign <-sapply(Filter(is.numeric, DT), function(x) tryCatch(cor.test(x, DT[, get(corvar)], use= "pairwise.complete.obs", method= "pearson")$p.value,error=function(e) NULL)) 
  corr_y_sign <-as.data.frame(t(corr_y_sign))
  corr_y_sign <-as.data.frame(t(corr_y_sign))
  # corr_y_sign_c <<- as.data.table(corr_y_sign)
  # corr_y_sign <-sapply(Filter(is.numeric, DT), function(x) cor.test(x, DT[, get(corvar)], use= "pairwise.complete.obs", method= "pearson")$p.value)
  # Values
  obs <- as.data.frame(colSums(!is.na(DT)))  # Get observations
  numcols <- names(Filter(is.numeric,DT)) 
  # Merging
  obs <- setDT(obs, keep.rownames = TRUE)[]
  labels <- setDT(labels, keep.rownames = FALSE)[]
  mean <- setDT(mean, keep.rownames = TRUE)[]
  variance <- setDT(variance, keep.rownames = TRUE)[]
  variance <- setDT(corr_y_sign, keep.rownames = TRUE)[]
  colnames(mean)[2] <- "mean"
  colnames(corr_y)[1] <- "rn"
  colnames(corr_y_sign)[1] <- "rn"
  obs <- merge(obs, mean, by= "rn", all.x = TRUE, allow.cartesian=TRUE)
  obs <- merge(obs, variance,  by= "rn", all.x = TRUE, allow.cartesian=TRUE)
  obs <- merge(obs, corr_y,  by= "rn", all.x = TRUE, allow.cartesian=TRUE)
  obs <- merge(obs, corr_y_sign,  by= "rn", all.x = TRUE, allow.cartesian=TRUE)
  obs <- obs[labels, on=c(rn = "Varcode")]
  colnames(obs)[1] <- "var"
  colnames(obs)[2] <- "Observations"
  colnames(obs)[4] <- "variance"
  colnames(obs)[5] <- "corr_y"
  colnames(obs)[6] <- "corr_y_sign"
  colnames(obs)[7] <- "Description"
  obs <- as.data.table(obs)
  return(obs)
}


as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


deltavar_overview <- function(DT, panelID, corvar=NULL) { # , Group=NULL
  # Melt - Take differences - Cast                                        # https://stackoverflow.com/questions/57406654/speeding-up-a-function/57407959#57407959
  DT <- as.data.table(DT)                                                 # Make sure it is a data.table 
  DT [, uniqueID := .I]                                                   # Add a unique ID
  obs <- as.data.frame(colSums(!is.na(DT))) # Get observations
  labels <- setNames(stack(lapply(DT, label))[2:1], c("Varcode", "Variables")) # Get labels
  cols = sapply(DT, is.numeric)                                           # Check numerical columns
  mean <- DT[,lapply(Filter(is.numeric,.SD),mean, na.rm=TRUE)]
  mean <- as.data.frame(t(mean))
  DTm <- melt(DT[, cols, with = FALSE][, !"uniqueID"], id = panelID)    # https://stackoverflow.com/questions/57406654/speeding-up-a-function/57407959#57407959
  DTm[, value := c(NA, diff(value)), by = .(panelID, variable)]           # https://stackoverflow.com/questions/57406654/speeding-up-a-function/57407959#57407959
  DT <- dcast(DTm, panelID + rowidv(DTm, cols = c("panelID", "variable")) ~ variable, value.var = "value") # ""
  DT <- DT[DT[, !Reduce(`&`, lapply(.SD , is.na)), .SDcols = 3:ncol(DT)]] # Removes T1 for which there is no difference
  # Correlation
  numcols <- names(Filter(is.numeric,DT)) 
  p_corr_y <- DT[, data.table(var=numcols, cor(.SD[, mget(numcols)], .SD[, mget(corvar)],use= "pairwise.complete.obs", method= "pearson"))] #, by=NULL]
  sp_corr_y <- DT[, data.table(var=numcols, cor(.SD[, mget(numcols)], .SD[, mget(corvar)],use= "pairwise.complete.obs", method= "spearman"))] #, by=NULL]
  # Values
  mean_of_differences <- DT[,lapply(Filter(is.numeric,.SD),mean, na.rm=TRUE)]
  mean_of_differences <- as.data.frame(t(mean_of_differences))
  mean_of_absolute_diff <- DT[,lapply(Filter(is.numeric,.SD),function(x) mean(abs(x),na.rm=TRUE))]
  mean_of_absolute_diff <- as.data.frame(t(mean_of_absolute_diff))
  mean_var_diff <- DT[,lapply(Filter(is.numeric,.SD),function(x) mean(var(x),na.rm=TRUE))]
  mean_var_diff <- as.data.frame(t(mean_var_diff))
  numcols <- names(Filter(is.numeric,DT))
  # Merging
  labels <- setDT(labels, keep.rownames = FALSE)[]
  mean <- setDT(mean, keep.rownames = TRUE)[]
  colnames(mean)[2] <- "mean"
  mean_of_differences <- setDT(mean_of_differences, keep.rownames = TRUE)[]
  mean_of_absolute_diff <- setDT(mean_of_absolute_diff, keep.rownames = TRUE)[]
  mean_var_diff <- setDT(mean_var_diff, keep.rownames = TRUE)[]
  obs <- setDT(obs, keep.rownames = TRUE)[]
  obs <- merge(obs, mean, by= "rn", all.x = TRUE, allow.cartesian=TRUE)
  obs <- merge(obs, mean_of_differences,  by= "rn", all.x = TRUE, allow.cartesian=TRUE)
  obs <- merge(obs, mean_of_absolute_diff,  by= "rn", all.x = TRUE, allow.cartesian=TRUE)
  obs <- merge(obs, mean_var_diff,  by= "rn", all.x = TRUE, allow.cartesian=TRUE)
  colnames(p_corr_y)[1] <- "rn"
  colnames(sp_corr_y)[1] <- "rn"
  obs <- merge(obs, p_corr_y,  by= "rn", all.x = TRUE, allow.cartesian=TRUE)
  obs <- merge(obs, sp_corr_y,  by= "rn", all.x = TRUE, allow.cartesian=TRUE)
  obs <- obs[labels, on=c(rn = "Varcode")]
  colnames(obs)[1] <- "var"
  colnames(obs)[2] <- "Observations"
  colnames(obs)[4] <- "mean_of_differences" 
  colnames(obs)[5] <- "mean_of_absolute_diff"
  colnames(obs)[6] <- "mean_var_diff" 
  colnames(obs)[7] <- "p_corr_y"
  colnames(obs)[8] <- "sp_corr_y"
  obs[,relative_diff := mean_of_differences/mean]
  obs[,relative_diff_abs := mean_of_absolute_diff/mean]
  obs[,relative_diff_var := mean_var_diff/mean]
  obs <- as.data.table(obs)
  return(obs)
}


rbindlistfun <- function(df1, df2) {
  x <- rbindlist(list(df1, df2), fill=TRUE, use.names=TRUE)
  return (x)
}


is.haven <- function(x) "labelled" %in% class(x) # filteredES1 <- Filter(is.haven, ES1)


get_var_corr<- function (df, comparison_var, other_vars = NULL, get_all = TRUE,
                         method= "pearson",...) {
  columns <- setdiff(names(df), comparison_var)
  if (get_all == TRUE){
    if(method=="pearson") {
    res <- plyr::ldply(lapply(columns, function(x) {
      res1 <- cor.test(get(comparison_var, as.environment(df)),
                       get(x, as.environment(df)),method=method,...)
      data.frame(Comparison_Var = comparison_var, Other_Var = x,
                 p_value = res1$p.value, Correlation = res1$estimate,
                 lower_ci= res1$conf.int[1], upper_ci= res1$conf.int[2]
                 )
    }), data.frame)
    res
    }
    else{
      res <- plyr::ldply(lapply(columns, function(x) {
        res1 <- cor.test(get(comparison_var, as.environment(df)),
                         get(x, as.environment(df)),method=method,...)

        data.frame(Comparison_Var = comparison_var, Other_Var = x,
                   p.value = res1$p.value, Correlation= res1$estimate)
      }), data.frame)
      res
    }
  }
    else{
      if(method=="pearson"){
 res <- plyr::ldply(lapply(other_vars, function(x) {
      res1 <- cor.test(get(comparison_var, as.environment(df)),
                       get(x, as.environment(df)),method=method,...)

      data.frame(Comparison_Var = comparison_var, Other_Var = x,
                 p.value = res1$p.value, Correlation= res1$estimate,
                 lower_ci= res1$conf.int[1], upper_ci= res1$conf.int[2])
    }), data.frame)
    res
      }
    else{
      res <- plyr::ldply(lapply(other_vars, function(x) {
        res1 <- cor.test(get(comparison_var, as.environment(df)),
                         get(x, as.environment(df)),method=method,...)

        data.frame(Comparison_Var = comparison_var, Other_Var = x,
                   p.value = res1$p.value, Correlation= res1$estimate)
      }), data.frame)
      res
    }

    }
}


first_preds <- function(dat, predictor) {
  cols <- which(dat[predictor, ] == 1)
  names(dat)[cols]
}


all_preds <- function(dat, predictors) {
  unique(unlist(lapply(predictors, function(x) names(dat)[which(dat[x, ] == 1 )])))
}


labelfun <- function(targetdf, sourcedf=ES1obs, columnlab=Variables, columnvar=var)
for (i in seq_len(ncol(targetdf))) { 
    label(targetdf[[i]]) <-  sourcedf$columnlab[match(names(targetdf)[i], sourcedf$columnvar)] 
 }

# lapply(ES1, attributes)

# https://stackoverflow.com/questions/57101282/a-function-for-referring-to-columns-by-either-name-or-index/57101418?noredirect=1#comment100723955_57101418


removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}


moveMeDataTable <-function(data, tomove, where = "first", ba = NULL) {
  data <- setDT(data)
  suppressWarnings(nums <- as.numeric(tomove))
  nums[is.na(nums)] <- suppressWarnings(sapply(tomove[is.na(as.numeric(tomove))], 
                              function(x) which(names(data) == x)))
  nums <- unlist(nums, use.names=FALSE)
  tomove <- (names(data)[nums])  
  temp <- setdiff(names(data), tomove)
  tomove <- unique(tomove)

  x <- switch(
    where,
    first = setcolorder(data,c(tomove, temp)),
    last = setcolorder(data,c(temp, tomove)),
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      order = append(temp, values = tomove, after = (match(ba, temp)-1))
      setcolorder(data,order)
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      order = append(temp, values = tomove, after = (match(ba, temp)))
      setcolorder(data,order)
    })
  x
}


moveMeDataTable <-function(data, tomove, where = "last", ba = NULL) {
  data <- setDT(data)
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = setcolorder(data,c(tomove, temp)),
    last = setcolorder(data,c(temp, tomove)),
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      order = append(temp, values = tomove, after = (match(ba, temp)-1))
      setcolorder(data,order)

    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      order = append(temp, values = tomove, after = (match(ba, temp)))
      setcolorder(data,order)
    })
  x
}


reform <- function(x) paste(x, collapse = " + ")
makeFo <- function(lhs, rhs1, rhs2 = NULL, env = parent.frame()) {
  s <- sprintf("%s ~ %s", lhs, reform(c(rhs1, rhs2)))
  if (!missing(rhs2)) s <- sprintf("%s | %s", s, reform(rhs2))
  as.formula(s, env = env)
}
# test
makeFo("y", c("x1", "x2"))
## y ~ x1 + x2

makeFo("y", c("x1", "x2"), c("u1", "u2"))
## y ~ x1 + x2 + u1 + u2 | u1 + u2


var_select <- function(...){
  as.character(as.list(match.call()[-1L]))
}


debug_contr_error <- function (dat, subset_vec = NULL) {
  if (!is.null(subset_vec)) {
    ## step 0
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
        }
      subset_log_vec <- subset_vec
      } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
        } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
        } 
      } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
      }
    dat <- base::subset(dat, subset = subset_log_vec)
    } else {
    ## step 1
    dat <- stats::na.omit(dat)
    }
  if (nrow(dat) == 0L) warning("no complete cases")
  ## step 2
  var_mode <- sapply(dat, mode)
  if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
  var_class <- sapply(dat, class)
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
    stop("matrix variables with 'AsIs' class must be 'numeric'")
    }
  ind1 <- which(var_mode %in% c("logical", "character"))
  dat[ind1] <- lapply(dat[ind1], as.factor)
  ## step 3
  fctr <- which(sapply(dat, is.factor))
  if (length(fctr) == 0L) warning("no factor variables to summary")
  ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
  dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)
  ## step 4
  lev <- lapply(dat[fctr], base::levels.default)
  nl <- lengths(lev)
  ## return
  list(nlevels = nl, levels = lev)
  }


## note: this function relies on `debug_contr_error`
debug_contr_error2 <- function (form, dat, subset_vec = NULL) {
  ## step 0
  if (!is.null(subset_vec)) {
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
        }
      subset_log_vec <- subset_vec
      } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
        } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
        } 
      } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
      }
    dat <- base::subset(dat, subset = subset_log_vec)
    }
  ## step 0 and 1
  dat_internal <- stats::lm(form, data = dat, method = "model.frame")
  attr(dat_internal, "terms") <- NULL
  ## rely on `debug_contr_error` for steps 2 to 4
  c(list(mf = dat_internal), debug_contr_error(dat_internal, NULL))
  }


NA_preproc <- function (dat) {
  for (j in 1:ncol(dat)) {
    x <- dat[[j]]
    if (is.factor(x) && anyNA(x)) dat[[j]] <- base::addNA(x)
    if (is.character(x)) dat[[j]] <- factor(x, exclude = NULL)
    }
  dat
  }

# https://stackoverflow.com/questions/64442511/writing-code-function-that-matches-column-names-by-highest-similarity/64483921#64483921


fuzzy_rowbind <- function(a, b, method = "cosine", max_dist = 0.9999) {
  a_name_df <- tibble(name = names(a))
  b_name_df <- tibble(name = names(b))
  
  fj <- 
    fuzzyjoin::stringdist_join(
      a_name_df,
      b_name_df, 
      by = "name",
      mode = "left",
      ignore_case = FALSE, 
      method = method, 
      max_dist = max_dist, 
      distance_col = "dist"
    ) %>%
    arrange(dist)
  
  name_mapping <- NULL
  while (nrow(fj) > 0 && !all(b_name_df$name %in% name_mapping$name.y)) {
    name_mapping <- bind_rows(name_mapping, fj %>% dplyr::slice(1))
    
    fj <- fj %>% filter(!name.x %in% name_mapping$name.x, !name.y %in% name_mapping$name.y)
  }
  
  new_names <- setNames(name_mapping$name.y, name_mapping$name.x)
  
  b_renamed <- rename(b, new_names[!is.na(new_names)])
  
  enframe(new_names, name = "new_name", value = "original_name") %>%
    filter(new_name != original_name, !is.na(new_name)) %>%
    as.data.frame() %>%
    print()
  cat("\n")
  
  bind_rows(a, b_renamed)
}


fuzzy_rowbind_all <- function(l) {
  last(accumulate(l, fuzzy_rowbind))
}

# Fuzzy merge a column with a list
# https://stackoverflow.com/questions/71866485/fuzzy-joining-a-column-with-a-list

make_formula <- function(depvar, exogenous=NULL, instruments=NULL, instrumentedvar=NULL, othervars=NULL, type=NULL) {
if (is.null(type) || type == "lm") {
	exogenous <- unique(exogenous)                          # Checks if there are no doubles in the independent variables
	exogenous <- exogenous[!exogenous %in% depvar]          # Checks if the dependent variable is not present in exogenous
	exogenous <- exogenous[!exogenous %in% instruments]     # Checks if the instrument is not present in exogenous
    # print("form = depvar ~ exogenous")
    rhs <- c(as.character(instrumentedvar), as.character(exogenous))
    formula <- as.formula(paste(depvar, paste(rhs, collapse = " + "), sep = " ~ "))
  } else if (type=="all") {
    rhs <- c(as.character(instrumentedvar), as.character(instruments), as.character(exogenous))
    formula <- as.formula(paste(depvar, paste(rhs, collapse = " + "), sep = " ~ "))
  } else if (type=="1st") {
	exogenous <- unique(exogenous)                          # Checks if there are no doubles in the independent variables
	exogenous <- exogenous[!exogenous %in% depvar]          # Checks if the dependent variable is not present in exogenous
	exogenous <- exogenous[!exogenous %in% instruments]     # Checks if the instrument is not present in exogenous
    # print("form = instrumentedvar ~ exogenous + instruments")
    rhs <- c(as.character(instruments), as.character(exogenous))
    rhs <- rhs[!rhs %in% instrumentedvar]
    formula <- as.formula(paste(instrumentedvar, paste(rhs, collapse = " + "), sep = " ~ "))
  } else if (type=="2nd") {
    # print("form = depvar ~ exogenous + instrumentedvar | exogenous + instruments")
	exogenous <- unique(exogenous)                          # Checks if there are no doubles in the independent variables
	exogenous <- exogenous[!exogenous %in% depvar]          # Checks if the dependent variable is not present in exogenous
	exogenous <- exogenous[!exogenous %in% instruments]     # Checks if the instrument is not present in exogenous
    rhsA <- c(as.character(instrumentedvar), as.character(exogenous)) # Instrumented var only necessary if not specified in independentvars
    rhsA <- rhsA[!rhsA %in% instruments]
    rhsB <- c(as.character(instruments), as.character(exogenous))
    rhsB <- rhsB[!rhsB %in% instrumentedvar]
    formula <- as.formula(
    paste(depvar, paste(paste(rhsA, collapse = " + "), paste("|", paste(rhsB, collapse=" + " ))), sep= " ~ "))
  } else if (type=="haus_end_2nd") {
    exogenous <- unique(exogenous)                          # Checks if there are no doubles in the independent variables
	exogenous <- exogenous[!exogenous %in% depvar]          # Checks if the dependent variable is not present in exogenous
	exogenous <- exogenous[!exogenous %in% instruments]     # Checks if the instrument is not present in exogenous
    # print("form = depvar ~ exogenous + resid")
    rhs <- c(as.character(othervars), as.character(exogenous))
    formula <- as.formula(paste(depvar, paste(rhs, collapse = " + "), sep = " ~ "))
  } else if (type=="haus_over_2nd") {
    exogenous <- unique(exogenous)                          # Checks if there are no doubles in the independent variables
	exogenous <- exogenous[!exogenous %in% depvar]          # Checks if the dependent variable is not present in exogenous
	exogenous <- exogenous[!exogenous %in% instruments]     # Checks if the instrument is not present in exogenous
    # print("form = depvar ~ exogenous + instruments")
    rhs <- c(as.character(exogenous), as.character(instruments))
    rhs <- rhs[!rhs %in% instrumentedvar]
    formula <- as.formula(paste(depvar, paste(rhs, collapse = " + "), sep = " ~ "))
  } 
return(formula)
}

# IVsubset_1 <- function (dataset, depvar, instrumentedvar, instruments, exogenous, cluster=NULL, method=NULL) {
#   # Step 1 = Subset rows with NA's
#   # sub_set <- dataset %>% filter_at(vars(exogenous, depvar, instrumentedvar, instruments), all_vars(!is.na(.))) %>% as.data.table(.)
#   sub_set <- dataset
#   sub_set <- dplyr::select(sub_set, depvar, instrumentedvar, instruments, exogenous)
#   #write.dta(sub_set, "C:/Rsaves/Data Files/ES1_ivsub.dta")
#   write.dta(sub_set, "C:/Users/tkisters/Dropbox/PhD/Proposal/Data/2) Working Data/Data Files/ES1_ivsub.dta")
# }

# IVsubset_2 <- function (dataset, depvar, instrumentedvar, instruments, exogenous, cluster=NULL, method=NULL) {
#   # Step 1 = Subset rows with NA's
#   # sub_set <- dataset %>% filter_at(vars(exogenous, depvar, instrumentedvar, instruments), all_vars(!is.na(.))) %>% as.data.table(.)
#   sub_set <- dataset
#   sub_set <- dplyr::select(sub_set, depvar, instrumentedvar, instruments, exogenous)
#   #write.dta(sub_set, "C:/Rsaves/Data Files/ES1_ivsub_p.dta")
#   write.dta(sub_set, "C:/Users/tkisters/Dropbox/PhD/Proposal/Data/2) Working Data/Data Files/ES1_ivsub_p.dta")
# }

# IVsubset_3 <- function (dataset, depvar, instrumentedvar, instruments, exogenous, cluster=NULL, method=NULL) {
#   # Step 1 = Subset rows with NA's
#   # sub_set <- dataset %>% filter_at(vars(exogenous, depvar, instrumentedvar, instruments), all_vars(!is.na(.))) %>% as.data.table(.)
#   sub_set <- dataset
#   sub_set <- dplyr::select(sub_set, depvar, instrumentedvar, instruments, exogenous)
#   #write.dta(sub_set, "C:/Rsaves/Data Files/ES1_ivsub_sub.dta")
#   write.dta(sub_set, "C:/Users/tkisters/Dropbox/PhD/Proposal/Data/2) Working Data/Data Files/ES1_ivsub_sub.dta")
# }

# IVfun <- function (dataset, depvar, instrumentedvar, instruments, exogenous, cluster=NULL, method=NULL, sample_incl=NULL, index=NULL, distribution="normal", weights=NULL,
# print_stargazer="yes", vif="yes", update=NULL) {
#   start_time_all <- Sys.time()
#   # cluster = 
#   # method        = IV - 2SLS - 2SLSP
#   # sample_incl   = Used for retaining the same sample. Excludes observations which have NA for sample_incl without having them in the regression
#   # index         = for creating pdata.frame if method is 2SLSP
#   # distribution  = normal or tobit
#   # update
#   #install.packages('stargazer', dependencies = TRUE)
#   #install.packages('ivmodel', dependencies = TRUE)
#   #install.packages('AER', dependencies = TRUE)
#   #install.packages('foreign', dependencies = TRUE)
#   # library(stargazer)
#   # library(ivmodel)
#   # library(AER)
#   # library(foreign)
#   # library(car)
#   formula <- function(depvar, exogenous=NULL, instruments=NULL, instrumentedvar=NULL, othervars=NULL, sample_incl=NULL, type=NULL) {
#     if (is.null(type) || type == "lm") {
#         exogenous <- unique(exogenous)                          # Checks if there are no doubles in the independent variables
#         exogenous <- exogenous[!exogenous %in% depvar]          # Checks if the dependent variable is not present in exogenous
#         exogenous <- exogenous[!exogenous %in% instruments]     # Checks if the instrument is not present in exogenous
#         # print("form = depvar ~ exogenous")
#         rhs <- c(as.character(instrumentedvar), as.character(exogenous))
#         formula <- as.formula(paste(depvar, paste(rhs, collapse = " + "), sep = " ~ "))
#     } else if (type=="all") {
#         rhs <- c(as.character(instrumentedvar), as.character(instruments), as.character(exogenous), as.character(sample_incl))
#         formula <- as.formula(paste(depvar, paste(rhs, collapse = " + "), sep = " ~ "))
#     } else if (type=="1st") {
#         exogenous <- unique(exogenous)                          # Checks if there are no doubles in the independent variables
#         exogenous <- exogenous[!exogenous %in% depvar]          # Checks if the dependent variable is not present in exogenous
#         exogenous <- exogenous[!exogenous %in% instruments]     # Checks if the instrument is not present in exogenous
#         # print("form = instrumentedvar ~ exogenous + instruments")
#         rhs <- c(as.character(instruments), as.character(exogenous))
#         rhs <- rhs[!rhs %in% instrumentedvar]
#         formula <- as.formula(paste(instrumentedvar, paste(rhs, collapse = " + "), sep = " ~ "))
#     } else if (type=="2nd") {
#         # print("form = depvar ~ exogenous + instrumentedvar | exogenous + instruments")
#         exogenous <- unique(exogenous)                          # Checks if there are no doubles in the independent variables
#         exogenous <- exogenous[!exogenous %in% depvar]          # Checks if the dependent variable is not present in exogenous
#         exogenous <- exogenous[!exogenous %in% instruments]     # Checks if the instrument is not present in exogenous
#         rhsA <- c(as.character(instrumentedvar), as.character(exogenous)) # Instrumented var only necessary if not specified in independentvars
#         rhsA <- rhsA[!rhsA %in% instruments]
#         rhsB <- c(as.character(instruments), as.character(exogenous))
#         rhsB <- rhsB[!rhsB %in% instrumentedvar]
#         formula <- as.formula(
#       paste(depvar, paste(paste(rhsA, collapse = " + "), paste("|", paste(rhsB, collapse=" + " ))), sep= " ~ "))
#     } else if (type=="haus_end_2nd") {
#         exogenous <- unique(exogenous)                          # Checks if there are no doubles in the independent variables
#         exogenous <- exogenous[!exogenous %in% depvar]          # Checks if the dependent variable is not present in exogenous
#         exogenous <- exogenous[!exogenous %in% instruments]     # Checks if the instrument is not present in exogenous
#         # print("form = depvar ~ exogenous + resid")
#         rhs <- c(as.character(othervars), as.character(exogenous))
#         formula <- as.formula(paste(depvar, paste(rhs, collapse = " + "), sep = " ~ "))
#     } else if (type=="haus_over_2nd") {
#         exogenous <- unique(exogenous)                          # Checks if there are no doubles in the independent variables
#         exogenous <- exogenous[!exogenous %in% depvar]          # Checks if the dependent variable is not present in exogenous
#         exogenous <- exogenous[!exogenous %in% instruments]     # Checks if the instrument is not present in exogenous
#         # print("form = depvar ~ exogenous + instruments")
#         rhs <- c(as.character(exogenouss), as.character(instruments))
#         rhs <- rhs[!rhs %in% instrumentedvar]
#         formula <- as.formula(paste(depvar, paste(rhs, collapse = " + "), sep = " ~ "))
#   } 
#   return(formula)
#   }
#   # Dealing with instrumented interaction terms
#   placeholder <- instrumentedvar
#   insvar <- formula(depvar=depvar, instrumentedvar=instrumentedvar, type="all")
#   insvar <- all.vars(insvar)
#   insvar <- insvar[-1]
#   if (length(insvar)==2) {
#     dataset <- as.data.frame(dataset)
#     dataset$insvar <- dataset[insvar[1]]*dataset[insvar[2]]
#   } else if (length(insvar)==3) {
#     dataset <- as.data.frame(dataset)
#     dataset$insvar <- dataset[insvar[1]]*dataset[insvar[2]]
#     dataset$insvarB <- dataset[insvar[1]]*dataset[insvar[3]]
#   }
#   # Step 1 = Subset rows with NA's
#   form_all <- formula(depvar=depvar, exogenous=exogenous, instrumentedvar=instrumentedvar, instruments=instruments, sample_incl=sample_incl, type="all")
#   sub_set_vars <<- all.vars(form_all)
#   sub_set <- dataset %>% filter_at(vars(sub_set_vars), all_vars(!is.na(.))) %>% as.data.table(.)
#   ivmodel_df <- dataset %>% filter_at(vars(sub_set_vars), all_vars(!is.na(.))) %>% as.data.table(.)
#   # Create pdata.frame if method is 2SLSP
#   if (method == "2SLSP") {
#     sub_set <- pdata.frame(sub_set, index=index)
#   }
#   ### OLS ###
#   form_lm <<- formula(depvar=depvar, exogenous=exogenous, instrumentedvar=instrumentedvar) 
#   # This one gets overwritten for 2SLSP
#   sub_set_lm <<- lm(form_lm, data=sub_set, weights=weights)
#   y <<- sub_set
#   start_time_tobit <- Sys.time()
#   if (distribution=="tobit") {
#   tobit_reg <<- censReg(form_lm, left=0, right=100, data=sub_set)
#   }
#   end_time_tobit <- Sys.time()
#   ### IV ###
#   if (method=="IV") {
#     # Step 2 = stage1 <- lm(x_biased_var ~ x2 + z1 + z2, data=sample)
#     # Step 3 = stage2 <- ivreg(y ~ x_biased_var + x2 | z1 + z2 + x2, data=sample)    # ivreg(y~ax+b | az + b)
#     ###############################################################################################################################################
#     # Step 2 = stage1 <- lm(x_biased_var ~ x2 + z1 + z2, data=sample)                # Predicting educ, income, social, cultural
#     if (length(insvar)>1) {
#       instrumentedvar <- "insvar"
#     }
#     form_1st <- formula(depvar=instrumentedvar, instrumentedvar=instrumentedvar, exogenous=exogenous, instruments=instruments, type="1st")
#     # First Instance
#     sub_set_stage1 <- lm(form_1st, data=sub_set, weights=weights)
#     if (length(insvar)>1) {
#       instrumentedvar <- placeholder
#     }
#     ###############################################################################################################################################
#     # Step 3 = stage2 <- ivreg(y ~ x_biased_var + x2 | z1 + z2 + x2, data=sample)    # ivreg(y~ax+b | az + b)
#     form_2st <- formula(depvar=depvar, exogenous=exogenous, instrumentedvar=instrumentedvar, instruments=instruments, type="2nd")
#     sub_set_stage2 <<- ivreg(form_2st, data=sub_set, weights=weights)
#     ###############################################################################################################################################
#   } else if (method == "2SLS") {
#     # Step 2 = stage1 <- lm(x_biased_var ~ x2 + z1 + z2, data=sample)
#     # Step 3 = stage2 <- ivreg(y ~ x_biased_var + x2 | z1 + z2 + x2, data=sample)    # ivreg(y~ax+b | az + b)
#     ###############################################################################################################################################
#     # Step 2 = stage1 <- lm(x_biased_var ~ x2 + z1 + z2, data=sample)                # Predicting educ, income, social, cultural
#     if (length(insvar)>1) {
#       instrumentedvar <- "insvar"
#     }
#     form_1st <- formula(depvar=instrumentedvar, instrumentedvar=instrumentedvar, exogenous=exogenous, instruments=instruments, type="1st")
#     # Second Instance
#     sub_set_stage1 <- lm(form_1st, data=sub_set, weights=weights)
#     if (length(insvar)>1) {
#       instrumentedvar <- placeholder
#     }
#     # This one will not be printed in Stargazer
#     if (length(insvar)==3){
#       instrumentedvar <- "insvarB"
#       form_1st_B <- formula(depvar=instrumentedvar, instrumentedvar=instrumentedvar, exogenous=exogenous, instruments=instruments, type="1st")
#       sub_set_stage1_B <<- lm(form_1st_B, data=sub_set, weights=weights)
#       instrumentedvar <- placeholder
#     }
#     ###############################################################################################################################################
#     # Step 3 = stage2 <- ivreg(y ~ x_biased_var + x2 | z1 + z2 + x2, data=sample)    # ivreg(y~ax+b | az + b)
#     form_2st <- formula(depvar=depvar, exogenous=exogenous, instrumentedvar=instrumentedvar, instruments=instruments, type="2nd")
#     sub_set_stage2 <<- ivreg(form_2st, data=sub_set, weights=weights)
#     ###############################################################################################################################################
#   } else if (method == "2SLSP") {
#       # This one overwrites the OLS !
#       sub_set_lm <<- plm(form_lm, data=sub_set, weights=weights, model="fd")
#       # sub_set$insvar <- sub_set[,insvar[1]] + sub_set[,insvar [2]] + sub_set[,insvar [3]]
#       # Step 2 = stage1 <- lm(x_biased_var ~ x2 + z1 + z2, data=sample)
#       # Step 3 = stage2 <- ivreg(y ~ x_biased_var + x2 | z1 + z2 + x2, data=sample)    # ivreg(y~ax+b | az + b)
#       ###############################################################################################################################################
#       # Step 2 = stage1 <- lm(x_biased_var ~ x2 + z1 + z2, data=sample)                # Predicting educ, income, social, cultural
#       if (length(insvar)>1) {
#         instrumentedvar <- "insvar"
#       }
#       form_1st <- formula(depvar=instrumentedvar, instrumentedvar=instrumentedvar, exogenous=exogenous, instruments=instruments, type="1st")
#       # This one overwrites the OLS !
#       sub_set_stage1 <<- plm(form_1st, data=sub_set, weights=weights, model="fd")
#       print(summary(sub_set_stage1))
#       if (length(insvar)>1) {
#       instrumentedvar <- placeholder
#       }
#       # This one will not be printed in Stargazer
#       if (length(insvar)==3){
#         instrumentedvar <- "insvarB"
#         form_1st_B <- formula(depvar=instrumentedvar, instrumentedvar=instrumentedvar, exogenous=exogenous, instruments=instruments, type="1st")
#         sub_set_stage1_B <<- lm(form_1st_B, data=sub_set, weights=weights)
#         instrumentedvar <- placeholder
#       }
#       ###############################################################################################################################################
#       # Step 3 = stage2 <- ivreg(y ~ x_biased_var + x2 | z1 + z2 + x2, data=sample)    # ivreg(y~ax+b | az + b)
#       form_2st <- formula(depvar=depvar, exogenous=exogenous, instrumentedvar=instrumentedvar, instruments=instruments, type="2nd")
#       # print (form_2st)
#       sub_set_stage2 <<- plm(form_2st, data=sub_set, weights=weights, model="fd")
#       ###############################################################################################################################################
#   }


# # PRINT STATEMENT
# print("############################################################")
# print("***OLS***")
# print(form_lm)
# print("############################################################")
# print(summary(sub_set_lm))
# if (distribution=="tobit") {
#   print("Hello")
#   print(summary(tobit_reg))
# }
# if (vif=="yes") {
#   print("https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif")
#   print(vif(sub_set_lm))
# }
# print("***Number of observations (nobs)***")
# print(nobs(sub_set_lm))
# print("############################################################")
# print("***IV -First Stage***")
# print(form_1st)
# print("############################################################")
# print(summary(sub_set_stage1))
# if (vif=="yes") {
#   print(vif(sub_set_stage1))
# }
# print("***Number of observations (nobs)***")
# print(nobs(sub_set_stage1))
# print("############################################################")
# print("***IV -Second Stage***")
# print(form_2st)
# print("############################################################")
# print(summary(sub_set_stage2))
# if (vif=="yes") {
#   print(vif(sub_set_stage2))
# }
# print("***Number of observations (nobs)***")
# print(nobs(sub_set_stage2))
# print("############################################################")
# ###############################################################################################################################################
# # Hausman test for endogeneity -> Coefficient on the residuals should be zero
# # Step 4 = depvar ~ exogenous + resid(resid_1)
# hausman_stage1 <- lm(form_1st, data=sub_set, weights=weights)
# sub_set <- sub_set %>% add_residuals(hausman_stage1, var="resid_1")
# form_lmhausman_end_form <<- formula(depvar=depvar, exogenous=exogenous, instruments=NULL, othervars="resid_1", type="haus_end_2nd")
# print("***Hausman Test for Endogeneity***")
# print(form_lmhausman_end_form)
# print("############################################################")
# form_lmhausman_end <<- lm(form_lmhausman_end_form, data=sub_set, weights=weights)
# print(summary(form_lmhausman_end))
# print("***Number of observations (nobs)***")
# print(nobs(form_lmhausman_end))
# print("############################################################")
# if (method=="2SLS" | method=="2SLSP") {
#       ###############################################################################################################################################
#       # Hausman test for overidentification -> Only valid if at least one of the instruments is valid
#       # Step 5 = resid(sub_set_stage2) ~ exogenous + instruments
#       sub_set_stage2 <- ivreg(form_2st, data=sub_set, weights=weights)
#       sub_set <- sub_set %>% add_residuals(sub_set_stage2, var="resid_2")
#       form_lmhausman_over_form <- formula(depvar="resid(sub_set_stage2)", exogenous=exogenous, instrumentedvar=instrumentedvar, instruments=instruments, type="1st")
#       form_lmhausman_over_form <- formula(depvar="resid_2", exogenous=exogenous, instrumentedvar=instrumentedvar, instruments=instruments, type="haus_over_2nd")
#       print("***Hausman Test for Overidentification***")
#       print(form_lmhausman_over_form)
#       print("############################################################")
#       form_lmhausman_over <- lm(form_lmhausman_over_form, data=sub_set)
#       print("***R2***")
#       print((r2 <- summary(form_lmhausman_over)$r.squared))
#       print("***Number of observations (nobs)***")
#       print((n <- nobs(form_lmhausman_over)))
#       print("***nobs*R2***")
#       print((teststat <- n*r2))
#       print("***p-value***")
#       print((pval <- 1-pchisq(teststat, 1))) # q=1, is overidentification
#       print("############################################################")
# }
# if (print_stargazer=="yes") {
#     stargazer(sub_set_lm, sub_set_stage1, sub_set_stage2, form_lmhausman_end, sub_set_lm, type="text", keep.stat=c("n", "rsq"), column.labels=c("OLS","1st Stage", "2nd Stage", "Hausman Endogeneity", "2SLS Tobit"),dep.var.labels = "Hours") 
# }
# # to deal with interaction terms
# print(instruments)
# form_inst <-  formula(depvar=depvar, instruments=instruments, type="all")
# instruments <- all.vars(form_inst)
# instruments <- instruments[!instruments %in% depvar_plain]          # Checks if the dependent variable is not present in instruments
# print(instruments)
# Z <- sub_set[,c("in25", "loon_th", "mtr_loon", "midlife45_50")]
# model2IV <<- ivmodelFormula(form_2st, data=sub_set, na.action = na.omit)
# print(model2IV)
# print("")
# print("***Dependent Variable***")
# print(depvar)
# print("")
# print("***Instrumented Variable***")
# print(instrumentedvar)
# print("")
# print("***Instruments***")
# print(instruments)
# print("")
# print("***Exogenous Variables***")
# print(exogenous)
# model2IV <<- ivmodel(Y=Y,D=D,Z=Z,X=X, na.action = na.omit) 
# print(model2IV)
# end_time_all <- Sys.time()
# print("tobit_time:")
# print(end_time_tobit-start_time_tobit)
# print("total_time:")
# print(end_time_all - start_time_all)
# }


# save.session(file="Data_Merged.RSession")

# https://stackoverflow.com/questions/31450603/change-levels-from-a-variable
# We can try with Map to loop over the corresponding columns of both datasets ('d1' and 'd2') and change the levels of each column with the corresponding column level of the second dataset.

# d1[] <- Map(function(x,y) {factor(x, levels=levels(y))}, d1, d2[names(d1)])
# Or an option using lapply will be loop over the column names

# d1[] <- lapply(names(d1), function(x) {factor(x, levels=levels(d2[,x]))
#                                       })

# setwd("C:/Users/tomki/Documents/Dropbox/PhD/Proposal/Data/2) Working Data/R Code/WVS_US_Elec")

# save.session(file="Lib.RSession")


# restore.session(file="Data_Merge.RData")
# rm(list = setdiff(ls(), c("WVSpanelpdf", "wvspanel_d_overview", "WVS", "ES1", "es1_overview", lsf.str()))) 
# restore.session(file="Lib.RSession")
# save.session(file="Analysis.RSession")
#################################################################################################################################################################











# if ( test_expression1) {
# statement1
# } else if ( test_expression2) {
# statement2
# } else if ( test_expression3) {
# statement3
# } else {
# statement4
# }



  # save.session(file="Data_Merged.RSession")

  # https://stackoverflow.com/questions/31450603/change-levels-from-a-variable
  # We can try with Map to loop over the corresponding columns of both datasets ('d1' and 'd2') and change the levels of each column with the corresponding column level of the second dataset.

  # d1[] <- Map(function(x,y) {factor(x, levels=levels(y))}, d1, d2[names(d1)])
  # Or an option using lapply will be loop over the column names

  # d1[] <- lapply(names(d1), function(x) {factor(x, levels=levels(d2[,x]))
  #                                       })

  # setwd("C:/Rsaves/Data Files/")
  # save.session(file="Lib.RSession")


  # restore.session(file="Data_Merge.RData")
  # rm(list = setdiff(ls(), c("WVSpanelpdf", "wvspanel_d_overview", "WVS", "ES1", "es1_overview", lsf.str()))) 
  # restore.session(file="Lib.RSession")
  # save.session(file="Analysis.RSession")
  #################################################################################################################################################################











  # if ( test_expression1) {
  # statement1
  # } else if ( test_expression2) {
  # statement2
  # } else if ( test_expression3) {
  # statement3
  # } else {
  # statement4
  # }

