formula <- function(depvar, exogenous=NULL, instruments=NULL, instrumentedvar=NULL, othervars=NULL, type=NULL) {
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

IVsubset_1 <- function (dataset, depvar, instrumentedvar, instruments, exogenous, cluster=NULL, method=NULL) {
  # Step 1 = Subset rows with NA's
  # sub_set <- dataset %>% filter_at(vars(exogenous, depvar, instrumentedvar, instruments), all_vars(!is.na(.))) %>% as.data.table(.)
  sub_set <- dataset
  sub_set <- dplyr::select(sub_set, depvar, instrumentedvar, instruments, exogenous)
  #write.dta(sub_set, "C:/Rsaves/Data Files/ES1_ivsub.dta")
  write.dta(sub_set, "C:/Users/tkisters/Dropbox/PhD/Proposal/Data/2) Working Data/Data Files/ES1_ivsub.dta")
}

IVsubset_2 <- function (dataset, depvar, instrumentedvar, instruments, exogenous, cluster=NULL, method=NULL) {
  # Step 1 = Subset rows with NA's
  # sub_set <- dataset %>% filter_at(vars(exogenous, depvar, instrumentedvar, instruments), all_vars(!is.na(.))) %>% as.data.table(.)
  sub_set <- dataset
  sub_set <- dplyr::select(sub_set, depvar, instrumentedvar, instruments, exogenous)
  #write.dta(sub_set, "C:/Rsaves/Data Files/ES1_ivsub_p.dta")
  write.dta(sub_set, "C:/Users/tkisters/Dropbox/PhD/Proposal/Data/2) Working Data/Data Files/ES1_ivsub_p.dta")
}

IVsubset_3 <- function (dataset, depvar, instrumentedvar, instruments, exogenous, cluster=NULL, method=NULL) {
  # Step 1 = Subset rows with NA's
  # sub_set <- dataset %>% filter_at(vars(exogenous, depvar, instrumentedvar, instruments), all_vars(!is.na(.))) %>% as.data.table(.)
  sub_set <- dataset
  sub_set <- dplyr::select(sub_set, depvar, instrumentedvar, instruments, exogenous)
  #write.dta(sub_set, "C:/Rsaves/Data Files/ES1_ivsub_sub.dta")
  write.dta(sub_set, "C:/Users/tkisters/Dropbox/PhD/Proposal/Data/2) Working Data/Data Files/ES1_ivsub_sub.dta")
}

IVfun <- function (dataset, depvar, instrumentedvar, instruments, exogenous, cluster=NULL, method=NULL, sample_incl=NULL, index=NULL, distribution="normal", weights=NULL,
print_stargazer="yes", vif="yes", update=NULL) {
  start_time_all <- Sys.time()
  # cluster = 
  # method        = IV - 2SLS - 2SLSP
  # sample_incl   = Used for retaining the same sample. Excludes observations which have NA for sample_incl without having them in the regression
  # index         = for creating pdata.frame if method is 2SLSP
  # distribution  = normal or tobit
  # update
  #install.packages('stargazer', dependencies = TRUE)
  #install.packages('ivmodel', dependencies = TRUE)
  #install.packages('AER', dependencies = TRUE)
  #install.packages('foreign', dependencies = TRUE)
  # library(stargazer)
  # library(ivmodel)
  # library(AER)
  # library(foreign)
  # library(car)
  formula <- function(depvar, exogenous=NULL, instruments=NULL, instrumentedvar=NULL, othervars=NULL, sample_incl=NULL, type=NULL) {
    if (is.null(type) || type == "lm") {
        exogenous <- unique(exogenous)                          # Checks if there are no doubles in the independent variables
        exogenous <- exogenous[!exogenous %in% depvar]          # Checks if the dependent variable is not present in exogenous
        exogenous <- exogenous[!exogenous %in% instruments]     # Checks if the instrument is not present in exogenous
        # print("form = depvar ~ exogenous")
        rhs <- c(as.character(instrumentedvar), as.character(exogenous))
        formula <- as.formula(paste(depvar, paste(rhs, collapse = " + "), sep = " ~ "))
    } else if (type=="all") {
        rhs <- c(as.character(instrumentedvar), as.character(instruments), as.character(exogenous), as.character(sample_incl))
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
  # Dealing with instrumented interaction terms
  placeholder <- instrumentedvar
  insvar <- formula(depvar=depvar, instrumentedvar=instrumentedvar, type="all")
  insvar <- all.vars(insvar)
  insvar <- insvar[-1]
  if (length(insvar)==2) {
    dataset <- as.data.frame(dataset)
    dataset$insvar <- dataset[insvar[1]]*dataset[insvar[2]]
  } else if (length(insvar)==3) {
    dataset <- as.data.frame(dataset)
    dataset$insvar <- dataset[insvar[1]]*dataset[insvar[2]]
    dataset$insvarB <- dataset[insvar[1]]*dataset[insvar[3]]
  }
  # Step 1 = Subset rows with NA's
  form_all <- formula(depvar=depvar, exogenous=exogenous, instrumentedvar=instrumentedvar, instruments=instruments, sample_incl=sample_incl, type="all")
  sub_set_vars <<- all.vars(form_all)
  sub_set <- dataset %>% filter_at(vars(sub_set_vars), all_vars(!is.na(.))) %>% as.data.table(.)
  ivmodel_df <- dataset %>% filter_at(vars(sub_set_vars), all_vars(!is.na(.))) %>% as.data.table(.)
  # Create pdata.frame if method is 2SLSP
  if (method == "2SLSP") {
    sub_set <- pdata.frame(sub_set, index=index)
  }
  ### OLS ###
  form_lm <<- formula(depvar=depvar, exogenous=exogenous, instrumentedvar=instrumentedvar) 
  # This one gets overwritten for 2SLSP
  sub_set_lm <<- lm(form_lm, data=sub_set, weights=weights)
  y <<- sub_set
  start_time_tobit <- Sys.time()
  if (distribution=="tobit") {
  tobit_reg <<- censReg(form_lm, left=0, right=100, data=sub_set)
  }
  end_time_tobit <- Sys.time()
  ### IV ###
  if (method=="IV") {
    # Step 2 = stage1 <- lm(x_biased_var ~ x2 + z1 + z2, data=sample)
    # Step 3 = stage2 <- ivreg(y ~ x_biased_var + x2 | z1 + z2 + x2, data=sample)    # ivreg(y~ax+b | az + b)
    ###############################################################################################################################################
    # Step 2 = stage1 <- lm(x_biased_var ~ x2 + z1 + z2, data=sample)                # Predicting educ, income, social, cultural
    if (length(insvar)>1) {
      instrumentedvar <- "insvar"
    }
    form_1st <- formula(depvar=instrumentedvar, instrumentedvar=instrumentedvar, exogenous=exogenous, instruments=instruments, type="1st")
    # First Instance
    sub_set_stage1 <- lm(form_1st, data=sub_set, weights=weights)
    if (length(insvar)>1) {
      instrumentedvar <- placeholder
    }
    ###############################################################################################################################################
    # Step 3 = stage2 <- ivreg(y ~ x_biased_var + x2 | z1 + z2 + x2, data=sample)    # ivreg(y~ax+b | az + b)
    form_2st <- formula(depvar=depvar, exogenous=exogenous, instrumentedvar=instrumentedvar, instruments=instruments, type="2nd")
    sub_set_stage2 <<- ivreg(form_2st, data=sub_set, weights=weights)
    ###############################################################################################################################################
  } else if (method == "2SLS") {
    # Step 2 = stage1 <- lm(x_biased_var ~ x2 + z1 + z2, data=sample)
    # Step 3 = stage2 <- ivreg(y ~ x_biased_var + x2 | z1 + z2 + x2, data=sample)    # ivreg(y~ax+b | az + b)
    ###############################################################################################################################################
    # Step 2 = stage1 <- lm(x_biased_var ~ x2 + z1 + z2, data=sample)                # Predicting educ, income, social, cultural
    if (length(insvar)>1) {
      instrumentedvar <- "insvar"
    }
    form_1st <- formula(depvar=instrumentedvar, instrumentedvar=instrumentedvar, exogenous=exogenous, instruments=instruments, type="1st")
    # Second Instance
    sub_set_stage1 <- lm(form_1st, data=sub_set, weights=weights)
    if (length(insvar)>1) {
      instrumentedvar <- placeholder
    }
    # This one will not be printed in Stargazer
    if (length(insvar)==3){
      instrumentedvar <- "insvarB"
      form_1st_B <- formula(depvar=instrumentedvar, instrumentedvar=instrumentedvar, exogenous=exogenous, instruments=instruments, type="1st")
      sub_set_stage1_B <<- lm(form_1st_B, data=sub_set, weights=weights)
      instrumentedvar <- placeholder
    }
    ###############################################################################################################################################
    # Step 3 = stage2 <- ivreg(y ~ x_biased_var + x2 | z1 + z2 + x2, data=sample)    # ivreg(y~ax+b | az + b)
    form_2st <- formula(depvar=depvar, exogenous=exogenous, instrumentedvar=instrumentedvar, instruments=instruments, type="2nd")
    sub_set_stage2 <<- ivreg(form_2st, data=sub_set, weights=weights)
    ###############################################################################################################################################
  } else if (method == "2SLSP") {
      # This one overwrites the OLS !
      sub_set_lm <<- plm(form_lm, data=sub_set, weights=weights, model="fd")
      # sub_set$insvar <- sub_set[,insvar[1]] + sub_set[,insvar [2]] + sub_set[,insvar [3]]
      # Step 2 = stage1 <- lm(x_biased_var ~ x2 + z1 + z2, data=sample)
      # Step 3 = stage2 <- ivreg(y ~ x_biased_var + x2 | z1 + z2 + x2, data=sample)    # ivreg(y~ax+b | az + b)
      ###############################################################################################################################################
      # Step 2 = stage1 <- lm(x_biased_var ~ x2 + z1 + z2, data=sample)                # Predicting educ, income, social, cultural
      if (length(insvar)>1) {
        instrumentedvar <- "insvar"
      }
      form_1st <- formula(depvar=instrumentedvar, instrumentedvar=instrumentedvar, exogenous=exogenous, instruments=instruments, type="1st")
      # This one overwrites the OLS !
      sub_set_stage1 <<- plm(form_1st, data=sub_set, weights=weights, model="fd")
      print(summary(sub_set_stage1))
      if (length(insvar)>1) {
      instrumentedvar <- placeholder
      }
      # This one will not be printed in Stargazer
      if (length(insvar)==3){
        instrumentedvar <- "insvarB"
        form_1st_B <- formula(depvar=instrumentedvar, instrumentedvar=instrumentedvar, exogenous=exogenous, instruments=instruments, type="1st")
        sub_set_stage1_B <<- lm(form_1st_B, data=sub_set, weights=weights)
        instrumentedvar <- placeholder
      }
      ###############################################################################################################################################
      # Step 3 = stage2 <- ivreg(y ~ x_biased_var + x2 | z1 + z2 + x2, data=sample)    # ivreg(y~ax+b | az + b)
      form_2st <- formula(depvar=depvar, exogenous=exogenous, instrumentedvar=instrumentedvar, instruments=instruments, type="2nd")
      # print (form_2st)
      sub_set_stage2 <<- plm(form_2st, data=sub_set, weights=weights, model="fd")
      ###############################################################################################################################################
  }


# PRINT STATEMENT
print("############################################################")
print("***OLS***")
print(form_lm)
print("############################################################")
print(summary(sub_set_lm))
if (distribution=="tobit") {
  print("Hello")
  print(summary(tobit_reg))
}
if (vif=="yes") {
  print("https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif")
  print(vif(sub_set_lm))
}
print("***Number of observations (nobs)***")
print(nobs(sub_set_lm))
print("############################################################")
print("***IV -First Stage***")
print(form_1st)
print("############################################################")
print(summary(sub_set_stage1))
if (vif=="yes") {
  print(vif(sub_set_stage1))
}
print("***Number of observations (nobs)***")
print(nobs(sub_set_stage1))
print("############################################################")
print("***IV -Second Stage***")
print(form_2st)
print("############################################################")
print(summary(sub_set_stage2))
if (vif=="yes") {
  print(vif(sub_set_stage2))
}
print("***Number of observations (nobs)***")
print(nobs(sub_set_stage2))
print("############################################################")
###############################################################################################################################################
# Hausman test for endogeneity -> Coefficient on the residuals should be zero
# Step 4 = depvar ~ exogenous + resid(resid_1)
hausman_stage1 <- lm(form_1st, data=sub_set, weights=weights)
sub_set <- sub_set %>% add_residuals(hausman_stage1, var="resid_1")
form_lmhausman_end_form <<- formula(depvar=depvar, exogenous=exogenous, instruments=NULL, othervars="resid_1", type="haus_end_2nd")
print("***Hausman Test for Endogeneity***")
print(form_lmhausman_end_form)
print("############################################################")
form_lmhausman_end <<- lm(form_lmhausman_end_form, data=sub_set, weights=weights)
print(summary(form_lmhausman_end))
print("***Number of observations (nobs)***")
print(nobs(form_lmhausman_end))
print("############################################################")
if (method=="2SLS" | method=="2SLSP") {
      ###############################################################################################################################################
      # Hausman test for overidentification -> Only valid if at least one of the instruments is valid
      # Step 5 = resid(sub_set_stage2) ~ exogenous + instruments
      sub_set_stage2 <- ivreg(form_2st, data=sub_set, weights=weights)
      sub_set <- sub_set %>% add_residuals(sub_set_stage2, var="resid_2")
      form_lmhausman_over_form <- formula(depvar="resid(sub_set_stage2)", exogenous=exogenous, instrumentedvar=instrumentedvar, instruments=instruments, type="1st")
      form_lmhausman_over_form <- formula(depvar="resid_2", exogenous=exogenous, instrumentedvar=instrumentedvar, instruments=instruments, type="haus_over_2nd")
      print("***Hausman Test for Overidentification***")
      print(form_lmhausman_over_form)
      print("############################################################")
      form_lmhausman_over <- lm(form_lmhausman_over_form, data=sub_set)
      print("***R2***")
      print((r2 <- summary(form_lmhausman_over)$r.squared))
      print("***Number of observations (nobs)***")
      print((n <- nobs(form_lmhausman_over)))
      print("***nobs*R2***")
      print((teststat <- n*r2))
      print("***p-value***")
      print((pval <- 1-pchisq(teststat, 1))) # q=1, is overidentification
      print("############################################################")
}
if (print_stargazer=="yes") {
    stargazer(sub_set_lm, sub_set_stage1, sub_set_stage2, form_lmhausman_end, sub_set_lm, type="text", keep.stat=c("n", "rsq"), column.labels=c("OLS","1st Stage", "2nd Stage", "Hausman Endogeneity", "2SLS Tobit"),dep.var.labels = "Hours") 
}
# to deal with interaction terms
print(instruments)
form_inst <-  formula(depvar=depvar, instruments=instruments, type="all")
instruments <- all.vars(form_inst)
instruments <- instruments[!instruments %in% depvar_plain]          # Checks if the dependent variable is not present in instruments
print(instruments)
Z <- sub_set[,c("in25", "loon_th", "mtr_loon", "midlife45_50")]
model2IV <<- ivmodelFormula(form_2st, data=sub_set, na.action = na.omit)
print(model2IV)
print("")
print("***Dependent Variable***")
print(depvar)
print("")
print("***Instrumented Variable***")
print(instrumentedvar)
print("")
print("***Instruments***")
print(instruments)
print("")
print("***Exogenous Variables***")
print(exogenous)
model2IV <<- ivmodel(Y=Y,D=D,Z=Z,X=X, na.action = na.omit) 
print(model2IV)
end_time_all <- Sys.time()
print("tobit_time:")
print(end_time_tobit-start_time_tobit)
print("total_time:")
print(end_time_all - start_time_all)
}

