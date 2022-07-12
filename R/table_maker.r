table_maker <- function(DAT, year=2020, category_type = NULL, LBT_data=NULL, strata = "FADN_2018", mark_low_vals = TRUE, calculate="none", BIN = TRUE, year_of_interest_for_approaching=2024, required_benefit_as_fraction=0.05) {
 
    # DAT                 = The name of main dataset. This is usually BIN data.

    # year                = Default is 2020             - The year of the data, or NULL for all data. For now the BIN and LBT year are the same.

    # LBT_data            = Data to replace empty BIN cells with or calculate fractions. This is usually LBT data.    

    # strata              = OPTIONS:
    #                       strata == "calculate"       -  Optimises strata based on year(s) selected
    #                       strata == "all"             -  Uses all SO boundaries as strata for each type
    #                       strata == "FADN_2018"       -  Uses the strata from the 2018 FADN Report
    #                       strata == NULL              -  Uses the strata from the 2018 FADN Report
    #                       Alternatively, a list with custom strata could be used (in the form of strata_in below)  

    # mark_low_vals       = TRUE/FALSE                  -  Whether to mark cells with few observations with colours    

    # calculate           = OPTIONS:
    #                       calculate == "fractions"                 -  Calculates Sampling fractions according to the Census by stratum
    #                       calculate == "variance"                  -  Uses all SO boundaries as strata for each type
    #                       calculate == "relative_svc_deviation"    -  Uses the strata from the 2018 FADN Report

    # BIN                 = TRUE/FALSE                  -  Whether DAT is BIN data

    # year_of_interest_for_approaching  = Default is 2024            - The year that you want to approach new firms for BIN

    ### List of packages 
    packages <- c(
        "Rcpp",
        "officer",
        "devtools",
        "readxl",
        "writexl",
        "fuzzyjoin", 
        "htmltools", 
        "purrr", 
        "flextable", 
        "stringr",
        "stringi",  
        "magrittr",
        "Hmisc", 
        "tidyr",
        "dplyr",
        "data.table"
    )

    # This function only installs packages if they are not already installed and installs all libraries.
    x <- lapply(packages, function(packages) {if (!require(packages, character.only = TRUE, quietly = TRUE, warn.conflicts=FALSE)) {library(packages, character.only = T, quietly = TRUE, warn.conflicts=FALSE)}})

    ### Custom functions #################################################################################################################################################

    # Changes base R dput to deal with data.table
    dput = function(x, ...) { if(is.data.table(x)) { setattr(x, '.internal.selfref', NULL) }; base::dput(x, ...) }

    # Checks if all are NA or not
    is_all_na <- function(x)all(is.na(x))

    # Checks whether
    is.consec <- function(x1, x2, vec) {
        x1 %in% vec & x2 %in% vec & abs(diff(match(c(x1, x2), vec))) == 1L
    }

    # C++ code for strata intervals (this was a major bottleneck speed wise)
    Rcpp::cppFunction(
        "std::vector<std::string> interval(std::vector<int> &x,
                                            std::vector<std::vector<int>> &y){
            std::vector<std::string> z;
            z.reserve(x.size());
            std::transform(x.begin(), x.end(), y.begin(), std::back_inserter(z), 
            [&](int a, std::vector<int> b) {
                auto it = std::find_if(b.begin(), b.end(), [=](int w) {return a < w;});
                return it == b.end()? \"NA\":
                    '[' + std::to_string(*(it-1)) + ',' + std::to_string(*it) + ')';
            });
            return z;
        }"
    )

    # Puts columns string names in correct order
    # https://stackoverflow.com/questions/72141429/converting-column-names-so-they-can-be-put-in-an-numerical-order/72141622?noredirect=1#comment127466664_72141622
    library(magrittr)
    order_cols <- function(dat) {
        
        # look for words to order by
        s_ordered <- stringi::stri_extract_all_regex(colnames(dat), "[[:alpha:]]+") %>% 
            unlist() %>% 
            unique() %>% 
            sort()
        
        if (length(s_ordered) > 1) {
            # replace words with their alphabetical index
            cnames <- stringi::stri_replace_all_fixed(colnames(dat), s_ordered, seq_along(s_ordered), vectorise_all = FALSE)
        } else {
            cnames <- colnames(dat)
        }
        
        cnames %>% 
            stringi::stri_extract_all_regex("\\d+") %>% # extract all numbers (including the alphabetical index numbers)
            lapply(as.numeric) %>% 
            do.call(rbind, .) %>%    # bind list items to a matrix
            as.data.frame %>%        # change the matrix to a data.frame (i.e. a list)
            do.call(order, .)        # use the list for ordering
    }

    ### Settings ######################################################################################################################################

    required_benefit <- required_benefit_as_fraction

    ### Calculate Strata ####################################################################################################################################################

    # Use strata from FADN 2018 if data is BIN data (default = TRUE)
    if (isTRUE(BIN)) {
        
        # English Names and Strata if no strata provided
        strata_in <- list(
            "Starch potatoes" = c(0, 25, 100, 250, 500, 1000000),
            "Organic crops" = c(0, 25, 100, 250, 500, 1000000),
            "Other field crops (non-organic)" = c(0, 25, 100, 250, 500,1000, 1000000),
            "Vegetables under glass" = c(0, 25, 100, 500, 1000, 1500, 3000, 1000000),
            "Plants under glass" = c(0, 25, 100, 500, 1000, 1500, 3000, 1000000),
            "Flowers under glass" = c(0, 25, 100, 500, 1000, 1500, 3000, 1000000),
            "Field vegetables" = c(0, 25, 100, 500, 1000, 1000000),
            "Fruit" = c(0, 25, 100, 250, 500, 1000000),
            "Tree nursery" = c(0, 25, 100, 500, 1000, 1000000),
            "Flower bulbs" = c(0, 25, 100, 500, 1000, 1000000),
            "Other horticulture" = c(0, 25, 100, 500, 1000, 1000000),
            "Dairy (organic)" = c(0, 25, 100, 250, 500, 1000000),
            "Dairy (non-organic)" = c(0, 25, 100, 250, 500,1000, 1000000),
            "Calf fattening" = c(0, 25, 100, 500, 1000, 1000000),
            "Goats" = c(0, 25, 100, 250, 500,1000, 1000000),
            "Other grazing livestock" = c(0, 25, 50,100, 250, 500, 1000000),
            "Pig rearing" = c(0, 25, 100, 250, 500,1000, 1000000),
            "Pig fattening" = c(0, 25, 100, 250, 500,1000, 1000000),
            "Combined pig rearing and fattening" = c(0, 25, 100, 250, 500,1000, 1000000),
            "Eggs for consumption" = c(0, 25, 100, 500, 1000, 1000000),
            "Broilers" = c(0, 25, 100, 500, 1000, 1000000),
            "Other intensive livestock"= c(0, 25, 100, 500, 1000, 1000000),
            "Combined" = c(0, 25, 100, 250, 500,1000000)
        )

        # Saved the order of the NSO types in the same order they are shown in the 2018 FADN report
        strat_order <- names(strata_in)

        if (is.null(strata) || strata == "FADN_2018") {

            print("Strata of FADN 2018 are used")

        } else if (strata=="all") {

            # If strata=all all possible brackets are used for each type
            for (i in seq_along(strata_in)) {
                    strata_in[[i]] <- c(0, 25, 50, 100, 250, 500, 1000, 1500, 3000, 1000000)
                }

            print("strata = all >>> All strata instead of FADN strata will be used.")

        } else if (strata == "calculate") {
            
            # Actual calculations are done further down
            print("strata = calculate >>> All strata are optimised with respect to the variance.")

        }
    }


    if (isTRUE(BIN) && isTRUE(strata == "calculate")) {

        # The sample size as they are used in the 2018 FADN report
        # The question arises whether setting these sample size before is defendable. This might be the case if other things than SO are relevant.

        print("Please note that the samples sizes are not optimised, but taken from the 2018 FADN report")

        sample_size_by_NSOtype <- list(
                "Starch potatoes" = 30,
                "Organic crops" = 30,
                "Other field crops (non-organic)" = 150,
                "Vegetables under glass" = 130,
                "Plants under glass" = 65,
                "Flowers under glass" = 118,
                "Field vegetables" = 55,
                "Fruit" = 38,
                "Tree nursery" = 70,
                "Flower bulbs" = 37,
                "Other horticulture" = 45,
                "Dairy (organic)" = 30,
                "Dairy (non-organic)" = 300,
                "Calf fattening" = 40,
                "Goats" = 30,
                "Other grazing livestock" = 33,
                "Pig rearing" = 48,
                "Pig fattening" = 48,
                "Combined pig rearing and fattening" = 38,
                "Eggs for consumption" = 30,
                "Broilers" = 30,
                "Other intensive livestock"= 30,
                "Combined" = 75
            )

    }

    # DAT ####################################################################################################################################################

    # Select variables necessary from data
    var_subset <- c("SO","ENG_name", "jaar", "NTW", "SVC")

    # If there are is category_type, add that category type (i.e. region)
    if (!is.null(category_type)) {

        var_subset <- append(var_subset, category_type)

    }

    # Turn data into data.table format and subset variables needed
    DAT <- data.table(DAT)
    DAT <- DAT[,..var_subset]


    # Subset with year if given. When year is NULL, all years are used
    if (!is.null(year)) {
        DAT <- DAT[jaar==year]  
        print(paste0("Showing results for ", year))
    }
    
    # LBT ####################################################################################################################################################
    
    if (!is.null(LBT_data)){

        # Select variables necessary from data
        var_subset <- c("SO","ENG_name", "jaar", "year_available_after_nonresp")

        # If there are is category_type, add that category type (i.e. region)
        if (!is.null(category_type)) {
            var_subset <- append(var_subset, category_type)
        }

        # Turn data into data.table format and subset variables needed
        LBT <- data.table(LBT_data)
        LBT <- LBT[,..var_subset]

        # Subset with year if given. When year is NULL, all years are used
        if (!is.null(year)) {
            LBT <- LBT[jaar==year]  
        }

        if (!is.null(year_of_interest_for_approaching)) {

            # Include only approachable farms in dat_lbt (have not been approach yet or can be approached again)
            LBT <- setDT(LBT)[is.na(year_available_after_nonresp) | year_available_after_nonresp < year_of_interest_for_approaching,]
            print(paste0("The year in which to look for new BIN candidates is set, only firms that can be approached in ", year_of_interest_for_approaching, " are included"))

        }
    }


    if ( isTRUE(strata == "calculate") ) {

        # remove observations with SO values below 25000, these are not used to calculate the strata but will be added later

        lbt_over_25 <- LBT[SO>25]

        print("Optimal stata are being calculated")

        if ( is.null(category_type) ){

            # When there is no category type, use order of 2018 FADN report
            NSO_types <- strat_order

        } else if (!is.null(category_type)) {

            NSO_names <- strat_order
            categories_vec <- unique(DAT[, ..category_type])
            categories_vec <- unlist(as.vector(categories_vec))
            NSO_types <- paste0( paste0(rep(NSO_names, length(categories_vec)) ), paste0( rep(categories_vec, (length(NSO_names)*length(categories_vec)) )))

        }
        
        strata_in <- list()
        optimal_sample_sizes_by_SOcat <- list()
        
        print(paste0("Required variance reduction is ", required_benefit*100, "%"))
        
        for (i in 1:length(NSO_types)) {
            max_sample_size = sample_size_by_NSOtype[[i]]
            all_strata <- c(0, 25, 50, 100, 250, 500, 1000, 1500, 3000, 1000000)
            # Filter SO by NSO type
            lbt_by_cat_over_25 <- filter(lbt_over_25, ENG_name == NSO_types[i])  

            print("                                             ")
            print("                                             ")
            print(NSO_types[i])            
            print(paste0("Maximum sample size is ", max_sample_size))
            print("                                             ")
            print("                                             ")            

            strata.bh.x <- lbt_by_cat_over_25$SO

            # Local variables by NSO type
            starting_variance <- var(strata.bh.x)
            # THIS SHOULD BE CHECKED
            strata_vals <- c(25, 1000000)
            list_of_results <- list()
            list_of_variances <- list()
            benefit <- 1 # This makes sure that the while function runs the first time

            # Keeps trying to add more strata as long as the benefit is large enough
            while( benefit >= required_benefit ) {

                # Makes a list of variances for each added stratum
                for (j in seq_along(all_strata)){
                    # Adds a strata from left over strata
                    bh <- all_strata[j]
                    # Adds it to the already selected strata
                    bh <- append(bh, strata_vals)
                    bh <- unique(bh)   
                    bh <- sort(bh)
                    # print(bh)
                    list_of_results[[j]] <- strata.bh(x=strata.bh.x, bh=bh, n = max_sample_size, CV = NULL, Ls = length(bh)+1, certain = NULL,
                    takenone = 0)
                    list_of_variances[[j]] <- list_of_results[[j]]$RRMSE
                }

                ### BENEFIT ####################################################################################################################

                # Compares best new variance with old variance
                if (!exists("old_var_value")) {
                    old_var_value <- starting_variance
                }

                # This makes it choose the best choice every time
                new_var_value <- list_of_variances[which.min(replace(list_of_variances, list_of_variances<=0, NA))][[1]]

                # Calculates the benefit of the best choice
                benefit <- (old_var_value-new_var_value)/old_var_value

                ###############################################################################################################################

                # Continuous if new variance is 5% better than old variance
                if (benefit >= required_benefit) {

                    # Selects the border value belonging to the best choice and adds it to optimal set
                    strata_vals <- append(strata_vals, all_strata[which.min(replace(list_of_variances, list_of_variances<=0, NA))][1])
                    strata_vals <- unique(strata_vals)
                    strata_vals <- sort(strata_vals)

                    # If there was a benefit the new value becomes the old value
                    old_var_value <- new_var_value

                    # Remove the chosen strata from the list of options
                    all_strata = all_strata[!(all_strata %in% strata_vals)]
                    all_strata <- sort(all_strata)

                    # This saves the strata when there is still a benefit. It is no longer overwritten when there is no longer a benefit.
                    bh_star <- strata_vals

                } else {

                    list_of_results[[j]] <- strata.bh(x=strata.bh.x, bh=bh_star, n = max_sample_size, CV = NULL, Ls = length(bh_star)+1, certain = NULL,
                    takenone = 0)

                    print(list_of_results[[j]])
                    
                    rm(old_var_value)
                    strata_vals <- sort(strata_vals)
                    sample_sizes_by_SOcat <- list_of_results[[j]]$nh

                }

            }

            # When the benefit is too small put all strata into a list.
            optimal_sample_sizes_by_SOcat[[i]] <- sample_sizes_by_SOcat
            strata_in[[i]] <- strata_vals   

        }

        strata_in[] <- lapply(strata_in, function(i)unique(c(0, 25, i, 1000000)))

        # Add NSO names to list
        names(strata_in) <- NSO_types
        names(optimal_sample_sizes_by_SOcat) <- NSO_types

    }  
    
    if ( isTRUE(strata == "calculate") ) {

        # These are the optimal numbers for BIN
        optimal_sample_sizes_by_SOcat <- data.frame(matrix(optimal_sample_sizes_by_SOcat, byrow = TRUE))
        optimal_sample_sizes_by_SOcat$rn <- names(strata_in)
        names(optimal_sample_sizes_by_SOcat)[1] <- "optimal_observations"
        optimal_sample_sizes_by_SOcat <- data.table(optimal_sample_sizes_by_SOcat)

    }

    # These are the strata coinciding with the optimal numbers for BIN
    strata_df <- data.frame(matrix(strata_in, byrow = TRUE))
    strata_df$rn <- names(strata_in)
    names(strata_df)[1] <- "strata_column"
    strata_df <- data.table(strata_df)

    DAT <- data.table(DAT)
    DAT <- merge(DAT, strata_df, all.x=TRUE, by.x="ENG_name", by.y="rn", sort=TRUE)

    if ( isTRUE(strata == "calculate") ) {
        DAT <- merge(DAT, optimal_sample_sizes_by_SOcat, all.x=TRUE, by.x="ENG_name", by.y="rn", sort=TRUE)
    }

    # Strata need to be decide based on the total population including observations that cannot be approached.
    # Because of this it is possible t

    # Create intervals
    DAT[, SO_cat:=interval(SO, strata_column)]
    # Remove white spaace
    DAT$SO_cat <- gsub('\\s+', '', DAT$SO_cat)

    # Current Strata
    DAT$SO_cat <- as.character(DAT$SO_cat)
    # # Current Strata by type
    DAT$SO_cat_type <- with(DAT, paste0(SO_cat, " ",ENG_name))

    # Amount of observations available in BIN per NSO type

    print("BIN data loaded")

    # LBT
    if (!is.null(LBT_data)){
        if (!is.null(year_of_interest_for_approaching)) {
            LBT <- setDT(LBT)[is.na(year_available_after_nonresp) | year_available_after_nonresp < year_of_interest_for_approaching,]
        }
        LBT <- merge(LBT, strata_df, all.x=TRUE, by.x="ENG_name", by.y="rn", sort=TRUE)

        LBT[, SO_cat:=interval(SO, strata_column)]

        # Remove white spaace
        LBT$SO_cat <- gsub('\\s+', '', LBT$SO_cat)

        # Current Strata
        LBT$SO_cat <- as.character(LBT$SO_cat)

        # # Current Strata by type
        LBT$SO_cat_type <- with(LBT, paste0(SO_cat," " ,ENG_name))
        print("LBT data loaded")

    }


    if (!is.null(category_type)) {
        categories_vec <- unique(DAT[, ..category_type])
        categories_vec <- unlist(as.vector(categories_vec))
        if (!is.null(LBT_data)){
        # CHANGE
            LBT$SO_cat_reg <- with(LBT, paste0(SO_cat," " , get(category_type)))
            DAT$SO_cat_reg <- with(DAT, paste0(SO_cat, " ", get(category_type)))
            print("Categories = TRUE")
        }
    }

    if (calculate=="variance" && is.null(category_type)) {
        LBT[, `:=` (var_SO = round( var(SO) ,0)), by = SO_cat_type]
        print("Variance calculated")
    } else if (calculate=="variance" && !is.null(category_type)) {
        LBT[, `:=` (var_SO = round(var(SO),0)), by = SO_cat_reg]
        print("Variance calculated")        
    } else if (calculate=="relative_svc_deviation") {
        rel_deviation_by_categories <- paste0("rel_deviation_by_", category_type)
        DAT <- data.table(DAT)
        DAT[, (rel_deviation_by_categories) :=  100 * mean( (SVC - NTW) / NTW , na.rm=TRUE), by=c(category_type)]        
        if ( length(year) > 1 ) {
            rel_deviation_by_categories <- paste0("rel_deviation_", category_type, "by_year" )
            DAT[, (rel_deviation_by_categories_by_year) := 100 * mean( ( SVC - NTW ) / NTW, na.rm=TRUE), by=c(category_type, if(year){"jaar"})]
        }
        print("Relative SVC deviations calculated")
        print("Please note that SVC deviations should be calculated on all years, so 'years' should be set to NULL")
    }

    if (is.null(category_type) && calculate == "none" && !is.null(LBT_data)) {
        table_lbt_input <- table(LBT$ENG_name, LBT$SO_cat)
        print("LBT table created")
        table_bin_input <- table(DAT$ENG_name, DAT$SO_cat)
        print("BIN table created")             
    } else if (!is.null(category_type) && calculate == "none" && !is.null(LBT_data)) {
        table_lbt_input <- table(LBT$ENG_name, LBT$SO_cat_reg)
        print("LBT table created")
        table_bin_input <- table(DAT$ENG_name, DAT$SO_cat_reg)
        print("BIN table created")        
    } else if (is.null(category_type) && calculate=="variance") {
        table_lbt_input <- xtabs(var_SO~ENG_name+SO_cat, LBT)
        print("Variance table created")
    } else if (!is.null(category_type) && calculate=="variance") {
        table_lbt_input <- xtabs(var_SO~ENG_name+SO_cat_reg, LBT)
        print("Variance table created")
    } else if (is.null(category_type) && calculate=="relative_svc_deviation") {
        # xtabs(as.formula(paste(mean_val_by_var, "~", by_var)), dat)
        table_bin_input <- xtabs(as.formula(paste(rel_deviation_by_categories, "~", "ENG_name", "+", "SO_cat")), DAT)
        print("Relative SVC deviations table created")
    } else if (!is.null(category_type) && calculate=="relative_svc_deviation") {
        table_bin_input <- xtabs(as.formula(paste(rel_deviation_by_categories, "~", "ENG_name", "+", "SO_cat_reg")), DAT)
        print("Relative SVC deviations table created")
    } else {
        table_bin_input <- table(DAT$ENG_name, DAT$SO_cat)
        print("BIN table created")
    }
    
    # Row sums stored before changes are made to the table
    index_order <- match(strat_order, row.names(table_bin_input))
    table_bin_input <- table_bin_input[index_order,]

    if (is.null(category_type)) {
        row_sums_stored_from_only_bin <- rowSums(table_bin_input)
    } else if ( !is.null(category_type) ) {
        list_of_row_sums_stored_from_only_bin <- list()
        tibble_format <- as.data.frame.matrix(table_bin_input)
        out_tibble_format <<- tibble_format
        out_categories_vec <<- categories_vec
        for (i in seq_along(categories_vec)) {
            tibble_format_temp <- tibble_format %>% select(ends_with(as.vector(unlist(categories_vec[[i]]))))
            list_of_row_sums_stored_from_only_bin[[i]] <- rowSums(tibble_format_temp)            
        }
        names(list_of_row_sums_stored_from_only_bin) <- categories_vec
    }
    
    print("Input tables created")

    combination_table <- function(table_bin_input, table_lbt_input) {

        # get all names from DAT en LBT
        names_bin_table <- colnames(table_bin_input)
        names_lbt_table <- colnames(table_lbt_input)

        # add columns (with zeroes) which are not present in DAT from LBT data
        add_to_bin_names <- setdiff(names_lbt_table, names_bin_table)

        if (length(add_to_bin_names)>1) {
            add_to_bin <- table_lbt_input[,add_to_bin_names]
            add_to_bin[add_to_bin > 0] <- 0 
            BIN_II <- cbind(table_bin_input, add_to_bin)
            BIN_II <- BIN_II[,..names_lbt_table]
            print(BIN_II)
        } else if (length(add_to_bin_names)==1) {
            print("1 column added")
            add_to_bin <- table_lbt_input[,add_to_bin_names]
            # Because there is only one column:
            add_to_bin <- data.table(add_to_bin)            
            colnames(add_to_bin) <- add_to_bin_names
            add_to_bin[add_to_bin > 0] <- 0 
            BIN_II <- cbind(as.data.frame.matrix(table_bin_input), add_to_bin)
            # BIN_II <- data.table(BIN_II, keep.rownames=TRUE)
            BIN_II <- BIN_II[,names_lbt_table]
        } else {
            BIN_II <- copy(table_bin_input)
        }
        
        # Put BIN_II strata columns right order
        replacement_order <- order_cols(BIN_II)
        BIN_II <- BIN_II[,replacement_order]

        # Put BIN NSO rows right order
        index_order <- match(strat_order, row.names(BIN_II))
        BIN_II <- BIN_II[index_order,]


        # Table mutations
        if (isTRUE(strata=="calculate")) {

            # Create a table using LBT table. These values are necessarily bigger than zero because otherwise no observations could be selected.
            table_for_optimal_N <- table_lbt_input

            # Put strata columns right order
            replacement_order <- order_cols(table_for_optimal_N)
            table_for_optimal_N <- table_for_optimal_N[,replacement_order]

            # Put NSO rows right order
            index_order <- match(strat_order, row.names(table_for_optimal_N))
            table_for_optimal_N <- table_for_optimal_N[index_order,]

            # Replace values
            tab <- t(table_for_optimal_N)

            # Exclude the first row
            tab <- tab[-1,]

            # Remove zero values
            positive_nr_vec <- lapply(optimal_sample_sizes_by_SOcat$optimal_observations, function(x) {x[x!=0]})

            tab[tab > 0] <- unlist(positive_nr_vec)

            tab <- t(tab)

            tab <- cbind(table_bin_input[,1],tab)

            # Put table_bin_input strata columns right order
            replacement_order <- order_cols(table_bin_input)
            table_bin_input <- table_bin_input[,replacement_order]
            # Put table_bin_input NSO rows right order
            index_order <- match(strat_order, row.names(table_bin_input))
            table_bin_input <- table_bin_input[index_order,]
            # Put table_lbt_input strata columns right order
            replacement_order <- order_cols(table_lbt_input)
            table_lbt_input <- table_lbt_input[,replacement_order]
            # Put table_lbt_input NSO rows right order
            index_order <- match(strat_order, row.names(table_lbt_input))
            table_lbt_input <- table_lbt_input[index_order,]
            # Put tab strata columns right order
            replacement_order <- order_cols(tab)
            tab <- tab[,replacement_order]
            # Put tab NSO rows right order
            index_order <- match(strat_order, row.names(tab))
            tab <- tab[index_order,]

            # Subtract optimal observations from available BIN observations, excluding[0,25]. This calculates observations surplus.
            BIN_II <- table_bin_input[,2:ncol(BIN_II)] - tab[,2:ncol(tab)]
            # Add back [0,25]
            BIN_II <- cbind(table_bin_input[,1], BIN_II)
            colnames(BIN_II)[1] <- "[0,25)"

            # out_BIN_II has the negative values needed further down below
            out_BIN_II <<- BIN_II
            # Replace values if surplus is negative (shortage)
            i2 <- BIN_II < 0 & table_lbt_input >= 0
            BIN_II[i2] <- table_lbt_input[i2]            
            # DON'T CHANGE NEXT LINE
            bin_replacements_index_table <<- i2 # Otherwise value is not returned
            
        } else if ( isTRUE(calculate == "none")) {
            i1 <- BIN_II == 0 & table_lbt_input >= 0
            BIN_II[i1] <- table_lbt_input[i1]            
            # DON'T CHANGE NEXT LINE
            bin_replacements_index_table <<- i1 # Otherwise value is not returned
        } else if (isTRUE(calculate == "fractions")) {
            BIN_II <- table_bin_input/table_lbt_input
            BIN_II <- round(BIN_II, 3)
        }
        
        # Put BIN_II strata columns right order
        replacement_order <- order_cols(BIN_II)
        BIN_II <- BIN_II[,replacement_order]

        # Put BIN NSO rows right order
        index_order <- match(strat_order, row.names(BIN_II))
        BIN_II <- BIN_II[index_order,]  
        return(BIN_II)
    }


    if (!is.null(LBT_data) && calculate == "none"){

        table_in <- combination_table(table_bin_input, table_lbt_input)

        # Put table_in strata columns right order
        replacement_order <- order_cols(table_in)
        table_in <- table_in[,replacement_order]

        # Put table_in NSO rows right order
        index_order <- match(strat_order, row.names(table_in))
        table_in <- table_in[index_order,]

        print("Replaced empty BIN cells with LBT data")
    } else if (is.null(LBT_data) && calculate == "none") {
        table_in <- table_bin_input
    } else if (!is.null(LBT_data) && calculate=="variance"){
        print("Showing variance of LBT data")
        table_in <- table_lbt_input
    } else if (calculate=="relative_svc_deviation") {
        table_in <- table_bin_input
        print("Showing Relative SVC deviations of BIN data")
    }

    # Deals with NA as a row name
    table_in <- table_in[!is.na(row.names(table_in)),]
    if (exists("bin_replacements_index_table") && length(out_add_to_bin_names)>1 ) {
        bin_replacements_index_table <- bin_replacements_index_table[!is.na(row.names(bin_replacements_index_table)),]
    }

    # Put table_in strata columns right order
    replacement_order <- order_cols(table_in)
    table_in <- table_in[,replacement_order]

    # Put table_in NSO rows right order
    index_order <- match(strat_order, row.names(table_in))
    table_in <- table_in[index_order,]


    # Deals with NA as a column name
    for (i in seq_along(colnames(table_in))){
        if (is.na(colnames(table_in)[i])) {
            colnames(table_in)[i] <- "Missing"
            table_in <- table_in[complete.cases(table_in), ]
            if (exists("bin_replacements_index_table")) {
                colnames(bin_replacements_index_table)[i] <- "Missing"
            }
        }
    }    

    # Put table_in strata columns right order
    replacement_order <- order_cols(table_in)
    table_in <- table_in[,replacement_order]

    # Put table_in NSO rows right order
    index_order <- match(strat_order, row.names(table_in))
    table_in <- table_in[index_order,]

    table_in <- as.data.frame.matrix(table_in)

    # Install necessary packages

    names(table_in)[duplicated(names(table_in))[]]

    colnames(table_in) = gsub(pattern = "NA*", replacement = "None", x = colnames(table_in))
    colnames(table_in) = iconv(colnames(table_in), to='ASCII//TRANSLIT') 
    colnames(table_in) = str_to_title(colnames(table_in)) # ; names(table_in)

    if (exists("bin_replacements_index_table")) {
        colnames(bin_replacements_index_table) = gsub(pattern = "NA*", replacement = "None", x = colnames(bin_replacements_index_table))
        colnames(bin_replacements_index_table) = iconv(colnames(bin_replacements_index_table), to='ASCII//TRANSLIT') 
        colnames(bin_replacements_index_table) = str_to_title(colnames(bin_replacements_index_table)) # ; names(table_in)
    }

    setnames(table_in, "Missinoneg", "Missing", skip_absent=TRUE) # Because of LMM data

    ###################################################################################################################################################
    ###################################################################################################################################################
    ###################################################################################################################################################

    if (!is.na(category_type) && ncol(table_in) < 8) {
        print("Less than 8 columns in table_in, assuming that the only categories in the data are region or soil-type")

        categories_vec <- colnames(table_in)
        table_in$Total <- rowSums(table_in)

        table_in <- setDT(table_in, keep.rownames = TRUE)[]

        if(isTRUE(BIN)){
            
            table_in <- table_in %>%
                slice(match(strat_order, rn))

            names(table_in)[1] <- "Category"
        }

        if (!is.null(LBT_data) && isFALSE(calculate == "fractions")) {
            print("Replacing BIN cells with zero observations with data from LBT")
            # Replace the empty BIN cells with LBT
            for (i in 1:(length(categories_vec))) {
                table_in[[i+1]][as.vector(unlist(bin_replacements_index_table[,i]))] = paste0(table_in[[i+1]][as.vector(unlist(bin_replacements_index_table[,i]))], " (in LBT)")        
            }
        }

        flextable_out <- flextable(table_in)

        flextable_out <- align(flextable_out, align = "center", part = "all")
        flextable_out <- bold(flextable_out, i = c(1), bold = TRUE, part = "head")
        flextable_out <- bold(flextable_out, j = c(1), bold = TRUE, part = "body")     

        my_color_fun <- function(x) {
            out <- rep("white", length(x))
            idx <- suppressWarnings(as.numeric(x) <= 5)
            out[idx] <- 'yellow'
            idx <- suppressWarnings(as.numeric(x) <= 3)
            out[idx] <- 'orange'
            idx <- suppressWarnings(as.numeric(x) <= 1)
            out[idx] <- 'pink'
            idx <- suppressWarnings(as.numeric(x) <= 0)
            out[idx] <- 'red'
            idx <- suppressWarnings(is.na(as.numeric(x)))
            out[idx] <- 'red'
            out
        }

        my_color_fun_total <- function(x) {
            out <- rep("white", length(x))
            idx <- suppressWarnings(as.numeric(x) < 33)
            out[idx] <- 'pink'
            idx <- suppressWarnings(as.numeric(x) < 30)
            out[idx] <- 'red'
            out
        }

        if (isTRUE(mark_low_vals)) {
            if( isTRUE(BIN) && isFALSE(calculate == "fractions") ){
                flextable_out <- bg(flextable_out, j=c(categories_vec), bg = my_color_fun, part="body")       
                flextable_out <- bg(flextable_out, j=c("Total"), bg = my_color_fun_total, part="body")   
            } else if(isTRUE(BIN) && isTRUE(calculate == "fractions")) {
                flextable_out <- bg(flextable_out, j=c(categories_vec), bg = my_color_fun_fractions, part="body")       
            } else {
                # FIX
                flextable_out <- bg(flextable_out, j = c("Cat_1", "Cat_2", "Cat_3", "Cat_4", "Total"), bg = my_color_fun, part="body")
            }
        }
        
        flextable_out <- fontsize(flextable_out, i = NULL, j = NULL, size = 8, part = "header")
        flextable_out <- fontsize(flextable_out, i = NULL, j = NULL, size = 8, part = "body")


        if(isTRUE(BIN)){
            flextable_out <- set_caption(flextable_out, 
                caption = "Sampling fraction according to the 2018 Agricultural Census by category", 
                style = "Table Caption", 
                autonum = run_autonum(seq_id = "tab", bkm = "tab1")
            )
        } else {
            flextable_out <- set_caption(flextable_out, 
                caption = "LMM observations", 
                style = "Table Caption", 
                autonum = run_autonum(seq_id = "tab", bkm = "tab1")
            )  
        }

        list_of_cats <- list()
        list_of_cats[[1]] <- flextable_out

    } else { ##########################################################################################################################################

        print("Input data has more than 7 columns; Assuming that SO strata are used.")
        # Check for categories
        name_vec <- names(table_in)
        name_vec <- data.frame(name_vec)

        name_vec <- name_vec %>%
            tidyr::extract(name_vec, c('lower', 'upper', 'threshold_categories'), '(\\d+),(\\d+)[\\]\\)]\\s*(\\w*)', convert = TRUE)
       
        # Adding missing columns that do not show up at all for some categories
        if (!is.na(name_vec$threshold_categories[1])) {
            present_combinations <- str_split(names(table_in), pattern=" ", n = 2, simplify=TRUE)
            present_strata_combinations <- unique(present_combinations[,1]) # 13

            present_categories <- unique(present_combinations[,2]) # 5

            all_combinations <- rbind(rep(present_strata_combinations, each = length(present_categories)))
            all_combinations <- rbind(all_combinations, present_categories)
            all_combinations <- paste0(all_combinations[1,], " ", all_combinations[2,])
            
            if (    length(names(table_in)) != length(all_combinations) ) {
                # Add missing categories
                add_name_vector <- all_combinations[all_combinations %nin% names(table_in)]

                for (i in seq_along(add_name_vector)) {
                    if (add_name_vector[i] %nin% names(table_in)) {
                        table_in[ , add_name_vector[i]] <- 0
                        if (exists(bin_replacements_index_table)) {
                            bin_replacements_index_table <- cbind(bin_replacements_index_table,FALSE)
                            colnames(bin_replacements_index_table)[ncol(bin_replacements_index_table)] <- add_name_vector[i]
                        }
                    }
                }
            }
            print("Missing table categories added")
        }

        lowers <- unique(name_vec$lower)
        uppers <- unique(name_vec$upper)
        categories_vec <- unique(name_vec$threshold_categories)

        lowers <- sort(lowers)
        uppers <- sort(uppers)
        categories_vec <- sort(categories_vec)

        strata_df <- tibble::enframe(strata_in, name = "ENG_name", value = "strata_column")

        set_flextable_defaults(
            font.size = 10, font.family = "Helvetica",
            text.align = "center",
            font.color = "#333333",
            border.color = "gray"
            # padding.top = 3, padding.bottom = 3,
            # padding.left = 4, padding.right = 4
            )

        names(table_in)[duplicated(names(table_in))[]]

    # Make thresholds
        thresholds_maker <- function(table_in, by_cat=FALSE) {

            if (!is.na(categories_vec[1])) {
                out <- rbind(rep(lowers, each = length(categories_vec)), rep(uppers, each = length(categories_vec)))
                colnames(out) <- rep(c(categories_vec), length.out = ncol(out))
                row.names(out) <- c('lower threshold x1000', 'upper threshold x1000')
                # https://stackoverflow.com/questions/62960127/convert-column-names-into-first-row-of-data-frame-in-r
                if (by_cat) {
                    firstrow <- colnames(out)
                    out <- setNames(rbind(firstrow, out), names(out))
                    rownames(out)[1] <- "region"
                    out <- data.frame(out)
                    # out[out=="1000000"]<-"Infinity"
                    names(out) <- apply(out, 2, paste0, collapse="_")
                    out <- data.table(out, keep.rownames = TRUE)[]
                } else {
                    lastrow <- colnames(out)
                    out <- setNames(rbind(out,lastrow), names(out))
                    rownames(out)[3] <- "region"
                    out <- data.frame(out)
                    # out[out=="1000000"]<-"Infinity"
                    names(out) <- apply(out, 2, paste0, collapse="_")
                    out <- data.table(out, keep.rownames = TRUE)[]
                }
            } else {
                out <- rbind(lowers, uppers)
                row.names(out) <- c('lower threshold x1000', 'upper threshold x1000')
                firstrow <- colnames(out)
                # https://stackoverflow.com/questions/62960127/convert-column-names-into-first-row-of-data-frame-in-r
                out <- setNames(rbind(firstrow, out), names(out))
                out <- data.frame(out)
                names(out) <- apply(out, 2, paste0, collapse="_")
                out <- data.table(out, keep.rownames = TRUE)[]
                names_out <- vector()
                out <- data.frame(out)
                for (i in seq_along(out[1,])) {
                    names_out <- append(names_out, paste0('str_', paste0(out[,i])[1], '_', paste0(out[,i])[2],collapse=""))
                }
                out <- data.frame(out)
                names(out) <- names_out
            }
            
            out
        }

        thresholds_cat <- thresholds_maker(table_in, by_cat=TRUE)
        thresholds_strata <- thresholds_maker(table_in)

        # COLUM ORDER
        # https://stackoverflow.com/questions/72141429/converting-column-names-so-they-can-be-put-in-an-numerical-order/72141622?noredirect=1#comment127466664_72141622
        replacement_order <- order_cols(table_in)
        table_in <- table_in[replacement_order]

        names(table_in)[duplicated(names(table_in))[]]

        # First by category
        if (!is.na(categories_vec[1])) {
            category_order <- stringr::word(names(table_in), -1)
            category_order <- order(category_order)
            table_in <- table_in[category_order]
        }

        category_order_sorted_by_strata <- stringr::word(names(thresholds_strata), 1)

        ############################################################################

        # total observations/population per category
        if (is.na(categories_vec[1])) {
            table_in$Total <- row_sums_stored_from_only_bin
        }
        # Convert to data.table (data.table does not support rownames)
        table_in <- data.table(table_in, keep.rownames = TRUE)[] # This table already has the right numbers for BIN and LBT

        # REPLACEMENT TABLE WITH LBT
        if (!is.null(LBT_data) && calculate == "none") {
            print("Replacing BIN cells with zero observations with data from LBT")
            # # Put table_in strata columns right order
            # replacement_order <- order_cols(table_in)
            # table_in <- table_in[,replacement_order]

            # # Put table_in NSO rows right order
            # index_order <- match(strat_order, row.names(table_in))
            # table_in <- table_in[index_order,]

            replacement_table_in <- table_in

            # Puts the rows of the replacement table in the correct order
            replacement_table_in <- replacement_table_in %>%
                slice(match(strat_order, rn))
        
            # BIN REPLACEMENTS WAS STORED IN COMBINATION TABLE
            bin_replacements_index_table <- bin_replacements_index_table[match(strat_order, rownames(bin_replacements_index_table)),]

            col_order <- names(replacement_table_in)[2:(ncol(replacement_table_in)-1)]

            bin_replacements_index_table <- bin_replacements_index_table[, col_order]

            # 2:(ncol(bin_replacements_index_table)-1)] simply skips the columns rn and Total
            replacement_table_in[,2:(ncol(bin_replacements_index_table)-1)] <- replacement_table_in[,2:(ncol(bin_replacements_index_table)-1)]    

            # Adapt negative values table to match replacement_table_in
            out_BIN_II <- as.data.frame.matrix(out_BIN_II*-1)

            # Replace the zero BIN cells with LBT
            # if (length(replacement_table_in)<16) {
            if (isTRUE(strata=="calculate"))
                for (i in 1:(ncol(replacement_table_in)-2)) {
                    replacement_table_in[[i+1]][as.vector(unlist(bin_replacements_index_table[,i]))] = paste0(replacement_table_in[[i+1]][as.vector(unlist(bin_replacements_index_table[,i]))], " in LBT - ", 
                    paste0("(", out_BIN_II[[i]][as.vector(unlist(bin_replacements_index_table[,i]))]," needed)")) # [i] instead of [i+1] because no "rn" column
            } else {
                for (i in 1:(ncol(replacement_table_in)-2)) {
                    replacement_table_in[[i+1]][as.vector(unlist(bin_replacements_index_table[,i]))] = 
                    paste0( replacement_table_in[[i+1]][as.vector(unlist(bin_replacements_index_table[,i]))], " in LBT"  )
                }
            }
            # }

            replacement_table_in <- merge(replacement_table_in, strata_df, all.x=TRUE, by.x="rn", by.y="ENG_name", sort=TRUE)

            replacement_table_in$Total <- as.character(replacement_table_in$Total)

            table_in <- replacement_table_in

            # # Put table_in strata columns right order
            # replacement_order <- order_cols(table_in)
            # table_in <- table_in[,replacement_order]

            # # Put table_in NSO rows right order
            # index_order <- match(strat_order, row.names(table_in))
            # table_in <- table_in[index_order,]

        } else if (calculate != "none") {
            table_in <- merge(table_in, strata_df, all.x=TRUE, by.x="rn", by.y="ENG_name", sort=TRUE)
        } 

    # Add lists of frequencies ###############################################

        
        if (!is.na(categories_vec[1])) {  
            table_in <- table_in %>% mutate_if(is.character,as.numeric)       
            print("Making a separate table for each of the categories")
            # Split table categories
            frequency_table <- table_in %>%
                pivot_longer(cols = -c(rn, strata_column, Total),
                names_to = c("lower", "upper", "direction"),
                names_pattern = "\\[(\\d+),(\\d+)[\\)\\]]\\s+(\\S+$)",
                    values_drop_na = TRUE) %>% 
                type.convert(as.is = TRUE) 

            # Remove total of all categories together
            frequency_table$Total <- NULL
            # Only keep rows of which the strata_column match
            frequency_table <- frequency_table %>%
                rowwise() %>% 
                filter(is.consec(as.numeric(lower), as.numeric(upper), strata_column)) %>%
                ungroup()

            frequency_table$value <- as.character(frequency_table$value)
            frequency_table$upper <- as.character(frequency_table$value)
            frequency_table <- frequency_table %>%
                group_by(rn, direction  ) %>% 
                summarise(value = as.character(sum(as.numeric(value), na.rm=TRUE)), .groups = 'drop_last', upper="1000001", strata_column=strata_column) %>%                  # get sum of sizes
                bind_rows(frequency_table, .)   

            # Remove the duplicate rows     
            frequency_table <- unique( frequency_table )

            # Convert upper back to numeric for sorting                     
            frequency_table$upper <- as.numeric(frequency_table$upper)
            frequency_table <- frequency_table %>%
                arrange(rn, direction, upper)

            # Create list
            frequency_table <- frequency_table %>%
                group_by(rn, strata_column) %>% 
                summarise(freq = list(value), .groups = 'drop')  

        } else {
            out_table_in <<- table_in
            frequency_table <- out_table_in  %>%
                pivot_longer(-c(rn, strata_column, Total)) %>%
                tidyr::extract(name, c('lower', 'upper', 'direction'), '(\\d+),(\\d+)[\\]\\)]\\s*(\\w*)', convert = TRUE) %>%
                select(-where(is_all_na)) %>%
                rowwise() %>% 
                filter(is.consec(lower, upper, strata_column)) %>%
                ungroup() %>% 
                group_by(rn, strata_column, Total) %>% 
                summarise(freq = list(value), .groups = 'drop')
                
            for (i in seq_along(frequency_table$freq)){
                frequency_table$freq[[i]] <- append(frequency_table$freq[[i]],frequency_table$Total[[i]])
            }

        }

        frequency_table <- frequency_table %>%
                slice(match(strat_order, rn))

        ##########################################################################################
        # Calculate standard colspan
        oldw <- getOption("warn")
        options(warn = -1)

        x <- as.numeric(unique(unlist(thresholds_strata)))
        options(warn = oldw)
        unique_num_values <- x[!is.na(x)]
        unique_num_values <- sort(unique_num_values)
        total_colspan <- c(unique_num_values, "Total")
        total_colspan <- gsub(1000000, "Infinity", total_colspan, fixed = T)

        frequency_table$strata_column <- lapply(frequency_table$strata_column, \(x){
            # x <- append(x, c("Total"))
            x <- gsub(1000000, "Infinity", x, fixed = T)
            x <- append(x, c("Total"))
            x
        })

        # Index differences
        l <- lapply(frequency_table$strata_column, \(y) sapply(y, \(x) which(total_colspan == x) - which(y == x)))
        frequency_table$l <- l
        frequency_table$l <- lapply(frequency_table$l, \(x) diff(x) + 1)

        if (!is.na(categories_vec[1])){

            if (length(frequency_table$freq[[1]]) > length(total_colspan)) {
                frequency_table$l <- lapply(frequency_table$l, \(x){
                    x <- append( x, rep( x, length(categories_vec) - 1 )  )
                    x
                })
            }
            print("For the following categories:")
            print(categories_vec)
        }
                    
        html_table_in <- frequency_table[c("rn", "freq","l")] %>% 
            unnest_longer(freq) %>% 
            mutate(colspan = unlist(frequency_table$l),
                width = colspan * 50)

        colspan <- html_table_in %>%
            group_by(rn) %>%
            summarise(colspan = list(colspan))

        frequency_table <- merge(frequency_table, colspan, by.x="rn", by.y="rn")
        
        if (!is.na(categories_vec[1])) {
            # SET TABLE ORDER
            frequency_table <- frequency_table %>%
                slice(order(factor(rn, strat_order)))
            print("Set table order to order used in FADN 2018")
        } else {
            # SET TABLE ORDER
            frequency_table <- frequency_table %>%
                slice(match(strat_order, rn))
            print("Set table order to order used in FADN 2018")                
        }

        frequency_table_out <- frequency_table[,c("rn", "freq", "colspan")]

        moveMeDataTable <-function(data, tomove, where = "last", ba = NULL) {
            data <- data.table(data)
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

        if (!is.na(categories_vec[1])){
            # Adapt
            all_categories <- gsub("_"," ", names(thresholds_strata))
            all_categories_1 <- stringr::word(all_categories, -1)
            all_category_order <- order(all_categories_1)
            thresholds_cat <- thresholds_cat[,all_category_order, with=FALSE]
            thresholds_cat <- data.table(thresholds_cat)
            thresholds_cat <- moveMeDataTable(thresholds_cat, "rn", "first")
            sum_categories <- vector()
            for (i in seq_along(categories_vec)) {
                sum_categories[i] <- paste0(categories_vec[i], "_total")
            }
            thresholds_cat[,sum_categories] <- NA
            thresholds_cat <- data.table(thresholds_cat)
            thresholds_cat <- as.data.frame(lapply(thresholds_cat, as.character))
            sum_categories_clean <- gsub("_", " ", sum_categories)
            thresholds_cat[3,sum_categories] <- lapply(sum_categories_clean, paste)
            thresholds_cat <- data.table(thresholds_cat)
            for (i in seq_along(categories_vec)) {
                thresholds_cat <- moveMeDataTable(data = thresholds_cat, tomove = sum_categories[i], where = "after", ba = paste0( categories_vec[i], "_3000_1000000"))
            }
        } else {
            thresholds_cat[,"Total"] <- "Total"
        }

        out <- map(1:nrow(frequency_table), function(index){
            out <- data.frame("freq" = frequency_table$freq[[index]], 
                                "span" = frequency_table$colspan[[index]]) %>% 
                tidyr::uncount(span, .id = 'span') %>% 
                mutate(freq = ifelse(span>1, NA, freq)) %>% 
                t %>% 
                as.data.frame() %>% 
                mutate(rn = frequency_table$rn[[index]],
                    across(everything(), ~as.character(.))) %>% 
                select(rn, everything()) %>%
                # Correct names need to be selected
                set_names(nm = names(thresholds_cat)) %>% 
                slice(1)
            return(out)
        }) 

        combined <- thresholds_cat %>% 
            mutate(across(everything(),  ~as.character(.))) %>% 
            bind_rows(out)

        if (!is.na(categories_vec[1])) {
            combined <- combined[-c(1:3),]
        } else {
            combined <- combined[-c(1:2),]
        }
        
        threshold_list <- list()
        if (!is.na(categories_vec[1])) {
            for (i in seq_along(categories_vec)) {
                first_col <- thresholds_cat[,1]
                cols <- thresholds_cat %>% select(starts_with(categories_vec[i]))
                threshold_list[[i]] <- cbind(first_col, cols)
            } 
        } else {
            threshold_list[[1]] <- thresholds_cat
        }

        spans <- map(1:length(frequency_table$colspan), function(index){
            spans <- frequency_table$colspan[[index]] %>%  
            as_tibble() %>% 
            mutate(idx = row_number()) %>% 
            tidyr::uncount(value, .remove = F) %>% 
            group_by(idx) %>%
            mutate(pos = 1:n(),
                    value = ifelse(pos != 1, 0, value)) %>% 
            ungroup() %>% 
            select(value) %>% 
            t
            return(append(1, spans))
        })    

        print("1")
        list_of_cats <- list()
        if (!is.na(categories_vec[1])) {
            for (i in seq_along(categories_vec)) {
                # assign(paste0(categories_vec[i]), combined %>% select(starts_with(categories_vec[i])))
                print(categories_vec[i])
                first_col <- combined[,1]
                other_cols <- combined %>% select(starts_with(categories_vec[i]))
                out_other_cols1 <<- other_cols
                # Too late for totals, since LBT data is already in there, but can be replaced!
                other_cols[,ncol(other_cols)] <- list_of_row_sums_stored_from_only_bin[[i]]
                out_other_cols2 <<- other_cols
                list_of_cats[[i]] <- cbind(first_col, other_cols)
                names(list_of_cats)[i] <- paste0(categories_vec[i], "-gebieden")

                list_of_cats[[i]] <- flextable(list_of_cats[[i]]) %>% theme_box()    
                for (i in seq_along(spans)){
                    spans[[i]] <- spans[[i]][1:11]
                }
                number_of_columns <- (ncol(combined)-1)/length(categories_vec)+1
            }
        } else {
            list_of_cats[[1]] <- flextable(combined) %>% theme_box()   
            number_of_columns <- ncol(combined)
        }



        # FIX IS IN WRONG SPOT
        for (i in 1:length(threshold_list)){
            threshold_list[[i]][[length(threshold_list[[1]])-1]] <- gsub("1000000", "Inf", threshold_list[[i]][[length(threshold_list[[1]])-1]] )
        }

        for (i in seq_along(list_of_cats)) {

            # FIX INDEX
            flextable_out <- list_of_cats[[i]]

            flextable_out$body$spans$rows[1:nrow(flextable_out$body$spans$rows),] <- matrix(unlist(spans), ncol = number_of_columns, byrow = TRUE)
            
            # Label var names
            a <- names(threshold_list[[i]])
            b <- as.vector(unlist(threshold_list[[i]][1,]))
            values <- as.list(setNames(b, a))
            flextable_out <- set_header_labels(flextable_out, values=values)
            for (j in 2:nrow(threshold_list[[1]])) {
                flextable_out <- add_header(flextable_out, top = FALSE, values = threshold_list[[i]][j,])
            }
            
            flextable_out <- align(flextable_out, align = "center", part = "all")

            FitFlextableToPage <- function(ft, pgwidth = 6) {

                ft_out <- ft %>% autofit()

                ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
                return(ft_out)
            }


            # set_table_properties(flextable_out, layout = "autofit")
            flextable_out <- FitFlextableToPage(flextable_out, pgwidth = 6)
            
            # GREY THRESHOLDS
            flextable_out <- bg(flextable_out, bg="#bfbfbf", part = "header")
            flextable_out <- color(flextable_out, color="white", part = "header")
            std_border = fp_border(color="white", width = 1)

            # BOLD FIRST COLUMN            
            flextable_out <- bold(flextable_out, j = c(1), bold = TRUE, part = "body")
            
            colwidths <- c(rep(1,length(threshold_list[[1]])))
            values <- c("Type of farm", rep("",length(threshold_list[[1]])-1))
            flextable_out <- add_header_row(flextable_out, top = FALSE, values = values, colwidths = colwidths)
            flextable_out <- italic(flextable_out, i= nrow(thresholds_cat)+1, italic = TRUE, part = "header")
            flextable_out <- bold(flextable_out, i= nrow(thresholds_cat)+1, bold=FALSE, part = "header")
            flextable_out <- color(flextable_out, i= nrow(thresholds_cat)+1, color="black", part = "header")
            flextable_out <- bg(flextable_out, i= nrow(thresholds_cat)+1, bg="white", part = "header")

            # WHITE BORDERS
            flextable_out <- border_inner(flextable_out, border = std_border, part = "header")

            flextable_out <- fontsize(flextable_out, i = NULL, j = NULL, size = 8, part = "header")
            flextable_out <- fontsize(flextable_out, i = NULL, j = NULL, size = 8, part = "body")

            my_color_fun <- function(x) {
                if (calculate=="relative_svc_deviation") {
                    out <- rep("white", length(x))  
                    idx <- suppressWarnings(as.numeric(x) < 25)
                    out[idx] <- 'green'
                    idx <- suppressWarnings(as.numeric(x) > 50)
                    out[idx] <- 'orange'
                    idx <- suppressWarnings(as.numeric(x) > 100)
                    out[idx] <- 'pink'
                    idx <- suppressWarnings(is.na(as.numeric(x)))
                    out[idx] <- 'red'
                    idx <- suppressWarnings(as.numeric(x) == 0)
                    out[idx] <- 'red'                    
                    out
                } else if (calculate=="variance") {
                    out <- rep("white", length(x))  
                    idx <- suppressWarnings(as.numeric(x) <= 10)
                    out[idx] <- 'yellow'
                    idx <- suppressWarnings(as.numeric(x) <= 5)
                    out[idx] <- 'orange'
                    idx <- suppressWarnings(as.numeric(x) <= 3)
                    out[idx] <- 'pink'
                    idx <- suppressWarnings(is.na(as.numeric(x)))
                    out[idx] <- 'red'
                    out
                } else if (is.na(categories_vec[1])) {
                    out <- rep("white", length(x))  
                    idx <- suppressWarnings(as.numeric(x) <= 10)
                    out[idx] <- 'yellow'
                    idx <- suppressWarnings(as.numeric(x) <= 5)
                    out[idx] <- 'orange'
                    idx <- suppressWarnings(as.numeric(x) <= 3)
                    out[idx] <- 'pink'
                    idx <- suppressWarnings(is.na(as.numeric(x)))
                    out[idx] <- 'red'
                    out
                } else {
                    out <- rep("white", length(x))  
                    idx <- suppressWarnings(as.numeric(x) <= 2)
                    out[idx] <- 'yellow'
                    idx <- suppressWarnings(as.numeric(x) <= 1)
                    out[idx] <- 'orange'
                    idx <- suppressWarnings(as.numeric(x) <= 0)
                    out[idx] <- 'pink'
                    idx <- suppressWarnings(is.na(as.numeric(x)))
                    out[idx] <- 'red'                    
                    out
                }
            }


            my_color_fun_fractions <- function(x) {
                out <- rep("white", length(x))
                idx <- suppressWarnings(as.numeric(x) == 0)
                out[idx] <- 'red'
                idx <- suppressWarnings(as.numeric(x) > 0.1)
                out[idx] <- 'orange'
                out
            }

            
            if (isTRUE(mark_low_vals)) {
                    flextable_out <- bg(flextable_out, bg = my_color_fun)
                    flextable_out <- bg(flextable_out, j=c(1), bg = "white")
                    # Sum column under 30 is red
                    flextable_out <- bg(flextable_out, j=c(2), bg = "white")
            } 

            if (!is.na(categories_vec[1])) {
                flextable_out <- set_caption(flextable_out, 
                    caption = "Sampling fraction according to the 2018 Agricultural Census by stratum and category", 
                    style = "Table Caption", 
                    autonum = run_autonum(seq_id = "tab", bkm = "tab1"))
            } else {
                flextable_out <- set_caption(flextable_out, 
                    caption = "Sampling fraction according to the 2018 Agricultural Census by stratum", 
                    style = "Table Caption", 
                    autonum = run_autonum(seq_id = "tab", bkm = "tab1"))
            }

            list_of_cats[[i]] <- flextable_out
            
        }    

    }

    return(list_of_cats)

}