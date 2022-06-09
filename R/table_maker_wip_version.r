table_maker_wip_version <- function(table_in, strata_in = NULL, mark_low_vals = TRUE, strata_all= FALSE) {

    table_in <- as.data.frame.matrix(table_in)

    # English Names and Strata if no strat provided
    when_strata_in_is_null_start <- list(
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

    if(strata_all==TRUE){
        for (i in when_strata_in_is_null_start) {
            when_strata_in_is_null_start[[i]] <- c(0, 25, 50, 100, 250, 500, 1000, 1500, 3000, "Infinity")
        }
    }

    # FOR TESTING
    # table_in <- table_bin_cat_soil_input 
    # table_in <- table_lbt_cat_reg_input
    # table_in <- table_lbt_cat_soil_input

    # Install necessary packages

    # List of packages
    packages <- c(
        "microbenchmark",
        "officer",
        "zeallot", 
        "devtools",
        "readxl",
        "writexl",
        "fuzzyjoin", 
        "htmltools", 
        "purrr", 
        "flextable", 
        "glmnet", 
        "stringr",
        "stringi",  
        "magrittr",
        "Hmisc", 
        "tidyr",
        "dplyr",
        "data.table"
    )

    x <- lapply(packages, function(packages) {if (!require(packages, character.only = TRUE, quietly = TRUE, warn.conflicts=FALSE)) {library(packages, character.only = T, quietly = TRUE, warn.conflicts=FALSE)}})

    # Changes dput to deal with data.table
    dput = function(x, ...) { if(is.data.table(x)) { setattr(x, '.internal.selfref', NULL) }; base::dput(x, ...) }

    is_all_na <- function(x)all(is.na(x))

    names(table_in)[duplicated(names(table_in))[]]

    names(table_in) = gsub(pattern = "NA*", replacement = "None", x = names(table_in))
    names(table_in) = iconv(names(table_in), to='ASCII//TRANSLIT') 
    names(table_in) = str_to_title(names(table_in)) # ; names(table_in)

    if (ncol(table_in) < 6) {
        table_in$Sum_lbt <- rowSums(table_in)
        table_in <- setDT(table_in, keep.rownames = TRUE)[]
        strat_order <-  names(when_strata_in_is_null_start)

        table_in <- table_in %>%
            slice(match(strat_order, rn))

        names(table_in)[1] <- "Category"
        names(table_in)[length(names(table_in))] <- "Total"

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
            out
        }

        if (isTRUE(mark_low_vals)) {
            flextable_out <- bg(flextable_out, bg = my_color_fun, part="body")
        }

        flextable_out <- set_caption(flextable_out, 
            caption = "Sampling fraction according to the 2018 Agricultural Census by category", 
            style = "Table Caption", 
            autonum = run_autonum(seq_id = "tab", bkm = "tab1")
        )

        list_of_cats <- list()
        list_of_cats[[1]] <- flextable_out

    } else {

        # Check for categories
        name_vec <- names(table_in)
        name_vec <- data.frame(name_vec)

        name_vec <- name_vec %>%
            tidyr::extract(name_vec, c('lower', 'upper', 'categories'), '(\\d+),(\\d+)[\\]\\)]\\s*(\\w*)', convert = TRUE)

        # Adding missing columns that do not show up at all for some categories
        if (!is.na(name_vec$categories[1])) {
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
                    }
                }
            }
        }
        
        lowers <- unique(name_vec$lower)
        uppers <- unique(name_vec$upper)
        categories <- unique(name_vec$categories)

        lowers <- sort(lowers)
        uppers <- sort(uppers)
        categories <- sort(categories)

        when_strata_in_is_null <- tibble::enframe(when_strata_in_is_null_start, name = "ENG_name", value = "strata")

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

        if (!is.na(categories[1])) {
            out <- rbind(rep(lowers, each = length(categories)), rep(uppers, each = length(categories)))
            colnames(out) <- rep(c(categories), length.out = ncol(out))
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
            lapply(sum) %>% 
            unlist() %>% 
            order()

        }

        order <- order_cols(table_in)
        table_in <- table_in[order]

        names(table_in)[duplicated(names(table_in))[]]

        # First by category
        if (!is.na(categories[1])) {
            category_order <- stringr::word(names(table_in), -1)
            category_order <- order(category_order)
            table_in <- table_in[category_order]
        }

        category_order_sorted_by_strata <- stringr::word(names(thresholds_strata), 1)
        ############################################################################
        # total observations/population per category
        table_in$Sum_table_in <- rowSums(table_in)

        # Convert to data.table (data.table does not support rownames)
        table_in <- data.table(table_in, keep.rownames = TRUE)[]

        # if (!exists("strata_in"))

        # WATCH OUT - IS TRUE IF STRATA 
        if (is.null(strata_in)) {
                table_in <- merge(table_in, when_strata_in_is_null, all.x=TRUE, by.x="rn", by.y="ENG_name", sort=TRUE)
        } else {
                table_in <- merge(table_in, strata_in, all.x=TRUE, by.x="rn", by.y="ENG_name", sort=TRUE)
        }


    # Add lists of frequencies ###############################################
        if (!is.na(categories[1])) {
            frequency_table <- table_in %>%
                pivot_longer(cols = -c(rn, strata, Sum_table_in),
                names_to = c("lower", "upper", "direction"),
                names_pattern = "\\[(\\d+),(\\d+)[\\)\\]]\\s+(\\S+$)",
                values_drop_na = TRUE) 
    
            frequency_table <- frequency_table %>% 
                type.convert(as.is = TRUE) %>% 
                select(-where(is_all_na)) %>%
                group_by(rn, direction) %>%
                filter(lower%in%strata[[1]] & upper %in% strata[[1]]) %>%
                group_by(upper,.add = TRUE) %>%   
                summarise(freq = sum(value), .groups = 'drop_last') %>% 
                group_modify(~add_row(.,freq = sum(.$freq))) %>% group_by(rn) %>%
                summarise(freq = list(freq), .groups = "drop")
        } else {
            frequency_table <- table_in %>%
                pivot_longer(-c(rn, strata)) %>%
                tidyr::extract(name, c('lower', 'upper', 'categories'), '(\\d+),(\\d+)[\\]\\)]\\s*(\\w*)', convert = TRUE)  %>% 
                select(-where(is_all_na)) %>%
                group_by(rn) %>%
                filter(lower%in%strata[[1]] & upper %in% strata[[1]]) %>%
                group_by(upper,.add = TRUE) %>%
                summarise(freq = sum(value), .groups = 'drop_last') %>%
                group_modify(~add_row(.,freq = sum(.$freq))) %>%
                summarise(freq = list(freq))
        }

        frequency_table <- merge(frequency_table, when_strata_in_is_null, all.x=TRUE, by.x="rn", by.y="ENG_name")

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
        atotal_colspan <<- total_colspan

        frequency_table$strata <- lapply(frequency_table$strata, \(x){
            # x <- append(x, c("Total"))
            x <- gsub(1000000, "Infinity", x, fixed = T)
            x <- append(x, c("Total"))
            x
        })
        
        afrequency_table <<- frequency_table
        
        # Index differences
        l <- lapply(frequency_table$strata, \(y) sapply(y, \(x) which(total_colspan == x) - which(y == x)))
        frequency_table$l <- l
        frequency_table$l <- lapply(frequency_table$l, \(x) diff(x) + 1)

        if (length(frequency_table$freq[[1]]) > length(total_colspan)) {
            frequency_table$l <- lapply(frequency_table$l, \(x){
                x <- append(x,rep(x,length(categories)-1))
                x
            })
        }

        html_table_in <- frequency_table[1:2] %>% 
            unnest_longer(freq) %>% 
            mutate(colspan = unlist(frequency_table$l),
                width = colspan * 50)

        colspan <- html_table_in %>%
            group_by(rn) %>%
            summarise(colspan = list(colspan))

        frequency_table <- merge(frequency_table, colspan, by.x="rn", by.y="rn")

        # SET TABLE ORDER
        strat_order <-  names(when_strata_in_is_null_start)

        frequency_table <- frequency_table %>%
            slice(match(strat_order, rn))

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

        if (!is.na(categories[1])){
            # Adapt
            all_categories <- gsub("_"," ", names(thresholds_strata))
            all_categories_1 <- stringr::word(all_categories, -1)
            all_category_order <- order(all_categories_1)
            thresholds_cat <- thresholds_cat[,all_category_order, with=FALSE]
            thresholds_cat <- data.table(thresholds_cat)
            thresholds_cat <- moveMeDataTable(thresholds_cat, "rn", "first")
            sum_categories <- vector()
            for (i in seq_along(categories)) {
                sum_categories[i] <- paste0(categories[i], "_total")
            }
            thresholds_cat[,sum_categories] <- NA
            thresholds_cat <- data.table(thresholds_cat)
            thresholds_cat <- as.data.frame(lapply(thresholds_cat, as.character))
            sum_categories_clean <- gsub("_", " ", sum_categories)
            thresholds_cat[3,sum_categories] <- lapply(sum_categories_clean, paste)
            thresholds_cat <- data.table(thresholds_cat)
            for (i in seq_along(categories)) {
                thresholds_cat <- moveMeDataTable(data = thresholds_cat, tomove = sum_categories[i], where = "after", ba = paste0( categories[i], "_3000_1000000"))
            }
        } else {
            thresholds_cat[,"Total"] <- "Total"
        }

        athresholds_cat <<- thresholds_cat

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

        if (!is.na(categories[1])) {
            combined <- combined[-c(1:3),]
        } else {
            combined <- combined[-c(1:2),]
        }

        threshold_list <- list()
        if (!is.na(categories[1])) {
            for (i in seq_along(categories)) {
                first_col <- thresholds_cat[,1]
                cols <- thresholds_cat %>% select(starts_with(categories[i]))
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
    
        list_of_cats <- list()
        if (!is.na(categories[1])) {
            for (i in seq_along(categories)) {
                # assign(paste0(categories[i]), combined %>% select(starts_with(categories[i])))
                first_col <- combined[,1]
                cols <- combined %>% select(starts_with(categories[i]))
                list_of_cats[[i]] <- cbind(first_col, cols)
                names(list_of_cats)[i] <- paste0(categories[i], "-gebieden")
                list_of_cats[[i]] <- flextable(list_of_cats[[i]]) %>% theme_box()    
                for (i in seq_along(spans)){
                    spans[[i]] <- spans[[i]][1:11]
                }
                number_of_columns <- (ncol(combined)-1)/length(categories)+1
            }
        } else {
            list_of_cats[[1]] <- flextable(combined) %>% theme_box()   
            number_of_columns <- ncol(combined)
        }

        # athreshold_list <- lapply(athreshold_list, \(x){
        #     # x <- append(x, c("Total"))
        #     x <- gsub(1000000, "Infinity", x, fixed = T)
        #     x
        # })

        # FIX IS IN WRONG SPOT
        for (i in 1:length(threshold_list)){
            threshold_list[[i]][[length(athreshold_list[[1]])-1]] <- gsub("1000000", "Inf", threshold_list[[i]][[length(athreshold_list[[1]])-1]] )
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

            my_color_fun <- function(x) {
                if (is.na(categories[1])) {
                    out <- rep("white", length(x))  
                    idx <- suppressWarnings(as.numeric(x) <= 10)
                    out[idx] <- 'yellow'
                    idx <- suppressWarnings(as.numeric(x) <= 5)
                    out[idx] <- 'orange'
                    idx <- suppressWarnings(as.numeric(x) <= 3)
                    out[idx] <- 'pink'
                    out
                } else {
                    out <- rep("white", length(x))  
                    idx <- suppressWarnings(as.numeric(x) <= 2)
                    out[idx] <- 'yellow'
                    idx <- suppressWarnings(as.numeric(x) <= 1)
                    out[idx] <- 'orange'
                    idx <- suppressWarnings(as.numeric(x) <= 0)
                    out[idx] <- 'pink'
                    out
                }
            }
            
            if (isTRUE(mark_low_vals)) {
                if (!is.na(categories[1])) {
                    flextable_out <- bg(flextable_out, bg = my_color_fun)
                } else {
                    flextable_out <- bg(flextable_out, bg = my_color_fun)
                }
            } 

            if (!is.na(categories[1])) {
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

table_lbt <- table_maker_wip_version(table_lbt_input); table_lbt

table_bin <- table_maker_wip_version(table_bin_input); table_bin



table_lbt_cat_soil <- table_maker_wip_version(table_lbt_cat_soil_input); table_lbt_cat_soil
table_bin_cat_soil <- table_maker_wip_version(table_bin_cat_soil_input); table_bin_cat_soil