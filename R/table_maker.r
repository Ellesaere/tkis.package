table_maker <- function(table_in, strata_in = NULL) {

    # FOR TESTING
    # table_in <- table_bin_cat_soil_input 
    # table_in <- table_lbt_cat_reg_input
    # table_in <- table_lbt_cat_soil_input

    packages <- c(
        "microbenchmark", 
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

    names(table_in) = gsub(pattern = "NA*", replacement = "Unknown", x = names(table_in))
    names(table_in) = iconv(names(table_in), to='ASCII//TRANSLIT') 
    names(table_in) = str_to_title(names(table_in)) # ; names(table_in)

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

    # English Names and Strata
    when_strata_in_is_null <- list(
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

    when_strata_in_is_null <- tibble::enframe(when_strata_in_is_null, name = "ENG_name", value = "strata")

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
        row.names(out) <- c('lower threshold', 'upper threshold')
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
          row.names(out) <- c('lower threshold', 'upper threshold')
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

    if (!exists("strata_in")) {
            table_in <- merge(table_in, when_strata_in_is_null, all.x=TRUE, by.x="rn", by.y="ENG_name")
    } else {
        if (is.null(strata_in)) {
            table_in <- merge(table_in, when_strata_in_is_null, all.x=TRUE, by.x="rn", by.y="ENG_name")
        }
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
    total_colspan <- c(unique_num_values, "SUM")
    total_colspan <- gsub(1000000, "Infinity", total_colspan, fixed = T)

    frequency_table$strata <- lapply(frequency_table$strata, \(x){
        # x <- append(x, c("SUM"))
        x <- gsub(1000000, "Infinity", x, fixed = T)
        x <- append(x, c("SUM"))
        x
    })
    
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

    # # FOR TESTING
    # temp <- frequency_table$rn 
    # for (i in seq_along(  temp  )   ) {
    #     print(frequency_table$rn[[i]])
    #     print(  length( frequency_table$freq[[i]]   )   )
    #     print(  length( frequency_table$l[[i]]  )   )
    # }

    # length(unlist(frequency_table$l[[23]])) # = 755
    # length(unlist(frequency_table$freq[[23]])) # = 749
    # length(unlist(frequency_table$l)) # = 755
    # length(unlist(frequency_table$freq)) # = 749

    html_table_in <- frequency_table[1:2] %>% 
        unnest_longer(freq) %>% 
        mutate(colspan = unlist(frequency_table$l),
            width = colspan * 50)

    colspan <- html_table_in %>%
        group_by(rn) %>%
        summarise(colspan = list(colspan))

    frequency_table <- merge(frequency_table, colspan, by.x="rn", by.y="rn")

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
            sum_categories[i] <- paste0(categories[i], "_sum")
        }
        thresholds_cat[,sum_categories] <- NA
        thresholds_cat <- data.table(thresholds_cat)
        thresholds_cat <- as.data.frame(lapply(thresholds_cat, as.character))
        thresholds_cat[3,sum_categories] <- lapply(sum_categories, paste)
        thresholds_cat <- data.table(thresholds_cat)
        for (i in seq_along(categories)) {
            # thresholds_cat[3, (sum_categories) := lapply(.SD, function(x) paste0(sum_categories[i]) ), .SDcols = sum_categories]
            thresholds_cat <- moveMeDataTable(data = thresholds_cat, tomove = sum_categories[i], where = "after", ba = paste0( categories[i], "_3000_1000000"))
        }
    } else {
        thresholds_cat[,"Sum"] <- "Sum"
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

    flextable_out <- flextable(combined) %>% 
        theme_box()    

    # FIX INDEX
    if (!is.na(categories[1])) {
        flextable_out$body$spans$rows[4:nrow(flextable_out$body$spans$rows),] <- matrix(unlist(spans), ncol = ncol(combined), byrow = TRUE)
    } else {
        flextable_out$body$spans$rows[3:nrow(flextable_out$body$spans$rows),] <- matrix(unlist(spans), ncol = ncol(combined), byrow = TRUE)    
    }

    flextable_out <- align(flextable_out, align = "center", part = "all")

    FitFlextableToPage <- function(ft, pgwidth = 6){

        ft_out <- ft %>% autofit()

        ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
        return(ft_out)
    }

    # set_table_properties(flextable_out, layout = "autofit")
    flextable_out <- FitFlextableToPage(flextable_out, pgwidth = 6)

    return(flextable_out)
}