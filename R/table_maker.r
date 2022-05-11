table_maker <- function(table_in, strata_in = NULL) {

  set_flextable_defaults(
      font.size = 10, font.family = "Helvetica",
      text.align = "center",
      font.color = "#333333",
      border.color = "gray"
      # padding.top = 3, padding.bottom = 3,
      # padding.left = 4, padding.right = 4
      )

  # Check for categories
  name_vec <- names(table_in)
  name_vec <- data.frame(name_vec)
  name_vec <- name_vec %>%
      tidyr::extract(name_vec, c('lower', 'upper', 'rest'), '(\\d+),(\\d+)[\\]\\)]\\s*(\\w*)', convert = TRUE)

  categories <- unique(name_vec$rest)

  # Make thresholds
  thresholds_maker <- function(table_in, by_cat=FALSE) {
      # name_vec <- names(table_in)
      # name_vec <- data.frame(name_vec)
      # name_vec <- name_vec %>%
      #     tidyr::extract(name_vec, c('lower', 'upper', 'rest'), '(\\d+),(\\d+)[\\]\\)]\\s*(\\w*)', convert = TRUE)

      # categories <- unique(name_vec$rest)
      lowers <- unique(name_vec$lower)
      uppers <- unique(name_vec$upper)

      categories <- setDT(data.frame(categories))
      lowers <- setDT(data.frame(lowers))
      uppers <- setDT(data.frame(uppers))

      categories <- setorder(categories, categories)
      lowers <- setorder(lowers, lowers)
      uppers <- setorder(uppers, uppers)

      categories <- t(categories)
      lowers <- t(lowers)
      uppers <- t(uppers)

      out <- rbind(rep(lowers, each = length(categories)), rep(uppers, each = length(categories)))

      if (!is.na(categories)) {
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
              out <- setDT(out, keep.rownames = TRUE)[]

          } else {
              lastrow <- colnames(out)
              out <- setNames(rbind(out,lastrow), names(out))
              rownames(out)[3] <- "region"
              out <- data.frame(out)
              # out[out=="1000000"]<-"Infinity"
              names(out) <- apply(out, 2, paste0, collapse="_")
              out <- setDT(out, keep.rownames = TRUE)[]
          }
      } else {
          row.names(out) <- c('lower threshold', 'upper threshold')
          firstrow <- colnames(out)
          # https://stackoverflow.com/questions/62960127/convert-column-names-into-first-row-of-data-frame-in-r
          out <- setNames(rbind(firstrow, out), names(out))
          out <- data.frame(out)
                  names(out) <- apply(out, 2, paste0, collapse="_")
          out <- setDT(out, keep.rownames = TRUE)[]
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

  # First by category
  if (!is.na(categories)) {
      category_order <- word(names(table_in), -1)
      category_order <- order(category_order)
      table_in <- table_in[category_order]
  }

  category_order <- word(names(thresholds_strata), 1)
  ############################################################################
  # total observations/population per category
  table_in$Sum_table_in <- rowSums(table_in)

  # Convert to data.table (data.table does not support rownames)
  table_in <- setDT(table_in, keep.rownames = TRUE)[]

  if (is.null(strata_in)) {
      table_in$strata <- reporting_strata_categories$strata
  }

  table_in$strata <- reporting_strata_categories$strata

  is_all_na <- function(x)all(is.na(x))
  # Add lists of frequencies

  ###############################################
  if (!is.na(categories)) {
  frequency_table <- table_in %>%
    pivot_longer(cols = -c(rn, strata, Sum_table_in),
    names_to = c("lower", "upper", "direction"),
    names_pattern = "\\[(\\d+),(\\d+)[\\)\\]]\\s+(\\S+$)",
      values_drop_na = TRUE) %>% 
    type.convert(as.is = TRUE) %>% 
    group_by(rn, direction) %>%
    filter(lower%in%strata[[1]] & upper %in% strata[[1]]) %>%
    group_by(upper,.add = TRUE) %>%   
    summarise(freq = sum(value), .groups = 'drop_last') %>% 
    group_modify(~add_row(.,freq = sum(.$freq))) %>% group_by(rn) %>%
    summarise(freq = list(freq), .groups = "drop")
  } else {
  frequency_table <- table_in %>%
    pivot_longer(-c(rn, strata)) %>%
    tidyr::extract(name, c('lower', 'upper', 'rest'), '(\\d+),(\\d+)[\\]\\)]\\s*(\\w*)', convert = TRUE)  %>% 
    select(-where(is_all_na)) %>%
    group_by(rn) %>%
    filter(lower%in%strata[[1]] & upper %in% strata[[1]]) %>%
    group_by(upper,.add = TRUE) %>%
    summarise(freq = sum(value), .groups = 'drop_last') %>%
    group_modify(~add_row(.,freq = sum(.$freq))) %>%
    summarise(freq = list(freq))
  }

  frequency_table$strata <- reporting_strata_categories$strata
  frequency_table$strata_list_for_tabel <- frequency_table$strata
  ##########################################################################################

  # Numerical values for reference
  x <- as.numeric(unique(unlist(thresholds_strata)))
  unique_num_values <- x[!is.na(x)]
  unique_num_values <- sort(unique_num_values)
  total_colspan <- c(unique_num_values, "SUM")
  total_colspan <- gsub(1000000, "Infinity", total_colspan, fixed = T)

  # total_colspan = c(0, 25, 50, 100, 250, 500, 1000, 1500, 3000, "Infinity")

  frequency_table$strata_list_for_tabel <- lapply(frequency_table$strata_list_for_tabel, \(x){
      # x <- append(x, c("SUM"))
      x <- gsub(1000000, "Infinity", x, fixed = T)
      x <- append(x, c("SUM"))
      x
  })

  # Index differences
  l <- lapply(frequency_table$strata_list_for_tabel, \(y) sapply(y, \(x) which(total_colspan == x) - which(y == x)))
  frequency_table$l <- l
  frequency_table$l <- lapply(frequency_table$l, \(x) diff(x) + 1)

  if (length(frequency_table$freq[[1]]) > length(total_colspan)) {
      frequency_table$l <- lapply(frequency_table$l, \(x){
          # Only if enough values in freq
          # Needs to be changed if categories are not four
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

  frequency_table_out <- frequency_table[,c("rn", "freq", "colspan")]

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

  if (!is.na(categories)){
      # Adapt
      all_categories <- gsub("_"," ", names(thresholds_strata))
      all_categories <- data.frame(all_categories)
      all_categories <- setDT(all_categories)
      all_categories_1 <- word(all_categories, -1)
      all_category_order <- order(all_categories_1)
      all_categories <- all_categories[all_category_order]
      thresholds_cat <- setDT(thresholds_cat)[, ..all_category_order]
      thresholds_cat <- moveMeDataTable(thresholds_cat, "rn", "first")
      sum_categories <- vector()
      for (i in seq_along(categories)) {
          sum_categories[i] <- paste0(categories[i], "_sum")
      }
      thresholds_cat[,sum_categories] <- "Sum"
      for (i in seq_along(categories)) {
          thresholds_cat <- moveMeDataTable(thresholds_cat, sum_categories[i], "after", paste0(categories[i], "_3000_1000000"))
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

  # spans <- frequency_table$colspan[[1]] %>%  
  # as_tibble() %>% 
  # mutate(idx = row_number()) %>% 
  # tidyr::uncount(value, .remove = F) %>% 
  # group_by(idx) %>%
  # mutate(pos = 1:n(),
  #         value = ifelse(pos != 1, 0, value)) %>% 
  # ungroup() %>% 
  # select(value) %>% 
  # t

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
  if (!is.na(categories)) {
      flextable_out$body$spans$rows[4:nrow(flextable_out$body$spans$rows),] <- matrix(unlist(spans), ncol = ncol(combined), byrow = TRUE)
  } else {
      flextable_out$body$spans$rows[3:nrow(flextable_out$body$spans$rows),] <- matrix(unlist(spans), ncol = ncol(combined), byrow = TRUE)    
  }
  flextable_out <- align(flextable_out, align = "center", part = "all")
  set_table_properties(flextable_out, layout = "autofit")
  return(flextable_out)
}