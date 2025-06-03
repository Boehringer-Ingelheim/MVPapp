#' @name today_numeric
#' @title Get today's date as YYYYMMDD as seed input
#'
#' @returns A numeric
#' @examples
#' \dontrun{
#' today_numeric()
#' }
#' @importFrom magrittr %>% %<>% %T>% %$%
#' @export
today_numeric  <- function() {
  format(Sys.Date(), "%Y%m%d") %>% as.numeric()
}

#---------------------------------------------------------------------------
#' @name lm_eqn_old
#' @title Linear regression text (not used)
#'
#' @param df A dataframe
#' @param x column name (string) of x-variable
#' @param y column name (string) of y-variable
#'
#' @returns A character string containing the expression of the R2 coefficient
#' for use in ggplot labels or title
#---------------------------------------------------------------------------

lm_eqn_old <- function(df, x, y){
  
  # Special handling of whether df is an object or a character string
  if(is.data.frame(df)) {
    df_plot <- df
    df_name <- deparse(substitute(df)) # Stores name of dataframe as string
  } else {
    df_plot <- get(df) # I.e. removes the quotation marks of the string to get the object
    df_name <- df
  }
  
  string.name <- paste0(y, "~", x)
  m <- lm(as.formula(string.name), df_plot)
  
  if(is.na(coef(m)[2])) {
    eq <- "" # no slope available
  } else {
    if(coef(m)[2] < 0) {
      # eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,
      #                  list(a = format(unname(coef(m)[1]), digits = 2),
      #                       b = format(unname(abs(coef(m)[2])), digits = 2),
      #                       r2 = format(summary(m)$r.squared, digits = 3)))
      
      a <- format(unname(coef(m)[1]), digits = 2)
      b <- format(unname(abs(coef(m)[2])), digits = 2)
      r2 <- format(summary(m)$r.squared, digits = 3)
      
      eq <- paste0("y = ", a, " - ", b, "\u00B7x, r\u00B2 = ", r2) # plotly unicode hack
      
    } else {
      # eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
      #                  list(a = format(unname(coef(m)[1]), digits = 2),
      #                       b = format(unname(coef(m)[2]), digits = 2),
      #                       r2 = format(summary(m)$r.squared, digits = 3)))
      
      a <- format(unname(coef(m)[1]), digits = 2)
      b <- format(unname(coef(m)[2]), digits = 2)
      r2 <- format(summary(m)$r.squared, digits = 3)
      
      eq <- paste0("y = ", a, " + ", b, "\u00B7x, r\u00B2 = ", r2) # plotly unicode hack
    }
    #eq <- as.character(as.expression(eq)) # uncomment for normal ggplot
  }
  
  return(eq)
}

#---------------------------------------------------------------------------
#' @name lm_eqn
#' @title Linear regression text (for plotly)
#'
#' @param df A dataframe
#' @param facet_name Optional column name (string) to facet by
#' @param x column name (string) of x-variable
#' @param y column name (string) of y-variable
#'
#' @returns A dataframe containing the column "label" containing the string
#' formula of R2 coefficient for use in ggplot labels or title
#' @importFrom dplyr group_by summarise if_else sym syms case_when n
#' @export
#---------------------------------------------------------------------------

lm_eqn <- function(df, facet_name, x, y) {
  if(!is.null(facet_name) && facet_name[1] != "") {
    df_stats <- df %>%
      dplyr::group_by(!!!dplyr::syms(facet_name))
  } else {
    df_stats <- df
  }
  
  string_name <- paste0(y, "~", x)
  
  if(x == y) { # workaround for when same variable is used for both y and x,
    # which returns an NA slope. It seems summarise can't handle this well
    df_stats <- df_stats %>%
      dplyr::summarise(rsq = 1,
                       slope = 1,
                       intercept = 0)
  } else {
    
    df_stats <- df_stats %>%
      dplyr::summarise(
        rsq = if(any(!is.na(!!dplyr::sym(y)))) { # safeguard for when there are no valid y-values to calculate lm
          summary(lm(as.formula(string_name)))$r.squared
        } else {
          NA_real_
        },
        slope = if(any(!is.na(!!dplyr::sym(y)))) {
          coef(lm(as.formula(string_name)))[2]
        } else {
          NA_real_
        },
        intercept = if(any(!is.na(!!dplyr::sym(y)))) {
          coef(lm(as.formula(string_name)))[1]
        } else {
          NA_real_
        }
      )
  }
  
  df_stats <- df_stats %>%
    dplyr::mutate(slope_direction = dplyr::if_else(slope >= 0, " + ", " - "),
                  label = dplyr::case_when(
                    is.na(intercept) ~ "", # this means no non-NA data in this group at all
                    !is.na(slope) ~
                      paste0("y = ",
                             format(unname(intercept), digits = 2),
                             slope_direction,
                             format(unname(abs(slope)), digits = 2),
                             "\u00B7x, R\u00B2 = ",
                             format(unname(rsq), digits = 3)),
                    is.na(slope) ~ # if there's no slope, display only the intercept
                      paste0("y = ", format(unname(intercept), digits = 2), " [mean]")
                  )
    )
  
  return(df_stats)
}

#-------------------------------------------------------------------------------
#' @name add_linear_regression_formula
#' @title Add Linear Regression Formula (not currently used)
#' @description
#' A function that adds linear regression formula and place it as a text annotation
#' on the top of a plot. Requires ggplot object as input.
#' @param p ggplot object
#'
#' @returns A ggplot object with the formula placed on the top
#' @importFrom ggplot2 ggplot_build annotate
#' @export
#-------------------------------------------------------------------------------
add_linear_regression_formula <- function(p) {
  data <- ggplot2::ggplot_build(p)$data[[1]]
  med_x <- median(data$x, na.rm = TRUE)
  max_y <- max(data$y, na.rm = TRUE)
  
  p + ggplot2::annotate("text",
                        x = med_x,
                        y = max_y,
                        label = lm_eqn_old(data, "x", "y"),
                        parse = FALSE, # TRUE for ggplots, FALSE for plotly
                        hjust = 0,
                        vjust = 1)
}

#-------------------------------------------------------------------------------
#' @name generate_log_breaks
#' @title Generating log breaks and axis labels, mainly for plotly
#'
#' @param base_values the base value(s) to apply the exponent to
#' @param start the starting exponent
#' @param end the last exponent
#'
#' @returns a numeric
#' @export
#-------------------------------------------------------------------------------

generate_log_breaks <- function(base_values, start, end) {
  powers      <- seq(from = start, to = end)
  breaks      <- rep(0, length(base_values) * length(powers))
  
  for (i in seq_along(powers)) {
    breaks[((i - 1) * length(base_values) + 1):(i * length(base_values))] <- base_values * 10^powers[i]
  }
  
  return(breaks)
}

#' @export
logbreaks_y <- generate_log_breaks(c(1,3), -10, 10) %>% signif(digits = 2)
#' @export
logbreaks_x <- logbreaks_y
#' @export
logbreaks_y_log10 <- generate_log_breaks(c(1), -10, 10) %>% signif(digits = 2)
#' @export
logbreaks_x_log10 <- logbreaks_y_log10
#' @export
logbreaks_y_minor <- generate_log_breaks(c(1:9), -10, 10) %>% signif(digits = 2)
#' @export
logbreaks_x_minor <- logbreaks_y_minor

#' @export
log10_axis_label <- rep("", length(logbreaks_y_minor))
#' @export
log10_axis_label[seq(1, length(logbreaks_y_minor), 9)] <- as.character(logbreaks_y_minor)[seq(1, length(logbreaks_y_minor), 9)] # every 9th tick is labelled


#-------------------------------------------------------------------------------
#' @name do_data_page_plot
#' @title Quick Plot for Data Exploration
#' 
#' @description
#' This is the main function for plotting uploaded datasets. It is designed to be
#' flexible enough to handle continuous/continuous, discrete/continuous, and 
#' discrete/discrete type of data to cover most use cases.
#' 
#'
#' @param nmd The NONMEM dataset for plotting (requires ID, TIME, DV at minimum)
#' @param filter_cmt Filter by this CMT
#' @param x_axis X-axis for plot
#' @param y_axis Y-axis for plot
#' @param color_by Color by this column
#' @param med_line When TRUE, will draw median line by equidistant X-axis bins
#' @param med_line_by Column name to draw median line by
#' @param boxplot Draws a geom_boxplot instead of geom_point and geom_line
#' @param num_quantiles Converts a continuous x-axis into discrete number of quantiles
#' @param dolm Insert linear regression with formula on top of plot
#' @param smoother Insert smoother
#' @param facet_name variable(s) to facet by
#' @param logy Log Y-axis
#' @param lby  Log breaks for Y-axis
#' @param logx Log X-axis
#' @param lbx  Log breaks for X-axis
#' @param plot_title Optional plot title
#' @param label_size font size for geom_text labels (N=x for boxplots or linear regressions)
#' @param discrete_threshold Draws geom_count if there are <= this number of unique Y-values
#' @param boxplot_x_threshold Throws an error when the unique values of x-axis exceeds this number
#' @param error_text_color error text color for element text
#' @param debug show debugging messages
#'
#' @returns a ggplot object
#' @importFrom ggplot2 ggplot aes geom_point geom_line xlab ylab theme_bw labs scale_size_area
#' @importFrom ggplot2 stat_summary stat_smooth scale_y_log10 scale_x_log10 after_stat geom_count
#' @importFrom ggplot2 annotation_logticks ggtitle theme facet_wrap geom_boxplot label_both vars
#' @importFrom ggplot2 scale_color_manual
#' @importFrom scales hue_pal
#' @importFrom dplyr filter distinct sym group_by summarise across all_of count ungroup mutate
#' @importFrom tibble glimpse
#' @export
#-------------------------------------------------------------------------------

do_data_page_plot <- function(nmd,
                              filter_cmt,
                              x_axis,
                              y_axis,
                              color_by,
                              med_line,
                              med_line_by,
                              boxplot,
                              num_quantiles,
                              dolm,
                              smoother,
                              facet_name,
                              logy,
                              lby = logbreaks_y,
                              lbx = logbreaks_x,
                              logx,
                              plot_title,
                              label_size = 3,
                              discrete_threshold = 7,
                              boxplot_x_threshold = 20,
                              error_text_color = "#F8766D",
                              debug = FALSE) {
  if(debug) {
    message(paste0("Creating data_page_plot"))
  }
  
  nmd <- nmd %>% dplyr::ungroup() # Safeguard to always ungroup() the data
  
  x_label     <- x_axis
  x_axis_orig <- x_axis
  can_quantize<- FALSE
  
  if(num_quantiles > 0) {
    if(is.numeric(nmd[[x_axis_orig]])) {
      boxplot <- TRUE # all conditions met to quantize and use box plot
      can_quantize <- TRUE
      nmd_q  <- calculate_quantiles(df = nmd, xvar = x_axis_orig, num_quantiles = num_quantiles)
      if(debug) {print(knitr::kable(nmd_q))}
      nmd    <- categorize_xvar(df = nmd, quantiles_df = nmd_q, xvar = x_axis_orig)
      if(debug) {print(knitr::kable(nmd %>% count(Quantile)))}
      
      # Replace x_axis argument with the newly created "QUANTILES"
      x_label  <- paste0(x_axis, " Quantiles")
      x_axis   <- "Quantile" # Replaced original x_axis
      
    } else {
      shiny::showNotification(paste0("ERROR: Cannot quantize ", x_axis_orig, " as it is not a continuous variable"), type = "error", duration = 10)
    }
  } # Automatically convert plot into a box plot
  
  if(filter_cmt != 'NULL') {
    nmd <- nmd %>% dplyr::filter(CMT %in% filter_cmt)
  }
  
  if ('EVID' %in% names(nmd)) {
    nmd <- nmd %>% dplyr::filter(EVID == 0)
    shiny::showNotification(paste0("Dosing rows (EVID >= 1) are excluded from the general plot."), type = "message", duration = 10)
  }
  
  if(debug) {
    message(paste0("Testing for blanks"))
  }
  
  # Safeguard for blanks ("") since plotly has bugs with handling it
  nmd <- handle_blanks(nmd, y_axis)
  nmd <- handle_blanks(nmd, x_axis)

  if (!is.null(facet_name) && facet_name[1] != "") {
    for (facet in facet_name) {
      if (facet != x_axis) {
        nmd <- handle_blanks(nmd, facet)
      }
    }
  }

  if(color_by != "") {
    
    if(can_quantize & color_by == x_axis_orig) {
      color_by <- "Quantile" # Replaced original color_by
    }
    
    if(all(is.na(nmd[[color_by]]))) {
      shiny::showNotification(paste0("WARNING: All values are NA in ", color_by, ". No coloring performed."), type = "warning", duration = 10)
    } else {
      # Safeguard for blanks ("") since plotly has bugs with handling it
      # Only applicable if the column is character type as int columns with NAs will fail
      nmd <- handle_blanks(nmd, color_by)
      nmd[[color_by]] <- as.factor(nmd[[color_by]])
      
      # Create a named vector of colors - this is required to be consistent with ind plots if some pages don't have all factor levels during color_by
      n <- nlevels(nmd[[color_by]])
      color_map <- scales::hue_pal()(n)
      named_color_vector <- setNames(color_map, levels(nmd[[color_by]]))
    }
  }
  
  if(color_by != "" && !boxplot & !all(is.na(nmd[[color_by]]))) {
    a <- ggplot2::ggplot(data = nmd, ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]], group = ID, color = !!dplyr::sym(color_by))) +
      ggplot2::scale_color_manual(values = named_color_vector)
  } else {
    a <- ggplot2::ggplot(data = nmd, ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]], group = ID))
  }
  
  if(boxplot) {
    if(length(unique(nmd[[x_axis]])) > boxplot_x_threshold) {
      a <- ggplot2::ggplot() +
        ggplot2::labs(title = paste0('ERROR: There are too many X-axis categories (>', boxplot_x_threshold, ') for boxplots.\nTry the "Quantize X-axis" option instead.')) +
        ggplot2::theme(panel.background = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(color = error_text_color))
      return(a)
    }
    if(num_quantiles == 0) {
      nmd[[x_axis]] <- as.factor(nmd[[x_axis]])
    }
    
    if(is.character(nmd[[y_axis]]) || length(unique(nmd[[y_axis]])) <= discrete_threshold ) { # ... or if there are <= discrete_threshold unique values of y-axis
      shiny::showNotification(paste0("WARNING: Treating Y-axis as discrete as it is a character type, or there are <=", discrete_threshold ," unique Y values."), type = "warning", duration = 10)
      treat_y_axis_as_discrete <- TRUE 
      nmd[[y_axis]] <- as.factor(nmd[[y_axis]])
    } else {
      treat_y_axis_as_discrete <- FALSE
      nmd[[y_axis]] <- as.numeric(nmd[[y_axis]])
    }
    
    ### Trim data to retain unique IDs or not, dependent on quantize
    if(!can_quantize) {
      nmd <- nmd %>% dplyr::distinct(ID, .keep_all = TRUE)
    }
    
    if(color_by != "" && !all(is.na(nmd[[color_by]]))) {
      a <- ggplot2::ggplot(data = nmd, ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]], color = !!dplyr::sym(color_by))) +
        ggplot2::scale_color_manual(values = named_color_vector)
    } else {
      a <- ggplot2::ggplot(data = nmd, ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]]))
    }
    
    # Calculate the number of observations for each category
    df_count <- nmd 
    
    if(!is.null(facet_name[1]) && facet_name[1] != "") {
      if(!(x_axis %in% facet_name)) {
        if(treat_y_axis_as_discrete) {
          df_count <- df_count %>%
            dplyr::count(!!dplyr::sym(x_axis), !!dplyr::sym(y_axis), !!!dplyr::syms(facet_name))      
        } else {
          df_count <- df_count %>%
            dplyr::count(!!dplyr::sym(x_axis), !!!dplyr::syms(facet_name))
        }
      } else {
        shiny::showNotification("ERROR: Facet variable cannot be the same as X-axis.", type = "error", duration = 10)
        if(treat_y_axis_as_discrete) {
          df_count <- df_count %>%
            dplyr::count(!!dplyr::sym(x_axis), !!dplyr::sym(y_axis))          
        } else {
          df_count <- df_count %>%
            dplyr::count(!!dplyr::sym(x_axis))
        }
      }
    } else { ## end of valid facet_name
      
      if(treat_y_axis_as_discrete) {
        df_count <- df_count %>%
          dplyr::count(!!dplyr::sym(x_axis), !!dplyr::sym(y_axis))        
      } else {
        df_count <- df_count %>%
          dplyr::count(!!dplyr::sym(x_axis)) 
      }
    }
    
    if(treat_y_axis_as_discrete) {
      a <- a + 
        ggplot2::geom_count() + ggplot2::scale_size_area(max_size = 12)
      
      if(label_size > 0) {
        a <- a +
          ggplot2::geom_text(data = df_count, aes(x = .data[[x_axis]], y = .data[[y_axis]], label = paste0(n), size = n, group = NULL), color = "black", vjust = 0.5, size = label_size) 
      }
      
    } else {
      a <- a + 
        ggplot2::geom_boxplot(varwidth = TRUE)
      if(label_size > 0) {
        if(can_quantize) { # For quantized plots, we are plotting all observations, not unique IDs
          a <- a +
            ggplot2::geom_text(data = df_count, aes(x = .data[[x_axis]], y = max(nmd[[y_axis]], na.rm = TRUE) * 1.02, label = paste0("Nobs=", n),  group = NULL), color = "black", vjust = 2, size = label_size)          
        } else {
          a <- a +
            ggplot2::geom_text(data = df_count, aes(x = .data[[x_axis]], y = max(nmd[[y_axis]], na.rm = TRUE) * 1.02, label = paste0("N=", n),  group = NULL), color = "black", vjust = 2, size = label_size)
        }
      }
    }
    
  } else { # end of boxplot check
    a <- a + ggplot2::geom_point(alpha = 0.2) +
      ggplot2::geom_line(alpha = 0.2)
  }
  
  a <- a + ggplot2::xlab(x_label) + # Using x_label to adapt name when Quantize
    ggplot2::ylab(y_axis) +
    ggplot2::theme_bw() +
    ggplot2::labs(color = color_by)
  
  if(med_line & is.numeric(nmd[[x_axis]]) & is.numeric(nmd[[y_axis]]) & !boxplot) { # can only do median line when both x & y are numeric
    
    nmd <- nmd %>%
      dplyr::mutate(
        binned_xvar = quantize(nmd[[x_axis]], levels = get_bin_times(nmd[[x_axis]], bin_num = 20, relative_threshold = 0.05))
      )
    
    if(med_line_by == "") {
      a <- a + ggplot2::stat_summary(data = nmd, ggplot2::aes(x = binned_xvar, y = .data[[y_axis]], group = NULL), fun = median, geom="line", colour = "black", alpha = 1.0)
    } else { # end of stat_summary_data_by NULL check
      a <- a + ggplot2::stat_summary(data = nmd, ggplot2::aes(x = binned_xvar, y = .data[[y_axis]], group = NULL, color = as.factor(.data[[med_line_by]])),
                                     fun = median, geom="line", alpha = 1.0)
    }
    
  } # end of stat_summary_data_option
  
  if(smoother & is.numeric(nmd[[x_axis]]) & is.numeric(nmd[[y_axis]]) & !boxplot) {
    a <- a + ggplot2::stat_smooth(ggplot2::aes(group = NULL), se = FALSE, linetype = "dashed")
  }
  
  if(dolm & is.numeric(nmd[[x_axis]]) & is.numeric(nmd[[y_axis]]) & !boxplot) {
    # Calculate linear regression and R-squared value for each facet
    df_stats <- lm_eqn(df = nmd, facet_name = facet_name, x = x_axis, y = y_axis)
    
    data <- ggplot2::ggplot_build(a)$data[[1]]
    med_x <- (min(data$x, na.rm = TRUE) + max(data$x, na.rm = TRUE))/2 # median works better for plotly, while min is better for ggplot
    max_y <- max(data$y, na.rm = TRUE)
    
    a <- a + ggplot2::stat_smooth(ggplot2::aes(group = NULL), method = "lm", formula = y ~ x, se = FALSE, colour = "grey", show.legend = FALSE)
    if(label_size > 0) {
      a <- a + ggplot2::geom_text(data = df_stats, aes(label = label, x = med_x, y = max_y, group = NULL, color = NULL),
                                  hjust = 0.5, vjust = 1, show.legend = FALSE, size = label_size)      
    }
  }
  
  if (!is.null(facet_name[1]) && facet_name[1] != "") {
    if(x_axis %in% facet_name) {
      shiny::showNotification("ERROR: Facet variable cannot be the same as X-axis.", type = "error", duration = 10)
    } else {
      num_facets <- prod(sapply(facet_name, function(v) length(unique(nmd[[v]]))))
      if(num_facets > 30 ) {
        shiny::showNotification("ERROR: Too many facets (>30) found. Please filter further or choose another variable(s).", type = "error", duration = 10)
      } else {
        facet_formula <- as.formula(paste0("~", paste(facet_name, collapse = "+")))
        a <- a + ggplot2::facet_wrap(facet_formula, labeller = ggplot2::label_both)
      }
    }
  }
  
  if (logy & is.numeric(nmd[[y_axis]])) {
    a <- a +
      ggplot2::scale_y_log10(breaks = logbreaks_y, labels = logbreaks_y) +
      ggplot2::annotation_logticks(sides = "l")
  }
  
  if (logx & is.numeric(nmd[[x_axis]]) & !boxplot) {
    a <- a +
      ggplot2::scale_x_log10(breaks = logbreaks_x, labels = logbreaks_x) +
      ggplot2::annotation_logticks(sides = "b")
  }
  
  if (!is.null(plot_title)) {
    a <- a +
      ggplot2::ggtitle(plot_title)
  }
  
  return(a)
}

#-------------------------------------------------------------------------------
#' @name do_data_page_ind_plot
#' @title Individual Plot for Data Exploration
#' 
#' @description
#' This is the main function for plotting individual plots from uploaded datasets.
#' It is designed to be flexible enough to handle continuous/continuous, discrete/continuous, and 
#' discrete/discrete type of data to cover most use cases.
#'
#' @param nmd The NONMEM dataset for plotting (requires ID, TIME, DV at minimum)
#' @param rownums How many rows per page
#' @param colnums How many cols per page
#' @param filter_id Filter by these IDs
#' @param filter_cmt Filter by this CMT
#' @param sort_by Sort by one or more variables (a list)
#' @param strat_by Stratify by 1 variable and highlight outliers
#' @param highlight_range Flag potential outliers when they are higher or below this mean value of the group, a string
#' @param x_axis X-axis for plot
#' @param y_axis Y-axis for plot
#' @param color_by Color by this column
#' @param med_line When TRUE, will draw median line by equidistant X-axis bins
#' @param med_line_by Column name to draw median line by
#' @param boxplot Draws a geom_boxplot instead of geom_point and geom_line
#' @param dolm Insert linear regression with formula on top of plot
#' @param smoother Insert smoother
#' @param facet_name variable to facet by
#' @param logy Log Y-axis
#' @param lby  Log breaks for Y-axis
#' @param logx Log X-axis
#' @param lbx  Log breaks for X-axis
#' @param plot_title Optional plot title
#' @param label_size font size for geom_text labels (N=x for boxplots or linear regressions)
#' @param discrete_threshold Draws geom_count if there are <= this number of unique Y-values
#' @param boxplot_x_threshold Throws an error when the unique values of x-axis exceeds this number
#' @param error_text_color error text color for element text
#' @param highlight_var variable name to be highlighted by a different shape
#' @param highlight_var_values variable values associated with highlight_var to be highlighted 
#' @param plot_dosing Plots dosing line and dose text when TRUE
#' @param same_scale Uses fixed scales based on entire dataset when TRUE
#' @param dose_col name of dose column, usually AMT or DOSE
#' @param dose_units (Optional) name of dose units, usually mg or nmol
#' @param lloq_name (Optional) Supply name of lloq column to be plotted as hline
#' @param debug show debugging messages
#'
#' @returns a ggplot object
#' @importFrom ggplot2 ggplot aes geom_point geom_line xlab ylab theme_bw labs scale_size_area
#' @importFrom ggplot2 stat_summary stat_smooth scale_y_log10 scale_x_log10 after_stat geom_count
#' @importFrom ggplot2 annotation_logticks ggtitle theme facet_wrap geom_boxplot label_both
#' @importFrom ggplot2 scale_color_manual geom_rect coord_cartesian scale_fill_manual
#' @importFrom scales hue_pal
#' @importFrom dplyr filter distinct sym syms group_by summarise across all_of count ungroup mutate any_of case_when rowwise
#' @importFrom tibble glimpse
#' @importFrom forcats fct_inorder
#' @importFrom ggrepel geom_text_repel
#' @importFrom purrr map_chr
#' @export
#-------------------------------------------------------------------------------

do_data_page_ind_plot <- function(nmd,
                                  rownums,
                                  colnums,
                                  pagenum = 1,
                                  filter_id,
                                  filter_cmt,
                                  sort_by,
                                  strat_by,
                                  highlight_range,
                                  x_axis,
                                  y_axis,
                                  color_by,
                                  med_line,
                                  med_line_by,
                                  boxplot,
                                  dolm,
                                  smoother,
                                  facet_name = "ID",
                                  logy,
                                  lby = logbreaks_y,
                                  lbx = logbreaks_x,
                                  logx,
                                  plot_title,
                                  label_size = 3,
                                  discrete_threshold = 7,
                                  boxplot_x_threshold = 20,
                                  error_text_color = "#F8766D",
                                  highlight_var,
                                  highlight_var_values,
                                  plot_dosing = TRUE,
                                  same_scale = FALSE,
                                  dose_col = "DOSE",
                                  dose_units,
                                  lloq_name = '',
                                  debug = FALSE) {
  if(debug) {
    message(paste0("Creating data_page_ind_plot"))
  }
  
  nmd <- nmd %>% dplyr::ungroup() # Safeguard to always ungroup() the data
  
  if(filter_cmt != 'NULL') {
    nmd <- nmd %>% dplyr::filter(CMT %in% filter_cmt)
  }
  
  # Check if any columns are selected for sorting, otherwise uses ID as default
  if (length(sort_by) > 0) {
    # Dynamically create the labeling column (facet_label) based on sort_by
    nmd <- create_facet_label(df = nmd, sort_by = sort_by)
  } else {
    nmd <- nmd %>% dplyr::arrange(ID) %>%
      mutate(facet_label = paste0("ID: ", ID))
  }
  
  # Calculate outliers
  if(!is.null(strat_by) && strat_by != '' && !boxplot) {
    nmd <- categorize_outliers(df              = nmd,
                               highlight_range = highlight_range,
                               y_axis          = y_axis,
                               strat_by        = strat_by,
                               debug           = debug)
  } # end of calculate outliers
  
  # Create a new factor for ID with levels in the order they appear in the sorted data frame
  nmd$facet_label <- factor(nmd$facet_label, levels = unique(nmd$facet_label))
  
  if(color_by != "") {
    if(all(is.na(nmd[[color_by]]))) {
      shiny::showNotification(paste0("WARNING: All values are NA in ", color_by, ". No coloring performed."), type = "warning", duration = 10)
    } else {
      # Safeguard for blanks ("") since plotly has bugs with handling it
      # Only applicable if the column is character type as int columns with NAs will fail
      nmd <- handle_blanks(nmd, color_by)
      nmd[[color_by]] <- as.factor(nmd[[color_by]])
      
      # Create a named vector of colors - this is required to be consistent with ind plots if some pages don't have all factor levels during color_by
      n <- nlevels(nmd[[color_by]])
      color_map <- scales::hue_pal()(n)
      named_color_vector <- setNames(color_map, levels(nmd[[color_by]]))
    }
  }
  
  # Safeguard for blanks ("") since plotly has bugs with handling it
  nmd <- handle_blanks(nmd, y_axis)
  nmd <- handle_blanks(nmd, x_axis)
  
  # Calculating the entire dataset's limits to ensure consistency across pages for same_scale
  if(!is.character(nmd[[x_axis]])) {
    nmdx <- nmd %>%
      filter(!!dplyr::sym(x_axis) != 0) 
  } else {
    nmdx <- nmd
  }
  if(!is.character(nmd[[y_axis]])) {
    nmdy <- nmd %>%
      filter(!!dplyr::sym(y_axis) != 0) 
  } else {
    nmdy <- nmd
  }
  min_data_x_all  <- min(as.numeric(as.character(nmdx[[x_axis]])), na.rm = TRUE)
  max_data_x_all  <- max(as.numeric(as.character(nmdx[[x_axis]])), na.rm = TRUE)
  min_data_y_all  <- min(as.numeric(as.character(nmdy[[y_axis]])), na.rm = TRUE)
  max_data_y_all  <- max(as.numeric(as.character(nmdy[[y_axis]])), na.rm = TRUE)
  
  ## Handling doses
  if(plot_dosing && 'EVID' %in% names(nmd) & !boxplot & dose_col != "") {
    # Creating dummy variables - XVAR, YVAR, NAMT
    # This is done so we don't have to deal with calling strings
    nmd$NAMT <- as.numeric(as.character(nmd[[dose_col]]))
    nmd$XVAR <- as.numeric(as.character(nmd[[x_axis]]))
    nmd$YVAR <- as.numeric(as.character(nmd[[y_axis]]))
    max_dose_all <- max(nmd$NAMT, na.rm = TRUE)
    
    # Scaling the dose such that it is plotted nicely with the maximum dose at height dose_height of the maximum y variable
    # A new column SAMT (Scaled AMT) is created which will be used for plotting the geom_rect
    nmd <- nmd %>%
      group_by(facet_label) %>% # group_by(ID)
      mutate(
        max_dose       = max(NAMT, na.rm = TRUE),
        min_xvar       = round(min(XVAR, na.rm = TRUE), digits = 3),
        max_xvar       = round(max(XVAR, na.rm = TRUE), digits = 3),
        min_yvar       = min(YVAR, na.rm = TRUE),
        max_yvar       = max(YVAR, na.rm = TRUE)) %>%
      ungroup()
    
    # Safeguard for when a subject has no valid Y- or X-values
    # Replaces any bad IDs with no observations to NA
    nmd[(nmd==Inf | nmd == -Inf)] <- NA
    
    dose_height     <- 0.5 # Changing how tall the doses should be in relation to the y variable (0.5 means max dose reaches middle of y_axis range)
    
    nmd <- nmd %>%
      mutate(
        max_dose_all = max(NAMT, na.rm = TRUE),
        min_data_y   = min(YVAR, na.rm = TRUE),
        max_data_y   = max(YVAR, na.rm = TRUE),
        min_data_x   = min(XVAR, na.rm = TRUE),
        max_data_x   = max(XVAR, na.rm = TRUE),
        min_yvar     = dplyr::case_when(is.na(min_yvar) ~ min_data_y, # Replace NA values in min_yvar and max_yvar to use the population's values
                                        TRUE            ~ min_yvar),
        max_yvar     = dplyr::case_when(is.na(max_yvar) ~ max_data_y,
                                        TRUE            ~ max_yvar),
        min_xvar     = dplyr::case_when(is.na(min_xvar) ~ min_data_x, # Replace NA values in min_xvar and max_xvar to use the population's values
                                        TRUE            ~ min_xvar),
        max_xvar     = dplyr::case_when(is.na(max_xvar) ~ max_data_x,
                                        TRUE            ~ max_xvar)
      )
    
    if(same_scale) {
      nmd$scaling_factor <- max_data_y_all / max_dose_all * dose_height
    } else {
      nmd$scaling_factor <- ((nmd$max_yvar - nmd$min_yvar) * dose_height + nmd$min_yvar) / nmd$max_dose #nmd$max_dose_all doesn't work well
    }
    
    nmd$SAMT           <- nmd$NAMT * nmd$scaling_factor
    
  }
  
  if(same_scale) { # Replace each subject's max and min values with the entire dataset's
    
    nmd <- nmd %>%
      mutate(min_xvar = min_data_x_all,
             max_xvar = max_data_x_all,
             min_yvar = min_data_y_all,
             max_yvar = max_data_y_all
      )
  }
  
  # Filter the data to be page Z, where each page has X rows * Y cols
  if(is.null(filter_id[1]) || filter_id[1] == '') { # selectizeInput with multiple choices are picky
    unique_ids         <- unique(nmd$ID)
    length_unique_ids  <- length(unique_ids)
    number_of_pages    <- ceiling(length_unique_ids / (rownums * colnums))
    ids_this_page      <- unique_ids[((rownums * colnums) * (pagenum - 1) + 1):((rownums * colnums) * pagenum)]
    nmd <- nmd %>% dplyr::filter(ID %in% ids_this_page)
  } else {
    nmd <- nmd %>% dplyr::filter(ID %in% filter_id)
  }
  
  
  if(plot_dosing && 'EVID' %in% names(nmd) & !boxplot & dose_col != "") {
    id_dose_expand <- nmd %>%
      filter(EVID == 1 | EVID == 4)
    
    if(nrow(id_dose_expand) >= 1) { # Create "DOSETIME" column if there are any valid dosing rows, which will be used for plotting dose lines
      id_dose_expand <- id_dose_expand %>%
        expand_addl_ii(., x_axis = x_axis, dose_col = dose_col) 
    }
    
    if(debug) {
      message("Dosing expanded:")
      dplyr::glimpse(id_dose_expand)
    }
    
    # Gets unique dose amount rows
    #id_dose_unique <- id_dose_expand %>% distinct(ID, !!dplyr::sym(dose_col), .keep_all = TRUE)
    id_dose_unique <- id_dose_expand %>% distinct(facet_label, !!dplyr::sym(dose_col), .keep_all = TRUE)
    id_dose_unique[[dose_col]] <- as.numeric(as.character(id_dose_unique[[dose_col]])) # in case dose_col is picked for Color by
    id_dose_unique <- id_dose_unique %>% mutate(dosename = paste0(round(.[[dose_col]], digits = 2), dose_units))
    
  }
  
  # Retain EVID == 0 for plotting
  if('EVID' %in% colnames(nmd)) {
    nmd <- nmd %>% filter(EVID == 0)
  }
  
  # Start of ggplot initialization
  if(color_by != "" && !boxplot & !all(is.na(nmd[[color_by]]))) {
    a <- ggplot2::ggplot(data = nmd, ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]], group = ID, color = !!dplyr::sym(color_by))) +
      ggplot2::scale_color_manual(values = named_color_vector)
    
  } else {
    a <- ggplot2::ggplot(data = nmd, ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]], group = ID))
    
  }
  
  if(boxplot) {
    if(length(unique(nmd[[x_axis]])) > boxplot_x_threshold) {
      a <- ggplot2::ggplot() +
        ggplot2::labs(title = paste0('ERROR: There are too many X-axis categories (>', boxplot_x_threshold, ') for boxplots.')) +
        ggplot2::theme(panel.background = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(color = error_text_color))
      return(a)
    }
    nmd[[x_axis]] <- as.factor(nmd[[x_axis]])
    
    if(is.character(nmd[[y_axis]]) || length(unique(nmd[[y_axis]])) <= discrete_threshold ) { # ... or if there are <= discrete_threshold unique values of y-axis
      shiny::showNotification(paste0("WARNING: Treating Y-axis as discrete as it is a character type, or there are <=", discrete_threshold ," unique Y values."), type = "warning", duration = 10)
      treat_y_axis_as_discrete <- TRUE 
      nmd[[y_axis]] <- as.factor(nmd[[y_axis]])
    } else {
      treat_y_axis_as_discrete <- FALSE
      nmd[[y_axis]] <- as.numeric(nmd[[y_axis]])
    }
    
    if(color_by != "" && !all(is.na(nmd[[color_by]]))) {
      a <- ggplot2::ggplot(data = nmd %>% dplyr::distinct(ID, .keep_all = TRUE), ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]], color = !!dplyr::sym(color_by))) +
        ggplot2::scale_color_manual(values = named_color_vector)
    } else {
      a <- ggplot2::ggplot(data = nmd %>% dplyr::distinct(ID, .keep_all = TRUE), ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]]))
    }
    
    # Calculate the number of observations for each category
    df_count <- nmd %>%
      dplyr::distinct(ID, .keep_all = TRUE)
    
    if(facet_name != "") {
      if(facet_name != x_axis) {
        if(treat_y_axis_as_discrete) {
          df_count <- df_count %>%
            dplyr::count(!!dplyr::sym(x_axis), !!dplyr::sym(y_axis), !!dplyr::sym(facet_name))      
        } else {
          df_count <- df_count %>%
            dplyr::count(!!dplyr::sym(x_axis), !!dplyr::sym(facet_name))
        }
      } else {
        shiny::showNotification("ERROR: Facet variable cannot be the same as X-axis.", type = "error", duration = 10)
        if(treat_y_axis_as_discrete) {
          df_count <- df_count %>%
            dplyr::count(!!dplyr::sym(x_axis), !!dplyr::sym(y_axis))          
        } else {
          df_count <- df_count %>%
            dplyr::count(!!dplyr::sym(x_axis))
        }
      }
    } else { ## end of valid facet_name
      
      if(treat_y_axis_as_discrete) {
        df_count <- df_count %>%
          dplyr::count(!!dplyr::sym(x_axis), !!dplyr::sym(y_axis))        
      } else {
        df_count <- df_count %>%
          dplyr::count(!!dplyr::sym(x_axis)) 
      }
    }
    
    if(treat_y_axis_as_discrete) {
      a <- a + 
        ggplot2::geom_count() + ggplot2::scale_size_area(max_size = 12)
      
      if(label_size > 0) {
        a <- a +
          ggplot2::geom_text(data = df_count, aes(x = .data[[x_axis]], y = .data[[y_axis]], label = paste0(n), size = n, group = NULL), color = "black", vjust = 2, size = label_size) 
      }
      
    } else {
      a <- a + 
        ggplot2::geom_boxplot(varwidth = TRUE)
      if(label_size > 0) {
        a <- a +
          ggplot2::geom_text(data = df_count, aes(x = .data[[x_axis]], y = max(nmd[[y_axis]], na.rm = TRUE) * 1.02, label = paste0("N=", n),  group = NULL), color = "black", vjust = 2, size = label_size)
      }
    }
    
  } else { # end of boxplot check
    
    ## Dosing lines
    if(plot_dosing && ("EVID" %in% colnames(nmd)) & dose_col != "" & !boxplot) {
      if('DOSETIME' %in% colnames(id_dose_expand)) { # If DOSETIME is not present that means expansion has failed - then don't do anything
        id_dose_expand$INFDUR <- id_dose_expand$DOSETIME # Infusion Duration is the same as dose time unless rate is supplied
        
        # Extends geom_rect by infusion rates
        if("RATE" %in% colnames(id_dose_expand)) {
          if(any(subset(id_dose_expand, !is.na(RATE))$RATE > 0)) {
            # Safeguard if RATE is negative or NA
            id_dose_expand <- id_dose_expand %>%
              mutate(
                INFDUR = dplyr::case_when(
                  !is.na(RATE) & RATE > 0 ~ DOSETIME + (NAMT / RATE),
                  TRUE                    ~ DOSETIME
                )
              )
          }
        }
        
        linecolour <- "#ED5C42A0"
        
        a <- a +
          ggplot2::geom_vline(data = id_dose_expand, aes(xintercept = DOSETIME), alpha = 0.1) +
          ggplot2::geom_rect(data   = id_dose_expand, aes(
            y = NULL,
            xmin = DOSETIME,
            xmax = INFDUR,
            ymin = min_yvar,
            ymax = SAMT,
            group = ID),
            fill = linecolour,
            color = linecolour,
            show.legend = FALSE
          )
        
        ## Dosing text
        if(require(ggrepel)) {
          a <- a + ggrepel::geom_text_repel(data = id_dose_unique, aes(x = .data[[x_axis]], y = SAMT, label = dosename, color = NULL),
                                            alpha = 0.8,
                                            size = label_size)
        } else {
          a <- a + ggplot2::geom_text(data = id_dose_unique, aes(x = .data[[x_axis]], y = SAMT, label = dosename, group = NULL, color = NULL),
                                      hjust = 1, vjust = 1, show.legend = FALSE, size = label_size, alpha = 0.8)
        }
      } # End of DOSETIME column check
    } # End of dosing lines
    
    ### LLOQ
    
    # LLOQ line
    if(lloq_name != '' && lloq_name %in% colnames(nmd)) {
      nmd[[lloq_name]] <- as.numeric(as.character(nmd[[lloq_name]]))
      a <- a + ggplot2::geom_hline(yintercept = unique(nmd[[lloq_name]]), colour = "orange", linetype = "dashed", size = 0.5, alpha = 0.5)
    }
    
    a <- a + ggplot2::geom_point(data = nmd, alpha = 1) +
      ggplot2::geom_line(data = nmd, alpha = 0.7)
    
    # Highlighting variable value(s)
    if(!is.null(highlight_var) && highlight_var != "" && !is.null(highlight_var_values[1]) && highlight_var_values[1] != "") {
      nmd_highlight <- nmd %>% dplyr::filter(!!dplyr::sym(highlight_var) %in% highlight_var_values)
      a <- a +
        geom_point(data = nmd_highlight, shape = 8, size = 4, alpha = 1, color = "red") # big red asterix shape
    }
    
  }
  
  a <- a + ggplot2::xlab(x_axis) +
    ggplot2::ylab(y_axis) +
    ggplot2::theme_bw() +
    ggplot2::labs(color = color_by)
  
  # median line and smoother not relevant for ind plots
  
  if(dolm & is.numeric(nmd[[x_axis]]) & is.numeric(nmd[[y_axis]]) & !boxplot) {
    # Calculate linear regression and R-squared value for each facet
    df_stats <- lm_eqn(df = nmd, facet_name = facet_name, x = x_axis, y = y_axis)
    
    data <- ggplot2::ggplot_build(a)$data[[1]]
    
    if(same_scale) {
      med_x <- (min_data_x_all + max_data_x_all) / 2
      max_y <- max_data_y_all
    } else {
      med_x <- (min(data$x, na.rm = TRUE) + max(data$x, na.rm = TRUE))/2 # median works better for plotly, while min is better for ggplot
      max_y <- max(data$y, na.rm = TRUE)
    }
    
    a <- a + ggplot2::stat_smooth(ggplot2::aes(group = NULL), method = "lm", formula = y ~ x, se = FALSE, colour = "grey", show.legend = FALSE)
    if(label_size > 0) {
      a <- a + ggplot2::geom_text(data = df_stats, aes(label = label, x = med_x, y = max_y, group = NULL, color = NULL),
                                  hjust = 0.5, vjust = 1, show.legend = FALSE, size = label_size)      
    }
  }
  
  if (facet_name != "") {
    if(facet_name == x_axis) {
      shiny::showNotification("ERROR: Facet variable cannot be the same as X-axis.", type = "error", duration = 10)
    } else {
      #facet_formula <- as.formula(paste0("~", facet_name)) # not used after replaced by "facet_label" column
      if(is.null(filter_id[1]) || filter_id[1] == "") {
        if(same_scale) {
          a <- a + ggplot2::facet_wrap(~facet_label, nrow = rownums, ncol = colnums, scales = "fixed") # labeller = ggplot2::label_both
        } else {
          a <- a + ggplot2::facet_wrap(~facet_label, nrow = rownums, ncol = colnums, scales = "free") # labeller = ggplot2::label_both
        }
      } else {
        if(same_scale) {
          a <- a + ggplot2::facet_wrap(~facet_label, scales = "fixed") # labeller = ggplot2::label_both
        } else {
          a <- a + ggplot2::facet_wrap(~facet_label, scales = "free") # labeller = ggplot2::label_both  
        }
      }
    }
  }
  
  if (logy & is.numeric(nmd[[y_axis]])) {
    a <- a +
      ggplot2::scale_y_log10(breaks = logbreaks_y, labels = logbreaks_y) +
      ggplot2::annotation_logticks(sides = "l")
  }
  
  if (logx & is.numeric(nmd[[x_axis]]) & !boxplot) {
    a <- a +
      ggplot2::scale_x_log10(breaks = logbreaks_x, labels = logbreaks_x) +
      ggplot2::annotation_logticks(sides = "b")
  }
  
  if(same_scale) { ## coord_cartesian not well supported with log scales?
    
    if(plot_dosing && ("EVID" %in% colnames(nmd)) & dose_col != "" & !boxplot) { # Gets the largest of either doses per individual or last observation time
      cc_xlim <- c(min(min_data_x_all, min(id_dose_expand$DOSETIME)),
                   max(max_data_x_all, max(id_dose_expand$DOSETIME)))
    } else {
      cc_xlim <- c(min_data_x_all, max_data_x_all)
    }
    
    cc_ylim <- c(min_data_y_all, # lower bound of y is not 0 to avoid log scale issues
                 max_data_y_all) 
    
    # if(logy & is.numeric(nmd[[y_axis]])) {
    #   cc_ylim <- log10(cc_ylim)
    # }
    # 
    # if(logx & is.numeric(nmd[[x_axis]])) {
    #   cc_xlim <- log10(cc_xlim)
    # }
    
    if(debug) {
      message(paste0("cc_ylim: ", cc_ylim))
      message(paste0("cc_xlim: ", cc_xlim))
    }
    
    a <- a + ggplot2::coord_cartesian(xlim = cc_xlim ,
                                      ylim = cc_ylim)
  } # end of same_scale
  
  if (!is.null(plot_title)) {
    a <- a +
      ggplot2::ggtitle(plot_title)
  }
  
  return(a)
}


#-------------------------------------------------------------------------------
#' @name lowerFn
#'
#' @title Function to support draw_correlation_plot
#'
#' @param data input df for ggplot
#' @param mapping mapping for ggplot
#' @param method plotting method for ggplot (default "lm")
#' @param ... other parameters to pass onto geoms
#'
#' @returns a ggplot object
#' @export
#-------------------------------------------------------------------------------

lowerFn <- function(data, mapping, method = "lm", ...) { ## Plots linear regression of the continuous
  p <- ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = method, color = "black", se = FALSE, ...)
  p
}

#-------------------------------------------------------------------------------
#' @name draw_correlation_plot
#'
#' @title Draw a correlation plot from a NONMEM-formatted dataset
#'
#' @param input_df Input dataframe
#' @param corr_variables Vector (string) of names to be used in the plot
#' @param color_sep Variable (string) name used for colour separator
#' @param catcov_threshold assumes a covariate is categorical if the unique values
#'                         in the covariate are less than this threshold (default 10),
#'                         should be less than nsubj that is available from the dataset
#' @param debug show debugging messages
#'
#' @returns a ggplot object
#' @importFrom dplyr distinct select all_of
#' @importFrom GGally ggpairs wrap
#' @importFrom ggplot2 theme_bw
#' @export
#-------------------------------------------------------------------------------

draw_correlation_plot <- function(input_df,
                                  corr_variables,
                                  color_sep = "",
                                  catcov_threshold = 10,
                                  debug = FALSE) {
  if(debug) {
    message("Creating correlation plot")
  }
  corr_data_id <- input_df %>% dplyr::distinct(ID, .keep_all = TRUE)
  
  if(color_sep %in% names(corr_data_id)) {
    cov_columnsf <- unique(c(corr_variables, color_sep)) # add the colour separator
  } else {
    cov_columnsf <- c(corr_variables)
  }
  
  #corr_data_id_trimmed <- corr_data_id[, cov_columnsf] %>% as.data.frame() # strange error
  corr_data_id_trimmed <- corr_data_id %>% dplyr::select(dplyr::all_of(cov_columnsf)) %>% as.data.frame()
  
  ##### If a column has less than the number of unique values as specified in catcov_threshold, it will automatically be turned into a factor
  for(i in 1:length(corr_data_id_trimmed)) {
    if(length(unique(corr_data_id_trimmed[[i]])) < catcov_threshold) {
      corr_data_id_trimmed[i] <- as.factor(unlist(corr_data_id_trimmed[i]))
    }
  }
  
  mapping <- if(color_sep %in% names(corr_data_id)) {
    ggplot2::aes(color = as.factor(.data[[color_sep]]), alpha = 0.5)
  } else {
    ggplot2::aes(alpha = 0.5)
  }
  
  corr_plot <- GGally::ggpairs(corr_data_id_trimmed, cardinality_threshold = 30,
                               mapping = mapping,
                               lower = list(continuous = GGally::wrap(lowerFn, method = "lm"))) +
    ggplot2::theme_bw()
  
  return(corr_plot)
}

#-------------------------------------------------------------------------------
#' @name safely_qsim
#'
#' @title purrr:safely wrappers for various functions
#' @param ... args to be passed
#'
#' @returns A list with two elements:
#' * `result`: The result of `mrgsolve::qsim`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#'
#' @seealso `mrgsolve::qsim`
#' @export
#-------------------------------------------------------------------------------

safely_qsim <- purrr::safely(mrgsolve::qsim)

#-------------------------------------------------------------------------------
#' @name safely_mrgsim_df
#'
#' @title purrr:safely wrappers for various functions
#' @param ... args to be passed
#'
#' @returns A list with two elements:
#' * `result`: The result of `mrgsolve::mrgsim_df`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#'
#' @seealso `mrgsolve::mrgsim_df`
#' @export
#-------------------------------------------------------------------------------

safely_mrgsim_df <- purrr::safely(mrgsolve::mrgsim_df)

#-------------------------------------------------------------------------------
#' @name run_single_sim
#'
#' @title Executes a mrgsim call based on user input
#'
#' @param input_model_object    mrgmod object
#' @param pred_model            Default FALSE. set to TRUE to set ev_df CMT to 0
#' @param ev_df                 ev() dataframe containing dosing info
#' @param model_dur             Default FALSE, set to TRUE to model duration inside the code
#' @param model_rate            Default FALSE, set to TRUE to model rate inside the code
#' @param sampling_times        A vector of sampling times (note: not a tgrid object)
#' @param seed                  Seed number for RNG for reproducibility
#' @param divide_by             Divide the TIME by this value, used for scaling x-axis
#' @param debug                 Default FALSE, set to TRUE to show more messages in console
#' @param nsubj                 Default 1, in which case mrgsolve::zero_re() will be applied
#' @param append_id_text        A string prefix to be inserted for each ID
#' @param ext_db                Default NULL, supply R object of external database
#' @param parallel_sim          Default TRUE, uses the future and mrgsim.parallel packages !Not implemented live!
#' @param parallel_n            The number of subjects required before parallelization is used !Not implemented live!
#'
#' @returns a df
#'
#' @importFrom dplyr mutate select rename
#' @importFrom data.table merge.data.table fwrite fread
#' @export
#-------------------------------------------------------------------------------

run_single_sim <- function(input_model_object,
                           pred_model         = FALSE,
                           ev_df,
                           model_dur          = FALSE,
                           model_rate         = FALSE,
                           sampling_times,
                           seed               = 1000,
                           divide_by          = 1,
                           debug              = FALSE,
                           nsubj              = 1,
                           append_id_text     = "m1-",
                           ext_db             = NULL,
                           parallel_sim       = FALSE,
                           parallel_n         = 200#,
                           #number_of_cores    = 8L # uses future_mrgsim_d as mc_mrgsim_d doesn't work in Shiny
) {
  
  # Early exit if input_model_object is NULL
  if (is.null(input_model_object)) {
    if (debug) {
      message("input_model_object is NULL")
    }
    return(NULL)
  }
  
  ev_df <- transform_ev_df(ev_df, model_dur, model_rate, pred_model, debug)
  
  ### If reading in Databases:
  if(nsubj > 1 & !is.null(ext_db)) { # Note that ext_db is not NULL even for "None" option

    # Joining ext_db with ev_df
    ev_df2 <- ev_df %>%
      mrgsolve::ev_rep((1:nrow(ext_db)))
    
    ext_db_ev <- data.table::merge.data.table(ev_df2, ext_db, by = "ID", all.x = TRUE)
    
    set.seed(seed) # Setting seed outside mrgsim to ensure reproducibility
    
    # if(parallel_sim & nsubj >= parallel_n) {
    #   shiny::showNotification(paste0("Performing simulations in parallel (N >= ", parallel_n, ")..."), type = "message", duration = 10)
    #   options(mc.cores = number_of_cores)
    #   input_model_object@digits <- 5 # how many sigdigs to output
    #
    #   solved_output <- mrgsim.parallel::mc_mrgsim_d( # error with mclapply
    #     mod       = input_model_object,
    #     data      = ext_db_ev,
    #     nchunk    = number_of_cores,
    #     carry_out = carry_out_cols,
    #     obsonly   = TRUE,
    #     tgrid     = sampling_times,
    #     tad       = TRUE#,
    #     #.parallel = FALSE
    #   )
    #
    # solved_output <- mrgsim.parallel::future_mrgsim_d(  # mc_mrgsim_d doesn't work in Shiny, and future_mrgsim_d is not reliable
    #   mod       = input_model_object,
    #   data      = ext_db_ev,
    #   nchunk    = number_of_cores,
    #   carry_out = carry_out_cols,
    #   obsonly   = TRUE,
    #   tgrid     = sampling_times,
    #   tad       = TRUE
    # ) #%T>%
    #system.time()
    #
    #} else {
    
    input_model_object@digits <- 5 # how many sigdigs to output
    
    solved_output <- safely_qsim(input_model_object,
                              data = ext_db_ev,
                              obsonly = TRUE,
                              tgrid = sampling_times,
                              tad = TRUE,
                              output = "df")
    
    # mrgsim_q / qsim does not support carry_out cols, so merging back in here
    if(is.null(solved_output$error)) {
      solved_output$result <- data.table::merge.data.table(solved_output$result, ext_db, by = "ID", all.x = TRUE)
    } 
    #} # not using parallel
  } # end of multiple nsubj sims
  
  ### If single subject (or bad number of subjects input)
  if(nsubj <= 1) {
    solved_output <- input_model_object %>%
      mrgsolve::obsonly() %>%
      mrgsolve::zero_re() %>%
      safely_mrgsim_df(events = ev_df,
                       tgrid  = sampling_times,
                       tad    = TRUE)
  }
  
  if(is.null(solved_output$error)) {
    solved_output <- solved_output$result %>%
      dplyr::rename(TIME    = time) %>%
      dplyr::mutate(TIMEADJ = TIME / divide_by,
                    ID      = as.factor(paste0(append_id_text, ID))
      )
  } else {
    shiny::showNotification(paste0(solved_output$error, " Potentially due to non-sensible parameter values."), type = "error", duration = 10)
    solved_output <- NULL
  }
    
  return(solved_output) # Successful sims will be returned as df; otherwise a NULL is returned
  
} # end of run_single_sim


#' @name sample_age_wt
#' @title Samples from existing databases
#' @description
#' Sampling from the loaded objects `nhanes.filtered`, `who.expand`, `cdc.expand`,
#' or no sampling at all ("None").
#' @param df_name        Name of databases, either "None", "CDC", "WHO", or "NHANES"
#' @param nsubj          Number of subjects
#' @param lower.agemo    Min Age (months) *not applicable for "None"*
#' @param upper.agemo    Max Age (months) *not applicable for "None"*
#' @param lower.wt       Min WT (kg)      *not applicable for "None"*
#' @param upper.wt       Max WT (kg)      *not applicable for "None"*
#' @param prop.male      Proportion of males (e.g. 0.5)  *not applicable for "None"*
#' @param seed.number    seed number
#' @returns a dataframe with nsubj number of rows
#' @importFrom dplyr slice_sample arrange filter
#' @export


sample_age_wt <- function(df_name     = "None",
                          nsubj       = 20,
                          lower.agemo = 18 * 12,
                          upper.agemo = 65 * 12,
                          lower.wt    = 0,
                          upper.wt    = 100,
                          prop.male   = 0.5,
                          seed.number = 1234) {
  
  set.seed(seed.number)
  
  if(df_name == "None") {
    df.combined <- dplyr::tibble(ID = 1:nsubj)
    return(df.combined)
  }
  
  df <- switch(df_name,
               "CDC" = cdc.expand,
               "WHO" = who.expand,
               "NHANES" = nhanes.filtered
  )
  
  if(upper.agemo > max(df$AGEMO)) {
    stop("Requested upper bound of age exceeds what's available in the database.")
  }
  
  if(lower.agemo < min(df$AGEMO)) {
    stop("Requested lower bound of age exceeds what's available in the database.")
  }
  
  if(lower.agemo == upper.agemo) { # Allows singular age
    df.sexes <- df %>%
      dplyr::filter(AGEMO == lower.agemo)
    
    if(nrow(df.sexes) == 0) { # CDC does not have exact whole months available
      df.sexes <- df %>% # Note we're using inclusive both ends to be more accurate of what the user wants
        dplyr::filter(AGEMO >= (lower.agemo - 0.5), AGEMO <= (upper.agemo + 0.5)) 
    }
  } else {
    df.sexes <- df %>%
      dplyr::filter(AGEMO >= lower.agemo, AGEMO < upper.agemo)
  }
  
  if(lower.wt == upper.wt) { # Allows singular weight
    df.sexes <- df.sexes %>%
      dplyr::filter(WT == lower.wt)
  } else {
    df.sexes <- df.sexes %>%
      dplyr::filter(WT >= lower.wt, WT < upper.wt)
  }
  
  df.boys <- df.sexes %>%
    dplyr::filter(SEX == 0) %>%
    dplyr::slice_sample(n = ceiling(nsubj * prop.male), replace = FALSE)
  
  df.girls <- df.sexes %>%
    dplyr::filter(SEX == 1) %>%
    dplyr::slice_sample(n = ceiling(nsubj * (1 - prop.male)), replace = FALSE)
  
  df.combined <- rbind(df.boys, df.girls) %>%
    dplyr::slice_sample(n = nsubj, replace = FALSE) %>%
    dplyr::arrange(AGEMO) %>%
    dplyr::rename(AGE = AGEYR) %>%
    dplyr::mutate(BMI = round(WT / (HT/100)^2,2)) %>% # Check for non-sensible values
    dplyr::mutate(BSA = round(0.20247 * WT^0.425 * (HT/100)^0.725,2)) # Du Bois formula for BSA, height in m
  
  return(cbind(dplyr::tibble(ID = 1:nsubj), df.combined))
}

#=============================================================================
#' @name calc_summary_stats
#'
#' @title Calculate summary stats with optional rounding
#'
#' @param orig_data           input dataframe
#' @param dp                  round to this many decimal places
#' @param sigdig              Set to TRUE to round using significant digits instead
#' @param convert_to_numeric  Default TRUE, convert df to numeric and turns characters to NA's
#' @param transpose           Set to TRUE to transpose the table such that each variable
#'                      (column) becomes a row
#' @param check_empty_rows    Default TRUE, will not perform summary stats if there are no rows in df
#' @param id_colname          The ID column to distinct by and then removed before summary calcs are done
#' @param comma_format        Set to TRUE to use big mark formatting
#' @param replace_non_numeric_to_NA Set to TRUE to replace non-numeric characters with NA
#'
#' @returns a dataframe with summary stats
#' @importFrom dplyr mutate mutate_all distinct select sym summarise across
#' @importFrom tidyr everything pivot_longer pivot_wider
#' @importFrom purrr modify_if
#' @export
#=============================================================================

calc_summary_stats <- function(orig_data,
                               dp = 1,
                               sigdig = FALSE,
                               convert_to_numeric = TRUE,
                               transpose = FALSE,
                               check_empty_rows = TRUE,
                               id_colname = "ID",
                               comma_format = TRUE,
                               replace_non_numeric_to_NA = TRUE) {
  
  data <- orig_data
  
  if(convert_to_numeric) {
    # Convert all columns to numeric, replacing non-numeric characters with NA
    data <- orig_data %>%
      dplyr::mutate_all(function(x) as.numeric(as.character(x)))
    
    # Replace non-numeric values with NA
    if(replace_non_numeric_to_NA) {
      data[orig_data != data] <- NA # numbers with long decimals are being recognized as NA?
    }
  }
  
  if(id_colname %in% names(data)) {
    data <- data %>% dplyr::distinct(!!dplyr::sym(id_colname), .keep_all = TRUE)
    data <- data %>% dplyr::select(-!!dplyr::sym(id_colname))
  }
  
  if(check_empty_rows & nrow(data) == 0) {
    tmp <- data
  } else {
    
    # Using data.table which is much quicker
    # Convert the data to a data.table
    data <- data.table::setDT(data)
    
    # Define list of metrics to summarise over
    stats_list <- list(
      "Min"       = function(x) min(x, na.rm = TRUE),
      "5%"        = function(x) quantile(x, probs = 0.05, na.rm = TRUE),
      "1st Qu."   = function(x) quantile(x, probs = 0.25, na.rm = TRUE),
      "Median"    = function(x) median(x, na.rm = TRUE),
      "Mean"      = function(x) mean(x, na.rm = TRUE),
      "3rd Qu."   = function(x) quantile(x, probs = 0.75, na.rm = TRUE),
      "95%"       = function(x) quantile(x, probs = 0.95, na.rm = TRUE),
      "Max"       = function(x) max(x, na.rm = TRUE),
      "CV%"       = function(x) sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE) * 100,
      "gMean"     = function(x) gm_mean(x),
      "gMean CV%" = function(x) gm_mean_cv(x)
    )
    
    # Apply each operation to each column
    tmp <- data.table::rbindlist(lapply(names(stats_list), function(name) {
      data[, lapply(.SD, stats_list[[name]]), .SDcols = names(data)][, Statistic := name]
    })) %>%
      as.data.frame() %>%
      dplyr::select(Statistic, dplyr::everything())
    
    # Apply rounding or sigdigs to the entire df
    if(sigdig) {
      tmp <- tmp %>% purrr::modify_if(is.numeric, ~signif(., dp))
    } else {
      tmp <- tmp %>% purrr::modify_if(is.numeric, ~round(., dp))
    }
  }
  
  if("AGEMO" %in% names(tmp)) {
    tmp <- tmp %>% dplyr::select(-AGEMO)
  }
  
  if("SEX" %in% names(tmp)) {
    tmp <- tmp %>% dplyr::select(-SEX)
  }
  
  if(comma_format) {
    # Define a formatter function
    comma_formatter <- function(x) format(x, big.mark = ",")
    
    # Apply the formatter to all numeric columns
    for(col in names(tmp)) {
      if(is.numeric(tmp[[col]])) {
        tmp[[col]] <- comma_formatter(tmp[[col]])
      }
    }
  }
  
  # Convert all columns to character for easier display
  tmp <- tmp %>% dplyr::mutate_all(as.character)
  
  # Convert such that each variable (column) is a row instead
  if(transpose) {
    tmp <- tmp %>%
      tidyr::pivot_longer(cols = -Statistic, names_to = "ColumnName", values_to = "value") %>%
      tidyr::pivot_wider(names_from = "Statistic", values_from = "value")
  }
  
  return(tmp)
}

#=============================================================================
#' @name calc_summary_stats_as_list
#'
#' @title Return summary stats as a list
#'
#' @param nca_df              input dataframe (after using NonCompart::tblNCA)
#' @param group_by_name       the singular "key" argument for NonCompart::tblNCA *minus* the ID column
#' @param list_of_nca_metrics columns to retain in the summary table
#' @param dp                  round to this many decimal places
#' @param sigdig              Set to TRUE to round using significant digits instead
#' @param convert_to_numeric  Default FALSE, convert df to numeric and turns characters to NA's
#' @param transpose           Set to TRUE to transpose the table such that each variable
#'                      (column) becomes a row
#' @param id_colname          The ID column to distinct by and then removed before summary calcs are done
#'
#' @returns a list containing X number of dataframes for each unique value of group_by_name
#' @importFrom dplyr mutate mutate_all distinct select sym rename one_of
#' @importFrom tidyr everything pivot_longer pivot_wider
#' @export
#=============================================================================

calc_summary_stats_as_list <- function(nca_df, group_by_name,
                                       list_of_nca_metrics = c("AUCLST", "AUCIFO", "CMAX", "CMAXD", "LAMZHL", "TMAX",
                                                               "MRTIVLST", "MRTIVIFO", "MRTIVIFP", "MRTEVLST", "MRTEVIFO", "MRTEVIFP",
                                                               "VZO", "VZP", "VZFO", "VZFP",
                                                               "CLO", "CLP", "CLFO", "CLFP"),
                                       dp = 3,
                                       sigdig = TRUE,
                                       convert_to_numeric = FALSE,
                                       transpose = TRUE,
                                       id_colname = "ID") {
  
  list_of_descriptive_stats_by_keys <- list() # initialize a list to store each df
  
  if(group_by_name %in% names(nca_df)) {
    for(i in 1:length(unique(nca_df[[group_by_name]]))) {
      if(transpose) {
        
        list_of_descriptive_stats_by_keys[[i]] <- calc_summary_stats(nca_df %>%
                                                                       dplyr::filter(!!dplyr::sym(group_by_name) == unique(nca_df[[group_by_name]])[i]) %>%
                                                                       dplyr::select(-!!dplyr::sym(group_by_name)),
                                                                     dp = dp,
                                                                     sigdig = sigdig,
                                                                     convert_to_numeric = convert_to_numeric,
                                                                     transpose = TRUE,
                                                                     id_colname = id_colname) %>%
          dplyr::filter(ColumnName != group_by_name) %>%
          dplyr::select(ColumnName, `Min`, `Mean`, `Median`, `Max`, `CV%`, `gMean`, `gMean CV%`) %>%
          dplyr::filter(ColumnName %in% list_of_nca_metrics) %>%
          dplyr::mutate(N = nrow(nca_df %>%
                                   dplyr::filter(!!dplyr::sym(group_by_name) == unique(nca_df[[group_by_name]])[i]))) %>%
          dplyr::select(ColumnName, N, tidyr::everything()) %>%
          dplyr::rename(Metric = ColumnName)
      } else {
        list_of_descriptive_stats_by_keys[[i]] <- calc_summary_stats(nca_df %>%
                                                                       dplyr::filter(!!dplyr::sym(group_by_name) == unique(nca_df[[group_by_name]])[i]) %>%
                                                                       dplyr::select(-!!dplyr::sym(group_by_name)),
                                                                     dp = dp,
                                                                     sigdig = sigdig,
                                                                     convert_to_numeric = convert_to_numeric,
                                                                     transpose = FALSE,
                                                                     id_colname = id_colname) %>%
          dplyr::select(dplyr::one_of(c("Statistic", list_of_nca_metrics))) %>%
          dplyr::mutate(N = nrow(nca_df %>%
                                   dplyr::filter(!!dplyr::sym(group_by_name) == unique(nca_df[[group_by_name]])[i]))) %>%
          dplyr::filter(Statistic %in% c("Min", "Mean", "Median", "Max", "CV%", "gMean", "gMean CV%")) %>%
          dplyr::select(Statistic, N, tidyr::everything())
        
      }
    }
  } else { # if can't find the group_by_name
    if(transpose) {
      list_of_descriptive_stats_by_keys[[1]] <- calc_summary_stats(nca_df,
                                                                   dp = dp,
                                                                   sigdig = sigdig,
                                                                   convert_to_numeric = convert_to_numeric,
                                                                   transpose = TRUE,
                                                                   id_colname = id_colname) %>%
        dplyr::select(ColumnName, `Min`, `Mean`, `Median`, `Max`, `CV%`, `gMean`, `gMean CV%`) %>%
        dplyr::filter(ColumnName %in% list_of_nca_metrics) %>%
        dplyr::mutate(N = nrow(nca_df)) %>%
        dplyr::select(ColumnName, N, tidyr::everything()) %>%
        dplyr::rename(Metric = ColumnName)
    } else {
      list_of_descriptive_stats_by_keys[[1]] <- calc_summary_stats(nca_df,
                                                                   dp = dp,
                                                                   sigdig = sigdig,
                                                                   convert_to_numeric = convert_to_numeric,
                                                                   transpose = FALSE,
                                                                   id_colname = id_colname) %>%
        dplyr::select(dplyr::one_of(c("Statistic", list_of_nca_metrics))) %>%
        dplyr::mutate(N = nrow(nca_df)) %>%
        dplyr::filter(Statistic %in% c("Min", "Mean", "Median", "Max", "CV%", "gMean", "gMean CV%")) %>%
        dplyr::select(Statistic, N, tidyr::everything())
    }
  } # end of group_by_name big loop
  
  return(list_of_descriptive_stats_by_keys)
}

#=============================================================================
#' @name gm_mean
#'
#' @title Geometric mean
#'
#' @param x A numeric
#' @param na.rm Set to TRUE to remove NAs
#'
#' @returns A numeric
#' @export
#=============================================================================

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#=============================================================================
#' @name gm_mean_cv
#'
#' @title Geometric mean CV %
#'
#' @param x A numeric
#' @param na.rm Set to TRUE to remove NAs
#'
#' @returns A numeric
#' @export
#=============================================================================

gm_mean_cv = function(x, na.rm = TRUE) {
  logx <- log(x)
  # Removes NaN, Inf, -Inf resulting from log of 0, negative numbers, +/- infinites
  if(na.rm) {
    logx <- logx[is.finite(logx)]
  }
  sd.logx <- sd(logx)
  gmeancv <- sqrt(exp(sd.logx^2) - 1) * 100 # Check wiki on Coefficient of variation
  return(gmeancv)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name print_demog_plots
#'
#' @title Print demographics plots.
#'
#' @description
#' By default AGE, WT, and SEX are shown.
#'
#'
#' @param data         input dataframe, must contain SEX (Male == 0, Female == 1),
#'               AGE, WT
#'
#' @returns a cowplot ggplot object
#' @importFrom dplyr case_when
#' @importFrom cowplot get_legend plot_grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print_demog_plots <- function(data) {
  
  data <- data %>% dplyr::mutate(SEX = dplyr::case_when(SEX == 0 ~ "Male",
                                                        SEX == 1 ~ "Female"))
  
  p.age <- ggplot2::ggplot(data, ggplot2::aes(x = AGE, fill = SEX)) +
    ggplot2::scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::labs(x = 'Age (years)', y = 'Count')
  
  if(max(data$AGE) < 18) {
    p.age <- p.age +
      ggplot2::geom_histogram(colour= "white", binwidth = 1) + ggplot2::facet_grid(. ~ SEX)
  } else {
    p.age <- p.age +
      ggplot2::geom_histogram(colour= "white", binwidth = 5) + ggplot2::facet_grid(. ~ SEX)
  }
  
  p.age <- p.age +
    ggplot2::theme(legend.position = "none") +  # Remove the legend
    ggplot2::theme(axis.title.y = ggplot2::element_blank())  # Remove the y-axis label
  
  p.wt <- ggplot2::ggplot(data, ggplot2::aes(x = WT, fill = SEX)) +
    ggplot2::scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::labs(x = 'Weight (kg)', y = '')
  
  if(max(data$WT) < 75) {
    p.wt <- p.wt +
      ggplot2::geom_histogram(colour= "white", binwidth = 1) + ggplot2::facet_grid(. ~ SEX)
  } else {
    p.wt <- p.wt +
      ggplot2::geom_histogram(colour= "white", binwidth = 5) + ggplot2::facet_grid(. ~ SEX)
  }
  
  p.wt <- p.wt +
    ggplot2::theme(legend.position = "none") + # Remove the legend
    ggplot2::theme(axis.title.y = ggplot2::element_blank())  # Remove the y-axis label
  
  # Create a separate legend plot with custom colors
  legend_plot <- cowplot::get_legend(
    ggplot2::ggplot(data, ggplot2::aes(x = AGE, fill = SEX)) +
      ggplot2::geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
      ggplot2::scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink"), name = "Sex") +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2, title = NULL))  # Set the number of columns in the legend
  )
  
  # Combine the plots side by side
  plots <- cowplot::plot_grid(p.age, p.wt, ncol = 2)
  
  # Combine the plots and the shared legend using plot_grid()
  if(length(unique(data$SEX)) == 2) {
    combined_plot <- cowplot::plot_grid(
      plots,
      legend_plot,
      ncol = 1,
      rel_heights = c(1, 0.1)  # Adjust the relative heights of the plots and legend
    )
  } else { # Otherwise don't show legend
    combined_plot <- plots
  }
  
  # p.sex <-ggplot2::ggplot(data)+
  #   #add_watermark() +
  #   ggplot2::geom_bar(ggplot2::aes(x=SEX),colour="white",fill='grey40')+
  #   ggplot2::labs(x='Sex', y='Frequency')+
  #   ggplot2::theme_bw(base_size = 14)
  
  # Plotting EGFR - uncomment if required
  # p.egfr <- ggplot2::ggplot(data)+
  #   add_watermark() +
  #   ggplot2::geom_histogram(ggplot2::aes(x=EGFR),colour="white",fill='grey40',boundary=1)+ # boundary is used to nudge the bars
  #   ggplot2::labs(x='eGFR (ml/min/1.73 m2)', y='Frequency')+
  #   ggplot2::theme_bw(base_size = 14) +
  #   ggplot2::scale_x_continuous(lim=c(0,150),breaks=c(0,15,30,60,90,120,150))
  
  return(combined_plot) # requires gridExtra package, deprecated
  
} # End of print_demog_plots function

#-------------------------------------------------------------------------------
#' @name print_cov_plot
#'
#' @title Prints distribution of a custom covariate
#'
#' @param data         input dataframe, must contain SEX (Male == 0, Female == 1),
#'                AGE, WT
#' @param lo_percentile  Default 0.025, (2.5th percentile)
#' @param hi_percentile  Default 0.975, (97.5th percentile)
#' @returns a ggplot object
#' @export
#-------------------------------------------------------------------------------

print_cov_plot <- function(data, lo_percentile = 0.025, hi_percentile = 0.975) {
  x_string <- names(data)[1]
  
  data_median <- round(quantile(data[[x_string]], probs = 0.5), 1)
  data_lo     <- round(quantile(data[[x_string]], probs = lo_percentile), 1)
  data_hi     <- round(quantile(data[[x_string]], probs = hi_percentile), 1)
  
  title <- paste0("Median = ", data_median, ", [95%: ", data_lo, " - ", data_hi, "]")
  
  plot_object <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_string]])) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::geom_histogram(alpha = 0.3, fill = "#FFB600", color = "black") +
    ggplot2::geom_vline(xintercept = data_median, alpha = 0.8) +
    ggplot2::geom_vline(xintercept = data_lo, linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = data_hi, linetype = "dashed", alpha = 0.5) +
    # ggplot2::annotate(x = data_median, y = 0, label = paste0("\n",data_median), geom = "text", lineheight = 0.6, vjust = 1, hjust = 0, size = 3) +
    # ggplot2::annotate(x = data_lo, y = 0, label = paste0("\n",data_lo), geom = "text", lineheight = 0.6, vjust = 1, hjust = 0, size = 3) +
    # ggplot2::annotate(x = data_hi, y = 0, label = paste0("\n",data_hi), geom = "text", lineheight = 0.6, vjust = 1, hjust = 0, size = 3) +
    #ggplot2::geom_histogram(ggplot2::aes(y = ..density..), alpha = 0.4, fill = "black") +
    #geom_density(alpha = 0.1, fill = "red")
    ggplot2::labs(y = "Count",
                  caption = "solid line = median, dashed line = 95% range") +
    ggplot2::ggtitle(title)
  return(plot_object)
}

#-------------------------------------------------------------------------------
#' NOT CURRENTLY USED
#'
#' @name compare_dist_histogram
#'
#' @title Sanity check plots comparing newly created
#'                         distributions vs NONMEM dataset (or databases)
#'
#' @param df         Dataframe of newly created distribution
#' @param variable_name  Name of variable (string) to use for plotting histograms
#' @param variable_label Nice label of variable_name
#' @param lo_percentile  Default 0.025, (2.5th percentile)
#' @param hi_percentile  Default 0.975, (97.5th percentile)
#'
#' @returns a ggplot object
#' @importFrom dplyr case_when mutate filter
#-------------------------------------------------------------------------------

compare_dist_histogram <- function(df, variable_name, variable_label,
                                   lo_percentile = 0.025, hi_percentile = 0.975) {
  
  df <- df %>% dplyr::mutate(SEX = dplyr::case_when(SEX == 0 ~ "Male",
                                                    SEX == 1 ~ "Female"))
  
  df_male   <- df %>% dplyr::filter(SEX == "Male")
  df_female <- df %>% dplyr::filter(SEX == "Female")
  
  df_male_median <- round(quantile(df_male[[variable_name]], probs = 0.5), 1)
  df_male_lo     <- round(quantile(df_male[[variable_name]], probs = lo_percentile), 1)
  df_male_hi     <- round(quantile(df_male[[variable_name]], probs = hi_percentile), 1)
  
  df_female_median <- round(quantile(df_female[[variable_name]], probs = 0.5), 1)
  df_female_lo     <- round(quantile(df_female[[variable_name]], probs = lo_percentile), 1)
  df_female_hi     <- round(quantile(df_female[[variable_name]], probs = hi_percentile), 1)
  
  range_text    <- (hi_percentile - lo_percentile) * 100
  
  plot_histo <- ggplot2::ggplot()+
    #add_watermark() +
    ggplot2::geom_histogram(data = df_male, ggplot2::aes(y=..count../sum(..count..) * 100, x= .data[[variable_name]]), fill='blue', binwidth = 5, alpha = 0.25) +
    ggplot2::geom_histogram(data = df_female, ggplot2::aes(y=..count../sum(..count..) * 100, x= .data[[variable_name]]), fill='red', binwidth = 5, alpha = 0.25) +
    ggplot2::geom_vline(xintercept = df_male_median, color = 'blue', alpha = 0.5) +
    ggplot2::geom_vline(xintercept = df_male_lo, color = 'blue', linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = df_male_hi, color = 'blue', linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = df_female_median, color = 'red', alpha = 0.5) +
    ggplot2::geom_vline(xintercept = df_female_lo, color = 'red', linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = df_female_hi, color = 'red', linetype = "dashed", alpha = 0.5) +
    ggplot2::annotate(x = df_male_median, y = -Inf, label = paste0("\n",df_male_median), geom = "text", color = 'blue', lineheight = 0.6, vjust = 0.8, size = 3) +
    ggplot2::annotate(x = df_male_lo, y = -Inf, label = paste0("\n",df_male_lo), geom = "text", color = 'blue', lineheight = 0.6, vjust = 0.8, size = 3) +
    ggplot2::annotate(x = df_male_hi, y = -Inf, label = paste0("\n",df_male_hi), geom = "text", color = 'blue', lineheight = 0.6, vjust = 0.8, size = 3) +
    ggplot2::annotate(x = df_female_median, y = -Inf, label = paste0("\n",df_female_median), geom = "text", color = 'red', lineheight = 0.6, vjust = 1.5, size = 3) +
    ggplot2::annotate(x = df_female_lo, y = -Inf, label = paste0("\n",df_female_lo), geom = "text", color = 'red', lineheight = 0.6, vjust = 1.5, size = 3) +
    ggplot2::annotate(x = df_female_hi, y = -Inf, label = paste0("\n",df_female_hi), geom = "text", color = 'red', lineheight = 0.6, vjust = 1.5, size = 3) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = variable_label, y='Percentage of Population (%)',
                  caption = paste0('Blue = Male, Red = Female, Solid Line = Median, Dashed Line = ', range_text, '% range')) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(plot.caption = element_text(size = 10))
  
  return(plot_histo)
}

#-------------------------------------------------------------------------------
#' @name extract_matrix
#'
#' @title Function to output a matrix from mrgsolve model object
#'
#' @param input_model_object  mrgsolve model object to extract from
#' @param name_of_matrix      either "omega" or "sigma", will use "omat" or "smat" accordingly
#' @param remove_upper_tri    When TRUE, turns upper triangle of matrix to NA_real_
#' @param remove_zero         In case any of them are 0, don't replace the diagonals
#' @param coerce_to_character Default TRUE, turns all output into a character as a workaround to prevent rounding
#' @param debug               When TRUE, show debugging messages
#'
#' @returns a matrix
#' @export
#-------------------------------------------------------------------------------

extract_matrix <- function(input_model_object,
                           name_of_matrix      = "omega",
                           remove_upper_tri    = TRUE,
                           remove_zero         = TRUE,
                           coerce_to_character = TRUE,
                           debug               = show_debugging_msg) {
  
  if(debug) {
    message("running extract_matrix()")
  }
  
  if(name_of_matrix == "omega") {
    om <- mrgsolve::omat(input_model_object)
    names_list <- as.list(unlist(mrgsolve::labels(om))) # must use mrgsolve::labels
    omsm_matrix <- mrgsolve::collapse_omega(input_model_object, name = "omega") %>%
      mrgsolve::omat() %>%
      mrgsolve::as.matrix() # must use mrgsolve::as.matrix, S4
  }
  
  if(name_of_matrix == "sigma") {
    sm <- mrgsolve::smat(input_model_object)
    names_list <- as.list(unlist(mrgsolve::labels(sm))) # must use mrgsolve::labels
    omsm_matrix <- mrgsolve::collapse_sigma(input_model_object, name = "sigma") %>%
      mrgsolve::smat() %>%
      mrgsolve::as.matrix() # must use mrgsolve::as.matrix, S4
  }
  
  if(debug) {
    print(omsm_matrix)
  }
  
  if(remove_upper_tri) {
    omsm_matrix[upper.tri(omsm_matrix, diag = FALSE)] <- NA_real_
  }
  if(remove_zero) {
    diag_elements <- diag(omsm_matrix) # In case any of them are 0, don't replace the diagonals
    omsm_matrix[omsm_matrix == 0] <- NA_real_
    diag(omsm_matrix) <- diag_elements
  }
  
  if(coerce_to_character) {
    omsm_matrix <- apply(omsm_matrix, c(1, 2), as.character) %>%
      matrix(., nrow = nrow(.), ncol= ncol(.))
  }
  
  colnames(omsm_matrix) <- names_list
  
  if(debug) {
    message("finish extract_matrix()")
  }
  
  return(omsm_matrix)
}

#-------------------------------------------------------------------------------
#' @name reconstruct_matrices
#'
#' @title
#' Converts a matrix object into a list
#' @description
#' Each element in the list corresponds to each omega block, corresponding to provided model object
#'
#' @param input_model_object original model object that contains matrix information
#' @param input_matrix       single collapsed matrix (assumes NA's for upper triangle and non-blocks)
#' @param name_of_matrix     "omega" or "sigma"
#' @param coerce_to_numeric  Default TRUE, will turn all input to numeric
#' @param debug              When TRUE, show debugging messages
#'
#' @note
#' The original model object is needed as the configurations of matrices
#'        are dependent on how $OMEGA and/or $SIGMA is defined in the model code.
#'        It seems like for each repetition of $OMEGA and/or $SIGMA, a new matrix
#'        is created.
#'
#' @returns a list with each element containing a block matrix
#' @export
#-------------------------------------------------------------------------------

reconstruct_matrices <- function(input_model_object,
                                 input_matrix,
                                 name_of_matrix    = "omega",
                                 coerce_to_numeric = TRUE,
                                 debug             = show_debugging_msg) {
  
  if(nrow(input_matrix) == 0) {
    if(debug) {
      message('no input matrix')
    }
    return(input_matrix)
  }
  
  if(debug) {
    message('Reconstructing Matrix')
  }
  
  ## Store how many matrices there are from the model object
  if(name_of_matrix == "omega") {
    number_of_matrices <- length(input_model_object$omega)
  }
  
  if(name_of_matrix == "sigma") {
    number_of_matrices <- length(input_model_object$sigma)
  }
  
  if(coerce_to_numeric) {
    input_matrix <- apply(input_matrix, c(1, 2), as.numeric) %>%
      matrix(., nrow = nrow(.), ncol= ncol(.))
  }
  
  result        <- list()
  start_row_col <- 1
  
  for(n in 1:number_of_matrices) {
    
    if(name_of_matrix == "omega") {
      nrow_current_matrix <- nrow(input_model_object$omega[[n]])
    }
    if(name_of_matrix == "sigma") {
      nrow_current_matrix <- nrow(input_model_object$sigma[[n]])
    }
    
    result[[n]] <- list()
    
    end_row_col <- start_row_col + nrow_current_matrix - 1
    if(debug) {
      message("Current matrix: ", n, ", start row/col: ", start_row_col)
    }
    result[[n]] <- input_matrix[start_row_col:end_row_col, start_row_col:end_row_col]
    
    # Mirror lower triangle to upper triangle
    lower_triangle <- lower.tri(result[[n]])
    result[[n]][!lower_triangle] <- t(result[[n]])[!lower_triangle]
    result[[n]][is.na(result[[n]])] <- 0 # Required for omat() as NA's don't work
    result[[n]] <- result[[n]] %>% as.matrix() # Required for 1x1 matrix otherwise it becomes a numeric
    
    start_row_col <- start_row_col + nrow_current_matrix # advancing to new starting row/col for next matrix
    
  } # end of number of matrix
  if(debug) {
    message('Matrix reconstruction complete')
  }
  return(result)
}

#-------------------------------------------------------------------------------
#' @name update_variability
#'
#' @title Updates variability for a mrgmod object
#'
#' @param input_model_object   original model object to be updated
#' @param input_matrix         list of matrices containing new parameter values
#' @param name_of_matrix       "omega" or "sigma"
#' @param check_validity       Default TRUE, where it checks that the matrix is sensible
#'                       If check fails, the model object is NOT updated
#' @param debug                When TRUE, show debugging messages
#'
#' @returns a mrgmod object
#' @export
#-------------------------------------------------------------------------------

update_variability <- function(input_model_object,
                               input_matrix,
                               name_of_matrix = "omega",
                               check_validity = TRUE,
                               debug          = show_debugging_msg) {
  
  if(check_validity) {
    matrix_is_valid <- check_matrix(input_model_object = input_model_object, input_matrix = input_matrix, debug = debug)
    if(!matrix_is_valid) {
      if(debug) {
        message("Matrix not valid. Returning original input model object")
      }
      return(input_model_object)
    } # model object is not updated if matrix is invalid
  }
  
  if(name_of_matrix == "omega") {
    tmp <- input_model_object %>% mrgsolve::omat(input_matrix)
  }
  
  if(name_of_matrix == "sigma") {
    tmp <- input_model_object %>% mrgsolve::smat(input_matrix)
  }
  
  return(tmp)
}

#-------------------------------------------------------------------------------
#' @name check_matrix
#'
#' @title Checks for validity of matrix
#'
#' @param input_model_object mrgmod object
#' @param input_matrix       a list where each element is a matrix
#' @param check_diagonal     Default TRUE, check that each diagonal must be >= 0
#' @param check_symmetry     Default FALSE, check for symmetry
#' @param debug              Show debugging messages
#' @param display_error      Displays a showNotification message pop-up
#'
#' @returns logical (TRUE/FALSE), where TRUE means pass (valid matrix)
#' @importFrom stringr str_detect
#' @export
#-------------------------------------------------------------------------------

check_matrix <- function(input_model_object,
                         input_matrix,
                         check_diagonal = TRUE,
                         check_symmetry = FALSE,
                         debug = FALSE,
                         display_error = TRUE) {
  #print(input_matrix)
  for(i in 1:length(input_matrix)) {
    
    if(check_diagonal) {
      if(any(diag(input_matrix[[i]]) < 0)) {
        if(display_error) {
          shiny::showNotification("ERROR: Each diagonal (variance) must be >= 0", type = "error", duration = 10)
        }
        return(FALSE)
      }
    } # End of check diagonal
    
    if(any(stringr::str_detect(input_model_object$code, "@correlation"))) {
      check_eigenvalues <- FALSE
      
    } else {
      check_eigenvalues <- TRUE
    }
    
    if(check_eigenvalues) { # check that the matrix must be positive semi-definite assuming '@correlation' is not used in ANY code
      if(debug) {
        message("No @correlation found, proceeding to checking eigenvalues")
      }
      if(any(eigen(input_matrix[[i]])$values < 0)) {
        if(display_error) {
          shiny::showNotification("ERROR: The matrix is not positive semi-definite", type = "error", duration = 10)
        }
        return(FALSE)
      }
    } # End of check eigenvalues
    
    if(check_symmetry) {
      if (!all(identical(input_matrix[[i]], t(input_matrix[[i]])))) {
        return(FALSE)
      }
    } # End of check symmetry
  } # End of each matrix loop
  
  if(debug) {
    message("Check matrix PASS")
  }
  return(TRUE)
}

#-------------------------------------------------------------------------------
#' @name quantile_output
#'
#' @title Creates upper and lower percentiles by TIME for a given dataset
#'
#' @param iiv_sim_input  a dataframe containing TIME column
#' @param yvar           name of column to do the quantile calculation on
#' @param lower_quartile lower quantile for yvar, default 0.025  (2.5%)
#' @param upper_quartile lower quantile for yvar, default 0.0975 (97.5%)
#' @param dp             decimal places for rounding
#'
#' @returns a dataframe
#' @importFrom dplyr group_by mutate ungroup
#' @export
#-------------------------------------------------------------------------------

quantile_output <- function(iiv_sim_input,
                            yvar = 'DV',
                            lower_quartile = 0.025,
                            upper_quartile = 0.975,
                            dp   = 5
) {
  iiv_sim_input <- iiv_sim_input %>%
    dplyr::group_by(TIME) %>%
    dplyr::mutate(median_yvar   = quantile(.data[[yvar]], probs = 0.5) %>% round(digits = dp),
                  mean_yvar     = mean(.data[[yvar]]) %>% round(digits = dp),
                  lower_yvar    = quantile(.data[[yvar]], probs = lower_quartile) %>% round(digits = dp),
                  upper_yvar    = quantile(.data[[yvar]], probs = upper_quartile) %>% round(digits = dp)) %>%
    dplyr::ungroup()
}

#-------------------------------------------------------------------------------
#' @name safely_eval
#'
#' @title purrr:safely wrappers for various functions
#' @param ... args to be passed
#'
#' @returns A list with two elements:
#' * `result`: The result of `eval`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#'
#' @seealso `eval`
#' @export
#-------------------------------------------------------------------------------

safely_eval  <- purrr::safely(eval)

#-------------------------------------------------------------------------------
#' @name safely_parse
#'
#' @title purrr:safely wrappers for various functions
#' @param ... args to be passed
#'
#' @returns A list with two elements:
#' * `result`: The result of `parse`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#'
#' @seealso `parse`
#' @export
#-------------------------------------------------------------------------------

safely_parse <- purrr::safely(parse)

#-------------------------------------------------------------------------------
#' @name eval_parse
#'
#' @title Short-hand function to eval, and then parse a string
#'
#' @param text Some text
#'
#' @returns A list with two elements:
#' * `result`: The result of `eval(parse(text))`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#' @export
#-------------------------------------------------------------------------------

eval_parse <- function(text) {
  eval(parse(text = text))
}

#-------------------------------------------------------------------------------
#' @name pknca_table
#'
#' @title Function to calculate summary statistics from simulated output
#'
#' @param input_simulated_table  a dataframe (usually created from mrgsim)
#' @param output_conc            y variable of interest to perform summary stats calc
#' @param start_time             start time interval for metrics
#' @param end_time               end time interval for metrics
#' @param debug                  show debugging messages
#'
#' @returns a dataframe with additional columns of summary stats
#' @importFrom dplyr mutate if_else ungroup select filter rename first
#' @export
#-------------------------------------------------------------------------------

pknca_table <- function(input_simulated_table,
                        output_conc,
                        start_time = NULL,
                        end_time   = NULL,
                        debug      = FALSE
) {
  
  if(is.null(input_simulated_table)) {
    return(NULL)
  }
  
  reactive_cmin <- paste0(output_conc, '_CMIN_ranged')
  reactive_cmax <- paste0(output_conc, '_CMAX_ranged')
  reactive_cavg <- paste0(output_conc, '_CAVG_ranged')
  reactive_tmax <- paste0(output_conc, '_TMAX_ranged')
  reactive_tmin <- paste0(output_conc, '_TMIN_ranged')
  reactive_cfbpct <- paste0(output_conc, '_CFBPCT_ranged')
  reactive_mcfbpct <- paste0(output_conc, '_MEANCFBPCT_ranged')
  reactive_nadirpct <- paste0(output_conc, '_NADIRPCT_ranged')
  
  zero_tlast_cmin <- paste0(output_conc, '_CMIN_tlast')
  zero_tlast_cmax <- paste0(output_conc, '_CMAX_tlast')
  zero_tlast_cavg <- paste0(output_conc, '_CAVG_tlast')
  zero_tlast_tmax <- paste0(output_conc, '_TMAX_tlast')
  zero_tlast_tmin <- paste0(output_conc, '_TMIN_tlast')
  zero_tlast_cfbpct <- paste0(output_conc, '_CFBPCT_tlast')
  zero_tlast_mcfbpct <- paste0(output_conc, '_MEANCFBPCT_tlast')
  zero_tlast_nadirpct <- paste0(output_conc, '_NADIRPCT_tlast')
  
  input_simulated_table$YVARNAME <- input_simulated_table[[output_conc]]
  
  if (debug) {
    message(head(input_simulated_table))
    message(paste0(max(input_simulated_table$YVARNAME)[1]))
  }
  
  metrics_table <- input_simulated_table %>%
    dplyr::mutate(CMIN = min(YVARNAME, na.rm = TRUE)[1],
                  CMAX = max(YVARNAME, na.rm = TRUE)[1],  ### First element if multiple values found
                  CAVG = mean(YVARNAME, na.rm = TRUE),
                  DVBL = dplyr::first(YVARNAME),
                  CFBPCT = ((YVARNAME - DVBL) / DVBL) * 100,
                  MEANCFBPCT = mean(CFBPCT, na.rm = TRUE), # only accurate if sampling points are equidistant
                  NADIRPCT = min(CFBPCT, na.rm = TRUE)[1]
    )
  
  DVBL_overall <- metrics_table$DVBL[1] # Note: Baseline is always first ever TIME overall
  
  metrics_table <- metrics_table %>%
    dplyr::mutate(TMAX = .$TIME[.$CMAX[1] == YVARNAME][1],
                  TMIN = .$TIME[.$CMIN[1] == YVARNAME][1])
  
  metrics_table  <- metrics_table %>%
    dplyr::mutate(YLAG       = dplyr::lag(YVARNAME ),
                  XLAG       = dplyr::lag(TIME),
                  dYVAR      = (YVARNAME  + YLAG) * (TIME - XLAG) * 0.5, # Area for trapezoid
                  dYVAR      = dplyr::if_else(is.na(dYVAR), 0, dYVAR),
                  AUC_tlast    = sum(dYVAR)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-ID, -tad, -TIMEADJ, -YLAG, -XLAG, -dYVAR, -DVBL)
  
  if (debug) {
    message(start_time)
    message(end_time)
    tmp <- input_simulated_table %>% dplyr::filter(TIME >= start_time)
    message(dplyr::glimpse(tmp))
    tmp2 <- input_simulated_table %>% dplyr::filter(TIME <= start_time)
    message(dplyr::glimpse(tmp2))
  }
  
  ### Repeat metrics for time range # note that the time range is inclusive on both ends
  metrics_table_time <- input_simulated_table %>% dplyr::filter(TIME >= start_time, TIME <= end_time) %>%
    dplyr::mutate(CMIN = min(YVARNAME, na.rm = TRUE)[1],
                  CMAX = max(YVARNAME, na.rm = TRUE)[1], ### First element if multiple values found
                  CAVG = mean(YVARNAME, na.rm = TRUE),
                  CFBPCT = ((YVARNAME - DVBL_overall) / DVBL_overall) * 100, # Note: Baseline is always first ever TIME overall
                  MEANCFBPCT = mean(CFBPCT, na.rm = TRUE), # only accurate if sampling points are equidistant
                  NADIRPCT = min(CFBPCT, na.rm = TRUE)[1]
    )
  
  metrics_table_time <- metrics_table_time %>%
    dplyr::mutate(TMAX = .$TIME[.$CMAX[1] == YVARNAME][1], ### First element if multiple values found
                  TMIN = .$TIME[.$CMIN[1] == YVARNAME][1])
  
  metrics_table_time  <- metrics_table_time %>%
    dplyr::mutate(YLAG          = dplyr::lag(YVARNAME ),
                  XLAG          = dplyr::lag(TIME),
                  dYVAR         = (YVARNAME + YLAG) * (TIME - XLAG) * 0.5, # Area for trapezoid
                  dYVAR         = dplyr::if_else(is.na(dYVAR), 0, dYVAR),
                  AUC_ranged    = sum(dYVAR)) %>%
    dplyr::ungroup()
  
  CMIN_time <-  metrics_table_time[["CMIN"]][1]
  CMAX_time <-  metrics_table_time[["CMAX"]][1]
  CAVG_time <-  metrics_table_time[["CAVG"]][1]
  TMAX_time <-  metrics_table_time[["TMAX"]][1]
  TMIN_time <-  metrics_table_time[["TMIN"]][1]
  AUC_time  <-  metrics_table_time$AUC_ranged[1]
  CFBPCT_time <- dplyr::last(metrics_table_time[["CFBPCT"]])
  MCFBPCT_time <- metrics_table_time[["MEANCFBPCT"]][1]
  NADIRPCT_time <- metrics_table_time[["NADIRPCT"]][1]
  
  metrics_table <- metrics_table %>%
    dplyr::mutate(!!reactive_cmin := CMIN_time,
                  !!reactive_cmax := CMAX_time,
                  !!reactive_cavg := CAVG_time,
                  !!reactive_tmax := TMAX_time,
                  !!reactive_tmin := TMIN_time,
                  AUC_ranged := AUC_time,
                  !!reactive_cfbpct := CFBPCT_time,
                  !!reactive_mcfbpct := MCFBPCT_time,
                  !!reactive_nadirpct := NADIRPCT_time) %>%
    dplyr::rename(!!zero_tlast_cmin := CMIN,
                  !!zero_tlast_cmax := CMAX,
                  !!zero_tlast_cavg := CAVG,
                  !!zero_tlast_tmax := TMAX,
                  !!zero_tlast_tmin := TMIN,
                  !!zero_tlast_cfbpct := CFBPCT,
                  !!zero_tlast_mcfbpct := MEANCFBPCT,
                  !!zero_tlast_nadirpct := NADIRPCT) %>%
    dplyr::select(-YVARNAME)
  
  return(metrics_table)
}

#-------------------------------------------------------------------------------
#' @name update_resistant_popover
#'
#' @title Function that adds on to the an existing bsPopover
#'        such that the tooltip persists after updateSelectInput etc
#' @inheritParams shinyBS::bsPopover
#' @param options other options
#' @returns A popover
#' @export
#-------------------------------------------------------------------------------

update_resistant_popover <- function(id, title, content, placement = "bottom", trigger = "hover", options = NULL){
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options, content)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      var target = document.querySelector('#", id, "');
      var observer = new MutationObserver(function(mutations) {
        setTimeout(function() {
          shinyBS.addTooltip('", id, "', 'popover', ", options, ");
        }, 200);
      });
      observer.observe(target, { childList: true });
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

#-------------------------------------------------------------------------------
#' @name smart_x_axis
#'
#' @title Function to improve default x-axis breaks
#'
#' @param p1               ggplot object
#' @param max_x            Max x-axis value, will be derived from p1 if it is set to NULL
#' @param xvar             x variable string to be used to derive max_x
#' @param xlabel           Will only apply when xlabel is either
#'                   "Time (hours)"
#'                   "Time (weeks)",
#'                   "Time (days)",
#'                   "Time (months)
#' @param debug            Shows debug messages
#'
#' @returns a ggplot object
#' @importFrom dplyr case_when
#' @export
#-------------------------------------------------------------------------------

smart_x_axis <- function(p1,
                         max_x = NULL,
                         xvar = "TIMEADJ",
                         xlabel,
                         debug = FALSE) {
  
  if(is.null(max_x)) {
    # Extract the maximum value of the x-axis - by default this is expanded by 5%,
    # so we have to divide it back to arrive at the true range
    max_x <- ggplot2::ggplot_build(p1)$layout$panel_params[[1]]$x.range[[2]] / 1.05
    #message("Max x is ", max_x)
  }
  
  if(debug) {
    message(paste0("Max X: ", max_x))
  }
  
  if(!is.na(max_x) & is.finite(max_x)) { # prevents app crashing when time_unit input is empty
    
    if(xlabel == "Time (hours)") {
      x_tick_size <- dplyr::case_when(max_x <=  4  ~ 0.5,
                                      max_x <= 12  ~ 1,
                                      max_x <= 24  ~ 2,
                                      max_x <= 48  ~ 4,
                                      max_x <= 72  ~ 12,
                                      max_x <= 96  ~ 12,
                                      max_x <= 168 ~ 24,
                                      max_x <= 336 ~ 48,
                                      max_x <= 480 ~ 48,
                                      max_x <= 2016~ 168,
                                      TRUE         ~ 672)
      
      if(debug) {
        message(paste0("x_tick_size: ", x_tick_size))
      }
      
      p1 <- p1 + ggplot2::scale_x_continuous(breaks = seq(0, max_x + x_tick_size, by = x_tick_size))
    }
    
    if(xlabel == "Time (days)") {
      x_tick_size <- dplyr::case_when(max_x <= 7   ~ 1,
                                      max_x <= 14  ~ 2,
                                      max_x <= 28  ~ 7,
                                      max_x <= 56  ~ 7,
                                      max_x <= 84  ~ 14,
                                      TRUE         ~ 28)
      
      if(debug) {
        message(print(paste0("x_tick_size: ", x_tick_size)))
      }
      
      p1 <- p1 + ggplot2::scale_x_continuous(breaks = seq(0, max_x + x_tick_size, by = x_tick_size))
    }
    
    if(xlabel == "Time (weeks)") {
      x_tick_size <- dplyr::case_when(max_x <= 2  ~ 0.2,
                                      max_x <= 4  ~ 0.5,
                                      max_x <= 16 ~ 1,
                                      max_x <= 32 ~ 2,
                                      max_x <= 52 ~ 4,
                                      TRUE        ~ 12)
      
      if(debug) {
        message(print(paste0("x_tick_size: ", x_tick_size)))
      }
      
      p1 <- p1 + ggplot2::scale_x_continuous(breaks = seq(0, max_x + x_tick_size, by = x_tick_size))
    }
    
    if(xlabel == "Time (months)") {
      x_tick_size <- dplyr::case_when(max_x <= 1  ~ 0.25,
                                      max_x <= 2  ~ 0.25,
                                      max_x <= 4  ~ 0.5,
                                      max_x <= 12 ~ 1,
                                      max_x <= 24 ~ 2,
                                      max_x <= 48 ~ 4,
                                      TRUE        ~ 12)
      
      if(debug) {
        message(print(paste0("x_tick_size: ", x_tick_size)))
      }
      
      p1 <- p1 + ggplot2::scale_x_continuous(breaks = seq(0, max_x + x_tick_size, by = x_tick_size))
    }
  }
  
  return(p1)
}

#-------------------------------------------------------------------------------
#' @name plot_data_with_nm
#'
#' @title Main plot for Simulation tab
#'
#' @param input_dataset1       simulated dataframe for Model 1
#' @param input_dataset2       simulated dataframe for Model 2
#' @param nonmem_dataset       uploaded NONMEM dataset
#' @param color_data_by        Color by a variable in the NONMEM dataset
#' @param xvar                 Name of X-variable to be plotted, string
#' @param yvar                 Name of Y-variable to be plotted, string (Model 1)
#' @param yvar_2               Name of Y-variable to be plotted, string (Model 2)
#' @param log_x_axis,log_x_ticks,log_x_labels,log_y_axis,log_y_ticks,log_y_labels
#'  Log options and axis tick labels
#' @param geom_point_sim_option,geom_point_data_option Plotting sampling points
#' @param stat_summary_data_option Plots median line of dataset
#' @param stat_summary_data_by Inserts median line by variable
#' @param nm_yvar              Name of Y-variable for NONMEM dataset, string
#' @param xlabel               Nice label for X-axis, string
#' @param ylabel               Nice label for Y-axis, string
#' @param debug                When TRUE, displays debugging messages
#' @param title                Title for plot
#' @param line_color_1         Color for Model 1
#' @param line_color_2         Color for Model 2
#'
#' @returns a ggplot object
#' @importFrom dplyr mutate
#' @export
#-------------------------------------------------------------------------------

plot_data_with_nm <- function(
    input_dataset1 = NULL,
    input_dataset2 = NULL,
    nonmem_dataset = NULL,
    color_data_by = NULL,
    xvar = NULL,
    yvar = NULL,
    yvar_2 = NULL,
    log_x_axis = FALSE,
    log_x_ticks = logbreaks_x,
    log_x_labels= logbreaks_x,
    log_y_axis  = FALSE,
    log_y_ticks = logbreaks_y,
    log_y_labels= logbreaks_y,
    geom_point_sim_option = FALSE,
    geom_point_data_option = FALSE,
    stat_summary_data_option = FALSE,
    stat_summary_data_by = NULL,
    nm_yvar = NULL,
    xlabel = xvar,
    ylabel = yvar,
    debug  = FALSE,
    title  = NULL,
    line_color_1 = "#F8766D",
    line_color_2 = "#7570B3") {
  
  if (debug) {
    message("Running plot_data_with_nm()")
  }
  
  if(!is.null(input_dataset1)) {
    p1 <- ggplot2::ggplot(data = input_dataset1, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]]))
  }
  
  if(!is.null(input_dataset2)) {
    p1 <- ggplot2::ggplot(data = input_dataset2, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar_2]]))
  }
  
  if (!is.null(nonmem_dataset)) {
    
    if ('EVID' %in% names(nonmem_dataset)) {
      nonmem_dataset <- nonmem_dataset %>% dplyr::filter(EVID == 0)
    }
    
    if(!is.null(color_data_by) & color_data_by %in% names(nonmem_dataset)) {
      nonmem_dataset[[color_data_by]] <- as.factor(nonmem_dataset[[color_data_by]])
      
      p1 <- p1 +
        ggplot2::geom_line(data = nonmem_dataset, ggplot2::aes(x=.data[[xvar]], y= .data[[nm_yvar]], group = ID, color = .data[[color_data_by]]), alpha = 0.2)
    } else {
      p1 <- p1 +
        ggplot2::geom_line(data = nonmem_dataset, ggplot2::aes(x=.data[[xvar]], y= .data[[nm_yvar]], group = ID), color = 'grey', alpha = 0.2)
    }
    if (geom_point_data_option) {
      if(!is.null(color_data_by) & color_data_by %in% names(nonmem_dataset)) {
        p1 <- p1 +
          ggplot2::geom_point(data = nonmem_dataset, ggplot2::aes(x=.data[[xvar]], y= .data[[nm_yvar]], group = ID, color = .data[[color_data_by]]), alpha = 0.2)
      } else {
        
        p1 <- p1 +
          ggplot2::geom_point(data = nonmem_dataset, ggplot2::aes(x=.data[[xvar]], y= .data[[nm_yvar]], group = ID), color = 'grey', alpha = 0.2)
      }
    }
    
    if(stat_summary_data_option) {
      
      nonmem_dataset <- nonmem_dataset %>%
        dplyr::mutate(
          binned_xvar = quantize(nonmem_dataset[[xvar]], levels = get_bin_times(nonmem_dataset[[xvar]], bin_num = 20, relative_threshold = 0.05))
        )
      
      if(stat_summary_data_by == "") {
        p1 <- p1 + ggplot2::stat_summary(data = nonmem_dataset, ggplot2::aes(x = binned_xvar, y = .data[[nm_yvar]]), fun = median, geom="line", colour = "black", alpha = 0.8)
      } else { # end of stat_summary_data_by NULL check
        p1 <- p1 + ggplot2::stat_summary(data = nonmem_dataset, ggplot2::aes(x = binned_xvar, y = .data[[nm_yvar]], color = as.factor(.data[[stat_summary_data_by]])),
                                         fun = median, geom="line", alpha = 0.8)
      }
    } # end of stat_summary_data_option
  } # end of nonmem_dataset
  
  if(!is.null(input_dataset1)) {
    p1 <- p1 +
      ggplot2::geom_line(data = input_dataset1, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]]), color = line_color_1) 
    
    if (geom_point_sim_option) {
      p1 <- p1 +
        ggplot2::geom_point(data = input_dataset1, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]]),  color = line_color_1)
    }
  }
  
  if(!is.null(input_dataset2)) {
    p1 <- p1 +
      ggplot2::geom_line(data = input_dataset2, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar_2]]), color = line_color_2)
    
    if (geom_point_sim_option) {
      p1 <- p1 +
        ggplot2::geom_point(data = input_dataset2, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar_2]]), color = line_color_2)
    }
  }
  
  p1 <- smart_x_axis(p1, xlabel = xlabel, debug = debug)
  
  ## Apply log axis if required
  if(log_x_axis) {
    p1 <- p1 +
      #ggplot2::scale_x_log10(guide = "axis_logticks")#, labels=log_x_labels) # Doesn't display as nice
      ggplot2::scale_x_log10(breaks=log_x_ticks, labels=log_x_labels) +
      ggplot2::annotation_logticks(sides = "b")
  }
  
  if(log_y_axis) {
    p1 <- p1 +
      #ggplot2::scale_y_log10(guide = "axis_logticks")#, labels=log_y_labels) # Doesn't display as nice
      ggplot2::scale_y_log10(breaks=log_y_ticks, labels=log_y_labels) +
      ggplot2::annotation_logticks(sides = "l")
  }
  
  p1 <- p1 +
    ggplot2::theme_bw() +
    ggplot2::labs(x = xlabel,
                  y = ylabel) +
    ggplot2::ggtitle(title)
  
  if(!is.null(nonmem_dataset) & !is.null(color_data_by) & color_data_by %in% names(nonmem_dataset)) {
    p1 <- p1 +
      ggplot2::theme(legend.position = "right")
  } else {
    p1 <- p1 +
      ggplot2::theme(legend.position = "none")
  }
  
  if (debug) {
    message("plot_data_with_nm() OK")
  }
  
  return(p1)
}

#-------------------------------------------------------------------------------
#' @name plot_three_data_with_nm
#'
#' @title Main function for the plot in Parameter Sensitivity Analysis
#'
#' @param input_dataset_min    simulated dataframe for Min Parameter
#' @param input_dataset_mid    simulated dataframe for Mid Parameter
#' @param input_dataset_max    simulated dataframe for Max Parameter
#' @param param_name           Name of parameter, string
#' @param param_min_value      Min value of parameter, numeric
#' @param param_mid_value      Mid value of parameter, numeric
#' @param param_max_value      Max value of parameter, numeric
#' @param x_min                Min X-axis value for time intervals to filter datasets by
#' @param x_max                Max X-axis value for time intervals to filter datasets by
#' @param nonmem_dataset       uploaded NONMEM dataset
#' @param xvar                 Name of X-variable to be plotted, string
#' @param yvar                 Name of Y-variable to be plotted, string
#' @param log_x_axis,log_x_ticks,log_x_labels,log_y_axis,log_y_ticks,log_y_labels
#'  Log options and axis tick labels
#' @param geom_point_sim_option,geom_point_data_option Plotting sampling points
#' @param stat_summary_data_option Plots median line of dataset
#' @param geom_ribbon_option   Plots AUC when TRUE
#' @param geom_vline_option    Plots time intervals as vertical lines when TRUE
#' @param stat_summary_data_option Plots median line of dataset
#' @param nm_yvar              Name of Y-variable for NONMEM dataset, string
#' @param xlabel               Nice label for X-axis, string
#' @param ylabel               Nice label for Y-axis, string
#' @param debug                When TRUE, displays debugging messages
#' @param title                Title for plot
#'
#' @returns a ggplot object
#' @importFrom dplyr mutate filter bind_rows
#' @importFrom forcats fct_inorder
#' @importFrom ggplot2 theme
#' @export
#-------------------------------------------------------------------------------

plot_three_data_with_nm <- function(
    input_dataset_min,
    input_dataset_mid,
    input_dataset_max,
    param_name = "ID", # dummy value
    param_min_value = -1, # dummy value
    param_mid_value = -2, # dummy value
    param_max_value = -3, # dummy value
    x_min = NULL,
    x_max = NULL,
    nonmem_dataset = NULL,
    xvar = 'TIMEADJ',
    yvar = NULL,
    log_x_axis = FALSE,
    log_x_ticks = logbreaks_x,
    log_x_labels= logbreaks_x,
    log_y_axis = FALSE,
    log_y_ticks = logbreaks_y,
    log_y_labels= logbreaks_y,
    geom_point_sim_option = FALSE,
    geom_point_data_option = FALSE,
    geom_ribbon_option = FALSE,
    geom_vline_option = FALSE,
    stat_summary_data_option = FALSE,
    nm_yvar = NULL,
    xlabel = xvar,
    ylabel = yvar,
    debug  = FALSE,
    title  = NULL
) {
  
  if (debug) {
    message(paste0("x_min: : ", x_min))
    message(paste0("x_max: : ", x_max))
  }
  
  if(is.null(c(input_dataset_min, input_dataset_mid, input_dataset_max))) {
    return(NULL)
  }
  
  ## Mutating a new column programmatically to use the selected parameter name
  if(!is.null(input_dataset_min)) {
    df_min <- input_dataset_min %>% dplyr::mutate(ID = "Min", "{param_name}" := param_min_value)
  } else {
    df_min <- NULL
  }
  
  if(!is.null(input_dataset_mid)) {
    df_mid <- input_dataset_mid %>% dplyr::mutate(ID = "Mid", "{param_name}" := param_mid_value)
  } else {
    df_mid <- NULL
  }
  
  if(!is.null(input_dataset_max)) {
    df_max <- input_dataset_max %>% dplyr::mutate(ID = "Max", "{param_name}" := param_max_value)
  } else {
    df_max <- NULL
  }
  
  ## Combining into a single dataset to easily facilitate legends in ggplot
  combined_input <- dplyr::bind_rows(df_min, df_mid, df_max)
  combined_input[[param_name]] <- as.factor(combined_input[[param_name]]) %>%
    forcats::fct_inorder()
  
  ## assigning maximum x-value for later checks
  max_x_value <- max(combined_input[[xvar]])
  
  # geom_ribbon input
  ribbon_id1 <- dplyr::filter(combined_input, ID == "Min" & .data[[xvar]] >= x_min & .data[[xvar]] <= x_max)
  ribbon_id2 <- dplyr::filter(combined_input, ID == "Mid" & .data[[xvar]] >= x_min & .data[[xvar]] <= x_max)
  ribbon_id3 <- dplyr::filter(combined_input, ID == "Max" & .data[[xvar]] >= x_min & .data[[xvar]] <= x_max)
  
  p1 <- ggplot2::ggplot(data = combined_input, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], group = ID, color = !!dplyr::sym(param_name))) #+
  #add_watermark()
  
  if (!is.null(nonmem_dataset)) {
    if ('EVID' %in% names(nonmem_dataset)) {
      nonmem_dataset <- nonmem_dataset %>% dplyr::filter(EVID == 0)
    }
    
    p1 <- p1 +
      ggplot2::geom_line(data = nonmem_dataset, ggplot2::aes(x= .data[[xvar]], y= .data[[nm_yvar]], group = ID, color = NULL), color = 'grey', alpha = 0.2)
    if (geom_point_data_option) {
      p1 <- p1 +
        ggplot2::geom_point(data = nonmem_dataset, ggplot2::aes(x= .data[[xvar]], y= .data[[nm_yvar]], group = ID, color = NULL), color = 'grey', alpha = 0.2)
    }
    
    if(stat_summary_data_option) { # 2023-07-24 steve
      
      nonmem_dataset <- nonmem_dataset %>%
        dplyr::mutate(
          binned_xvar = quantize(nonmem_dataset[[xvar]], levels = get_bin_times(nonmem_dataset[[xvar]], bin_num = 20, relative_threshold = 0.05))
        )
      
      p1 <- p1 + ggplot2::stat_summary(data = nonmem_dataset, ggplot2::aes(x = binned_xvar, y = .data[[nm_yvar]], group = NULL, color = NULL), geom="line", fun = median, colour = "black", alpha = 0.8)
    } # end of stat_summary_data_option
  } # end of dataset check
  
  if (geom_ribbon_option) {
    p1 <- p1 +
      ggplot2::geom_ribbon(data = ribbon_id1, ggplot2::aes(ymax = .data[[yvar]], ymin = 0), alpha = 0.3, fill = "#1B9E77") +
      ggplot2::geom_ribbon(data = ribbon_id2, ggplot2::aes(ymax = .data[[yvar]], ymin = 0), alpha = 0.4, fill = "#D95F02") +
      ggplot2::geom_ribbon(data = ribbon_id3, ggplot2::aes(ymax = .data[[yvar]], ymin = 0), alpha = 0.5, fill = "#7570B3")
  }
  
  if(geom_vline_option) {
    if(x_min > min(combined_input[["TIMEADJ"]]) | x_max < max_x_value) { # Only plot vlines if the ranges are not default
      p1 <- p1 +
        ggplot2::geom_vline(xintercept = x_min, linetype = "longdash", alpha = 0.3) +
        ggplot2::geom_vline(xintercept = x_max, linetype = "longdash", alpha = 0.3)
    }
  }
  
  p1 <- p1 +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = xlabel,
                  y = ylabel) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    #ggplot2::scale_colour_discrete(name  = param_name) +
    ggplot2::theme(legend.position="bottom")
  
  if (geom_point_sim_option) {
    p1 <- p1 +
      ggplot2::geom_point(alpha = 0.7)
  }

  p1 <- smart_x_axis(p1, xlabel = xlabel) # max_x = max_x_value,
  
  ### Apply log axis if required
  if(log_x_axis) {
    p1<- p1 +
      ggplot2::scale_x_log10(breaks=log_x_ticks, labels=log_x_labels) +
      annotation_logticks(sides = "b")
  }
  
  if(log_y_axis) {
    p1<- p1 +
      ggplot2::scale_y_log10(breaks=log_y_ticks, labels=log_y_labels) +
      annotation_logticks(sides = "l")
  }
  
  # Align legends to be right hand side since plotly always shows right hand side only
  p1 <- p1 +
    #ggplot2::theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.box.just = "right")
    theme(legend.position = "right")
  
  return(p1)
}

#-------------------------------------------------------------------------------
#' @name plot_iiv_data_with_nm
#'
#' @title Function to plot variability data
#'
#' @param input_dataset1       simulated dataframe for Model 1
#' @param input_dataset2       simulated dataframe for Model 2
#' @param nonmem_dataset       uploaded NONMEM dataset
#' @param xvar                 Name of X-variable to be plotted, string
#' @param yvar                 Name of Y-variable to be plotted, string (Model 1)
#' @param yvar_2               Name of Y-variable to be plotted, string (Model 2)
#' @param log_x_axis,log_x_ticks,log_x_labels,log_y_axis,log_y_ticks,log_y_labels
#'  Log options and axis tick labels
#' @param geom_point_sim_option,geom_point_data_option Plotting sampling points
#' @param stat_summary_data_option Plots median line of dataset
#' @param show_ind_profiles    If TRUE, plots individual profiles instead of prediction intervals
#' @param y_median             Prediction interval median (when show_ind_profiles == FALSE)
#' @param y_mean               Prediction interval mean (when show_ind_profiles == FALSE)
#' @param show_y_mean          Show Mean of Y-value in a dashed line
#' @param y_min                Prediction interval min (when show_ind_profiles == FALSE)
#' @param y_max                Prediction interval max (when show_ind_profiles == FALSE)
#' @param line_color_1         Color for Model 1 prediction intervals
#' @param line_color_2         Color for Model 2 prediction intervals
#' @param nm_yvar              Name of Y-variable for NONMEM dataset, string
#' @param xlabel               Nice label for X-axis, string
#' @param ylabel               Nice label for Y-axis, string
#' @param debug                When TRUE, displays debugging messages
#' @param title                Title for plot
#' @param show_x_intercept     Display X-intercept for threshold calculation
#' @param x_intercept_value    Numeric for X-intercept
#' @param show_y_intercept     Display Y-intercept for threshold calculation
#' @param y_intercept_value    Numeric for Y-intercept
#'
#' @returns a ggplot object
#' @importFrom dplyr select mutate all_of
#' @export
#-------------------------------------------------------------------------------

plot_iiv_data_with_nm <- function(
    input_dataset1 = NULL,
    input_dataset2 = NULL,
    nonmem_dataset = NULL,
    xvar = NULL,
    yvar = NULL,
    yvar_2 = NULL,
    log_x_axis = FALSE,
    log_x_ticks = logbreaks_x,
    log_x_labels= logbreaks_x,
    log_y_axis  = FALSE,
    log_y_ticks = logbreaks_y,
    log_y_labels= logbreaks_y,
    geom_point_sim_option = FALSE,
    geom_point_data_option = FALSE,
    show_ind_profiles = FALSE,
    y_median = NULL,
    y_mean = NULL,
    show_y_mean = FALSE,
    y_min = NULL,
    y_max = NULL,
    line_color_1 = 'black',
    line_color_2 = 'black',
    stat_summary_data_option = FALSE,
    nm_yvar = NULL,
    xlabel = xvar,
    ylabel = yvar,
    debug  = FALSE,
    title  = NULL,
    show_x_intercept = FALSE,
    x_intercept_value = NULL,
    show_y_intercept = FALSE,
    y_intercept_value = NULL
) {
  
  if (debug) {
    message("Running plot_iiv_data_with_nm()")
  }
  
  if(!is.null(input_dataset1)) {
    if(debug) {
      message('dataset1 provided')
    }
    
    input_dataset1 <- input_dataset1 %>% dplyr::select(ID, dplyr::all_of(c(xvar, yvar)), median_yvar, mean_yvar, lower_yvar, upper_yvar) # Only include relevant columns to be plotted
    
    p1 <- ggplot2::ggplot(data = input_dataset1, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]]))
  }
  
  if(!is.null(input_dataset2)) {
    if(debug) {
      message('dataset2 provided')
    }
    input_dataset2 <- input_dataset2 %>% dplyr::select(ID, all_of(c(xvar, yvar_2)), median_yvar, mean_yvar, lower_yvar, upper_yvar) # Only include relevant columns to be plotted
    
    p1 <- ggplot2::ggplot(data = input_dataset2, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar_2]]))
  }
  
  if(show_x_intercept) {
    if(!is.null(x_intercept_value) & !is.na(x_intercept_value)) {
      p1 <- p1 +
        ggplot2::geom_vline(xintercept = x_intercept_value, linetype = "longdash", alpha = 0.3)
    }
  }
  
  if(show_y_intercept) {
    if(!is.null(y_intercept_value) & !is.na(y_intercept_value)) {
      p1 <- p1 +
        ggplot2::geom_hline(yintercept = y_intercept_value, linetype = "longdash", alpha = 0.3)
    }
  }
  
  if (!is.null(nonmem_dataset)) {
    if ('EVID' %in% names(nonmem_dataset)) {
      nonmem_dataset <- nonmem_dataset %>% dplyr::filter(EVID == 0)
    }
    nonmem_dataset <- nonmem_dataset %>% dplyr::select(ID, dplyr::all_of(c(xvar, nm_yvar))) # Only include relevant columns to be plotted
    p1 <- p1 +
      ggplot2::geom_line(data = nonmem_dataset, ggplot2::aes(x=.data[[xvar]], y= .data[[nm_yvar]], group = ID), color = 'grey', alpha = 0.2)
    if (geom_point_data_option) {
      p1 <- p1 +
        ggplot2::geom_point(data = nonmem_dataset, ggplot2::aes(x=.data[[xvar]], y= .data[[nm_yvar]], group = ID), color = 'grey', alpha = 0.2)
    }
    
    if(stat_summary_data_option) { # 2023-07-24 steve
      
      nonmem_dataset <- nonmem_dataset %>%
        dplyr::mutate(
          binned_xvar = quantize(nonmem_dataset[[xvar]], levels = get_bin_times(nonmem_dataset[[xvar]], bin_num = 20, relative_threshold = 0.05))
        )
      
      p1 <- p1 + ggplot2::stat_summary(data = nonmem_dataset, ggplot2::aes(x = binned_xvar, y = .data[[nm_yvar]]), fun = median, geom="line", colour = "black", alpha = 0.7)
    } # end of stat_summary_data_option
  } # end of nonmem_dataset
  
  if(!is.null(input_dataset1)) {
    if(!show_ind_profiles) {
      p1 <- p1 +
        ggplot2::geom_ribbon(data = input_dataset1, ggplot2::aes(ymax = .data[[y_max]], ymin = .data[[y_min]]), fill = line_color_1, alpha = 0.4, color = line_color_1) +
        ggplot2::geom_line(data = input_dataset1, ggplot2::aes(x = .data[[xvar]], y = .data[[y_median]]), color = line_color_1, linewidth = 1.2)
    } else {
      p1 <- p1 +
        ggplot2::geom_line(data = input_dataset1, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], color = ID), alpha = 0.4)
    }
    if(show_y_mean) {
      p1 <- p1 +
        ggplot2::geom_line(data = input_dataset1, ggplot2::aes(x = .data[[xvar]], y = .data[[y_mean]]), color = line_color_1, linewidth = 1.2, linetype = "dashed")
    }
  } # end of input_dataset1
  
  if(!is.null(input_dataset2)) {
    if(!show_ind_profiles) {
      p1 <- p1 +
        ggplot2::geom_ribbon(data = input_dataset2, ggplot2::aes(ymax = .data[[y_max]], ymin = .data[[y_min]]), fill = line_color_2, alpha = 0.4, color = line_color_2) +
        ggplot2::geom_line(data = input_dataset2, ggplot2::aes(x = .data[[xvar]], y = .data[[y_median]]), color = line_color_2, linewidth = 1.2)
    } else {
      p1 <- p1 +
        ggplot2::geom_line(data = input_dataset2, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar_2]], color = ID), alpha = 0.4) 
    }
    if(show_y_mean) {
      p1 <- p1 +
        ggplot2::geom_line(data = input_dataset2, ggplot2::aes(x = .data[[xvar]], y = .data[[y_mean]]), color = line_color_2, linewidth = 1.2, linetype = "dashed")
    }
  } # end of input_dataset2
  
  p1 <- smart_x_axis(p1, xlabel = xlabel) # max_x = max_x_value,
  
  ### Apply log axis if required
  if(log_x_axis) {
    p1 <- p1 +
      ggplot2::scale_x_log10(breaks=log_x_ticks, labels=log_x_labels) +
      ggplot2::annotation_logticks(sides = "b")
  }
  
  if(log_y_axis) {
    p1 <- p1 +
      ggplot2::scale_y_log10(breaks=log_y_ticks, labels=log_y_labels) +
      ggplot2::annotation_logticks(sides = "l")
  }
  
  p1 <- p1 +
    ggplot2::theme_bw() +
    ggplot2::labs(x = xlabel,
                  y = ylabel) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(legend.position = "none")
  
  if(debug) {
    message('iiv plot generated')
  }
  
  return(p1)
}

#-------------------------------------------------------------------------------
#' @name extract_model_params
#'
#' @title Function to extract params from model object
#'
#' @param input_model_object Expects a mrgsolve model object
#'
#' @returns a dataframe of all names inside $PARAM where each row contains
#' a parameter name in column 1, and parameter value in column 2
#'
#' @note
#' Should be a tibble of 2 columns and X rows where X = number of params
#' @importFrom tidyr pivot_longer everything
#' @export
#-------------------------------------------------------------------------------

extract_model_params <- function(input_model_object) {
  tmp_df <- as.data.frame(mrgsolve::param(input_model_object)) %>%
    dplyr::as_tibble() %>%
    tidyr::pivot_longer(cols = tidyr::everything())
  return(tmp_df)
}

#-------------------------------------------------------------------------------
#' @name update_model_object
#' @title Function to update model object
#'
#' @param input_model_object  original model object
#' @param input_new_df        dataframe containing new name/value combinations
#'
#' @returns a mrgsolve model object
#' @importFrom tidyr pivot_wider
#' @export
#-------------------------------------------------------------------------------

update_model_object <- function(input_model_object, input_new_df) {
  
  tmp1 <- as.data.frame(input_new_df) %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    as.list()
  
  new_mod <- mrgsolve::param(input_model_object, tmp1)
  
  return(new_mod)
}

#-------------------------------------------------------------------------------
#' @name convert_to_plotly_watermark
#'
#' @title Converts a ggplot object to plotly object and then apply watermark, and other options
#'
#' @param ggplot_object  a ggplot object
#' @param opacity        the alpha or transparency, goes from 0 to 1
#' @param font_name      font name as a string
#' @param format         one of "png", "svg", "jpeg", "webp"
#' @param filename       name of output file when saved
#' @param width          width in pixels. Default NULL - uses current resolution
#' @param height         height in pixels. Default NULL - uses current resolution
#' @param debug          Set to TRUE to show debug messages
#' @param plotly_watermark Set to TRUE to insert "For Internal Use Only"
#'
#' @returns a plotly object
#' @importFrom plotly ggplotly add_annotations config renderPlotly plotlyOutput
#' @export
#-------------------------------------------------------------------------------

convert_to_plotly_watermark <- function(ggplot_object,
                                        # logx        = FALSE,
                                        # logy        = FALSE,
                                        # logticks    = c(1,2,3,4,5,6,7,8,9,10),
                                        # loglabels   = c(1,10),
                                        opacity     = 0.1,
                                        font_name   = "Arial",
                                        format      = "png",
                                        filename    = "newplot",
                                        width       = NULL,
                                        height      = NULL,
                                        debug       = FALSE,
                                        plotly_watermark = TRUE) {
  
  if (debug) {
    message("Converting ggplot object to plotly")
  }
  
  tmp <- plotly::ggplotly(ggplot_object)
  
  tmp <- tmp %>%
    plotly::add_annotations(
      text = dplyr::if_else(plotly_watermark, "For Internal Use Only", ""),
      xref = "paper",
      yref = "paper",
      x = 0.5,
      y = 0.5,
      showarrow = FALSE,
      font = list(family = font_name, size = 58, color = paste0("rgba(0, 0, 0, ", opacity, ")"))
    ) %>%
    plotly::config(toImageButtonOptions = list(format   = format, # one of png, svg, jpeg, webp
                                               filename = filename,
                                               height   = height,
                                               width    = width,
                                               scale    = 1 ),
                   modeBarButtonsToAdd = c('drawopenpath',
                                           'drawline',
                                           'drawcircle',
                                           'drawrect',
                                           'eraseshape'),
                   #modeBarButtonsToRemove = c('lasso2d')) ## For some unknown reason removal of lasso2d breaks the plot
                   displayModeBar = TRUE,
                   displaylogo = FALSE) #,
  #scrollZoom = TRUE,
  
  if (debug) {
    message("Converting ggplot object to plotly successful.")
  }
  
  return(tmp)
}

#=============================================================================
#' @name quantize
#'
#' @title Used for dividing bins for median line plots
#' @param x vector of values (e.g. x-axis)
#' @param levels bins
#' @param ... extra params to pass through to cut()
#' @export
#=============================================================================

quantize <- function (x, levels, ...) {
  stopifnot(!anyNA(levels), is.numeric(levels), is.numeric(x))
  midpoints <- (head(levels, -1) + tail(levels, -1))/2
  breaks <- c(-Inf, midpoints, Inf)
  idx <- cut(x, breaks, labels = FALSE, ...)
  levels[idx]
}

#=============================================================================
#' @name sanitize_numeric_input
#'
#' @title expects numeric as input, and cleans it
#'
#' @description
#' 1. If value is empty (NA) and allow_zero = TRUE, will return 0
#' 2. If value is empty (NA) and allow_zero = FALSE, will return 1
#' 3. If value is <= 0 and allow_zero = TRUE, will return 0
#' 4. If value is <= 0 and allow_zero = FALSE, will return 1
#' 5. If a legal maximum or minimum is provided and the value falls outside of
#'    those ranges, the legal value will be used instead.
#' 6. If a return_value has been specified, any time conditions 1-5 is triggered, the
#'    return_value will be used instead. (therefore the return_value MUST make sense)
#'
#' In all other cases the value is returned as-is.
#'
#' @param numeric_input  a numeric e.g. from numericInput
#' @param allow_zero     Default TRUE, turns the numeric to 0 if input is NA or <= 0, otherwise 1
#' @param as_integer     Default FALSE, coerce input into integer
#' @param legal_minimum  Lower bound numeric that is accepted
#' @param legal_maximum  Upper bound numeric that is accepted
#' @param return_value   If the input is bad, return value X. Default NULL i.e.
#'                       returns 0 (if allow_zero = TRUE) or 1 (if allow_zero = FALSE)
#' @param display_error  Displays showNotification message box
#' @returns a numeric
#' @export
#=============================================================================

sanitize_numeric_input <- function(numeric_input,
                                   allow_zero = TRUE,
                                   as_integer = FALSE,
                                   return_value = NULL,
                                   legal_maximum = NULL,
                                   legal_minimum = NULL,
                                   display_error = FALSE) {
  
  sanitized_input <- as.numeric(numeric_input)
  
  if(as_integer) {
    sanitized_input <- as.integer(sanitized_input)
  }
  
  # Handling NA's
  if(is.na(sanitized_input)) {
    if(allow_zero) {
      sanitized_input <- 0
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      if(display_error) {shiny::showNotification(paste0("WARNING: Bad numeric input. Trying ", sanitized_input, " instead."), type = "warning", duration = 5)}
      return(sanitized_input)
    } else {
      sanitized_input <- 1
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      if(display_error) {shiny::showNotification(paste0("WARNING: Bad numeric input. Trying ", sanitized_input, " instead."), type = "warning", duration = 5)}
      return(sanitized_input)
    }
  }
  
  # Handling zeroes
  if(sanitized_input == 0) {
    if(allow_zero) {
      return(sanitized_input)
    } else {
      sanitized_input <- 1
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      if(display_error) {shiny::showNotification(paste0("WARNING: Bad numeric input. Trying ", sanitized_input, " instead."), type = "warning", duration = 5)}
      return(sanitized_input)
    }
  }
  
  # Handling negative numbers
  if(sanitized_input < 0) {
    if(allow_zero) {
      sanitized_input <- 0
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      if(display_error) {shiny::showNotification(paste0("WARNING: Bad numeric input. Trying ", sanitized_input, " instead."), type = "warning", duration = 5)}
      return(sanitized_input)
    } else {
      sanitized_input <- 1
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      if(display_error) {shiny::showNotification(paste0("WARNING: Bad numeric input. Trying ", sanitized_input, " instead."), type = "warning", duration = 5)}
      return(sanitized_input)
    }
  }
  
  if(!is.null(legal_maximum)) {
    if(sanitized_input > legal_maximum) {
      if(display_error) {
        shiny::showNotification(paste0("WARNING: Bad numeric input (exceeded legal maximum). Trying ", legal_maximum, " instead."), type = "warning", duration = 5)
      }
      sanitized_input <- legal_maximum
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      return(sanitized_input)
    }
  }
  
  if(!is.null(legal_minimum)) {
    if(sanitized_input < legal_minimum) {
      if(display_error) {
        shiny::showNotification(paste0("WARNING: Bad numeric input (below legal minimum). Trying ", legal_minimum, " instead."), type = "warning", duration = 5)
      }
      sanitized_input <- legal_minimum
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      return(sanitized_input)
    }
  }
  
  return(sanitized_input)
}

#-------------------------------------------------------------------------------
#' @name create_value_box
#' @title Short-hand for creating NCA metrics valueBoxes
#'
#' @param input_dataset       dataset to extract metrics from
#' @param name_ends_with      name of metrics column that ends with x (string)
#' @param value_box_subtitle  name of subtitle in value box
#' @param width               width of valueBox
#' @param color               color of valueBox
#' @param sigdig              significant digits to output
#' @param dp                  if TRUE, uses decimal rounding instead
#' @returns a shinydashboard valueBox
#' @importFrom shinydashboard valueBox box
#' @export
#-------------------------------------------------------------------------------

create_value_box <- function(input_dataset, name_ends_with, value_box_subtitle, width = infoBox_width, color, sigdig = 4, dp = FALSE) {
  
  if(is.null(input_dataset)) {
    metric_value <- NA_real_
  } else {
    metric_value <- input_dataset %>%
      dplyr::select(dplyr::ends_with(name_ends_with)) %>%
      unique()  # %>%
  }
  
  if(dp) {
    metric_value <- metric_value %>% round(digits = as.integer(sigdig))
  } else {
    metric_value <- metric_value %>% signif(digits = as.integer(sigdig))
  }
  
  created_box <- shinydashboard::valueBox(
    value = tags$p(style = font_size, metric_value),
    subtitle = value_box_subtitle,
    width = width,
    color = color,
    href = NULL)
  return(created_box)
}

#-------------------------------------------------------------------------------
#' @name check_and_combine_df
#'
#' @title combined 2 datasets for download
#' @param model_1_is_valid  logical TRUE/FALSE
#' @param model_2_is_valid  logical TRUE/FALSE
#' @param input_df_1        Model 1 simulated data
#' @param input_df_2        Model 2 simulated data
#'
#' @returns a df
#' @importFrom dplyr intersect full_join
#' @export
#-------------------------------------------------------------------------------

check_and_combine_df <- function(model_1_is_valid,
                                 model_2_is_valid,
                                 input_df_1 = NULL,
                                 input_df_2 = NULL) {
  
  if(model_1_is_valid & model_2_is_valid) {
    
    common_columns <- dplyr::intersect(names(input_df_1), names(input_df_2))
    combined_model <- dplyr::full_join(input_df_1, input_df_2, by = common_columns)
    
    return(combined_model)
    
  } else if (model_1_is_valid) {
    return(input_df_1)
  } else if (model_2_is_valid) {
    return(input_df_2)
  } else {stop('There are no valid datasets to be downloaded.')}
}

#-------------------------------------------------------------------------------
#' @name pct_above_y_at_x
#' @title Function that calculates % of population > Y-value at
#'                       a given X-value
#'
#' @param model_is_valid    checkpoint before rest of function proceeds, default FALSE
#' @param input_df          Dataset containing ID column with more than 1 ID
#' @param y_name            Name of column (string) for Y-value
#' @param y_value           Y-value threshold (>), e.g. plasma concentration
#' @param x_name            Name of column (string) for X-value
#' @param x_value           X-value threshold (==), e.g. time
#' @param return_number_ids Return number of IDs that fits the criteria instead of %
#'                    Default FALSE
#'
#' @returns a numeric (either as a proportion (from 0 to 1, where 0 means 0% and
#'                    1 means 100%), or the actual number of IDs)
#' @importFrom dplyr filter sym
#' @export
#-------------------------------------------------------------------------------

pct_above_y_at_x <- function(model_is_valid = FALSE,
                             input_df,
                             y_name  = "DV",
                             y_value = NA,
                             x_name  = "TIME",
                             x_value = NA,
                             return_number_ids = FALSE) {
  
  if(!model_is_valid | is.null(x_value) | is.null(y_value)) {
    return(NA)
  }
  
  if(model_is_valid & !is.na(y_value) & !is.na(x_value) ) {
    
    input_df_filtered <- input_df %>%
      dplyr::filter(!!dplyr::sym(y_name)  > y_value,
                    !!dplyr::sym(x_name) == x_value)
    
    number_of_ids_in_df          <- length(unique(input_df$ID))
    number_of_ids_in_df_filtered <- length(unique(input_df_filtered$ID))
    
    if(return_number_ids) {
      return(number_of_ids_in_df_filtered)
    } else {
      return(round(number_of_ids_in_df_filtered/number_of_ids_in_df * 100,1))
    }
    
  } else {
    return(NA)
  }
}

#-------------------------------------------------------------------------------
#' @name create_alert
#' @title Function that generates a shinyalert
#'
#' @param ppm_name       Name of Project Pharmacometrician
#' @param ppm_email      Email of Project Pharmacometrician
#'
#' @returns a shinyalert() popup
#' @importFrom shinyalert shinyalert
#' @export
#-------------------------------------------------------------------------------

create_alert <- function(ppm_name = "Firstname Lastname",
                         ppm_email = "dummy.email@company.com") {
  
  email_html <- paste0("<a href='mailto:",
                       ppm_email,
                       "?subject=Model%20Visualization%20Platform%20(MVP)%20Usage'>",
                       ppm_name,
                       "</a>")
  
  warning_text <- paste0("The unlocked model is provided for exploratory purposes only.<br>Please consult with your Project Pharmacometrician (PPM), ",
                         email_html,
                         ", for more information.<br><br>",
                         "<b><font color='red'>Usage of any output produced in this App without the PPM's prior knowledge and approval is strictly prohibited.</font></b>")
  
  password_alert <- shinyalert::shinyalert(
    title = "Disclaimer",
    text = warning_text,
    size = "m",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = TRUE,
    type = "warning",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  return(password_alert)
}

#-------------------------------------------------------------------------------
#' @name useShinydashboardMVP
#'
#' @title Manually define useShinydashboard before it will be removed in a future
#' release of shinyWidgets
#'
#' @note
#' https://github.com/dreamRs/shinyWidgets/blob/26838f9e9ccdc90a47178b45318d110f5812d6e1/R/useShinydashboard.R
#'
#' @returns Attaches shinydashboard
#'
#' @export
#-------------------------------------------------------------------------------
useShinydashboardMVP <- function() {
  if (!requireNamespace(package = "shinydashboard"))
    message("Package 'shinydashboard' is required to run this function")
  deps <- htmltools::findDependencies(shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(),
    sidebar = shinydashboard::dashboardSidebar(),
    body = shinydashboard::dashboardBody()
  ))
  htmltools::attachDependencies(tags$div(class = "main-sidebar", style = "display: none;"), value = deps)
}

#-------------------------------------------------------------------------------
#' @name general_warning_modal
#' @title Creates a general warning modal
#' @param title            Title for the modal box
#' @param text_description UI elements
#'
#' @returns a modal dialog
#' @export
#-------------------------------------------------------------------------------

general_warning_modal <- function(title = "Warning", text_description = "Test") {
  shiny::showModal(shiny::modalDialog(
    title  = title,
    text_description,
    footer = shiny::modalButton("OK"),
    size   = "m",
    easyClose = FALSE,
    fade = FALSE
  ))
}

#-----------------------------------------------------------------------------
#' @name split_data_frame
#' @title Function to split dataframes into roughly equal portions to facilitate
#' distribution to each core
#'
#' @param df data.frame to split
#' @param N number of chunks / cores
#'
#' @returns A list containing N data.frames. If N=1, then the original df
#'         is returned unchanged
#' @export
#-----------------------------------------------------------------------------

split_data_frame <- function(df, N = 4) {
  if(N == 1) {
    return(list(df)) # Note we're coercing df to a list when single core is desired
  } else if (N < 1) { # This may be redundant as n.cores already have a safeguard
    stop("N can't be less than one")
  } else if (N > nrow(df)) { # This may be redundant as n.cores already have a safeguard
    stop("N must be less than nrows(df)")
  }
  
  # split the data.frame into a list
  res <- split(df, cut(seq_len(nrow(df)), N, labels = FALSE))
  return(res)
}

#-------------------------------------------------------------------------------
#' @name binary_cat_dist
#' @title Function to generate a binary categorical covariate
#'
#' @param n         number of subjects
#' @param percent   percent of subjects (approximate) in first category
#' @param catvalue1 numeric value for first category
#' @param catvalue2 numeric value for second category
#'
#' @returns a numeric vector
#' @export
#-------------------------------------------------------------------------------
binary_cat_dist <- function(n = 20, percent = 50, catvalue1 = 1, catvalue2 = 0) {
  
  # Calculate the number of 1s and 2s
  n_in_first  <- round(n * percent/100)
  n_in_second <- n - n_in_first
  
  # Generate the vector
  vec <- sample(c(rep(catvalue1, n_in_first), rep(catvalue2, n_in_second)))
  
  return(vec)
}

#-------------------------------------------------------------------------------
#' @name check_cov_name
#'
#' @title Function to return a DUMMY name if supplied string is
#' reserved
#'
#' @param orig_name                 original name
#' @param replaced_name             replacement name
#' @param list_of_reserved_strings  unavailable names
#'
#' @returns a string
#' @export
#-------------------------------------------------------------------------------

check_cov_name <- function(orig_name, replaced_name = "DUMMY", list_of_reserved_strings = c("AGE", "AGEMO", "SEX", "WT", "BMI", "BSA")) {
  
  if (orig_name %in% list_of_reserved_strings) {
    new_name <- replaced_name
    shiny::showNotification(paste0("WARNING: Covariate name is reserved. Will show up as ", replaced_name, " instead."), type = "warning", duration = 10)
  } else {
    new_name <- orig_name
  }
  return(new_name)
}

#-------------------------------------------------------------------------------
#' @name check_cov_name_duplicate
#' @title Function to check covariate names do not duplicate
#'
#' @param current_id : id of current textInput
#' @param all_ids    : list of ids for textInputs to check
#' @export
#-------------------------------------------------------------------------------

check_cov_name_duplicate <- function(current_id, all_ids) {
  shiny::observeEvent(input[[current_id]], {
    if(current_id != "") { # do not perform check if textInput is empty i.e. ""
      if(any(sapply(all_ids[all_ids != current_id], function(x) input[[current_id]] == input[[x]]))) {
        shiny::showNotification(paste0("ERORR: Covariate names must be unique from each other. Name is reset."), type = "error", duration = 10)
        shiny::updateTextInput(session, current_id, value = "")
      }
    }
  })
}

#------------------------------------------------------------------------------
#' @name add_watermark
#' @title Function to add a watermark to a ggplot
#'
#' @description
#' The function is based on the example found here:
#' https://www.r-bloggers.com/2012/05/adding-watermarks-to-plots/.
#'
#' @param watermark_toggle Set to TRUE to insert a watermark layer for ggplot
#' @param lab Text to be displayed
#' @param col Color of watermark text
#' @param alpha Text transparency
#' @param fontface "plain" | "bold" | "italic" | "bold.italic"
#' @param rot rotation (0,360), NA = from lower left to upper right corner
#' @param width text width relative to plot
#' @param pos x- and y-position relative to plot (vector of length 2)
#' @param align "left" | "right" | "center" | "centre" | "top" | "bottom",
#' can also be given as vector of length 2, first horizontal, then vertical
#' @import grid
#' @importFrom gridExtra arrangeGrob grid.arrange marrangeGrob
#' @export
#------------------------------------------------------------------------------

# ----- Watermark function -----
add_watermark <- function(watermark_toggle = TRUE,
                          lab = "For Internal Use Only",  # Text to be displayed
                          col = "grey",                   # text colour
                          alpha = 0.7,                    # text transparency [0,1]
                          fontface = "plain",              # "plain" | "bold" | "italic" | "bold.italic"
                          rot = 0,                       # rotation (0,360); NA = from lower left to upper right corner
                          width = 0.6,                    # text width relative to plot
                          pos = c(0.5, 0.5),              # x- and y-position relative to plot (vector of length 2)
                          align = "centre"                # alignment of text relative to position
                          #   "left" | "right" | "center" | "centre" | "top" | "bottom"
                          #   can also be given as vector of length 2, first horizontal, then vertical
) {
  
  if(watermark_toggle) {
    
    watermark_grob <- grid::grob(
      lab = lab, cl = "watermark",
      col = col, alpha = alpha, fontface = fontface,
      rot = rot, width = width, pos = pos, align = align
    )
    ggplot2::annotation_custom(xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf,
                               watermark_grob)
  } else {
    ggplot2::geom_blank()
  }
}

# ----- Draw details for watermark -----
#' @name drawDetails.watermark
#' @title Auxillary function for watermark in ggplot (S3method?)
#' @param x  x
#' @param ... Other parameters passed on
#' @import grid
#' @export
drawDetails.watermark <- function(x, ...) {
  plot_width_half  <- grid::convertUnit(
    grid::unit(1, "npc"),
    unitTo = "mm",
    val = TRUE,
    axisFrom = "x"
  ) / 2
  plot_height_half <- grid::convertUnit(
    grid::unit(1, "npc"),
    unitTo = "mm",
    val = TRUE,
    axisFrom = "y"
  ) / 2
  rotation <-
    if (is.na(x$rot)) {
      atan(plot_height_half / plot_width_half) * 180 / pi
    } else {
      x$rot
    }
  rotation_max90 <-
    if (abs(rotation) > 90) {
      abs(180 - abs(rotation))
    } else {
      abs(rotation)
    }
  target_text_width <- min(
    plot_width_half / cos(rotation_max90 * pi / 180),
    plot_height_half / cos((90 - rotation_max90) * pi / 180)
  ) * 2
  scale_to_target_width <- target_text_width /
    grid::convertUnit(grid::grobWidth(grid::textGrob(x$lab)), unitTo = "mm", val = TRUE) *
    x$width
  grid::grid.text(x$lab,
                  rot = rotation,
                  gp = grid::gpar(
                    cex = scale_to_target_width,
                    col = x$col,
                    fontface = x$fontface,
                    alpha = x$alpha),
                  x = grid::unit(x$pos[1], "npc"), y = grid::unit(x$pos[2], "npc"), just = x$align
  )
}

#-------------------------------------------------------------------------------
#' @name tblNCA_progress
#' @title Modified NonCompart::tblNCA to support progress bars
#'
#' @inheritParams NonCompart::tblNCA
#' @param show_progress display progress messages
#' @importFrom NonCompart sNCA tblNCA
#' @export
#-------------------------------------------------------------------------------

tblNCA_progress <- function (concData, key = "Subject", colTime = "Time", colConc = "conc", 
                             dose = 0, adm = "Extravascular", dur = 0, doseUnit = "mg", 
                             timeUnit = "h", concUnit = "ug/L", down = "Linear", R2ADJ = 0, 
                             MW = 0, SS = FALSE, iAUC = "", excludeDelta = 1, show_progress = TRUE) 
{
  class(concData) = "data.frame"
  nKey = length(key)
  for (i in 1:nKey) {
    if (sum(is.na(concData[, key[i]])) > 0) 
      stop(paste(key[i], "has NA value, which is not allowed!"))
  }
  IDs = unique(as.data.frame(concData[, key], ncol = nKey))
  nID = nrow(IDs)
  if (length(dose) == 1) {
    dose = rep(dose, nID)
  }
  else if (length(dose) != nID) {
    stop("Count of dose does not match with number of NCAs!")
  }
  Res = vector()
  withProgress(message = "Calculating NCA", value = 0, {
    for (i in 1:nID) {
      if(show_progress) {setProgress(value = i / nID, detail = paste0("Subject ", i, "/", nID))}
      strHeader = paste0(key[1], "=", IDs[i, 1])
      strCond = paste0("concData[concData$", key[1], "=='", 
                       IDs[i, 1], "'")
      if (nKey > 1) {
        for (j in 2:nKey) {
          strCond = paste0(strCond, " & concData$", key[j], 
                           "=='", IDs[i, j], "'")
          strHeader = paste0(strHeader, ", ", key[j], "=", 
                             IDs[i, j])
        }
      }
      strCond = paste0(strCond, ",]")
      tData = eval(parse(text = strCond))
      if (nrow(tData) > 0) {
        tRes = sNCA(tData[, colTime], tData[, colConc], dose = dose[i], 
                    adm = adm, dur = dur, doseUnit = doseUnit, timeUnit = timeUnit, 
                    concUnit = concUnit, R2ADJ = R2ADJ, down = down, 
                    MW = MW, SS = SS, iAUC = iAUC, Keystring = strHeader, 
                    excludeDelta = excludeDelta)
        Res = rbind(Res, tRes)
      }
    }
  })
  Res = cbind(IDs, Res)
  rownames(Res) = NULL
  colnames(Res)[1:nKey] = key
  attr(Res, "units") = c(rep("", nKey), attr(tRes, "units"))
  return(Res)
}

#-------------------------------------------------------------------------------
#' @name pdfNCA_wm
#' @title Modified ncar::pdfNCA to support watermarks
#'
#' @param watermark Insert watermark when TRUE
#' @param internal_version changes temp dir pathing as a workaround for cloud hosting
#' where access rights prevent writing
#' @param debug_msg show debug messages
#' @param show_progress display progress messages
#' @inheritParams ncar::pdfNCA
#' @importFrom ncar pdfNCA
#' @importFrom NonCompart sNCA tblNCA
#' @export
#-------------------------------------------------------------------------------

pdfNCA_wm <- function (fileName = "Temp-NCA.pdf", concData, key = "Subject",
                       colTime = "Time", colConc = "conc", dose = 0, adm = "Extravascular",
                       dur = 0, doseUnit = "mg", timeUnit = "h", concUnit = "ug/L",
                       down = "Linear", R2ADJ = 0, MW = 0, SS = FALSE, iAUC = "",
                       excludeDelta = 1, watermark = TRUE, internal_version = TRUE, debug_msg = TRUE,
                       show_progress = TRUE)
{
  
  if(debug_msg) {
    message(fileName)
    message(getwd())
  }
  
  if(!internal_version) { # Workaround for AWS hosting
    #fileName <- paste0("/tmp/", fileName) # didn't work
    setwd("/tmp") # Trying setwd method
  }
  
  if(debug_msg) {
    message(getwd())
  }
  
  class(concData) = "data.frame"
  defPar = par(no.readonly = TRUE)
  
  if(debug_msg) {
    message("Trying to open pdf device")
  }
  ncar::PrepPDF(fileName)
  
  ncar::AddPage()
  ncar::Text1(1, 1, "Individual Noncompartmental Analysis Result (Non-Validated)",
              Cex = 1.2)
  maxx = max(concData[, colTime], na.rm = TRUE)
  maxy = max(concData[, colConc], na.rm = TRUE)
  miny = min(concData[concData[, colConc] > 0, colConc], na.rm = TRUE)
  nKey = length(key)
  IDs = unique(as.data.frame(concData[, key], ncol = nKey))
  nID = nrow(IDs)
  if (length(dose) == 1) {
    dose = rep(dose, nID)
  }
  else if (length(dose) != nID) {
    stop("Count of dose does not match with number of NCAs!")
  }
  Res = vector()
  for (i in 1:nID) {
    if(show_progress) {
      shiny::incProgress(1/nID, detail = paste("Subject ", i, "/", nID))
    }
    strHeader = paste0(key[1], "=", IDs[i, 1])
    strCond = paste0("concData[concData$", key[1], "=='",
                     IDs[i, 1], "'")
    if (nKey > 1) {
      for (j in 2:nKey) {
        strCond = paste0(strCond, " & concData$", key[j],
                         "=='", IDs[i, j], "'")
        strHeader = paste0(strHeader, ", ", key[j], "=",
                           IDs[i, j])
      }
    }
    strCond = paste0(strCond, ",]")
    tData = eval(parse(text = strCond))
    if (nrow(tData) > 0) {
      x = tData[, colTime]
      y = tData[, colConc]
      tabRes = NonCompart::sNCA(x, y, dose = dose[i], adm = adm, dur = dur,
                                doseUnit = doseUnit, timeUnit = timeUnit, concUnit = concUnit,
                                down = down, R2ADJ = R2ADJ, MW = MW, SS = SS,
                                iAUC = iAUC, Keystring = strHeader, excludeDelta = excludeDelta)
      UsedPoints = attr(tabRes, "UsedPoints")
      txtRes = ncar::Res2Txt(tabRes, x, y, dose = dose[i], adm = adm,
                             dur = dur, doseUnit = doseUnit, down = down)
      Res = c(Res, txtRes)
      ncar::AddPage(Header1 = strHeader)
      ncar::TextM(txtRes, StartRow = 1, Header1 = strHeader)
      scrnmat = matrix(0, 3, 4)
      scrnmat[1, ] = c(0, 1, 0, 1)
      scrnmat[2, ] = c(0.1, 0.9, 0.5, 0.95)
      scrnmat[3, ] = c(0.1, 0.9, 0.05, 0.5)
      ScrNo = split.screen(scrnmat)
      screen(ScrNo[1])
      par(adj = 0)
      ncar::Text1(1, 1, strHeader, Cex = 1)
      screen(ScrNo[2])
      par(oma = c(1, 1, 1, 1), mar = c(4, 4, 3, 1), adj = 0.5)
      
      if(watermark) {
        grid::pushViewport(grid::viewport(angle = 50, name = "WM"))
        grid::grid.text("For Internal Use Only", x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                        gp = grid::gpar(col = "grey", fontsize = 52, alpha = 0.5))
        grid::popViewport()
      }
      
      plot(x, y, type = "b", cex = 0.7, xlim = c(0, maxx),
           ylim = c(0, maxy), xlab = paste0("Time (", timeUnit,
                                            ")"), ylab = paste0("Concentration (", concUnit,
                                                                ")"))
      
      screen(ScrNo[3])
      par(oma = c(1, 1, 1, 1), mar = c(4, 4, 3, 1), adj = 0.5)
      x0 = x[!is.na(y) & y > 0]
      y0 = y[!is.na(y) & y > 0]
      if (length(x0) > 0) {
        
        if(watermark) {
          grid::pushViewport(grid::viewport(angle = 50, name = "WM"))
          grid::grid.text("For Internal Use Only", x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                          gp = grid::gpar(col = "grey", fontsize = 52, alpha = 0.5))
          grid::popViewport()
        }
        
        plot(x0, log10(y0), type = "b", cex = 0.7, xlim = c(0,
                                                            maxx), ylim = c(log10(miny), log10(maxy)),
             yaxt = "n", xlab = paste0("Time (", timeUnit,
                                       ")"), ylab = paste0("Concentration (log interval) (",
                                                           concUnit, ")"))
        
        points(x[UsedPoints], log10(y[UsedPoints]), pch = 16)
        yticks = seq(round(min(log10(y0))), ceiling(max(log10(y0))))
        ylabels = sapply(yticks, function(i) as.expression(bquote(10^.(i))))
        axis(2, at = yticks, labels = ylabels)
        x1 = tabRes["LAMZLL"]
        x2 = tabRes["LAMZUL"]
        deltaX = x1 * 0.05
        y1 = log10(2.718281828) * (tabRes["b0"] - tabRes["LAMZ"] *
                                     (x1 - deltaX))
        y2 = log10(2.718281828) * (tabRes["b0"] - tabRes["LAMZ"] *
                                     (x2 + deltaX))
        lines(c(x1 - deltaX, x2 + deltaX), c(y1, y2),
              lty = 2, col = "red")
      }
      close.screen(all.screens = TRUE)
    }
  }
  par(defPar)
  ncar::ClosePDF()
  
}

#-------------------------------------------------------------------------------
#' @name generate_dosing_regimens
#' @title Generate Dosing Regimens
#' 
#' @description
#' This is the main function to supply dosing regimens to simulations.
#' If there are no valid dose amounts from any dosing regimens, a dummy mrgsolve::ev()
#' object is created to supply a dose of 0 to be used for simulations.
#'
#' @param amt1 Dose Amount Regimen 1
#' @param delay_time1 Delay Time Regimen 1
#' @param cmt1 Input CMT Regimen 1
#' @param tinf1 Infusion Time Regimen 1
#' @param total1 Total Doses Regimen 1
#' @param ii1 Interdose Interval Regimen 1
#' @param amt2 Dose Amount Regimen 2
#' @param delay_time2 Delay Time Regimen 2
#' @param cmt2 Input CMT Regimen 2
#' @param tinf2 Infusion Time Regimen 2
#' @param total2 Total Doses Regimen 2
#' @param ii2 Interdose Interval Regimen 2
#' @param amt3 Dose Amount Regimen 3
#' @param delay_time3 Delay Time Regimen 3
#' @param cmt3 Input CMT Regimen 3
#' @param tinf3 Infusion Time Regimen 3
#' @param total3 Total Doses Regimen 3
#' @param ii3 Interdose Interval Regimen 3
#' @param amt4 Dose Amount Regimen 4
#' @param delay_time4 Delay Time Regimen 4
#' @param cmt4 Input CMT Regimen 4
#' @param tinf4 Infusion Time Regimen 4
#' @param total4 Total Doses Regimen 4
#' @param ii4 Interdose Interval Regimen 4
#' @param amt5 Dose Amount Regimen 5
#' @param delay_time5 Delay Time Regimen 5
#' @param cmt5 Input CMT Regimen 5
#' @param tinf5 Infusion Time Regimen 5
#' @param total5 Total Doses Regimen 5
#' @param ii5 Interdose Interval Regimen 5
#' @param mw_conversion MW conversion value
#' @param wt_multiplication_value Weight multiplication value
#' @param create_dummy_ev Default TRUE, to create dummy ev if there are no valid
#' dose amounts, set to FALSE to not create one
#' @param debug set to TRUE to show debug messages
#'
#' @importFrom mrgsolve ev as.ev
#' @importFrom dplyr arrange filter if_else
#' @returns A mrgsolve::ev event object

#' @export
#-------------------------------------------------------------------------------

generate_dosing_regimens <- function(amt1, delay_time1, cmt1, tinf1, total1, ii1,
                                     amt2, delay_time2, cmt2, tinf2, total2, ii2,
                                     amt3, delay_time3, cmt3, tinf3, total3, ii3,
                                     amt4, delay_time4, cmt4, tinf4, total4, ii4,
                                     amt5, delay_time5, cmt5, tinf5, total5, ii5,
                                     mw_conversion = 1,
                                     wt_multiplication_value = 1,
                                     create_dummy_ev = TRUE,
                                     debug = FALSE
) {
  
  # Special handling of amt to treat a user-input of dose number 0 to insert a dummy amount of 0,
  # As mrgsolve::ev does not allow zero doses
  
  dosing_scheme_1 <- mrgsolve::ev(amt     =  dplyr::if_else(sanitize_numeric_input(total1, allow_zero = TRUE, as_integer = TRUE) <= 0,
                                                            0,
                                                            sanitize_numeric_input(amt1) * mw_conversion * wt_multiplication_value),
                                  time    =  sanitize_numeric_input(delay_time1),
                                  cmt     =  cmt1,
                                  tinf    =  sanitize_numeric_input(tinf1),
                                  total   =  sanitize_numeric_input(total1, allow_zero = FALSE, as_integer = TRUE),
                                  ii      =  sanitize_numeric_input(ii1, allow_zero = FALSE)
  )
  dosing_scheme_2 <- mrgsolve::ev(amt     =  dplyr::if_else(sanitize_numeric_input(total2, allow_zero = TRUE, as_integer = TRUE) <= 0,
                                                            0,
                                                            sanitize_numeric_input(amt2) * mw_conversion * wt_multiplication_value),
                                  time    =  sanitize_numeric_input(delay_time2),
                                  cmt     =  cmt2,
                                  tinf    =  sanitize_numeric_input(tinf2),
                                  total   =  sanitize_numeric_input(total2, allow_zero = FALSE, as_integer = TRUE),
                                  ii      =  sanitize_numeric_input(ii2, allow_zero = FALSE)
  )
  dosing_scheme_3 <- mrgsolve::ev(amt     =  dplyr::if_else(sanitize_numeric_input(total3, allow_zero = TRUE, as_integer = TRUE) <= 0,
                                                            0,
                                                            sanitize_numeric_input(amt3) * mw_conversion * wt_multiplication_value),
                                  time    =  sanitize_numeric_input(delay_time3),
                                  cmt     =  cmt3,
                                  tinf    =  sanitize_numeric_input(tinf3),
                                  total   =  sanitize_numeric_input(total3, allow_zero = FALSE, as_integer = TRUE),
                                  ii      =  sanitize_numeric_input(ii3, allow_zero = FALSE)
  )
  dosing_scheme_4 <- mrgsolve::ev(amt     =  dplyr::if_else(sanitize_numeric_input(total4, allow_zero = TRUE, as_integer = TRUE) <= 0,
                                                            0,
                                                            sanitize_numeric_input(amt4) * mw_conversion * wt_multiplication_value),
                                  time    =  sanitize_numeric_input(delay_time4),
                                  cmt     =  cmt4,
                                  tinf    =  sanitize_numeric_input(tinf4),
                                  total   =  sanitize_numeric_input(total4, allow_zero = FALSE, as_integer = TRUE),
                                  ii      =  sanitize_numeric_input(ii4, allow_zero = FALSE)
  )
  dosing_scheme_5 <- mrgsolve::ev(amt     =  dplyr::if_else(sanitize_numeric_input(total5, allow_zero = TRUE, as_integer = TRUE) <= 0,
                                                            0,
                                                            sanitize_numeric_input(amt5) * mw_conversion * wt_multiplication_value),
                                  time    =  sanitize_numeric_input(delay_time5),
                                  cmt     =  cmt5,
                                  tinf    =  sanitize_numeric_input(tinf5),
                                  total   =  sanitize_numeric_input(total5, allow_zero = FALSE, as_integer = TRUE),
                                  ii      =  sanitize_numeric_input(ii5, allow_zero = FALSE)
  )
  
  total_doses <- c(dosing_scheme_1, dosing_scheme_2, dosing_scheme_3,
                   dosing_scheme_4, dosing_scheme_5) %>%
    as.data.frame() %>%
    dplyr::arrange(time) %>%
    dplyr::filter(amt > 0) %>%
    mrgsolve::as.ev()
  
  if(create_dummy_ev) {
    if(nrow(total_doses) == 0) {
      total_doses <- mrgsolve::ev(amt   = 0,
                                  time  = 0,
                                  cmt   = cmt1,
                                  tinf  = 0,
                                  total = 1,
                                  ii    = 0)
      
    }
  }
  
  if(debug) {
    message("Dosing regimen generated")
  }
  
  return(total_doses)
}

#-------------------------------------------------------------------------------
#' @name search_id_col
#' @title Search for Likely ID Columns
#' 
#' @description
#' Searches through several common column names that could be used to create the
#' "ID" column in the dataset, and push that to the first column of the dataset
#' 
#' @param orig_df The dataframe used for searching
#' @param names_of_id_cols Likely column names, in order of search priority
#' 
#' @returns a dataframe
#' @importFrom dplyr mutate select
#' @export
#-------------------------------------------------------------------------------

search_id_col <- function(orig_df,
                          names_of_id_cols = c("SUBJIDN", "SUBJID", "USUBJID", "PTNO")) {
  
  df <- orig_df
  
  for(i in seq_along(names_of_id_cols)) {
    if('ID' %in% names(df)) {
      return(df)
    } else {
      if(names_of_id_cols[i] %in% names(df)) {
        df <- df %>% dplyr::mutate(ID = !!dplyr::sym(names_of_id_cols[i])) %>%
          dplyr::select(ID, dplyr::everything())
        shiny::showNotification(paste0("ID column has been created from '", names_of_id_cols[i], "' column."), type = "message", duration = 10)
      }
    }
  } # end of loop
  
  return(orig_df) # if can't find any
}

#-------------------------------------------------------------------------------
#' @name search_time_col
#' @title Search for Likely TIME Columns
#' 
#' @description
#' Searches through several common column names that could be used to create the
#' "TIME" column in the dataset
#' 
#' @param orig_df The dataframe used for searching
#' @param names_of_time_cols Likely column names, in order of search priority
#' 
#' @returns a dataframe
#' @importFrom dplyr mutate select
#' @export
#-------------------------------------------------------------------------------

search_time_col <- function(orig_df,
                            names_of_time_cols = c("TAFD", "TSFD", "ATFD", "ATSD")) {
  
  df <- orig_df
  
  for(i in seq_along(names_of_time_cols)) {
    if('TIME' %in% names(df)) {
      return(df)
    } else {
      if(names_of_time_cols[i] %in% names(df)) {
        df <- df %>% dplyr::mutate(TIME = !!dplyr::sym(names_of_time_cols[i])) #%>%
        #dplyr::select(ID, dplyr::everything())
        shiny::showNotification(paste0("TIME column has been created from '", names_of_time_cols[i], "' column."), type = "message", duration = 10)
      }
    }
  } # end of loop
  
  return(orig_df) # if can't find any
}

#-------------------------------------------------------------------------------
#' @name expand_addl_ii
#' @title Expand ADDL and II dosing rows
#'
#' @description
#' Expands ADDL and II dosing rows. If there are no ADDL and II columns, return
#' original dataframe unchanged
#' 
#' @param data The dataframe used for expansion
#' @param x_axis The x_axis variable, usually TIME or TAFD etc
#' @param dose_col The dose variable, usually AMT or DOSE
#' @param debug Show debugging messages
#' 
#' @returns a dataframe
#' @importFrom dplyr filter mutate arrange rename rowwise ungroup
#' @importFrom tidyr unnest
#' @export
#-------------------------------------------------------------------------------

expand_addl_ii <- function(data, x_axis, dose_col, debug = FALSE) {
  
  data$DOSETIME <- as.numeric(as.character(data[[x_axis]])) # Still create DOSETIME if no ADDL II is found
  
  # Check if necessary columns are present
  if(all(c("ADDL", "II", "EVID", x_axis, dose_col) %in% colnames(data))) {
    
    data$ADDL <- as.integer(data$ADDL) # number of additional doses must be whole numbers
    data$II   <- as.numeric(data$II)
    
    # Split data into rows with ADDL > 0 and others
    addl_rows <- data %>% dplyr::filter(ADDL > 0 & !is.na(II))
    non_addl_rows <- data %>%
      dplyr::filter(!(ADDL > 0 & !is.na(II)))
    
    # message(paste0("nrow(addl_rows): ", nrow(addl_rows)))
    # message(paste0("nrow(non_addl_rows): ", nrow(non_addl_rows)))
    
    # Process addl_rows to expand
    if(nrow(addl_rows) > 0) { # Edge case where entire dataset has ADDL == 0
      expanded_addl <- addl_rows %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          # Generate a sequence of times including original and additional doses
          DOSETIME = list(DOSETIME + II * 0:ADDL)
        ) %>%
        tidyr::unnest(cols = c(DOSETIME)) %>%
        dplyr::ungroup() 
    } else {
      expanded_addl <- NULL
    }
    
    # Combine with non_addl_rows and arrange by ID and TIME
    df <- dplyr::bind_rows(expanded_addl, non_addl_rows) %>%
      dplyr::mutate(ADDL = NA, II = NA) %>% # Clean up for clarity
      dplyr::arrange(ID, DOSETIME) 
  } else {
    df <- data
  }
  
  if("RATE" %in% colnames(df)) { # NAMT, SAMT, min_yvar is previously calculated
    
    df$RATE <- as.numeric(as.character(data$RATE))
    
    df <- df %>% 
      select(ID, facet_label, DOSETIME, !!dplyr::sym(x_axis), !!dplyr::sym(dose_col), NAMT, SAMT, min_yvar, RATE)
  } else {
    df <- df %>%
      select(ID, facet_label, DOSETIME, !!dplyr::sym(x_axis), !!dplyr::sym(dose_col), NAMT, SAMT, min_yvar)
  }
  
  return(df)
}

#-------------------------------------------------------------------------------
#' @name trim_columns
#' @title Trim columns to only retain essential columns for plotting
#'
#' @description
#' Useful to cut down datasets when they are large and unwieldy
#' 
#' @param data The dataframe used for trimming
#' @param x_axis X-axis column name used for plotting
#' @param y_axis Y-axis column name used for plotting
#' @param color (optional) Color by column
#' @param sort_by (optional) A list of columns to sort by
#' @param strat_by (optional) A variable to flag outliers by
#' @param type_of_plot "general_plot", "ind_plot", "sim_plot"
#' @param facet_name (optional) column(s) to facet by
#' @param insert_med_line (optional) insert median line
#' @param med_line_by (optional) The median line to insert, insert_med_line must be TRUE
#' @param ind_dose_colname (optional) Individual plot dose column name
#' @param highlight_var (optional) highlight variable column name
#' @param lloq_colname (optional) LLOQ column name
#' 
#' @returns a dataframe
#' @importFrom dplyr select all_of filter mutate arrange rename rowwise ungroup
#' @export
#-------------------------------------------------------------------------------

trim_columns <- function(data,
                         x_axis,
                         y_axis,
                         color = "",
                         sort_by = NULL,
                         strat_by = "",
                         type_of_plot,
                         facet_name = NULL,
                         insert_med_line = FALSE,
                         med_line_by = "",
                         ind_dose_colname = "",
                         highlight_var = "",
                         lloq_colname = "") {
  
  # Required columns that are needed to make the plots. Optional columns are added afterwards
  essential_columns <- c("ID",
                         x_axis,
                         y_axis
  )
  
  if(!is.na(color) && color != "") {
    essential_columns <- c(essential_columns, color)
  }
  
  if("CMT" %in% colnames(data)) {
    essential_columns <- c(essential_columns, "CMT")
  }
  
  if(type_of_plot == "general_plot" | type_of_plot == "sim_plot") {
    
    if(type_of_plot == "general_plot") {
      if(!is.null(facet_name[1]) && facet_name[1] != "") {
        essential_columns <- c(essential_columns, facet_name)
      }
    }
    
    if(insert_med_line && med_line_by != '') {
      essential_columns <- c(essential_columns, med_line_by)
    }
    
  } # end of "general_plot" or "sim_plot"
  
  if(type_of_plot == "ind_plot") {
    if("EVID" %in% colnames(data)) {
      essential_columns <- c(essential_columns, "EVID")
    }
    
    if(ind_dose_colname != "") {
      essential_columns <- c(essential_columns, ind_dose_colname)
    }
    
    if(length(sort_by) > 0) {
      essential_columns <- c(essential_columns, unlist(sort_by))
    }
    
    if(!is.na(strat_by) && strat_by != '') {
      essential_columns <- c(essential_columns, strat_by)
    }
    
    if(all(c("ADDL", "II") %in% colnames(data))) {
      if(any(!is.na(data$ADDL))) { # Checks if there are any populated ADDL values despite the column is present
        essential_columns <- c(essential_columns, "ADDL", "II")
      }
    }
    
    if("RATE" %in% colnames(data)) {
      if(any(subset(data, !is.na(RATE))$RATE > 0)) {
        essential_columns <- c(essential_columns, "RATE")
      }
    }
    
    if(highlight_var != "") {
      essential_columns <- c(essential_columns, highlight_var)
    }
    
    if(lloq_colname != "") {
      essential_columns <- c(essential_columns, lloq_colname)
    }
  } # End of "ind_plot" check
  
  # Creates dataset used to plot and dropping any non-unique columns
  data_to_plot <- data %>% 
    dplyr::select(dplyr::all_of(unique(essential_columns)))
  
  return(data_to_plot)
  
}

#-------------------------------------------------------------------------------
#' @name exposures_table
#'
#' @title Function to calculate exposures from IIV output
#'
#' @param input_simulated_table  a dataframe (usually created from mrgsim)
#' @param output_conc            y variable of interest to perform summary stats calc 
#' @param start_time             start time interval for metrics
#' @param end_time               end time interval for metrics
#' @param debug                  show debugging messages
#'
#' @returns a dataframe with additional columns of summary stats
#' @importFrom dplyr mutate if_else ungroup select filter rename first group_by distinct summarise
#' @export
#-------------------------------------------------------------------------------

exposures_table <- function(input_simulated_table,
                            output_conc,
                            start_time = NULL,
                            end_time   = NULL,
                            debug      = FALSE
) {
  
  input_simulated_table$YVARNAME <- input_simulated_table[[output_conc]]
  
  # if (debug) {
  #   message(start_time)
  #   message(end_time)
  #   tmp <- input_simulated_table %>% dplyr::filter(TIME >= start_time)
  #   message(dplyr::glimpse(tmp))
  #   tmp2 <- input_simulated_table %>% dplyr::filter(TIME <= start_time)
  #   message(dplyr::glimpse(tmp2))
  # }
  
  metrics_table <- input_simulated_table %>% dplyr::filter(TIME >= start_time, TIME <= end_time) %>%
    group_by(ID) %>%
    dplyr::mutate(CMIN = min(YVARNAME, na.rm = TRUE)[1],
                  CMAX = max(YVARNAME, na.rm = TRUE)[1], ### First element if multiple values found
                  CAVG = mean(YVARNAME, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Calculate CMAX and TMAX in a separate table
  tmax_table <- metrics_table %>%
    group_by(ID) %>%
    summarise(TMIN = TIME[which.min(YVARNAME)[1]],
              TMAX = TIME[which.max(YVARNAME)[1]]) %>% # First element if multiple values found
    ungroup()
  
  metrics_table <- left_join(metrics_table, tmax_table, by = "ID")
  
  metrics_table  <- metrics_table %>%
    group_by(ID) %>%
    dplyr::mutate(YLAG          = dplyr::lag(YVARNAME),
                  XLAG          = dplyr::lag(TIME),
                  dYVAR         = (YVARNAME + YLAG) * (TIME - XLAG) * 0.5, # Area for trapezoid
                  dYVAR         = dplyr::if_else(is.na(dYVAR), 0, dYVAR),
                  AUC           = sum(dYVAR)) %>%
    dplyr::ungroup()
  
  metrics_table_id <- metrics_table %>%
    dplyr::select(ID, CMIN, CMAX, CAVG, AUC, TMAX, TMIN) %>%
    dplyr::distinct(ID, .keep_all = TRUE)
  return(metrics_table_id)
}

#-------------------------------------------------------------------------------
#' @name plot_iiv_exp_data
#'
#' @title Function to plot variability exposure data
#'
#' @param input_dataset        Input dataset of Model 1 and/or Model 2 that contains MODEL ID CMIN CAVG CMAX AUC
#' @param yvar                 Name of Y-variable (exposure metric) to be plotted, string 
#' @param ylab                 Optional name for yvar
#' @param model_1_name         Optional name for Model 1
#' @param model_2_name         Optional name for Model 2
#' @param model_1_color        Color for Model 1
#' @param model_2_color        Color for Model 2
#' @param show_stats           Display texts of stats for each box plot
#' @param title                Title for plot
#'
#' @returns a ggplot object
#' @importFrom dplyr select mutate all_of summarise
#' @importFrom ggplot2 theme_bw geom_boxplot scale_fill_manual geom_text
#' @importFrom forcats fct_inorder
#' @importFrom ggrepel geom_text_repel
#' @export
#-------------------------------------------------------------------------------

plot_iiv_exp_data <- function(input_dataset,
                              yvar = 'yvar',
                              ylab = yvar,
                              model_1_name = '',
                              model_2_name = '',
                              model_1_color = "#F8766D",
                              model_2_color = "#7570B3",
                              show_stats = TRUE,
                              xlab = '',
                              title = "") {
  
  input_dataset$MODEL <- gsub("Model 1", model_1_name, input_dataset$MODEL)
  input_dataset$MODEL <- gsub("Model 2", model_2_name, input_dataset$MODEL)
  input_dataset$MODEL <- as.factor(input_dataset$MODEL) %>%
    forcats::fct_inorder()
  
  # Define the colors for the models
  model_colors <- c()
  
  # Check if "Model 1" exists in the data
  if (model_1_name %in% input_dataset$MODEL) {
    model_colors[model_1_name] <- model_1_color
  }
  
  # Check if "Model 2" exists in the data
  if (model_2_name %in% input_dataset$MODEL) {
    model_colors[model_2_name] <- model_2_color
  }
  
  p <- ggplot2::ggplot(data = input_dataset, aes(x = MODEL, y = .data[[yvar]], group = MODEL, fill = MODEL)) +
    ggplot2::theme_bw() +
    ggplot2::geom_boxplot(alpha = 0.5) +
    ggplot2::scale_fill_manual(values = model_colors)
  
  if(show_stats) {
    # Calculate statistics for each group
    stats_df <- input_dataset %>%
      dplyr::group_by(MODEL) %>%
      dplyr::summarise(maximum = max(.data[[yvar]], na.rm = TRUE) %>% round(digits = 2),
                       upper975 = quantile(.data[[yvar]], probs = 0.975, na.rm = TRUE) %>% round(digits = 2),
                       upper95  = quantile(.data[[yvar]], probs = 0.95, na.rm = TRUE) %>% round(digits = 2),
                       mean     = mean(.data[[yvar]], na.rm = TRUE) %>% round(digits = 2),
                       median   = median(.data[[yvar]], na.rm = TRUE) %>% round(digits = 2),
                       lower025 = quantile(.data[[yvar]], probs = 0.025, na.rm = TRUE) %>% round(digits = 2),
                       lower05  = quantile(.data[[yvar]], probs = 0.05, na.rm = TRUE) %>% round(digits = 2),
                       minimum  = min(.data[[yvar]], na.rm = TRUE) %>% round(digits = 2)) %>%
      dplyr::ungroup()
    
    p <- p +
      ggplot2::geom_text(data = stats_df, aes(x = MODEL, y = maximum, label = paste("Max:", maximum)), vjust = -1) +
      ggplot2::geom_text(data = stats_df, aes(x = MODEL, y = upper95, label = paste("95%:", upper95)), vjust = -1) +
      ggplot2::geom_text(data = stats_df, aes(x = MODEL, y = mean,    label = paste("Mean:", mean)), vjust = -1) +
      ggplot2::geom_text(data = stats_df, aes(x = MODEL, y = median,  label = paste("Median:", median)), vjust = -1) +
      ggplot2::geom_text(data = stats_df, aes(x = MODEL, y = lower05, label = paste("5%:", lower05)), vjust = -1) +
      ggplot2::geom_text(data = stats_df, aes(x = MODEL, y = minimum, label = paste("Min:", minimum)), vjust = 1)
    
    # p <- p + 
    #   ggrepel::geom_text_repel(data = stats_df, aes(x = MODEL, y = maximum, label = paste("Max:", maximum))) +
    #   ggrepel::geom_text_repel(data = stats_df, aes(x = MODEL, y = upper95, label = paste("95%:", upper95))) +
    #   ggrepel::geom_text_repel(data = stats_df, aes(x = MODEL, y = mean,    label = paste("Mean:", mean))) +
    #   ggrepel::geom_text_repel(data = stats_df, aes(x = MODEL, y = median,  label = paste("Median:", median))) +
    #   ggrepel::geom_text_repel(data = stats_df, aes(x = MODEL, y = lower05, label = paste("5%:", lower05))) +
    #   ggrepel::geom_text_repel(data = stats_df, aes(x = MODEL, y = minimum, label = paste("Min:", minimum)))      
    
  }
  
  if(ylab != '') {
    p <- p + ggplot2::labs(x = "", y = ylab)
  } else {
    p <- p + ggplot2::labs(x = "", y = yvar)
  }
  
  p <- p +
    ggplot2::ggtitle(title) +
    ggplot2::theme(legend.position = "none")
  
  return(p)
}

#-------------------------------------------------------------------------------
#' @name calculate_quantiles
#'
#' @title Function to split a continuous X-variable for a number of quantiles 
#'
#' @param df              Name of dataframe
#' @param xvar            Name of x-axis variable to split by
#' @param num_quantiles   Number of discrete quantiles
#'
#' @returns a dataframe containing the quantile limits
#' @importFrom dplyr select mutate distinct starts_with
#' @export
#-------------------------------------------------------------------------------

calculate_quantiles <- function(df, xvar, num_quantiles) {
  
  probs <- seq(1/as.numeric(num_quantiles), 1, length.out = as.numeric(num_quantiles))
  
  for (i in seq_along(probs)) {
    df <- df %>%
      dplyr::mutate(!!paste0("Q", i) := quantile(.data[[xvar]], probs = probs[i], na.rm = TRUE))
  }
  
  df %>%
    dplyr::select(starts_with("Q")) %>%
    dplyr::distinct()
}

#-------------------------------------------------------------------------------
#' @name quantile_ranges_name
#'
#' @title Function that stores the names and ranges of quantiles as a 
#' character string to be used in Figure footnotes etc (Not used)
#'
#' @param quantiles_df    Name of dataframe containing columns corresponding to number of quantiles
#' @param df_orig         Name of original dataframe containing the continuous x-axis variable
#' @param xvar            Name of x-axis variable to split by
#'
#' @returns a character string containing the quantile limits
#' @export
#-------------------------------------------------------------------------------

quantile_ranges_name <- function(quantiles_df, df_orig, xvar) {
  range_name <- ""
  for(i in seq_along(quantiles_df)) {
    if(i == 1) { # Special case for first quantile
      range_name <- paste0(range_name, "Q", i, " = [", round(min(df_orig[[xvar]], na.rm = TRUE)), " - ", round(quantiles_df[[names(quantiles_df)[i]]]), "]")
    } else {
      range_name <- paste0(range_name, ", Q", i, " = (", round(quantiles_df[[names(quantiles_df)[i-1]]]), " - ", round(quantiles_df[[names(quantiles_df)[i]]]), "]")
    }
  }
  return(range_name)
}

#-------------------------------------------------------------------------------
#' @name categorize_xvar
#'
#' @title Function that categorizes X-axis variable according to the quantile df
#'
#' @param df              Name of dataframe containing columns corresponding to number of quantiles
#' @param quantiles_df    Name of dataframe containing the quantiles limits
#' @param xvar            Name of x-axis variable to split by
#'
#' @importFrom purrr map2
#' @importFrom dplyr case_when filter mutate sym syms summarise
#' @importFrom forcats fct_relevel
#' @returns a character string containing the quantile limits
#' @export
#-------------------------------------------------------------------------------

categorize_xvar <- function(df, quantiles_df, xvar) {
  
  # Check for any blanks / NAs in X-axis
  # if(any(is.na(df[[xvar]]))) {
  #   df <- df %>% filter(!is.na(!!dplyr::sym(xvar)))
  #   shiny::showNotification(paste0("WARNING: Some ", xvar, " values are NA and are removed prior to plotting."), type = "warning", duration = 10)
  # }
  
  # print("before rename quantiles_df:")
  # print(knitr::kable(quantiles_df))
  
  # Rename the quantile df to have limits inserted as part of the column name
  for(i in seq_along(quantiles_df)) {
    ending_bracket   <- "]" # Note that [ ] means inclusive intervals, ( ) means exclusive intervals
    if(i == 1) { # Special case for first quantile
      starting_bracket <- "\n["
      names(quantiles_df)[i] <- paste0(names(quantiles_df)[i], starting_bracket, round(min(df[[xvar]], na.rm = TRUE)), "-", round(quantiles_df[[1,1]]), ending_bracket)
    } else {
      starting_bracket <- "\n("
      #names(quantiles_df)[i] <- paste0(names(quantiles_df)[i], starting_bracket, round(quantiles_df[,..i-1]), "-", round(quantiles_df[,..i]), ending_bracket)
      names(quantiles_df)[i] <- paste0(names(quantiles_df)[i], starting_bracket, round(quantiles_df[[1,i-1]]), "-", round(quantiles_df[[1,i]]), ending_bracket)
    }
  }
  
  # print("after rename quantiles_df:")
  # print(knitr::kable(quantiles_df))
  
  conditions <- purrr::map2(
    quantiles_df,
    names(quantiles_df),
    ~rlang::expr(!!dplyr::sym(xvar) <= !!.x ~ !!.y)
  )
  
  conditions <- c(rlang::expr(is.na(!!dplyr::sym(xvar)) ~ "NA"), conditions)
  
  df <- df %>%
    dplyr::mutate(Quantile = dplyr::case_when(!!!conditions)) # triple-bang for list
  
  # Rename NAs
  df <- df %>%
    dplyr::mutate(Quantile = dplyr::case_when(is.na(Quantile) ~ "NA", TRUE ~ Quantile))
  
  # Reorder factor levels to place "NA" first
  df <- df %>%
    dplyr::mutate(Quantile = forcats::fct_relevel(Quantile, "NA", names(quantiles_df)))
  
  return(df)
}

#-------------------------------------------------------------------------------
#' @name create_facet_label
#'
#' @title Function that combines multiple columns to create a new one that contains
#' the facet labels
#'
#' @param df              Name of input dataframe
#' @param sort_by         A list containing columns to create label by. Uses first row only
#'
#' @importFrom dplyr group_by slice arrange syms rowwise mutate c_across all_of ungroup select
#' @importFrom dplyr left_join filter
#' @returns a dataframe with the new column called "facet_label"
#' @export
#-------------------------------------------------------------------------------

create_facet_label <- function(df, sort_by) {
  
  if(length(sort_by) == 1 && sort_by == "ID") { # Edge case
    df <- df %>%
      dplyr::mutate(facet_label = paste0("ID: ", ID))
  } else {
    
    # Remove "ID" from sort_by if it exists
    sort_by <- setdiff(sort_by, "ID")
    
    df <- df %>% dplyr::arrange(!!!dplyr::syms(sort_by))
    
    # Group by ID, then slice to keep only the first row of each group
    df_first <- df %>%
      dplyr::distinct(ID, .keep_all = TRUE)
    
    # Create the facet_label using the first row values of each ID group
    df_first <- df_first %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        facet_label = paste0("ID: ", ID, ", ", sapply(unlist(sort_by), function(col_name) {
          col_value <- dplyr::c_across(dplyr::all_of(col_name))
          paste0(col_name, ": ", col_value)
        }) 
        %>% paste(collapse = ", ")
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(ID, facet_label)
    
    df <- dplyr::left_join(df, df_first, by = "ID")
  }
  return(df)
}

#-------------------------------------------------------------------------------
#' @name categorize_outliers
#'
#' @title Function that categorizes outliers relative to a supplied stratification
#' group
#'
#' @param df              Name of input dataframe
#' @param highlight_range A character string in percentage e.g. "80%" to define outlier threshold
#' @param y_axis          Y-axis to derive the arithmetic mean for ID and group
#' @param strat_by        Column name to be used as a stratification variable for group
#' @param debug           Show debugging messages
#'
#' @importFrom dplyr group_by sym summarise ungroup left_join mutate case_when
#' @returns a dataframe with the new column called "outlier_status", with three categories: "Above", "Below", "Within"
#' @export
#-------------------------------------------------------------------------------

categorize_outliers <- function(df,
                                highlight_range,
                                y_axis,
                                strat_by,
                                debug = FALSE) {
  
  # Remove the percentage sign and convert to numeric
  highlight_numeric <- as.numeric(gsub("%", "", highlight_range)) / 100
  
  if(!is.character(df[[y_axis]])) {
    group_means <- df %>%
      dplyr::group_by(!!dplyr::sym(strat_by)) %>%
      dplyr::summarise(meanYVARGRP = mean(!!dplyr::sym(y_axis))) %>%
      dplyr::ungroup()
    
    id_means <- df %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(meanYVARID = mean(!!dplyr::sym(y_axis))) %>%
      dplyr::ungroup()
    
    # Join it back to main df
    df <- dplyr::left_join(df, group_means, by = strat_by)
    df <- dplyr::left_join(df, id_means,    by = "ID")
    
    df <- df %>%
      dplyr::mutate(outlier_status = dplyr::case_when(
        meanYVARID > (meanYVARGRP * (1 + highlight_numeric)) ~ "Above",
        meanYVARID < (meanYVARGRP * (1 - highlight_numeric)) ~ "Below",
        TRUE                                                 ~ "Within")
      )
  } else { # Do nothing if Y-axis is of character type
    df <- df %>%
      dplyr::mutate(outlier_status = "Within")
  }
  
  df <- df %>%
    dplyr::mutate(facet_label = dplyr::case_when(
      outlier_status == "Above" ~ paste0("**>", highlight_numeric * 100,"%** ", facet_label), # paste0(facet_label, " [>", highlight_numeric * 100, "%]")
      outlier_status == "Below" ~ paste0("**<", (1 - highlight_numeric) * 100,"%** ", facet_label), # paste0(facet_label, " [<", (1 - highlight_numeric) * 100, "%]")
      outlier_status == "Within"~ facet_label
    ))
  
  if(debug) {
    message("categorize_outliers done")
    # Check levels in the data
    #print(levels(nmd$outlier_status))
  }
  
  return(df)
}


#-------------------------------------------------------------------------------
#' @name get_bin_times
#'
#' @title Function that derives bin times of x_axis
#' group
#'
#' @param df              Input dataframe column of x-axis
#' @param bin_num         Maximum number of bins, integer
#' @param relative_threshold relative threshold of lumping bins that are close together
#'
#' @returns a vector of unique bin times
#' @export
#-------------------------------------------------------------------------------

get_bin_times <- function(dfcol, bin_num = 20, relative_threshold = 0.05) {
  
  # If number of unique times are less than 20, use that instead
  if(length(unique(as.numeric(dfcol))) < bin_num) {bin_num <- length(unique(as.numeric(dfcol)))}
  
  # Determine the unique bin boundaries based on quantiles
  bin_times <- unique(quantile(as.numeric(dfcol), probs = seq(0, 1, length.out = bin_num + 1), na.rm = TRUE))
  
  # Initialize the lumped bin times
  lumped_bin_times <- c(bin_times[1])  # Start with the first value
  
  # Iterate through the bin times
  for (i in 2:length(bin_times)) {
    # Check the relative difference between the current value and the last value in lumped_bin_times
    if (abs(bin_times[i] / lumped_bin_times[length(lumped_bin_times)] - 1) > relative_threshold) {
      # If the relative difference exceeds the threshold, add the current value to lumped_bin_times
      lumped_bin_times <- c(lumped_bin_times, bin_times[i])
    }
  }  
  
  return(lumped_bin_times)
}


#-------------------------------------------------------------------------------
#' @name handle_blanks
#'
#' @title Function that turns blank values into ".(blanks)"
#' group
#'
#' @param df              Input dataframe
#' @param column_name     Name of column (must be character type) to check for blanks
#'
#' @importFrom dplyr mutate sym
#' @returns a dataframe with the blank values converted into ".(blanks)
#' @export
#-------------------------------------------------------------------------------

handle_blanks <- function(df, column_name) {
  if (is.character(df[[column_name]]) && any(df[[column_name]] == "")) {
    shiny::showNotification(paste0("WARNING: Some ", column_name, " values are blank. Renamed to '.(blank)'."), type = "warning", duration = 10)
    df <- df %>% dplyr::mutate(!!column_name := ifelse(!!dplyr::sym(column_name) == "", ".(blank)", !!dplyr::sym(column_name)))
  }
  return(df)
}

#-------------------------------------------------------------------------------
#' @name transform_ev_df
#'
#' @title Helper function to handle ev_df transformations
#' group
#'
#' @param ev_df           Input event dataframe
#' @param model_dur       When TRUE, models duration
#' @param model_rate      When TRUE, models rate
#' @param pred_model      When TRUE, the model is a PRED model
#' @param debug           When TRUE, outputs debugging messages
#'
#' @importFrom dplyr mutate select any_of
#' @importFrom mrgsolve as.ev
#' @returns a cleaned event dataframe
#' @export
#-------------------------------------------------------------------------------

# Helper function to handle ev_df transformations
transform_ev_df <- function(ev_df, model_dur, model_rate, pred_model, debug = FALSE) {
  
  if (model_dur) { # modeling duration, it cannot coexist with tinf
    if(debug) {message("Applying model_dur transformation")}
    ev_df <- ev_df %>%
      as.data.frame() %>%
      dplyr::mutate(rate = -2) %>%
      dplyr::select(-tinf) %>%
      mrgsolve::as.ev()
  }
  
  if (model_rate) {
    if(debug) {message("Applying model_rate transformation")}
    ev_df <- ev_df %>%
      as.data.frame() %>%
      dplyr::mutate(rate = -1) %>%
      dplyr::select(-tinf) %>%
      mrgsolve::as.ev()
  }
  
  if (pred_model) { # set all CMTs to zero as that is required for PRED models
    if(debug) {message("Applying pred_model transformation")}
    ev_df <- ev_df %>%
      dplyr::mutate(cmt = 0) %>%
      dplyr::select(-dplyr::any_of(c("tinf", "rate")))
  }
  
  return(ev_df)
}