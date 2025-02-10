# Model Visualization Platform -------------------------------------------------
#
# Authors: Jin Gyu Kim (2023), Steve Choy (2023-2025)
#
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Debug mode:
# Set debug_mode = TRUE to run app.R as standalone app without loading MVPapp
# (i.e. not using run_mvp())
#-------------------------------------------------------------------------------
debug_mode <- FALSE

#######################
if(debug_mode) {

  source("packages.R") # Loading packages here to side-step JS compatibility issues

  default_options <- options()
  options(scipen=3) # Set the penalty to a high value to avoid scientific notation, this value is good up until 3e-07 / 1e+08
  options(DT.options = list(pageLength = 20, language = list(search = 'Filter:'), scrollX = T)) # dataTable options
  options(shiny.maxRequestSize = 100*1024^2) # Maximum file upload size

  # Pre-loads external patient databases ('cdc.expand', 'who.expand', 'nhanes.filtered')
  # The raw data used to create .rda is available on Github inside 'data-raw' folder
  source("databases_v_0_2_1.R")
  source("ui_settings_v_0_2_14.R")       # List of UI settings e.g. labels and descriptions
  source("code_templates_v_0_2_10.R")    # List of example mrgsolve models
  source("functions_v_0_2_14.R")         # List of helper functions required for the app

  ## Start-up options for the App
  #source("config.R")   # options - handled below
  show_debugging_msg  = TRUE
  authentication_code = NULL
  insert_watermark    = TRUE
  internal_version    = TRUE
  pw_models_path      = NULL  # "passworded_models_example.R"
  use_bi_styling      = FALSE
}
#######################

if(!is.null(pw_models_path)) {
  source(pw_models_path)
}


# UI ----
ui <- shiny::navbarPage(
  if(use_bi_styling) {
    title = htmltools::div(bi_logo, page_title)
  } else {
    title = htmltools::div(page_title)
  },
  if(use_bi_styling) {tags$head(tags$link(rel = "icon", type = "image/png", href = "BI_favicon-16x16.png"))},
  selected = 'Simulation',
  theme = shinythemes::shinytheme('flatly'),
  shinyjs::useShinyjs(),
  navbar_bg_color,
  ## Page 1 Data Input ----
  tabPanel('Data Input', icon = icon('file'),
           sidebarLayout(
             sidebarPanel(width = sidebar_width,
                          fluidRow(
                            shinyBS::bsPopover("bspop_upload_dataset", title = "Upload Dataset", content = bspop_upload_dataset, placement = "right", trigger = "hover"),
                            shinydashboard::box(width = 12,
                                                title = tags$span(htmltools::HTML("Upload Dataset&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_upload_dataset")),
                                                status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                fileInput("upload", label = NULL,  accept = c("text/csv",
                                                                                              "text/comma-separated-values,text/plain",
                                                                                              ".csv"), placeholder = 'Upload a NONMEM-formatted Dataset (.csv) or tab-delimited text (.txt)')
                            ),
                            shinyBS::bsPopover("bspop_dataset_cleaning", title = "Built-in Dataset Cleaning Options", content = bspop_dataset_cleaning, placement = "right", trigger = "hover"),
                            shinydashboard::box(width = 12,
                                                title = tags$span(htmltools::HTML("Built-in Dataset Cleaning Options&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_dataset_cleaning")),
                                                status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                                checkboxInput('change_all_to_upper', 'Enforce all column names to upper case', width = '100%', TRUE),
                                                checkboxInput('remove_pound_sign', 'Remove "#" or "@" from column names', width = '100%', TRUE),
                                                checkboxInput('create_cmt_col', 'Create CMT column if not found (CMT = 2)', width = '100%', TRUE),
                                                checkboxInput('create_id_col', 'Create ID column if not found (from "SUBJIDN", "USUBJID", or "PTNO")', width = '100%', TRUE),
                                                checkboxInput('create_time_col', 'Create TIME column if not found (from "TAFD", "TSFD", "ATFD", or "ATSD")', width = '100%', TRUE),
                                                checkboxInput('BLQ_filter', 'Remove BLQ observations (exclude BLQ >= 1)', width = '100%', TRUE),
                                                checkboxInput('EVID_filter', 'Remove dosing rows (exclude EVID == 1 or EVID == 4)', width = '100%', FALSE),
                                                checkboxInput('turn_all_numeric', 'Coerce Dataset to Numeric (all characters becomes "NA")', width = '100%', FALSE)

                            ),
                            shinyBS::bsPopover("bspop_deselect", title = "De-select Columns to Display", content = bspop_deselect, placement = "right", trigger = "hover"),
                            shinydashboard::box(width = 12,
                                                title = tags$span(htmltools::HTML("De-select Columns to Display&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_deselect")),
                                                status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                selectizeInput(inputId = 'column', label = NULL, character(0), multiple = TRUE),
                                                shinyBS::bsPopover('column', title = 'De-Select columns', content = bspop_select_columns, trigger = 'focus', placement = 'right'),
                                                update_resistant_popover('column', title = 'De-Select columns', content = bspop_select_columns, trigger = 'click', placement = 'right')
                            ),
                            shinydashboard::box(width = 12,
                                                title = tags$span(htmltools::HTML("Additional Dataset Cleaning or Filtering&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_dataset_code")),
                                                status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                shinyAce::aceEditor('codes', mode = 'r', value = code_editor_init, height = '200px',
                                                                    autoComplete = 'live',
                                                                    autoCompleters = c('rlang', 'keyword', 'snippet',
                                                                                       'static', 'text'),
                                                                    placeholder = 'Use dplyr codes for data manipulation
                      ex) mutate, filter, etc.'),
                                                column(width = 6,
                                                       checkboxInput('enableAutocomplete', 'Enable AutoComplete', TRUE)),
                                                column(width = 6,
                                                       shinyBS::bsButton('eval_button', 'Apply', class = 'pull-right', style = 'default'),
                                                       shinyBS::bsPopover('eval_button', 'Apply' , content = bspop_apply, placement = "right", trigger = "hover"))
                            ),
                            shinyBS::bsPopover("bspop_dataset_code", title = "Additional Dataset Cleaning or Filtering", content = bspop_dataset_code, placement = "right", trigger = "hover"),
                            shinydashboard::box(width = 12,
                                                title = 'Console Error Messages (if any)',
                                                status = 'primary',
                                                solidHeader = TRUE,
                                                collapsible = TRUE,
                                                collapsed   = FALSE,
                                                column(width = 12,
                                                       verbatimTextOutput("console_data_1")
                                                )
                            )
                          )                       # end of fluidRow
             ),                      # end of sidebarPanel
             mainPanel(width = mainbar_width,
                       shinydashboard::tabBox(
                         side = 'left', width = 12,
                         tabPanel('Filtered Data',
                                  fluidRow(
                                    column(width = 12,
                                           DT::dataTableOutput('dataset_page_table')),
                                    downloadButton("download_nmdataset_for_plot", "Download Data (.csv)")
                                  )       # end of fluidRow
                         ),              # end of tabPanel
                         tabPanel('Summary Statistics',
                                  DT::dataTableOutput('data_info'),
                                  checkboxInput('transpose_data_info', transpose_checkbox),
                                  shinyBS::bsPopover('transpose_data_info', transpose_checkbox, content = bspop_transpose, placement = 'left'),
                                  downloadButton("download_data_info", "Download Summary Statistics")
                         ),
                         tabPanel('Descriptive Statistics (NCA)',
                                  p(tags$span('This feature is experimental (based on the ',
                                              tags$a('"NonCompart" and "ncar"', href = "https://asancpt.github.io/book-ncar-en/methods.html#r-packages-noncompart-and-ncar", target ="_blank"),
                                              ' R packages) and assumes that the dataset contains one dose per subject only.', style = "color: red; font-weight: bold;")),
                                  p(tags$span('All results are provided for exploratory purposes only and is not a substitute for a GxP-compliant NCA!', style = "color: red; font-weight: bold;")),
                                  fluidRow(
                                    shinydashboard::box(width = 12,
                                                        title = tags$span(htmltools::HTML("Define Options for NCA&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_nca_tooltip")),
                                                        status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                        fluidRow(
                                                          column(width = 3,
                                                                 selectizeInput('subject_colname', "Subject Column Name", "ID", multiple = FALSE)),
                                                          column(width = 3,
                                                                 selectizeInput('additional_keys', "Group by Additional Column", NULL, multiple = FALSE)),
                                                          column(width = 3,
                                                                 selectizeInput('time_colname', "Time Column Name", "TIME")),
                                                          column(width = 3,
                                                                 textInput('desc_time_unit', "Time Unit", "h"))
                                                        ),
                                                        fluidRow(
                                                          column(width = 3,
                                                                 selectizeInput('conc_colname', "Conc Column Name", "DV")),
                                                          column(width = 3,
                                                                 textInput('desc_conc_unit', "Conc Unit", "nmol/L")),
                                                          column(width = 3,
                                                                 selectizeInput('dose_colname', "Dose Column Name", "DOSE", multiple = FALSE)),
                                                          #numericInput('dose_value', "Dose Amount", 0, min = 0),
                                                          #shinyBS::bsPopover('dose_value', title = 'Dose Amount', content = bspop_dose_value, trigger = 'hover', placement = 'bottom')),
                                                          column(width = 3,
                                                                 textInput('desc_dose_unit', "Dose Unit", "mg"))
                                                        ),
                                                        fluidRow(
                                                          column(width = 3,
                                                                 selectizeInput('adm_route', "Route of Administration", choices = c("Bolus", "Infusion", "Extravascular"), selected = "Extravascular")),
                                                          column(width = 3,
                                                                 numericInput('dur_inf', "Duration of Infusion", 0, min = 0)),
                                                          column(width = 3,
                                                                 selectizeInput('down_method', "AUC Method", choices = c("Linear", "Log"), selected = "Linear")),
                                                          column(width = 3,
                                                                 numericInput('mw_value', "Molecular Weight", 0, min = 0),
                                                                 shinyBS::bsPopover('mw_value', title = 'Molecular Weight', content = bspop_mw_value, trigger = 'hover', placement = 'bottom'))
                                                        ),
                                                        fluidRow(
                                                          column(width = 6),
                                                          column(width = 3,
                                                                 checkboxInput('transpose_nca', transpose_checkbox, value = TRUE),
                                                                 shinyBS::bsPopover('transpose_nca', transpose_checkbox, content = bspop_transpose, placement = 'left')),
                                                          column(width = 3,
                                                                 actionButton('calc_nca', label = htmltools::HTML('<i class="fa fa-spin fa-gears"></i> Calculate NCA'),
                                                                              class = 'pull-right'),
                                                                 shinyBS::bsPopover('calc_nca', 'Calculate NCA' , content = bspop_calc_nca, placement = "left", trigger = "hover"))
                                                        )
                                    ), # end of box
                                    shinyBS::bsPopover("bspop_nca_tooltip", title = "Define Options for NCA", content = bspop_nca_tooltip, placement = "right", trigger = "hover")
                                  ), # end of fluidRow
                                  uiOutput("descriptive_stats_summary") %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen),
                                  downloadButton("download_descriptive_stats", "Download Individual Results"),
                                  downloadButton("download_nca_report", "Generate NCA Report", icon = icon("cog", class = "fa-spin"))
                         ), # end of tabPanel
                         tabPanel('General Plot',
                                  fluidRow(
                                    shinydashboard::box(width = 12,
                                                        title = 'Data Exploration', status = 'primary', solidHeader = TRUE, collapsible = FALSE,
                                                        uiOutput('dataset_page_plot'),
                                                        downloadButton("download_data_plot", "Download Non-Interactive Plot"),
                                                        shinyBS::bsPopover('download_data_plot', 'Download Non-Interactive Plot' , content = bspop_download_plot, placement = "left", trigger = "hover")
                                    ),
                                    shinyBS::bsPopover("bspop_data_plot_options", title = "Plotting Options", content = bspop_data_plot_options, placement = "right", trigger = "hover"),
                                    shinydashboard::box(width = 12,
                                                        title = tags$span(htmltools::HTML("Plotting Options&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_data_plot_options")),
                                                        status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                        fluidRow(
                                                          column(width = 2,
                                                                 selectizeInput('y_axis', select_y_axis, character(0))),
                                                          column(width = 2,
                                                                 selectizeInput('x_axis', select_x_axis, character(0))),
                                                          column(width = 2,
                                                                 selectizeInput('color', select_color, character(0))),
                                                          column(width = 2,
                                                                 selectizeInput('facet_by',
                                                                                facet_by_label, character(0), selected = NULL, multiple = TRUE)),
                                                          column(width = 2,
                                                                 selectizeInput('median_line_by', select_median, character(0))),
                                                          column(width = 2,
                                                                 selectizeInput('filter_cmt_data', label_filter_cmt, character(0), selected = NULL))
                                                        ),
                                                        fluidRow(
                                                          column(width = 2,
                                                                 checkboxInput('log_y_axis_data', log_y_axis_label)),
                                                          column(width = 2,
                                                                 checkboxInput('log_x_axis_data', log_x_axis_label)),
                                                          column(width = 2,
                                                                 checkboxInput('insert_smoother', insert_smoother_label, value = FALSE)),
                                                          column(width = 2,
                                                                 checkboxInput('insert_lm_eqn', insert_lm_eqn_label, value = FALSE)),
                                                          column(width = 2,
                                                                 checkboxInput('median_line_data', median_line_label, value = TRUE),
                                                                 shinyBS::bsPopover('median_line_data', title = add_data_stat_sum, content = bspop_data_stat_sum, trigger = 'hover', placement = 'top')
                                                          ),
                                                          column(width = 2,
                                                                 #checkboxInput('do_boxplot', boxplot_label, value = FALSE),
                                                                 div(style = "height: 10px;"),  # Empty div to add space
                                                                 shinyWidgets::prettySwitch('do_boxplot', label = boxplot_label, status = "warning", slim = TRUE, bigger = TRUE),
                                                                 shinyBS::bsPopover('do_boxplot', title = boxplot_label, content = bspop_do_boxplot, trigger = 'hover', placement = 'top')
                                                          )
                                                        ),
                                                        fluidRow(
                                                          column(width = 6,
                                                                 textInput('plot_title_data', plot_title_label, value = NULL, placeholder = plot_title_placeholder)),
                                                          column(width = 2,
                                                                 selectInput('select_label_size', label = select_label_size_label,
                                                                             choices = seq(0, 10, by = 1),
                                                                             selected = 4,
                                                                             selectize = FALSE),
                                                                 shinyBS::bsPopover('select_label_size',  select_label_size_label, content = bspop_select_label_size, placement = 'left', trigger = 'hover')),
                                                          column(width = 4,
                                                                 div(style = "height: 20px;"),  # Empty div to add space
                                                                 shinyBS::bsPopover('do_data_plotly', 'Interactive Plot (Slower)' , content = bspop_do_data_plotly, placement = "top", trigger = "hover"),
                                                                 checkboxInput('do_data_plotly', 'Interactive Plot (Slower)', value = FALSE))
                                                        )

                                    ), # end of box
                                    shinydashboard::box(width = 12,
                                                        title = 'Download Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                                        column(width = 6,
                                                               textInput('plotlyd_filename', plotly_filename_label, value = paste0(today_numeric(), '_data_plot')),
                                                               shinyBS::bsPopover('plotlyd_filename', title = plotly_filename_label, content = bspop_plotly_file_name_label, placement = 'left', trigger = "focus")
                                                        ),
                                                        column(width = 2,
                                                               selectInput('plotlyd_format', label = plotly_format_label,
                                                                           choices = c("png","jpeg","svg","webp"),
                                                                           selected = 'png',
                                                                           selectize = FALSE)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('plotlyd_width', plotly_width_label, value = NULL, min = 1, step = 10),
                                                               shinyBS::bsPopover('plotlyd_width', title = plotly_width_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus")
                                                        ),
                                                        column(width = 2,
                                                               numericInput('plotlyd_height', plotly_height_label, value = NULL, min = 1, step = 10),
                                                               shinyBS::bsPopover('plotlyd_height', title = plotly_height_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus")
                                                        )
                                    )          # end of box
                                  )               # end of fluidRow
                         ),                     # end of tabPanel from tabBox
                         tabPanel('Ind. Plot',
                                  fluidRow(
                                    shinydashboard::box(width = 12,
                                                        title = 'Data Exploration', status = 'primary', solidHeader = TRUE, collapsible = FALSE,
                                                        uiOutput('dataset_page_ind_plot'),
                                                        downloadButton("download_data_ind_plot", "Download Non-Interactive Plot"),
                                                        shinyBS::bsPopover('download_data_ind_plot', 'Download Non-Interactive Plot' , content = bspop_download_plot, placement = "left", trigger = "hover"),
                                                        tags$span(style = "padding: 10px;"), # Add horizontal space
                                                        downloadButton("download_data_ind_plot_all", "Generate All Individuals (.pdf)", icon = icon("cog", class = "fa-spin")),
                                                        shinyBS::bsPopover('download_data_ind_plot_all', 'Generate All Individuals (.pdf)' , content = bspop_download_plot_all, placement = "left", trigger = "hover")
                                    ),
                                    shinyBS::bsPopover("bspop_ind_plot_options", title = "Plotting Options", content = bspop_ind_plot_options, placement = "right", trigger = "hover"),
                                    shinydashboard::box(width = 12,
                                                        title = tags$span(htmltools::HTML("Plotting Options&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_ind_plot_options")),
                                                        status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                        fluidRow(
                                                          column(width = 2,
                                                                 selectizeInput('filter_by_id', label_filter_by_id, NULL, multiple = TRUE),
                                                                 shinyBS::bsPopover('filter_by_id',  label_filter_by_id, content = bspop_filter_by_id, placement = 'left', trigger = 'focus')
                                                          ),
                                                          column(width = 2,
                                                                 selectInput('page_number', label = "Page:",
                                                                             choices = seq(1, 10, by = 1),
                                                                             selected = 1,
                                                                             selectize = FALSE)),
                                                          column(width = 2,
                                                                 selectInput('number_of_rows', label = "Rows per Page",
                                                                             choices = seq(1, 6, by = 1),
                                                                             selected = 3,
                                                                             selectize = FALSE)),
                                                          column(width = 2,
                                                                 selectInput('number_of_cols', label = "Columns per Page",
                                                                             choices = seq(1, 6, by = 1),
                                                                             selected = 3,
                                                                             selectize = FALSE)),
                                                          column(width = 2,
                                                                 selectizeInput('highlight_var', label_highlight, NULL, multiple = FALSE),
                                                                 shinyBS::bsPopover('highlight_var',  label_highlight, content = bspop_highlight_var, placement = 'left', trigger = 'hover')#,
                                                                 #update_resistant_popover('highlight_var', title = label_highlight, content = bspop_highlight_var, trigger = 'left', placement = 'hover') # overlays the choices, disable for now
                                                          ),
                                                          column(width = 2,
                                                                 selectizeInput('highlight_var_values', label_highlight_values, NULL, multiple = TRUE)#,
                                                                 #shinyBS::bsPopover('highlight_var_values',  label_highlight_values, content = bspop_highlight_var, placement = 'top', trigger = 'hover')
                                                          )
                                                        ), # end of fluidRow
                                                        fluidRow(
                                                          column(width = 2,
                                                                 selectizeInput('lloq_colname', label_lloq_colname, NULL, multiple = FALSE)),
                                                          #shinyBS::bsPopover('lloq_colname',  label_lloq_colname, content = bspop_lloq_colname, placement = 'left', trigger = 'hover'),
                                                          #update_resistant_popover('lloq_colname', title = label_lloq_colname, content = bspop_lloq_colname, trigger = 'left', placement = 'hover')),
                                                          column(width = 2,
                                                                 selectizeInput('ind_dose_colname', label_dose_colname, NULL, multiple = FALSE),
                                                                 shinyBS::bsPopover('ind_dose_colname',  label_dose_colname, content = bspop_dose_colname, placement = 'left', trigger = 'hover'),
                                                                 update_resistant_popover('ind_dose_colname', title = label_dose_colname, content = bspop_dose_colname, trigger = 'left', placement = 'hover')),
                                                          column(width = 2,
                                                                 textInput('dose_units', label_dose_units, value = "", placeholder = "(Optional)"),
                                                                 shinyBS::bsPopover('dose_units',  label_dose_units, content = bspop_dose_units, placement = 'top', trigger = 'hover')),
                                                          column(width = 2,
                                                                 div(style = "height: 20px;"),  # Empty div to add space
                                                                 shinyBS::bsPopover('insert_dosing', label_insert_dosing, content = bspop_insert_dosing, placement = "top", trigger = "hover"),
                                                                 checkboxInput('insert_dosing', label_insert_dosing, value = TRUE)),
                                                          column(width = 2,
                                                                 div(style = "height: 20px;"),  # Empty div to add space
                                                                 shinyBS::bsPopover('fixed_scale', label_fixed_scale , content = bspop_fixed_scale, placement = "top", trigger = "hover"),
                                                                 checkboxInput('fixed_scale', label_fixed_scale, value = FALSE)),
                                                          column(width = 2,
                                                                 div(style = "height: 20px;"),  # Empty div to add space
                                                                 shinyBS::bsPopover('do_data_ind_plotly', 'Interactive Plot (Slower)' , content = bspop_do_data_plotly, placement = "top", trigger = "hover"),
                                                                 checkboxInput('do_data_ind_plotly', 'Interactive Plot', value = FALSE))
                                                        ) # end of fluidRow
                                    ), # end of box
                                    shinydashboard::box(width = 12,
                                                        title = 'Download Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                                        fluidRow(
                                                          column(width = 6,
                                                                 textInput('plotlydi_filename', plotly_filename_label, value = paste0(today_numeric(), '_ind_plot')),
                                                                 shinyBS::bsPopover('plotlydi_filename', title = plotly_filename_label, content = bspop_plotly_file_name_label, placement = 'left', trigger = "focus")
                                                          ),
                                                          column(width = 2,
                                                                 selectInput('plotlydi_format', label = plotly_format_label,
                                                                             choices = c("png","jpeg","svg","webp"),
                                                                             selected = 'png',
                                                                             selectize = FALSE)
                                                          ),
                                                          column(width = 2,
                                                                 numericInput('plotlydi_width', plotly_width_label, value = NULL, min = 1, step = 10),
                                                                 shinyBS::bsPopover('plotlydi_width', title = plotly_width_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus")
                                                          ),
                                                          column(width = 2,
                                                                 numericInput('plotlydi_height', plotly_height_label, value = NULL, min = 1, step = 10),
                                                                 shinyBS::bsPopover('plotlydi_height', title = plotly_height_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus")
                                                          )
                                                        ),
                                                        fluidRow(tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px; width: 95%; margin-left: auto; margin-right: auto;"))),
                                                        fluidRow(
                                                          column(width = 6,
                                                                 textInput('plotlydi_all_filename', plotly_filename_all_label, value = paste0(today_numeric(), '_ind_plot_all')),
                                                                 shinyBS::bsPopover('plotlydi_filename_all', title = plotly_filename_all_label, content = bspop_plotly_file_name_all_label, placement = 'left', trigger = "focus")
                                                          ),
                                                          column(width = 2,
                                                                 selectInput('plotlydi_all_format', label = plotly_format_all_label,
                                                                             choices = c("pdf"),
                                                                             selected = 'pdf',
                                                                             selectize = FALSE)
                                                          ),
                                                          column(width = 2,
                                                                 numericInput('plotlydi_all_width', plotly_width_all_label, value = 10, min = 1, step = 1)
                                                          ),
                                                          column(width = 2,
                                                                 numericInput('plotlydi_all_height', plotly_height_all_label, value = 8, min = 1, step = 1)
                                                          )
                                                        ) # end fluidRow
                                    )          # end of box
                                  )               # end of fluidRow
                         ),                     # end of tabPanel from tabBox
                         tabPanel('Corr. Plot',
                                  fluidRow(
                                    shinydashboard::box(width = 12,
                                                        title = 'Data Exploration', status = 'primary', solidHeader = TRUE, collapsible = FALSE,
                                                        plotOutput('dataset_page_plot_corr', height = '600px') %>%
                                                          shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen),
                                                        downloadButton("download_corr_plot", "Download Non-Interactive Plot")),
                                    shinydashboard::box(width = 12,
                                                        title = 'Plotting Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                        column(width = 9,
                                                               selectizeInput('var_corr', 'Select Correlation Variables (Note: Time consuming if more than a few is selected)',
                                                                              choices = NULL,
                                                                              multiple = TRUE)
                                                        ),
                                                        column(width = 3,
                                                               selectizeInput('color_corr', 'Color by: ', NULL)
                                                        )
                                    ), # end of box
                                    shinydashboard::box(width = 12,
                                                        title = 'Download Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                                                        column(width = 6,
                                                               textInput('plotlyd_corr_filename', plotly_filename_label, value = paste0(today_numeric(), '_corr_plot')),
                                                               shinyBS::bsPopover('plotlyd_corr_filename', title = plotly_filename_label, content = bspop_plotly_file_name_label, placement = 'left', trigger = "focus")
                                                        ),
                                                        column(width = 2,
                                                               selectInput('plotlyd_corr_format', label = plotly_format_label,
                                                                           choices = c("png","pdf","jpeg","svg"),
                                                                           selected = 'png',
                                                                           selectize = FALSE)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('plotlyd_corr_width', plotly_width_label, value = NULL, min = 1, step = 10),
                                                               shinyBS::bsPopover('plotlyd_corr_width', title = plotly_width_label, content = bspop_plotly_width_height_corr, placement = 'left', trigger = "focus")
                                                        ),
                                                        column(width = 2,
                                                               numericInput('plotlyd_corr_height', plotly_height_label, value = NULL, min = 1, step = 10),
                                                               shinyBS::bsPopover('plotlyd_corr_height', title = plotly_height_label, content = bspop_plotly_width_height_corr, placement = 'left', trigger = "focus")
                                                        )
                                    )          # end of box
                                  ) # end of fluidRow
                         ), # end of corr plot tabPanel
                       ) # end of tabBox
             ) # end of mainPanel
           ) # end of sidebarLayout
  ), # end of tabPanel from navbarPage
  ## Page 2 Simulation ----
  tabPanel('Simulation', icon = icon("chart-line"),
           sidebarLayout(
             sidebarPanel(width = sidebar_width,
                          shinydashboard_box_format,
                          bslib::navset_pill(
                            bslib::nav_panel(title = 'Model 1',
                                             fluidRow(title = 'Model Input',
                                                      shinyBS::bsPopover("bspop_select_model_model_1", title = "Select Model", content = bspop_select_model, placement = "right", trigger = "hover"),
                                                      shinydashboard::box(width = 12,
                                                                          title = tags$span(htmltools::HTML("Select Model 1&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_select_model_model_1")),
                                                                          status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                                          id = "select_model_panel_model_1",
                                                                          tabPanel(title = "Model 1",
                                                                                   column(width = 7,
                                                                                          selectInput('model_select', label = NULL,
                                                                                                      choices = model_examples_list,
                                                                                                      selectize = FALSE),
                                                                                          uiOutput("upload_cpp_model_1")),
                                                                                   column(width = 5,
                                                                                          shinyBS::bsButton('generate_model', 'Generate Model 1', class = 'pull-right', style = 'default', icon = icon("circle-play")),
                                                                                          shinyBS::bsPopover('generate_model', 'Generate Model' , content = bspop_generate_model, placement = "bottom", trigger = "hover")
                                                                                   )
                                                                          )
                                                      ),  # end of Box
                                                      shinyBS::bsPopover("bspop_param_values_model_1", title = "Parameter Values (Fixed Effects)", content = bspop_param_values, placement = "right", trigger = "hover"),
                                                      shinydashboard::box(width = 12,
                                                                          title = tags$span(htmltools::HTML("Parameter Values&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_param_values_model_1")),
                                                                          status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                                          id = "parameter_values_panel",
                                                                          tabPanel(title = "Model 1",
                                                                                   uiOutput('param_output_model_1') %>%
                                                                                     shinycssloaders::withSpinner(type = 8, color = bi_darkgreen, size = 0.5, proxy.height = '50px')
                                                                          )
                                                      ),
                                                      shinyBS::bsPopover("bspop_model_code_1", title = "Model Code", content = bspop_model_code, placement = "right", trigger = "hover"),
                                                      shinydashboard::box(width = 12,
                                                                          title = tags$span(htmltools::HTML("Model 1 Code&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_model_code_1")),
                                                                          status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                                          id = "model_code_panel",
                                                                          tabPanel(title = "Model 1",
                                                                                   shinyAce::aceEditor('model_input', mode = 'r', value = one_cmt, height = '550px',
                                                                                                       autoComplete = 'disable',
                                                                                                       autoCompleters = c('rlang', 'keyword', 'snippet',
                                                                                                                          'static', 'text'),
                                                                                                       placeholder = 'Input mrgsolve format model code and run to generate the model'),
                                                                                   downloadButton('download_cpp_model_1', 'Download Model (.cpp)'),
                                                                                   shinyBS::bsPopover('download_cpp_model_1', 'Download Model' , content = bspop_download_cpp_model, placement = "right", trigger = "hover")
                                                                          )
                                                      ),
                                                      shinydashboard::box(width = 12,
                                                                          title = 'Model 1 Info (Console)',
                                                                          status = 'primary',
                                                                          solidHeader = TRUE,
                                                                          collapsible = TRUE,
                                                                          collapsed   = TRUE,
                                                                          column(width = 12,
                                                                                 verbatimTextOutput("console_output_model_1")
                                                                          )
                                                      )
                                             ) # end of fluidRow
                            ),                                 # end of tabPanel_1
                            bslib::nav_panel(title = 'Model 2',
                                             fluidRow(title = 'Model Input',
                                                      shinyBS::bsPopover("bspop_select_model_model_2", title = "Select Model", content = bspop_select_model, placement = "right", trigger = "hover"),
                                                      shinydashboard::box(width = 12,
                                                                          title = tags$span(htmltools::HTML("Select Model 2&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_select_model_model_2")),
                                                                          status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                                          id = "select_model_panel_model_2",
                                                                          tabPanel(title = "Model 2",
                                                                                   column(width = 7,
                                                                                          selectInput('model_select2', label = NULL,
                                                                                                      choices = model_examples_list,
                                                                                                      selected = '1 Compartment PK with Absorption Compartment',
                                                                                                      selectize = FALSE),
                                                                                          uiOutput("upload_cpp_model_2")),
                                                                                   column(width = 5,
                                                                                          shinyBS::bsButton('generate_model2', 'Generate Model 2', class = 'pull-right', style = 'default', icon = icon("circle-play")),
                                                                                          shinyBS::bsPopover('generate_model2', 'Generate Model' , content = bspop_generate_model, placement = "bottom", trigger = "hover")
                                                                                   )
                                                                          )
                                                      ),  # end of Box
                                                      shinyBS::bsPopover("bspop_param_values_model_2", title = "Parameter Values (Fixed Effects)", content = bspop_param_values, placement = "right", trigger = "hover"),
                                                      shinydashboard::box(width = 12,
                                                                          title = tags$span(htmltools::HTML("Parameter Values&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_param_values_model_2")),
                                                                          status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                                          id = "parameter_values_panel_2",
                                                                          tabPanel(title = "Model 2",
                                                                                   uiOutput('param_output_model_2') %>%
                                                                                     shinycssloaders::withSpinner(type = 8, color = bi_darkgreen, size = 0.5, proxy.height = '50px')
                                                                          )
                                                      ),
                                                      shinyBS::bsPopover("bspop_model_code_2", title = "Model Code", content = bspop_model_code, placement = "right", trigger = "hover"),
                                                      shinydashboard::box(width = 12,
                                                                          title = tags$span(htmltools::HTML("Model 2 Code&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_model_code_2")),
                                                                          status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                                          id = "model_code_panel_2",
                                                                          tabPanel(title = "Model 2",
                                                                                   shinyAce::aceEditor('model_input2', mode = 'r', value = one_cmt, height = '550px',
                                                                                                       autoComplete = 'disable',
                                                                                                       autoCompleters = c('rlang', 'keyword', 'snippet',
                                                                                                                          'static', 'text'),
                                                                                                       placeholder = 'Input mrgsolve format model code and run to generate the model'),
                                                                                   downloadButton('download_cpp_model_2', 'Download Model (.cpp)'),
                                                                                   shinyBS::bsPopover('download_cpp_model_2', 'Download Model' , content = bspop_download_cpp_model, placement = "right", trigger = "hover")
                                                                          )
                                                      ),
                                                      shinydashboard::box(width = 12,
                                                                          title = 'Model 2 Info (Console)',
                                                                          status = 'primary',
                                                                          solidHeader = TRUE,
                                                                          collapsible = TRUE,
                                                                          collapsed   = TRUE,
                                                                          column(width = 12,
                                                                                 verbatimTextOutput("console_output_model_2")
                                                                          )
                                                      )
                                             )                     # end of model 2 fluidRow
                            ),# end of model 2 nav_panel
                            #if(internal_version) {
                            bslib::nav_panel(title = "More...", id = "nav_panel", icon = icon('lock'),
                                             fluidRow(title = "Additional Options",
                                                      shinydashboard::box(width = 12,
                                                                          title = 'Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                                          id = "options_panel",
                                                                          tabPanel(title = "Additional Options",
                                                                                   textInput('password', label = "Enter Password:", placeholder = placeholder_password),
                                                                                   shinyBS::bsPopover('password', 'Locked Models' ,
                                                                                                      content = case_when(internal_version  ~ bspop_password,
                                                                                                                          !internal_version ~ bspop_password_github
                                                                                                      ),
                                                                                                      placement = "right", trigger = "focus")
                                                                          )
                                                      )
                                             ) # end of fluidRow
                            ) # end of password nav_panel
                            #}
                          ),                     # end of navset_pill
                          fluidRow(
                            shinydashboard::box(width = 12,
                                                title = 'Solver Settings (Advanced)',
                                                status = 'primary',
                                                solidHeader = TRUE,
                                                collapsible = TRUE,
                                                collapsed   = TRUE,
                                                column(width = 12,
                                                       numericInput('solver_maxsteps', 'maxsteps: ', value = 40000, min = 20000, step = 20000),
                                                       shinyBS::bsPopover('solver_maxsteps', 'Maximum Steps' , content = bspop_solversettings, placement = "right", trigger = "focus"),
                                                       numericInput('solver_atol',     'atol: ',     value = 1E-08, min = 1E-50, max = 1E-03, step = 0.00001),
                                                       shinyBS::bsPopover('solver_atol', 'Absolute tolerance' , content = bspop_solversettings, placement = "right", trigger = "focus"),
                                                       numericInput('solver_rtol',     'rtol: ',     value = 1E-08, min = 1E-50, max = 1E-03, step = 0.00001),
                                                       shinyBS::bsPopover('solver_rtol', 'Relative tolerance' , content = bspop_solversettings, placement = "right", trigger = "focus")
                                                       # fluidRow(
                                                       #   column(width = 6,
                                                       #          checkboxInput('para_checkbox', label_para, value = FALSE),
                                                       #          shinyBS::bsPopover('para_checkbox', label_para, content = bspop_para, trigger = 'hover', placement = 'right')
                                                       #          ),
                                                       #   column(width = 6,
                                                       #          numericInput('para_n', label_para_n, value = 200, min = 100, step = 100))
                                                       # )
                                                )
                            )  # end of box
                          ), # end of fluidRow
             ),                    # end of sidebarPanel
             mainPanel(width = mainbar_width,
                       fluidRow(
                         shinydashboard::box(width = 12,
                                             title = 'Dosing Options (Model 1)', status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                             tabsetPanel(
                                               id = 'dosing_tabset_panel',
                                               tabPanel(title = 'Regimen 1',
                                                        column(width = 2,
                                                               selectInput('cmt1_model_1', label_input_cmt, character(0))
                                                        ),
                                                        column(width = 2,
                                                               numericInput('amt1', label_dose, value = 50, min = 0, step = 1),
                                                               shinyBS::bsPopover('amt1', label_dose, content = bspop_dose, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('delay_time1', label_first_dose_time, value = 0, min = 0, step = 1),
                                                               shinyBS::bsPopover('delay_time1', label_first_dose_time, content = bspop_time_units, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('total1', label_total_doses, value = 1, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('ii1', label_ii, value = 1, min = 1),
                                                               shinyBS::bsPopover('ii1', label_ii, content = bspop_time_units, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('tinf1', label_infdur, value = 0, min = 0),
                                                               shinyBS::bsPopover('tinf1', title = label_infdur, content = bspop_infdur, placement = 'left', trigger = 'focus')
                                                        )

                                               ), # end of tabPanel
                                               tabPanel(title = 'Regimen 2',
                                                        column(width = 2,
                                                               selectInput('cmt2_model_1', label_input_cmt, character(0))
                                                        ),
                                                        column(width = 2,
                                                               numericInput('amt2', label_dose, value = 0, min = 0, step = 1),
                                                               shinyBS::bsPopover('amt2', label_dose, content = bspop_dose, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('delay_time2', label_first_dose_time, value = 12, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('total2', label_total_doses, value = 1, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('ii2', label_ii, value = 1, min = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('tinf2', label_infdur, value = 0, min = 0),
                                                               shinyBS::bsPopover('tinf2', title = label_infdur, content = bspop_infdur, placement = 'left', trigger = 'focus')
                                                        )
                                               ), # end of tabPanel
                                               tabPanel(title = 'Regimen 3',
                                                        column(width = 2,
                                                               selectInput('cmt3_model_1', label_input_cmt, character(0))
                                                        ),
                                                        column(width = 2,
                                                               numericInput('amt3', label_dose, value = 0, min = 0, step = 1),
                                                               shinyBS::bsPopover('amt3', label_dose, content = bspop_dose, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('delay_time3', label_first_dose_time, value = 24, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('total3', label_total_doses, value = 1, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('ii3', label_ii, value = 1, min = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('tinf3', label_infdur, value = 0, min = 0),
                                                               shinyBS::bsPopover('tinf3', title = label_infdur, content = bspop_infdur, placement = 'left', trigger = 'focus')
                                                        )
                                               ), # end of tabPanel
                                               tabPanel(title = 'Regimen 4',
                                                        column(width = 2,
                                                               selectInput('cmt4_model_1', label_input_cmt, character(0))
                                                        ),
                                                        column(width = 2,
                                                               numericInput('amt4', label_dose, value = 0, min = 0, step = 1),
                                                               shinyBS::bsPopover('amt4', label_dose, content = bspop_dose, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('delay_time4', label_first_dose_time, value = 36, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('total4', label_total_doses, value = 1, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('ii4', label_ii, value = 1, min = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('tinf4', label_infdur, value = 0, min = 0),
                                                               shinyBS::bsPopover('tinf4', title = label_infdur, content = bspop_infdur, placement = 'left', trigger = 'focus')
                                                        )
                                               ), # end of tabPanel
                                               tabPanel(title = 'Regimen 5',
                                                        column(width = 2,
                                                               selectInput('cmt5_model_1', label_input_cmt, character(0))
                                                        ),
                                                        column(width = 2,
                                                               numericInput('amt5', label_dose, value = 0, min = 0, step = 1),
                                                               shinyBS::bsPopover('amt5', label_dose, content = bspop_dose, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('delay_time5', label_first_dose_time, value = 48, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('total5', label_total_doses, value = 1, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('ii5', label_ii, value = 1, min = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('tinf5', label_infdur, value = 0, min = 0),
                                                               shinyBS::bsPopover('tinf5', title = label_infdur, content = bspop_infdur, placement = 'left', trigger = 'focus')
                                                        )
                                               ),
                                               tabPanel(title = 'Transform Dose',
                                                        fluidRow(
                                                          column(width = 3,
                                                                 checkboxInput('mw_checkbox', label_MW_checkbox),
                                                                 shinyBS::bsPopover('mw_checkbox', label_MW_checkbox, content = bspop_MW_checkbox, placement = 'left')
                                                          ),
                                                          column(width = 3,
                                                                 numericInput('mw', label_MW, value = 200000, min = 1),
                                                                 shinyBS::bsPopover('mw', label_MW, content = bw_MW, placement = 'left', trigger = 'focus')
                                                          ),
                                                          column(width = 3,
                                                                 numericInput('multi_factor', label_multi_number, value = 1000000, min = 1),
                                                                 shinyBS::bsPopover('multi_factor', label_multi_number, content = bw_MW, placement = 'left', trigger = 'focus')
                                                          )
                                                        ),
                                                        fluidRow(
                                                          column(width = 3,
                                                                 checkboxInput('wt_based_dosing_checkbox', label_wt_based_dosing_checkbox),
                                                                 shinyBS::bsPopover('wt_based_dosing_checkbox', label_wt_based_dosing_checkbox, content = bspop_wt_based_dosing_checkbox, placement = 'left')
                                                          ),
                                                          column(width = 3,
                                                                 textInput('wt_based_dosing_name', label_wt_based_dosing_name, value = "WT"),
                                                                 shinyBS::bsPopover('wt_based_dosing_name', label_wt_based_dosing_name, content = bspop_wt_based_dosing_name, placement = 'left')
                                                          )
                                                        )
                                               ), # end of tabPanel
                                               tabPanel(title = "Model Duration/Rate",
                                                        column(width = 3,
                                                               checkboxInput('model_dur_checkbox', model_dur_checkbox),
                                                               shinyBS::bsPopover('model_dur_checkbox', model_dur_checkbox, content = bspop_infdur_cb, placement = 'left')
                                                        ),
                                                        column(width = 3,
                                                               checkboxInput('model_rate_checkbox', model_rate_checkbox),
                                                               shinyBS::bsPopover('model_rate_checkbox', model_rate_checkbox, content = bspop_rate, placement = 'left')
                                                        )
                                               ) # end of tabPanel
                                             ) # end of tabsetPanel
                         ), # end of box
                         shinydashboard::box(width = 12,
                                             title = 'Dosing Options (Model 2)', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                             tabsetPanel(
                                               id = 'dosing_tabset_panel2',
                                               tabPanel(title = 'Regimen 1',
                                                        column(width = 2,
                                                               selectInput('cmt1_model_2', label_input_cmt, character(0))
                                                        ),
                                                        column(width = 2,
                                                               numericInput('amt1_2', label_dose, value = 50, min = 0, step = 1),
                                                               shinyBS::bsPopover('amt1_2', label_dose, content = bspop_dose, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('delay_time1_2', label_first_dose_time, value = 0, min = 0, step = 1),
                                                               shinyBS::bsPopover('delay_time1_2', label_first_dose_time, content = bspop_time_units, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('total1_2', label_total_doses, value = 1, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('ii1_2', label_ii, value = 1, min = 1),
                                                               shinyBS::bsPopover('ii1_2', label_ii, content = bspop_time_units, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('tinf1_2', label_infdur, value = 0, min = 0),
                                                               shinyBS::bsPopover('tinf1_2', title = label_infdur, content = bspop_infdur, placement = 'left', trigger = 'focus')
                                                        )

                                               ), # end of tabPanel
                                               tabPanel(title = 'Regimen 2',
                                                        column(width = 2,
                                                               selectInput('cmt2_model_2', label_input_cmt, character(0))
                                                        ),
                                                        column(width = 2,
                                                               numericInput('amt2_2', label_dose, value = 0, min = 0, step = 1),
                                                               shinyBS::bsPopover('amt2_2', label_dose, content = bspop_dose, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('delay_time2_2', label_first_dose_time, value = 12, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('total2_2', label_total_doses, value = 1, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('ii2_2', label_ii, value = 1, min = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('tinf2_2', label_infdur, value = 0, min = 0),
                                                               shinyBS::bsPopover('tinf2_2', title = label_infdur, content = bspop_infdur, placement = 'left', trigger = 'focus')
                                                        )
                                               ), # end of tabPanel
                                               tabPanel(title = 'Regimen 3',
                                                        column(width = 2,
                                                               selectInput('cmt3_model_2', label_input_cmt, character(0))
                                                        ),
                                                        column(width = 2,
                                                               numericInput('amt3_2', label_dose, value = 0, min = 0, step = 1),
                                                               shinyBS::bsPopover('amt3_2', label_dose, content = bspop_dose, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('delay_time3_2', label_first_dose_time, value = 24, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('total3_2', label_total_doses, value = 1, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('ii3_2', label_ii, value = 1, min = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('tinf3_2', label_infdur, value = 0, min = 0),
                                                               shinyBS::bsPopover('tinf3_2', title = label_infdur, content = bspop_infdur, placement = 'left', trigger = 'focus')
                                                        )
                                               ), # end of tabPanel
                                               tabPanel(title = 'Regimen 4',
                                                        column(width = 2,
                                                               selectInput('cmt4_model_2', label_input_cmt, character(0))
                                                        ),
                                                        column(width = 2,
                                                               numericInput('amt4_2', label_dose, value = 0, min = 0, step = 1),
                                                               shinyBS::bsPopover('amt4_2', label_dose, content = bspop_dose, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('delay_time4_2', label_first_dose_time, value = 36, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('total4_2', label_total_doses, value = 1, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('ii4_2', label_ii, value = 1, min = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('tinf4_2', label_infdur, value = 0, min = 0),
                                                               shinyBS::bsPopover('tinf4_2', title = label_infdur, content = bspop_infdur, placement = 'left', trigger = 'focus')
                                                        )
                                               ), # end of tabPanel
                                               tabPanel(title = 'Regimen 5',
                                                        column(width = 2,
                                                               selectInput('cmt5_model_2', label_input_cmt, character(0))
                                                        ),
                                                        column(width = 2,
                                                               numericInput('amt5_2', label_dose, value = 0, min = 0, step = 1),
                                                               shinyBS::bsPopover('amt5_2', label_dose, content = bspop_dose, trigger = 'focus', placement = 'left')
                                                        ),
                                                        column(width = 2,
                                                               numericInput('delay_time5_2', label_first_dose_time, value = 48, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('total5_2', label_total_doses, value = 1, min = 0, step = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('ii5_2', label_ii, value = 1, min = 1)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('tinf5_2', label_infdur, value = 0, min = 0),
                                                               shinyBS::bsPopover('tinf5_2', title = label_infdur, content = bspop_infdur, placement = 'left', trigger = 'focus')
                                                        )
                                               ),
                                               tabPanel(title = 'Transform Dose',
                                                        fluidRow(
                                                          column(width = 3,
                                                                 checkboxInput('mw_checkbox_2', label_MW_checkbox),
                                                                 shinyBS::bsPopover('mw_checkbox_2', label_MW_checkbox, content = bspop_MW_checkbox, placement = 'left')
                                                          ),
                                                          column(width = 3,
                                                                 numericInput('mw_2', label_MW, value = 200000, min = 1),
                                                                 shinyBS::bsPopover('mw_2', label_MW, content = bw_MW, placement = 'left', trigger = 'focus')
                                                          ),
                                                          column(width = 3,
                                                                 numericInput('multi_factor_2', label_multi_number, value = 1000000, min = 1),
                                                                 shinyBS::bsPopover('multi_factor_2', label_multi_number, content = bw_MW, placement = 'left', trigger = 'focus')
                                                          )
                                                        ),
                                                        fluidRow(
                                                          column(width = 3,
                                                                 checkboxInput('wt_based_dosing_checkbox_2', label_wt_based_dosing_checkbox),
                                                                 shinyBS::bsPopover('wt_based_dosing_checkbox_2', label_wt_based_dosing_checkbox, content = bspop_wt_based_dosing_checkbox, placement = 'left')
                                                          ),
                                                          column(width = 3,
                                                                 textInput('wt_based_dosing_name_2', label_wt_based_dosing_name, value = "WT"),
                                                                 shinyBS::bsPopover('wt_based_dosing_name_2', label_wt_based_dosing_name, content = bspop_wt_based_dosing_name, placement = 'left')
                                                          )
                                                        )
                                               ), # end of tabPanel
                                               tabPanel(title = "Model Duration/Rate",
                                                        column(width = 3,
                                                               checkboxInput('model_dur_checkbox_2', model_dur_checkbox),
                                                               shinyBS::bsPopover('model_dur_checkbox_2', model_dur_checkbox, content = bspop_infdur_cb, placement = 'left')
                                                        ),
                                                        column(width = 3,
                                                               checkboxInput('model_rate_checkbox_2', model_rate_checkbox),
                                                               shinyBS::bsPopover('model_rate_checkbox_2', model_rate_checkbox, content = bspop_rate, placement = 'left')
                                                        )
                                               ) # end of tabPanel
                                             ) # end of tabsetPanel
                         ), # end of box
                         shinydashboard::box(width = 12,
                                             title = label_main_sim_plot, status = 'primary', solidHeader = TRUE, collapsible = FALSE,
                                             column(width = 12,
                                                    uiOutput('simulation_plot_output'),
                                                    downloadButton("download_sim_plot", "Download Non-Interactive Plot"),
                                                    shinyBS::bsPopover('download_sim_plot', 'Download Non-Interactive Plot' , content = bspop_download_plot, placement = "left", trigger = "hover"),
                                                    tags$span(style = "padding: 10px;"), # Add horizontal space
                                                    downloadButton("download_sim_data", "Download Data (.csv)")
                                             )
                         ),
                         shinydashboard::box(width = 8,
                                             title = 'Plotting Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                             fluidRow(
                                               column(width = 6,
                                                      selectizeInput('time_unit', label_scale_x_axis, choices = list('Hours'    = '1',
                                                                                                                     'Days'     = '24',
                                                                                                                     'Weeks'    = '168',
                                                                                                                     'Months'   = '672'),
                                                                     options = list(create = TRUE)),
                                                      shinyBS::bsPopover('time_unit', label_scale_x_axis, content = bspop_scale_x_axis, placement = 'left', trigger = 'focus')),
                                               column(width = 6,
                                                      textInput('x_axis_label', label_x_axis, value = "Time (hours)", placeholder = 'write label on x axis'),
                                                      shinyBS::bsPopover('x_axis_label', label_x_axis, content = bspop_x_axis_label, placement = 'left', trigger = 'focus'))
                                             ),
                                             fluidRow(
                                               column(width = 6,
                                                      selectInput('yaxis_name', label_y_axis_sim_1, character(0)),
                                                      shinyBS::bsPopover('yaxis_name',  label_y_axis_sim_1, content = bspop_y_axis_sim, placement = 'left', trigger = 'focus'),
                                                      update_resistant_popover('yaxis_name',  label_y_axis_sim_1, content = bspop_y_axis_sim, placement = 'left', trigger = 'focus'),
                                                      selectInput('yaxis_name_2', label_y_axis_sim_2, character(0)),
                                                      shinyBS::bsPopover('yaxis_name_2',  label_y_axis_sim_2, content = bspop_y_axis_sim, placement = 'left', trigger = 'focus'),
                                                      update_resistant_popover('yaxis_name_2',  label_y_axis_sim_2, content = bspop_y_axis_sim, placement = 'left', trigger = 'focus')
                                               ),
                                               column(width = 6,
                                                      textInput('y_axis_label', label_y_axis, value = "Concentration", placeholder = 'write label on y axis'),
                                                      textInput('plot_title_sim', plot_title_label, value = NULL, placeholder = plot_title_placeholder)
                                               )
                                             ),
                                             fluidRow(
                                               column(width = 4,
                                                      checkboxInput('log_y_axis', log_y_axis_label)
                                               ),
                                               column(width = 4,
                                                      checkboxInput('log_x_axis', log_x_axis_label)
                                               ),
                                               column(width = 4,
                                                      checkboxInput('geom_point_sim_option', add_sim_geom_point, width = '100%', value = TRUE)
                                               )
                                             ),
                                             fluidRow(
                                               column(width = 4,
                                                      checkboxInput('show_model_1', show_model_1_label, value = TRUE)
                                               ),
                                               column(width = 4,
                                                      checkboxInput('show_model_2', show_model_2_label, value = TRUE)
                                               ),
                                               column(width = 4,
                                                      shinyBS::bsPopover('do_sim_plotly', 'Interactive Plot (Slower)' , content = bspop_do_sim_plotly, placement = "top", trigger = "hover"),
                                                      checkboxInput('do_sim_plotly', 'Interactive Plot (Slower)', value = TRUE))
                                             ),
                                             ############################# divider line ###############################
                                             fluidRow(tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px; width: 95%; margin-left: auto; margin-right: auto;"))),
                                             fluidRow(
                                               column(width = 6,
                                                      selectizeInput('nonmem_y_axis', label_y_axis_data, character(0))),
                                               column(width = 6,
                                                      selectizeInput('filter_cmt', label_filter_cmt, character(0), selected = NULL))
                                             ),    # end of fluidRow
                                             fluidRow(
                                               column(width = 6,
                                                      selectizeInput('color_data_by', label_color_data_by, character(0), selected = NULL),
                                                      shinyBS::bsPopover('color_data_by',  label_color_data_by, content = bspop_color_data_by, placement = 'left', trigger = 'focus'),
                                                      update_resistant_popover('color_data_by',  label_color_data_by, content = bspop_color_data_by, placement = 'left', trigger = 'focus')
                                               ),
                                               column(width = 6,
                                                      selectizeInput('stat_sum_data_by', label_stat_sum_data_by, character(0), selected = NULL),
                                                      shinyBS::bsPopover('stat_sum_data_by',  label_stat_sum_data_by, content = bspop_stat_sum_data_by, placement = 'left', trigger = 'focus'),
                                                      update_resistant_popover('stat_sum_data_by',  label_stat_sum_data_by, content = bspop_stat_sum_data_by, placement = 'left', trigger = 'focus')
                                               ),
                                             ),    # end of fluidRow
                                             fluidRow(
                                               column(width = 4,
                                                      checkboxInput('combine_nmdata', label_combine_nm_data, width = '100%'),
                                                      shinyBS::bsPopover('combine_nmdata', title = label_combine_nm_data, content = bspop_combine_nm_data, trigger = 'hover', placement = 'left')
                                               ),
                                               column(width = 4,
                                                      checkboxInput('stat_sum_data_option', add_data_stat_sum, width = '100%', value = TRUE),
                                                      shinyBS::bsPopover('stat_sum_data_option', title = add_data_stat_sum, content = bspop_data_stat_sum, trigger = 'focus', placement = 'top')
                                               ),
                                               column(width = 4,
                                                      checkboxInput('geom_point_data_option', add_data_geom_point, width = '100%', value = TRUE)
                                               )
                                             ) # end of fluidRow
                         ),          # end of Plotting options box
                         shinydashboard::box(width = 4,
                                             title = 'Simulation Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                             numericInput('tgrid_max', set_max_observation, value = 72, min = 0),
                                             shinyBS::bsPopover('tgrid_max', set_max_observation, content = bspop_time_units, trigger = 'focus', placement = 'left'),
                                             numericInput('delta', set_delta_observation, value = 1, step = 1, min = 0),
                                             shinyBS::bsPopover('delta', set_delta_observation, content = bspop_time_units, trigger = 'focus', placement = 'left'),
                                             textInput('custom_sampling_time_text', "Custom Sampling Schedule", value = 'c(0.25, 0.5, 1, 2, 4, 8, 12, 24, 48, 72)'),
                                             shinyBS::bsPopover('custom_sampling_time_text', "Custom Sampling Schedule", content = bspop_custom_sampling_text, trigger = 'focus', placement = 'left'),
                                             checkboxInput('custom_sampling_time_cb', "Custom Sampling Times"),
                                             shinyBS::bsPopover('custom_sampling_time_cb', title = "Custom Sampling Times", content = bspop_custom_sampling_cb, placement = 'left', trigger = "focus"),
                                             checkboxInput('add_time_zero', 'Add Sampling at Time 0', value = TRUE),
                                             shinyBS::bsPopover('add_time_zero', "Add Sampling at Time 0", content = bspop_time_0_text, trigger = 'focus', placement = 'left')

                         ),          # end of box
                         shinydashboard::box(width = 4,
                                             title = 'Download Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                             textInput('plotly_filename', plotly_filename_label, value = paste0(today_numeric(),'_sim_plot')),
                                             shinyBS::bsPopover('plotly_filename', title = plotly_filename_label, content = bspop_plotly_file_name_label, placement = 'left', trigger = "focus"),
                                             selectInput('plotly_format', label = plotly_format_label,
                                                         choices = c("png","jpeg","svg","webp"),
                                                         selected = 'png',
                                                         selectize = FALSE),
                                             numericInput('plotly_width', plotly_width_label, value = NULL, min = 1, step = 10),
                                             shinyBS::bsPopover('plotly_width', title = plotly_width_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus"),
                                             numericInput('plotly_height', plotly_height_label, value = NULL, min = 1, step = 10),
                                             shinyBS::bsPopover('plotly_height', title = plotly_height_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus")
                         )          # end of box
                       )                         # end of fluidRow from mainPanel
             )                                 # end of mainPanel
           )                               # end of sidbarLayout
  ),                                          # end of tabPanel
  ## Page 3 Parameter Sensitivity Analysis ----
  tabPanel('Parameter Sensitivity Analysis', icon = icon('magnifying-glass-chart'),
           bslib::navset_pill(
             tabPanel(title = 'Model 1',
                      sidebarLayout(
                        sidebarPanel(width = sidebar_width,
                                     fluidRow(
                                       shinyBS::bsPopover("bspop_select_psa_model_1", title = "Select Parameter for Sensitivity Analysis", content = bspop_select_psa, placement = "right", trigger = "hover"),
                                       shinydashboard::box(width = 12,
                                                           title = tags$span(htmltools::HTML("Select Parameter for Sensitivity Analysis&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_select_psa_model_1")),
                                                           status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                           column(width = 12,
                                                                  selectInput('param_selector_model_1', label = NULL, choices = character(0)))
                                       ),
                                       shinyBS::bsPopover("adjust_param_popover_model_1", title = "Adjust Parameter Values", content = bspop_adjust_param, placement = "right", trigger = "hover"),
                                       shinydashboard::box(width = 12,
                                                           title = tags$span(htmltools::HTML("Adjust Parameter Values&nbsp;"), tags$i(class="fa fa-circle-question", id = "adjust_param_popover_model_1")),
                                                           status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                           column(width = 4,
                                                                  uiOutput('param_widget_output_min_model_1')),
                                                           column(width = 4,
                                                                  uiOutput('param_widget_output_mid_model_1')),
                                                           column(width = 4,
                                                                  uiOutput('param_widget_output_max_model_1'))
                                       ),
                                       shinyBS::bsPopover("metrics_by_param_range_model_1", title = "Metrics by Parameter Range (Original Time Scale)", content = bspop_metrics_by_param_range, placement = "right", trigger = "click"),
                                       shinydashboard::box(width = 12,
                                                           title = tags$span(htmltools::HTML("Metrics by Parameter Range (Original Time Scale)&nbsp;"), tags$i(class="fa fa-circle-question", id = "metrics_by_param_range_model_1")),
                                                           status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                           tabsetPanel(
                                                             id = 'psa_tabset_panel_model_1',
                                                             tabPanel(title = 'PK',
                                                                      tags$div(tags$h5(tags$b('Min Parameter Metrics:'))),
                                                                      shinydashboard::valueBoxOutput('cmax_min_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('cavg_min_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('cmin_min_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('auc_min_model_1', width = infoBox_width),
                                                                      htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(),
                                                                      tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;"), h5(tags$b('Mid Parameter Metrics:'))),
                                                                      shinydashboard::valueBoxOutput('cmax_mid_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('cavg_mid_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('cmin_mid_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('auc_mid_model_1', width = infoBox_width),
                                                                      htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(),
                                                                      tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;"), h5(tags$b('Max Parameter Metrics:'))),
                                                                      shinydashboard::valueBoxOutput('cmax_max_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('cavg_max_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('cmin_max_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('auc_max_model_1', width = infoBox_width)
                                                             ), # end of tabPanel
                                                             tabPanel(title = 'PD',
                                                                      tags$div(tags$h5(tags$b('Min Parameter Metrics:'))),
                                                                      shinydashboard::valueBoxOutput('tmax_min_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('tmin_min_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('mcfbpct_min_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('nadirpct_min_model_1', width = infoBox_width),
                                                                      htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(),
                                                                      tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;"), h5(tags$b('Mid Parameter Metrics:'))),
                                                                      shinydashboard::valueBoxOutput('tmax_mid_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('tmin_mid_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('mcfbpct_mid_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('nadirpct_mid_model_1', width = infoBox_width),
                                                                      htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(),
                                                                      tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;"), h5(tags$b('Max Parameter Metrics:'))),
                                                                      shinydashboard::valueBoxOutput('tmax_max_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('tmin_max_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('mcfbpct_max_model_1', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('nadirpct_max_model_1', width = infoBox_width)
                                                             ), # end of tabPanel
                                                             tabPanel(title = 'Rounding Options',
                                                                      htmltools::br(),
                                                                      column(width = 6,
                                                                             numericInput('digits_model_1', digits_name, value = 4, min = 1, step = 1)
                                                                      ),
                                                                      column(width = 6,
                                                                             htmltools::br(),
                                                                             checkboxInput('dp_checkbox_model_1', dp_checkbox_name),
                                                                             shinyBS::bsPopover('dp_checkbox_model_1', dp_checkbox_name , content = bspop_dp_checkbox, placement = "bottom", trigger = "hover")
                                                                      )
                                                             )# end of tabPanel
                                                           )# end of tabsetPanel
                                       )   # end of box
                                     )
                        ),                         # end of sidebarPanel
                        mainPanel(width = mainbar_width,
                                  shinydashboard::box(width = 12,
                                                      title = label_main_sim_plot, status = 'primary', solidHeader = TRUE, collapsible = FALSE,
                                                      uiOutput('psa_plot_output_model_1'),
                                                      downloadButton("download_psa_plot_model_1", "Download Non-Interactive Plot"),
                                                      shinyBS::bsPopover('download_psa_plot_model_1', 'Download Non-Interactive Plot' , content = bspop_download_plot, placement = "left", trigger = "hover")
                                  ),
                                  shinydashboard::box(width = 12,
                                                      title = 'Select Time Interval for Deriving Metrics (Original Time Scale)', status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                      column(width = 6,
                                                             shinyWidgets::pickerInput('min_nca_obs_time_model_1', label = 'Start Time', choices = character(0), width = '300px', options = list(`live-search` = TRUE))),
                                                      column(width = 6,
                                                             shinyWidgets::pickerInput('max_nca_obs_time_model_1', label = 'End Time', choices = character(0), width = '300px', options = list(`live-search` = TRUE)))
                                  ),
                                  shinydashboard::box(width = 12,
                                                      title = 'Plotting Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                                                      column(width = 3,
                                                             checkboxInput('log_y_axis_model_1', log_y_axis_label),
                                                             checkboxInput('geom_point_sim_option_model_1', add_sim_geom_point, width = '100%', value = TRUE)
                                                      ),
                                                      column(width = 3,
                                                             checkboxInput('log_x_axis_model_1', log_x_axis_label),
                                                             checkboxInput('geom_point_data_option_model_1', add_data_geom_point, width = '100%', value = TRUE)),
                                                      column(width = 3,
                                                             checkboxInput('geom_vline_option_model_1', add_geom_vline, width = '100%', value = TRUE),
                                                             checkboxInput('combine_nmdata_1_model_1', label_combine_nm_data, width = '100%'),
                                                             shinyBS::bsPopover('combine_nmdata_1_model_1', title = label_combine_nm_data, content = bspop_combine_nm_data, trigger = 'hover', placement = 'top'),
                                                      ),
                                                      column(width = 3,
                                                             checkboxInput('geom_ribbon_option_model_1', add_geom_ribbon, width = '100%', value = FALSE),
                                                             checkboxInput('stat_sum_data_option_model_1', add_data_stat_sum, width = '100%', value = TRUE),
                                                             shinyBS::bsPopover('stat_sum_data_option_model_1', title = add_data_stat_sum, content = bspop_data_stat_sum, placement = 'right')),
                                                      column(width = 9,
                                                             textInput('plot_title_psa_model_1', plot_title_label, value = NULL, placeholder = plot_title_placeholder)
                                                      ),
                                                      column(width = 3,
                                                             div(style = "height: 15px;"),  # Empty div to add space
                                                             shinyBS::bsPopover('do_psa_plotly_model_1', 'Interactive Plot (Slower)' , content = bspop_do_sim_plotly, placement = "top", trigger = "hover"),
                                                             checkboxInput('do_psa_plotly_model_1', 'Interactive Plot (Slower)', value = TRUE)
                                                      )
                                  ),       # end of box
                                  shinydashboard::box(width = 12,
                                                      title = 'Download Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                                      column(width = 6,
                                                             textInput('plotly3_filename_model_1', plotly_filename_label, value = paste0(today_numeric(), '_psa_plot_model_1')),
                                                             shinyBS::bsPopover('plotly3_filename_model_1', title = plotly_filename_label, content = bspop_plotly_file_name_label, placement = 'left', trigger = "focus")
                                                      ),
                                                      column(width = 2,
                                                             selectInput('plotly3_format_model_1', label = plotly_format_label,
                                                                         choices = c("png","jpeg","svg","webp"),
                                                                         selected = 'png',
                                                                         selectize = FALSE)
                                                      ),
                                                      column(width = 2,
                                                             numericInput('plotly3_width_model_1', plotly_width_label, value = NULL, min = 1, step = 10),
                                                             shinyBS::bsPopover('plotly3_width_model_1', title = plotly_width_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus")
                                                      ),
                                                      column(width = 2,
                                                             numericInput('plotly3_height_model_1', plotly_height_label, value = NULL, min = 1, step = 10),
                                                             shinyBS::bsPopover('plotly3_height_model_1', title = plotly_height_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus")
                                                      )
                                  )
                        )
                      )
             ),
             tabPanel(title = 'Model 2',
                      sidebarLayout(
                        sidebarPanel(width = sidebar_width,
                                     fluidRow(
                                       shinyBS::bsPopover("bspop_select_psa_model_2", title = "Select Parameter for Sensitivity Analysis", content = bspop_select_psa, placement = "right", trigger = "hover"),
                                       shinydashboard::box(width = 12,
                                                           title = tags$span(htmltools::HTML("Select Parameter for Sensitivity Analysis&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_select_psa_model_2")),
                                                           status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                           column(width = 12,
                                                                  selectInput('param_selector_model_2', label = NULL, choices = character(0)))
                                       ),
                                       shinyBS::bsPopover("adjust_param_popover_model_2", title = "Adjust Parameter Values", content = bspop_adjust_param, placement = "right", trigger = "hover"),
                                       shinydashboard::box(width = 12,
                                                           title = tags$span(htmltools::HTML("Adjust Parameter Values&nbsp;"), tags$i(class="fa fa-circle-question", id = "adjust_param_popover_model_2")),
                                                           status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                           column(width = 4,
                                                                  uiOutput('param_widget_output_min_model_2')),
                                                           column(width = 4,
                                                                  uiOutput('param_widget_output_mid_model_2')),
                                                           column(width = 4,
                                                                  uiOutput('param_widget_output_max_model_2'))
                                       ),
                                       shinyBS::bsPopover("metrics_by_param_range_model_2", title = "Metrics by Parameter Range (Original Time Scale)", content = bspop_metrics_by_param_range, placement = "right", trigger = "click"),
                                       shinydashboard::box(width = 12,
                                                           title = tags$span(htmltools::HTML("Metrics by Parameter Range (Original Time Scale)&nbsp;"), tags$i(class="fa fa-circle-question", id = "metrics_by_param_range_model_2")),
                                                           status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                           tabsetPanel(
                                                             id = 'psa_tabset_panel_model_2',
                                                             tabPanel(title = 'PK',
                                                                      tags$div(tags$h5(tags$b('Min Parameter Metrics:'))),
                                                                      shinydashboard::valueBoxOutput('cmax_min_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('cavg_min_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('cmin_min_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('auc_min_model_2', width = infoBox_width),
                                                                      htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(),
                                                                      tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;"), h5(tags$b('Mid Parameter Metrics:'))),
                                                                      shinydashboard::valueBoxOutput('cmax_mid_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('cavg_mid_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('cmin_mid_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('auc_mid_model_2', width = infoBox_width),
                                                                      htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(),
                                                                      tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;"), h5(tags$b('Max Parameter Metrics:'))),
                                                                      shinydashboard::valueBoxOutput('cmax_max_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('cavg_max_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('cmin_max_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('auc_max_model_2', width = infoBox_width)
                                                             ), # end of tabPanel
                                                             tabPanel(title = 'PD',
                                                                      tags$div(tags$h5(tags$b('Min Parameter Metrics:'))),
                                                                      shinydashboard::valueBoxOutput('tmax_min_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('tmin_min_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('mcfbpct_min_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('nadirpct_min_model_2', width = infoBox_width),
                                                                      htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(),
                                                                      tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;"), h5(tags$b('Mid Parameter Metrics:'))),
                                                                      shinydashboard::valueBoxOutput('tmax_mid_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('tmin_mid_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('mcfbpct_mid_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('nadirpct_mid_model_2', width = infoBox_width),
                                                                      htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(),
                                                                      tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;"), h5(tags$b('Max Parameter Metrics:'))),
                                                                      shinydashboard::valueBoxOutput('tmax_max_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('tmin_max_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('mcfbpct_max_model_2', width = infoBox_width),
                                                                      shinydashboard::valueBoxOutput('nadirpct_max_model_2', width = infoBox_width)
                                                             ), # end of tabPanel
                                                             tabPanel(title = 'Rounding Options',
                                                                      htmltools::br(),
                                                                      column(width = 6,
                                                                             numericInput('digits_model_2', digits_name, value = 4, min = 1, step = 1)
                                                                      ),
                                                                      column(width = 6,
                                                                             htmltools::br(),
                                                                             checkboxInput('dp_checkbox_model_2', dp_checkbox_name),
                                                                             shinyBS::bsPopover('dp_checkbox_model_2', dp_checkbox_name , content = bspop_dp_checkbox, placement = "bottom", trigger = "hover")
                                                                      )
                                                             )# end of tabPanel
                                                           ) # end of tabsetPanel
                                       )   # end of box
                                     )
                        ),            # end of sidebarPanel
                        mainPanel(width = mainbar_width,
                                  shinydashboard::box(width = 12,
                                                      title = label_main_sim_plot, status = 'primary', solidHeader = TRUE, collapsible = FALSE,
                                                      uiOutput('psa_plot_output_model_2'),
                                                      downloadButton("download_psa_plot_model_2", "Download Non-Interactive Plot"),
                                                      shinyBS::bsPopover('download_psa_plot_model_2', 'Download Non-Interactive Plot' , content = bspop_download_plot, placement = "left", trigger = "hover")
                                  ),
                                  shinydashboard::box(width = 12,
                                                      title = 'Select Time Interval for Deriving Metrics (Original Time Scale)', status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                      column(width = 6,
                                                             shinyWidgets::pickerInput('min_nca_obs_time_model_2', label = 'Start Time', choices = character(0), width = '300px', options = list(`live-search` = TRUE))),
                                                      column(width = 6,
                                                             shinyWidgets::pickerInput('max_nca_obs_time_model_2', label = 'End Time', choices = character(0), width = '300px', options = list(`live-search` = TRUE)))
                                  ),             # end of box
                                  shinydashboard::box(width = 12,
                                                      title = 'Plotting Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                                                      column(width = 3,
                                                             checkboxInput('log_y_axis_model_2', log_y_axis_label),
                                                             checkboxInput('geom_point_sim_option_model_2', add_sim_geom_point, width = '100%', value = TRUE)
                                                      ),
                                                      column(width = 3,
                                                             checkboxInput('log_x_axis_model_2', log_x_axis_label),
                                                             checkboxInput('geom_point_data_option_model_2', add_data_geom_point, width = '100%', value = TRUE)),
                                                      column(width = 3,
                                                             checkboxInput('geom_vline_option_model_2', add_geom_vline, width = '100%', value = TRUE),
                                                             checkboxInput('combine_nmdata_1_model_2', label_combine_nm_data, width = '100%'),
                                                             shinyBS::bsPopover('combine_nmdata_1_model_2', title = label_combine_nm_data, content = bspop_combine_nm_data, trigger = 'hover', placement = 'top')

                                                      ),
                                                      column(width = 3,
                                                             checkboxInput('geom_ribbon_option_model_2', add_geom_ribbon, width = '100%', value = FALSE),
                                                             checkboxInput('stat_sum_data_option_model_2', add_data_stat_sum, width = '100%', value = TRUE),
                                                             shinyBS::bsPopover('stat_sum_data_option_model_2', title = add_data_stat_sum, content = bspop_data_stat_sum, placement = 'right')),
                                                      column(width = 9,
                                                             textInput('plot_title_psa_model_2', plot_title_label, value = NULL, placeholder = plot_title_placeholder)
                                                      ),
                                                      column(width = 3,
                                                             div(style = "height: 15px;"),  # Empty div to add space
                                                             shinyBS::bsPopover('do_psa_plotly_model_2', 'Interactive Plot (Slower)' , content = bspop_do_sim_plotly, placement = "top", trigger = "hover"),
                                                             checkboxInput('do_psa_plotly_model_2', 'Interactive Plot (Slower)', value = TRUE)
                                                      )
                                  ),       # end of box
                                  shinydashboard::box(width = 12,
                                                      title = 'Download Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                                      column(width = 6,
                                                             textInput('plotly3_filename_model_2', plotly_filename_label, value = paste0(today_numeric(), '_psa_plot_model_2')),
                                                             shinyBS::bsPopover('plotly3_filename_model_2', title = plotly_filename_label, content = bspop_plotly_file_name_label, placement = 'left', trigger = "focus")
                                                      ),
                                                      column(width = 2,
                                                             selectInput('plotly3_format_model_2', label = plotly_format_label,
                                                                         choices = c("png","jpeg","svg","webp"),
                                                                         selected = 'png',
                                                                         selectize = FALSE)
                                                      ),
                                                      column(width = 2,
                                                             numericInput('plotly3_width_model_2', plotly_width_label, value = NULL, min = 1, step = 10),
                                                             shinyBS::bsPopover('plotly3_width_model_2', title = plotly_width_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus")
                                                      ),
                                                      column(width = 2,
                                                             numericInput('plotly3_height_model_2', plotly_height_label, value = NULL, min = 1, step = 10),
                                                             shinyBS::bsPopover('plotly3_height_model_2', title = plotly_height_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus")
                                                      )
                                  )
                        )# end of mainPanel
                      )# end of sidebarLayout
             )# end of tabPanel_2
           )                                      # end of navset_pill
  ),                                         # end of tabPanel
  ## Page 4 Tables ----
  tabPanel('Tables', icon = icon('table'),
           bslib::navset_pill(
             tabPanel('Model 1',
                      htmltools::br(),
                      DT::dataTableOutput('psa_data_mid_model_1', width = '100%'),
                      downloadButton("download_psa_data_mid_model_1", "Download Data (.csv)")),
             tabPanel('Model 2',
                      htmltools::br(),
                      DT::dataTableOutput('psa_data_mid_model_2', width = '100%'),
                      downloadButton("download_psa_data_mid_model_2", "Download Data (.csv)")
             )
           )
  ),
  ## Page 5 Variability ----
  tabPanel('Variability', icon = icon('people-arrows'),
           sidebarLayout(
             sidebarPanel(width = sidebar_width,
                          bslib::navset_pill(
                            tabPanel(id = 'iiv_model_1', title = 'Model 1',
                                     fluidRow(
                                       shinyBS::bsPopover("varsim_popover_model_1", title = "Simulation Options", content = bspop_varsim, placement = "right", trigger = "hover"),
                                       shinydashboard::box(width = 12,
                                                           title = tags$span(htmltools::HTML("Simulation Options&nbsp;"), tags$i(class="fa fa-circle-question", id = "varsim_popover_model_1")),
                                                           status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                           tabsetPanel(
                                                             id = 'sim_subj_panel_model_1',
                                                             tabPanel(id = 'px_db_model_1', title = "Demographics",
                                                                      column(width = 12,
                                                                             selectizeInput('db_model_1', label_db, choices = c("None", "NHANES", "CDC", "WHO"), selected = "None"),
                                                                             shinyBS::bsPopover('db_model_1',  'Patient Database', content = bspop_db, placement = 'right', trigger = 'focus'),
                                                                             update_resistant_popover('db_model_1',  'Patient Database', content = bspop_db, placement = 'right', trigger = 'focus')
                                                                      ),
                                                                      column(width = 6,
                                                                             numericInput('n_subj_model_1', label = 'Number of Subjects', value = 20, min = 1, max = 3000, step = 1),
                                                                             shinyBS::bsPopover('n_subj_model_1',  'Number of Subjects', content = bspop_nsubj_warning, placement = 'right', trigger = 'focus'),
                                                                             update_resistant_popover('n_subj_model_1',  'Number of Subjects', content = bspop_nsubj_warning, placement = 'right', trigger = 'focus')
                                                                      ),
                                                                      column(width = 6,
                                                                             numericInput('seed_number_model_1', label = 'Seed', value = today_numeric(), min = 1),
                                                                             shinyBS::bsPopover('seed_number_model_1',  'Seed', content = bspop_seed, placement = 'right', trigger = 'focus')
                                                                      ),
                                                                      column(width = 12,
                                                                             sliderInput('age_db_model_1', label = age_range_label, min = 0, max = 100, value = c(18,65), step = 1)
                                                                      ),
                                                                      column(width = 6,
                                                                             sliderInput('wt_db_model_1', label = weight_range_label, min = 0, max = 150, value = c(0,150), step = 1)
                                                                      ),
                                                                      column(width = 6,
                                                                             sliderInput('males_db_model_1', label = male_range_label, min = 0, max = 100, value = 50, step = 5)
                                                                      )
                                                             ), # end of Patient Databases tabPanel
                                                             tabPanel(id = "db_custom_cov_model_1",
                                                                      title = "Covariate Distributions",
                                                                      fluidRow(
                                                                        column(width = 6,
                                                                               textInput('custom_cov_1_model_1', paste0(custom_cov_label, " 1"), value = NULL, placeholder = custom_cov_placeholder),
                                                                               shinyBS::bsPopover('custom_cov_1_model_1', paste0(custom_cov_label, " 1"), content = bspop_custom_cov, trigger = 'hover', placement = 'right')
                                                                        ),
                                                                        column(width = 6,
                                                                               selectInput('custom_cov_1_dist_model_1', label = custom_cov_dist_lab,
                                                                                           choices = c("Normal", "Log-Normal", "Uniform", "Binary Categorical"),
                                                                                           selected = 'Normal',
                                                                                           selectize = FALSE)
                                                                        )
                                                                      ),
                                                                      uiOutput("custom_cov_1_ui_model_1"),
                                                                      uiOutput("cov_1_plot_ui_model_1"),
                                                                      tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;")),
                                                                      ############# divider ###############
                                                                      fluidRow(
                                                                        column(width = 6,
                                                                               textInput('custom_cov_2_model_1', paste0(custom_cov_label, " 2"), value = NULL, placeholder = custom_cov_placeholder),
                                                                               shinyBS::bsPopover('custom_cov_2_model_1', paste0(custom_cov_label, " 2"), content = bspop_custom_cov, trigger = 'hover', placement = 'right')
                                                                        ),
                                                                        column(width = 6,
                                                                               selectInput('custom_cov_2_dist_model_1', label = custom_cov_dist_lab,
                                                                                           choices = c("Normal", "Log-Normal", "Uniform", "Binary Categorical"),
                                                                                           selected = 'Normal',
                                                                                           selectize = FALSE)
                                                                        )
                                                                      ),
                                                                      uiOutput("custom_cov_2_ui_model_1"),
                                                                      uiOutput("cov_2_plot_ui_model_1"),
                                                                      tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;")),
                                                                      ############# divider ###############
                                                                      fluidRow(
                                                                        column(width = 6,
                                                                               textInput('custom_cov_3_model_1', paste0(custom_cov_label, " 3"), value = NULL, placeholder = custom_cov_placeholder),
                                                                               shinyBS::bsPopover('custom_cov_3_model_1', paste0(custom_cov_label, " 3"), content = bspop_custom_cov, trigger = 'hover', placement = 'right')
                                                                        ),
                                                                        column(width = 6,
                                                                               selectInput('custom_cov_3_dist_model_1', label = custom_cov_dist_lab,
                                                                                           choices = c("Normal", "Log-Normal", "Uniform", "Binary Categorical"),
                                                                                           selected = 'Normal',
                                                                                           selectize = FALSE)
                                                                        )
                                                                      ),
                                                                      uiOutput("custom_cov_3_ui_model_1"),
                                                                      uiOutput("cov_3_plot_ui_model_1")
                                                             ), # end of Covariate Distributions tabPanel
                                                             tabPanel(id = "px_db_info_model_1",
                                                                      title = "Summary Statistics",
                                                                      column(width = 12,
                                                                             uiOutput("demog_info_model_1") %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen),
                                                                             downloadButton("download_demog_data_model_1", "Download Demographics")
                                                                      )
                                                             ), # end of Demographics tabPanel
                                                             tabPanel(id = "px_db_plots_model_1",
                                                                      title = "Plots",
                                                                      column(width = 12,
                                                                             uiOutput("demog_plots_model_1") %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen),
                                                                             downloadButton("download_demog_plot_model_1", "Download Plot")
                                                                      )
                                                             )
                                                           ) # end of tabsetPanel
                                       ), # end of box
                                       shinyBS::bsPopover("bspop_varmat_model_1", title = "Variability Matrix", content = bspop_varmat, placement = "right", trigger = "hover"),
                                       shinydashboard::box(width = 12,
                                                           title = tags$span(htmltools::HTML("Variability Matrix&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_varmat_model_1")),
                                                           status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                           column(width = 7,
                                                                  tags$div(h5('OMEGA (Between-Subject Variability):', style = "font-weight: bold;"))
                                                           ),
                                                           column(width = 5,
                                                                  actionButton('iiv_action_model_1', label = htmltools::HTML('<i class="fa fa-spin fa-refresh"></i> Update Model 1'),
                                                                               class = 'pull-right'),
                                                                  shinyBS::bsPopover('iiv_action_model_1', 'Update Model 1' , content = bspop_update_model, placement = "bottom", trigger = "hover")
                                                           ),
                                                           column(width = 12,
                                                                  rhandsontable::rHandsontableOutput('omega_model_1'),
                                                                  tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;")),
                                                                  tags$div(h5('SIGMA (Residual Unexplained Variability):', style = "font-weight: bold;")),
                                                                  rhandsontable::rHandsontableOutput('sigma_model_1')
                                                           )
                                       ), # end of box
                                       shinydashboard::box(width = 12,
                                                           title = 'Matrix Info', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                                           verbatimTextOutput('console_output_iiv_model_1')
                                       )
                                     )    # end of fluidRow
                            ),       #end of tabPanel
                            tabPanel(id = 'iiv_model_2', title = 'Model 2',
                                     fluidRow(
                                       shinyBS::bsPopover("varsim_popover_model_2", title = "Simulation Options", content = bspop_varsim, placement = "right", trigger = "hover"),
                                       shinydashboard::box(width = 12,
                                                           title = tags$span(htmltools::HTML("Simulation Options&nbsp;"), tags$i(class="fa fa-circle-question", id = "varsim_popover_model_2")),
                                                           status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                           tabsetPanel(
                                                             id = 'sim_subj_panel_model_2',
                                                             tabPanel(
                                                               id = 'px_db_model_2',
                                                               title = "Demographics",
                                                               column(width = 12,
                                                                      selectizeInput('db_model_2', label_db, choices = c("None", "NHANES", "CDC", "WHO"), selected = "None"),
                                                                      shinyBS::bsPopover('db_model_2',  'Patient Database', content = bspop_db, placement = 'right', trigger = 'focus'),
                                                                      update_resistant_popover('db_model_2',  'Patient Database', content = bspop_db, placement = 'right', trigger = 'focus')
                                                               ),
                                                               column(width = 6,
                                                                      numericInput('n_subj_model_2', label = 'Number of Subjects', value = 20, min = 1, max = 3000, step = 1),
                                                                      shinyBS::bsPopover('n_subj_model_2',  'Number of Subjects', content = bspop_nsubj_warning, placement = 'right', trigger = 'focus'),
                                                                      update_resistant_popover('n_subj_model_2',  'Number of Subjects', content = bspop_nsubj_warning, placement = 'right', trigger = 'focus')
                                                               ),
                                                               column(width = 6,
                                                                      numericInput('seed_number_model_2', label = 'Seed', value = today_numeric(), min = 1),
                                                                      shinyBS::bsPopover('seed_number_model_2',  'Seed', content = bspop_seed, placement = 'right', trigger = 'focus')
                                                               ),
                                                               column(width = 12,
                                                                      sliderInput('age_db_model_2', label = age_range_label, min = 0, max = 100, value = c(18,65), step = 1)
                                                               ),
                                                               column(width = 6,
                                                                      sliderInput('wt_db_model_2', label = weight_range_label, min = 0, max = 150, value = c(0,150), step = 1)
                                                               ),
                                                               column(width = 6,
                                                                      sliderInput('males_db_model_2', label = male_range_label, min = 0, max = 100, value = 50, step = 5)
                                                               )
                                                             ), # end of Patient Databases tabPanel
                                                             tabPanel(id = "db_custom_cov_model_2",
                                                                      title = "Covariate Distributions",
                                                                      fluidRow(
                                                                        column(width = 6,
                                                                               textInput('custom_cov_1_model_2', paste0(custom_cov_label, " 1"), value = NULL, placeholder = custom_cov_placeholder),
                                                                               shinyBS::bsPopover('custom_cov_1_model_2', paste0(custom_cov_label, " 1"), content = bspop_custom_cov, trigger = 'hover', placement = 'right')
                                                                        ),
                                                                        column(width = 6,
                                                                               selectInput('custom_cov_1_dist_model_2', label = custom_cov_dist_lab,
                                                                                           choices = c("Normal", "Log-Normal", "Uniform", "Binary Categorical"),
                                                                                           selected = 'Normal',
                                                                                           selectize = FALSE)
                                                                        )
                                                                      ),
                                                                      uiOutput("custom_cov_1_ui_model_2"),
                                                                      uiOutput("cov_1_plot_ui_model_2"),
                                                                      tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;")),
                                                                      ############# divider ###############
                                                                      fluidRow(
                                                                        column(width = 6,
                                                                               textInput('custom_cov_2_model_2', paste0(custom_cov_label, " 2"), value = NULL, placeholder = custom_cov_placeholder),
                                                                               shinyBS::bsPopover('custom_cov_2_model_2', paste0(custom_cov_label, " 2"), content = bspop_custom_cov, trigger = 'hover', placement = 'right')
                                                                        ),
                                                                        column(width = 6,
                                                                               selectInput('custom_cov_2_dist_model_2', label = custom_cov_dist_lab,
                                                                                           choices = c("Normal", "Log-Normal", "Uniform", "Binary Categorical"),
                                                                                           selected = 'Normal',
                                                                                           selectize = FALSE)
                                                                        )
                                                                      ),
                                                                      uiOutput("custom_cov_2_ui_model_2"),
                                                                      uiOutput("cov_2_plot_ui_model_2"),
                                                                      tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;")),
                                                                      ############# divider ###############
                                                                      fluidRow(
                                                                        column(width = 6,
                                                                               textInput('custom_cov_3_model_2', paste0(custom_cov_label, " 3"), value = NULL, placeholder = custom_cov_placeholder),
                                                                               shinyBS::bsPopover('custom_cov_3_model_2', paste0(custom_cov_label, " 3"), content = bspop_custom_cov, trigger = 'hover', placement = 'right')
                                                                        ),
                                                                        column(width = 6,
                                                                               selectInput('custom_cov_3_dist_model_2', label = custom_cov_dist_lab,
                                                                                           choices = c("Normal", "Log-Normal", "Uniform", "Binary Categorical"),
                                                                                           selected = 'Normal',
                                                                                           selectize = FALSE)
                                                                        )
                                                                      ),
                                                                      uiOutput("custom_cov_3_ui_model_2"),
                                                                      uiOutput("cov_3_plot_ui_model_2")
                                                             ), # end of Covariate Distributions tabPanel
                                                             tabPanel(
                                                               id = "px_db_info_model_2",
                                                               title = "Summary Statistics",
                                                               column(width = 12,
                                                                      uiOutput("demog_info_model_2") %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen),
                                                                      downloadButton("download_demog_data_model_2", "Download Demographics")
                                                               )
                                                             ), # end of Demographics tabPanel
                                                             tabPanel(
                                                               id = "px_db_plots_model_2",
                                                               title = "Plots",
                                                               column(width = 12,
                                                                      uiOutput("demog_plots_model_2") %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen),
                                                                      downloadButton("download_demog_plot_model_2", "Download Plot")
                                                               )
                                                             )
                                                           ) # end of tabsetPanel
                                       ), # end of box
                                       shinyBS::bsPopover("bspop_varmat_model_2", title = "Variability Matrix", content = bspop_varmat, placement = "right", trigger = "hover"),
                                       shinydashboard::box(width = 12,
                                                           title = tags$span(htmltools::HTML("Variability Matrix&nbsp;"), tags$i(class="fa fa-circle-question", id = "bspop_varmat_model_2")),
                                                           status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                           column(width = 7,
                                                                  tags$div(h5('OMEGA (Between-Subject Variability):', style = "font-weight: bold;"))
                                                           ),
                                                           column(width = 5,
                                                                  actionButton('iiv_action_model_2', label = htmltools::HTML('<i class="fa fa-spin fa-refresh"></i> Update Model 2'),
                                                                               class = 'pull-right'),
                                                                  shinyBS::bsPopover('iiv_action_model_2', 'Update Model 2' , content = bspop_update_model, placement = "bottom", trigger = "hover")
                                                           ),
                                                           column(width = 12,
                                                                  rhandsontable::rHandsontableOutput('omega_model_2'),
                                                                  tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;")),
                                                                  tags$div(h5('SIGMA (Residual Unexplained Variability):', style = "font-weight: bold;")),
                                                                  rhandsontable::rHandsontableOutput('sigma_model_2')
                                                           )
                                       ), # end of box
                                       shinydashboard::box(width = 12,
                                                           title = 'Matrix Info', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                                           verbatimTextOutput('console_output_iiv_model_2')
                                       )
                                     ) # end of fluidRow
                            )   #end of tabPanel Model 2
                          )     # end of tabsetPanel
             ),    # end of sidebarPanel
             mainPanel(width = mainbar_width,
                       shinydashboard::tabBox(
                         side = 'left', width = 12,
                         tabPanel('Simulated Data',
                                  fluidRow(
                                    shinydashboard::box(width = 12,
                                                        title = 'Simulated Model Plot', status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                        fluidRow(
                                                          column(width = 12,
                                                                 uiOutput('iiv_plot_output'),
                                                                 downloadButton("download_iiv_plot", "Download Non-Interactive Plot"),
                                                                 shinyBS::bsPopover('download_iiv_plot', 'Download Non-Interactive Plot' , content = bspop_download_plot, placement = "left", trigger = "hover"),
                                                                 tags$span(style = "padding: 10px;"), # Add horizontal space
                                                                 downloadButton("download_variability_table", "Download Data (.csv)")
                                                          )
                                                        )
                                    ), # end of box
                                    shinydashboard::box(width = 7,
                                                        title = 'Plotting Options', status = 'primary', solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,
                                                        fluidRow(
                                                          column(width = 6,
                                                                 numericInput('upper_quartile', 'Upper Percentile (%)', value = 97.5, min = 50, max = 100, step = 0.5),
                                                                 shinyBS::bsPopover('upper_quartile', title = 'Percentiles', content = bspop_percentiles, trigger = 'focus', placement = 'top')
                                                          ),
                                                          column(width = 6,
                                                                 numericInput('lower_quartile', 'Lower Percentile (%)', value = 2.5, min = 0, max = 50, step = 0.5),
                                                                 shinyBS::bsPopover('lower_quartile', title = 'Percentiles', content = bspop_percentiles, trigger = 'focus', placement = 'top')
                                                          ),
                                                          column(width = 6,
                                                                 checkboxInput('show_ind_profiles', 'Show Individual Profiles', value = FALSE)
                                                          ),
                                                          column(width = 6,
                                                                 shinyBS::bsPopover('do_iiv_plotly', 'Interactive Plot (Slower)' , content = bspop_do_iiv_plotly, placement = "top", trigger = "hover"),
                                                                 checkboxInput('do_iiv_plotly', 'Interactive Plot (Slower)', value = FALSE)
                                                          )
                                                        ),
                                                        tags$div(tags$hr(style="border-color: #CCCCCC; border-width: 4px;")),
                                                        fluidRow(
                                                          column(width = 3,
                                                                 checkboxInput('show_iiv_model_1', label = 'Show Model 1', value = TRUE),
                                                                 checkboxInput('log_y_axis_iiv', log_y_axis_label),
                                                                 checkboxInput('show_mean_iiv', show_mean_iiv_label)),
                                                          column(width = 3,
                                                                 checkboxInput('show_iiv_model_2', label = 'Show Model 2', value = TRUE),
                                                                 checkboxInput('log_x_axis_iiv', log_x_axis_label)),
                                                          column(width = 6,
                                                                 checkboxInput('combine_nmdata_iiv', label_combine_nm_data, width = '100%'),
                                                                 shinyBS::bsPopover('combine_nmdata_iiv', title = label_combine_nm_data, content = bspop_combine_nm_data, trigger = 'hover', placement = 'left'),
                                                                 checkboxInput('stat_sum_data_option_iiv', add_data_stat_sum, width = '100%', value = TRUE),
                                                                 shinyBS::bsPopover('stat_sum_data_option_iiv', title = add_data_stat_sum, content = bspop_data_stat_sum, placement = 'right'),
                                                                 checkboxInput('geom_point_data_option_iiv', add_data_geom_point, value = TRUE))
                                                        ),
                                                        fluidRow(
                                                          column(width = 12,
                                                                 textInput('plot_title_iiv', plot_title_label, value = NULL, placeholder = plot_title_placeholder))
                                                        )
                                    ),       # end of box
                                    shinydashboard::box(width = 5,
                                                        title = "Threshold Calculations", status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                                                        numericInput('y_value_threshold', 'Select Y-value:', value = NA),
                                                        shinyBS::bsPopover('y_value_threshold', title = 'Select Y-value', content = bspop_select_y_value_threshold, placement = 'left', trigger = 'focus'),
                                                        shinyWidgets::pickerInput('x_value_threshold', label = 'At Time (Original Scale):', choices = character(0), selected = NA, options = list(`live-search` = TRUE)),
                                                        uiOutput('proportion_above_threshold_model_1'),
                                                        uiOutput('proportion_above_threshold_model_2'),
                                                        checkboxInput('show_y_intercept_threshold', label = 'Show Y-intercept', value = TRUE),
                                                        checkboxInput('show_x_intercept_threshold', label = 'Show X-intercept', value = FALSE)
                                    ), # end of box
                                    shinydashboard::box(width = 12,
                                                        title = 'Download Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                                        column(width = 6,
                                                               textInput('plotly_iiv_filename', plotly_filename_label, value = paste0(today_numeric(), '_iiv_plot')),
                                                               shinyBS::bsPopover('plotly_iiv_filename', title = plotly_filename_label, content = bspop_plotly_file_name_label, placement = 'left', trigger = "focus")
                                                        ),
                                                        column(width = 2,
                                                               selectInput('plotly_iiv_format', label = plotly_format_label,
                                                                           choices = c("png","jpeg","svg","webp"),
                                                                           selected = 'png',
                                                                           selectize = FALSE)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('plotly_iiv_width', plotly_width_label, value = NULL, min = 1, step = 10),
                                                               shinyBS::bsPopover('plotly_iiv_width', title = plotly_width_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus")
                                                        ),
                                                        column(width = 2,
                                                               numericInput('plotly_iiv_height', plotly_height_label, value = NULL, min = 1, step = 10),
                                                               shinyBS::bsPopover('plotly_iiv_height', title = plotly_height_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus")
                                                        )
                                    )# end of box
                                  ) # end of fluidRow
                         ), # end of tabPanel
                         tabPanel('Exposure Box Plots',
                                  fluidRow(
                                    shinydashboard::box(width = 12,
                                                        title = 'Simulated Exposure Plot', status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                        fluidRow(
                                                          column(width = 12,
                                                                 uiOutput('iiv_exp_output'),
                                                                 downloadButton("download_exp_plot", "Download Non-Interactive Plot"),
                                                                 shinyBS::bsPopover('download_exp_plot', 'Download Non-Interactive Plot' , content = bspop_download_plot, placement = "left", trigger = "hover"),
                                                                 tags$span(style = "padding: 10px;"), # Add horizontal space
                                                                 downloadButton("download_exposures_table", "Download Data (.csv)")
                                                          )
                                                        ) # end of fluidRow
                                    ), # end of box
                                    shinydashboard::box(width = 12,
                                                        title = 'Select Time Interval for Deriving Exposures (Original Time Scale)', status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                                        column(width = 6,
                                                               shinyWidgets::pickerInput('min_exp_obs_time_model', label = 'Start Time', choices = character(0), width = '300px', options = list(`live-search` = TRUE))),
                                                        column(width = 6,
                                                               shinyWidgets::pickerInput('max_exp_obs_time_model', label = 'End Time', choices = character(0), width = '300px', options = list(`live-search` = TRUE)))
                                    ), # end of box
                                    shinydashboard::box(width = 12,
                                                        title = 'Plotting Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                                                        fluidRow(
                                                          column(width = 2,
                                                                 selectInput('select_exp', label = 'Select Metric',
                                                                             choices = c("CMAX","CAVG","CMIN","AUC", "TMAX", "TMIN"),
                                                                             selected = 'CMAX',
                                                                             selectize = FALSE)
                                                          ),
                                                          column(width = 2,
                                                                 textInput('exp_yaxis_label', 'Y-axis Label', value = '', placeholder = 'Optional Name')
                                                          ),
                                                          column(width = 2,
                                                                 textInput('exp_model_1_name', 'Model 1 Name', value = 'Model 1')
                                                          ),
                                                          column(width = 2,
                                                                 textInput('exp_model_2_name', 'Model 2 Name', value = 'Model 2')
                                                          ),
                                                          column(width = 2,
                                                                 div(style = "height: 20px;"),  # Empty div to add space
                                                                 checkboxInput('exp_show_model_1', 'Show Model 1', value = TRUE)
                                                          ),
                                                          column(width = 2,
                                                                 div(style = "height: 20px;"),  # Empty div to add space
                                                                 checkboxInput('exp_show_model_2', 'Show Model 2', value = TRUE)
                                                          )
                                                        ),
                                                        fluidRow(
                                                          column(width = 6,
                                                                 textInput('plot_title_exp_model', plot_title_label, value = NULL, placeholder = plot_title_placeholder)
                                                          ),
                                                          column(width = 2,
                                                                 div(style = "height: 20px;"),  # Empty div to add space
                                                                 checkboxInput('exp_display_stats', 'Display Stats', value = TRUE)
                                                          ),
                                                          column(width = 4,
                                                                 div(style = "height: 20px;"),  # Empty div to add space
                                                                 shinyBS::bsPopover('do_exp_plotly', 'Interactive Plot (Slower)' , content = bspop_do_sim_plotly, placement = "top", trigger = "hover"),
                                                                 checkboxInput('do_exp_plotly', 'Interactive Plot (Slower)', value = TRUE)
                                                          )
                                                        )
                                    ),       # end of box
                                    shinydashboard::box(width = 12,
                                                        title = 'Download Options', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                                        column(width = 6,
                                                               textInput('plotly_exp_filename', plotly_filename_label, value = paste0(today_numeric(), '_exp_plot')),
                                                               shinyBS::bsPopover('plotly_exp_filename', title = plotly_filename_label, content = bspop_plotly_file_name_label, placement = 'left', trigger = "focus")
                                                        ),
                                                        column(width = 2,
                                                               selectInput('plotly_exp_format', label = plotly_format_label,
                                                                           choices = c("png","jpeg","svg","webp"),
                                                                           selected = 'png',
                                                                           selectize = FALSE)
                                                        ),
                                                        column(width = 2,
                                                               numericInput('plotly_exp_width', plotly_width_label, value = NULL, min = 1, step = 10),
                                                               shinyBS::bsPopover('plotly_exp_width', title = plotly_width_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus")
                                                        ),
                                                        column(width = 2,
                                                               numericInput('plotly_exp_height', plotly_height_label, value = NULL, min = 1, step = 10),
                                                               shinyBS::bsPopover('plotly_exp_height', title = plotly_height_label, content = bspop_plotly_width_height, placement = 'left', trigger = "focus")
                                                        )
                                    ) # end of box
                                  ) # end of fluidRow
                         ) # end of tabPanel
                       ) # end of tabBox
             ) # end of mainPanel
           ) # sidebarLayout
  ),  # end of tabPanel
  ## Page 6 About ----
  tabPanel('About', icon = icon('circle-info'),
           shinydashboard::box(width = 12,
                               title = 'Background', status = 'primary', solidHeader = TRUE,
                               p('Model Visualization Platform (MVP) is an interactive pharmacometrics environment requiring minimal setup from the users. It is a flexible tool that allows users to explore model structures, parameters, dosing regimens, and compare with observations.'),
                               p('The goal of MVP is to support initial model development and facilitate internal discussions more efficiently.'),
                               p('Additional features include parameter sensitivity analysis, displaying
                 NCA metrics using a time range defined by users, basic data exploration, variability assessment, and more.'),
                               p(tags$span('Disclaimer: The tool is intended for exploratory purposes only. It is not validated for use in regulatory-relevant analyses where quality-controlled analyses with formal documentation are required.', style = "color: red; font-weight: bold;")),
           ),
           shinydashboard::box(width = 12,
                               title = 'Usage Instructions', status = 'primary', solidHeader = TRUE,
                               p('Please refer to the ', a(href = "https://github.com/Boehringer-Ingelheim/MVPapp", "MVPapp GitHub page", target = "_blank"), ' to get started. An abbreviated usage instruction is included below.'),
                               htmltools::br(),
                               p('1. Choose a pre-set model from the "Select Model" option in the "Simulation" page, or enter your own', a(href = "https://mrgsolve.org/", "mrgsolve", target = "_blank"), ' code by selecting "Blank Template" in the list of models. Alternatively a user-supplied .cpp file can be uploaded (and edited) via the "Upload .cpp File" drop-down option.'),
                               p('2. Click "Generate Model" after any necessary code adjustments. Interactive boxes will be generated for each parameter. For model comparison, repeat for Model 2. Note: check the Console below the code editor for potential errors in the code if no parameter value boxes are generated.'),
                               p('3. Adjust the various Dosing Regimens (including any dose transformations and/or modeling duration or rate), Simulation Options, and Plotting Options as required. Note: the plot is interactive, hover over the plot for additional options (download button etc).'),
                               p('4. (Optional): To visualize observations, navigate to the "Data Input" page and upload your own NONMEM-compatible dataset, and apply any custom filtering in the provided code editor. Quick NCA in addition to data exploration is provided in the various sub-tabs on the right hand side. Note: Any uploaded data is erased when the session is closed, and not accessible by anyone else at any time.'),
                               p('5. (Optional) If the dataset is present, it can be displayed by checking the "Overlay Dataset" option inside "Plotting options" in the "Simulation" or "Parameter Sensitivity Analysis" page.'),
                               p('6. Parameter sensitivity analysis can be explored in the corresponding page. All model code, dosing options, and simulations options are carried over from the "Simulation" tab. NCA Metrics is dynamically derived by selecting the desired time intervals.'),
                               p('7. (Optional) After parameter sensitivity analysis, the simulation results containing derived metrics for the middle parameter can be viewed and downloaded from the "Tables" page.'),
                               p('8. (Optional) Random effects can be explored in the Variability page. External population databases can be selected with a range of covariate options to sample from. Covariate distributions can be customized and be used as input for the model. Note: Generated distributions of external population databases can be assessed and downloaded separately.')
           ),
           shinydashboard::box(width = 12,
                               title = 'Known Issues', status = 'primary', solidHeader = TRUE,
                               p('For bug reports or general feedback, please ', a(href = "https://github.com/Boehringer-Ingelheim/MVPapp/issues", "submit an issue on GitHub.", target = "_blank"), ''),
                               p(
                                 tags$ul(
                                   tags$li("Using 'outvars' would sometimes fail to display the plot. A current workaround is to re-define the outvars to a compartment name and then switching back."),
                                   tags$li("Model will crash if model code contains 'R_' pattern which does not refer to modelling rate."),
                                   tags$li("Weight-based dosing is not propagated in IIV models."),
                                   tags$li("When 'Model Duration' is checked and then a dose is inserted into a compartment where the appropriate syntax (e.g. 'D_[CMT]') is not present, the app will crash."),
                                   tags$li("Bad inputs to parameter values (e.g. negative values when there isn't supposed to be one) may crash the app."),
                                   tags$li("Graphical issues when using the Show AUC option with the Log Y axis option."),
                                   tags$li("Maximum upload dataset size is currently limited to 100 MB."),
                                   tags$li("Recommended minimum resolution is 1920 * 1080 pixels in full screen mode.")
                                 )
                               )
           ),
           shinydashboard::box(width = 12,
                               title = 'Authors / Contact Info', status = 'primary', solidHeader = TRUE,
                               p(a(href = "mailto:steve.choy@boehringer-ingelheim.com?subject=Model%20Visualization%20Platform%20(MVP)%20Feedback", "Steve Choy", target = "_blank"), ' (Project Lead & Developer, 2023-2025) '),
                               p("Jin Gyu Kim (Developer, 2023)")
                               #p('Please cite this work as: Kim & Choy. Model Visualization Platform (MVP). American Conference of Pharmacometrics 14 (2023) [Poster].')

           ),
           shinydashboard::box(width = 12,
                               title = 'Acknowledgements', status = 'primary', solidHeader = TRUE,
                               p('The authors would like to acknowledge Nik Onufrak, Jan-George Wojtyniak, Hugo Maas, and the PMxT working group in Boehringer-Ingelheim for their support in this project.'),
                               p('MVP is inspired by various open-source tools, in particular ', a(href = "https://github.com/PavanVaddady/modvizpop", "ModVizPop", target = "_blank"), ' by Pavan Vaddady.')


           ),
           shinydashboard::box(width = 12,
                               title = 'Changelog', status = 'primary', solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                               p('Please visit the ', a(href = "https://github.com/Boehringer-Ingelheim/MVPapp/releases", "Github release page", target = "_blank"), ' for more information.'),
                               htmltools::br(),
                               p('v0.2.14 (2025-02-07) - Exposure box plots for Variability plots. Minor updates to tooltips.'),
                               p('v0.2.13 (2025-02-04) - Expanded features for Individual Plots on Data Input page - dosing info, scaling, and LLOQ. Minor bugfixes for General Plot and Individual Plots.'),
                               p('v0.2.12 (2025-01-03) - Mean value display for Variability Plot. Allows minimum total doses to be 0. Individual Plots for Datasets. Multiple facets for General Plot. Progress bars for long computations.'),
                               p('v0.2.11 (2024-12-04) - Support for uploading tab-delimited .txt files on Data Upload (also option to automatically create ID column). Uses dose column instead of manually entering dose amount for NCA. NCA unit and report bugfixes. Download Options now support non-interactive plots.'),
                               p('v0.2.10 (2024-11-27) - Support for mrgsolve v1.5.2. Fixed some typos. Minor QoL improvements to selectizeInputs to sort alphabetically. Interactive plot toggle for Sim and Data page. Less strict dataset requirements for Data Exploration plots. Boxplot functionality for Data Exploration - Plot Output.'),
                               p('v0.2.9 (2024-10-20) - Smoother, linear regression, and facet options for Data Page Plot Output. Consistently include dates for all downloadable files. Bugfix to disallow PSA plots when previous model fails to compile. Bugfix for Median Line binning.'),
                               p('v0.2.8 (2024-10-12) - More template models (1 CMT lag time). Increasing default decimal places (3 -> 5) for variability quantiles. Bugfix for WT-based dosing to propagate correctly to PSA models when WT is a parameter. Supports $PRED syntax. Using recover = TRUE in mcode by default to output more helpful error messages when mrgsolve does not compile.'),
                               p('v0.2.7 (2024-09-30) - Re-factoring for package. Minor bugfixes and optimizations.'),
                               p('v0.2.6 (2024-09-12) - More informative error messages when non-NONMEM formatted data is being uploaded. NCA support in Data Input tab (experimental). More tooltips. Minor bugfixes.'),
                               p('v0.2.5 (2024-08-15) - Split demographics tables by Sex in Variability tab.'),
                               p('v0.2.4 (2024-08-06) - Support for mrgsolve v1.5.1 (download model code with the mwrite_cpp() function).'),
                               p('v0.2.3 (2024-07-12) - Smarter automatic handling of common x-axis time scales.'),
                               p('v0.2.2 (2024-07-10) - UI improvements. Option to disable interactive plots to improve performance in Variability tab. Automatic detection when modeling rate or duration.'),
                               p('v0.2.1 (2024-07-02) - Minor performance improvements when drawing from databases for Variability simulations. Minor tooltip changes. Supports .cpp file uploads for models.'),
                               p('v0.2.0 (2024-06-06) - Support simulation of custom covariates distributions (up to 3 covariates) for demographics in the Variability tab. Minor cosmetic changes. Corrected code for PK with sequential zero and first-order absorption model.'),
                               p('v0.1.19 (2024-05-14) - New Template models (PK with sequential zero and first-order absorption, TTE models for weibull and log-logistic). Rounding options for parameter sensitivity analysis metrics. Minor label changes.'),
                               p('v0.1.18 (2024-04-24) - Minor label changes. Transpose option for Summary Statistics data table. Bugfix to include Model 2 data when downloading simulated data. Log-axis extension before scientific notation is used.'),
                               p('v0.1.17 (2024-02-26) - Support for evtools plugin (mrgsolve v1.4.1). Fixed range labels for Model 2 in Variability Tab to be consistent with Model 1.'),
                               p('v0.1.16 (2024-01-11) - Allows filtering of CMT for Data Input -> Plot Output -> Data Exploration.'),
                               p('v0.1.15 (2024-01-08) - Minor label changes and bug fixes. Sampling points [dataset] option for Variability simulations. More safety checks for custom sampling. Bugfix in Data Exploration to enable newly created column names to be searchable in Plots. Plot titles for simulation plots.'),
                               p('v0.1.14 (2023-12-04) - Support modeling of rate ("R_xxx"). Minor refactoring and renaming of models and labels. Removed watermark for public release.'),
                               p('v0.1.13 (2023-11-22) - Median line can now be colored by a y-variable for the Data and Simulation page. Minor bugfixes.'),
                               p('v0.1.12 (2023-11-16) - Minor template model code changes, text changes, and UI adjustments. Safeguard for high number of subjects. QoL error notifications. Safeguard for dataset inputs.'),
                               p('v0.1.11 (2023-10-30) - Color scheme change.'),
                               p('v0.1.10 (2023-10-04) - App-wide authentication mode support. Console bug fixes before any model is run (R4).'),
                               p('v0.1.9  (2023-09-29) - Fixed a bug where multiple concurrent users would crash due to shared global mrgsolve object names. R4 compatibility check.'),
                               p('v0.1.8  (2023-09-11) - Weight-based dosing is now supported, and testing automatic tick sizes for Time in weeks. Minor text changes.'),
                               p('v0.1.7  (2023-09-07) - Password input available to unlock hidden models (with disclaimer), minor text changes and re-factoring.'),
                               p('v0.1.6  (2023-08-25) - Deprecated css format script, more info on databases script, cleaning up functions, minor text changes.'),
                               p('v0.1.5  (2023-08-24) - Minor text changes, layout changes, code-refactoring, and threshold calculation in Variability tab.'),
                               p('v0.1.4  (2023-08-18) - Model 1 / Model 2 variability can now be displayed together in the same plot.'),
                               p('v0.1.3  (2023-08-17) - Supporting external patient databases (beta), dataset filtering bugfixes.'),
                               p('v0.1.2  (2023-08-15) - Minor clean up in text and functions, more tooltips, and bug fixes in example models.'),
                               p('v0.1.1  (2023-08-14) - Variability Tab implemented (beta).'),
                               p('v0.1.0  (2023-08-09) - Stable release with model comparison and console messages.')
           )
  ),                  # end of tabPanel
  if(!internal_version) {
    ## Page 7 Legal ----
    tabPanel('Impressum', icon = icon('gavel'),
             shinydashboard::box(width = 12,
                                 title = 'Legal Disclaimer', status = 'primary', solidHeader = TRUE,
                                 p('This App has been created for personal use only. The use of any result generated is in any case the sole risk and responsibility of the user. Decisions in drug development should not solely rely on the App as information provided by the App does not replace scientific judgement. There is no guarantee for the accuracy of the provided results. When using the App you automatically agree with this disclaimer and the legal notices.'),
                                 p('The authors reserves the right not to be responsible for the topicality, correctness, completeness or quality of the information provided. Liability claims regarding damage caused by the use of any information provided, including any kind of information which is incomplete or incorrect, will therefore be rejected. All offers are not-binding and without obligation. Parts of the pages or the complete publication including all offers and information might be extended, changed or partly or completely deleted by the author without separate announcement.')
             ),
             shinydashboard::box(width = 12,
                                 title = 'Impressum', status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                                 p('Boehringer Ingelheim Pharma GmbH & Co. KG'),
                                 p('Binger Strasse 173'),
                                 p('D-55216 Ingelheim am Rhein'),
                                 htmltools::br(),
                                 p('Handelsregister'),
                                 p('Registergericht Mainz HR A 22206'),
                                 htmltools::br(),
                                 p('vertreten durch die Komplementärin Boehringer Ingelheim Deutschland GmbH'),
                                 htmltools::br(),
                                 p('Geschäftsführung:'),
                                 p('Dr. Sabine Nikolaus (Vorsitzende),'),
                                 p('Martin Beck, Jan Faßbender, Christjan Knudsen, Andreas Krüger;'),
                                 htmltools::br(),
                                 p('Vorsitzende des Aufsichtsrates: Dr. Elke Simon;'),
                                 p('Sitz: Ingelheim am Rhein; Registergericht Mainz: HR B 23260'),
                                 htmltools::br(),
                                 p('USt.Id.-Nr. DE 143290578'),
                                 htmltools::br(),
                                 p('Telefon: +49 61 32 77 0'),
                                 p('Telefax: +49 61 32 77 30 00'),
                                 p('E-Mail: press@boehringer-ingelheim.com'),
                                 htmltools::br(),
                                 p('Die Boehringer Ingelheim Pharma GmbH & Co. KG ist nicht bereit oder verpflichtet, an Streitbeilegungsverfahren vor einer Verbraucherschlichtungsstelle teilzunehmen.'),
                                 htmltools::br(),
                                 p('Zuständige Aufsichtsbehörden'),
                                 p('Standort Ingelheim am Rhein:'),
                                 p('Landesamt für Soziales, Jugend und Versorgung in Mainz'),
                                 p('Rheinallee 97 - 101'),
                                 p('55118 Mainz'),
                                 htmltools::br(),
                                 p('Standort Biberach an der Riss:'),
                                 p('Regierungspräsidium Tübingen'),
                                 p('Konrad-Adenauer-Straße 20'),
                                 p('72072 Tübingen)'),
             )
    )
  },
  useShinydashboardMVP(),
  tags$script(src = "https://kit.fontawesome.com/<you>.js"),
) # end of ui

# server ----
server <- function(input, output, session) {

  if(!is.null(authentication_code)) {
    # Function to show the authentication modal
    show_auth_modal <- function() {
      showModal(modalDialog(
        title = "Authentication",
        textInput("auth_code", "Enter password:"),
        footer = tagList(
          actionButton("submit_code", " Submit",
                       icon = icon("key"))
        ),
        easyClose = FALSE,
        fade = FALSE
      ))
    }

    # Show the authentication modal on app startup
    show_auth_modal()

    # Observe the submit button click
    observeEvent(input$submit_code, {
      if (!is.na(input$auth_code)) {
        if (input$auth_code == authentication_code) {
          removeModal()
        }
      }
    })
  } # end authentication

  # Page 1 Data Input ----
  ## uploaded_data() ----
  # Disable the checkboxes
  shinyjs::disable("change_all_to_upper") # Always required and not changeable by the user
  shinyjs::disable("remove_pound_sign")   # Always required and not changeable by the user
  shinyjs::disable("create_cmt_col")   # Always required and not changeable by the user

  uploaded_data <- reactive({
    shiny::req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(
      ext,
      #csv = read.csv(input$upload$datapath, sep = ","),
      csv = data.table::fread(input$upload$datapath, sep = ","),
      txt = data.table::fread(input$upload$datapath, sep = "\t"), # assumes tab-delimited
      shiny::validate("Invalid file; Please upload a .csv or .txt (tab-delimited) file")
    )
  }, label = 'uploaded_nm_data')

  # Data after checkBoxed:
  ## built_in_filtered_data() ----
  built_in_filtered_data <- reactive({
    tmp <- uploaded_data()

    if(input$change_all_to_upper) {
      tmp <- tmp %>% dplyr::rename_all(toupper)
    }

    if(input$remove_pound_sign) {
      tmp <- tmp %>% dplyr::rename_all(~stringr::str_replace_all(., "#", ""))
      tmp <- tmp %>% dplyr::rename_all(~stringr::str_replace_all(., "@", ""))
    }

    if (input$EVID_filter) {
      if ('EVID' %in% names(tmp)) {
        tmp <- tmp %>% dplyr::filter(EVID != 1, EVID != 4)
      }
    }

    if (input$BLQ_filter) {
      if('BLQ' %in% names(tmp)) {
        tmp <- tmp %>% dplyr::filter(BLQ < 1)
      }
    }

    if (input$turn_all_numeric) {
      tmp <- as.data.frame(lapply(tmp, function(x) as.numeric(as.character(x))))
    }

    if(input$create_cmt_col) {
      if(!('CMT' %in% names(tmp))) {
        tmp <- tmp %>% dplyr::mutate(CMT = 2)
      }
    }

    if (input$create_id_col) {
      tmp <- search_id_col(tmp, names_of_id_cols = c("SUBJIDN", "SUBJID", "USUBJID", "PTNO"))
    }

    if (input$create_time_col) {
      tmp <- search_time_col(tmp, names_of_time_cols = c("TAFD", "TSFD", "ATFD", "ATSD"))
    }

    return(tmp)
  }, label = 'built_in_filtered_data')

  ### Update selectize columns  ----
  observeEvent(uploaded_data(), {
    updateSelectizeInput(session, "column",
                         choices = names(built_in_filtered_data())
    )
  }, label = 'updated_column_after_upload')

  # Integrated with built_in_filtered_data and selectized columns:
  ## de_selectized_data() ----
  de_selectized_data <- reactive({
    tmp <- built_in_filtered_data()

    if(!is.null(input$column)) {
      tmp <- tmp %>% dplyr::select(!dplyr::all_of(input$column))
    }

    return(tmp)
  }, label = 'de_selectized_data')

  ### Enable auto completion with live ----
  observe({
    autoComplete <- if (input$enableAutocomplete) {
      'live'
    } else {
      'disabled'
    }
    if (input$enableAutocomplete) {
      mutateOb <- shinyAce::aceAutocomplete('codes')
      mutateOb$resume()
    }
    shinyAce::updateAceEditor(session, 'codes', autoComplete = autoComplete)
  }, label = 'ShinyAce autocompletion')

  observe({
    input$enableAutocomplete
    comps <- colnames(de_selectized_data())
    shinyAce::updateAceEditor(session, "codes", autoCompleteList = comps)
  }, label = 'ShinyAce autocomplete columns')

  # Integrated data selection between built_in_filtered_data and de_selectized_data:
  ## nmdata() ----
  nmdata <- reactive({
    tmp_data <- uploaded_data()

    if (!is.null(input$EVID_filter | input$BLQ_filter)) {
      tmp_data <- built_in_filtered_data()
    }
    if (!is.null(input$column)) {
      tmp_data <- de_selectized_data()
    }
    if (show_debugging_msg) {
      message('nmdata() formed')
    }
    return(tmp_data)
  }, label = 'nmdata() before addtional filtering')

  safely_nmdata_code <- reactive({
    if(is.data.frame(nmdata())) {

      if (show_debugging_msg) {
        message("nmdata() is dataframe")
      }

      nmdata_global     <<- nmdata() # globally assigned as shinyAce editor is not scoped to access this environment
      parsed_text <- safely_parse(text = paste('nmdata_global <<- nmdata_global %>% ', input$codes))

      if (show_debugging_msg) {
        message(parsed_text$result)
      }

      eval_text   <- safely_eval(parsed_text$result)
      return(eval_text)
    } else {
      eval_text <- list()
      eval_text$error <- "nmdata() is not a dataframe"
      eval_text$result <- NULL
      return(eval_text)
    }
  }, label = 'Evaluate Safely additional filtering code box') %>% bindEvent(input$eval_button, nmdata())


  nmdata_code_is_valid <- reactiveVal(FALSE)

  observe({
    if(is.null(safely_nmdata_code()$error)) {
      if(is.data.frame(safely_nmdata_code()$result)) {

        if (show_debugging_msg) {
          message('The dataset and code is valid')
        }

        nmdata_code_is_valid(TRUE)
      }
    } else {

      if (show_debugging_msg) {
        message('Invalid Code')
      }

      nmdata_code_is_valid(FALSE)
      shiny::showNotification("ERROR: Dataset cleaning code failed to run. Check Console Error Message for more info.", type = "error", duration = 10)
    }
  }, label = 'nmdata_code_is_valid')

  # Additional_filtered_data
  ## final_output() ----
  final_output <- reactive({
    nonmem_dataset <- nmdata()

    if (nmdata_code_is_valid()) {
      nonmem_dataset <- safely_nmdata_code()$result
      if (show_debugging_msg) {
        message('Returning uploaded dataset as final_output()')
      }
    }

    return(nonmem_dataset)
  }, label = 'Final output after additional filtering')

  observe({
    if ('CMT' %in% names(final_output())) {
      unique_cmt_values <- sort(as.numeric(unique(final_output()$CMT)))
    } else {
      unique_cmt_values <- NULL
    }

    unique_cmt_values <- c('NULL', unique_cmt_values)

    updateSelectizeInput(session,
                         "filter_cmt",
                         choices = unique_cmt_values,
                         selected = 'NULL')

    updateSelectizeInput(session,
                         "filter_cmt_data",
                         choices = unique_cmt_values,
                         selected = 'NULL')
  }, label = 'Update selectizeInput nmdata() filtered CMT')

  output$console_data_1 <- renderPrint({
    if(!nmdata_code_is_valid()) {
      print(safely_nmdata_code()$error)

    }
  }) %>% bindEvent(nmdata_code_is_valid(), safely_nmdata_code())

  final_output_executed <- reactiveVal(FALSE)
  id_is_in_dataset      <- reactiveVal(FALSE) # A less strict dataset for Data Exploration, only requiring "ID" column

  # Additional processing for nmdata plot
  ## nmdataset_for_plot() ----
  nmdataset_for_plot <- reactive({

    if (show_debugging_msg) {
      message('Creating nmdataset_for_plot()')
    }

    nonmem_dataset <- final_output()

    if('ID' %in% names(nonmem_dataset)) {
      nonmem_dataset <- nonmem_dataset %>% dplyr::mutate(ID   = as.character(ID))
      nonmem_dataset$ID <- as.factor(nonmem_dataset$ID)
      nonmem_dataset$ID <- forcats::fct_inorder(nonmem_dataset$ID) # factor levels in order by which they appear
      id_is_in_dataset(TRUE)
    } else {
      id_is_in_dataset(FALSE)
    }

    if('TIME' %in% names(nonmem_dataset)) {
      nonmem_dataset <- nonmem_dataset %>% dplyr::mutate(TIME = as.numeric(TIME))
    }

    if('DV' %in% names(nonmem_dataset)) {
      nonmem_dataset <- nonmem_dataset %>% dplyr::mutate(DV   = as.numeric(DV))
    }

    if (all(c('ID', 'TIME', 'DV') %in% names(final_output()))) {

      if (show_debugging_msg) {
        message('Found ID TIME DV')
      }
      final_output_executed(TRUE)
    } else {

      final_output_executed(FALSE)
      shiny::showNotification("ERROR: Not all required columns (ID, TIME, DV) are present. Data overlay options are disabled.", type = "error", duration = 12)
      updateCheckboxInput(session, "turn_all_numeric", value = FALSE)
      shiny::showNotification("WARNING: Dataset appears to not be NONMEM-formatted. Re-filtering to retain characters...", type = "warning", duration = 12)
    }

    if (show_debugging_msg) {
      dplyr::glimpse(nonmem_dataset)
      message(paste0("final_output_executed() status: ", final_output_executed()))
    }

    return(nonmem_dataset)
  }, label = 'nmdataset_for_plot')

  ### Update variables for inputted_file ----
  observeEvent(nmdataset_for_plot(), {
    #if(final_output_executed()) {
    if (show_debugging_msg) {
      message('Attempting to update default variables for nmdataset_for_plot()')
    }

    updateSelectizeInput(session,
                         "x_axis",
                         choices = names(nmdataset_for_plot()) %>% sort(),
                         selected = dplyr::case_when("TIME" %in% names(nmdataset_for_plot()) ~ "TIME",
                                                     "TAFD" %in% names(nmdataset_for_plot()) ~ "TAFD",
                                                     "ATFD" %in% names(nmdataset_for_plot()) ~ "ATFD",
                                                     "TSFD" %in% names(nmdataset_for_plot()) ~ "TSFD",
                                                     "TSLD" %in% names(nmdataset_for_plot()) ~ "TSLD",
                                                     "ARELTMSL" %in% names(nmdataset_for_plot()) ~ "ARELTMSL",
                                                     "ARTMSLR" %in% names(nmdataset_for_plot()) ~ "ARTMSLR",
                                                     "ARELTMEL" %in% names(nmdataset_for_plot()) ~ "ARELTMEL",
                                                     TRUE                                        ~ "")
    )

    updateSelectizeInput(session,
                         "y_axis",
                         choices = names(nmdataset_for_plot()) %>% sort(),
                         selected = dplyr::case_when("DV" %in% names(nmdataset_for_plot()) ~ "DV",
                                                     #"AVAL" %in% names(nmdataset_for_plot()) ~ "AVAL",
                                                     #"AVALREP" %in% names(nmdataset_for_plot()) ~ "AVALREP",
                                                     #"ACVALREP" %in% names(nmdataset_for_plot()) ~ "ACVALREP",
                                                     TRUE                                        ~ "")
    )

    updateSelectizeInput(session,
                         "color",
                         choices = names(nmdataset_for_plot()) %>% sort(),
                         selected = '')

    updateSelectizeInput(session,
                         "median_line_by",
                         choices = names(nmdataset_for_plot()) %>% sort(),
                         selected = '')

    updateSelectizeInput(session,
                         "var_corr",
                         choices = names(nmdataset_for_plot()) %>% sort())

    updateSelectizeInput(session,
                         "facet_by",
                         choices = names(nmdataset_for_plot()) %>% sort(),
                         selected = NULL)

    if('DOSE' %in% names(nmdataset_for_plot())) {
      updateSelectizeInput(session,
                           "color_corr",
                           choices = names(nmdataset_for_plot()) %>% sort(),
                           selected = 'DOSE')
    }

    ## Update selections for NCA
    updateSelectizeInput(session,
                         "subject_colname",
                         choices = names(nmdataset_for_plot()) %>% sort(),
                         selected = dplyr::case_when("SUBJID" %in% names(nmdataset_for_plot()) ~ "SUBJID",
                                                     "SUBJIDN" %in% names(nmdataset_for_plot()) ~ "SUBJIDN",
                                                     "ID" %in% names(nmdataset_for_plot()) ~ "ID",
                                                     "USUBJID" %in% names(nmdataset_for_plot()) ~ "USUBJID")
    )

    updateSelectizeInput(session,
                         "time_colname",
                         choices = names(nmdataset_for_plot()) %>% sort(),
                         selected = dplyr::case_when("ARELTMSL" %in% names(nmdataset_for_plot()) ~ "ARELTMSL",
                                                     "TIME" %in% names(nmdataset_for_plot()) ~ "TIME",
                                                     "TAFD" %in% names(nmdataset_for_plot()) ~ "TAFD",
                                                     "ATFD" %in% names(nmdataset_for_plot()) ~ "ATFD",
                                                     "TSFD" %in% names(nmdataset_for_plot()) ~ "TSFD",
                                                     "TSLD" %in% names(nmdataset_for_plot()) ~ "TSLD",
                                                     "ARTMSLR" %in% names(nmdataset_for_plot()) ~ "ARTMSLR",
                                                     "ARELTMEL" %in% names(nmdataset_for_plot()) ~ "ARELTMEL")
    )

    updateSelectizeInput(session,
                         "conc_colname",
                         choices = names(nmdataset_for_plot()) %>% sort(),
                         selected = dplyr::case_when("AVALREP" %in% names(nmdataset_for_plot()) ~ "AVALREP",
                                                     "ACVALREP" %in% names(nmdataset_for_plot()) ~ "ACVALREP",
                                                     "DV" %in% names(nmdataset_for_plot()) ~ "DV",
                                                     "AVAL" %in% names(nmdataset_for_plot()) ~ "AVAL")
    )

    updateSelectizeInput(session,
                         "dose_colname",
                         choices = names(nmdataset_for_plot()) %>% sort(),
                         selected = dplyr::case_when("DOSEA" %in% names(nmdataset_for_plot()) ~ "DOSEA",
                                                     "DOSEP" %in% names(nmdataset_for_plot()) ~ "DOSEP",
                                                     "DOSEAM" %in% names(nmdataset_for_plot()) ~ "DOSEAM",
                                                     "DOSE" %in% names(nmdataset_for_plot()) ~ "DOSE")
    )

    updateSelectizeInput(session,
                         "additional_keys",
                         choices = names(nmdataset_for_plot()) %>% sort(),
                         selected = dplyr::case_when("ATRTCD" %in% names(nmdataset_for_plot()) ~ "ATRTCD",
                                                     "ATRT" %in% names(nmdataset_for_plot()) ~ "ATRT",
                                                     "DOSE" %in% names(nmdataset_for_plot()) ~ "DOSE",
                                                     "DOSEA" %in% names(nmdataset_for_plot()) ~ "DOSEA",
                                                     "DOSEMG" %in% names(nmdataset_for_plot()) ~ "DOSEMG",
                                                     #"AMT" %in% names(nmdataset_for_plot()) ~ "AMT",
                                                     TRUE ~ "DUMMY")
    )

    if('ID' %in% names(nmdataset_for_plot())) {
      updateSelectizeInput(session,
                           "filter_by_id",
                           choices = unique(nmdataset_for_plot()$ID),
                           selected = NULL,
                           server = TRUE
      )
    }

    updateSelectizeInput(session,
                         "highlight_var",
                         choices = names(nmdataset_for_plot()) %>% sort(),
                         selected = ''
    )

    updateSelectizeInput(session,
                         "ind_dose_colname",
                         choices = names(nmdataset_for_plot()) %>% sort(),
                         selected = dplyr::case_when("DOSE" %in% names(nmdataset_for_plot()) ~ "DOSE",
                                                     "DOSEMG" %in% names(nmdataset_for_plot()) ~ "DOSEMG",
                                                     "AMT" %in% names(nmdataset_for_plot()) ~ "AMT",
                                                     "AMTNM" %in% names(nmdataset_for_plot()) ~ "AMT",
                                                     TRUE                                        ~ "")
    )

    updateSelectizeInput(session,
                         "lloq_colname",
                         choices = names(nmdataset_for_plot()) %>% sort(),
                         selected = dplyr::case_when("LLOQ" %in% names(nmdataset_for_plot()) ~ "LLOQ",
                                                     TRUE                                        ~ "")
    )

    #}
  }, label = 'Update selectizeInput after uploaded_data()')

  observeEvent(c(nmdataset_for_plot(), input$number_of_rows, input$number_of_cols), {
    if('ID' %in% names(nmdataset_for_plot())) {
      updateSelectizeInput(session,
                           "page_number",
                           choices = 1:(ceiling(length(unique(nmdataset_for_plot()$ID)) / (as.numeric(input$number_of_rows) * as.numeric(input$number_of_cols)))),
                           selected = 1
      )
    }
  }, label = 'Update page_number')

  observeEvent(c(nmdataset_for_plot(), input$highlight_var), {
    if(!is.null(input$highlight_var) && input$highlight_var != '') {
      updateSelectizeInput(session,
                           "highlight_var_values",
                           choices = unique(nmdataset_for_plot()[[input$highlight_var]]) %>% sort(),
                           selected = ''
      )
    }
  }, label = 'Update highlight_var_values')

  ## Download nmdataset_for_plot() ----
  output$download_nmdataset_for_plot <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_nonmem_data_cleaned.csv")
    },
    content = function(file) {
      #write.csv(nmdataset_for_plot(), file, quote = FALSE, row.names = FALSE)
      data.table::fwrite(nmdataset_for_plot(), file, quote = FALSE, row.names = FALSE)
    })

  # table output of nmdata with code evaluation
  ## UI: output$dataset_page_table ----
  output$dataset_page_table <- DT::renderDataTable({

    if (show_debugging_msg) {
      message('Creating dataset_page_table')
    }

    DT::datatable(nmdataset_for_plot(),
                  rownames = FALSE,
                  options = list(
                    initComplete = DT::JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#08312A', 'color': '#00E47C'});",
                      "}")))
  })

  ## UI: output$data_info ----
  # Summary Stats
  data_summary_stats <- reactive({
    if (show_debugging_msg) {
      message('Attempting to calc_summary_stats on nmdataset_for_plot()')
    }
    nmdataset_for_plot() %>% calc_summary_stats(transpose = input$transpose_data_info, comma_format = FALSE)
  })

  output$data_info <- DT::renderDataTable({
    DT::datatable(data_summary_stats(),
                  rownames = FALSE,
                  options = list(
                    initComplete = DT::JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#08312A', 'color': '#00E47C'});",
                      "}")
                  )

    )
  })

  output$download_data_info <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_nonmem_data_summ_stats.csv")
    },
    content = function(file) {
      #write.csv(data_summary_stats(), file, quote = FALSE, row.names = FALSE)
      data.table::fwrite(data_summary_stats(), file, quote = FALSE, row.names = FALSE)
    })

  ## UI: output$descriptive_stats ----
  stat_table <- reactive({
    shiny::req(nmdataset_for_plot())

    if(input$mw_value == 0) {
      shiny::showNotification("WARNING: Set Molecular Weight to a non-zero value to get Clearance and Volume estimates.", type = "warning", duration = 10)
    }

    nmdataset_for_nca <- nmdataset_for_plot() %>% # sort by ID and TIME to prevent "Check if the x is sorted in order!" error
      arrange(!!dplyr::sym(input$subject_colname), !!dplyr::sym(input$time_colname))

    doses_by_id <- nmdataset_for_nca %>%
      dplyr::select(!!dplyr::sym(input$subject_colname), !!dplyr::sym(input$additional_keys), !!dplyr::sym(input$dose_colname)) %>%
      dplyr::distinct() %>%
      .[[input$dose_colname]]

    tblNCA_progress(nmdataset_for_nca, ## originally was NonCompart::tblNCA, modified to support progress bars
                    key = c(input$subject_colname, input$additional_keys),
                    colTime = input$time_colname,
                    colConc = input$conc_colname,
                    dose = doses_by_id,
                    #dose = input$dose_value,
                    adm = input$adm_route,
                    dur = input$dur_inf,
                    doseUnit = input$desc_dose_unit,
                    timeUnit = input$desc_time_unit,
                    concUnit = input$desc_conc_unit,
                    down = input$down_method,
                    #SS = input$SS_value,
                    #R2ADJ = input$R2ADJ_value,
                    MW = input$mw_value) %>%

      dplyr::select(where(~any(!is.na(.)))) ## Remove columns with NA's if inputs are non-sensible

  }) %>%
    bindEvent(input$calc_nca)

  # Create a named vector for renaming
  rename_vectors <- reactive({

    list_of_name_pairs <- c("CMAX"     = paste0("Cmax (", input$desc_conc_unit, ")"),
                            "CMAXD"    = paste0("Cmax,norm (", input$desc_conc_unit, "/", input$desc_dose_unit, ")"),
                            "AUCLST"   = paste0("AUClast (", input$desc_time_unit, "*", input$desc_conc_unit, ")"),
                            #"AUCIFO"   = paste0("AUCinf,obs (", input$desc_time_unit, "*", input$desc_conc_unit, ")"),
                            "AUCIFP"   = paste0("AUCinf,pred (", input$desc_time_unit, "*", input$desc_conc_unit, ")"),
                            "AUCIFPD"  = paste0("AUCinf,pred_norm (", input$desc_time_unit, "*", input$desc_conc_unit, ")"),
                            #"AUCPEO"   = paste0("AUC%extrap,obs (%)"),
                            "AUCPEO"   = paste0("AUC%extrap,pred (%)"),
                            "LAMZHL"   = paste0("T1/2,lambdaz (", input$desc_time_unit, ")"),
                            "TMAX"     = paste0("Tmax (", input$desc_time_unit, ")"),
                            "MRTIVLST" = paste0("MRT,iv (", input$desc_time_unit, ")"),
                            "MRTIVIFO" = paste0("MRT,iv_inf (", input$desc_time_unit, ")"),
                            "MRTIVLFP" = paste0("MRT,iv_inf_pred (", input$desc_time_unit, ")"),
                            "MRTEVLST" = paste0("MRT,ex_tlast (", input$desc_time_unit, ")"),
                            "MRTEVIFO" = paste0("MRT,ex_inf (", input$desc_time_unit, ")"),
                            "MRTEVIFP" = paste0("MRT,ex_inf_pred (", input$desc_time_unit, ")"),
                            "VZO"      = paste0("Vz,obs (L)"),
                            "VZP"      = paste0("Vz,pred (L)"),
                            "VZFO"     = paste0("Vz/F,obs (L)"),
                            "VZFP"     = paste0("Vz/F,pred (L)"),
                            "CLO"      = paste0("CL,obs (L/", input$desc_time_unit, ")"),
                            "CLP"      = paste0("CL,pred (L/", input$desc_time_unit, ")"),
                            "CLFO"     = paste0("CL/F,obs (L/", input$desc_time_unit, ")"),
                            "CLFP"     = paste0("CL/F,pred (L/", input$desc_time_unit, ")"),
                            "DUMMY"    = paste0("dummy_placeholder")
    )
    return(list_of_name_pairs)
  })

  output$descriptive_stats_summary <- renderUI({
    shiny::req(stat_table())
    shiny::req(rename_vectors())

    rename_vectors_reverse <- setNames(names(rename_vectors()), unname(rename_vectors()))
    rename_vectors_exist   <- rename_vectors_reverse[names(rename_vectors()) %in% colnames(stat_table())]

    # Rename the columns
    stat_table_renamed <- dplyr::rename(stat_table(), !!!rename_vectors_exist)

    if(show_debugging_msg) {
      message(rename_vectors_exist)
      dplyr::glimpse(stat_table_renamed)
    }

    list_of_nca <- calc_summary_stats_as_list(stat_table_renamed,
                                              group_by_name = input$additional_keys,
                                              list_of_nca_metrics = names(rename_vectors_reverse),
                                              transpose = input$transpose_nca,
                                              dp = 3,
                                              sigdig = TRUE,
                                              id_colname = input$subject_colname)

    withProgress(message = "Tabulating NCA", value = 0, {
      for(i in 1:length(list_of_nca)) {
        setProgress(value = i / length(list_of_nca))
        if(input$transpose_nca) {
          list_of_nca[[i]] <- list_of_nca[[i]] %>%
            flextable::flextable() %>%
            flextable::font(font = "Arial") %>%
            flextable::bold(part = "header") %>%
            flextable::add_header_lines(paste0(input$additional_keys, " = ", unique(stat_table_renamed[[input$additional_keys]])[i])) %>%
            flextable::bold(j = c("gMean")) %>%
            flextable::autofit() %>%
            flextable::theme_zebra() %>%
            flextable::htmltools_value()
        } else {
          max_n <- max(list_of_nca[[i]]$N) # removing N from Statistic to be technically correct
          list_of_nca[[i]] <- list_of_nca[[i]] %>%
            dplyr::select(-N) %>%
            flextable::flextable() %>%
            flextable::font(font = "Arial") %>%
            flextable::bold(part = "header") %>%
            flextable::add_header_lines(paste0(input$additional_keys, " = ", unique(stat_table_renamed[[input$additional_keys]])[i], " (n=", max_n, ")")) %>%
            flextable::bold(i = 6) %>%
            flextable::autofit() %>%
            flextable::theme_zebra() %>%
            flextable::htmltools_value()
        }
      }
    })

    fluidRow(list_of_nca)
  })

  output$descriptive_stats <- DT::renderDataTable({
    shiny::req(nmdataset_for_plot())
    if (show_debugging_msg) {
      message('Creating descriptive_stats')
    }

    DT::datatable(stat_table(),
                  rownames = FALSE,
                  options = list(
                    initComplete = DT::JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#08312A', 'color': '#00E47C'});",
                      "}")))
  })

  output$download_descriptive_stats <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_descriptive_stats.csv")
    },
    content = function(file) {
      #write.csv(stat_table(), file, quote = FALSE, row.names = FALSE)
      data.table::fwrite(stat_table(), file, quote = FALSE, row.names = FALSE)
    })

  output$download_nca_report <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_nca_report.pdf")
    },
    content = function(file) {

      nmdataset_for_nca2 <- nmdataset_for_plot() %>% # sort by ID and TIME to prevent "Check if the x is sorted in order!" error
        arrange(!!dplyr::sym(input$subject_colname), !!dplyr::sym(input$time_colname))

      doses_by_id2 <- nmdataset_for_nca2 %>%
        dplyr::select(!!dplyr::sym(input$subject_colname), !!dplyr::sym(input$additional_keys), !!dplyr::sym(input$dose_colname)) %>%
        dplyr::distinct() %>%
        .[[input$dose_colname]]

      withProgress(message = 'Generating report', value = 0, {
        pdfNCA_wm(fileName = file, ## originally was ncar::pdfNCA, modified to support progress bars and watermarks in plots
                  nmdataset_for_nca2,
                  key = c(input$subject_colname, input$additional_keys),
                  colTime = input$time_colname,
                  colConc = input$conc_colname,
                  dose = doses_by_id2,
                  adm = input$adm_route,
                  dur = input$dur_inf,
                  doseUnit = input$desc_dose_unit,
                  timeUnit = input$desc_time_unit,
                  concUnit = input$desc_conc_unit,
                  down = input$down_method,
                  #SS = input$SS_value,
                  #R2ADJ = input$R2ADJ_value,
                  MW = input$mw_value,
                  watermark = insert_watermark,
                  internal_version = internal_version,
                  debug_msg = FALSE,
                  show_progress = TRUE)
      })


    })

  # Data output plotting
  ## UI: output$dataset_page_plot ----

  dataset_page_plot <- reactive({
    if (show_debugging_msg) {
      message('Creating dataset_page_plot')
    }

    if (id_is_in_dataset()) {
      if(input$x_axis == "" | input$y_axis == "") { # Dummy panel
        return(ggplot2::ggplot() + ggplot2::theme(panel.background = ggplot2::element_blank()))
      }

      shiny::req(input$x_axis)
      shiny::req(input$y_axis)

      # data_to_plot <- trim_columns(data = nmdataset_for_plot(),
      #                              x_axis = input$x_axis,
      #                              y_axis = input$y_axis,
      #                              color = input$color,
      #                              type_of_plot = "general_plot",
      #                              facet_name = input$facet_by,
      #                              insert_med_line = input$median_line_data,
      #                              med_line_by = input$median_line_by)#,
      #                              #ind_dose_colname = input$ind_dose_colname,
      #                              #highlight_var = input$highlight_var)

      a <- do_data_page_plot(nmd         = nmdataset_for_plot(), #data_to_plot,
                             filter_cmt  = input$filter_cmt_data,
                             x_axis      = input$x_axis,
                             y_axis      = input$y_axis,
                             color_by    = input$color,
                             med_line    = input$median_line_data,
                             med_line_by = input$median_line_by,
                             boxplot     = input$do_boxplot,
                             dolm        = input$insert_lm_eqn,
                             smoother    = input$insert_smoother,
                             facet_name  = input$facet_by,
                             logy        = input$log_y_axis_data,
                             logx        = input$log_x_axis_data,
                             lby         = logbreaks_y,
                             lbx         = logbreaks_x,
                             plot_title  = input$plot_title_data,
                             label_size  = as.numeric(input$select_label_size),
                             debug       = show_debugging_msg)

    } else {
      a <- ggplot2::ggplot() +
        ggplot2::labs(title = unsupported_dataset) +
        ggplot2::theme(panel.background = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(color = model_1_color))
    }

    return(a)
  }, label = 'dataset_page_plot')

  output$dataset_page_plot <- renderUI({
    shiny::conditionalPanel(
      condition = "true",
      div(style = "height:600px",
          if (!is.null(dataset_page_plot())) {
            if(input$do_data_plotly) {
              plotly::plotlyOutput("data_plotly", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
            } else {
              plotOutput("data_ggplot", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
            }
          }
      )
    )
  })
  #### size = 16 is used for regular ggplots to approximate looks compared to plotly in browser. For download, this size change is not applied
  output$data_ggplot <- renderPlot(dataset_page_plot() + add_watermark(watermark_toggle = insert_watermark) + ggplot2::theme(text = ggplot2::element_text(size = 16)))

  output$data_plotly <- plotly::renderPlotly(convert_to_plotly_watermark(dataset_page_plot(),
                                                                         format           = input$plotlyd_format,
                                                                         filename         = input$plotlyd_filename,
                                                                         width            = input$plotlyd_width,
                                                                         height           = input$plotlyd_height,
                                                                         plotly_watermark = insert_watermark,
                                                                         debug            = show_debugging_msg)
  )

  #### Data Plot download section
  observeEvent(input$do_data_plotly, {
    if (input$do_data_plotly) {
      shinyjs::disable("download_data_plot")
      updateSelectInput(session, "plotlyd_format", label = plotly_format_label,
                        choices = c("png", "jpeg", "svg", "webp"))
    } else {
      shinyjs::enable("download_data_plot")
      updateSelectInput(session, "plotlyd_format", label = plotly_format_label,
                        choices = c("png", "pdf", "jpeg", "svg"))
    }
  }, label = "update_download_data_plot")

  output$download_data_plot <- downloadHandler(
    filename = function() {
      paste0(input$plotlyd_filename, ".", input$plotlyd_format)
    },
    content = function(file) {
      ggplot2::ggsave(file,
                      plot = dataset_page_plot() + add_watermark(watermark_toggle = insert_watermark),
                      device = input$plotlyd_format,
                      units = "px",
                      width = input$plotlyd_width,
                      height = input$plotlyd_height
      )
    }
  )

  ## UI: output$dataset_page_plot_corr ----
  correlation_plot <- reactive({
    if (shiny::req(input$color_corr) %in% names(nmdataset_for_plot()) &
        shiny::req(input$var_corr[1]) %in% names(nmdataset_for_plot())) {

      draw_correlation_plot(input_df         = nmdataset_for_plot(),
                            corr_variables   = input$var_corr,
                            color_sep        = input$color_corr,
                            catcov_threshold = 10,
                            debug            = show_debugging_msg)
    }
  })

  output$dataset_page_plot_corr <- renderPlot({

    if (id_is_in_dataset()) {
      correlation_plot()
    } else {
      ggplot2::ggplot() +
        ggplot2::labs(title = unsupported_dataset) +
        ggplot2::theme(panel.background = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(color = model_1_color, size = 16)) +
        add_watermark(watermark_toggle = insert_watermark)
    }
  })


  #### Corr Plot download section
  output$download_corr_plot <- downloadHandler(
    filename = function() {
      paste0(input$plotlyd_corr_filename, ".", input$plotlyd_corr_format)
    },
    content = function(file) {
      ggplot2::ggsave(file,
                      plot = correlation_plot() + add_watermark(watermark_toggle = insert_watermark),
                      device = input$plotlyd_corr_format,
                      units = "px",
                      width = input$plotlyd_corr_width,
                      height = input$plotlyd_corr_height
      )
    }
  )

  ## UI: output$dataset_page_ind_plot ----
  dataset_page_ind_plot <- reactive({
    if (show_debugging_msg) {
      message('Creating dataset_page_ind_plot')
    }

    if (id_is_in_dataset()) {
      if(input$x_axis == "" | input$y_axis == "") { # Dummy panel
        return(ggplot2::ggplot() + ggplot2::theme(panel.background = ggplot2::element_blank()))
      }

      shiny::req(input$x_axis)
      shiny::req(input$y_axis)

      data_to_plot <- trim_columns(data = nmdataset_for_plot(),
                                   x_axis = input$x_axis,
                                   y_axis = input$y_axis,
                                   color = input$color,
                                   type_of_plot = "ind_plot",
                                   #facet_name = NULL,
                                   #insert_med_line = FALSE,
                                   #med_line_by = "",
                                   ind_dose_colname = input$ind_dose_colname,
                                   highlight_var = input$highlight_var,
                                   lloq_colname = input$lloq_colname)

      a <- do_data_page_ind_plot(nmd         = data_to_plot,
                                 rownums     = as.numeric(input$number_of_rows),
                                 colnums     = as.numeric(input$number_of_cols),
                                 pagenum     = as.numeric(input$page_number),
                                 filter_id   = input$filter_by_id,
                                 filter_cmt  = input$filter_cmt_data,
                                 x_axis      = input$x_axis,
                                 y_axis      = input$y_axis,
                                 color_by    = input$color,
                                 med_line    = input$median_line_data,
                                 med_line_by = input$median_line_by,
                                 boxplot     = input$do_boxplot,
                                 dolm        = input$insert_lm_eqn,
                                 smoother    = input$insert_smoother,
                                 #facet_name  = input$facet_by,
                                 logy        = input$log_y_axis_data,
                                 logx        = input$log_x_axis_data,
                                 lby         = logbreaks_y,
                                 lbx         = logbreaks_x,
                                 plot_title  = input$plot_title_data,
                                 label_size  = as.numeric(input$select_label_size),
                                 highlight_var = input$highlight_var,
                                 highlight_var_values = input$highlight_var_values,
                                 plot_dosing = input$insert_dosing,
                                 dose_col    = input$ind_dose_colname,
                                 dose_units  = input$dose_units,
                                 lloq_name   = input$lloq_colname,
                                 same_scale  = input$fixed_scale,
                                 debug       = show_debugging_msg)

    } else {
      a <- ggplot2::ggplot() +
        ggplot2::labs(title = unsupported_dataset) +
        ggplot2::theme(panel.background = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(color = model_1_color))
    }

    return(a)
  }, label = 'dataset_page_ind_plot')

  output$dataset_page_ind_plot <- renderUI({
    shiny::conditionalPanel(
      condition = "true",
      div(style = "height:600px",
          if (!is.null(dataset_page_ind_plot())) {
            if(input$do_data_ind_plotly) {
              plotly::plotlyOutput("data_ind_plotly", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
            } else {
              plotOutput("data_ind_ggplot", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
            }
          }
      )
    )
  })
  #### size = 16 is used for regular ggplots to approximate looks compared to plotly in browser. For download, this size change is not applied
  output$data_ind_ggplot <- renderPlot(dataset_page_ind_plot() + add_watermark(watermark_toggle = insert_watermark) + ggplot2::theme(text = ggplot2::element_text(size = 16)))

  output$data_ind_plotly <- plotly::renderPlotly(convert_to_plotly_watermark(dataset_page_ind_plot(),
                                                                             format           = input$plotlydi_format,
                                                                             filename         = input$plotlydi_filename,
                                                                             width            = input$plotlydi_width,
                                                                             height           = input$plotlydi_height,
                                                                             plotly_watermark = insert_watermark,
                                                                             debug            = show_debugging_msg)
  )

  #### Individual Plot download section
  observeEvent(input$do_data_ind_plotly, {
    if (input$do_data_ind_plotly) {
      shinyjs::disable("download_data_ind_plot")
      updateSelectInput(session, "plotlydi_format", label = plotly_format_label,
                        choices = c("png", "jpeg", "svg", "webp"))
    } else {
      shinyjs::enable("download_data_ind_plot")
      updateSelectInput(session, "plotlydi_format", label = plotly_format_label,
                        choices = c("png", "pdf", "jpeg", "svg"))
    }
  }, label = "update_download_data_ind_plot")

  output$download_data_ind_plot <- downloadHandler(
    filename = function() {
      paste0(input$plotlydi_filename, ".", input$plotlydi_format)
    },
    content = function(file) {
      ggplot2::ggsave(file,
                      plot = dataset_page_ind_plot() + add_watermark(watermark_toggle = insert_watermark),
                      device = input$plotlydi_format,
                      units = "px",
                      width = input$plotlydi_width,
                      height = input$plotlydi_height
      )
    }
  )

  ### All Individual Plots download
  output$download_data_ind_plot_all <- downloadHandler(
    filename = function() {
      paste0(input$plotlydi_all_filename, ".", input$plotlydi_all_format)
    },
    content = function(file) {

      unique_ids         <- unique(nmdataset_for_plot()$ID)
      length_unique_ids  <- length(unique_ids)
      number_of_pages    <- ceiling(length_unique_ids / (as.numeric(input$number_of_rows) * as.numeric(input$number_of_cols)))

      p_ind_all       <- list() # Initiating empty lists to store multiple pages

      data_to_plot <- trim_columns(data = nmdataset_for_plot(),
                                   x_axis = input$x_axis,
                                   y_axis = input$y_axis,
                                   color = input$color,
                                   type_of_plot = "ind_plot",
                                   #facet_name = NULL,
                                   #insert_med_line = FALSE,
                                   #med_line_by = "",
                                   ind_dose_colname = input$ind_dose_colname,
                                   highlight_var = input$highlight_var,
                                   lloq_colname = input$lloq_colname)

      withProgress(message = 'Generating plots', value = 0, {
        for (i in 1:number_of_pages) {
          # Estimating that generation of plots take approx 20% of the total time
          setProgress(value = i/number_of_pages * 0.2, detail = paste0("plot ", i, "/", number_of_pages))
          p_ind_all[[i]]  <- do_data_page_ind_plot(
            nmd         = data_to_plot,
            rownums     = as.numeric(input$number_of_rows),
            colnums     = as.numeric(input$number_of_cols),
            pagenum     = i, #as.numeric(input$page_number),
            filter_id   = '',
            filter_cmt  = input$filter_cmt_data,
            x_axis      = input$x_axis,
            y_axis      = input$y_axis,
            color_by    = input$color,
            med_line    = input$median_line_data,
            med_line_by = input$median_line_by,
            boxplot     = input$do_boxplot,
            dolm        = input$insert_lm_eqn,
            smoother    = input$insert_smoother,
            #facet_name  = input$facet_by,
            logy        = input$log_y_axis_data,
            logx        = input$log_x_axis_data,
            lby         = logbreaks_y,
            lbx         = logbreaks_x,
            plot_title  = input$plot_title_data,
            label_size  = as.numeric(input$select_label_size),
            highlight_var = input$highlight_var,
            highlight_var_values = input$highlight_var_values,
            plot_dosing = input$insert_dosing,
            dose_col    = input$ind_dose_colname,
            dose_units  = input$dose_units,
            lloq_name   = input$lloq_colname,
            same_scale  = input$fixed_scale,
            debug       = show_debugging_msg) +
            add_watermark(watermark_toggle = insert_watermark)
        }

        pdf(file = file, width = input$plotlydi_all_width, height = input$plotlydi_all_height)
        for(m in seq_along(p_ind_all)) {
          setProgress(value = 0.2 + (m/length(p_ind_all) * 0.8), message = 'Compiling PDF', detail = paste0("page ", m, "/", length(p_ind_all)))
          print(p_ind_all[[m]])
        }
        dev.off()
      })
    }
  )

  # Page 2 Simulation ----
  mcode_model_1  <- paste0("\nmodel_object <- mcode('Model-1-", runif(min = 1, max = 9999999, n = 1) %>% round(), "', model_code, recover = TRUE)") # Insert random number as part of name to avoid global object namespace clash
  mcode_model_2  <- paste0("\nmodel_object <- mcode('Model-2-", runif(min = 1, max = 9999999, n = 1) %>% round(), "', model_code, recover = TRUE)")

  ## Outline ----
  ### Update model selectizeInput ----
  observeEvent(input$password, {
    update_model_choices(input_password = input$password, session = session)
  }, label = 'update_model_choices_default_values')

  observeEvent(input$model_select, {
    shinyAce::updateAceEditor(session, 'model_input', value = model_switch_conditions(input_model_select = input$model_select, mcode_model_choice = mcode_model_1))

    if (show_debugging_msg) {
      message(paste0('Model 1 selected to: ', input$model_select))
    }

    # Turn all dose amounts to 0 for TTE models as they are not applicable
    if(stringr::str_detect(input$model_select, "Time To Event")) {
      shiny::showNotification("Setting all dose amounts to 0 for TTE models.", type = "message", duration = 10)
      updateNumericInput(session, "amt1", value = 0)
      updateNumericInput(session, "amt2", value = 0)
      updateNumericInput(session, "amt3", value = 0)
      updateNumericInput(session, "amt4", value = 0)
      updateNumericInput(session, "amt5", value = 0)
    }

  }, label = 'updateAceEditor Model 1')

  observeEvent(input$model_select2, {
    shinyAce::updateAceEditor(session, 'model_input2', value = model_switch_conditions(input_model_select = input$model_select2, mcode_model_choice = mcode_model_2))

    if (show_debugging_msg) {
      message(paste0('Model 2 selected to: ', input$model_select2))
    }

    # Turn all dose amounts to 0 for TTE models as they are not applicable
    if(stringr::str_detect(input$model_select2, "Time To Event")) {
      shiny::showNotification("Setting all dose amounts to 0 for TTE models.", type = "message", duration = 10)
      updateNumericInput(session, "amt1_2", value = 0)
      updateNumericInput(session, "amt2_2", value = 0)
      updateNumericInput(session, "amt3_2", value = 0)
      updateNumericInput(session, "amt4_2", value = 0)
      updateNumericInput(session, "amt5_2", value = 0)
    }

  }, label = 'updateAceEditor Model 2')

  ## Handling upload of .cpp file option
  output$upload_cpp_model_1 <- renderUI({
    if (input$model_select == "Upload .cpp File") {
      fileInput("cppfile_model_1", "Upload External .cpp File",
                accept = c(
                  "text/cpp",
                  ".cpp")
      )
    }
  })

  observeEvent(input$cppfile_model_1, {
    shiny::req(input$cppfile_model_1)
    cpp_text_model_1 <- paste(readLines(input$cppfile_model_1$datapath), collapse = "\n")
    shinyAce::updateAceEditor(session, "model_input", value = paste0(cpp_preamble, '"', cpp_text_model_1, '"', mcode_model_1))
  })

  output$upload_cpp_model_2 <- renderUI({
    if (input$model_select2 == "Upload .cpp File") {
      fileInput("cppfile_model_2", "Upload External .cpp File",
                accept = c(
                  "text/cpp",
                  ".cpp")
      )
    }
  })

  observeEvent(input$cppfile_model_2, {
    shiny::req(input$cppfile_model_2)
    cpp_text_model_2 <- paste(readLines(input$cppfile_model_2$datapath), collapse = "\n")
    shinyAce::updateAceEditor(session, "model_input2", value = paste0(cpp_preamble, '"', cpp_text_model_2, '"', mcode_model_2))
  })

  ### Update x_axis_label ----
  observe({
    updateTextInput(session, 'x_axis_label', value = switch(
      input$time_unit,
      '1'      = 'Time (hours)',
      '24'     = 'Time (days)',
      '168'    = 'Time (weeks)',
      '672'    = 'Time (months)'))
  }, label = 'update x-axis label textInput by input$time_unit', priority = 99)
  # Model 1 ----  ###############################################################
  # Model generation action button:
  ## inputted_model_1() ----
  model_1_checkpoint <- reactiveValues(param_columns_generated_model_1 = FALSE,
                                       param_input_generated_model_1   = FALSE,
                                       updated_values_model_1          = NULL,
                                       param_updated_model_1           = FALSE,
                                       sim_generated_model_1           = FALSE,
  )

  iiv_checkpoint_model_1 <- reactiveValues(extract_model_omega = FALSE,
                                           extract_model_sigma = FALSE,
                                           updated_matrix_omega = FALSE,
                                           updated_matrix_sigma = FALSE,
                                           reconstructed_iiv = FALSE,
                                           iiv_simulation = FALSE)

  safely_inputted_model_1 <- reactive({
    tmp <- safely_eval(parse(text = input$model_input))
    return(tmp)
  }, label = 'safely_inputted_model_1') %>% bindEvent(input$generate_model)

  inputted_model_1 <- reactive({
    model_1_checkpoint$updated_values_model_1          <- NULL
    model_1_checkpoint$param_updated_model_1           <- FALSE
    model_1_checkpoint$sim_generated_model_1           <- FALSE
    model_1_checkpoint$param_input_generated_model_1   <- FALSE
    model_1_checkpoint$param_columns_generated_model_1 <- FALSE

    iiv_checkpoint_model_1$extract_model_omega <- FALSE
    iiv_checkpoint_model_1$extract_model_sigma <- FALSE
    iiv_checkpoint_model_1$updated_matrix_omega <- FALSE
    iiv_checkpoint_model_1$updated_matrix_sigma <- FALSE
    iiv_checkpoint_model_1$reconstructed_iiv <- FALSE
    iiv_checkpoint_model_1$iiv_simulation <- FALSE
    if(mrgsolve::is.mrgmod(safely_inputted_model_1()$result)) {
      return(safely_inputted_model_1()$result)
    } else {
      return(NULL) # compatibility for many if conditionals failing
    }
  }, label = 'inputted_model_1')


  ## Model 1 validation ----
  model_1_is_valid <- reactive({
    if(mrgsolve::is.mrgmod(safely_inputted_model_1()$result)) {
      if (show_debugging_msg) {
        message("Model 1 is valid.")
      }
      return(TRUE)
    } else {
      if (show_debugging_msg) {
        message("Model 1 is not valid.")
      }
      shiny::showNotification("ERROR: Model code failed to compile. Check Model Info Console for more information.", type = "error", duration = 10)
      return(FALSE)
    }
  }, label = 'model_1_is_valid')

  output$console_output_model_1 <- renderPrint({
    if(!model_1_is_valid()) {
      if(is.null(safely_inputted_model_1()$result)) {
        print(safely_inputted_model_1()$error)
      } else {
        print(safely_inputted_model_1()$result$out$stderr) # when recover = TRUE is used in mcode
      }
    } else {
      if(mrgsolve::is.mrgmod(changed_reacted_param_model_1())) {
        print(changed_reacted_param_model_1())
        cat("\n")
        print(dosing_regimen_model_1())
        cat("\nModel Code:\n")
        cat(mrgsolve:::code(changed_reacted_param_model_1()), sep = "\n")
      }
    }
  }) %>% bindEvent(changed_reacted_param_model_1(), dosing_regimen_model_1(), safely_inputted_model_1(), model_1_is_valid())

  ## param_columns_generated_model_1() ----
  param_columns_generated_model_1 <- eventReactive(model_1_is_valid(), {
    if(model_1_is_valid()) {
      if (show_debugging_msg) {
        message(names(mrgsolve::param(inputted_model_1())))
      }
      input_widgets <-
        lapply(1:length(mrgsolve::param(inputted_model_1())), function(i) {
          column(width = 3,
                 numericInput(
                   inputId = names(mrgsolve::param(inputted_model_1())[i]),
                   label = paste0(names(mrgsolve::param(inputted_model_1())[i]), ': '),
                   value = mrgsolve::param(inputted_model_1())[[i]]
                 ))
        })
      model_1_checkpoint$param_columns_generated_model_1 <- TRUE
      tmp <- do.call(tagList, input_widgets)
      return(tmp)
    }
  }, label = 'param_columns_generated_model_1')

  # Generate parameter UI based on numbers of $PARAM
  ## UI: output$param_output_model_1 ----
  output$param_output_model_1 <- renderUI({
    param_columns_generated_model_1()
  })

  ### Update param and imply into model ----
  # Create a reactive value to store the updated input values
  param_input_model_1 <- reactiveVal()

  observe({
    if(model_1_checkpoint$param_columns_generated_model_1) {
      param_names <- names(mrgsolve::param(inputted_model_1()))

      if (!shinyAce::is.empty(input[[param_names[1]]])) {
        dataframe_list <- list()

        for (param_name in param_names) {
          if (!is.null(input[[param_name]])) {
            model_1_checkpoint$updated_values_model_1[[param_name]] <- input[[param_name]]

            dataframe <- data.frame(name = param_name, value = model_1_checkpoint$updated_values_model_1[[param_name]])

            dataframe_list[[param_name]] <- dataframe
          }
        }
        final_dataframe <- do.call(rbind, dataframe_list) %>% dplyr::as_tibble()

        if (show_debugging_msg) {
          message('created_param_input')
          GGally::print_if_interactive(final_dataframe)
        }
        model_1_checkpoint$param_input_generated_model_1 <- TRUE
        param_input_model_1(final_dataframe)
      }
    }
  }, label = 'param_input_model_1()')

  # Imply modification on params into the model
  ## changed_reacted_param_model_1() ----
  changed_reacted_param_model_1 <- reactiveVal()

  observe({
    if (model_1_checkpoint$param_input_generated_model_1) {
      if (show_debugging_msg) {
        message('update_model_object_initiated')
      }
      new_model <- update_model_object(inputted_model_1(), param_input_model_1())
      new_model <- mrgsolve::update(new_model,
                                    rtol     = sanitize_numeric_input(input$solver_rtol),
                                    atol     = sanitize_numeric_input(input$solver_atol),
                                    maxsteps = sanitize_numeric_input(input$solver_maxsteps))
      changed_reacted_param_model_1(new_model)

      if (show_debugging_msg) {
        message('update_model_object_completed')
      }
      model_1_checkpoint$param_updated_model_1 <- TRUE
    }
  }, label = 'changed_reacted_param_model_1()')

  ## dosing_regimen_model_1() ----
  dosing_regimen_model_1 <- reactive({

    dose_regimen <- generate_dosing_regimens(
      amt1 = input$amt1, delay_time1 = input$delay_time1, cmt1 = input$cmt1_model_1, tinf1 = input$tinf1, total1 = input$total1, ii1 = input$ii1,
      amt2 = input$amt2, delay_time2 = input$delay_time2, cmt2 = input$cmt2_model_1, tinf2 = input$tinf2, total2 = input$total2, ii2 = input$ii2,
      amt3 = input$amt3, delay_time3 = input$delay_time3, cmt3 = input$cmt3_model_1, tinf3 = input$tinf3, total3 = input$total3, ii3 = input$ii3,
      amt4 = input$amt4, delay_time4 = input$delay_time4, cmt4 = input$cmt4_model_1, tinf4 = input$tinf4, total4 = input$total4, ii4 = input$ii4,
      amt5 = input$amt5, delay_time5 = input$delay_time5, cmt5 = input$cmt5_model_1, tinf5 = input$tinf5, total5 = input$total5, ii5 = input$ii5,
      mw_conversion = mw_conversion_model_1(),
      wt_multiplication_value = wt_multiplication_model_1(),
      create_dummy_ev = TRUE,
      debug = show_debugging_msg
    )

    return(dose_regimen)
  }, label = 'dosing_regimen_model_1')

  ## mw_conversion_model_1() ----
  mw_conversion_model_1 <- reactive({
    if(input$mw_checkbox) {
      conversion <- (1/input$mw)*input$multi_factor   # unit conversion factor accounting for Molecular Weight, set to 1 for no conversion
    } else {
      conversion <- 1
    }
    return(conversion)
  }, label = 'mw_conversion_model_1')

  wt_multiplication_model_1 <- reactive({
    wt_multiplication_value <- 1
    if(model_1_is_valid()) {
      if(input$wt_based_dosing_checkbox & input$wt_based_dosing_name %in% names(mrgsolve::param(inputted_model_1()))) {
        wt_multiplication_value <- input[[input$wt_based_dosing_name]] # E.g. "input$WT"
      }
    }
    return(wt_multiplication_value)
  }, label = 'wt_multiplication_model_1')

  ### Update cmt from the model ----
  observeEvent(inputted_model_1(), {
    # Automatically handle D_ and R_ if present
    if (any(stringr::str_starts(inputted_model_1()$code, "D_"))) {
      updateCheckboxInput(session, "model_dur_checkbox", value = TRUE)
      shiny::showNotification("Duration syntax (D_xxx) detected in the model code and will be modeled by default.", type = "message", duration = 10)
    } else {
      updateCheckboxInput(session, "model_dur_checkbox", value = FALSE)
    }

    if (any(stringr::str_starts(inputted_model_1()$code, "R_") &
            !stringr::str_starts(inputted_model_1()$code, "R_tot"))) {
      updateCheckboxInput(session, "model_rate_checkbox", value = TRUE)
      shiny::showNotification("Rate syntax (R_xxx) detected in the model code and will be modeled by default.", type = "message", duration = 10)
    } else {
      updateCheckboxInput(session, "model_rate_checkbox", value = FALSE)
    }

    if(model_1_is_valid()) {
      updateSelectizeInput(session,
                           "cmt1_model_1",
                           choices  = mrgsolve::outvars(inputted_model_1())$cmt,
                           selected = mrgsolve::outvars(inputted_model_1())$cmt[1])

      updateSelectizeInput(session,
                           "cmt2_model_1",
                           choices  = mrgsolve::outvars(inputted_model_1())$cmt,
                           selected = mrgsolve::outvars(inputted_model_1())$cmt[1])

      updateSelectizeInput(session,
                           "cmt3_model_1",
                           choices  = mrgsolve::outvars(inputted_model_1())$cmt,
                           selected = mrgsolve::outvars(inputted_model_1())$cmt[1])

      updateSelectizeInput(session,
                           "cmt4_model_1",
                           choices  = mrgsolve::outvars(inputted_model_1())$cmt,
                           selected = mrgsolve::outvars(inputted_model_1())$cmt[1])

      updateSelectizeInput(session,
                           "cmt5_model_1",
                           choices  = mrgsolve::outvars(inputted_model_1())$cmt,
                           selected = mrgsolve::outvars(inputted_model_1())$cmt[1])

    }
  }, label = 'update simulation CMT dosing_regimen_model_1')

  model_duration_argument_model_1 <- reactive({
    tmp <- FALSE

    ## Handling edge case where if D_syntax is used and amt is 0, mrgsolve will crash with the following error:
    ## Warning: Error in : non-zero rate requires positive amt
    ## ID: 1, row: 1, rate: -2, amt: 0
    ## Solution is to uncheck the model_dur_checkbox

    if (input$model_dur_checkbox) {
      if (any(stringr::str_starts(inputted_model_1()$code, "D_"))) {
        if(any(dosing_regimen_model_1()$amt > 0)) {
          tmp <- TRUE
        } else {
          shiny::showNotification("WARNING: Duration syntax (D_xxx) detected in the model code, however there are no valid dose amounts. Duration not modeled.", type = "warning", duration = 10)
        }
      } else {
        shiny::showNotification("ERROR: Duration syntax (D_xxx) not identified in the model code. Duration not modeled.", type = "error", duration = 10)
      }
    }

    return(tmp)
  })

  model_rate_argument_model_1 <- reactive({
    tmp <- FALSE

    if (input$model_rate_checkbox) {
      if (any(stringr::str_starts(inputted_model_1()$code, "R_"))) {
        if(any(dosing_regimen_model_1()$amt > 0)) {
          tmp <- TRUE
        } else {
          shiny::showNotification("WARNING: Rate syntax (R_xxx) detected in the model code, however there are no valid dose amounts. Rate not modeled.", type = "warning", duration = 10)
        }
      } else {
        shiny::showNotification("ERROR: Rate syntax (R_xxx) not identified in the model code. Rate not modeled.", type = "error", duration = 10)
      }
    }

    return(tmp)
  })

  # Check if model 1 is using $PRED syntax
  model_1_is_pred <- reactive({
    if(model_1_is_valid()) {
      if (any(stringr::str_starts(tolower(inputted_model_1()$code), "\\$pred") |
              stringr::str_starts(tolower(inputted_model_1()$code), "\\[ pred \\]") |
              stringr::str_starts(tolower(inputted_model_1()$code), "\\[pred\\]")
      )
      ) {

        if (show_debugging_msg) {
          message("Model 1 is modelled using $PRED.")
        }
        shinyjs::disable("amt1")
        shinyjs::disable("amt2")
        shinyjs::disable("amt3")
        shinyjs::disable("amt4")
        shinyjs::disable("amt5")
        shiny::showNotification("Warning: Model uses $PRED. All dosing settings will be ignored.", type = "warning", duration = 10)
        tmp <- TRUE

      } else {

        if (show_debugging_msg) {
          message("Model 1 is not modelled using $PRED.")
        }
        shinyjs::enable("amt1")
        shinyjs::enable("amt2")
        shinyjs::enable("amt3")
        shinyjs::enable("amt4")
        shinyjs::enable("amt5")
        tmp <- FALSE
      }
    } else { # end of model_1_is_valid check
      tmp <- FALSE
    }
    return(tmp)
  }, label = "model_1_is_pred")

  ## simulation_output_model_1() ----
  simulation_output_model_1 <- reactive({
    if (model_1_checkpoint$param_updated_model_1) {
      if(model_1_is_pred() || (dosing_regimen_model_1()$cmt[1] %in% inputted_model_1()$cmt)) {
        if (input$generate_model) {
          if(show_debugging_msg) {
            message("generate_model clicked. Either model 1 is $PRED or model_1_checkpoint$param_updated_model_1 met.")
          }
          sim_output <- NULL
        }
        if(mrgsolve::is.mrgmod(changed_reacted_param_model_1())) {
          if(show_debugging_msg) {
            message("changed_reacted_param_model_1 is mrgmod. will execute run_single_sim")
          }
          sim_output <-
            run_single_sim(
              input_model_object = changed_reacted_param_model_1(),
              pred_model         = model_1_is_pred(),
              ev_df              = dosing_regimen_model_1(),
              model_dur          = model_duration_argument_model_1(),
              model_rate         = model_rate_argument_model_1(),
              sampling_times     = sampling_options(),
              debug              = show_debugging_msg,
              divide_by          = time_value()
            )
        }

        if(is.data.frame(sim_output)) {
          model_1_checkpoint$sim_generated_model_1 <- TRUE

          if (show_debugging_msg) {
            message('simulation1 generated')
          }

          return(sim_output)
        }
      }
    }
  }, label = 'simulation_output_model_1()')

  # Model 2 ----   ###############################################################
  ## inputted_model_2() ----
  model_2_checkpoint <- reactiveValues(param_columns_generated_model_2 = FALSE,
                                       updated_values_model_2          = NULL,
                                       param_input_generated_model_2   = FALSE,
                                       param_updated_model_2           = FALSE,
                                       sim_generated_model_2           = FALSE,
  )

  iiv_checkpoint_model_2 <- reactiveValues(extract_model_omega = FALSE,
                                           extract_model_sigma = FALSE,
                                           updated_matrix_omega = FALSE,
                                           updated_matrix_sigma = FALSE,
                                           reconstructed_iiv = FALSE,
                                           iiv_simulation = FALSE)

  safely_inputted_model_2 <- reactive({
    tmp2 <- safely_eval(parse(text = input$model_input2))
    return(tmp2)
  }, label = 'safely_inputted_model_2') %>% bindEvent(input$generate_model2)

  inputted_model_2 <- reactive({
    model_2_checkpoint$param_columns_generated_model_2 <- FALSE
    model_2_checkpoint$updated_values_model_2          <- NULL
    model_2_checkpoint$param_input_generated_model_2   <- FALSE
    model_2_checkpoint$param_updated_model_2           <- FALSE
    model_2_checkpoint$sim_generated_model_2           <- FALSE

    iiv_checkpoint_model_2$extract_model_omega <- FALSE
    iiv_checkpoint_model_2$extract_model_sigma <- FALSE
    iiv_checkpoint_model_2$updated_matrix_omega <- FALSE
    iiv_checkpoint_model_2$updated_matrix_sigma <- FALSE
    iiv_checkpoint_model_2$reconstructed_iiv <- FALSE
    iiv_checkpoint_model_2$iiv_simulation <- FALSE
    if(mrgsolve::is.mrgmod(safely_inputted_model_2()$result)) {
      return(safely_inputted_model_2()$result)
    } else {
      return(NULL)
    }
  }, label = 'inputted_model_2')

  ## Model 2 validation ----
  model_2_is_valid <- reactive({
    if(mrgsolve::is.mrgmod(safely_inputted_model_2()$result)) {
      if (show_debugging_msg) {
        message("Model 2 is valid.")
      }
      return(TRUE)
    } else {
      if (show_debugging_msg) {
        message("Model 2 is not valid.")
      }
      shiny::showNotification("ERROR: Model code failed to compile. Check Model Info Console for more information.", type = "error", duration = 10)
      return(FALSE)
    }
  }, label = 'model_2_is_valid')

  output$console_output_model_2 <- renderPrint({
    if(!model_2_is_valid()) {
      if(is.null(safely_inputted_model_2()$result)) {
        print(safely_inputted_model_2()$error)
      } else {
        print(safely_inputted_model_2()$result$out$stderr) # when recover = TRUE is used in mcode
      }
    } else {
      if(mrgsolve::is.mrgmod(changed_reacted_param_model_2())) {
        print(changed_reacted_param_model_2())
        cat("\n")
        print(dosing_regimen_model_2())
        cat("\nModel Code:\n")
        cat(mrgsolve:::code(changed_reacted_param_model_2()), sep = "\n")
      }
    }
  }) %>% bindEvent(changed_reacted_param_model_2(), dosing_regimen_model_2(), safely_inputted_model_2())


  ## param_columns_generated_model_2() ----
  param_columns_generated_model_2 <- eventReactive(model_2_is_valid(), {
    if(model_2_is_valid()) {
      if (show_debugging_msg) {
        message(names(mrgsolve::param(inputted_model_2())))
      }
      input_widgets2 <-
        lapply(1:length(mrgsolve::param(inputted_model_2())), function(i) {
          column(width = 3,
                 numericInput(
                   inputId = paste0(names(mrgsolve::param(inputted_model_2())[i]), '_model_2'),
                   label = paste0(names(mrgsolve::param(inputted_model_2())[i]), ': '),
                   value = mrgsolve::param(inputted_model_2())[[i]]
                 ))
        })
      tmp <- do.call(tagList, input_widgets2)
      model_2_checkpoint$param_columns_generated_model_2 <- TRUE
      return(tmp)
    }
  }, label = 'param_columns_generated_model_2')

  # Generate parameter UI based on numbers of $PARAM
  ## UI: output$param_output_model_2 ----
  output$param_output_model_2 <- renderUI({
    param_columns_generated_model_2()
  })

  ### Update param and imply into model ----
  # Create a reactive value to store the updated input values
  param_input_model_2 <- reactiveVal()

  observe({
    if(model_2_checkpoint$param_columns_generated_model_2) {
      param_names2 <- names(mrgsolve::param(inputted_model_2()))

      if (!is.null(input[[paste0(names(mrgsolve::param(inputted_model_2())[1]), '_model_2')]])) {
        dataframe_list2 <- list()

        for (param_name2 in param_names2) {
          if (!is.null(input[[paste0(param_name2, '_model_2')]])) {
            model_2_checkpoint$updated_values_model_2[[param_name2]] <- input[[paste0(param_name2, '_model_2')]]

            dataframe2 <- data.frame(name = param_name2, value = model_2_checkpoint$updated_values_model_2[[param_name2]])

            dataframe_list2[[param_name2]] <- dataframe2
          }
        }
        final_dataframe2 <- do.call(rbind, dataframe_list2) %>% dplyr::as_tibble()

        if (show_debugging_msg) {
          message('created_param_input')
          GGally::print_if_interactive(final_dataframe2)
        }
        model_2_checkpoint$param_input_generated_model_2 <- TRUE
        param_input_model_2(final_dataframe2)
      }
    }
  }, label = 'param_input_model_2()')

  # Imply modification on params into the model
  ## changed_reacted_param_model_2() ----
  changed_reacted_param_model_2 <- reactiveVal()

  observe({
    if (model_2_checkpoint$param_input_generated_model_2) {
      if (show_debugging_msg) {
        message('update_model_2_object_observe')
      }

      new_model2 <- update_model_object(inputted_model_2(), param_input_model_2())
      new_model2 <- mrgsolve::update(new_model2,
                                     rtol     = sanitize_numeric_input(input$solver_rtol),
                                     atol     = sanitize_numeric_input(input$solver_atol),
                                     maxsteps = sanitize_numeric_input(input$solver_maxsteps))
      changed_reacted_param_model_2(new_model2)

      if (show_debugging_msg) {
        message('model_updated_observe_model_2')
      }
      model_2_checkpoint$param_updated_model_2 <- TRUE
    }
  }, label = 'changed_reacted_param_model_2()')

  ## dosing_regimen_model_2() ----
  dosing_regimen_model_2 <- reactive({

    dose_regimen_2 <- generate_dosing_regimens(
      amt1 = input$amt1_2, delay_time1 = input$delay_time1_2, cmt1 = input$cmt1_model_2, tinf1 = input$tinf1_2, total1 = input$total1_2, ii1 = input$ii1_2,
      amt2 = input$amt2_2, delay_time2 = input$delay_time2_2, cmt2 = input$cmt2_model_2, tinf2 = input$tinf2_2, total2 = input$total2_2, ii2 = input$ii2_2,
      amt3 = input$amt3_2, delay_time3 = input$delay_time3_2, cmt3 = input$cmt3_model_2, tinf3 = input$tinf3_2, total3 = input$total3_2, ii3 = input$ii3_2,
      amt4 = input$amt4_2, delay_time4 = input$delay_time4_2, cmt4 = input$cmt4_model_2, tinf4 = input$tinf4_2, total4 = input$total4_2, ii4 = input$ii4_2,
      amt5 = input$amt5_2, delay_time5 = input$delay_time5_2, cmt5 = input$cmt5_model_2, tinf5 = input$tinf5_2, total5 = input$total5_2, ii5 = input$ii5_2,
      mw_conversion = mw_conversion_model_2(),
      wt_multiplication_value = wt_multiplication_model_2(),
      create_dummy_ev = TRUE,
      debug = show_debugging_msg
    )

    return(dose_regimen_2)

  }, label = 'dosing_regimen_model_2')

  ## mw_conversion_model_2() ----
  mw_conversion_model_2 <- reactive({
    if(input$mw_checkbox_2) {
      conversion2 <- (1/input$mw_2)*input$multi_factor_2   # unit conversion factor accounting for Molecular Weight, set to 1 for no conversion
    } else {
      conversion2 <- 1
    }
    return(conversion2)
  }, label = 'mw_conversion_model_2')

  wt_multiplication_model_2 <- reactive({
    wt_multiplication_value2 <- 1
    if(model_2_is_valid()) {
      if(input$wt_based_dosing_checkbox_2 & input$wt_based_dosing_name_2 %in% names(mrgsolve::param(inputted_model_2()))) {
        wt_multiplication_value2 <- input[[paste0(input$wt_based_dosing_name_2, '_model_2')]] # E.g. input$WT_model_2
      }
    }
    return(wt_multiplication_value2)
  }, label = 'wt_multiplication_model_2')

  ### Update cmt from the model ----
  observeEvent(inputted_model_2(), {
    # Automatically handle D_ and R_ if present
    if (any(stringr::str_starts(inputted_model_2()$code, "D_"))) {
      updateCheckboxInput(session, "model_dur_checkbox_2", value = TRUE)
      shiny::showNotification("Duration syntax (D_xxx) detected in the model code and will be modeled by default.", type = "message", duration = 10)
    } else {
      updateCheckboxInput(session, "model_dur_checkbox_2", value = FALSE)
    }

    if (any(stringr::str_starts(inputted_model_2()$code, "R_") &
            !stringr::str_starts(inputted_model_2()$code, "R_tot"))) {
      updateCheckboxInput(session, "model_rate_checkbox_2", value = TRUE)
      shiny::showNotification("Rate syntax (R_xxx) detected in the model code and will be modeled by default.", type = "message", duration = 10)
    } else {
      updateCheckboxInput(session, "model_rate_checkbox_2", value = FALSE)
    }

    if(model_2_is_valid()) {
      updateSelectizeInput(session,
                           "cmt1_model_2",
                           choices  = mrgsolve::outvars(inputted_model_2())$cmt,
                           selected = mrgsolve::outvars(inputted_model_2())$cmt[1])

      updateSelectizeInput(session,
                           "cmt2_model_2",
                           choices  = mrgsolve::outvars(inputted_model_2())$cmt,
                           selected = mrgsolve::outvars(inputted_model_2())$cmt[1])

      updateSelectizeInput(session,
                           "cmt3_model_2",
                           choices  = mrgsolve::outvars(inputted_model_2())$cmt,
                           selected = mrgsolve::outvars(inputted_model_2())$cmt[1])

      updateSelectizeInput(session,
                           "cmt4_model_2",
                           choices  = mrgsolve::outvars(inputted_model_2())$cmt,
                           selected = mrgsolve::outvars(inputted_model_2())$cmt[1])

      updateSelectizeInput(session,
                           "cmt5_model_2",
                           choices  = mrgsolve::outvars(inputted_model_2())$cmt,
                           selected = mrgsolve::outvars(inputted_model_2())$cmt[1])
    }
  }, label = 'update simulation CMT dosing_regimen_model_2')

  model_duration_argument_model_2 <- reactive({
    tmp <- FALSE

    if (input$model_dur_checkbox_2) {
      if (any(stringr::str_starts(inputted_model_2()$code, "D_"))) {
        if(any(dosing_regimen_model_2()$amt > 0)) {
          tmp <- TRUE
        } else {
          shiny::showNotification("WARNING: Duration syntax (D_xxx) detected in the model code, however there are no valid dose amounts. Duration not modeled.", type = "warning", duration = 10)
        }
      } else {
        shiny::showNotification("ERROR: Duration syntax (D_xxx) not identified in the model code. Duration not modeled.", type = "error", duration = 10)
      }
    }

    return(tmp)
  })

  model_rate_argument_model_2 <- reactive({
    tmp <- FALSE

    if (input$model_rate_checkbox_2) {
      if (any(stringr::str_starts(inputted_model_2()$code, "R_"))) {
        if(any(dosing_regimen_model_2()$amt > 0)) {
          tmp <- TRUE
        } else {
          shiny::showNotification("WARNING: Rate syntax (R_xxx) detected in the model code, however there are no valid dose amounts. Rate not modeled.", type = "warning", duration = 10)
        }
      } else {
        shiny::showNotification("ERROR: Rate syntax (R_xxx) not identified in the model code. Rate not modeled.", type = "error", duration = 10)
      }
    }

    return(tmp)
  })


  # Check if model 2 is using $PRED syntax
  model_2_is_pred <- reactive({
    if(model_2_is_valid()) {
      if (any(stringr::str_starts(tolower(inputted_model_2()$code), "\\$pred") |
              stringr::str_starts(tolower(inputted_model_2()$code), "\\[ pred \\]") |
              stringr::str_starts(tolower(inputted_model_2()$code), "\\[pred\\]")
      )
      ){

        if (show_debugging_msg) {
          message("Model 2 is modelled using $PRED.")
        }
        shinyjs::disable("amt1_2")
        shinyjs::disable("amt2_2")
        shinyjs::disable("amt3_2")
        shinyjs::disable("amt4_2")
        shinyjs::disable("amt5_2")
        shiny::showNotification("Warning: Model uses $PRED. All dosing settings will be ignored.", type = "warning", duration = 10)
        tmp <- TRUE

      } else {

        if (show_debugging_msg) {
          message("Model 2 is not modelled using $PRED.")
        }
        shinyjs::enable("amt1_2")
        shinyjs::enable("amt2_2")
        shinyjs::enable("amt3_2")
        shinyjs::enable("amt4_2")
        shinyjs::enable("amt5_2")
        tmp <- FALSE
      }
    } else { # end of model_2_is_valid check
      tmp <- FALSE
    }
    return(tmp)
  }, label = "model_2_is_pred")

  ## simulation_output_model_2() ----
  simulation_output_model_2 <- reactive({
    if (model_2_checkpoint$param_updated_model_2) {
      if(model_2_is_pred() || dosing_regimen_model_2()$cmt[1] %in% inputted_model_2()$cmt) {
        if (input$generate_model2) {

          if(show_debugging_msg) {
            message("generate_model2 clicked. Either model 2 is $PRED or model_2_checkpoint$param_updated_model_2 met.")
          }

          sim_output <- NULL
        }

        if(mrgsolve::is.mrgmod(changed_reacted_param_model_2())) {
          if(show_debugging_msg) {
            message("changed_reacted_param_model_2 is mrgmod. will execute run_single_sim")
          }
          sim_output <-
            run_single_sim(
              input_model_object = changed_reacted_param_model_2(),
              pred_model         = model_2_is_pred(),
              ev_df              = dosing_regimen_model_2(),
              model_dur          = model_duration_argument_model_2(),
              model_rate         = model_rate_argument_model_2(),
              sampling_times     = sampling_options(),
              debug              = show_debugging_msg,
              divide_by          = time_value(),
              append_id_text     = "m2-"
            )
        }
        if(is.data.frame(sim_output)) {
          if (show_debugging_msg) {
            message('simulation2 generated')
          }
          model_2_checkpoint$sim_generated_model_2 <- TRUE

          return(sim_output)
        }
      }
    }
  }, label = 'simulation_output_model_2()')

  # End of model 1 and model 2 ##################################################
  ## Create a new variable that sanitizes Max Sampling time
  tend <- reactive({
    tmp <- sanitize_numeric_input(input$tgrid_max, allow_zero = FALSE, return_value = 24, display_error = TRUE)
    return(tmp)
  }, label = 'tend: tgrid_max')

  ## Create a new variable that sanitizes Frequency
  tdelta <- reactive({
    tmp <- sanitize_numeric_input(input$delta, allow_zero = FALSE, legal_maximum = tend(), display_error = TRUE)
    return(tmp)
  }, label = 'tdelta')

  # checking if user provided custom sampling is valid
  eval_custom_time <- reactive({
    tmp <- try(eval(parse(text=paste0("c(",input$custom_sampling_time_text,")"))), silent=TRUE)
    return(tmp)
  }, label = 'eval_custom_time')

  # Only evaluate and return a non-error message (presumably a vector)
  custom_time <- NULL
  custom_time <- reactive({
    if(!is(eval_custom_time(),"try-error")) {
      sanitized_times <- eval(parse(text=paste0("c(", input$custom_sampling_time_text, ")")))
      if(is.numeric(sanitized_times)) {
        return(sanitized_times)
      } else {
        shiny::showNotification("ERROR: Custom sampling is not a numeric vector. Reverting to regular sampling time and frequency.", type = "error", duration = 5)
        return(NULL)
      }
    }
  }, label = 'custom_time after sanitized')

  sampling_options <- reactive({
    # Default scenario uses Max Sampling and Sampling Frequency
    sampling_argument <- seq(from = tdelta(), to = tend(), by = tdelta())

    # If Custom sampling CB & Non-error Sampling Times & it is not NULL, then use custom time
    if(input$custom_sampling_time_cb) {
      if (show_debugging_msg) {
        message("Trigger sampling cb")
      }
      if(is(eval_custom_time(), "try-error")) {
        shiny::showNotification("ERROR: Custom sampling has a syntax error. Reverting to regular sampling time and frequency.", type = "error", duration = 5)
      } else {
        if(!is.null(custom_time())) {
          sampling_argument <- custom_time()
          #message("times: ", sampling_argument)
          shiny::showNotification("Applying custom sampling time...", type = "message", duration = 5)
        }
      }
    }
    if(input$add_time_zero) {
      sampling_argument <- c(0, sampling_argument)
    }
    return(sampling_argument)
  }, label = 'sampling_options')

  ## TIME selection ----
  time_value <- reactiveVal()

  observe({
    time_value(as.numeric(input$time_unit))
  }, label = 'convert time_value() to numeric', priority = 98)

  nmdata_cmt_filtered <- reactive({
    if (input$filter_cmt != 'NULL') {
      cmt_filtered <- nmdataset_for_plot() %>% dplyr::filter(CMT %in% input$filter_cmt)

      if(show_debugging_msg) {
        message(dplyr::glimpse(cmt_filtered))
      }

      if("TIME" %in% names(cmt_filtered)) {
        cmt_filtered <- cmt_filtered %>% dplyr::mutate(TIMEADJ = TIME/time_value())
      } else {
        cmt_filtered <- cmt_filtered %>% dplyr::mutate(TIMEADJ = 1) # dummy value
      }
    } else {
      cmt_filtered <- nmdataset_for_plot()
      if("TIME" %in% names(cmt_filtered)) {
        cmt_filtered <- cmt_filtered %>% dplyr::mutate(TIMEADJ = TIME/time_value())
      } else {
        cmt_filtered <- cmt_filtered %>% dplyr::mutate(TIMEADJ = 1) # dummy value
      }
    }
  }, label = 'nmdata_cmt_filtered')

  ### Update plotting variables to capture ----
  observeEvent(inputted_model_1(), {
    if(model_1_is_valid()) {
      updateSelectizeInput(session,
                           'yaxis_name',
                           choices = c(mrgsolve::outvars(inputted_model_1())$capture, mrgsolve::outvars(inputted_model_1())$cmt),
                           selected = mrgsolve::outvars(inputted_model_1())$capture[1])

      if (show_debugging_msg) {
        message('updated yaxis_name')
      }
    }
  }, label = 'update_y_axis_name')

  observeEvent(inputted_model_2(), {
    if(model_2_is_valid()) {
      updateSelectizeInput(session,
                           'yaxis_name_2',
                           choices = c(mrgsolve::outvars(inputted_model_2())$capture, mrgsolve::outvars(inputted_model_2())$cmt),
                           selected = mrgsolve::outvars(inputted_model_2())$capture[1])

      if (show_debugging_msg) {
        message('updated yaxis_name_2')
      }
    }
  }, label = 'update_y_axis_name_2')

  observeEvent(nmdata_cmt_filtered(), {
    updateSelectizeInput(session,
                         "nonmem_y_axis",
                         choices = names(nmdata_cmt_filtered()) %>% sort(),
                         selected = 'DV')
  }, label = 'update_nonmem_y_axis')

  observeEvent(nmdata_cmt_filtered(), {
    updateSelectizeInput(session,
                         "color_data_by",
                         choices = names(nmdata_cmt_filtered()) %>% sort(),
                         selected = NA)
  }, label = 'update_color_data_by')

  observeEvent(c(nmdata_cmt_filtered(), input$color_data_by), {
    updateSelectizeInput(session,
                         "stat_sum_data_by",
                         choices = names(nmdata_cmt_filtered()) %>% sort(),
                         selected = input$color_data_by)
  }, label = 'update_stat_sum_data_by')

  sim_1_dataset_arg <- reactive({
    tmp <- NULL

    if (model_1_checkpoint$sim_generated_model_1) {
      if (input$yaxis_name %in% colnames(simulation_output_model_1())) {
        if (input$show_model_1) {
          tmp <- simulation_output_model_1()
        }}}
    return(tmp)
  }, label = 'sim_1_dataset_arg')

  sim_2_dataset_arg <- reactive({
    tmp2 <- NULL

    if (model_2_checkpoint$sim_generated_model_2) {
      if (input$yaxis_name_2 %in% colnames(simulation_output_model_2())) {
        if (input$show_model_2) {
          tmp2 <- simulation_output_model_2()
        }}}
    return(tmp2)
  }, label = 'sim_2_dataset_arg')

  nonmem_dataset_arg <- reactive({
    tmp <- NULL

    if (input$combine_nmdata && final_output_executed() && input$nonmem_y_axis %in% colnames(nmdata_cmt_filtered())) {

      # Do some more trimming
      tmp <- trim_columns(data = nmdata_cmt_filtered(),
                          x_axis = "TIMEADJ",
                          y_axis = input$nonmem_y_axis,
                          color = input$color_data_by,
                          type_of_plot = "sim_plot",
                          #facet_name = input$facet_by,
                          insert_med_line = input$stat_sum_data_option,
                          med_line_by = input$stat_sum_data_by)

    }
    return(tmp)
  }, label = 'nonmem_dataset_arg')

  ## simulation_page_plot() ----
  simulation_page_plot <- reactive({

    if (!is.null(sim_1_dataset_arg()) || !is.null(sim_2_dataset_arg())) {
      if (show_debugging_msg) {
        message('Plotting condition met, generating a plot')
      }

      sim_plot <- plot_data_with_nm(input_dataset1 = sim_1_dataset_arg(),
                                    input_dataset2 = sim_2_dataset_arg(),
                                    nonmem_dataset = nonmem_dataset_arg(),
                                    color_data_by = input$color_data_by,
                                    xvar = 'TIMEADJ',
                                    yvar = input$yaxis_name,
                                    yvar_2 = input$yaxis_name_2,
                                    log_x_axis = input$log_x_axis,
                                    log_y_axis = input$log_y_axis,
                                    nm_yvar = input$nonmem_y_axis,
                                    geom_point_sim_option = input$geom_point_sim_option,
                                    geom_point_data_option = input$geom_point_data_option,
                                    stat_summary_data_option = input$stat_sum_data_option,
                                    stat_summary_data_by     = input$stat_sum_data_by,
                                    xlabel = input$x_axis_label,
                                    ylabel = input$y_axis_label,
                                    title = input$plot_title_sim,
                                    line_color_1 = model_1_color,
                                    line_color_2 = model_2_color,
                                    debug  = show_debugging_msg
      )

      if (input$combine_nmdata && is.null(nonmem_dataset_arg())) {
        sim_plot <- sim_plot +
          ggplot2::labs(title = unsupported_dataset) +
          ggplot2::theme(plot.title = ggplot2::element_text(color = model_1_color))
      }

      return(sim_plot)
    }
  }, label = 'simulation_page_plot')

  output$simulation_plot_output <- renderUI({
    shiny::conditionalPanel(
      condition = "true",
      div(style = "height:600px",
          if (!is.null(simulation_page_plot())) {
            if(input$do_sim_plotly) {
              plotly::plotlyOutput("sim_plotly", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
            } else {
              plotOutput("sim_ggplot", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
            }
          }
      )
    )
  })

  output$sim_ggplot <- renderPlot(simulation_page_plot() + add_watermark(watermark_toggle = insert_watermark) + ggplot2::theme(text = ggplot2::element_text(size = 16)))

  output$sim_plotly <- plotly::renderPlotly(convert_to_plotly_watermark(simulation_page_plot(),
                                                                        format           = input$plotly_format,
                                                                        filename         = input$plotly_filename,
                                                                        width            = input$plotly_width,
                                                                        height           = input$plotly_height,
                                                                        plotly_watermark = insert_watermark,
                                                                        debug            = show_debugging_msg)
  )

  combine_sim_download_data <- reactive({
    downloadable_df <- check_and_combine_df(model_1_is_valid = model_1_checkpoint$sim_generated_model_1,
                                            model_2_is_valid = model_2_checkpoint$sim_generated_model_2,
                                            input_df_1 = simulation_output_model_1(),
                                            input_df_2 = simulation_output_model_2())
    return(downloadable_df)
  })


  ## Download simulated combined table ----
  output$download_sim_data <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_simulated_data.csv")
    },
    content = function(file) {
      #write.csv(combine_sim_download_data(), file, quote = FALSE, row.names = FALSE)
      data.table::fwrite(combine_sim_download_data(), file, quote = FALSE, row.names = FALSE)
    }
  )

  #### Sim Plot download section
  observeEvent(input$do_sim_plotly, {
    if (input$do_sim_plotly) {
      shinyjs::disable("download_sim_plot")
      updateSelectInput(session, "plotly_format", label = plotly_format_label,
                        choices = c("png", "jpeg", "svg", "webp"))
    } else {
      shinyjs::enable("download_sim_plot")
      updateSelectInput(session, "plotly_format", label = plotly_format_label,
                        choices = c("png", "pdf", "jpeg", "svg"))
    }
  }, label = "update_download_sim_plot")

  output$download_sim_plot <- downloadHandler(
    filename = function() {
      paste0(input$plotly_filename, ".", input$plotly_format)
    },
    content = function(file) {
      ggplot2::ggsave(file,
                      plot = simulation_page_plot() + add_watermark(watermark_toggle = insert_watermark),
                      device = input$plotly_format,
                      units = "px",
                      width = input$plotly_width,
                      height = input$plotly_height
      )
    }
  )

  ## Download model code as .cpp ----
  output$download_cpp_model_1 <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_model_1.cpp")
    },
    content = function(file) {
      mrgsolve::mwrite_cpp(changed_reacted_param_model_1(), file = file, update = FALSE)
    }
  )

  output$download_cpp_model_2 <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_model_2.cpp")
    },
    content = function(file) {
      mrgsolve::mwrite_cpp(changed_reacted_param_model_2(), file = file, update = FALSE)
    }
  )

  # Page 3 Parameter Sensitivity Analysis ----
  ## Outline ----
  ### Update input$auc_time_range ----
  observe({
    shiny::req(simulation_output_model_1())
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'min_nca_obs_time_model_1',
                                    label = NULL,
                                    choices = sort(unique(simulation_output_model_1()$TIME)),
                                    select = min(unique(simulation_output_model_1()$TIME))
    )

    shinyWidgets::updatePickerInput(session,
                                    inputId = 'max_nca_obs_time_model_1',
                                    label = NULL,
                                    choices = sort(unique(simulation_output_model_1()$TIME)),
                                    select = max(unique(simulation_output_model_1()$TIME))
    )
  }, label = 'update model_1 time selection')

  observeEvent(input$min_nca_obs_time, {
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'max_nca_obs_time_model_1',
                                    label = NULL,
                                    choices = unique(sort(simulation_output_model_1()$TIME[simulation_output_model_1()$TIME > as.numeric(input$min_nca_obs_time_model_1)])),
                                    selected = input$max_nca_obs_time_model_1
    )
  }, label = 'update model_1 time selection (Max)')

  observeEvent(input$max_nca_obs_time_model_1, {
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'min_nca_obs_time_model_1',
                                    label = NULL,
                                    choices = unique(sort(simulation_output_model_1()$TIME[simulation_output_model_1()$TIME < as.numeric(input$max_nca_obs_time_model_1)])),
                                    selected = input$min_nca_obs_time_model_1
    )
  }, label = 'update model_1 time selection (Min)')

  numeric_obs_min_model_1 <- reactive({
    as.numeric(input$min_nca_obs_time_model_1)
  }, label = 'convert model_1 time to numeric (Min)')

  numeric_obs_max_model_1 <- reactive({
    as.numeric(input$max_nca_obs_time_model_1)
  }, label = 'convert model_1 time to numeric (Max)')

  ### Update input$param_selector ----
  observeEvent(inputted_model_1(), {
    updateSelectInput(session,
                      "param_selector_model_1",
                      choices  = names(inputted_model_1())$param)
  }, label = 'model_1 PSA param_selector')

  # Read selected param name
  ## pas_init_param_name() ----
  pas_init_param_name <- reactive({
    reactive_name <- lapply(1:3, function(i) {
      paste0(input$param_selector_model_1, i)
    })
    do.call(tagList, reactive_name)

    return(reactive_name)
  }, label = 'pas_init_param_name_model_1')

  # Read selected param value
  ## pas_init_param_value ----
  pas_init_param_value <- reactive({
    changed_reacted_param_model_1()[[input$param_selector_model_1]]
  }, label = 'pas_init_param_value_MODEL1')

  ## Generate param min/mid/max UI ----
  ### param_min_ui_model_1() ----
  param_min_ui_model_1 <- eventReactive(pas_init_param_name(), {
    numericInput(
      inputId = "param_min_id_model_1",
      label = paste0('Min ', input$param_selector_model_1) , #' Default Value: '),
      value = pas_init_param_value() * min_param_multiple,
      step = 0.1
    )
  }, label = 'param_min_ui_model_1')

  ### param_mid_ui_model_1() ----
  param_mid_ui_model_1 <- eventReactive(pas_init_param_name(), {
    numericInput(
      inputId = "param_mid_id_model_1",
      label = paste0('Mid ', input$param_selector_model_1) , #' Default Value: '),
      value = pas_init_param_value() * mid_param_multiple,
      step = 0.1
    )
  }, label = 'param_mid_ui_model_1')

  ### param_max_ui() ----
  param_max_ui_model_1 <- eventReactive(pas_init_param_name(), {
    numericInput(
      inputId = "param_max_id_model_1",
      label = paste0('Max ', input$param_selector_model_1) , #' Default Value: '),
      value = pas_init_param_value() * max_param_multiple,
      step = 0.1
    )
  }, label = 'param_max_ui_model_1')

  ## Generate min/mid/max modified param into model ----
  ### applied_param_min() ----
  applied_param_min_model_1 <- reactive({

    shiny::req(!shinyAce::is.empty(input$param_min_id_model_1))

    dataframe <- data.frame(name = input$param_selector_model_1, value = input$param_min_id_model_1) %>%
      dplyr::as_tibble()

    new_model_min <- update_model_object(changed_reacted_param_model_1(), dataframe)
    return(new_model_min)
  }, label = 'applied_param_min_model_1')

  ### applied_param_mid() ----
  applied_param_mid_model_1 <- reactive({

    shiny::req(!shinyAce::is.empty(input$param_mid_id_model_1))

    dataframe <- data.frame(name = input$param_selector_model_1, value = input$param_mid_id_model_1) %>%
      dplyr::as_tibble()

    new_model_mid <- update_model_object(changed_reacted_param_model_1(), dataframe)

    return(new_model_mid)
  }, label = 'applied_param_mid_model_1')

  ### applied_param_max() ----
  applied_param_max_model_1 <- reactive({

    shiny::req(!shinyAce::is.empty(input$param_max_id_model_1))

    dataframe <- data.frame(name = input$param_selector_model_1, value = input$param_max_id_model_1) %>%
      dplyr::as_tibble()

    new_model_max <- update_model_object(changed_reacted_param_model_1(), dataframe)

    return(new_model_max)
  }, label = 'applied_param_max_model_1')

  ### Handling edge case where WT is a param and WT-based dosing is used, which
  ### necessitates a unique wt_multiplication_value for each model

  wt_multiplication_min_model_1 <- reactive({
    shiny::req(applied_param_min_model_1())
    wt_multiplication_value <- 1
    if(model_1_is_valid()) {
      if(input$wt_based_dosing_checkbox & input$wt_based_dosing_name %in% names(mrgsolve::param(applied_param_min_model_1()))) {
        wt_multiplication_value <- mrgsolve::param(applied_param_min_model_1())[[input$wt_based_dosing_name]] # E.g. "input$WT"
      }
    }
    return(wt_multiplication_value)
  }, label = 'wt_multiplication_min_model_1')

  wt_multiplication_mid_model_1 <- reactive({
    shiny::req(applied_param_mid_model_1())
    wt_multiplication_value <- 1
    if(model_1_is_valid()) {
      if(input$wt_based_dosing_checkbox & input$wt_based_dosing_name %in% names(mrgsolve::param(applied_param_mid_model_1()))) {
        wt_multiplication_value <- mrgsolve::param(applied_param_mid_model_1())[[input$wt_based_dosing_name]] # E.g. "input$WT"
      }
    }
    return(wt_multiplication_value)
  }, label = 'wt_multiplication_mid_model_1')

  wt_multiplication_max_model_1 <- reactive({
    shiny::req(applied_param_max_model_1())
    wt_multiplication_value <- 1
    if(model_1_is_valid()) {
      if(input$wt_based_dosing_checkbox & input$wt_based_dosing_name %in% names(mrgsolve::param(applied_param_max_model_1()))) {
        wt_multiplication_value <- mrgsolve::param(applied_param_max_model_1())[[input$wt_based_dosing_name]] # E.g. "input$WT"
      }
    }
    return(wt_multiplication_value)
  }, label = 'wt_multiplication_max_model_1')


  dosing_regimen_min_model_1 <- reactive({
    shiny::req(applied_param_min_model_1())
    dose_regimen <- generate_dosing_regimens(
      amt1 = input$amt1, delay_time1 = input$delay_time1, cmt1 = input$cmt1_model_1, tinf1 = input$tinf1, total1 = input$total1, ii1 = input$ii1,
      amt2 = input$amt2, delay_time2 = input$delay_time2, cmt2 = input$cmt2_model_1, tinf2 = input$tinf2, total2 = input$total2, ii2 = input$ii2,
      amt3 = input$amt3, delay_time3 = input$delay_time3, cmt3 = input$cmt3_model_1, tinf3 = input$tinf3, total3 = input$total3, ii3 = input$ii3,
      amt4 = input$amt4, delay_time4 = input$delay_time4, cmt4 = input$cmt4_model_1, tinf4 = input$tinf4, total4 = input$total4, ii4 = input$ii4,
      amt5 = input$amt5, delay_time5 = input$delay_time5, cmt5 = input$cmt5_model_1, tinf5 = input$tinf5, total5 = input$total5, ii5 = input$ii5,
      mw_conversion = mw_conversion_model_1(),
      wt_multiplication_value = wt_multiplication_min_model_1(),
      create_dummy_ev = TRUE,
      debug = show_debugging_msg
    )
    return(dose_regimen)
  }, label = 'dosing_regimen_min_model_1')

  dosing_regimen_mid_model_1 <- reactive({
    shiny::req(applied_param_mid_model_1())
    dose_regimen <- generate_dosing_regimens(
      amt1 = input$amt1, delay_time1 = input$delay_time1, cmt1 = input$cmt1_model_1, tinf1 = input$tinf1, total1 = input$total1, ii1 = input$ii1,
      amt2 = input$amt2, delay_time2 = input$delay_time2, cmt2 = input$cmt2_model_1, tinf2 = input$tinf2, total2 = input$total2, ii2 = input$ii2,
      amt3 = input$amt3, delay_time3 = input$delay_time3, cmt3 = input$cmt3_model_1, tinf3 = input$tinf3, total3 = input$total3, ii3 = input$ii3,
      amt4 = input$amt4, delay_time4 = input$delay_time4, cmt4 = input$cmt4_model_1, tinf4 = input$tinf4, total4 = input$total4, ii4 = input$ii4,
      amt5 = input$amt5, delay_time5 = input$delay_time5, cmt5 = input$cmt5_model_1, tinf5 = input$tinf5, total5 = input$total5, ii5 = input$ii5,
      mw_conversion = mw_conversion_model_1(),
      wt_multiplication_value = wt_multiplication_mid_model_1(),
      create_dummy_ev = TRUE,
      debug = show_debugging_msg
    )
    return(dose_regimen)
  }, label = 'dosing_regimen_mid_model_1')

  dosing_regimen_max_model_1 <- reactive({
    shiny::req(applied_param_max_model_1())
    dose_regimen <- generate_dosing_regimens(
      amt1 = input$amt1, delay_time1 = input$delay_time1, cmt1 = input$cmt1_model_1, tinf1 = input$tinf1, total1 = input$total1, ii1 = input$ii1,
      amt2 = input$amt2, delay_time2 = input$delay_time2, cmt2 = input$cmt2_model_1, tinf2 = input$tinf2, total2 = input$total2, ii2 = input$ii2,
      amt3 = input$amt3, delay_time3 = input$delay_time3, cmt3 = input$cmt3_model_1, tinf3 = input$tinf3, total3 = input$total3, ii3 = input$ii3,
      amt4 = input$amt4, delay_time4 = input$delay_time4, cmt4 = input$cmt4_model_1, tinf4 = input$tinf4, total4 = input$total4, ii4 = input$ii4,
      amt5 = input$amt5, delay_time5 = input$delay_time5, cmt5 = input$cmt5_model_1, tinf5 = input$tinf5, total5 = input$total5, ii5 = input$ii5,
      mw_conversion = mw_conversion_model_1(),
      wt_multiplication_value = wt_multiplication_max_model_1(),
      create_dummy_ev = TRUE,
      debug = show_debugging_msg
    )
    return(dose_regimen)
  }, label = 'dosing_regimen_max_model_1')

  ### new_sim_min() ----
  new_sim_min_model_1 <- reactive({
    shiny::req(model_1_is_valid())
    sim_output_min_model_1 <-
      run_single_sim(
        input_model_object = applied_param_min_model_1(),
        pred_model         = model_1_is_pred(),
        ev_df = dosing_regimen_min_model_1(),
        model_dur = model_duration_argument_model_1(),
        model_rate= model_rate_argument_model_1(),
        sampling_times = sampling_options(),
        debug = show_debugging_msg,
        divide_by      = time_value()
      )
    return(sim_output_min_model_1)
  }, label = 'new_sim_min_model_1')

  ### new_sim_mid() ----
  new_sim_mid_model_1 <- reactive({
    shiny::req(model_1_is_valid())
    sim_output_mid_model_1 <-
      run_single_sim(
        input_model_object = applied_param_mid_model_1(),
        pred_model         = model_1_is_pred(),
        ev_df = dosing_regimen_mid_model_1(),
        model_dur = model_duration_argument_model_1(),
        model_rate= model_rate_argument_model_1(),
        sampling_times = sampling_options(),
        debug = show_debugging_msg,
        divide_by      = time_value()
      )
    return(sim_output_mid_model_1)

  }, label = 'new_sim_mid_model_1')

  ### new_sim_max() ----
  new_sim_max_model_1 <- reactive({
    shiny::req(model_1_is_valid())
    sim_output_max_model_1 <-
      run_single_sim(
        input_model_object = applied_param_max_model_1(),
        pred_model         = model_1_is_pred(),
        ev_df = dosing_regimen_max_model_1(),
        model_dur = model_duration_argument_model_1(),
        model_rate= model_rate_argument_model_1(),
        sampling_times = sampling_options(),
        debug = show_debugging_msg,
        divide_by      = time_value()
      )
    return(sim_output_max_model_1)

  }, label = 'new_sim_max_model_1')

  ## Generate min/mid/max ui ----
  ### UI: input$param min ----
  output$param_widget_output_min_model_1 <- renderUI({
    param_min_ui_model_1()
  })

  ### UI: input$param mid ----
  output$param_widget_output_mid_model_1 <- renderUI({
    param_mid_ui_model_1()
  })

  ### UI: input$param max ----
  output$param_widget_output_max_model_1 <- renderUI({
    param_max_ui_model_1()
  })

  ## Generate min/mid/max metrics df ----
  react_metrics_min_model_1 <- reactive({pknca_table(new_sim_min_model_1(),
                                                     input$yaxis_name,
                                                     start_time = numeric_obs_min_model_1(),
                                                     end_time = numeric_obs_max_model_1(),
                                                     debug = show_debugging_msg)
  }, label = 'react_metrics_min_model_1')

  react_metrics_mid_model_1 <- reactive({pknca_table(new_sim_mid_model_1(),
                                                     input$yaxis_name,
                                                     start_time = numeric_obs_min_model_1(),
                                                     end_time = numeric_obs_max_model_1(),
                                                     debug = show_debugging_msg)
  }, label = 'react_metrics_mid_model_1')

  react_metrics_max_model_1 <- reactive({pknca_table(new_sim_max_model_1(),
                                                     input$yaxis_name,
                                                     start_time = numeric_obs_min_model_1(),
                                                     end_time = numeric_obs_max_model_1(),
                                                     debug = show_debugging_msg)

  }, label = 'react_metrics_max_model_1')

  ## Generate metrics valueBoxes ----
  output$cmax_min_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_1(), 'CMAX_ranged',     'Cmax', color = "olive", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$cavg_min_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_1(), 'CAVG_ranged',     'Cavg', color = "olive", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$cmin_min_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_1(), 'CMIN_ranged',     'Cmin', color = "olive", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$auc_min_model_1      <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_1(), 'AUC_ranged' ,     'AUC' , color = "olive", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$tmax_min_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_1(), 'TMAX_ranged',     'Tmax', color = "olive", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$tmin_min_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_1(), 'TMIN_ranged',     'Tmin', color = "olive", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$cfbpct_min_model_1   <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_1(), 'CFBPCT_ranged',   '% CFB', color = "olive", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$mcfbpct_min_model_1  <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_1(), 'MEANCFBPCT_ranged',   'Mean % CFB', color = "olive", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$nadirpct_min_model_1 <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_1(), 'NADIRPCT_ranged', 'Nadir % CFB', color = "olive", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})

  output$cmax_mid_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_1(), 'CMAX_ranged',     'Cmax', color = "orange", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$cavg_mid_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_1(), 'CAVG_ranged',     'Cavg', color = "orange", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$cmin_mid_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_1(), 'CMIN_ranged',     'Cmin', color = "orange", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$auc_mid_model_1      <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_1(), 'AUC_ranged' ,     'AUC' , color = "orange", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$tmax_mid_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_1(), 'TMAX_ranged',     'Tmax', color = "orange", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$tmin_mid_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_1(), 'TMIN_ranged',     'Tmin', color = "orange", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$cfbpct_mid_model_1   <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_1(), 'CFBPCT_ranged',   '% CFB', color = "orange", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$mcfbpct_mid_model_1  <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_1(), 'MEANCFBPCT_ranged',   'Mean % CFB', color = "orange", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$nadirpct_mid_model_1 <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_1(), 'NADIRPCT_ranged', 'Nadir % CFB', color = "orange", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})

  output$cmax_max_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_1(), 'CMAX_ranged',     'Cmax', color = "purple", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$cavg_max_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_1(), 'CAVG_ranged',     'Cavg', color = "purple", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$cmin_max_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_1(), 'CMIN_ranged',     'Cmin', color = "purple", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$auc_max_model_1      <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_1(), 'AUC_ranged' ,     'AUC' , color = "purple", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$tmax_max_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_1(), 'TMAX_ranged',     'Tmax', color = "purple", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$tmin_max_model_1     <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_1(), 'TMIN_ranged',     'Tmin', color = "purple", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$cfbpct_max_model_1   <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_1(), 'CFBPCT_ranged',   '% CFB', color = "purple", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$mcfbpct_max_model_1  <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_1(), 'MEANCFBPCT_ranged',   'Mean % CFB', color = "purple", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})
  output$nadirpct_max_model_1 <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_1(), 'NADIRPCT_ranged', 'Nadir % CFB', color = "purple", sigdig = input$digits_model_1, dp = input$dp_checkbox_model_1)})

  # PSA plot from new_sim_min/mid/max
  ## psa_page_plot_model_1() ----
  psa_page_plot_model_1 <- reactive({
    shiny::req(model_1_is_valid())

    nonmem_dataset <- if (input$combine_nmdata_1_model_1 && final_output_executed()) {
      nmdata_cmt_filtered()
    } else {
      NULL
    }

    title <- if (input$combine_nmdata_1_model_1 && is.null(nonmem_dataset)) {
      unsupported_dataset
    } else {
      if (!is.null(input$plot_title_psa_model_1)) {input$plot_title_psa_model_1} else {
        NULL
      }
    }

    psa_plot_model_1 <- plot_three_data_with_nm(
      input_dataset_min = new_sim_min_model_1(),
      input_dataset_mid = new_sim_mid_model_1(),
      input_dataset_max = new_sim_max_model_1(),
      param_name = input$param_selector_model_1,
      param_min_value = input$param_min_id_model_1,
      param_mid_value = input$param_mid_id_model_1,
      param_max_value = input$param_max_id_model_1,
      x_min = numeric_obs_min_model_1()/time_value(),
      x_max = numeric_obs_max_model_1()/time_value(),
      nonmem_dataset = nonmem_dataset,
      xvar = 'TIMEADJ',
      yvar = input$yaxis_name,
      log_x_axis = input$log_x_axis_model_1,
      log_y_axis = input$log_y_axis_model_1,
      geom_point_sim_option = input$geom_point_sim_option_model_1,
      geom_point_data_option = input$geom_point_data_option_model_1,
      geom_ribbon_option = input$geom_ribbon_option_model_1,
      geom_vline_option = input$geom_vline_option_model_1,
      stat_summary_data_option = input$stat_sum_data_option_model_1,
      nm_yvar = input$nonmem_y_axis,
      xlabel = input$x_axis_label,
      ylabel = input$y_axis_label,
      debug  = show_debugging_msg,
      title = title
    )

    return(psa_plot_model_1)
  }, label = 'psa_page_plot_model_1')

  ## UI: output$psa_plot_output ----
  output$psa_plot_output_model_1 <- renderUI({
    shiny::conditionalPanel(
      condition = "true",
      div(style = "height:600px",
          if(input$do_psa_plotly_model_1) {
            plotly::plotlyOutput("psa_plotly_model_1", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
          } else {
            plotOutput("psa_ggplot_model_1", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
          }
      )
    )
  })

  output$psa_ggplot_model_1 <- renderPlot(psa_page_plot_model_1() + add_watermark(watermark_toggle = insert_watermark) + ggplot2::theme(text = ggplot2::element_text(size = 16)))

  output$psa_plotly_model_1 <- plotly::renderPlotly(convert_to_plotly_watermark(psa_page_plot_model_1(),
                                                                                format           = input$plotly3_format_model_1,
                                                                                filename         = input$plotly3_filename_model_1,
                                                                                width            = input$plotly3_width_model_1,
                                                                                height           = input$plotly3_height_model_1,
                                                                                plotly_watermark = insert_watermark,
                                                                                debug            = show_debugging_msg)
  )

  #### PSA Plot download section
  observeEvent(input$do_psa_plotly_model_1, {
    if (input$do_psa_plotly_model_1) {
      shinyjs::disable("download_psa_plot_model_1")
      updateSelectInput(session, "plotly3_format_model_1", label = plotly_format_label,
                        choices = c("png", "jpeg", "svg", "webp"))
    } else {
      shinyjs::enable("download_psa_plot_model_1")
      updateSelectInput(session, "plotly3_format_model_1", label = plotly_format_label,
                        choices = c("png", "pdf", "jpeg", "svg"))
    }
  }, label = "update_download_psa_plot_model_1")

  output$download_psa_plot_model_1 <- downloadHandler(
    filename = function() {
      paste0(input$plotly3_filename_model_1, ".", input$plotly3_format_model_1)
    },
    content = function(file) {
      ggplot2::ggsave(file,
                      plot = psa_page_plot_model_1() + add_watermark(watermark_toggle = insert_watermark),
                      device = input$plotly3_format_model_1,
                      units = "px",
                      width = input$plotly3_width_model_1,
                      height = input$plotly3_height_model_1
      )
    }
  )

  # Model 2 ----
  ### Update input$auc_time_range ----
  observe({
    shiny::req(simulation_output_model_2())
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'min_nca_obs_time_model_2',
                                    label = NULL,
                                    choices = sort(unique(simulation_output_model_2()$TIME)),
                                    select = min(unique(simulation_output_model_2()$TIME))
    )

    shinyWidgets::updatePickerInput(session,
                                    inputId = 'max_nca_obs_time_model_2',
                                    label = NULL,
                                    choices = sort(unique(simulation_output_model_2()$TIME)),
                                    select = max(unique(simulation_output_model_2()$TIME))
    )
  }, label = 'update model_2 time selection')

  observeEvent(input$min_nca_obs_time_model_2, {
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'max_nca_obs_time_model_2',
                                    label = NULL,
                                    choices = unique(sort(simulation_output_model_2()$TIME[simulation_output_model_2()$TIME > as.numeric(input$min_nca_obs_time_model_2)])),
                                    selected = input$max_nca_obs_time_model_2
    )
  }, label = 'update model_2 time selection (Max)')

  observeEvent(input$max_nca_obs_time_model_2, {
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'min_nca_obs_time_model_2',
                                    label = NULL,
                                    choices = unique(sort(simulation_output_model_2()$TIME[simulation_output_model_2()$TIME < as.numeric(input$max_nca_obs_time_model_2)])),
                                    selected = input$min_nca_obs_time_model_2
    )
  }, label = 'update model_2 time selection (Min)')

  numeric_obs_min_model_2 <- reactive({
    as.numeric(input$min_nca_obs_time_model_2)
  }, label = 'convert model_2 time to numeric (Min)')

  numeric_obs_max_model_2 <- reactive({
    as.numeric(input$max_nca_obs_time_model_2)
  }, label = 'convert model_2 time to numeric (Min)')

  ### Update input$param_selector_model_2 ----
  observeEvent(inputted_model_2(), {
    updateSelectInput(session,
                      "param_selector_model_2",
                      choices  = names(inputted_model_2())$param)
  }, label = 'model_2 PSA param_selector')

  # Read selected param name
  ## pas_init_param_name_model_2() ----
  pas_init_param_name_model_2 <- reactive({
    reactive_name <- lapply(1:3, function(i) {
      paste0(input$param_selector_model_2, i)
    })
    do.call(tagList, reactive_name)

    return(reactive_name)
  }, label = 'pas_init_param_name_model_2')

  # Read selected param value
  ## pas_init_param_value_model_2 ----
  pas_init_param_value_model_2 <- reactive({
    changed_reacted_param_model_2()[[input$param_selector_model_2]]
  }, label = 'pas_init_param_value_model_2')

  ## Generate param min/mid/max UI ----
  ### param_min_ui_model_2() ----
  param_min_ui_model_2 <- eventReactive(pas_init_param_name_model_2(), {
    numericInput(
      inputId = "param_min_id_model_2",
      label = paste0('Min ', input$param_selector_model_2) , #' Default Value: '),
      value = pas_init_param_value_model_2() * min_param_multiple,
      step = 0.1
    )
  }, label = 'param_min_ui_model_2')

  ### param_mid_ui() ----
  param_mid_ui_model_2 <- eventReactive(pas_init_param_name_model_2(), {
    numericInput(
      inputId = "param_mid_id_model_2",
      label = paste0('Mid ', input$param_selector_model_2) , #' Default Value: '),
      value = pas_init_param_value_model_2() * mid_param_multiple,
      step = 0.1
    )
  }, label = 'param_mid_ui_model_2')

  ### param_max_ui_model_2() ----
  param_max_ui_model_2 <- eventReactive(pas_init_param_name_model_2(), {
    numericInput(
      inputId = "param_max_id_model_2",
      label = paste0('Max ', input$param_selector_model_2) , #' Default Value: '),
      value = pas_init_param_value_model_2() * max_param_multiple,
      step = 0.1
    )
  }, label = 'param_max_ui_model_2')

  ## Generate min/mid/max ui ----
  ### UI: input$param min ----
  output$param_widget_output_min_model_2 <- renderUI({
    param_min_ui_model_2()
  })

  ### UI: input$param mid ----
  output$param_widget_output_mid_model_2 <- renderUI({
    param_mid_ui_model_2()
  })

  ### UI: input$param max ----
  output$param_widget_output_max_model_2 <- renderUI({
    param_max_ui_model_2()
  })

  # Generate min/mid/max modified param into model ----
  ## applied_param_min_MODEL2() ----
  applied_param_min_model_2 <- reactive({

    shiny::req(!shinyAce::is.empty(input$param_min_id_model_2))

    dataframe <- data.frame(name = input$param_selector_model_2, value = input$param_min_id_model_2) %>%
      dplyr::as_tibble()

    new_model_min <- update_model_object(changed_reacted_param_model_2(), dataframe)

    return(new_model_min)
  }, label = 'applied_param_min_model_2')

  ### applied_param_mid_model_2() ----
  applied_param_mid_model_2 <- reactive({

    shiny::req(!shinyAce::is.empty(input$param_mid_id_model_2))

    dataframe <- data.frame(name = input$param_selector_model_2, value = input$param_mid_id_model_2) %>%
      dplyr::as_tibble()

    new_model_mid <- update_model_object(changed_reacted_param_model_2(), dataframe)

    return(new_model_mid)
  }, label = 'applied_param_mid_model_2')

  ### applied_param_max_model_2() ----
  applied_param_max_model_2 <- reactive({

    shiny::req(!shinyAce::is.empty(input$param_max_id_model_2))

    dataframe <- data.frame(name = input$param_selector_model_2, value = input$param_max_id_model_2) %>%
      dplyr::as_tibble()

    new_model_max <- update_model_object(changed_reacted_param_model_2(), dataframe)

    return(new_model_max)
  }, label = 'applied_param_max_model_2')

  ### Handling edge case where WT is a param and WT-based dosing is used, which
  ### necessitates a unique wt_multiplication_value for each model

  wt_multiplication_min_model_2 <- reactive({
    shiny::req(applied_param_min_model_2())
    wt_multiplication_value <- 1
    if(model_2_is_valid()) {
      if(input$wt_based_dosing_checkbox_2 & input$wt_based_dosing_name_2 %in% names(mrgsolve::param(applied_param_min_model_2()))) {
        wt_multiplication_value <- mrgsolve::param(applied_param_min_model_2())[[input$wt_based_dosing_name_2]] # E.g. "input$WT"
      }
    }
    return(wt_multiplication_value)
  }, label = 'wt_multiplication_min_model_1')

  wt_multiplication_mid_model_2 <- reactive({
    shiny::req(applied_param_mid_model_2())
    wt_multiplication_value <- 1
    if(model_2_is_valid()) {
      if(input$wt_based_dosing_checkbox_2 & input$wt_based_dosing_name_2 %in% names(mrgsolve::param(applied_param_mid_model_2()))) {
        wt_multiplication_value <- mrgsolve::param(applied_param_mid_model_2())[[input$wt_based_dosing_name_2]] # E.g. "input$WT"
      }
    }
    return(wt_multiplication_value)
  }, label = 'wt_multiplication_mid_model_2')

  wt_multiplication_max_model_2 <- reactive({
    shiny::req(applied_param_max_model_2())
    wt_multiplication_value <- 1
    if(model_2_is_valid()) {
      if(input$wt_based_dosing_checkbox_2 & input$wt_based_dosing_name_2 %in% names(mrgsolve::param(applied_param_max_model_2()))) {
        wt_multiplication_value <- mrgsolve::param(applied_param_max_model_2())[[input$wt_based_dosing_name_2]] # E.g. "input$WT"
      }
    }
    return(wt_multiplication_value)
  }, label = 'wt_multiplication_max_model_2')

  dosing_regimen_min_model_2 <- reactive({
    shiny::req(applied_param_min_model_2())
    dose_regimen <- generate_dosing_regimens(
      amt1 = input$amt1_2, delay_time1 = input$delay_time1_2, cmt1 = input$cmt1_model_2, tinf1 = input$tinf1_2, total1 = input$total1_2, ii1 = input$ii1_2,
      amt2 = input$amt2_2, delay_time2 = input$delay_time2_2, cmt2 = input$cmt2_model_2, tinf2 = input$tinf2_2, total2 = input$total2_2, ii2 = input$ii2_2,
      amt3 = input$amt3_2, delay_time3 = input$delay_time3_2, cmt3 = input$cmt3_model_2, tinf3 = input$tinf3_2, total3 = input$total3_2, ii3 = input$ii3_2,
      amt4 = input$amt4_2, delay_time4 = input$delay_time4_2, cmt4 = input$cmt4_model_2, tinf4 = input$tinf4_2, total4 = input$total4_2, ii4 = input$ii4_2,
      amt5 = input$amt5_2, delay_time5 = input$delay_time5_2, cmt5 = input$cmt5_model_2, tinf5 = input$tinf5_2, total5 = input$total5_2, ii5 = input$ii5_2,
      mw_conversion = mw_conversion_model_2(),
      wt_multiplication_value = wt_multiplication_min_model_2(),
      create_dummy_ev = TRUE,
      debug = show_debugging_msg
    )
    return(dose_regimen)
  }, label = 'dosing_regimen_min_model_2')

  dosing_regimen_mid_model_2 <- reactive({
    shiny::req(applied_param_mid_model_2())
    dose_regimen <- generate_dosing_regimens(
      amt1 = input$amt1_2, delay_time1 = input$delay_time1_2, cmt1 = input$cmt1_model_2, tinf1 = input$tinf1_2, total1 = input$total1_2, ii1 = input$ii1_2,
      amt2 = input$amt2_2, delay_time2 = input$delay_time2_2, cmt2 = input$cmt2_model_2, tinf2 = input$tinf2_2, total2 = input$total2_2, ii2 = input$ii2_2,
      amt3 = input$amt3_2, delay_time3 = input$delay_time3_2, cmt3 = input$cmt3_model_2, tinf3 = input$tinf3_2, total3 = input$total3_2, ii3 = input$ii3_2,
      amt4 = input$amt4_2, delay_time4 = input$delay_time4_2, cmt4 = input$cmt4_model_2, tinf4 = input$tinf4_2, total4 = input$total4_2, ii4 = input$ii4_2,
      amt5 = input$amt5_2, delay_time5 = input$delay_time5_2, cmt5 = input$cmt5_model_2, tinf5 = input$tinf5_2, total5 = input$total5_2, ii5 = input$ii5_2,
      mw_conversion = mw_conversion_model_2(),
      wt_multiplication_value = wt_multiplication_mid_model_2(),
      create_dummy_ev = TRUE,
      debug = show_debugging_msg
    )
    return(dose_regimen)
  }, label = 'dosing_regimen_mid_model_2')

  dosing_regimen_max_model_2 <- reactive({
    shiny::req(applied_param_max_model_2())
    dose_regimen <- generate_dosing_regimens(
      amt1 = input$amt1_2, delay_time1 = input$delay_time1_2, cmt1 = input$cmt1_model_2, tinf1 = input$tinf1_2, total1 = input$total1_2, ii1 = input$ii1_2,
      amt2 = input$amt2_2, delay_time2 = input$delay_time2_2, cmt2 = input$cmt2_model_2, tinf2 = input$tinf2_2, total2 = input$total2_2, ii2 = input$ii2_2,
      amt3 = input$amt3_2, delay_time3 = input$delay_time3_2, cmt3 = input$cmt3_model_2, tinf3 = input$tinf3_2, total3 = input$total3_2, ii3 = input$ii3_2,
      amt4 = input$amt4_2, delay_time4 = input$delay_time4_2, cmt4 = input$cmt4_model_2, tinf4 = input$tinf4_2, total4 = input$total4_2, ii4 = input$ii4_2,
      amt5 = input$amt5_2, delay_time5 = input$delay_time5_2, cmt5 = input$cmt5_model_2, tinf5 = input$tinf5_2, total5 = input$total5_2, ii5 = input$ii5_2,
      mw_conversion = mw_conversion_model_2(),
      wt_multiplication_value = wt_multiplication_max_model_2(),
      create_dummy_ev = TRUE,
      debug = show_debugging_msg
    )
    return(dose_regimen)
  }, label = 'dosing_regimen_max_model_2')

  ## Generate min/mid/max simulation ----
  ### new_sim_min_model_2() ----
  new_sim_min_model_2 <- reactive({
    shiny::req(model_2_is_valid())
    sim_output_min <-
      run_single_sim(
        input_model_object = applied_param_min_model_2(),
        pred_model         = model_2_is_pred(),
        ev_df = dosing_regimen_min_model_2(),
        model_dur = model_duration_argument_model_2(),
        model_rate= model_rate_argument_model_2(),
        sampling_times = sampling_options(),
        debug = show_debugging_msg,
        divide_by      = time_value()
      )

    return(sim_output_min)
  }, label = 'new_sim_min_model_2') #%>% bindEvent(input$param_min_id_model_2, changed_reacted_param_model_2())

  ### new_sim_mid_model_2() ----
  new_sim_mid_model_2 <- reactive({
    shiny::req(model_2_is_valid())
    sim_output_mid <-
      run_single_sim(
        input_model_object = applied_param_mid_model_2(),
        pred_model         = model_2_is_pred(),
        ev_df = dosing_regimen_mid_model_2(),
        model_dur = model_duration_argument_model_2(),
        model_rate= model_rate_argument_model_2(),
        sampling_times = sampling_options(),
        debug = show_debugging_msg,
        divide_by      = time_value()
      )

    return(sim_output_mid)
  }, label = 'new_sim_mid_model_2') #%>% bindEvent(input$param_mid_id_model_2, changed_reacted_param_model_2())

  ### new_sim_max_model_2() ----
  new_sim_max_model_2 <- reactive({
    shiny::req(model_2_is_valid())
    sim_output_max <-
      run_single_sim(
        input_model_object = applied_param_max_model_2(),
        pred_model         = model_2_is_pred(),
        ev_df = dosing_regimen_max_model_2(),
        model_dur = model_duration_argument_model_2(),
        model_rate= model_rate_argument_model_2(),
        sampling_times = sampling_options(),
        debug = show_debugging_msg,
        divide_by      = time_value()
      )

    return(sim_output_max)
  }, label = 'new_sim_max_model_2') #%>% bindEvent(input$param_max_id_model_2, changed_reacted_param_model_2())

  ## Generate min/mid/max metrics df ----
  react_metrics_min_model_2 <- reactive({pknca_table(new_sim_min_model_2(),
                                                     input$yaxis_name_2,
                                                     start_time = numeric_obs_min_model_2(),
                                                     end_time = numeric_obs_max_model_2(),
                                                     debug = show_debugging_msg)
  }, label = 'react_metrics_min_model_2')

  react_metrics_mid_model_2 <- reactive({pknca_table(new_sim_mid_model_2(),
                                                     input$yaxis_name_2,
                                                     start_time = numeric_obs_min_model_2(),
                                                     end_time = numeric_obs_max_model_2(),
                                                     debug = show_debugging_msg)
  }, label = 'react_metrics_mid_model_2')

  react_metrics_max_model_2 <- reactive({pknca_table(new_sim_max_model_2(),
                                                     input$yaxis_name_2,
                                                     start_time = numeric_obs_min_model_2(),
                                                     end_time = numeric_obs_max_model_2(),
                                                     debug = show_debugging_msg)
  }, label = 'react_metrics_max_model_2')

  output$cmax_min_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_2(), 'CMAX_ranged',     'Cmax', color = "olive", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$cavg_min_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_2(), 'CAVG_ranged',     'Cavg', color = "olive", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$cmin_min_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_2(), 'CMIN_ranged',     'Cmin', color = "olive", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$auc_min_model_2      <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_2(), 'AUC_ranged' ,     'AUC' , color = "olive", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$tmax_min_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_2(), 'TMAX_ranged',     'Tmax', color = "olive", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$tmin_min_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_2(), 'TMIN_ranged',     'Tmin', color = "olive", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$cfbpct_min_model_2   <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_2(), 'CFBPCT_ranged',   '% CFB', color = "olive", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$mcfbpct_min_model_2  <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_2(), 'MEANCFBPCT_ranged',   'Mean % CFB', color = "olive", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$nadirpct_min_model_2 <- shinydashboard::renderValueBox({create_value_box(react_metrics_min_model_2(), 'NADIRPCT_ranged', 'Nadir % CFB', color = "olive", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})

  output$cmax_mid_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_2(), 'CMAX_ranged',     'Cmax', color = "orange", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$cavg_mid_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_2(), 'CAVG_ranged',     'Cavg', color = "orange", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$cmin_mid_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_2(), 'CMIN_ranged',     'Cmin', color = "orange", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$auc_mid_model_2      <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_2(), 'AUC_ranged' ,     'AUC' , color = "orange", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$tmax_mid_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_2(), 'TMAX_ranged',     'Tmax', color = "orange", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$tmin_mid_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_2(), 'TMIN_ranged',     'Tmin', color = "orange", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$cfbpct_mid_model_2   <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_2(), 'CFBPCT_ranged',   '% CFB', color = "orange", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$mcfbpct_mid_model_2  <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_2(), 'MEANCFBPCT_ranged',   'Mean % CFB', color = "orange", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$nadirpct_mid_model_2 <- shinydashboard::renderValueBox({create_value_box(react_metrics_mid_model_2(), 'NADIRPCT_ranged', 'Nadir % CFB', color = "orange", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})

  output$cmax_max_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_2(), 'CMAX_ranged',     'Cmax', color = "purple", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$cavg_max_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_2(), 'CAVG_ranged',     'Cavg', color = "purple", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$cmin_max_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_2(), 'CMIN_ranged',     'Cmin', color = "purple", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$auc_max_model_2      <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_2(), 'AUC_ranged' ,     'AUC' , color = "purple", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$tmax_max_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_2(), 'TMAX_ranged',     'Tmax', color = "purple", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$tmin_max_model_2     <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_2(), 'TMIN_ranged',     'Tmin', color = "purple", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$cfbpct_max_model_2   <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_2(), 'CFBPCT_ranged',   '% CFB', color = "purple", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$mcfbpct_max_model_2  <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_2(), 'MEANCFBPCT_ranged',   'Mean % CFB', color = "purple", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})
  output$nadirpct_max_model_2 <- shinydashboard::renderValueBox({create_value_box(react_metrics_max_model_2(), 'NADIRPCT_ranged', 'Nadir % CFB', color = "purple", sigdig = input$digits_model_2, dp = input$dp_checkbox_model_2)})

  # PSA plot from new_sim_min_model_2/mid/max
  ## psa_page_plot_model_2() ----
  psa_page_plot_model_2 <- reactive({
    shiny::req(model_2_is_valid())
    nonmem_dataset <- if (input$combine_nmdata_1_model_2 && final_output_executed()) {
      nmdata_cmt_filtered()
    } else {
      NULL
    }

    title <- if (input$combine_nmdata_1_model_2 && is.null(nonmem_dataset)) {
      unsupported_dataset
    } else {
      if (!is.null(input$plot_title_psa_model_2)) {input$plot_title_psa_model_2} else {
        NULL
      }
    }

    psa_plot_model_2 <- plot_three_data_with_nm(
      input_dataset_min = new_sim_min_model_2(),
      input_dataset_mid = new_sim_mid_model_2(),
      input_dataset_max = new_sim_max_model_2(),
      param_name = input$param_selector_model_2,
      param_min_value = input$param_min_id_model_2,
      param_mid_value = input$param_mid_id_model_2,
      param_max_value = input$param_max_id_model_2,
      x_min = numeric_obs_min_model_2()/time_value(),
      x_max = numeric_obs_max_model_2()/time_value(),
      nonmem_dataset = nonmem_dataset,
      xvar = 'TIMEADJ',
      yvar = input$yaxis_name_2,
      log_x_axis = input$log_x_axis_model_2,
      log_y_axis = input$log_y_axis_model_2,
      geom_point_sim_option = input$geom_point_sim_option_model_2,
      geom_point_data_option = input$geom_point_data_option_model_2,
      geom_ribbon_option = input$geom_ribbon_option_model_2,
      geom_vline_option = input$geom_vline_option_model_2,
      stat_summary_data_option = input$stat_sum_data_option_model_2,
      nm_yvar = input$nonmem_y_axis,
      xlabel = input$x_axis_label,
      ylabel = input$y_axis_label,
      title = title
    )

    return(psa_plot_model_2)
  })

  ## UI: output$psa_plot_output_model_2 ----
  output$psa_plot_output_model_2 <- renderUI({
    shiny::conditionalPanel(
      condition = "true",
      div(style = "height:600px",
          if(input$do_psa_plotly_model_2) {
            plotly::plotlyOutput("psa_plotly_model_2", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
          } else {
            plotOutput("psa_ggplot_model_2", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
          }
      )
    )
  })

  output$psa_ggplot_model_2 <- renderPlot(psa_page_plot_model_2() + add_watermark(watermark_toggle = insert_watermark) + ggplot2::theme(text = ggplot2::element_text(size = 16)))

  output$psa_plotly_model_2 <- plotly::renderPlotly(convert_to_plotly_watermark(psa_page_plot_model_2(),
                                                                                format           = input$plotly3_format_model_2,
                                                                                filename         = input$plotly3_filename_model_2,
                                                                                width            = input$plotly3_width_model_2,
                                                                                height           = input$plotly3_height_model_2,
                                                                                plotly_watermark = insert_watermark,
                                                                                debug            = show_debugging_msg)
  )

  #### PSA Plot download section
  observeEvent(input$do_psa_plotly_model_2, {
    if (input$do_psa_plotly_model_2) {
      shinyjs::disable("download_psa_plot_model_2")
      updateSelectInput(session, "plotly3_format_model_2", label = plotly_format_label,
                        choices = c("png", "jpeg", "svg", "webp"))
    } else {
      shinyjs::enable("download_psa_plot_model_2")
      updateSelectInput(session, "plotly3_format_model_2", label = plotly_format_label,
                        choices = c("png", "pdf", "jpeg", "svg"))
    }
  }, label = "update_download_psa_plot_model_2")

  output$download_psa_plot_model_2 <- downloadHandler(
    filename = function() {
      paste0(input$plotly3_filename_model_2, ".", input$plotly3_format_model_2)
    },
    content = function(file) {
      ggplot2::ggsave(file,
                      plot = psa_page_plot_model_2() + add_watermark(watermark_toggle = insert_watermark),
                      device = input$plotly3_format_model_2,
                      units = "px",
                      width = input$plotly3_width_model_2,
                      height = input$plotly3_height_model_2
      )
    }
  )

  ## Page 4 -- Download Mid Metrics table ----
  output$psa_data_mid_model_1 <- DT::renderDT({
    DT::datatable(react_metrics_mid_model_1() %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ signif(., 5))),
                  caption = paste0('Table results from the middle parameter value are displayed. NCA ranged metrics are derived based on the "Select Time Interval for Deriving Metrics" (Currently from time ', input$min_nca_obs_time_model_1, ' - ', input$max_nca_obs_time_model_1, ')'),
                  rownames = FALSE, # Remove row names
                  options = list(
                    initComplete = DT::JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#08312A', 'color': '#00E47C', 'border-color': '#00E47C'});",
                      "}")
                  ))
  })

  output$psa_data_mid_model_2 <- DT::renderDT({
    DT::datatable(react_metrics_mid_model_2() %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ signif(., 5))),
                  caption = paste0('Table results from the middle parameter value are displayed. NCA ranged metrics are derived based on the "Select Time Interval for Deriving Metrics" (Currently from time ', input$min_nca_obs_time_model_2, ' - ', input$max_nca_obs_time_model_2, ')'),
                  rownames = FALSE, # Remove row names
                  options = list(
                    initComplete = DT::JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#08312A', 'color': '#00E47C', 'border-color': '#00E47C'});",
                      "}")
                  ))
  })

  output$download_psa_data_mid_model_1 <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_mid_metrics_model_1.csv")
    },
    content = function(file) {
      data.table::fwrite(react_metrics_mid_model_1(), file, quote = FALSE, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  output$download_psa_data_mid_model_2 <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_mid_metrics_model_2.csv")
    },
    content = function(file) {
      data.table::fwrite(react_metrics_mid_model_2(), file, quote = FALSE, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  # Page 5 IIV ----
  ## MODEL 1 ----
  extracted_omega_model_1 <- reactiveVal()
  extracted_sigma_model_1 <- reactiveVal()

  n_subj_model_1_clean <- reactive({
    if(sanitize_numeric_input(input$n_subj_model_1, allow_zero = FALSE, as_integer = TRUE) > max_sim_n) {
      general_warning_modal(title = "Error", text_description = max_sim_n_error)
      updateNumericInput(session, "n_subj_model_1", value = 20)
      return(20)
    } else {
      return(sanitize_numeric_input(input$n_subj_model_1, allow_zero = FALSE, as_integer = TRUE))
    }
  })

  # Create a reactive value to hold the dataframe
  rv_cov_1_model_1 <- reactiveValues(df = NULL)
  rv_cov_2_model_1 <- reactiveValues(df = NULL)
  rv_cov_3_model_1 <- reactiveValues(df = NULL)

  # Initialize the dataframe
  observe({
    rv_cov_1_model_1$df <- dplyr::tibble(ID = seq_len(n_subj_model_1_clean()))
    rv_cov_2_model_1$df <- dplyr::tibble(ID = seq_len(n_subj_model_1_clean()))
    rv_cov_3_model_1$df <- dplyr::tibble(ID = seq_len(n_subj_model_1_clean()))
  })

  observeEvent(input$db_model_1, {
    if (input$db_model_1 == "None") {
      shinyjs::disable("age_db_model_1")
      shinyjs::disable("wt_db_model_1")
      shinyjs::disable("males_db_model_1")
    }
    if (input$db_model_1 == "NHANES") {
      shinyjs::enable("age_db_model_1")
      shinyjs::enable("wt_db_model_1")
      shinyjs::enable("males_db_model_1")
      updateSliderInput(session, "age_db_model_1", min = 0, max = 100, value = c(18, 65))
    }
    if (input$db_model_1 == "CDC") {
      shinyjs::enable("age_db_model_1")
      shinyjs::enable("wt_db_model_1")
      shinyjs::enable("males_db_model_1")
      updateSliderInput(session, "age_db_model_1", min = 0, max = 20,  value = c(6, 18), step = 0.5)
    }
    if (input$db_model_1 == "WHO") {
      shinyjs::enable("age_db_model_1")
      shinyjs::enable("wt_db_model_1")
      shinyjs::enable("males_db_model_1")
      updateSliderInput(session, "age_db_model_1", min = 0, max = 10,  value = c(2, 6), step = 0.25)
    }
  }, label = "update_db_slider_model_1")

  ##### UI / Plotting elements for Covariate 1 Model 1

  output$custom_cov_1_ui_model_1 <- renderUI({
    if (input$custom_cov_1_dist_model_1 == "Normal") {
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_1_mean_model_1', 'Mean', value = 110),
               shinyBS::bsPopover('custom_cov_1_mean_model_1', title = 'Mean', content = bspop_cov_mean, trigger = 'hover', placement = 'right')
        ),
        column(width = 6,
               numericInput('custom_cov_1_sd_model_1', 'Standard Deviation', value = 20, min = 0),
               shinyBS::bsPopover('custom_cov_1_sd_model_1', title = 'Standard Deviation', content = bspop_cov_sd, trigger = 'hover', placement = 'right')
        )
      )
    } else if (input$custom_cov_1_dist_model_1 == "Log-Normal") { # end of Normal dist conditional
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_1_meanlog_model_1', 'Mean (Normal Scale)', value = 110, min = 0),
               shinyBS::bsPopover('custom_cov_1_meanlog_model_1', title = 'Mean (Normal Scale)', content = bspop_cov_meanlog, trigger = 'hover', placement = 'right')
        ),
        column(width = 6,
               numericInput('custom_cov_1_sdlog_model_1', 'SD (Normal Scale)', value = 1.3, min = 1, step = 0.05),
               shinyBS::bsPopover('custom_cov_1_sdlog_model_1', title = 'SD (Normal Scale)', content = bspop_cov_sdlog, trigger = 'hover', placement = 'right')
        )
      )
    } else if (input$custom_cov_1_dist_model_1 == "Uniform") {
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_1_min_model_1', 'Minimum Value', value = 30)),
        column(width = 6,
               numericInput('custom_cov_1_max_model_1', 'Maximum Value', value = 60))
      )
    } else if (input$custom_cov_1_dist_model_1 == "Binary Categorical") { # end of Uniform dist conditional
      fluidRow(
        column(width = 6,
               sliderInput('custom_cov_1_catprop_model_1', label = 'Percentage of Subjects in Category 1', min = 0, max = 100, value = 50, step = 5)),
        column(width = 3,
               numericInput('custom_cov_1_catvalue1_model_1', 'Category 1 Value',  value = 1),
               shinyBS::bsPopover('custom_cov_1_catvalue1_model_1', title = 'Category 1 Value', content = bspop_catvalue1, trigger = 'hover', placement = 'right')),
        column(width = 3,
               numericInput('custom_cov_1_catvalue2_model_1', 'Category 2 Value',  value = 0),
               shinyBS::bsPopover('custom_cov_1_catvalue2_model_1', title = 'Category 2 Value', content = bspop_catvalue1, trigger = 'hover', placement = 'right'))
      )
    }
  }) # end of custom_cov_1_ui_model_1 renderUI

  cleaned_cov_1_name_model_1 <- reactive({
    check_cov_name(orig_name = input$custom_cov_1_model_1, replaced_name = "COV1")
  }) %>% bindEvent(input$custom_cov_1_model_1)

  observe({
    if(!is.null(cleaned_cov_1_name_model_1()) & cleaned_cov_1_name_model_1() != "") {
      if (input$custom_cov_1_dist_model_1 == "Normal") {
        shiny::req(input$custom_cov_1_mean_model_1)
        shiny::req(input$custom_cov_1_sd_model_1)
        set.seed(input$seed_number_model_1 + 10000) # Seed is perturbed for each covariate(s)
        cov_1_model_1_df <- rnorm(n = n_subj_model_1_clean(),
                                  mean = input$custom_cov_1_mean_model_1,
                                  sd = input$custom_cov_1_sd_model_1) %>% round(digits = 2)
      }
      if (input$custom_cov_1_dist_model_1 == "Log-Normal") {
        shiny::req(input$custom_cov_1_meanlog_model_1)
        shiny::req(input$custom_cov_1_sdlog_model_1)
        set.seed(input$seed_number_model_1 + 10000) # Seed is perturbed for each covariate(s)
        cov_1_model_1_df <- rlnorm(n = n_subj_model_1_clean(),
                                   meanlog = log(input$custom_cov_1_meanlog_model_1),
                                   sdlog = log(input$custom_cov_1_sdlog_model_1)) %>% round(digits = 2)
      }
      if (input$custom_cov_1_dist_model_1 == "Uniform") { # without rounding does something silly
        shiny::req(input$custom_cov_1_min_model_1)
        shiny::req(input$custom_cov_1_max_model_1)
        set.seed(input$seed_number_model_1 + 10000) # Seed is perturbed for each covariate(s)
        cov_1_model_1_df <- runif(n  = n_subj_model_1_clean(),
                                  min = input$custom_cov_1_min_model_1,
                                  max = input$custom_cov_1_max_model_1) %>% round(digits = 2)
      }
      if (input$custom_cov_1_dist_model_1 == "Binary Categorical") {
        shiny::req(input$custom_cov_1_catprop_model_1)
        shiny::req(input$custom_cov_1_catvalue1_model_1)
        shiny::req(input$custom_cov_1_catvalue2_model_1)
        set.seed(input$seed_number_model_1 + 10000) # Seed is perturbed for each covariate(s)
        cov_1_model_1_df <- binary_cat_dist(
          n = n_subj_model_1_clean(),
          percent = input$custom_cov_1_catprop_model_1,
          catvalue1 = input$custom_cov_1_catvalue1_model_1,
          catvalue2 = input$custom_cov_1_catvalue2_model_1) %>% round(digits = 2)
      }

      cov_1_model_1_df <- dplyr::as_tibble(cov_1_model_1_df)
      names(cov_1_model_1_df) <- cleaned_cov_1_name_model_1()

      # Update rv_cov_1_model_1$df
      rv_cov_1_model_1$df <- dplyr::bind_cols(cov_1_model_1_df, dplyr::tibble(ID = seq_len(n_subj_model_1_clean())))

    } else {
      rv_cov_1_model_1$df <- dplyr::tibble(ID = seq_len(n_subj_model_1_clean()))
    }
  }) # end of big observe to update rv_cov_1_model_1

  output$cov_1_plot_ui_model_1 <- renderUI({
    if(!is.null(rv_cov_1_model_1$df)) {
      if(ncol(rv_cov_1_model_1$df) > 1) {
        if(names(rv_cov_1_model_1$df[1]) != "") {
          column(width = 12,
                 plotOutput("cov_1_plot_model_1", height = "300px") %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
          )
        }
      }
    }
  })

  output$cov_1_plot_model_1 <- renderPlot({
    print_cov_plot(rv_cov_1_model_1$df) # %>% convert_to_plotly_watermark(plotly_watermark = FALSE)
  })

  ### Repeat for Covariate 2 Model 1

  output$custom_cov_2_ui_model_1 <- renderUI({
    if (input$custom_cov_2_dist_model_1 == "Normal") {
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_2_mean_model_1', 'Mean', value = 110),
               shinyBS::bsPopover('custom_cov_2_mean_model_1', title = 'Mean', content = bspop_cov_mean, trigger = 'hover', placement = 'right')
        ),
        column(width = 6,
               numericInput('custom_cov_2_sd_model_1', 'Standard Deviation', value = 20, min = 0),
               shinyBS::bsPopover('custom_cov_2_sd_model_1', title = 'Standard Deviation', content = bspop_cov_sd, trigger = 'hover', placement = 'right')
        )
      )
    } else if (input$custom_cov_2_dist_model_1 == "Log-Normal") { # end of Normal dist conditional
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_2_meanlog_model_1', 'Mean (Normal Scale)', value = 110, min = 0),
               shinyBS::bsPopover('custom_cov_2_meanlog_model_1', title = 'Mean (Normal Scale)', content = bspop_cov_meanlog, trigger = 'hover', placement = 'right')
        ),
        column(width = 6,
               numericInput('custom_cov_2_sdlog_model_1', 'SD (Normal Scale)', value = 1.3, min = 1, step = 0.05),
               shinyBS::bsPopover('custom_cov_2_sdlog_model_1', title = 'SD (Normal Scale)', content = bspop_cov_sdlog, trigger = 'hover', placement = 'right')
        )
      )
    } else if (input$custom_cov_2_dist_model_1 == "Uniform") {
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_2_min_model_1', 'Minimum Value', value = 30)),
        column(width = 6,
               numericInput('custom_cov_2_max_model_1', 'Maximum Value', value = 60))
      )
    } else if (input$custom_cov_2_dist_model_1 == "Binary Categorical") { # end of Uniform dist conditional
      fluidRow(
        column(width = 6,
               sliderInput('custom_cov_2_catprop_model_1', label = 'Percentage of Subjects in Category 1', min = 0, max = 100, value = 50, step = 5)),
        column(width = 3,
               numericInput('custom_cov_2_catvalue1_model_1', 'Category 1 Value',  value = 1),
               shinyBS::bsPopover('custom_cov_2_catvalue1_model_1', title = 'Category 1 Value', content = bspop_catvalue1, trigger = 'hover', placement = 'right')),
        column(width = 3,
               numericInput('custom_cov_2_catvalue2_model_1', 'Category 2 Value',  value = 0),
               shinyBS::bsPopover('custom_cov_2_catvalue2_model_1', title = 'Category 2 Value', content = bspop_catvalue2, trigger = 'hover', placement = 'right'))
      )
    }
  }) # end of custom_cov_2_ui_model_1 renderUI

  cleaned_cov_2_name_model_1 <- reactive({
    check_cov_name(orig_name = input$custom_cov_2_model_1, replaced_name = "COV2")
  }) %>% bindEvent(input$custom_cov_2_model_1)

  observe({
    if(!is.null(cleaned_cov_2_name_model_1()) & cleaned_cov_2_name_model_1() != "") {
      if (input$custom_cov_2_dist_model_1 == "Normal") {
        shiny::req(input$custom_cov_2_mean_model_1)
        shiny::req(input$custom_cov_2_sd_model_1)
        set.seed(input$seed_number_model_1 + 20000) # Seed is perturbed for each covariate(s)
        cov_2_model_1_df <- rnorm(n = n_subj_model_1_clean(),
                                  mean = input$custom_cov_2_mean_model_1,
                                  sd = input$custom_cov_2_sd_model_1) %>% round(digits = 2)
      }
      if (input$custom_cov_2_dist_model_1 == "Log-Normal") {
        shiny::req(input$custom_cov_2_meanlog_model_1)
        shiny::req(input$custom_cov_2_sdlog_model_1)
        set.seed(input$seed_number_model_1 + 20000) # Seed is perturbed for each covariate(s)
        cov_2_model_1_df <- rlnorm(n = n_subj_model_1_clean(),
                                   meanlog = log(input$custom_cov_2_meanlog_model_1),
                                   sdlog = log(input$custom_cov_2_sdlog_model_1)) %>% round(digits = 2)
      }
      if (input$custom_cov_2_dist_model_1 == "Uniform") { # without rounding does something silly
        shiny::req(input$custom_cov_2_min_model_1)
        shiny::req(input$custom_cov_2_max_model_1)
        set.seed(input$seed_number_model_1 + 20000) # Seed is perturbed for each covariate(s)
        cov_2_model_1_df <- runif(n  = n_subj_model_1_clean(),
                                  min = input$custom_cov_2_min_model_1,
                                  max = input$custom_cov_2_max_model_1) %>% round(digits = 2)
      }
      if (input$custom_cov_2_dist_model_1 == "Binary Categorical") {
        shiny::req(input$custom_cov_2_catprop_model_1)
        shiny::req(input$custom_cov_2_catvalue1_model_1)
        shiny::req(input$custom_cov_2_catvalue2_model_1)
        set.seed(input$seed_number_model_1 + 20000) # Seed is perturbed for each covariate(s)
        cov_2_model_1_df <- binary_cat_dist(
          n = n_subj_model_1_clean(),
          percent = input$custom_cov_2_catprop_model_1,
          catvalue1 = input$custom_cov_2_catvalue1_model_1,
          catvalue2 = input$custom_cov_2_catvalue2_model_1) %>% round(digits = 2)
      }

      cov_2_model_1_df <- dplyr::as_tibble(cov_2_model_1_df)
      names(cov_2_model_1_df) <- cleaned_cov_2_name_model_1()

      # Update rv_cov_2_model_1$df
      rv_cov_2_model_1$df <- dplyr::bind_cols(cov_2_model_1_df, dplyr::tibble(ID = seq_len(n_subj_model_1_clean())))

    } else {
      rv_cov_2_model_1$df <- dplyr::tibble(ID = seq_len(n_subj_model_1_clean()))
    }
  }) # end of big observe to update rv_cov_2_model_1

  output$cov_2_plot_ui_model_1 <- renderUI({
    if(!is.null(rv_cov_2_model_1$df)) {
      if(ncol(rv_cov_2_model_1$df) > 1) {
        if(names(rv_cov_2_model_1$df[1]) != "") {
          column(width = 12,
                 plotOutput("cov_2_plot_model_1", height = "300px") %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
          )
        }
      }
    }
  })

  output$cov_2_plot_model_1 <- renderPlot({
    print_cov_plot(rv_cov_2_model_1$df) # %>% convert_to_plotly_watermark(plotly_watermark = FALSE)
  })

  ### Repeat for Covariate 3 Model 1

  output$custom_cov_3_ui_model_1 <- renderUI({
    if (input$custom_cov_3_dist_model_1 == "Normal") {
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_3_mean_model_1', 'Mean', value = 110),
               shinyBS::bsPopover('custom_cov_3_mean_model_1', title = 'Mean', content = bspop_cov_mean, trigger = 'hover', placement = 'right')
        ),
        column(width = 6,
               numericInput('custom_cov_3_sd_model_1', 'Standard Deviation', value = 20, min = 0),
               shinyBS::bsPopover('custom_cov_3_sd_model_1', title = 'Standard Deviation', content = bspop_cov_sd, trigger = 'hover', placement = 'right')
        )
      )
    } else if (input$custom_cov_3_dist_model_1 == "Log-Normal") { # end of Normal dist conditional
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_3_meanlog_model_1', 'Mean (Normal Scale)', value = 110, min = 0),
               shinyBS::bsPopover('custom_cov_3_meanlog_model_1', title = 'Mean (Normal Scale)', content = bspop_cov_meanlog, trigger = 'hover', placement = 'right')
        ),
        column(width = 6,
               numericInput('custom_cov_3_sdlog_model_1', 'SD (Normal Scale)', value = 1.3, min = 1, step = 0.05),
               shinyBS::bsPopover('custom_cov_3_sdlog_model_1', title = 'SD (Normal Scale)', content = bspop_cov_sdlog, trigger = 'hover', placement = 'right')
        )
      )
    } else if (input$custom_cov_3_dist_model_1 == "Uniform") {
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_3_min_model_1', 'Minimum Value', value = 30)),
        column(width = 6,
               numericInput('custom_cov_3_max_model_1', 'Maximum Value', value = 60))
      )
    } else if (input$custom_cov_3_dist_model_1 == "Binary Categorical") { # end of Uniform dist conditional
      fluidRow(
        column(width = 6,
               sliderInput('custom_cov_3_catprop_model_1', label = 'Percentage of Subjects in Category 1', min = 0, max = 100, value = 50, step = 5)),
        column(width = 3,
               numericInput('custom_cov_3_catvalue1_model_1', 'Category 1 Value',  value = 1),
               shinyBS::bsPopover('custom_cov_3_catvalue1_model_1', title = 'Category 1 Value', content = bspop_catvalue1, trigger = 'hover', placement = 'right')),
        column(width = 3,
               numericInput('custom_cov_3_catvalue2_model_1', 'Category 2 Value',  value = 0),
               shinyBS::bsPopover('custom_cov_3_catvalue2_model_1', title = 'Category 2 Value', content = bspop_catvalue2, trigger = 'hover', placement = 'right'))
      )
    }
  }) # end of custom_cov_3_ui_model_1 renderUI

  cleaned_cov_3_name_model_1 <- reactive({
    check_cov_name(orig_name = input$custom_cov_3_model_1, replaced_name = "COV3")
  }) %>% bindEvent(input$custom_cov_3_model_1)

  observe({
    if(!is.null(cleaned_cov_3_name_model_1()) & cleaned_cov_3_name_model_1() != "") {
      if (input$custom_cov_3_dist_model_1 == "Normal") {
        shiny::req(input$custom_cov_3_mean_model_1)
        shiny::req(input$custom_cov_3_sd_model_1)
        set.seed(input$seed_number_model_1 + 30000) # Seed is perturbed for each covariate(s)
        cov_3_model_1_df <- rnorm(n = n_subj_model_1_clean(),
                                  mean = input$custom_cov_3_mean_model_1,
                                  sd = input$custom_cov_3_sd_model_1) %>% round(digits = 2)
      }
      if (input$custom_cov_3_dist_model_1 == "Log-Normal") {
        shiny::req(input$custom_cov_3_meanlog_model_1)
        shiny::req(input$custom_cov_3_sdlog_model_1)
        set.seed(input$seed_number_model_1 + 30000) # Seed is perturbed for each covariate(s)
        cov_3_model_1_df <- rlnorm(n = n_subj_model_1_clean(),
                                   meanlog = log(input$custom_cov_3_meanlog_model_1),
                                   sdlog = log(input$custom_cov_3_sdlog_model_1)) %>% round(digits = 2)
      }
      if (input$custom_cov_3_dist_model_1 == "Uniform") { # without rounding does something silly
        shiny::req(input$custom_cov_3_min_model_1)
        shiny::req(input$custom_cov_3_max_model_1)
        set.seed(input$seed_number_model_1 + 30000) # Seed is perturbed for each covariate(s)
        cov_3_model_1_df <- runif(n  = n_subj_model_1_clean(),
                                  min = input$custom_cov_3_min_model_1,
                                  max = input$custom_cov_3_max_model_1) %>% round(digits = 2)
      }
      if (input$custom_cov_3_dist_model_1 == "Binary Categorical") {
        shiny::req(input$custom_cov_3_catprop_model_1)
        shiny::req(input$custom_cov_3_catvalue1_model_1)
        shiny::req(input$custom_cov_3_catvalue2_model_1)
        set.seed(input$seed_number_model_1 + 30000) # Seed is perturbed for each covariate(s)
        cov_3_model_1_df <- binary_cat_dist(
          n = n_subj_model_1_clean(),
          percent = input$custom_cov_3_catprop_model_1,
          catvalue1 = input$custom_cov_3_catvalue1_model_1,
          catvalue2 = input$custom_cov_3_catvalue2_model_1) %>% round(digits = 2)
      }

      cov_3_model_1_df <- dplyr::as_tibble(cov_3_model_1_df)
      names(cov_3_model_1_df) <- cleaned_cov_3_name_model_1()

      # Update rv_cov_3_model_1$df
      rv_cov_3_model_1$df <- dplyr::bind_cols(cov_3_model_1_df, dplyr::tibble(ID = seq_len(n_subj_model_1_clean())))

    } else {
      rv_cov_3_model_1$df <- dplyr::tibble(ID = seq_len(n_subj_model_1_clean()))
    }
  }) # end of big observe to update rv_cov_3_model_1

  output$cov_3_plot_ui_model_1 <- renderUI({
    if(!is.null(rv_cov_3_model_1$df)) {
      if(ncol(rv_cov_3_model_1$df) > 1) {
        if(names(rv_cov_3_model_1$df[1]) != "") {
          column(width = 12,
                 plotOutput("cov_3_plot_model_1", height = "300px") %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
          )
        }
      }
    }
  })

  output$cov_3_plot_model_1 <- renderPlot({
    print_cov_plot(rv_cov_3_model_1$df) # %>% convert_to_plotly_watermark(plotly_watermark = FALSE)
  })

  database_model_1 <- reactive({
    dbm1 <- sample_age_wt(df_name     = input$db_model_1,
                          nsubj       = n_subj_model_1_clean(),
                          lower.agemo = input$age_db_model_1[1] * 12,
                          upper.agemo = input$age_db_model_1[2] * 12,
                          lower.wt    = input$wt_db_model_1[1],
                          upper.wt    = input$wt_db_model_1[2],
                          prop.male   = input$males_db_model_1/100, # convert % into proportion (0 - 1)
                          seed.number = input$seed_number_model_1
    )

    # Get the column names of all dataframes, excluding the common column ("ID")
    column_names_model_1 <- c(setdiff(names(rv_cov_1_model_1$df), "ID"),
                              setdiff(names(rv_cov_2_model_1$df), "ID"),
                              setdiff(names(rv_cov_3_model_1$df), "ID"))

    # Check if all column names are unique
    if (length(unique(column_names_model_1)) == length(column_names_model_1)) {
      dbm1_cov <- dbm1 %>%
        dplyr::left_join(rv_cov_1_model_1$df, by = "ID") %>%
        dplyr::left_join(rv_cov_2_model_1$df, by = "ID") %>%
        dplyr::left_join(rv_cov_3_model_1$df, by = "ID")
    } else {
      shiny::showNotification("ERROR: All custom covariate names must be different from each other. Please rename them first.", type = "error", duration = 10)
      dbm1_cov <- dbm1
    }

    return(dbm1_cov) # using bindEvent to prevent sampling pre-maturely based on outdated age ranges and throws an error
  }) #%>% bindEvent(input$db_model_1, n_subj_model_1_clean(), input$age_db_model_1, input$wt_db_model_1, input$males_db_model_1,
  #              input$seed_number_model_1, rv_cov_1_model_1$df, rv_cov_2_model_1$df, rv_cov_3_model_1$df)

  output$demog_info_model_1 <- renderUI({
    if(input$db_model_1 == "None" & all(c(input$custom_cov_1_model_1, input$custom_cov_2_model_1, input$custom_cov_3_model_1) == "")) {
      htmltools::HTML(paste("<br><strong style='color: red;'><p>", "Summary statistics is not available when no database or custom covariates are selected.","</strong></p>"))
    } else {
      db_summ_model_1 <- database_model_1() %>%
        calc_summary_stats()

      if("SEX" %in% names(database_model_1())) {
        db_summ_model_1_male <- database_model_1() %>% dplyr::filter(SEX == 0) %>%
          calc_summary_stats()

        db_summ_model_1_female <- database_model_1() %>% dplyr::filter(SEX == 1) %>%
          calc_summary_stats()
      } else {
        db_summ_model_1_male   <- db_summ_model_1
        db_summ_model_1_female <- db_summ_model_1
      }

      if(input$db_model_1 != "None") {
        db_summ_model_1 <- db_summ_model_1 %>%
          dplyr::rename(
            `AGE (y)`  = AGE,
            `WT (kg)`  = WT,
            `HT (cm)`  = HT,
            `BSA (m²)` = BSA
          )
        db_summ_model_1_male <- db_summ_model_1_male %>%
          dplyr::rename(
            `AGE (y)`  = AGE,
            `WT (kg)`  = WT,
            `HT (cm)`  = HT,
            `BSA (m²)` = BSA
          )
        db_summ_model_1_female <- db_summ_model_1_female %>%
          dplyr::rename(
            `AGE (y)`  = AGE,
            `WT (kg)`  = WT,
            `HT (cm)`  = HT,
            `BSA (m²)` = BSA
          )
      }
      db_summ_model_1 <- db_summ_model_1 %>%
        flextable::flextable() %>%
        flextable::font(font = "Arial") %>%
        flextable::bold(part = "header") %>%
        flextable::add_header_lines(paste0("All Subjects [Database: ", input$db_model_1, "] (n = ", nrow(database_model_1() %>% dplyr::distinct(ID)),")")) %>%
        flextable::bold(i = 4) %>%
        flextable::autofit() %>%
        flextable::theme_zebra() %>%
        flextable::htmltools_value()

      if("SEX" %in% names(database_model_1())) {
        db_summ_model_1_male <- db_summ_model_1_male %>%
          flextable::flextable() %>%
          flextable::font(font = "Arial") %>%
          flextable::bold(part = "header") %>%
          flextable::add_header_lines(paste0("Males (n = ", nrow(database_model_1() %>% dplyr::filter(SEX == 0) %>% dplyr::distinct(ID)),")")) %>%
          flextable::bold(i = 4) %>%
          flextable::autofit() %>%
          flextable::theme_zebra() %>%
          flextable::htmltools_value()

        db_summ_model_1_female <- db_summ_model_1_female %>%
          flextable::flextable() %>%
          flextable::font(font = "Arial") %>%
          flextable::bold(part = "header") %>%
          flextable::add_header_lines(paste0("Females (n = ", nrow(database_model_1() %>% dplyr::filter(SEX == 1) %>% dplyr::distinct(ID)),")")) %>%
          flextable::bold(i = 4) %>%
          flextable::autofit() %>%
          flextable::theme_zebra() %>%
          flextable::htmltools_value()
      } else {
        db_summ_model_1_male   <- db_summ_model_1
        db_summ_model_1_female <- db_summ_model_1
      }

      message("Created db_summ_model_1 flextables")

      if(input$males_db_model_1 == 0 | input$males_db_model_1 == 100 | input$db_model_1 == "None") {
        fluidRow(db_summ_model_1)
      } else {
        fluidRow(db_summ_model_1, htmltools::br(), db_summ_model_1_male, htmltools::br(), db_summ_model_1_female)
      }
    }
  })

  demog_plot_model_1 <- reactive({
    print_demog_plots(database_model_1())
  })

  output$demog_plots_model_1 <- renderUI({
    if(input$db_model_1 == "None") {
      htmltools::HTML(paste("<br><strong style='color: red;'><p>", "Plots of weight and sex are not available when no database is selected.","</strong></p>"))
    } else {
      renderPlot(demog_plot_model_1())
    }
  })

  #### Demog download section
  output$download_demog_data_model_1 <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_demog_data_model_1.csv")
    },
    content = function(file) {
      #write.csv(database_model_1(), file, row.names = FALSE)
      data.table::fwrite(database_model_1(), file, quote = FALSE, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  output$download_demog_plot_model_1 <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_demographics_plot_model_1.pdf")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = demog_plot_model_1(), device = "pdf", width = 16, height = 8)
    }
  )

  observeEvent(changed_reacted_param_model_1(), {
    if (!shinyAce::is.empty(mrgsolve::as.matrix(mrgsolve::omat(changed_reacted_param_model_1())))) {
      tmp <- extract_matrix(changed_reacted_param_model_1(), name_of_matrix = "omega", debug = show_debugging_msg)
      extracted_omega_model_1(tmp)
      iiv_checkpoint_model_1$extract_model_omega <- TRUE
    }

    if (!shinyAce::is.empty(mrgsolve::as.matrix(mrgsolve::smat(changed_reacted_param_model_1())))) {
      tmp1 <- extract_matrix(changed_reacted_param_model_1(), name_of_matrix = "sigma", debug = show_debugging_msg)
      extracted_sigma_model_1(tmp1)
      iiv_checkpoint_model_1$extract_model_sigma <- TRUE
    }
  })

  omega_matrix_model_1 <- eventReactive(extracted_omega_model_1(), {
    rhandsontable::rhandsontable(extracted_omega_model_1(), colTypes = rep("text", ncol(extracted_omega_model_1())), contextMenu = FALSE) %>%
      rhandsontable::hot_cols(renderer = "
             function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               if (value === null || value === 'NA') {
                 td.style.background = '#D3D3D3';  // Grey color
                 cellProperties.readOnly = true;  // Make cell read-only
               }
             }")
  })

  output$omega_model_1 <- rhandsontable::renderRHandsontable({
    omega_matrix_model_1()
  })

  sigma_matrix_model_1 <- eventReactive(extracted_sigma_model_1(), {
    rhandsontable::rhandsontable(extracted_sigma_model_1(), colTypes = rep("text", ncol(extracted_sigma_model_1())), contextMenu = FALSE) %>%
      rhandsontable::hot_cols(renderer = "
           function(instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.TextRenderer.apply(this, arguments);
             if (value === null || value === 'NA') {
               td.style.background = '#D3D3D3';  // Grey color
               cellProperties.readOnly = true;  // Make cell read-only
             }
           }")
  })

  output$sigma_model_1 <- rhandsontable::renderRHandsontable({
    sigma_matrix_model_1()
  })

  observeEvent(input$iiv_action_model_1, {
    if (iiv_checkpoint_model_1$extract_model_omega && !shinyAce::is.empty(input$omega_model_1)) {
      if(show_debugging_msg) {
        message('omega input fulfilled')
      }
      tmp1 <- rhandsontable::hot_to_r(input$omega_model_1)
      extracted_omega_model_1(tmp1)
      iiv_checkpoint_model_1$updated_matrix_omega <- TRUE
    }

    if (iiv_checkpoint_model_1$extract_model_sigma && !shinyAce::is.empty(input$sigma_model_1)) {
      if(show_debugging_msg) {
        message('sigma input fulfilled')
      }
      tmp2 <- rhandsontable::hot_to_r(input$sigma_model_1)
      extracted_sigma_model_1(tmp2)
      iiv_checkpoint_model_1$updated_matrix_sigma <- TRUE
    }
    if(show_debugging_msg) {
      message('matrix applied')
    }
  })

  changed_matrix_model_1 <- reactiveVal()

  observe({
    variability_object_model_1 <- changed_reacted_param_model_1()

    if (iiv_checkpoint_model_1$updated_matrix_omega) {
      if(show_debugging_msg) {
        message('updating matrix omega')
      }
      updated_omega_model_1 <- reconstruct_matrices(variability_object_model_1,
                                                    extracted_omega_model_1(),
                                                    name_of_matrix = "omega",
                                                    debug = show_debugging_msg)
      variability_object_model_1 <- update_variability(variability_object_model_1,
                                                       updated_omega_model_1,
                                                       name_of_matrix = "omega",
                                                       debug = show_debugging_msg)
      iiv_checkpoint_model_1$reconstructed_iiv <- TRUE
      if(show_debugging_msg) {
        message('updated complete matrix omega')
      }
    }

    if (iiv_checkpoint_model_1$updated_matrix_sigma) {
      if(show_debugging_msg) {
        message('updating matrix sigma')
      }
      updated_sigma_model_1 <- reconstruct_matrices(variability_object_model_1,
                                                    extracted_sigma_model_1(),
                                                    name_of_matrix = "sigma",
                                                    debug = show_debugging_msg)
      variability_object_model_1 <- update_variability(variability_object_model_1,
                                                       updated_sigma_model_1,
                                                       name_of_matrix = "sigma",
                                                       debug = show_debugging_msg)
      iiv_checkpoint_model_1$reconstructed_iiv <- TRUE
      if(show_debugging_msg) {
        message('updated complete matrix sigma')
      }
    }
    changed_matrix_model_1(variability_object_model_1)
  })

  output$console_output_iiv_model_1 <- renderPrint({
    shiny::req(changed_matrix_model_1())
    if(mrgsolve::is.mrgmod(changed_matrix_model_1())) {
      cat(matrix_info_message)
      cat("\n\nCurrent Model Matrices:\n\n")
      print(mrgsolve::revar(changed_matrix_model_1()))
    }
  })

  ## IIV simulation 1 ----
  simulation_IIV_output_model_1 <- reactive({
    if (iiv_checkpoint_model_1$reconstructed_iiv) {

      iiv_sim_output_model_1 <-
        run_single_sim(
          input_model_object = changed_matrix_model_1(),
          pred_model         = model_1_is_pred(),
          ev_df              = dosing_regimen_model_1(),
          model_dur          = model_duration_argument_model_1(),
          model_rate         = model_rate_argument_model_1(),
          sampling_times     = sampling_options(),
          seed               = input$seed_number_model_1,
          debug              = show_debugging_msg,
          divide_by          = time_value(),
          nsubj              = n_subj_model_1_clean(),
          ext_db             = database_model_1(),
          parallel_sim       = FALSE, #input$para_checkbox,
          parallel_n         = 100 # input$para_n,
        )

      iiv_sim_output_model_1 <- quantile_output(iiv_sim_output_model_1,
                                                yvar = input$yaxis_name,
                                                lower_quartile = sanitize_numeric_input(input$lower_quartile, legal_minimum = 0, display_error = TRUE)/100,
                                                upper_quartile = sanitize_numeric_input(input$upper_quartile, legal_maximum = 100, display_error = TRUE)/100
      )

      iiv_sim_output_model_1$yvar <- iiv_sim_output_model_1[[input$yaxis_name]]

      if(is.data.frame(iiv_sim_output_model_1)) {
        iiv_checkpoint_model_1$iiv_simulation <- TRUE
        if (show_debugging_msg) {
          message('simulation1 iiv generated')
        }
        return(iiv_sim_output_model_1)
      }
    }
  }, label = 'iiv_output_model_1()')


  combine_iiv_download_data <- reactive({
    downloadable_df <- check_and_combine_df(model_1_is_valid = iiv_checkpoint_model_1$iiv_simulation,
                                            model_2_is_valid = iiv_checkpoint_model_2$iiv_simulation,
                                            input_df_1 = simulation_IIV_output_model_1(),
                                            input_df_2 = simulation_IIV_output_model_2())
    return(downloadable_df)
  })


  ## Download variability MODEL 1 table ----
  output$download_variability_table <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_variability_data.csv")
    },
    content = function(file) {
      #write.csv(combine_iiv_download_data(), file, quote = FALSE, row.names = FALSE)
      data.table::fwrite(combine_iiv_download_data(), file, quote = FALSE, row.names = FALSE)
    }
  )

  simtime_text_model_1 <- reactive({
    tmp <- x1
    return(tmp)
  })

  output$simtime_model_1 <- renderUI({
    htmltools::HTML(paste("<b>", simtime_text_model_1()))
  })

  ## MODEL 2 ----
  extracted_omega_model_2 <- reactiveVal()
  extracted_sigma_model_2 <- reactiveVal()

  n_subj_model_2_clean <- reactive({
    if(sanitize_numeric_input(input$n_subj_model_2, allow_zero = FALSE, as_integer = TRUE) > 5000) {
      general_warning_modal(title = "Error", text_description = "Maximum number of subjects cannot exceed 5000. Reverting to using 20.")
      updateNumericInput(session, "n_subj_model_2", value = 20)
      return(20)
    } else {
      return(sanitize_numeric_input(input$n_subj_model_2, allow_zero = FALSE, as_integer = TRUE))
    }
  })

  # Create a reactive value to hold the dataframe
  rv_cov_1_model_2 <- reactiveValues(df = NULL)
  rv_cov_2_model_2 <- reactiveValues(df = NULL)
  rv_cov_3_model_2 <- reactiveValues(df = NULL)

  # Initialize the dataframe
  observe({
    rv_cov_1_model_2$df <- dplyr::tibble(ID = seq_len(n_subj_model_2_clean()))
    rv_cov_2_model_2$df <- dplyr::tibble(ID = seq_len(n_subj_model_2_clean()))
    rv_cov_3_model_2$df <- dplyr::tibble(ID = seq_len(n_subj_model_2_clean()))
  })

  observeEvent(input$db_model_2, {
    if (input$db_model_2 == "None") {
      shinyjs::disable("age_db_model_2")
      shinyjs::disable("wt_db_model_2")
      shinyjs::disable("males_db_model_2")
    }
    if (input$db_model_2 == "NHANES") {
      shinyjs::enable("age_db_model_2")
      shinyjs::enable("wt_db_model_2")
      shinyjs::enable("males_db_model_2")
      updateSliderInput(session, "age_db_model_2", min = 0, max = 100, value = c(18, 65))
    }
    if (input$db_model_2 == "CDC") {
      shinyjs::enable("age_db_model_2")
      shinyjs::enable("wt_db_model_2")
      shinyjs::enable("males_db_model_2")
      updateSliderInput(session, "age_db_model_2", min = 0, max = 20,  value = c(6, 18), step = 0.5)
    }
    if (input$db_model_2 == "WHO") {
      shinyjs::enable("age_db_model_2")
      shinyjs::enable("wt_db_model_2")
      shinyjs::enable("males_db_model_2")
      updateSliderInput(session, "age_db_model_2", min = 0, max = 10,  value = c(2, 6), step = 0.25)
    }
  }, label = "update_db_slider_model_2")

  ##### UI / Plotting elements for Covariate 1 Model 2

  output$custom_cov_1_ui_model_2 <- renderUI({
    if (input$custom_cov_1_dist_model_2 == "Normal") {
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_1_mean_model_2', 'Mean', value = 110),
               shinyBS::bsPopover('custom_cov_1_mean_model_2', title = 'Mean', content = bspop_cov_mean, trigger = 'hover', placement = 'right')
        ),
        column(width = 6,
               numericInput('custom_cov_1_sd_model_2', 'Standard Deviation', value = 20, min = 0),
               shinyBS::bsPopover('custom_cov_1_sd_model_2', title = 'Standard Deviation', content = bspop_cov_sd, trigger = 'hover', placement = 'right')
        )
      )
    } else if (input$custom_cov_1_dist_model_2 == "Log-Normal") { # end of Normal dist conditional
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_1_meanlog_model_2', 'Mean (Normal Scale)', value = 110, min = 0),
               shinyBS::bsPopover('custom_cov_1_meanlog_model_2', title = 'Mean (Normal Scale)', content = bspop_cov_meanlog, trigger = 'hover', placement = 'right')
        ),
        column(width = 6,
               numericInput('custom_cov_1_sdlog_model_2', 'SD (Normal Scale)', value = 1.3, min = 1, step = 0.05),
               shinyBS::bsPopover('custom_cov_1_sdlog_model_2', title = 'SD (Normal Scale)', content = bspop_cov_sdlog, trigger = 'hover', placement = 'right')
        )
      )
    } else if (input$custom_cov_1_dist_model_2 == "Uniform") {
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_1_min_model_2', 'Minimum Value', value = 30)),
        column(width = 6,
               numericInput('custom_cov_1_max_model_2', 'Maximum Value', value = 60))
      )
    } else if (input$custom_cov_1_dist_model_2 == "Binary Categorical") { # end of Uniform dist conditional
      fluidRow(
        column(width = 6,
               sliderInput('custom_cov_1_catprop_model_2', label = 'Percentage of Subjects in Category 1', min = 0, max = 100, value = 50, step = 5)),
        column(width = 3,
               numericInput('custom_cov_1_catvalue1_model_2', 'Category 1 Value',  value = 1),
               shinyBS::bsPopover('custom_cov_1_catvalue1_model_2', title = 'Category 1 Value', content = bspop_catvalue1, trigger = 'hover', placement = 'right')),
        column(width = 3,
               numericInput('custom_cov_1_catvalue2_model_2', 'Category 2 Value',  value = 0),
               shinyBS::bsPopover('custom_cov_1_catvalue2_model_2', title = 'Category 2 Value', content = bspop_catvalue1, trigger = 'hover', placement = 'right'))
      )
    }
  }) # end of custom_cov_1_ui_model_2 renderUI

  cleaned_cov_1_name_model_2 <- reactive({
    check_cov_name(orig_name = input$custom_cov_1_model_2, replaced_name = "COV1")
  }) %>% bindEvent(input$custom_cov_1_model_2)

  observe({
    if(!is.null(cleaned_cov_1_name_model_2()) & cleaned_cov_1_name_model_2() != "") {
      if (input$custom_cov_1_dist_model_2 == "Normal") {
        shiny::req(input$custom_cov_1_mean_model_2)
        shiny::req(input$custom_cov_1_sd_model_2)
        set.seed(input$seed_number_model_2 + 10000) # Seed is perturbed for each covariate(s)
        cov_1_model_2_df <- rnorm(n = n_subj_model_2_clean(),
                                  mean = input$custom_cov_1_mean_model_2,
                                  sd = input$custom_cov_1_sd_model_2) %>% round(digits = 2)
      }
      if (input$custom_cov_1_dist_model_2 == "Log-Normal") {
        shiny::req(input$custom_cov_1_meanlog_model_2)
        shiny::req(input$custom_cov_1_sdlog_model_2)
        set.seed(input$seed_number_model_2 + 10000) # Seed is perturbed for each covariate(s)
        cov_1_model_2_df <- rlnorm(n = n_subj_model_2_clean(),
                                   meanlog = log(input$custom_cov_1_meanlog_model_2),
                                   sdlog = log(input$custom_cov_1_sdlog_model_2)) %>% round(digits = 2)
      }
      if (input$custom_cov_1_dist_model_2 == "Uniform") { # without rounding does something silly
        shiny::req(input$custom_cov_1_min_model_2)
        shiny::req(input$custom_cov_1_max_model_2)
        set.seed(input$seed_number_model_2 + 10000) # Seed is perturbed for each covariate(s)
        cov_1_model_2_df <- runif(n  = n_subj_model_2_clean(),
                                  min = input$custom_cov_1_min_model_2,
                                  max = input$custom_cov_1_max_model_2) %>% round(digits = 2)
      }
      if (input$custom_cov_1_dist_model_2 == "Binary Categorical") {
        shiny::req(input$custom_cov_1_catprop_model_2)
        shiny::req(input$custom_cov_1_catvalue1_model_2)
        shiny::req(input$custom_cov_1_catvalue2_model_2)
        set.seed(input$seed_number_model_2 + 10000) # Seed is perturbed for each covariate(s)
        cov_1_model_2_df <- binary_cat_dist(
          n = n_subj_model_2_clean(),
          percent = input$custom_cov_1_catprop_model_2,
          catvalue1 = input$custom_cov_1_catvalue1_model_2,
          catvalue2 = input$custom_cov_1_catvalue2_model_2) %>% round(digits = 2)
      }

      cov_1_model_2_df <- dplyr::as_tibble(cov_1_model_2_df)
      names(cov_1_model_2_df) <- cleaned_cov_1_name_model_2()

      # Update rv_cov_1_model_2$df
      rv_cov_1_model_2$df <- dplyr::bind_cols(cov_1_model_2_df, dplyr::tibble(ID = seq_len(n_subj_model_2_clean())))

    } else {
      rv_cov_1_model_2$df <- dplyr::tibble(ID = seq_len(n_subj_model_2_clean()))
    }
  }) # end of big observe to update rv_cov_1_model_2

  output$cov_1_plot_ui_model_2 <- renderUI({
    if(!is.null(rv_cov_1_model_2$df)) {
      if(ncol(rv_cov_1_model_2$df) > 1) {
        if(names(rv_cov_1_model_2$df[1]) != "") {
          column(width = 12,
                 plotOutput("cov_1_plot_model_2", height = "300px") %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
          )
        }
      }
    }
  })

  output$cov_1_plot_model_2 <- renderPlot({
    print_cov_plot(rv_cov_1_model_2$df) # %>% convert_to_plotly_watermark(plotly_watermark = FALSE)
  })

  ### Repeat for Covariate 2 Model 2

  output$custom_cov_2_ui_model_2 <- renderUI({
    if (input$custom_cov_2_dist_model_2 == "Normal") {
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_2_mean_model_2', 'Mean', value = 110),
               shinyBS::bsPopover('custom_cov_2_mean_model_2', title = 'Mean', content = bspop_cov_mean, trigger = 'hover', placement = 'right')
        ),
        column(width = 6,
               numericInput('custom_cov_2_sd_model_2', 'Standard Deviation', value = 20, min = 0),
               shinyBS::bsPopover('custom_cov_2_sd_model_2', title = 'Standard Deviation', content = bspop_cov_sd, trigger = 'hover', placement = 'right')
        )
      )
    } else if (input$custom_cov_2_dist_model_2 == "Log-Normal") { # end of Normal dist conditional
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_2_meanlog_model_2', 'Mean (Normal Scale)', value = 110, min = 0),
               shinyBS::bsPopover('custom_cov_2_meanlog_model_2', title = 'Mean (Normal Scale)', content = bspop_cov_meanlog, trigger = 'hover', placement = 'right')
        ),
        column(width = 6,
               numericInput('custom_cov_2_sdlog_model_2', 'SD (Normal Scale)', value = 1.3, min = 1, step = 0.05),
               shinyBS::bsPopover('custom_cov_2_sdlog_model_2', title = 'SD (Normal Scale)', content = bspop_cov_sdlog, trigger = 'hover', placement = 'right')
        )
      )
    } else if (input$custom_cov_2_dist_model_2 == "Uniform") {
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_2_min_model_2', 'Minimum Value', value = 30)),
        column(width = 6,
               numericInput('custom_cov_2_max_model_2', 'Maximum Value', value = 60))
      )
    } else if (input$custom_cov_2_dist_model_2 == "Binary Categorical") { # end of Uniform dist conditional
      fluidRow(
        column(width = 6,
               sliderInput('custom_cov_2_catprop_model_2', label = 'Percentage of Subjects in Category 1', min = 0, max = 100, value = 50, step = 5)),
        column(width = 3,
               numericInput('custom_cov_2_catvalue1_model_2', 'Category 1 Value',  value = 1),
               shinyBS::bsPopover('custom_cov_2_catvalue1_model_2', title = 'Category 1 Value', content = bspop_catvalue1, trigger = 'hover', placement = 'right')),
        column(width = 3,
               numericInput('custom_cov_2_catvalue2_model_2', 'Category 2 Value',  value = 0),
               shinyBS::bsPopover('custom_cov_2_catvalue2_model_2', title = 'Category 2 Value', content = bspop_catvalue2, trigger = 'hover', placement = 'right'))
      )
    }
  }) # end of custom_cov_2_ui_model_2 renderUI

  cleaned_cov_2_name_model_2 <- reactive({
    check_cov_name(orig_name = input$custom_cov_2_model_2, replaced_name = "COV2")
  }) %>% bindEvent(input$custom_cov_2_model_2)

  observe({
    if(!is.null(cleaned_cov_2_name_model_2()) & cleaned_cov_2_name_model_2() != "") {
      if (input$custom_cov_2_dist_model_2 == "Normal") {
        shiny::req(input$custom_cov_2_mean_model_2)
        shiny::req(input$custom_cov_2_sd_model_2)
        set.seed(input$seed_number_model_2 + 20000) # Seed is perturbed for each covariate(s)
        cov_2_model_2_df <- rnorm(n = n_subj_model_2_clean(),
                                  mean = input$custom_cov_2_mean_model_2,
                                  sd = input$custom_cov_2_sd_model_2) %>% round(digits = 2)
      }
      if (input$custom_cov_2_dist_model_2 == "Log-Normal") {
        shiny::req(input$custom_cov_2_meanlog_model_2)
        shiny::req(input$custom_cov_2_sdlog_model_2)
        set.seed(input$seed_number_model_2 + 20000) # Seed is perturbed for each covariate(s)
        cov_2_model_2_df <- rlnorm(n = n_subj_model_2_clean(),
                                   meanlog = log(input$custom_cov_2_meanlog_model_2),
                                   sdlog = log(input$custom_cov_2_sdlog_model_2)) %>% round(digits = 2)
      }
      if (input$custom_cov_2_dist_model_2 == "Uniform") { # without rounding does something silly
        shiny::req(input$custom_cov_2_min_model_2)
        shiny::req(input$custom_cov_2_max_model_2)
        set.seed(input$seed_number_model_2 + 20000) # Seed is perturbed for each covariate(s)
        cov_2_model_2_df <- runif(n  = n_subj_model_2_clean(),
                                  min = input$custom_cov_2_min_model_2,
                                  max = input$custom_cov_2_max_model_2) %>% round(digits = 2)
      }
      if (input$custom_cov_2_dist_model_2 == "Binary Categorical") {
        shiny::req(input$custom_cov_2_catprop_model_2)
        shiny::req(input$custom_cov_2_catvalue1_model_2)
        shiny::req(input$custom_cov_2_catvalue2_model_2)
        set.seed(input$seed_number_model_2 + 20000) # Seed is perturbed for each covariate(s)
        cov_2_model_2_df <- binary_cat_dist(
          n = n_subj_model_2_clean(),
          percent = input$custom_cov_2_catprop_model_2,
          catvalue1 = input$custom_cov_2_catvalue1_model_2,
          catvalue2 = input$custom_cov_2_catvalue2_model_2) %>% round(digits = 2)
      }

      cov_2_model_2_df <- dplyr::as_tibble(cov_2_model_2_df)
      names(cov_2_model_2_df) <- cleaned_cov_2_name_model_2()

      # Update rv_cov_2_model_2$df
      rv_cov_2_model_2$df <- dplyr::bind_cols(cov_2_model_2_df, dplyr::tibble(ID = seq_len(n_subj_model_2_clean())))

    } else {
      rv_cov_2_model_2$df <- dplyr::tibble(ID = seq_len(n_subj_model_2_clean()))
    }
  }) # end of big observe to update rv_cov_2_model_2

  output$cov_2_plot_ui_model_2 <- renderUI({
    if(!is.null(rv_cov_2_model_2$df)) {
      if(ncol(rv_cov_2_model_2$df) > 1) {
        if(names(rv_cov_2_model_2$df[1]) != "") {
          column(width = 12,
                 plotOutput("cov_2_plot_model_2", height = "300px") %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
          )
        }
      }
    }
  })

  output$cov_2_plot_model_2 <- renderPlot({
    print_cov_plot(rv_cov_2_model_2$df) # %>% convert_to_plotly_watermark(plotly_watermark = FALSE)
  })

  ### Repeat for Covariate 3 Model 2

  output$custom_cov_3_ui_model_2 <- renderUI({
    if (input$custom_cov_3_dist_model_2 == "Normal") {
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_3_mean_model_2', 'Mean', value = 110),
               shinyBS::bsPopover('custom_cov_3_mean_model_2', title = 'Mean', content = bspop_cov_mean, trigger = 'hover', placement = 'right')
        ),
        column(width = 6,
               numericInput('custom_cov_3_sd_model_2', 'Standard Deviation', value = 20, min = 0),
               shinyBS::bsPopover('custom_cov_3_sd_model_2', title = 'Standard Deviation', content = bspop_cov_sd, trigger = 'hover', placement = 'right')
        )
      )
    } else if (input$custom_cov_3_dist_model_2 == "Log-Normal") { # end of Normal dist conditional
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_3_meanlog_model_2', 'Mean (Normal Scale)', value = 110, min = 0),
               shinyBS::bsPopover('custom_cov_3_meanlog_model_2', title = 'Mean (Normal Scale)', content = bspop_cov_meanlog, trigger = 'hover', placement = 'right')
        ),
        column(width = 6,
               numericInput('custom_cov_3_sdlog_model_2', 'SD (Normal Scale)', value = 1.3, min = 1, step = 0.05),
               shinyBS::bsPopover('custom_cov_3_sdlog_model_2', title = 'SD (Normal Scale)', content = bspop_cov_sdlog, trigger = 'hover', placement = 'right')
        )
      )
    } else if (input$custom_cov_3_dist_model_2 == "Uniform") {
      fluidRow(
        column(width = 6,
               numericInput('custom_cov_3_min_model_2', 'Minimum Value', value = 30)),
        column(width = 6,
               numericInput('custom_cov_3_max_model_2', 'Maximum Value', value = 60))
      )
    } else if (input$custom_cov_3_dist_model_2 == "Binary Categorical") { # end of Uniform dist conditional
      fluidRow(
        column(width = 6,
               sliderInput('custom_cov_3_catprop_model_2', label = 'Percentage of Subjects in Category 1', min = 0, max = 100, value = 50, step = 5)),
        column(width = 3,
               numericInput('custom_cov_3_catvalue1_model_2', 'Category 1 Value',  value = 1),
               shinyBS::bsPopover('custom_cov_3_catvalue1_model_2', title = 'Category 1 Value', content = bspop_catvalue1, trigger = 'hover', placement = 'right')),
        column(width = 3,
               numericInput('custom_cov_3_catvalue2_model_2', 'Category 2 Value',  value = 0),
               shinyBS::bsPopover('custom_cov_3_catvalue2_model_2', title = 'Category 2 Value', content = bspop_catvalue2, trigger = 'hover', placement = 'right'))
      )
    }
  }) # end of custom_cov_3_ui_model_2 renderUI

  cleaned_cov_3_name_model_2 <- reactive({
    check_cov_name(orig_name = input$custom_cov_3_model_2, replaced_name = "COV3")
  }) %>% bindEvent(input$custom_cov_3_model_2)

  observe({
    if(!is.null(cleaned_cov_3_name_model_2()) & cleaned_cov_3_name_model_2() != "") {
      if (input$custom_cov_3_dist_model_2 == "Normal") {
        shiny::req(input$custom_cov_3_mean_model_2)
        shiny::req(input$custom_cov_3_sd_model_2)
        set.seed(input$seed_number_model_2 + 30000) # Seed is perturbed for each covariate(s)
        cov_3_model_2_df <- rnorm(n = n_subj_model_2_clean(),
                                  mean = input$custom_cov_3_mean_model_2,
                                  sd = input$custom_cov_3_sd_model_2) %>% round(digits = 2)
      }
      if (input$custom_cov_3_dist_model_2 == "Log-Normal") {
        shiny::req(input$custom_cov_3_meanlog_model_2)
        shiny::req(input$custom_cov_3_sdlog_model_2)
        set.seed(input$seed_number_model_2 + 30000) # Seed is perturbed for each covariate(s)
        cov_3_model_2_df <- rlnorm(n = n_subj_model_2_clean(),
                                   meanlog = log(input$custom_cov_3_meanlog_model_2),
                                   sdlog = log(input$custom_cov_3_sdlog_model_2)) %>% round(digits = 2)
      }
      if (input$custom_cov_3_dist_model_2 == "Uniform") { # without rounding does something silly
        shiny::req(input$custom_cov_3_min_model_2)
        shiny::req(input$custom_cov_3_max_model_2)
        set.seed(input$seed_number_model_2 + 30000) # Seed is perturbed for each covariate(s)
        cov_3_model_2_df <- runif(n  = n_subj_model_2_clean(),
                                  min = input$custom_cov_3_min_model_2,
                                  max = input$custom_cov_3_max_model_2) %>% round(digits = 2)
      }
      if (input$custom_cov_3_dist_model_2 == "Binary Categorical") {
        shiny::req(input$custom_cov_3_catprop_model_2)
        shiny::req(input$custom_cov_3_catvalue1_model_2)
        shiny::req(input$custom_cov_3_catvalue2_model_2)
        set.seed(input$seed_number_model_2 + 30000) # Seed is perturbed for each covariate(s)
        cov_3_model_2_df <- binary_cat_dist(
          n = n_subj_model_2_clean(),
          percent = input$custom_cov_3_catprop_model_2,
          catvalue1 = input$custom_cov_3_catvalue1_model_2,
          catvalue2 = input$custom_cov_3_catvalue2_model_2) %>% round(digits = 2)
      }

      cov_3_model_2_df <- dplyr::as_tibble(cov_3_model_2_df)
      names(cov_3_model_2_df) <- cleaned_cov_3_name_model_2()

      # Update rv_cov_3_model_2$df
      rv_cov_3_model_2$df <- dplyr::bind_cols(cov_3_model_2_df, dplyr::tibble(ID = seq_len(n_subj_model_2_clean())))

    } else {
      rv_cov_3_model_2$df <- dplyr::tibble(ID = seq_len(n_subj_model_2_clean()))
    }
  }) # end of big observe to update rv_cov_3_model_2

  output$cov_3_plot_ui_model_2 <- renderUI({
    if(!is.null(rv_cov_3_model_2$df)) {
      if(ncol(rv_cov_3_model_2$df) > 1) {
        if(names(rv_cov_3_model_2$df[1]) != "") {
          column(width = 12,
                 plotOutput("cov_3_plot_model_2", height = "300px") %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
          )
        }
      }
    }
  })

  output$cov_3_plot_model_2 <- renderPlot({
    print_cov_plot(rv_cov_3_model_2$df) # %>% convert_to_plotly_watermark(plotly_watermark = FALSE)
  })

  database_model_2 <- reactive({
    dbm2 <- sample_age_wt(df_name     = input$db_model_2,
                          nsubj       = n_subj_model_2_clean(),
                          lower.agemo = input$age_db_model_2[1] * 12,
                          upper.agemo = input$age_db_model_2[2] * 12,
                          lower.wt    = input$wt_db_model_2[1],
                          upper.wt    = input$wt_db_model_2[2],
                          prop.male   = input$males_db_model_2/100, # convert % into proportion (0 - 1)
                          seed.number = input$seed_number_model_2
    )

    # Get the column names of all dataframes, excluding the common column ("ID")
    column_names_model_2 <- c(setdiff(names(rv_cov_1_model_2$df), "ID"),
                              setdiff(names(rv_cov_2_model_2$df), "ID"),
                              setdiff(names(rv_cov_3_model_2$df), "ID"))

    # Check if all column names are unique
    if (length(unique(column_names_model_2)) == length(column_names_model_2)) {
      dbm2_cov <- dbm2 %>%
        dplyr::left_join(rv_cov_1_model_2$df, by = "ID") %>%
        dplyr::left_join(rv_cov_2_model_2$df, by = "ID") %>%
        dplyr::left_join(rv_cov_3_model_2$df, by = "ID")
    } else {
      shiny::showNotification("ERROR: All custom covariate names must be different from each other. Please rename them first.", type = "error", duration = 10)
      dbm2_cov <- dbm2
    }

    return(dbm2_cov)
  })

  output$demog_info_model_2 <- renderUI({
    if(input$db_model_2 == "None" & all(c(input$custom_cov_1_model_2, input$custom_cov_2_model_2, input$custom_cov_3_model_2) == "")) {
      htmltools::HTML(paste("<br><strong style='color: red;'><p>", "Summary statistics is not available when no database or custom covariates are selected.","</strong></p>"))
    } else {
      db_summ_model_2 <- database_model_2() %>%
        calc_summary_stats()

      if("SEX" %in% names(database_model_2())) {
        db_summ_model_2_male <- database_model_2() %>% dplyr::filter(SEX == 0) %>%
          calc_summary_stats()

        db_summ_model_2_female <- database_model_2() %>% dplyr::filter(SEX == 1) %>%
          calc_summary_stats()
      } else {
        db_summ_model_2_male   <- db_summ_model_2
        db_summ_model_2_female <- db_summ_model_2
      }

      if(input$db_model_2 != "None") {
        db_summ_model_2 <- db_summ_model_2 %>%
          dplyr::rename(
            `AGE (y)`  = AGE,
            `WT (kg)`  = WT,
            `HT (cm)`  = HT,
            `BSA (m²)` = BSA
          )
        db_summ_model_2_male <- db_summ_model_2_male %>%
          dplyr::rename(
            `AGE (y)`  = AGE,
            `WT (kg)`  = WT,
            `HT (cm)`  = HT,
            `BSA (m²)` = BSA
          )
        db_summ_model_2_female <- db_summ_model_2_female %>%
          dplyr::rename(
            `AGE (y)`  = AGE,
            `WT (kg)`  = WT,
            `HT (cm)`  = HT,
            `BSA (m²)` = BSA
          )
      }
      db_summ_model_2 <- db_summ_model_2 %>%
        flextable::flextable() %>%
        flextable::font(font = "Arial") %>%
        flextable::bold(part = "header") %>%
        flextable::add_header_lines(paste0("All Subjects [Database: ", input$db_model_2, "] (n = ", nrow(database_model_2() %>% dplyr::distinct(ID)),")")) %>%
        flextable::bold(i = 4) %>%
        flextable::autofit() %>%
        flextable::theme_zebra() %>%
        flextable::htmltools_value()

      if("SEX" %in% names(database_model_2())) {
        db_summ_model_2_male <- db_summ_model_2_male %>%
          flextable::flextable() %>%
          flextable::font(font = "Arial") %>%
          flextable::bold(part = "header") %>%
          flextable::add_header_lines(paste0("Males (n = ", nrow(database_model_2() %>% dplyr::filter(SEX == 0) %>% dplyr::distinct(ID)),")")) %>%
          flextable::bold(i = 4) %>%
          flextable::autofit() %>%
          flextable::theme_zebra() %>%
          flextable::htmltools_value()

        db_summ_model_2_female <- db_summ_model_2_female %>%
          flextable::flextable() %>%
          flextable::font(font = "Arial") %>%
          flextable::bold(part = "header") %>%
          flextable::add_header_lines(paste0("Females (n = ", nrow(database_model_2() %>% dplyr::filter(SEX == 1) %>% dplyr::distinct(ID)),")")) %>%
          flextable::bold(i = 4) %>%
          flextable::autofit() %>%
          flextable::theme_zebra() %>%
          flextable::htmltools_value()
      } else {
        db_summ_model_2_female <- db_summ_model_2
        db_summ_model_2_male   <- db_summ_model_2
      }

      if(input$males_db_model_2 == 0 | input$males_db_model_2 == 100 | input$db_model_2 == "None") { # Do not show tables by sex if proportion is 0% or 100%
        fluidRow(db_summ_model_2)
      } else {
        fluidRow(db_summ_model_2, htmltools::br(), db_summ_model_2_male, htmltools::br(), db_summ_model_2_female)
      }
    }
  })

  demog_plot_model_2 <- reactive({
    print_demog_plots(database_model_2())
  })

  output$demog_plots_model_2 <- renderUI({
    if(input$db_model_2 == "None") {
      htmltools::HTML(paste("<br><strong style='color: red;'><p>", "Plots of weight and sex are not available when no database is selected.","</strong></p>"))
    } else {
      renderPlot(demog_plot_model_2())
    }
  })

  #### Demog download section
  output$download_demog_data_model_2 <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_demog_data_model_2.csv")
    },
    content = function(file) {
      #write.csv(database_model_2(), file, row.names = FALSE)
      data.table::fwrite(database_model_2(), file, quote = FALSE, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  output$download_demog_plot_model_2 <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_demographics_plot_model_2.pdf")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = demog_plot_model_2(), device = "pdf", width = 16, height = 8)
    }
  )

  observeEvent(changed_reacted_param_model_2(), {
    if (!shinyAce::is.empty(mrgsolve::as.matrix(mrgsolve::omat(changed_reacted_param_model_2())))) {
      tmp <- extract_matrix(changed_reacted_param_model_2(), name_of_matrix = "omega", debug = show_debugging_msg)
      extracted_omega_model_2(tmp)
      iiv_checkpoint_model_2$extract_model_omega <- TRUE
    }

    if (!shinyAce::is.empty(mrgsolve::as.matrix(mrgsolve::smat(changed_reacted_param_model_2())))) {
      tmp1 <- extract_matrix(changed_reacted_param_model_2(), name_of_matrix = "sigma", debug = show_debugging_msg)
      extracted_sigma_model_2(tmp1)
      iiv_checkpoint_model_2$extract_model_sigma <- TRUE
    }

  })

  omega_matrix_model_2 <- eventReactive(extracted_omega_model_2(), {
    rhandsontable::rhandsontable(extracted_omega_model_2(), colTypes = rep("text", ncol(extracted_omega_model_2())), contextMenu = FALSE) %>%
      rhandsontable::hot_cols(renderer = "
             function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               if (value === null || value === 'NA') {
                 td.style.background = '#D3D3D3';  // Grey color
                 cellProperties.readOnly = true;  // Make cell read-only
               }
             }")
  })

  output$omega_model_2 <- rhandsontable::renderRHandsontable({
    omega_matrix_model_2()
  })

  sigma_matrix_model_2 <- eventReactive(extracted_sigma_model_2(), {
    rhandsontable::rhandsontable(extracted_sigma_model_2(), colTypes = rep("text", ncol(extracted_sigma_model_2())), contextMenu = FALSE) %>%
      rhandsontable::hot_cols(renderer = "
           function(instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.TextRenderer.apply(this, arguments);
             if (value === null || value === 'NA') {
               td.style.background = '#D3D3D3';  // Grey color
               cellProperties.readOnly = true;  // Make cell read-only
             }
           }")

  })

  output$sigma_model_2 <- rhandsontable::renderRHandsontable({
    sigma_matrix_model_2()
  })

  observeEvent(input$iiv_action_model_2, {
    #message('sanity check')
    if (iiv_checkpoint_model_2$extract_model_omega && !shinyAce::is.empty(input$omega_model_2)) {
      if(show_debugging_msg) {
        message('omega input fulfilled')
      }
      tmp1 <- rhandsontable::hot_to_r(input$omega_model_2)
      extracted_omega_model_2(tmp1)
      iiv_checkpoint_model_2$updated_matrix_omega <- TRUE
    }

    if (iiv_checkpoint_model_2$extract_model_sigma && !shinyAce::is.empty(input$sigma_model_2)) {
      if(show_debugging_msg) {
        message('sigma input fulfilled')
      }
      tmp2 <- rhandsontable::hot_to_r(input$sigma_model_2)
      extracted_sigma_model_2(tmp2)
      iiv_checkpoint_model_2$updated_matrix_sigma <- TRUE
    }
    if(show_debugging_msg) {
      message('matrix applied')
    }
  })

  changed_matrix_model_2 <- reactiveVal()

  observe({
    variability_object_model_2 <- changed_reacted_param_model_2()

    if (iiv_checkpoint_model_2$updated_matrix_omega) {
      if(show_debugging_msg) {
        message('updating matrix omega')
      }
      updated_omega_model_2 <- reconstruct_matrices(variability_object_model_2,
                                                    extracted_omega_model_2(),
                                                    name_of_matrix = "omega",
                                                    debug = show_debugging_msg)
      variability_object_model_2 <- update_variability(variability_object_model_2,
                                                       updated_omega_model_2,
                                                       name_of_matrix = "omega",
                                                       debug = show_debugging_msg)
      iiv_checkpoint_model_2$reconstructed_iiv <- TRUE
      if(show_debugging_msg) {
        message('updated complete matrix omega')
      }
    }

    if (iiv_checkpoint_model_2$updated_matrix_sigma) {
      if(show_debugging_msg) {
        message('updating matrix sigma')
      }
      updated_sigma_model_2 <- reconstruct_matrices(variability_object_model_2,
                                                    extracted_sigma_model_2(),
                                                    name_of_matrix = "sigma",
                                                    debug = show_debugging_msg)
      variability_object_model_2 <- update_variability(variability_object_model_2,
                                                       updated_sigma_model_2,
                                                       name_of_matrix = "sigma",
                                                       debug = show_debugging_msg)
      iiv_checkpoint_model_2$reconstructed_iiv <- TRUE
      if(show_debugging_msg) {
        message('updated complete matrix sigma')
      }
    }
    changed_matrix_model_2(variability_object_model_2)
  })

  output$console_output_iiv_model_2 <- renderPrint({
    shiny::req(changed_matrix_model_2())
    if(mrgsolve::is.mrgmod(changed_matrix_model_2())) {
      cat(matrix_info_message)
      cat("\n\nCurrent Model Matrices:\n\n")
      print(mrgsolve::revar(changed_matrix_model_2()))
    }
  })

  ## IIV simulation 2 ----
  simulation_IIV_output_model_2 <- reactive({
    if (iiv_checkpoint_model_2$reconstructed_iiv) {
      iiv_sim_output_model_2 <-
        run_single_sim(
          input_model_object = changed_matrix_model_2(),
          pred_model         = model_2_is_pred(),
          ev_df              = dosing_regimen_model_2(),
          model_dur          = model_duration_argument_model_2(),
          model_rate         = model_rate_argument_model_2(),
          sampling_times     = sampling_options(),
          seed               = input$seed_number_model_2,
          debug              = show_debugging_msg,
          divide_by          = time_value(),
          nsubj              = n_subj_model_2_clean(),
          append_id_text     = "m2-",
          ext_db             = database_model_2(),
          parallel_sim       = FALSE, #input$para_checkbox,
          parallel_n         = 100 # input$para_n
        )

      iiv_sim_output_model_2 <- quantile_output(iiv_sim_output_model_2,
                                                yvar = input$yaxis_name_2,
                                                lower_quartile = sanitize_numeric_input(input$lower_quartile, legal_minimum = 0, display_error = TRUE)/100,
                                                upper_quartile = sanitize_numeric_input(input$upper_quartile, legal_maximum = 100, display_error = TRUE)/100
      )

      iiv_sim_output_model_2$yvar <- iiv_sim_output_model_2[[input$yaxis_name_2]]

      if(is.data.frame(iiv_sim_output_model_2)) {
        iiv_checkpoint_model_2$iiv_simulation <- TRUE
        if (show_debugging_msg) {
          message('simulation1 generated')
        }
        return(iiv_sim_output_model_2)
      }
    }
  }, label = 'iiv_output_model_2()')


  ### Plot ---
  sim_1_dataset_iiv_arg <- reactive({
    tmp <- NULL

    if (input$yaxis_name %in% colnames(simulation_IIV_output_model_1()) && iiv_checkpoint_model_1$iiv_simulation) {
      if (input$show_iiv_model_1) {
        tmp <- simulation_IIV_output_model_1()
      }
    }
    return(tmp)
  }, label = 'sim_1_iiv_dataset_arg')

  sim_2_dataset_iiv_arg <- reactive({
    tmp2 <- NULL

    if (input$yaxis_name_2 %in% colnames(simulation_IIV_output_model_2()) && iiv_checkpoint_model_2$iiv_simulation) {
      if (input$show_iiv_model_2) {
        if(show_debugging_msg) {
          message('model 2 argument fulfilled')
        }
        tmp2 <- simulation_IIV_output_model_2()
      }
    }
    return(tmp2)
  }, label = 'sim_2_iiv_dataset_arg')


  iiv_page_plot <- reactive({
    nonmem_dataset <- if (input$combine_nmdata_iiv && final_output_executed()) {
      nmdata_cmt_filtered()
    } else {
      NULL
    }

    title <- if (input$combine_nmdata_iiv && is.null(nonmem_dataset)) {
      unsupported_dataset
    } else {
      NULL
    }

    if (!is.null(sim_1_dataset_iiv_arg()) || !is.null(sim_2_dataset_iiv_arg())) {

      sim_plot <- plot_iiv_data_with_nm(input_dataset1 = sim_1_dataset_iiv_arg(),
                                        input_dataset2 = sim_2_dataset_iiv_arg(),
                                        nonmem_dataset = nonmem_dataset,
                                        line_color_1 = model_1_color,
                                        line_color_2 = model_2_color,
                                        xvar = 'TIMEADJ',
                                        yvar = 'yvar',
                                        yvar_2 = 'yvar',
                                        log_x_axis = input$log_x_axis_iiv,
                                        log_y_axis = input$log_y_axis_iiv,
                                        geom_point_data_option = input$geom_point_data_option_iiv,
                                        nm_yvar = input$nonmem_y_axis,
                                        show_ind_profiles = input$show_ind_profiles,
                                        y_median = 'median_yvar',
                                        y_mean = 'mean_yvar',
                                        show_y_mean = input$show_mean_iiv,
                                        y_min = 'lower_yvar',
                                        y_max = 'upper_yvar',
                                        title = input$plot_title_iiv,
                                        stat_summary_data_option = input$stat_sum_data_option_iiv,
                                        xlabel = input$x_axis_label,
                                        ylabel = input$y_axis_label,
                                        debug  = show_debugging_msg,
                                        show_x_intercept = input$show_x_intercept_threshold,
                                        x_intercept_value = sanitize_numeric_input(input$x_value_threshold)/time_value(),
                                        show_y_intercept = input$show_y_intercept_threshold,
                                        y_intercept_value = input$y_value_threshold
      )


      if (!is.null(title)) {
        sim_plot <- sim_plot + ggplot2::theme(plot.title = ggplot2::element_text(color = model_1_color))
      }

      return(sim_plot)
    }
  }, label = 'iiv_page_plot')

  ### UI: output$simulation_plot_output ----

  output$iiv_plot_output <- renderUI({
    shiny::conditionalPanel(
      condition = "true",
      div(style = "height:600px",
          if (!is.null(iiv_page_plot())) {
            if(input$do_iiv_plotly) {
              plotly::plotlyOutput("iiv_plotly", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
            } else {
              plotOutput("iiv_ggplot", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
            }
          }
      )
    )
  })

  output$iiv_ggplot <- renderPlot(iiv_page_plot() + add_watermark(watermark_toggle = insert_watermark) + ggplot2::theme(text = ggplot2::element_text(size = 16)))

  output$iiv_plotly <- plotly::renderPlotly(convert_to_plotly_watermark(iiv_page_plot(),
                                                                        format = input$plotly_iiv_format,
                                                                        filename = input$plotly_iiv_filename,
                                                                        width = input$plotly_iiv_width,
                                                                        height = input$plotly_iiv_height,
                                                                        plotly_watermark = insert_watermark)
  )

  #### IIV Plot download section
  observeEvent(input$do_iiv_plotly, {
    if (input$do_iiv_plotly) {
      shinyjs::disable("download_iiv_plot")
      updateSelectInput(session, "plotly_iiv_format", label = plotly_format_label,
                        choices = c("png", "jpeg", "svg", "webp"))
    } else {
      shinyjs::enable("download_iiv_plot")
      updateSelectInput(session, "plotly_iiv_format", label = plotly_format_label,
                        choices = c("png", "pdf", "jpeg", "svg"))
    }
  }, label = "update_download_iiv_plot")

  output$download_iiv_plot <- downloadHandler(
    filename = function() {
      paste0(input$plotly_iiv_filename, ".", input$plotly_iiv_format)
    },
    content = function(file) {
      ggplot2::ggsave(file,
                      plot = iiv_page_plot() + add_watermark(watermark_toggle = insert_watermark),
                      device = input$plotly_iiv_format,
                      units = "px",
                      width = input$plotly_iiv_width,
                      height = input$plotly_iiv_height
      )
    }
  )

  ### UI: output$proportion_above_threshold ----
  observeEvent(sampling_options(), {
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'x_value_threshold',
                                    label = NULL,
                                    choices = sort(unique(sampling_options())),
                                    select = dplyr::last(unique(sampling_options()))
    )
  }, label = 'update x value selection proportion')

  pct_above_y_at_x_model_1 <- reactive({
    pct_above_y_at_x(model_is_valid = iiv_checkpoint_model_1$iiv_simulation,
                     input_df       = simulation_IIV_output_model_1(),
                     y_name         = input$yaxis_name,
                     y_value        = input$y_value_threshold,
                     x_name         = "TIME",
                     x_value        = input$x_value_threshold)
  })

  pct_above_y_at_x_model_1_n <- reactive({
    pct_above_y_at_x(model_is_valid = iiv_checkpoint_model_1$iiv_simulation,
                     input_df       = simulation_IIV_output_model_1(),
                     y_name         = input$yaxis_name,
                     y_value        = input$y_value_threshold,
                     x_name         = "TIME",
                     x_value        = input$x_value_threshold,
                     return_number_ids = TRUE)
  })

  pct_above_y_at_x_model_2 <- reactive({
    pct_above_y_at_x(model_is_valid = iiv_checkpoint_model_2$iiv_simulation,
                     input_df       = simulation_IIV_output_model_2(),
                     y_name         = input$yaxis_name_2,
                     y_value        = input$y_value_threshold,
                     x_name         = "TIME",
                     x_value        = input$x_value_threshold)
  })

  pct_above_y_at_x_model_2_n <- reactive({
    pct_above_y_at_x(model_is_valid = iiv_checkpoint_model_2$iiv_simulation,
                     input_df       = simulation_IIV_output_model_2(),
                     y_name         = input$yaxis_name_2,
                     y_value        = input$y_value_threshold,
                     x_name         = "TIME",
                     x_value        = input$x_value_threshold,
                     return_number_ids = TRUE)
  })

  model_1_proportion_text <- reactive({
    paste0("Model 1: ", pct_above_y_at_x_model_1(), "% Above (n=", pct_above_y_at_x_model_1_n(), "/", n_subj_model_1_clean(),")")
  })

  model_2_proportion_text <- reactive({
    paste0("Model 2: ", pct_above_y_at_x_model_2(), "% Above (n=", pct_above_y_at_x_model_2_n(), "/", n_subj_model_2_clean(),")")
  })

  output$proportion_above_threshold_model_1 <- renderUI({
    htmltools::HTML(paste("<b>", model_1_proportion_text()))
  })

  output$proportion_above_threshold_model_2 <- renderUI({
    htmltools::HTML(paste("<b>", model_2_proportion_text()))
  })

  ### Update input$exp_time_range ----
  observe({
    (shiny::req(simulation_output_model_1()))
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'min_exp_obs_time_model',
                                    label = NULL,
                                    choices = sort(unique(simulation_output_model_1()$TIME)),
                                    select = min(unique(simulation_output_model_1()$TIME))
    )

    shinyWidgets::updatePickerInput(session,
                                    inputId = 'max_exp_obs_time_model',
                                    label = NULL,
                                    choices = sort(unique(simulation_output_model_1()$TIME)),
                                    select = max(unique(simulation_output_model_1()$TIME))
    )
  }, label = 'update exp time selection')

  observeEvent(input$min_exp_obs_time_model, {
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'max_exp_obs_time_model',
                                    label = NULL,
                                    choices = unique(sort(simulation_output_model_1()$TIME[simulation_output_model_1()$TIME > as.numeric(input$min_exp_obs_time_model)])),
                                    selected = input$max_exp_obs_time_model
    )
  }, label = 'update exp time selection (Max)')

  observeEvent(input$max_exp_obs_time_model, {
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'min_exp_obs_time_model',
                                    label = NULL,
                                    choices = unique(sort(simulation_output_model_1()$TIME[simulation_output_model_1()$TIME < as.numeric(input$max_exp_obs_time_model)])),
                                    selected = input$min_exp_obs_time_model
    )
  }, label = 'update exp time selection (Min)')

  # Model 2
  observe({
    (shiny::req(simulation_output_model_2()))
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'min_exp_obs_time_model',
                                    label = NULL,
                                    choices = sort(unique(simulation_output_model_2()$TIME)),
                                    select = min(unique(simulation_output_model_2()$TIME))
    )

    shinyWidgets::updatePickerInput(session,
                                    inputId = 'max_exp_obs_time_model',
                                    label = NULL,
                                    choices = sort(unique(simulation_output_model_2()$TIME)),
                                    select = max(unique(simulation_output_model_2()$TIME))
    )
  }, label = 'update exp time selection model 2')

  observeEvent(input$min_exp_obs_time_model, {
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'max_exp_obs_time_model',
                                    label = NULL,
                                    choices = unique(sort(simulation_output_model_2()$TIME[simulation_output_model_2()$TIME > as.numeric(input$min_exp_obs_time_model)])),
                                    selected = input$max_exp_obs_time_model
    )
  }, label = 'update exp time selection (Max) model 2')

  observeEvent(input$max_exp_obs_time_model, {
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'min_exp_obs_time_model',
                                    label = NULL,
                                    choices = unique(sort(simulation_output_model_2()$TIME[simulation_output_model_2()$TIME < as.numeric(input$max_exp_obs_time_model)])),
                                    selected = input$min_exp_obs_time_model
    )
  }, label = 'update exp time selection (Min) model 2')

  # exposure_data
  exposure_data <- reactive({
    both_model_exp <- NULL
    if (!is.null(sim_1_dataset_iiv_arg()) || !is.null(sim_2_dataset_iiv_arg())) {

      if(!is.null(sim_1_dataset_iiv_arg())) {
        model_1_exp <- exposures_table(input_simulated_table = sim_1_dataset_iiv_arg(),
                                       output_conc = 'yvar',
                                       start_time = as.numeric(input$min_exp_obs_time_model),
                                       end_time   = as.numeric(input$max_exp_obs_time_model)
        ) %>% mutate(MODEL = "Model 1")
      }

      if(!is.null(sim_2_dataset_iiv_arg())) {
        model_2_exp <- exposures_table(input_simulated_table = sim_2_dataset_iiv_arg(),
                                       output_conc = 'yvar',
                                       start_time = as.numeric(input$min_exp_obs_time_model),
                                       end_time   = as.numeric(input$max_exp_obs_time_model)
        ) %>% mutate(MODEL = "Model 2")
      }

      if(!is.null(sim_1_dataset_iiv_arg()) && !is.null(sim_2_dataset_iiv_arg())) {
        both_model_exp <- dplyr::bind_rows(model_1_exp, model_2_exp)
      } else {
        if(!is.null(sim_1_dataset_iiv_arg())) {
          both_model_exp <- model_1_exp
        }
        if(!is.null(sim_2_dataset_iiv_arg())) {
          both_model_exp <- model_2_exp
        }
      }
    }

    return(both_model_exp)
  }, label = "exposure_data")

  ## Download variability exposures table ----
  output$download_exposures_table <- downloadHandler(
    filename = function() {
      paste0(today_numeric(), "_exposures_data.csv")
    },
    content = function(file) {
      data.table::fwrite(exposure_data(), file, quote = FALSE, row.names = FALSE)
    }
  )

  # iiv_exp_plot
  iiv_exp_plot <- reactive({

    if (!is.null(exposure_data())) {

      # Handling filtering of dataset outside the function as for some unknown reason plotly has
      # resizing glitches when this code is inside the function?

      both_model_exp <- exposure_data()

      if(!input$exp_show_model_1) {
        both_model_exp <- both_model_exp %>% filter(MODEL != "Model 1")
      }

      if(!input$exp_show_model_2) {
        both_model_exp <- both_model_exp %>% filter(MODEL != "Model 2")
      }

      if(nrow(both_model_exp) > 0) { # At least some rows are present to do a ggplot

        exp_plot <- plot_iiv_exp_data(input_dataset = both_model_exp,
                                      yvar          = input$select_exp,
                                      ylab          = input$exp_yaxis_label,
                                      model_1_name  = input$exp_model_1_name,
                                      model_2_name  = input$exp_model_2_name,
                                      model_1_color = model_1_color,
                                      model_2_color = model_2_color,
                                      show_stats    = input$exp_display_stats,
                                      title         = input$plot_title_exp_model)
      } else {
        exp_plot <- NULL
      }
      return(exp_plot)
    }
  }, label = 'iiv_exp_plot')

  output$iiv_exp_output <- renderUI({
    shiny::conditionalPanel(
      condition = "true",
      div(style = "height:600px",
          if (!is.null(iiv_exp_plot())) {
            if(input$do_exp_plotly) {
              plotly::plotlyOutput("iiv_exp_plotly", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
            } else {
              plotOutput("iiv_exp_ggplot", height = '600px') %>% shinycssloaders::withSpinner(type = 8, hide.ui = FALSE, color = bi_darkgreen)
            }
          }
      )
    )
  })

  output$iiv_exp_ggplot <- renderPlot(iiv_exp_plot() + add_watermark(watermark_toggle = insert_watermark) + ggplot2::theme(text = ggplot2::element_text(size = 16)))

  output$iiv_exp_plotly <- plotly::renderPlotly(convert_to_plotly_watermark(iiv_exp_plot(),
                                                                            format = input$plotly_exp_format,
                                                                            filename = input$plotly_exp_filename,
                                                                            width = input$plotly_exp_width,
                                                                            height = input$plotly_exp_height,
                                                                            plotly_watermark = insert_watermark)
  )

  #### IIV Exposures Plot download section
  observeEvent(input$do_exp_plotly, {
    if (input$do_exp_plotly) {
      shinyjs::disable("download_exp_plot")
      updateSelectInput(session, "plotly_exp_format", label = plotly_format_label,
                        choices = c("png", "jpeg", "svg", "webp"))
    } else {
      shinyjs::enable("download_exp_plot")
      updateSelectInput(session, "plotly_exp_format", label = plotly_format_label,
                        choices = c("png", "pdf", "jpeg", "svg"))
    }
  }, label = "update_download_exp_plot")

  output$download_exp_plot <- downloadHandler(
    filename = function() {
      paste0(input$plotly_exp_filename, ".", input$plotly_exp_format)
    },
    content = function(file) {
      ggplot2::ggsave(file,
                      plot = iiv_exp_plot() + add_watermark(watermark_toggle = insert_watermark),
                      device = input$plotly_exp_format,
                      units = "px",
                      width = input$plotly_exp_width,
                      height = input$plotly_exp_height
      )
    }
  )

} # end of server

shinyApp(ui, server)
