# Page Title 
#' @export
page_title            <- 'Model Visualization Platform (MVP)'

## UI settings
#' @export
sidebar_width         <- 5  
#' @export
mainbar_width         <- 12 - sidebar_width
#' @export
debounce_timer_fast   <- 500
#' @export
debounce_timer_slow   <- 900
#' @export
max_samples           <- 20000 # Max samples = max sampling time * frequency of samples

# Colours
#' @export
bi_blue               <- "#003366" # BI blue color
#' @export
bi_lightblue          <- "#99CCFF"
#' @export
bi_darkgreen          <- "#08312A" # BI dark green
#' @export
bi_green              <- "#00E47C" # BI accent green
#' @export
bi_warmgray           <- "#E5E3DE"
#' @export
bi_lightgray          <- "#F6F5F3"
#' @export
slider_color          <- "#FFB600" # yellow
#' @export
model_1_color         <- "#F8766D" # Shade of red
#' @export
model_2_color         <- "#7570B3" # Shade of blue   

#### Page 1 Data Input
#' @export
bspop_upload_dataset  <- 'Supplying a NONMEM-formatted dataset is completely optional.<br><br>Once it is uploaded (and filtered), you may visualize it in other Tabs by ticking the "Overlay Dataset" checkbox.<br><br>Note: Maximum file size is currently limited to 100 MB.'
#' @export
bspop_dataset_cleaning<- 'The uploaded dataset is automatically "cleaned" by the options included here.<br><br>Note: some options are always enabled to ensure compatibility in plotting.'
#' @export
bspop_deselect        <- '(Optional) choose any number of columns to remove from the dataset prior to passing onto the editor below to help simplify the dataset.'
#' @export
bspop_apply           <- 'After pressing Apply, check the Console below to see if there are any errors.'
#' @export
bspop_select_columns  <- 'ID, TIME, and DV are required columns for the Dataset to be fully supported in plotting. <br><br> If missing, the user can create their own in the code editor.'
#' @export
bspop_dataset_code    <- '(Optional) Adjust the uploaded dataset to facilitate quick data exploration or to ensure full compatibility with the App (requires the ID, TIME, and DV columns).<br><br>At minimum, the ID column needs to be present for Data Exploration.'

#' @export
transpose_checkbox    <- 'Transpose Table'
#' @export
bspop_transpose       <- 'Check to transpose summary table such that each original column becomes a row instead.'

#' @export
bspop_nca_tooltip     <- 'Some fields are automatically pre-filled based on standard column names. If errors are encountered, try filtering the dataset on the editor on the left hand side.<br><br>Note - for a non-NONMEM dataset, it is necessary to uncheck the "Coerce Dataset to Numeric" option within the "Built-in Dataset Cleaning Options" tab to retain characters.'
#' @export
bspop_calc_nca        <- 'Whenever the above settings are adjusted, a new NCA should be generated.'
#' @export
bspop_dose_value      <- '(Optional) Provide a dose amount to derive metrics normalized by dose.'
#' @export
bspop_mw_value        <- '(Optional) Provide a molecular weight to derive metrics related to Clearance and Volume (assumed to be in L).'

## Plotting options
#' @export
select_x_axis         <- 'Select X-axis'
#' @export
select_y_axis         <- 'Select Y-axis'
#' @export
select_color          <- 'Color by:'
#' @export
select_median         <- 'Median line by:'
#' @export
median_line_label     <- 'Median line'
#' @export
log_x_axis_label      <- 'Log X-axis'
#' @export
log_y_axis_label      <- 'Log Y-axis'
#' @export
plot_title_label      <- 'Plot Title'
#' @export
insert_lm_eqn_label   <- 'Linear Regression'
#' @export
insert_smoother_label <- 'Smoother'
#' @export
facet_by_label        <- 'Facet(s) by:'
#' @export
plot_title_placeholder<- "Insert optional plot title"
#' @export
select_label_size_label<- "Text Label Size"
#' @export
bspop_select_label_size<- "Label sizes are only applicable for linear regression formulae, counts (N=x), or dosing info."
#' @export
quantize_x_label       <- "Quantize X-axis"
#' @export
bspop_quantize_x       <- "(Optional) Split a continuous (i.e. numeric) X-variable into a number of discrete quantiles with approximately equal number of observations in each quantile.<br><br>Note: the plot will be converted into a box plot automatically (not grouped by ID, unlike the normal box plot option)."

#' @export
add_data_stat_sum     <- 'Median line [dataset]'
#' @export
bspop_data_stat_sum   <- 'Insert a line connecting median values for grouped observations (up to 20 quantiled bins) for continuous plots. Bins that are within 5% to each other will be merged.'
#' @export
boxplot_label         <- 'Box Plot (by unique ID)'
#' @export
bspop_do_boxplot      <- 'Box plot figure assumes the X-axis variable is categorical, the Y-axis variable is continuous, and counts (N) represents the number of unique IDs i.e. usually the baseline value.<br><br>Turning off interactive plots may achieve better visual results for box plots (Color variable is dodged and box plot widths are proportional to the square-roots of the number of observations in the groups).'
#' @export
bspop_do_data_plotly  <- 'Interactive plots using the "plotly" package could take a longer time for big datasets. Turning it off may achieve better visual results for box plots.'# <br><br>Note - "Download Options" do not apply when plots are not interactive.'
#' @export
label_fixed_scale     <- 'Same Scale'
#' @export
bspop_fixed_scale     <- 'Check to ensure the same scale is used for all plots, otherwise uncheck to allow free X- and Y- scaling for each sub-plot.' # <br><br>Note: For Log axis, the same scale is used for plots on the same page only.'
#' @export
label_insert_dosing   <- 'Show Dose Info'
#' @export
bspop_insert_dosing   <- 'Shows dosing info as scaled vertical lines in continuous plots, with text labels denoting each unique dose (if "EVID" column is found, and expanded accordingly by "ADDL", "II", and "RATE").<br><br>Note: Scales may be affected due the way dose lines are handled. Turning off interactive plot may achieve better results.'
#' @export
label_dose_colname    <- 'Dose Column'
#' @export
bspop_dose_colname    <- 'Provide column name that contains the dosing info, usually "AMT" or "DOSE".'
#' @export
label_dose_units      <- 'Dose Units'
#' @export
bspop_dose_units      <- '(Optional) Provide units to be appended on to the text label.'
#' @export
label_lloq_colname    <- 'LLOQ Column' # shiny::HTML(paste0("LLOQ Column <i class='fa fa-question-circle' title='(Optional) Provide LLOQ/BLQ column to be plotted as a dashed horizontal line. Not applicable for box plots.' style='margin-left: 3px;'></i>"))
#' @export
bspop_lloq_colname    <- '(Optional) Provide LLOQ/BLQ column to be plotted as dashed horizontal lines.'

#' @export
bspop_download_plot   <- 'Adjust output settings in the "Download Options" box below.<br><br>This button will be disabled when the plot is interactive, instead please download by using the Camera icon on the top right of the plot.'
#' @export
plotly_filename_label <- "Plot name"
#' @export
bspop_plotly_file_name_label <- 'The plot can be downloaded by hovering over the plot and clicking on the Camera icon (for interactive plots), or by clicking the Download Plot button (for non-interactive plots).'
#' @export
plotly_format_label   <- "File format"
#' @export
plotly_width_label    <- "Width (px)"
#' @export
plotly_height_label   <- "Height (px)"
#' @export
bspop_plotly_width_height <- 'Leave empty to use current dimensions.'
#' @export
bspop_plotly_width_height_corr <- 'Leave empty to use current dimensions.<br><br>Tip: a larger size (e.g. >5000) is recommended if there are more than a few variables.'
#' @export
bspop_data_plot_options <- 'Most options set here (with the exception of "Facet by" and "Quantize X-axis") will be carried over to the "Ind. Plot" tab.'
#' @export
bspop_download_plot_all <- 'Generate and download all individual profiles in a single file (.pdf only). Adjust output settings in the "Download Options" box below.<br><br>Note: It can be time consuming if there are a large number of subjects! (Tip - More plots per page tends to be quicker.)'
#' @export
bspop_ind_plot_options  <- 'Most options (with the exception of "Facet by") are carried over from the "General Plot" tab.<br><br>Note: Smoother and median lines are not relevant for individual plots and are not inserted.'
#' @export
label_filter_by_id      <- 'Select ID(s):'
#' @export
bspop_filter_by_id      <- '(Optional) Page, Row, and Column settings will be ignored when any IDs are selected.'
#' @export
label_sort_by_ind       <- shiny::HTML(paste0("Sort by: <i class='fa fa-question-circle' title='(Optional) Arrange the order of plots by one or more variables (default is ID only). Note: First value of each ID is used (not time-varying).' style='margin-left: 3px;'></i>"))
#' @export
bspop_sort_by_ind       <- '(Optional) Arrange the order of plots by one or more variables (default is ID only).<br><br>Note: First value of each ID is used (not time-varying).'
#' @export
label_strat_by_ind      <- shiny::HTML(paste0("Flag Outliers by: <i class='fa fa-question-circle' title='(Optional) Try to stratify by a group where potential outliers (more than X% above/below the arithmetic mean of the group) will be appended to the ID label. Not applicable for box plots.' style='margin-left: 3px;'></i>"))
#' @export
bspop_strat_by_ind      <- '(Optional) Stratify by a grouping variable (usually a dose group).<br><br>Note: First value of each ID is used for stratifying (not time-varying).'
#' @export
label_outlier_threshold <- 'Outlier Threshold:'
#' @export
bspop_outlier_threshold <- '(Optional) Define a threshold (%) where if the arithmetic mean of the Y-axis of an ID is above/below the corresponding stratification group by this %, that ID will be flagged as a potential outlier. Only applicable when Flag Outliers is used.'
#' @export
label_highlight         <- shiny::HTML(paste0("Flag Variable: <i class='fa fa-question-circle' title='(Optional) First select a variable and then choose any of its value(s) to be highlighted in bigger asterisks. Not applicable for box plots.' style='margin-left: 3px;'></i>"))
#' @export
bspop_highlight_var     <- '(Optional) First select a variable and then choose any of its value(s) to be highlighted in bigger asterisks (only applicable for non- box plots).'
#' @export
label_highlight_values  <- 'Flag Value(s):'
#' @export
plotly_filename_all_label <- 'Plot name (all individuals)'
#' @export
plotly_format_all_label <- "File format"
#' @export
bspop_plotly_file_name_all_label <- 'All individual profiles can be downloaded by clicking the "Generate All Individuals" button.'
#' @export
plotly_width_all_label <- 'Width (inches)'
#' @export
plotly_height_all_label <- 'Height (inches)'

## shinyAce placeholder
#' @export
code_editor_init      <- '# Please apply all filtering and data cleaning directly using dplyr syntax.\n# e.g.: filter(...) %>% mutate(...)\n# Note: Please wait a few seconds after editing code before pressing Apply.\n\nfilter() %>% mutate()'

#### Page 2 Simulation
#' @export
bspop_select_model    <- 'Choose a template model, or select "Blank Template" to use a basic model as a starting point. Uploading of .cpp files is also supported. After finishing code editing, click the "Generate Model" button.<br><br>Note: Code changes are not permanent and will be reset when a different model is selected.'
#' @export
bspop_param_values    <- 'Once a model has been generated, all fixed effects parameters from the model code (i.e. inside $PARAM) will become available to be adjusted dynamically in real-time (however it does not feedback into the model code text).<br><br>Note: Non-sensible / unphysiological values may result in the app crashing and should be avoided.'
#' @export
bspop_download_cpp_model <- 'Save current model as a .cpp file including any real-time changes to the parameter values (fixed effects).<br><br>Note: The model must first be generated before the code can be downloaded!'
#' @export
bspop_model_code      <- 'Refer to the mrgsolve user guide (provided on the first line in the code editor) on the required syntax. After editing, click on the "Generate Model" button.<br><br>Note: If the model fails to compile, check the "Model Info (Console)" box below for more information.'
#' @export
bspop_model_info_console <- 'If the model build was successful, the generated model info will be summarized below. Otherwise, it will display the error message during compilation.'


## Dosing Regimen
#' @export
placeholder_password  <- 'Optional password to unlock more model choices and changes default values.'
#' @export
bspop_password        <- 'Please contact the App maintainer if you would like your current project to be included as a passworded-model.<br><br>Note: This feature is intended for Project Pharmacometricians to facilitate internal discussions only.'
#' @export
bspop_password_github <- 'Please visit the <a href="https://github.com/Boehringer-Ingelheim/MVPapp" target="_blank">MVPapp GitHub</a> page to install MVP as a R package to use this feature locally.'
#' @export
bspop_generate_model  <- "Please wait a few seconds after selecting models or editing code before clicking the Generate Model button."
#' @export
label_dose            <- 'Dose Amount'
#' @export
bspop_dose            <- 'Dose Amount is intentionally unitless, as the user has the flexibility to adjust the Y-axis label option in "Plotting Options".<br><br> For dosing in molecular weight or weight-based dosing, navigate to the Transform Dose tab.'

#' @export
label_para            <- 'Simulate in Parallel (experimental)'
#' @export
bspop_para            <- 'Simulation in parallel is only relevant for multiple subjects (i.e. in the Variability tab).<br><br> When the number of subjects being simulated is >= the threshold set in "Subjects Threshold for Parallelization", parallel simulation will be utilized.'
#' @export
label_para_n          <- 'Subjects Threshold for Parallelization'

#' @export
bspop_dosing_options  <- 'Up to 5 completely independent dosing regimens per Model are supported (e.g. loading dose + maintenance doses), which will be combined to form an overall dosing scenario.<br><br>Dosing amounts can be scaled i.e. molar doses or weight-based dosing in the "Transform Dose" sub-tab.'
#' @export
label_first_dose_time <- 'First Dose Time'
#' @export
label_input_cmt       <- 'Input CMT'
#' @export
label_total_doses     <- 'Total Doses'
#' @export
label_ii              <- 'Dosing Interval'
#' @export
bspop_time_units      <- 'Time units are dependent on model code, which are typically in hours'
#' @export
bspop_time_sampling   <- paste0('Time units are dependent on model code, which are typically in hours.<br><br>Note: Total samples (i.e. Max Sampling Time * Sampling Frequency) should not exceed ', max_samples, ' to avoid memory issues.')

#' @export
label_infdur          <- 'Inf. Duration'
#' @export
bspop_infdur          <- 'If either "Model Duration" or "Model Rate" option is checked, Infusion Duration will be ignored. <br><br>Note: the mrgsolve code needs to have the D_[compartment] (if modeling duration) or R_[compartment] (if modeling rate) syntax in $MAIN or the code will fail.'
#' @export
bspop_infdur_cb       <- 'If the "Model Duration" option is checked, Infusion Duration will be ignored  (this option toggle will be handled automatically). <br><br>Note: the mrgsolve code needs to have the D_[compartment] syntax in $MAIN or the code will fail.'
#' @export
bspop_rate            <- 'If the "Model Rate" option is checked, Infusion Duration will be ignored (this option toggle will be handled automatically). <br><br>Note: the mrgsolve code needs to have the R_[compartment] syntax in $MAIN or the code will fail.'

#' @export
bspop_solversettings  <- 'Please visit the <a href="https://mrgsolve.org/user-guide/components.html#solver-settings" target="_blank">mrgsolve user guide</a> for an explanation of the solver settings.'

## Simulation options
#' @export
label_multi_number    <- 'MW Multiplication Factor'

#' @export
label_MW              <- 'Molecular Weight (g/mol)'
#' @export
bw_MW                 <- 'Will only be applied when the "Molecular Weight" checkbox is applied.'

#' @export
set_max_observation   <- 'Max Sampling Time'
#' @export
set_delta_observation <- 'Sampling Frequency'

#' @export
label_MW_checkbox     <- 'Molecular Weight'
#' @export
bspop_MW_checkbox     <- 'When checked, Dose Amount will be divided by MW and then multiplied by the Multiplication Factor.'

#' @export
label_wt_based_dosing_checkbox <- "Weight-Based Dosing"
#' @export
bspop_wt_based_dosing_checkbox <- 'When checked, Dose Amount will be multiplied by the Weight parameter as defined in $PARAM.'
#' @export
label_wt_based_dosing_name     <- "Name of Weight Parameter"
#' @export
bspop_wt_based_dosing_name     <- "Name provided here must match what is defined in $PARAM."

#' @export
model_dur_checkbox    <- 'Model Duration (Auto)'
#' @export
label_filter_cmt      <- 'Filter CMT:'
#' @export
bspop_custom_sampling_text <- "The Custom Sampling Times checkbox needs to be set for this to take effect. Only accepts numeric vectors. <br><br>Will override the Max Sampling Time and Sampling Frequency."
#' @export
bspop_custom_sampling_cb <- "When checked, the Custom Sampling Schedule will override the Max Sampling Time and Sampling Frequency."
#' @export
bspop_time_0_text     <- 'Append Sampling at Time 0. This option is recommended when modeling PD to better calculate change from baselines.<br><br>Note: Dosing events are not counted as valid sampling times.'
#' @export
model_rate_checkbox   <- 'Model Rate (Auto)'


## Plotting options
#' @export
label_x_axis          <- 'X-axis Label'
#' @export
label_y_axis          <- 'Y-axis Label'

#' @export
label_scale_x_axis    <- 'Scale X-axis (Divide by)'
#' @export
bspop_scale_x_axis    <- 'Hours, Days, Weeks, Months corresponds to dividing by 1 (no change), 24, 168, or 672 (Note: this assumes original time unit is in hours). <br><br>X-axis labels will be automatically changed. <br><br>Users can also insert their own custom scaling if desired (reminder: change the x-axis label accordingly).'
#' @export
bspop_x_axis_label    <- 'Automatic tick sizes will be provided if the labels match one of the following:<br><br>Time (hours), Time (days), Time (weeks), Time (months)<br><br>Note - change the label slightly if you do not want automatic tick sizes, e.g. "Time (h)" instead of "Time (hours)"'

#' @export
label_y_axis_sim_1    <- 'Select Y-axis for Model 1'
#' @export
label_y_axis_sim_2    <- 'Select Y-axis for Model 2'
#' @export
bspop_y_axis_sim      <- 'Variables inside $TABLE or $CAPTURE in the model code will be available for selection.'

#' @export
label_y_axis_data     <- 'Select Y-axis for Dataset'
#' @export
label_color_data_by   <- 'Color by:'
#' @export
bspop_color_data_by   <- 'Stratify the observations by the selected variable in the dataset (column name).<br><br>Note: Click on the legend in the interactive plot to toggle on/off each sub-stratification.'
#' @export
label_stat_sum_data_by<- 'Median line for Dataset by:'
#' @export
bspop_stat_sum_data_by<- 'Displays median line for each unique value in the selected variable from the dataset (column name). Defaults to the user-defined "Color by" choice.'
#' @export
label_main_sim_plot   <- 'Simulated Model Plot'

#' @export
show_model_1_label    <- "Show Model 1 (red)"
#' @export
show_model_2_label    <- "Show Model 2 (blue)"
#' @export
bspop_do_sim_plotly   <- 'Interactive plots using the "plotly" package could take a long time for simulations with lots of data (e.g. from overlaying datasets).' #<br><br>Note - "Download Options" do not apply when plots are not interactive.'


#' @export
label_combine_nm_data <- 'Overlay Dataset'
#' @export
bspop_combine_nm_data <- "Requires a valid dataset to be uploaded first on the Data Input page."
#' @export
add_sim_geom_point    <- 'Show Sampling Points [sims]'
#' @export
add_data_geom_point   <- 'Show Sampling Points [dataset]'

#' @export
unsupported_dataset   <- 'No Dataset imported or ID, DV, and TIME columns are not present.'

#### Page 3 Parameter Sensitivity Analysis
#' @export
bspop_select_psa      <- 'Select any structural parameter from the model (i.e. inside $PARAM) carried over from the Simulation tab to perform sensitivity analysis.'
#' @export
add_geom_vline        <- 'Show Time Intervals'
#' @export
add_geom_ribbon       <- 'Highlight AUC'
#' @export
bspop_adjust_param    <- 'Once a parameter has been selected, 3 separate profiles perturbing the parameter value (default: 50% [green], 100% [orange], 150% [blue]) will be simulated.'
#' @export
bspop_metrics_by_param_range <- 'PK/PD metrics are updated dynamically for each profile based on the selected time range (see right hand side).<br><br>Notes:<br>Cmax/Cmin/Tmax/Tmin returns the first occurrence if there are multiple matches.<br>Cavg is derived using the arithmetic mean of all sampled times.<br>AUC is derived using trapezoidal rule (linear up/down).<br>Mean %CFB is derived using mean(((Y-variable - Baseline) / Baseline) * 100), i.e. only accurate if the time samples are equidistant.'

#' @export
min_param_multiple    <- 0.5 # i.e. 50%
#' @export
mid_param_multiple    <- 1   # i.e. no change
#' @export
max_param_multiple    <- 1.5 # i.e. 150%

#' @export
digits_name           <- "Number of Digits"
#' @export
dp_checkbox_name      <- "Use decimal places"
#' @export
bspop_dp_checkbox     <- "By default the rounding uses significant digits. Check this box to use rounding by decimal places instead."

#' @export
bspop_select_time_interval <- "(Optional) Select the desired time intervals to derive metrics shown on the left hand side of the page (e.g. for partial AUC). The original time scale is provided for selection, independent of whether any scaling of the x-axis from the Simulation page was performed.<br><br>Note: The time interval is inclusive on both ends (i.e. start time <= time interval <= end time)."
#' @export
label_batch_table     <- shiny::HTML(paste0("Batch Run Parameters: <i class='fa fa-question-circle' title='(Optional) Tweak any parameter value for batch runs. Changes to reference values will automatically update the corresponding bounds. Edit upper/lower bounds last to avoid values being reset. Note: Changes will be lost when the original model updates from the \"Simulation\" page.' style='margin-left: 3px;'></i>"))
#' @export
bspop_batch_table     <- '(Optional) Tweak any parameter value for batch runs. Changes to reference values will automatically update the corresponding bounds. Edit upper/lower bounds last to avoid values being accidentally overwritten. Note: Changes will be lost when the original model updates from the \"Simulation\" page.'
#' @export
bspop_trim_tor         <- 'Filter the data to only display the top X number of parameters, sorted in order of impact on metric.'
#label_trim_tor        <- shiny::HTML(paste0("Param Numbers: <i class='fa fa-question-circle' title='Filter the data to only display the top X number of parameters.' style='margin-left: 3px;'></i>"))
#' @export
label_lower_multiplier <- "Lower Multiplier"
#' @export
label_upper_multiplier <- "Upper Multiplier"
#' @export
label_tor_show_digits  <- 'Display Values Verbatim'
#' @export
bspop_tor_show_digits  <- 'Display all parameter values as-is (i.e. treating it as a character and not rounded to 2 dp in the UI).<br><br>Note: This option should be toggled before editing the table as values may reset.'
#' @export
label_tor_variable     <- 'Select Variable to Derive Metrics'
#' @export
label_tor_display      <- 'Display as:'
#' @export
label_trim_tor         <- 'Limit Params'
#' @export
label_tor_display_text <- "Show Text"
#' @export
bspop_tor_display_text <- "Displays effect size on each end of the bar. Effect sizes which are <1% change relative to the reference are not displayed."
#' @export
bspop_show_bioeq       <- 'Insert visual guide to represent the bioequivalence criteria of 80% - 125%.'
#' @export
label_show_bioeq       <- 'Bioequivalence'

#' @export
label_fix_parameters  <- shiny::HTML(paste0("Fix Parameters: <i class='fa fa-question-circle' title='(Optional) Remove any parameters from the list of runs, e.g. parameters that may not make physiological sense to adjust.' style='margin-left: 3px;'></i>"))
#' @export
bspop_reset_reference <- 'Press to reset all reference values back to the original model from \"Simulation\" page.'
#' @export
bspop_generate_batch  <- 'Press to generate batch runs according to the Batch Run Parameters Table. The total number of simulations are 2 * number of parameters + 1 (the reference model).'
#' @export
bspop_batch_runs      <- 'Perform sensitivity analysis for all parameters to visualize the impact on exposure metrics, ordered by relative importance (Tornado Plots). Parameters are pre-configured with a upper/lower bound (default: \u00B150%).<br><br>Press the "Batch Run" button after any further adjustments to the table below.' #<br><br>Note: could be time consuming for larger models and/or frequent sampling.'
#' @export
bspop_bounds          <- 'Pre-configures batch runs by multiplying the reference value of all parameter by this number as the upper/lower bound.<br><br>The Table can also be further fine-tuned (i.e. directly edit) for all values, including the reference value.'


#### Page 5 Variability
#' @export
bspop_varmat          <- 'Initial values are carried over from the Simulation Tab. For the first model in the session, click on "Update Model" to initialize the plot.<br><br>As long as it is the same model, the plot will be refreshed whenever fixed effects ($PARAM), dosing, or demographics are updated (either from this page or from the Simulation Tab). Any changes to the variability values below require clicking on "Update Model" to refresh the plot.'
#' @export
bspop_varsim          <- 'Pre-configured demographics from external databases and/or custom covariate distributions can be inserted to be used as the input dataset for the simulations.<br><br>Note: The relevant covariate name(s) must first exist in the $PARAM section for it to be replaced.'
#' @export
max_sim_n             <- 5000 # Max number of allowed simulated subjects
#' @export
matrix_info_message   <- "If the user input values don't match the current model matrices below after pressing Update, \nit means the user input is invalid and has caused an error."
#' @export
bspop_update_model    <- "After pressing Update, please also check the Matrix Info below to confirm the inputs are valid. If not, the model will re-use the last known valid matrices."
#' @export
bspop_percentiles     <- "Note: Percentiles are applicable only when Show Individual Profiles is unchecked, i.e. does not affect individual subjects."
#' @export
bspop_nsubj_warning   <- paste0("Caution is advised when simulating a large number of subjects (e.g. >1000) with a frequent sampling schedule may result in long computational times and/or app instability.<br><br>Note: Maximum number of subjects able to be simulated is currently capped to ", max_sim_n,".")
#' @export
max_sim_n_error       <- paste0("Maximum number of subjects cannot exceed ", max_sim_n, ". Reverting to using 20.")
#' @export
label_db              <- "Select Population (Covariate) Database:"
#' @export
bspop_db              <- 'The choice of database is specific to project needs, where:<br><br>"None" - only number of subjects and seed are relevant, other settings are disabled (apart from custom covariates, if defined).<br><br><a href="https://wwwn.cdc.gov/nchs/nhanes/Default.aspx" target="_blank">NHANES - All ages</a> (general population [1999 - 2020 pre-pandemic], usually suitable for adults (>18))<br><br><a href="https://www.cdc.gov/growthcharts/percentile_data_files.htm" target="_blank">CDC - Age 0 - 20 yo</a> (general ped use between 2 - <18 yo)<br><br><a href="https://www.who.int/childgrowth/standards/weight_for_age/en/" target="_blank">WHO - Age 0 to 10 yo</a> (better granularity for neonates to toddlers)<br><br>Note: The covariates will replace whatever it is defined in $PARAM'
#' @export
bspop_seed            <- 'Seed number is applicable to both the sampling of databases (including covariates) AND simulations (uses current date as default value).'
#' @export
bspop_select_y_value_threshold <- 'The percentage (%) of subjects above the selected y-value at a certain time will be displayed below.'
#' @export
age_range_label       <- "Age Range ('AGE', y) [Post-Natal]"
#' @export
weight_range_label    <- "Weight Range ('WT', kg) [Total Body Mass]"
#' @export
male_range_label      <- "Male Percentage ('SEX = 0', %) [Biological]"
#' @export
custom_cov_label      <- "Covariate Name"
#' @export
custom_cov_placeholder<- "E.g. EGFR"
#' @export
bspop_custom_cov      <- "Covariate names must match the corresponding parameter name in the model code (i.e. inside $PARAM) for it to take effect.<br><br>E.g. If EGFR is defined inside $PARAM in the model code, the covariate name should also be called EGFR to replace the pre-defined parameter value.<br><br>Note: name cannot be AGE, AGEMO, SEX, WT, BMI, BSA, and must be unique from each other."
#' @export
custom_cov_dist_lab   <- "Select Distribution"
#' @export
bspop_cov_mean        <- "Mean value of the covariate to be used for a normal distribution."
#' @export
bspop_cov_meanlog     <- "Mean value of the covariate to be used for a log-normal distribution. Log of this value will be taken (cannot be < 0)."
#' @export
bspop_cov_sd          <- "Note that standard deviations cannot be negative."
#' @export
bspop_cov_sdlog       <- "Log of this value will be taken (cannot be < 1)."
#' @export
bspop_catvalue1       <- "The percentage slider determines the approximate proportion of subjects with this numeric value."
#' @export
bspop_catvalue2       <- "(100 - percentage slider) determines the approximate proportion of subjects with this numeric value."
#' @export
bspop_do_iiv_plotly   <- 'Interactive plots using the "plotly" package could take a long time for simulations with more than a few hundred subjects.'#<br><br>Note - "Download Options" do not apply when plots are not interactive.'
#' @export
show_mean_iiv_label   <- 'Show Mean Trend'
#' @export
bspop_select_time_interval_exp <- "(Optional) Select the desired time intervals to derive metrics to be plotted above. The original time scale is provided for selection, independent of whether any scaling of the x-axis from the Simulation page was performed.<br><br>Note: The time interval is inclusive on both ends (i.e. start time <= time interval <= end time)."

#infoBox
#' @export
infoBox_width         <- 3
#' @export
font_size             <- 'font-size: 23px;'

## css formatting
#' @export
bi_logo <- htmltools::img(src = "BI_Logo_Green.png",
                          style = "float:left; margin-top: -5px; padding-right:20px",
                          width = 120
)

#' @export
navbar_bg_color <-   htmltools::tags$head(
  htmltools::tags$style(
    htmltools::HTML('.navbar-static-top {background-color: #08312A;}',
                    '.navbar-default .navbar-nav>.active>a {background-color: #08312A;}',
                    '.navbar-default .navbar-nav>li>a:hover {background-color: transparent;}',
                    # '#param_output_model_1 {font-size: 10px; }', 
                    # '#param_output_model_1 {font-size: 12px; }',
                    '#console_output_model_1 { font-size: 10px; }',
                    '#console_output_model_2 { font-size: 10px; }',
                    '#console_output_iiv_model_1 { font-size: 10px; }',
                    '#console_output_iiv_model_2 { font-size: 10px; }',
                    '#console_data_1 { font-size: 10px; }',
                    '.popover {
             max-width: 500px;
             height: auto;
             overflow-y: auto !important; /* Add a scrollbar if the content exceeds the max-height */
          }'
    )
  )
)

#' @export
shinydashboard_box_format <- htmltools::tags$style(htmltools::HTML("
                                         .box.box-solid.box-primary>.box-header {
                                            color:#fff;
                                            background:#08312A
                                                              }
                                          
                                          .box.box-solid.box-primary{
                                          border-bottom-color:#08312A;
                                          border-left-color:#08312A;
                                          border-right-color:#08312A;
                                          border-top-color:#08312A;
                                          }
                                          
                                          .popover{
                                          width:400px;
                                          height:270px;   
                                          }
                                                         ")
)

