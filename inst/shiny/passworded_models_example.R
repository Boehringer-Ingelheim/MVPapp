#-------------------------------------------------------------------------------
#' Passworded models and functions required for the app to update the list of
#' model choices by password input
#'
#' Usage:
#'
#' 1. Add in the new model's display name inside the 'model_switch_conditions'
#'    function. Note that the model's internal R name should match with the
#'    eventual model code. (see Step 3)
#'
#' 2. Add in a conditional statement with the desired password inside the
#'    'update_model_choices' function. See other examples for an idea of how
#'    to update default values in the App.
#'
#'    Note: Add in your details to the create_alert() to have the Disclaimer warning
#'    shown every time.
#'
#'    Remember to also include your new password to the 'list_of_valid_passwords'
#'    inside the function.
#'
#' 3. Define the model code at the bottom of the script. Check earlier examples
#'    regarding formatting, including of the 'code_preamble' boilerplate text.
#'    Note: some minor adjustments in the code may be required to remove R code
#'    calls in quotation marks (e.g. calling Sys.date() etc)
#'
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#' @name model_switch_conditions
#' @title shorthand function to switch model code based on
#'                           choice of model input, and append model name
#'
#' @param input_model_select       From the UI (input$model_select, input$model_select2)
#' @param mcode_model_choice       What to append at the end of the model code
#'
#' @returns a switch for 'model_input' / 'model_input2'
#'
#' @note
#' This function is re-defined here to include all the hidden models
#'
#-------------------------------------------------------------------------------

model_switch_conditions <- function(input_model_select, mcode_model_choice) {
  return(switch(
    input_model_select,
    'Test Passworded Model'                               = paste0(one_cmt_abs, mcode_model_choice),
    ###### Password gated models above #######################################
    '1 Compartment PK'                                    = paste0(one_cmt, mcode_model_choice),
    '1 Compartment PK with Lag time'                      = paste0(one_cmt_lag, mcode_model_choice),
    '1 Compartment PK with Absorption Compartment'        = paste0(one_cmt_abs, mcode_model_choice),
    '1 Compartment PK (Transit Absorption)'               = paste0(one_cmt_transit, mcode_model_choice),
    '2 Compartment PK'                                    = paste0(two_cmt, mcode_model_choice),
    '2 Compartment PK with Absorption Compartment'        = paste0(two_cmt_abs, mcode_model_choice),
    '2 Compartment PK (Michaelis-Menten)'                 = paste0(model_code_2cmt_depot_MM, mcode_model_choice),
    '2 Compartment PK (Target Mediated Drug Disposition)' = paste0(model_code_2cmt_TMDD, mcode_model_choice),
    '2 Compartment PK (Quasi Equilibrium TMDD)'           = paste0(model_code_2cmt_QE_TMDD, mcode_model_choice),
    '2 Compartment PK (Michaelis-Menten TMDD)'            = paste0(model_code_2cmt_MM_TMDD, mcode_model_choice),
    '2 Compartment PK/PD (Indirect Effect)'               = paste0(pkpd_te, mcode_model_choice),
    'PK Tumor Growth Inhibition Model'                    = paste0(pk_tgi, mcode_model_choice),
    'KPD Tumor Growth Inhibition Model'                   = paste0(pd_gompertz_effect, mcode_model_choice),
    'Time To Event Model (Gompertz)'                      = paste0(pk_tte_gompertz, mcode_model_choice),
    'Time To Event Model (Weibull)'                       = paste0(pk_tte_weibull, mcode_model_choice),
    'Time To Event Model (Log-logistic)'                  = paste0(pk_tte_loglogistic, mcode_model_choice),
    'PKPD with Adaptive Dosing Regimen'                   = paste0(pkpd_adaptive_dosing, mcode_model_choice),
    'PK with Parallel Zero and First-order Absorption'    = paste0(pk_par_first_order, mcode_model_choice),
    'PK with Adaptive Dosing Interval'                    = paste0(pk_adaptive_dosing_int, mcode_model_choice),
    'PK with Sequential Zero and First-order Absorption'  = paste0(pk_seq_first_order, mcode_model_choice),
    #'------------------------------------------'
    'Mrgsolve internal model library (modlib())'          = modlib_examples,
    'Blank Template'                                      = paste0(blank_template, mcode_model_choice),
    'Upload .cpp File'                                    = paste0(blank_template, mcode_model_choice)
  ))
}

#-------------------------------------------------------------------------------
#' @name update_model_choices
#'
#' @title Shorthand function to update list of models and
#'                        default values by input password
#'
#' @param input_password        From the UI (input$password)
#' @param session               Shiny session (don't change this)
#' @param model_list            List of models (don't change this)
#'
#' input$model_select & input$model_select2 are updated as a side effect
#'
#' @returns a logical TRUE/FALSE if password is valid
#'
#' @note
#' This function is re-defined here to include all the model defaults
#-------------------------------------------------------------------------------

update_model_choices <- function(input_password, session, model_list = model_examples_list) {

  new_choices <- model_list
  list_of_valid_passwords <- c("test")

  ## Begin passworded model settings, insert new model settings below

  ### Example model
  if (input_password == "test") {
    create_alert("FirstName LastName", "first.last@xxx.com")
    new_choices <- c('Test Passworded Model', model_examples_list)
    updateNumericInput(session, "mw", value = 12345)
    updateCheckboxInput(session, "mw_checkbox", value = TRUE)
    updateNumericInput(session, "mw2", value = 12345)
    updateCheckboxInput(session, "mw_checkbox2", value = TRUE)
    updateNumericInput(session, "tgrid_max", value = 2016)
    updateNumericInput(session, "delta", value = 2)
    updateNumericInput(session, "total1", value = 3)
    updateNumericInput(session, "amt1", value = 2500)
    updateNumericInput(session, "tota1", value = 3)
    updateNumericInput(session, "ii1", value = 168)
    updateNumericInput(session, "tinf1", value = 1)
    updateNumericInput(session, "amt2", value = 1125)
    updateNumericInput(session, "total2", value = 4)
    updateNumericInput(session, "ii22", value = 672)
    updateNumericInput(session, "tinf2", value = 1)
    updateNumericInput(session, "delay_time2", value = 1176)
    updateTextInput(session, "y_axis_label", value = "Concentration (nM)")
    updateSelectizeInput(session, "time_unit", selected = '168', options = list(create = TRUE))
  }

  ## End of passworded model settings

  # Update the selectInput with the new choices
  if(input_password %in% list_of_valid_passwords) {
    updateSelectInput(session, "model_select",  choices = new_choices)
    updateSelectInput(session, "model_select2", choices = new_choices)
  } else {
    updateSelectInput(session, "model_select",  choices = model_list)
    updateSelectInput(session, "model_select2", choices = model_list)
  }

  return(input_password %in% list_of_valid_passwords)
}

### Password models ###

one_cmt_abs <- paste0(code_preamble, '

"
$Global

$Prob
- 1 Compartment model + Absorption CMT w/ Weight effect on PK parameters

$CMT  @annotated
ABS  : Absorption compartment
CENT : Central compartment (mg)

$PARAM @annotated
KA   :  1   : Absorption rate constant (1/time)
CL   :  2   : Clearance (volume/time)
V    : 20   : Central volume (volume)
WT   : 70   : Weight (kg)
WTCL : 0.75 : Exponent of weight effect on CL
WTV  : 1.0  : Exponent of weight effect on V
F    : 1.0  : Bioavailability (fraction)

$MAIN
double KAVAR = KA * exp(EKA);
double VVAR  = V * exp(EV);
double CLVAR = CL * exp(ECL);
double CLCOV = CLVAR*pow(WT/70, WTCL);
double VCOV  = VVAR*pow(WT/70, WTV);
double K20   = CLCOV/VCOV;

$ODE
dxdt_ABS  = -KAVAR*ABS;
dxdt_CENT = KAVAR*ABS*F -K20*CENT;

$OMEGA @annotated @block
EKA : 0.09 : ETA on Absorption rate
ECL : 0.01 0.09 : ETA on CL
EV  : 0.01 0.02 0.09 : ETA on V

$SIGMA @annotated
PROP: 0.1 : Proportional residual error

$TABLE
double DV = (CENT/VCOV)*(1+PROP);

//prevent simulation of negative concentrations
int i = 0;
while(DV <0 && i < 100){
    simeps();
    DV = (CENT/VCOV)*(1+PROP);
    ++i;
}

$CAPTURE
DV
"
', code_postamble)


### End of Password-gated Models #####
