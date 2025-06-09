#-------------------------------------------------------------------------------
#' Code editor preamble and postambles
#' 
#-------------------------------------------------------------------------------

#' @export
code_preamble  <- "## Notes: mrgsolve [v1.5.2] (https://mrgsolve.org/user-guide/) syntax is required.\n##      : All parameters inside $PARAM will be dynamically generated.\n##      : Changes to values above will not be reflected back in the code editor.\n##      : All sources of variability will be ignored (see Variability tab).\n##      : Model code must be enclosed in quotation marks and assigned to the 'model_code' object.\n\nmodel_code <- "
#' @export
cpp_preamble   <- "## Notes: mrgsolve [v1.5.2] (https://mrgsolve.org/user-guide/) syntax is required.\n##      : All parameters inside $PARAM will be dynamically generated.\n##      : Changes to values above will not be reflected back in the code editor.\n##      : All sources of variability will be ignored (see Variability tab).\n##      : Model code must be enclosed in quotation marks and assigned to the 'model_code' object.\n##      :\n##      : ESCAPE (\\) OR REMOVE ALL SINGLE / DOUBLE QUOTATION MARKS IN .CPP FILE!!\n\nmodel_code <- "
#' @export
code_postamble <- "\n## IMPORTANT: Model 1 and Model 2 must have different 'model_object' names!\n"

#-------------------------------------------------------------------------------
#' List of template models available for all users
#' 
#-------------------------------------------------------------------------------

#' @export
model_examples_list <- c('1 Compartment PK',
                         '1 Compartment PK with Lag time',
                         '1 Compartment PK with Absorption Compartment',
                         '1 Compartment PK (Transit Absorption)',
                         '2 Compartment PK', 
                         '2 Compartment PK with Absorption Compartment',
                         '2 Compartment PK (Michaelis-Menten)',
                         '2 Compartment PK (Target Mediated Drug Disposition)',
                         '2 Compartment PK (Quasi Equilibrium TMDD)',
                         '2 Compartment PK (Michaelis-Menten TMDD)',
                         '2 Compartment PK/PD (Indirect Effect)',
                         'PK Tumor Growth Inhibition Model',
                         'KPD Tumor Growth Inhibition Model',
                         'Time To Event Model (Gompertz)',
                         'Time To Event Model (Weibull)',
                         'Time To Event Model (Log-logistic)',
                         'PKPD with Adaptive Dosing Regimen',
                         'PK with Adaptive Dosing Interval',
                         'PK with Parallel Zero and First-order Absorption',
                         'PK with Sequential Zero and First-order Absorption',
                         '--------------------------------------------',
                         'Mrgsolve internal model library (modlib())',
                         'Blank Template',
                         'Upload .cpp File')

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
#' This function will be re-defined if passworded models is sourced externally,
#' i.e. when provided as an argument for run_mvp(pw_models_path = "path/to/your/private/models.R").
#' 
#' An example passworded model called "Test Passworded Model" is included by default as an example,
#' which can be unlocked by using the password "test".
#'                                                 
#' @export
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
    'PK with Adaptive Dosing Interval'                    = paste0(pk_adaptive_dosing_int, mcode_model_choice),
    'PK with Parallel Zero and First-order Absorption'    = paste0(pk_par_first_order, mcode_model_choice),
    'PK with Sequential Zero and First-order Absorption'  = paste0(pk_seq_first_order, mcode_model_choice),
    #####--------------------------------------------',
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
#' *Note* this function will be re-defined if passworded models is sourced externally,
#'        i.e. when provided as an argument for run_mvp(pw_models_path = "path/to/your/private/models.R")
#' @export
update_model_choices <- function(input_password, session, model_list = model_examples_list) {
  
  new_choices <- model_list
  list_of_valid_passwords <- c("test")
  
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

#-------------------------------------------------------------------------------
# Code Templates 
#' 
#-------------------------------------------------------------------------------

#' @export
one_cmt <- paste0(code_preamble, '

"
$Global

$Prob
- 1 Compartment model w/ Weight effect on PK parameters

$CMT  @annotated
CENT : Central compartment (mg)

$PARAM @annotated
CL   :  2   : Clearance (volume/time)
V    : 20   : Central volume (volume)
WT   : 70   : Weight (kg)
WTCL : 0.75 : Exponent of weight effect on CL
WTV  : 1.0  : Exponent of weight effect on V

$MAIN
double CLVAR = CL * exp(ECL);
double VVAR  = V  * exp(EV);
double CLCOV = CLVAR*pow(WT/70, WTCL);
double VCOV  = VVAR*pow(WT/70, WTV);
double K20   = CLCOV/VCOV;

$ODE
dxdt_CENT = -K20*CENT;

$OMEGA @annotated
ECL: 0.09 : ETA on clearance
EV : 0.09 : ETA on volume

$SIGMA @annotated
PROP: 0.1  : Proportional residual error
ADD : 0    : Additive residual error

$TABLE
double DV = (CENT/VCOV)*(1+PROP) + ADD;

//prevent simulation of negative concentrations
int i = 0;
while(DV <0 && i < 100){
    simeps();
    DV = (CENT/VCOV)*(1+PROP) + ADD;
    ++i;
}

$CAPTURE
DV
"
', code_postamble)

#' @export
one_cmt_lag <- paste0(code_preamble, '

"
$Global

$Prob
- 1 Compartment model + Absorption CMT w/ Weight effect on PK parameters (Lag time)

$CMT  @annotated
GUT  : Absorption compartment
CENT : Central compartment (mg)

$PARAM @annotated
TVKA   :  1   : Absorption rate constant (1/time)
TVCL   :  2   : Clearance (volume/time)
TVV    : 20   : Central volume (volume)
WT     : 70   : Weight (kg)
WTCL   : 0.75 : Exponent of weight effect on CL
WTV    : 1.0  : Exponent of weight effect on V
F1     : 1.0  : Bioavailability (fraction)
ALAG   : 1.5  : Lag time (hours)

$MAIN
double KA = TVKA * exp(EKA);
double V  = TVV * pow(WT/70, WTV) * exp(EV);
double CL = TVCL * pow(WT/70, WTCL)* exp(ECL);
double K20 = CL/V;

F_GUT    = F1;
ALAG_GUT = ALAG * exp(ELAG);

$ODE
dxdt_GUT  = -KA*GUT;
dxdt_CENT = KA*GUT -K20*CENT;

$OMEGA @annotated @block
EKA : 0.09 : ETA on Absorption rate
ECL : 0.01 0.09 : ETA on CL
EV  : 0.01 0.02 0.09 : ETA on V

$OMEGA @annotated
ELAG : 0.09 : ETA on lag time

$SIGMA @annotated
PROP: 0.1 : Proportional residual error

$TABLE
double DV = (CENT/V)*(1+PROP);

//prevent simulation of negative concentrations
int i = 0;
while(DV <0 && i < 100){
    simeps();
    DV = (CENT/V)*(1+PROP);
    ++i;
}

$CAPTURE
DV
"
', code_postamble)


#' @export
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

#' @export
two_cmt  <- paste0(code_preamble, '

"
$Global

$Prob
- 2 Compartment model w/ Weight effect on PK parameters

$CMT  @annotated
CENT : Central compartment (mg)
PERI : Peripheral (mg)

$PARAM @annotated
CL   :  2   : Clearance (volume/time)
VC   : 20   : Central volume (volume)
VP   :  2   : Peripheral volume (volume)
Q    :  1   : Intercompartmental clearance (volume/time)
WT   : 70   : Weight (kg)
WTCL : 0.75 : Exponent of weight effect on CL
WTV  : 1.0  : Exponent of weight effect on V

$MAIN
double CLCOV = (CL * exp(ECL))*pow(WT/70, WTCL);
double VCCOV = (VC * exp(EVC))*pow(WT/70, WTV);
double QCOV  = (Q * exp(EQ))*pow(WT/70, WTCL);
double VPCOV = (VP * exp(EVP))*pow(WT/70, WTV);
double K20   = CLCOV/VCCOV;
double K23   = QCOV/VCCOV;
double K32   = QCOV/VPCOV;
 
$ODE
dxdt_CENT = - K20*CENT - K23*CENT + K32*PERI;
dxdt_PERI = K23*CENT - K32*PERI;

$OMEGA @annotated @block
ECL : 0.09 : ETA on CL
EVC : 0.02 0.09 : ETA on VC
$OMEGA @annotated
EVP : 0 : ETA on VP
EQ  : 0 : ETA on Q

$SIGMA @annotated
PROP: 0.1 : Proportional residual error
ADD : 0   : Additive residual error

$TABLE
double CP = (CENT/VCCOV) * (1 + PROP) + ADD;
double CT = (PERI/VPCOV);

//prevent simulation of negative concentrations
int i = 0;
while(CP <0 && i < 100){
    simeps();
    CP = (CENT/VCCOV) * (1 + PROP) + ADD;
    ++i;
}

$capture
CP CT
"
', code_postamble)

#' @export
two_cmt_abs  <- paste0(code_preamble, '

"
$Global

$Prob
- 2 Compartment model + Absorption CMT w/ Weight effect on PK parameters

$CMT  @annotated
ABS  : Absorption compartment
CENT : Central compartment (mg)
PERI : Peripheral (mg)

$PARAM @annotated
KA   :  1   : Absorption rate constant (1/time)
CL   :  2   : Clearance (volume/time)
VC   : 20   : Central volume (volume)
VP   :  2   : Peripheral volume (volume)
Q    :  1   : Intercompartmental clearance (volume/time)
WT   : 70   : Weight (kg)
WTCL : 0.75 : Exponent of weight effect on CL
WTV  : 1.0  : Exponent of weight effect on V
F    : 1.0  : Bioavailability (fraction)

$MAIN
double KAVAR = KA * exp(EKA);
double CLCOV = (CL * exp(ECL))*pow(WT/70, WTCL);
double VCCOV = (VC * exp(EVC))*pow(WT/70, WTV);
double QCOV  = (Q * exp(EQ))*pow(WT/70, WTCL);
double VPCOV = (VP * exp(EVP))*pow(WT/70, WTV);
double K20   = CLCOV/VCCOV;
double K23   = QCOV/VCCOV;
double K32   = QCOV/VPCOV;
 
$ODE
dxdt_ABS  = -KAVAR*ABS;
dxdt_CENT = KAVAR*ABS*F - K20*CENT - K23*CENT + K32*PERI;
dxdt_PERI = K23*CENT - K32*PERI;

$OMEGA @annotated @block
EKA :  0.09 : ETA on KA
ECL :  0.01 0.09 : ETA on CL
EVC :  0.01 0.02 0.09 : ETA on VC
$OMEGA @annotated
EVP : 0 : ETA on VP
EQ  : 0 : ETA on Q

$SIGMA @annotated
PROP: 0.1 : Proportional residual error
ADD : 0   : Additive residual error

$TABLE
double CP = (CENT/VCCOV) * (1 + PROP) + ADD;
double CT = (PERI/VPCOV);

//prevent simulation of negative concentrations
int i = 0;
while(CP <0 && i < 100){
    simeps();
    CP = (CENT/VCCOV) * (1 + PROP) + ADD;
    ++i;
}

$CAPTURE
CP CT
"
', code_postamble)

#' @export
model_code_2cmt_depot_MM <- paste0(code_preamble, '

"
$Global

$Prob
- 2 Compartment model with Michaelis-Menten elimination

$CMT  @annotated
EV   : Extravascular compartment (mg)
CENT : Central compartment (mg)
PERI : Peripheral (mg)

$PARAM @annotated
VC   :  2 : Central volume (volume)
VP   : 20 : Peripheral volume (volume)
Q    :  2 : Intercompartmental clearance (volume/time)
KA1  :  1 : Absorption rate constant (1/time)
Km   :  1 : Michaelis constant (mass/volume) 
Vmax :  3 : Maximum rate of metabolism (mass/time)

$MAIN
double KA      = KA1 * exp(EKA1);
double QVAR    = Q  * exp(EQ);
double VCVAR   = VC * exp(EVC);
double VPVAR   = VP * exp(EVP);
double K23     = QVAR/VCVAR;
double K32     = QVAR/VPVAR;
double VmaxVAR = Vmax * exp(EVmax);
double KmVAR   = Km * exp(EKm);
double C2      = CENT/VCVAR;
 
$ODE
dxdt_EV   = -KA*EV;
dxdt_CENT =  KA*EV + K32*PERI - (VmaxVAR * C2)/(KmVAR + C2) - K23*CENT;
dxdt_PERI =  K23*CENT - K32*PERI;

$OMEGA @annotated 
EKA1 :  0.09  : ETA on KA1
EVC  :  0.09  : ETA on VC
EVP  :  0     : ETA on VP
EQ   :  0     : ETA on Q
EKm  :  0.09  : ETA on Km
EVmax:  0.09  : ETA on Vmax

$SIGMA @annotated
PROP: 0.1 : Proportional residual error
ADD : 0   : Additive residual error

$TABLE
double CP = (CENT/VCVAR) * (1 + PROP) + ADD;
double CT = (PERI/VPVAR);

//prevent simulation of negative concentrations
int i = 0;
while(CP <0 && i < 100){
    simeps();
    CP = (CENT/VCVAR) * (1 + PROP) + ADD;
    ++i;
}

$CAPTURE
CP CT
"
', code_postamble)

#' @export
model_code_2cmt_TMDD <- paste0(code_preamble, '

"
$Global

$Prob
- 2 Compartment model with Target Mediated Drug Disposition (TMDD)

$CMT @annotated
DEPOT     : Subcutaneous adminstration compartment (nmol)
L         : Free drug in central compartment (nmol/L)
LPERI     : Drug in peripheral compartment (nmol/L)
FreeR     : Free target (nmol/L)
RL        : Drug-target complex (nmol/L)

$PARAM @annotated
// PK parameters;
TVV     :   3.5      :  Central Volume (L) 
TVCL    :   0.07     :  Clearance (L/h)
TVV2    :   7        :  Peripheral Volume (L) Not used
TVQ     :   0.01     :  Intercompartmental clearance (L/h)
TVKA    :   0.01     :  Absorption rate constant (1/h) 
TVF     :   0.7      :  Fraction Relative Bioavailability

// Receptor parameters;
TVBASE  :  10        :  Target baseline (nmol/L)
TVKDEG  :   0.01     :  Elimination rate constant of free target (1/h)

// Drug-Receptor Complex parameters;
TVKON   :   1        :  Binding rate constant (L/(nmol*h))
TVKOFF  :   0.1      :  Dissociation rate constant (1/h)
TVKINT  :   0.01     :  Elimination/internalization rate constant of complex (1/h)

$MAIN
double V     =  TVV * exp(EV);
double CL    =  TVCL * exp(ECL);
double V2    =  TVV2;
double KA    =  TVKA * exp(EKA);
double F     =  TVF;
double Q     =  TVQ;
double BASE  =  TVBASE * exp(EBASE);
double KDEG  =  TVKDEG * exp(EKDEG);
double KSYN  =  KDEG * BASE;
double KON   =  TVKON * exp(EKON);
double KOFF  =  TVKOFF * exp(EKOFF);
double KINT  =  TVKINT * exp(EKINT);

FreeR_0      =  BASE; 
F_L          =  1/V;
F_DEPOT      =  F;

$ODE

dxdt_DEPOT   = -KA*DEPOT              ; // Depot SC (nmol)
dxdt_L       =  KA*DEPOT/V - (CL/V)*L - (Q/V)*(L-LPERI) - KON*FreeR*L + KOFF*RL ; // Free drug in central compartment (nmol/L)
dxdt_LPERI   =                            (Q/V2)*(L-LPERI) ; // Drug in peripheral compartment (nmol/L)
dxdt_FreeR   =  KSYN - KDEG*FreeR - KON*FreeR*L + KOFF*RL ; // Free target (nmol/L)
dxdt_RL      =  KON*FreeR*L - (KINT+KOFF)*RL ; // Drug-target complex (nmol/L)

$OMEGA @annotated
EV    : 0.09 : ETA on V
ECL   : 0.09 : ETA on CL
EKA   : 0.09 : ETA on KA
EBASE : 0.09 : ETA on BASE
EKDEG : 0.09 : ETA on KDEG
EKON  : 0.09 : ETA on KON
EKOFF : 0.09 : ETA on KOFF
EKINT : 0.09 : ETA on KINT

$SIGMA @annotated
PROP : 0.1 : Prop RUV
ADD  : 0   : Add RUV

$TABLE
double PK        = L * (1 + PROP) + ADD;

//prevent simulation of negative concentrations
int i = 0;
while(PK <0 && i < 100){
    simeps();
    PK = L * (1 + PROP) + ADD;
    ++i;
}

double LTOT = L + RL; // Total drug in central compartment (nmol/L)
double RTOT = FreeR + RL; // Total target (nmol/L)
double RO   = (RL / RTOT) * 100;   // Receptor occupancy (%)

$CAPTURE
PK LTOT RTOT RO 
"
', code_postamble)

#' @export
blank_template <- paste0(code_preamble, '

"
$Global

$Prob
- Template (1 Compartment model)

$CMT  @annotated
CENT : Central compartment (mg)

$PARAM @annotated
CL   :  2   : Clearance (volume/time)
V    : 20   : Central volume (volume)

$MAIN
double CLVAR = CL * exp(ECL);
double VVAR  = V * exp(EV);
double K20   = CLVAR / VVAR;

$ODE
dxdt_CENT = -K20*CENT;

$OMEGA @annotated
ECL   : 0.09 : ETA on CL
EV    : 0.09 : ETA on V

$SIGMA @annotated
PROP: 0.1  : Proportional residual error
ADD : 0    : Additive residual error

$TABLE
double DV = (CENT/VVAR)*(1+PROP) + ADD;

//prevent simulation of negative concentrations
int i = 0;
while(DV <0 && i < 100){
    simeps();
    DV = (CENT/VVAR)*(1+PROP) + ADD;
    ++i;
}

$CAPTURE
DV
"
', code_postamble)

#' @export
pk_tgi <- paste0(code_preamble, '

"
$PROB 
PK Tumor Growth Inhibition Model 

$CMT @annotated 

SLD   : When Emax ER, Tumor measurement (Sum of longest diameter of target lesions, mm) 

$PARAM @annotated
pBSmm    : 50        :  Tumor volume at baseline (mm)
pKGd     : 0.002     :  Tumor intrinsic growth rate (/day)
pKDd     : 0.01      :  Tumor death rate (/day)
pRHOd    : 0.015     :  Resistance (/day)
pPKxx    : 1         :  Normalised PK exposure
pBSmm_NN : 120       :  Coefficient (Non-NSCLC baseline) (mm)
pLUNG    : 1         :  NSCLC flag (1 = yes, 0 = No)

$MAIN
double BSmm  = (pBSmm*pLUNG + pBSmm_NN*(1-pLUNG));
double KGd   = pKGd;
double KGhr  = KGd/24;
double KDd   = pKDd;
double KDhr  = KDd/24;
double RHOd  = pRHOd;
double RHOhr = RHOd/24;
double NDRUG = pPKxx;

SLD_0 = BSmm;

$ODE

dxdt_SLD   = KGhr*SLD - KDhr*SLD*exp(- RHOhr*SOLVERTIME)*log(1 + NDRUG); 

double T_SLD = SLD;

$TABLE

capture Pop_SLD = T_SLD;
capture Ind_SLD = Pop_SLD;

"
', code_postamble)

#' @export
pk_tte_gompertz <- paste0(code_preamble, '

"
$PROB 
- Time To Event model (Gompertz)
- Dose amounts should be 0.

$CMT @annotated 
HZ   : Hazard

$PARAM @annotated
pHZ0     : 0.0002       :  Base hazard
pHZ1     : -0.001       :  Base hazard 2 
pPKXX    : 0            :  Used PK metric
pBETA1   : 1.5      	  :  Covariate coefficient 1

$MAIN
double HZ0     = pHZ0;
double HZ1     = pHZ1;
double PKXX    = pPKXX;
double Norm_PK = PKXX/25000;
double BETA1   = pBETA1; 

$ODE
double cov1  = exp(BETA1*log(1 + Norm_PK));
dxdt_HZ      = HZ0*exp(HZ1*SOLVERTIME)*cov1; 
double CUMHZ = HZ;
double SURV  = exp(-CUMHZ);

$TABLE

capture Pred_surv = SURV;

"
', code_postamble)

#' @export
pk_tte_weibull <- paste0(code_preamble, '

"  
$PROB 
- Time To Event model (Weibull)
- Dose amounts should be 0.

$CMT @annotated 

HZ   : Hazard

$PARAM @annotated
pHZ0     : 0.001 :  Base hazard (only positive)
pHZ1     : 0.01  :  Base hazard 2 (negative or positive) 
pPKXX    : 1     :  Drug exposure / PK metric
pBETA1   : 0.02  :  Covariate coefficient 1

$MAIN

double HZ0     = pHZ0;
double HZ1     = pHZ1;
double PKXX    = pPKXX*exp(ePKXX);
double Norm_PK = PKXX/45000;
double BETA1   = pBETA1; 


$ODE
double del    = 0.000001;
double cov1   = exp(BETA1*log(1 + Norm_PK));
dxdt_HZ       = HZ0*exp(HZ1*log(del+SOLVERTIME))*cov1; 
double CUMHZ  = HZ;
double SURV   = exp(-CUMHZ);

$OMEGA @annotated 
ePKXX    : 0    : ETA on PK metric

$SIGMA @annotated  
EPS1   : 0     : add. error

$TABLE

capture Pred_surv = SURV + EPS1;

int i = 0;
while(HZ <0 && i < 200){
    simeps();
    CUMHZ = HZ + EPS1;
    SURV = exp(-CUMHZ);
    Pred_surv = exp(-CUMHZ) ;
    ++i;
}

"
', code_postamble)

#' @export
pk_tte_loglogistic <- paste(code_preamble, '

"
$PROB 
- Time To Event model (Log-logistic)
- Dose amounts should be 0.

$CMT @annotated 

HZ   : Hazard

$PARAM @annotated
pHZ0     : 0.001 :  Base hazard (must be positive)
pHZ1     : 2     :  Base hazard 2 (must be positive)
pPKXX    : 1     :  Drug exposure / PK metric
pBETA1   : 0.01  :  Covariate coefficient 1

$MAIN

double del     = 0.0000001; 
double HZ0     = pHZ0;
double HZ1     = pHZ1;
double PKXX    = pPKXX*exp(ePKXX);
double Norm_PK = PKXX/45000;
double BETA1   = pBETA1; 


$ODE

double cov1      = exp(BETA1*log(1 + Norm_PK));
dxdt_HZ   = pow(HZ0*HZ1*(HZ0*(SOLVERTIME + del)),(HZ1 - 1))/pow(1 + (HZ0*(SOLVERTIME + del)),HZ1);

double CUMHZ = HZ;
double SURV = exp(-CUMHZ);

$OMEGA @annotated 
ePKXX    : 0    : ETA on PK metric 

$SIGMA @annotated  
EPS1   : 0     : add. error

$TABLE

capture Pred_surv = SURV + EPS1;

int i = 0;
while(HZ <0 && i < 200){
  simeps();
  CUMHZ = HZ + EPS1;
  SURV = exp(-CUMHZ);
  Pred_surv = exp(-CUMHZ) ;
  ++i;
}

"
', code_postamble)

#' @export
pkpd_te <- paste0(code_preamble, '

"
$PROB
# Model: `XXXXXX Preclinical PK/TE model`
  - Two-compartment linear PK model, allometric scaling
  - Indirect effect PD model (inhibit XXX KIN)
  
$PARAM @ annotated
TVF1       : 0.5       : Bioavailability (-)
TVKA       : 0.6       : Absorption rate constant (1/h)
TVCL       : 0.2       : Systemic clearance (L/h/kg)
TVVC       : 0.3       : Central volume (L/kg)
TVQ        : 0.1       : Intercompartmental clearance (L/h/kg)
TVVP       : 0.4       : Peripheral volume (L/kg)
WT         : 70        : Baseline total body weight (kg)
TVIC50     : 20        : XXXXXX concentration producing half IMAX (nM)
TVKOUT     : 0.3       : Turnover rate constant for XXX (1/h)
KIN_KO     : 0.4       : Production rate constant for XXX in knockout mice (XXX units/h)
KIN_WT     : 0.1       : Production rate constant for XXX in wild-type mice (XXX units/h)
KO_FLAG    : 0         : Flag for knockout vs. wild-type KIN (1=Y / 0=N)

$CMT @ annotated
GUT    : Depot
CENT   : Central
PERI   : Peripheral
AUC    : Plasma AUC
PD     : XXX Gene Score
PDAUC  : AUC of PD
  
$SET
delta = 0.1

$MAIN

double F1   = TVF1;
F_GUT       = F1;

double KA   = TVKA                           * exp(ETA(1));
double CL   = TVCL * WT * pow((WT/70), 0.75) * exp(ETA(2)); 
double VC   = TVVC * WT * pow((WT/70), 1)    * exp(ETA(3));
double Q    = TVQ  * WT * pow((WT/70), 0.75) * exp(ETA(4));
double VP   = TVVP * WT * pow((WT/70), 1)    * exp(ETA(5));
double IC50 = TVIC50                         * exp(ETA(6));
double KOUT = TVKOUT                         * exp(ETA(7));

double KIN     = KIN_KO * KO_FLAG + KIN_WT * (1 - KO_FLAG);
double BASE_WT = KIN_WT/KOUT;
double BASE_KO = KIN_KO/KOUT;
double IMAX    = (1 - (BASE_WT/BASE_KO)); 

PD_0 = KIN/KOUT;

$OMEGA @annotated
EKA     : 0.09     : ETA on KA
ECL     : 0.09     : ETA on CL
EVC     : 0.09     : ETA on VC
EQ      : 0        : ETA on Q
EVP     : 0        : ETA on VP
EIC50   : 0.09     : ETA on IC50
EKOUT   : 0.09     : ETA on KOUT

$SIGMA @ annotated
RVPK    : 0     : RUV PK
RVPD    : 0     : RUV PD

$ODE
double KEL   = CL/VC;
double K12   = Q/VC;
double K21   = Q/VP;
double CP    = CENT/VC;
double REDUC = 100 * (BASE_KO - PD)/(BASE_KO - BASE_WT);

dxdt_GUT     = - KA * GUT;
dxdt_CENT    = KA * GUT - KEL * CENT - K12 * CENT + K21 * PERI;
dxdt_PERI    = K12 * CENT - K21 * PERI;
dxdt_AUC     = CP;
dxdt_PD      = KIN * (1 - IMAX * CP/(IC50 + CP)) - KOUT * PD;
dxdt_PDAUC   = PD;

$TABLE 
double IPRED_PK = CP;
double IPRED_PD = REDUC;

double DV_PK    = IPRED_PK * (1 + EPS(1));
double DV_PD    = IPRED_PD * (1 + EPS(2));

//prevent simulation of negative concentrations
int i = 0;
while(DV_PK <0 && i < 100){
	simeps();
	DV_PK = IPRED_PK * (1 + EPS(1));
	++i;
}

while(DV_PD <0 && i < 100){
	simeps();
	DV_PD = IPRED_PD * (1 + EPS(2)) + EPS(3);
	++i;
}

$CAPTURE
DV_PK DV_PD

"
', code_postamble)

#' @export
one_cmt_transit <- paste0(code_preamble, '

"
$PROB 
 - 1 Compartment model with Transit Absorption

$PARAM @ annotated
CL    : 1     : Clearance (L/h)
V     : 30    : Central Volume (L)
MTT   : 5     : Mean Transit Time (h)
NN    : 5     : Number of Transit Compartments
BIO   : 1     : Bioavailability (Fraction)

$CMT @ annotated
DEPOT : Depot
CENT  : Central

$GLOBAL 

int NDOSE = 0;
double dosetime[300];
double dose[300];

$MAIN 

if(NEWIND < 2) NDOSE = 0; 

if(self.amt > 0 && self.cmt==1) {
 NDOSE = NDOSE + 1; 
 dosetime[NDOSE] = self.time;
 dose[NDOSE] = self.amt;
}

F_DEPOT = 0; 
double VVAR = V * exp(EV);
double CLVAR= CL * exp(ECL);
double KTR  = ((NN * exp(ENN))+1) / (MTT * exp(EMTT)); 
double NFAC = exp(lgamma(NN+1));
double KINPT = BIO * pow(KTR,(NN+1)) / NFAC; 

$ODE

double INPT = 0;
int i = 0;
while(i <= NDOSE) {
  double IPT = 0;
  if(SOLVERTIME >= dosetime[i]) {
    double delta = SOLVERTIME - dosetime[i];
    IPT = dose[i] * pow(delta, NN) * exp(-KTR * delta);  
  }
  INPT = INPT + IPT;
  ++i;
}

dxdt_DEPOT = KINPT * INPT - KTR * DEPOT;
dxdt_CENT = KTR * DEPOT - (CLVAR/VVAR) * CENT;

$OMEGA @annotated
ECL  : 0.09 : ETA on clearance
EV   : 0.09 : ETA on volume
EMTT : 0.09 : ETA on MTT
ENN  : 0.09 : ETA on NN

$SIGMA @annotated
PROP: 0.1  : Proportional residual error
ADD : 0    : Additive residual error

$TABLE
capture DV = (CENT/VVAR)*(1+PROP) + ADD;

"
', code_postamble)

#' @export
modlib_examples <- 
'
## Notes: The models below are from the mrgsolve internal library:
##      : (https://mrgsolve.org/docs/reference/modlib.html)
##      :
##      : Pick and uncomment one line to directly read in the model object
##      : Check the Console info below to review model code.
##      :
##      : All parameters inside $PARAM will be dynamically generated.
##      : Changes to values above will not be reflected back in the code editor.
##      : All sources of variability will be ignored (see Variability tab).
                          
# model_object <- mread("pk1cmt",  modlib())
# model_object <- mread("pk2cmt",  modlib()) 
# model_object <- mread("pk3cmt",  modlib()) 
# model_object <- mread("pk1",     modlib()) # one compartment pk model in closed-form
# model_object <- mread("pk2",     modlib()) # two compartment pk model in closed-form
# model_object <- mread("popex",   modlib()) # a simple population pk model
# model_object <- mread("irm1",    modlib()) # inhibition of response production
# model_object <- mread("irm2",    modlib()) # inhibition of response loss
# model_object <- mread("irm3",    modlib()) # stimulation of response production
# model_object <- mread("irm4",    modlib()) # stimulation of response loss
# model_object <- mread("emax",    modlib()) # sigmoid emax model
# model_object <- mread("effect",  modlib()) # effect compartment model
# model_object <- mread("tmdd",    modlib()) # target-mediated drug disp.
# model_object <- mread("viral1",  modlib()) 
# model_object <- mread("viral2",  modlib()) 
# model_object <- mread("pred1",   modlib()) # $PRED syntax
# model_object <- mread("pbpk",    modlib())
# model_object <- mread("1005",    modlib()) # embedded NONMEM result
# model_object <- mread("nm-like", modlib()) # model with nonmem-like syntax


### IMPORTANT: Do not have same model for both Model 1 / Model 2 as internal model names cannot be identical!                    
'

#-------------------------------------------------------------------------------
#' PD model of gemcitabine, with gompertz and effect compartment
#' Tham LS, Holford NH, Wang L, Soo RA, Lee SC, Lee HS, et al. A pharmacodynamic model for the time course of tumor shrinkage by gemcitabine + carboplatin in non-small cell lung cancer patients. Clin Cancer Res. 2008;14(13):4213-8.
#' 
#-------------------------------------------------------------------------------

#' @export
pd_gompertz_effect <- paste0(code_preamble, '

"
$PROB 
 - KPD tumor growth inhibition model w/ gompertz and effect cmt.
 - Adapted from Tham LS et al., Clin Cancer Res. 2008;14(13):4213-8.

$CMT @ annotated
DRUG   : Amount of drug in effect compartment
TUMOR  : Tumor size

$PARAM @ annotated
BSIZE : 6.66  : Baseline tumor size (cm)
TEQ   : 7.67  : Teq, Drug equilibration half-life (week)
TURN  : 21.8  : Tumor turnover half-life (week)
E50   : 10600 : Drug conc. causing 50% tumor size decrease (mg)

$MAIN
double KOVER  = log(2)/(TURN * exp(ETURN)); //  1/(week)
double KEQ    = log(2)/(TEQ * exp(ETEQ));   // 1/(week)
double SIZE0  = BSIZE * exp(EBSIZE);
double RATEIN = SIZE0 * KOVER ; // cm/(week)
double AE50   = E50 * exp(EAE50);

TUMOR_0       = SIZE0;
F_DRUG        = 1000; // convert gemcitabine dose from g -> mg

$ODE
double DCE = DRUG/1 ; // nominal volume of 1L
double DEFF = 1 - DCE / (DCE + AE50); // Drug effect on tumor size

dxdt_DRUG  = - DRUG * KEQ; // Amt of drug in effect compartment
dxdt_TUMOR = (RATEIN * DEFF - KOVER * TUMOR) * TUMOR; // Gompertz model for tumor growth

$OMEGA @annotated
EBSIZE : 0.09 : ETA on BSIZE
ETEQ   : 0.09 : ETA on TEQ
ETURN  : 0.09 : ETA on TURN
EAE50  : 0.09 : ETA on AE50

$SIGMA @annotated
PROP: 0.1 : Proportional residual error

$TABLE
double TSIZE = TUMOR*(1+PROP);

//prevent simulation of negative concentrations
int i = 0;
while(TSIZE <0 && i < 100){
    simeps();
    TSIZE = TUMOR*(1+PROP);
    ++i;
}

$CAPTURE
TSIZE

"
', code_postamble)

#-------------------------------------------------------------------------------
#' Adaptive dosing regimen example from mrgsolve v1.4.1
#' https://mrgsolve.org/blog/posts/2024-new-1-4-0-evtools.html#update-the-dose-regimen
#' 
#-------------------------------------------------------------------------------

#' @export
pkpd_adaptive_dosing <- paste0(code_preamble, '

"
$PROB 
 - Adaptive dosing example using evtools plugin (mrgsolve v1.4.1 example, evtools-4.mod)
 - Adapted from https://mrgsolve.org/blog/posts/2024-new-1-4-0-evtools.html#update-the-dose-regimen
 - Note: dosing settings are handled as a parameter. All dosing regimens in the UI should be set to 0.
 
$CMT  @annotated
GUT     : Depot
CENT    : Central
PD      : PD Compartment

$PARAM @ annotated
CL      :     1  : Clearance (L/h)
V       :    10  : Central Volume (L)
KA      :     1  : Absorption rate constant (1/h)
KIN     :   100  : Production rate constant (XXX units/h)
KOUT    :  0.02  : Turnover rate constant for XXX (1/h)
EC50    :     7  : inhib EC50 for concentration (units/vol)

// Dosing is handled as a parameter!
DOSE    :   200  : Dose parameter
INTERVAL:    24  : Dosing interval (h)
DUR     :   0.5  : Infusion duration (h)
UNTIL   :  2400  : Treatment period
WHERE   :     2  : Dose to which CMT
PDTHRESH:  3000  : PD value threshold

$OMEGA @annotated
EEC50   : 0.5 : ETA on EC50

$PLUGIN evtools

$GLOBAL
evt::regimen reg;

$PK

if(NEWIND <= 1) {
  reg.init(self);
  reg.amt(DOSE);
  reg.ii(INTERVAL);
  reg.rate(DOSE/DUR);
  reg.until(UNTIL);
  reg.cmt(WHERE);
}

PD_0 = KIN / KOUT;

double ec50 = exp(log(EC50) + EEC50);

$ODE
dxdt_GUT = -KA * GUT;
dxdt_CENT = KA * GUT - (CL/V) * CENT;

double cp = CENT/V;
double inh = cp/(ec50 + cp);

dxdt_PD = KIN * (1-inh) - KOUT * PD;

$ERROR
capture PK = CENT/V;

if(fmod(TIME, 168)==0 && PD < PDTHRESH) {
  reg.amt(reg.amt() * 0.75);
  reg.rate(reg.amt() / DUR);
}

capture dose = reg.amt();

reg.execute();

$CAPTURE ec50
"
', code_postamble)


#-------------------------------------------------------------------------------
#' Adaptive dosing interval example from mrgsolve v1.4.1
#' https://mrgsolve.org/blog/posts/2024-new-1-4-0-evtools.html#update-the-dose-regimen
#' 
#-------------------------------------------------------------------------------

#' @export
pk_adaptive_dosing_int <- paste0(code_preamble, '

"
$PROB 
 - Adaptive dosing example using evtools plugin (mrgsolve v1.4.1 example, evtools-5.mod)
 - Adapted from https://mrgsolve.org/blog/posts/2024-new-1-4-0-evtools.html#dynamic-change-in-dosing-interval
 - Note: dosing settings are handled as a parameter. All dosing regimens in the UI should be set to 0.

$CMT  @annotated
GUT     : Depot
CENT    : Central

$PARAM @ annotated
TVCL    :     1  : Clearance (L/h)
TVV     :    10  : Central Volume (L)
KA      :     1  : Absorption rate constant (1/h)

// Dosing is handled as a parameter!
DOSE    :   100  : Dose parameter
INTERVAL:    24  : Dosing interval (h)
DUR     :   0.5  : Infusion duration (h)
UNTIL   :   540  : Treatment period
WHERE   :     2  : Dose to which CMT

$PLUGIN evtools

$GLOBAL
evt::regimen reg;

$PK

if(NEWIND <= 1) {
  reg.init(self);
  reg.amt(DOSE);
  reg.ii(INTERVAL);
  reg.cmt(WHERE);
  reg.until(UNTIL);
  reg.flagnext();
}

double CL = TVCL * exp(ECL);
double V  = TVV  * exp(EV);

$ODE
dxdt_GUT = -KA * GUT;
dxdt_CENT = KA * GUT - (CL/V) * CENT;

$OMEGA @annotated
ECL: 0.09 : ETA on clearance
EV : 0.09 : ETA on volume

$ERROR
if(evt::near(TIME, 168) && EVID > 0) {
  reg.ii(reg.ii() / 2.0);
  reg.amt(reg.amt() / 2.0);
}

if(evt::near(TIME, 300) && EVID > 0) {
  reg.ii(reg.ii() * 4);
  reg.amt(reg.amt() * 4);
}

reg.execute();

capture CP = CENT/V;
"
', code_postamble)

#-------------------------------------------------------------------------------
#' Sequential zero and first order absorption model (by Jinju Guk)
#' 
#-------------------------------------------------------------------------------

#' @export
pk_seq_first_order <- paste0(code_preamble, '

"
$GLOBAL
  
$PROB 
- Sequential zero and first-order absorption 2-cmt PK model

$CMT 
GUT
CENT
PERI
AUCx


$PARAM @annotated
TVKA      : 1.2   : Absorption rate constant Ka (1/h)
TVCL      : 8     : Clearance CL (L/h)
TVV2      : 80    : Central volume Vc (L)
TVV3      : 60    : Peripheral volume Vp (L)
TVQ       : 4     : Intercompartmental clearance Q (L/h)
F1        : 1     : Bioavailability (proportion)
TVDUR     : 4     : Duration of zero order supply to GUT

$MAIN
double KA = TVKA*exp(EKA)  ;
double CL = TVCL  ;
double VC = TVV2  ;
double VP = TVV3  ;
double Q  = TVQ   ;
double K20 = CL/VC;
double K23 = Q/VC ;
double K32 = Q/VP ;
double DUR = TVDUR*exp(EDUR); 

D_GUT  = DUR;
D_CENT = 0.00001 ; // Workaround to avoid error in MVP

$ODE
dxdt_GUT     = -KA*GUT;
dxdt_CENT    =  KA*GUT*F1 - K20*CENT - K23*CENT + K32*PERI;
dxdt_PERI    =  K23*CENT - K32*PERI;
dxdt_AUCx    = (CENT/VC);

double CP=CENT/VC;

$OMEGA @labels EKA EDUR
0.09 
0.09

$SIGMA @labels  EPSP   EPSA     
0.01
0

$TABLE
capture ICONC         = CP*(1+EPS(1)) + EPS(2);
"
', code_postamble)

#-------------------------------------------------------------------------------
#' QE approximation of TMDD, see Dua et al (2015)
#' https://doi.org/10.1002/psp4.41
#-------------------------------------------------------------------------------

#' @export
model_code_2cmt_QE_TMDD <- paste0(code_preamble, '

"
$Global

$Prob
- 2 Compartment model with Quasi-equilibrium / rapid binding (QE/RB) model for TMDD
- Mathematically equivalent to quasi-steady state (QSS) approximation

$CMT @annotated 
DEPOT   : depot administration compartment (nmol)
LTOT    : total drug in central compartment (nM)
LPERI   : drug in peripheral compartment (nM)
RTOT    : total receptor conc (nM)

$PARAM @annotated
// PK parameters;
TVV    :   3.5     :  Central Volume (L)  
TVV2   :   7       :  Peripheral Volume (L)
TVCL   :   0.07    :  Clearance (L/h) 
TVQ    :   0.01    :  Intercompartmental clearance (L/h)
TVKA   :   0.01    :  Absorption rate constant (1/h) 
TVF    :   0.7     :  Fraction Bioavailability

// Receptor parameters;
TVBASET:   10      :  Total Receptor at Baseline (nM)
TVKDEG :   0.01    :  Degradation rate constant of free receptor (1/h) 

// Drug-Receptor Complex parameters;
TVKD   :   0.1     :  Equilibrium dissociation rate constant (1/h)
TVKINT :   0.01    :  Internalization rate constant of complex (1/h) 

$MAIN
double V     =  TVV * exp(EV);
double V2    =  TVV2; 
double KA    =  TVKA * exp(EKA);
double F     =  TVF;
double CL    =  TVCL * exp(ECL);
double Q     =  TVQ;
double BASET =  TVBASET * exp(EBASET);
double KDEG  =  TVKDEG;
double KSYN  =  KDEG * BASET;
double KD    =  TVKD * exp(EKD);
double KINT  =  TVKINT * exp(EKINT);
RTOT_0       =  BASET;
F_LTOT       =  1/V;

$ODE
double L     = 0.5 * ((LTOT-RTOT-KD) + sqrt(pow(LTOT-RTOT-KD, 2) + 4*KD*LTOT)); // free drug conc (nM)
double RC    = (RTOT*L)/(KD+L);

dxdt_DEPOT   = -KA*DEPOT              ; // Depot SC (nmol)
dxdt_LTOT    = (KA*DEPOT/V)*F - (CL/V)*L - (Q/V)*(L-LPERI) - KINT*RC ;  // Total drug conc (nM)
dxdt_LPERI   =                            (Q/V2)*(L-LPERI); // DP conc (nM)
dxdt_RTOT    =  KSYN - KDEG*RTOT - (KINT-KDEG)*RC; // Total receptor conc

$OMEGA @annotated
EV    : 0.09 : ETA on V
ECL   : 0.09 : ETA on CL
EKA   : 0.09 : ETA on KA
EBASET: 0.09 : ETA on BASET
EKD   : 0.09 : ETA on KD
EKINT : 0.09 : ETA on KINT

$SIGMA @annotated
PROP : 0.1 : Prop RUV
ADD  : 0   : Add RUV

$TABLE
double PK        = L * (1 + PROP) + ADD;

//prevent simulation of negative concentrations
int i = 0;
while(PK <0 && i < 100){
    simeps();
    PK = L * (1 + PROP) + ADD;
    ++i;
}

double Complex   = RC ; // Concentration of complex (nM)
double FreeR     = RTOT*KD/(KD+L); // Concentration of free receptor (nM)
double RO        = Complex/RTOT * 100 ; // Receptor occupancy (%)
double Lfrac     = L / LTOT * 100 ; 

$CAPTURE
PK FreeR Complex RO Lfrac
"
', code_postamble)

#-------------------------------------------------------------------------------
#' MM approximation of TMDD, see Dua et al (2015)
#' https://doi.org/10.1002/psp4.41
#-------------------------------------------------------------------------------

#' @export
model_code_2cmt_MM_TMDD <- paste0(code_preamble, '

"
$Global

$Prob
- 2 Compartment model with Michaelis-Menten approximation for TMDD

$CMT @annotated 
DEPOT   : depot administration compartment (nmol)
L       : free drug in central compartment (nM)
LPERI   : drug in peripheral compartment (nM)

$PARAM @annotated
// PK parameters;
TVV    :   3.5     :  Central Volume (L)  
TVV2   :   7       :  Peripheral Volume (L)
TVCL   :   0.07    :  Clearance (L/h) 
TVQ    :   0.01    :  Intercompartmental clearance (L/h)
TVKA   :   0.01    :  Absorption rate constant (1/h) 
TVF    :   0.7     :  Fraction Bioavailability
TVKM   :   0.1     :  Michaelis-Menten constant (nM)
TVVMAX :   0.1     :  Maximal nonlinear elimination rate (nmol/(L*h))

$MAIN
double V     =  TVV * exp(EV);
double V2    =  TVV2; 
double KA    =  TVKA * exp(EKA);
double F     =  TVF;
double CL    =  TVCL * exp(ECL);
double Q     =  TVQ;
double KM    =  TVKM * exp(EKM);
double VMAX  =  TVVMAX * exp(EVMAX);

F_L          =  1/V;

$ODE

dxdt_DEPOT   = -KA*DEPOT              ; // Depot SC (nmol)
dxdt_L       = (KA*DEPOT/V)*F - (CL/V)*L - (Q/V)*(L-LPERI) - VMAX*L/(L+KM) ;  // free drug conc (nM)
dxdt_LPERI   =                            (Q/V2)*(L-LPERI); // DP conc (nM)

$OMEGA @annotated
EV    : 0.09 : ETA on V
ECL   : 0.09 : ETA on CL
EKA   : 0.09 : ETA on KA
EKM   : 0.09 : ETA on KM
EVMAX : 0.09 : ETA on VMAX

$SIGMA @annotated
PROP : 0.1 : Prop RUV
ADD  : 0   : Add RUV

$TABLE
double PK        = L * (1 + PROP) + ADD;

//prevent simulation of negative concentrations
int i = 0;
while(PK <0 && i < 100){
    simeps();
    PK = L * (1 + PROP) + ADD;
    ++i;
}

$CAPTURE
PK 
"
', code_postamble)

#-------------------------------------------------------------------------------
#' Parallel zero and first order absorption model
#' Adapted from https://github.com/mrgsolve/gallery/blob/master/absorption/parallel.md
#-------------------------------------------------------------------------------

#' @export
pk_par_first_order <- paste0(code_preamble, '

"
$GLOBAL
  
$PROB 
- Parallel zero and first-order absorption 2-cmt PK model
- Note that dosing regimen must be duplicated i.e. full dose on Regimen 1 & 2,
- where one input goes to GUT, and the other goes to CENT, i.e. splitting the dose

$CMT 
GUT
CENT
PERI
AUCx


$PARAM @annotated
TVKA      : 1.2   : Absorption rate constant Ka (1/h) from GUT
TVCL      : 8     : Clearance CL (L/h)
TVV2      : 80    : Central volume Vc (L)
TVV3      : 60    : Peripheral volume Vp (L)
TVQ       : 4     : Intercompartmental clearance Q (L/h)
F1        : 1     : Bioavailability (proportion)
TVDUR     : 4     : Duration of zero order infusion to CENT
TVFRAC    : 0.4   : Fraction of dose via zero order infusion to CENT


$MAIN
double KA = TVKA*exp(EKA)  ;
double CL = TVCL  ;
double VC = TVV2  ;
double VP = TVV3  ;
double Q  = TVQ   ;
double K20 = CL/VC;
double K23 = Q/VC ;
double K32 = Q/VP ;
double DUR = TVDUR*exp(EDUR); 

D_GUT  = 0.00001 ; // Workaround to avoid error in MVP
D_CENT = DUR;
F_CENT = TVFRAC;
F_GUT  = 1 - TVFRAC;


$ODE
dxdt_GUT     = -KA*GUT;
dxdt_CENT    =  KA*GUT*F1 - K20*CENT - K23*CENT + K32*PERI;
dxdt_PERI    =  K23*CENT - K32*PERI;
dxdt_AUCx    = (CENT/VC);

double CP    = CENT/VC;

$OMEGA @labels EKA EDUR
0.09 
0.09

$SIGMA @labels  EPSP   EPSA     
0.01
0

$TABLE
capture ICONC         = CP*(1+EPS(1)) + EPS(2);
"
', code_postamble)


##### End of Normal Code Template #####
