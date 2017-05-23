# Infliximab population model code for infliximabapp
# ------------------------------------------------------------------------------
# Define the model parameters and equations
# Using mrgsolve - differential equations
# This compiled model is used for simulating n individuals and their concentration-time profiles
code <- '
$SET			atol = 1e-8, rtol = 1e-8
					maxsteps = 100000

$INIT			// Initial conditions for PK compartments
					CENT = 0,
					PERI = 0,
					AUC = 0

$PARAM		// Population parameters
					POPCL = 0.294,
					POPV1 = 3.33,
					POPQ = 0.0719,
					POPV2 = 1.14,

					// Covariate effects
					WT_CL = 0.614,	// Effect of weight on clearance
					ALB_CL = -1.17,	// Effect of albumin on clearance
					ADA_CL = 0.257,	// Effect of anti-drug antibodies on clearance
					WT_V1 = 0.691,	// Effect of weight on V1
					WT_Q = 1.1,	// Effect of weight on Q
					WT_V2 = 0.59,	// Effect of weight on V2

					// Covariate values for simulation
					WT = 70,	// Weight (kg)
					ALB = 4,	// Albumin (g/dL)
					ADA = 0,	// Presence of anti-drug antibodies
					target = 3,	// Target trough concentration (mg/L)

					// Presimulated PPV values
					ETA1 = 0,
					ETA2 = 0,
					ETA3 = 0,
					ETA4 = 0,
					ERRPRO = 0

$OMEGA		@annotated
					PPVCL: 0.106929	: ETA on clearance
					PPVV1: 0.0225 : ETA on volume of the central compartment
					PPVQ: 1.21	: ETA on inter-compartmental clearance
					PPVV2: 0.638401	: ETA on volume of the peripheral compartment

$SIGMA		@annotated
					ERR_PRO: 0.175561	: Proportional residual error

$MAIN			// Infusion duration
					D_CENT = 0.08333333;  // 2 hours

					// Covariate effects
						// Anti-drug antibodies
						double ADACOV = 1;	// No anti-drug antibodies
						if (ADA == 1) ADACOV = 1+ADA_CL; // Anti-drug antibodies

					// Population parameter values
					double TVCL = POPCL*pow(WT/70,WT_CL)*pow(ALB/4,ALB_CL)*ADACOV;
					double TVV1 = POPV1*pow(WT/70,WT_V1);
					double TVQ = POPQ*pow(WT/70,WT_Q);
					double TVV2 = POPV2*pow(WT/70,WT_V2);

					// Individual parameter values
					double CL = TVCL*exp(ETA1);
					double V1 = TVV1*exp(ETA2);
					double Q = TVQ*exp(ETA3);
					double V2 = TVV2*exp(ETA4);

					// Individual micro-rate constants
					double K10 = CL/V1;
					double K12 = Q/V1;
					double K21 = Q/V2;

$ODE			// Individual differential equations
					dxdt_CENT = -K12*CENT +K21*PERI -K10*CENT;
					dxdt_PERI = K12*CENT -K21*PERI;

					// Area under the curve
					double CP = CENT/V1;	// Concentration in central compartment
					dxdt_AUC = CP;

$TABLE		// Calculate concentrations in the central compartment
					double IPRE = CENT/V1;
					double DV = IPRE*(1+ERRPRO);

$CAPTURE	IPRE DV CL V1 Q V2 WT ALB ADA ETA1 ETA2 ETA3 ETA4
'
# Compile the model code
mod <- mcode("popINFLIX",code)
	# There is opportunity to simply update model parameters after the model code has been compiled

# Create single line data frame to be repeated x times
	input.sim.data <- data.frame(
		ID = 1,
		time = 0,
		amt = 0,
		cmt = 1,
		evid = 0,
		rate = 0,
		obs = as.numeric(NA)
	)	# data frame to be input into mrgsolve
