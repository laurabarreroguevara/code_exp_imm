// Implementation of the SE2I2R model of varicella transmission
// Two sources of seasonal forcing: 
////school terms (square wave) and climate (covariate(exponent function))
// State variables are PROPORTIONS
// Time is measured in WEEKS; rates are per WEEK

//start_globs

// Indicator function of school term
// Returns 1 during school term, -1 during holidays
static double isSchool(double time, int country) {
	//Args:
	// time: time 
	// country: character indicating country (1 for Colombia, 
	                                        //2 for Mexico, 
	                                        //3 for Panama, 
	                                        //4 for Costa Rica, 
	                                        //5 for Nicaragua, 
	                                        //6 for Honduras, 
	                                        //7 for El Salvador, 
	                                        //8 for Guatemala,
	                                        //9 for Belize)
	// Returns: 1 during school term, -1 during holidays
	double wk_no = fmod(time, 52.0); // Week no (remainder of integer division by 52)
	double out; 

	if(country == 1) {
		out = ((wk_no >= 3.0 && wk_no < 15.0) || 
			(wk_no >= 16.0 && wk_no < 24.0) ||
			(wk_no >= 27.0 && wk_no < 40.0) ||
			(wk_no >= 41.0 && wk_no < 48.0)) ? 1.0 : -1.0;
	} else if(country == 2) {
		out = ((wk_no >= 1.0 && wk_no < 14.0) || 
			(wk_no >= 16.0 && wk_no < 29.0) ||
			(wk_no >= 33.0 && wk_no < 52.0)) ? 1.0 : -1.0;
	} else if(country == 3) {
	  out = ((wk_no >= 9.0 && wk_no < 10.0) || 
	    (wk_no >= 11.0 && wk_no < 23.0) ||
	    (wk_no >= 24.0 && wk_no < 38.0) ||
	    (wk_no >= 39.0 && wk_no < 45.0) ||
	    (wk_no >= 46.0 && wk_no < 51.0)) ? 1.0 : -1.0;
	} else if(country == 4) {
	  out = ((wk_no >= 7.0 && wk_no < 27.0) || 
	    (wk_no >= 29.0 && wk_no < 51.0)) ? 1.0 : -1.0;
	} else if(country == 5) {
	  out = ((wk_no >= 7.0 && wk_no < 16.0) || 
	    (wk_no >= 17.0 && wk_no < 28.0) ||
	    (wk_no >= 29.0 && wk_no < 51.0)) ? 1.0 : -1.0;
	} else if(country == 6) {
	  out = ((wk_no >= 6.0 && wk_no < 16.0) || 
	    (wk_no >= 17.0 && wk_no < 49.0)) ? 1.0 : -1.0;
	} else if(country == 7) { 
	  out = ((wk_no >= 4.0 && wk_no < 14.0) || 
	    (wk_no >= 15.0 && wk_no < 31.0) ||
	    (wk_no >= 32.0 && wk_no < 48.0)) ? 1.0 : -1.0;
	} else if(country == 8) {
	  out = ((wk_no >= 2.0 && wk_no < 16.0) || 
	    (wk_no >= 17.0 && wk_no < 26.0) ||
	    (wk_no >= 27.0 && wk_no < 45.0)) ? 1.0 : -1.0;
	} else if(country == 9) {
	  out = ((wk_no >= 2.0 && wk_no < 16.0) || 
	    (wk_no >= 18.0 && wk_no < 27.0) ||
	    (wk_no >= 35.0 && wk_no < 51.0)) ? 1.0 : -1.0;
	} else {
		out = 0.0;
	}
	return(out);

}
//end_globs

//start_rinit

// INITIAL CONDITIONS
S = S0; 
E1 = E0 / 2.0;
E2 = E0 / 2.0;
I1 = I0 / 2.0;
I2 = I0 / 2.0; 
R = 1.0 - S0 - E0 - I0;
CC = 0;
//end_rinit

//start_skel

// DETERMINISTIC SKELETON 

// Calculate seasonal terms
double beta0 = R0 * gamma; // Transmission rate

double p_school; // Proportion of weeks at school 
double values[] = {43.0 / 52.0,
                   40.0 / 52.0,  
                   45.0 / 52.0,
                   38.0 / 52.0,
                   42.0 / 52.0,
                   42.0 / 52.0,
                   42.0 / 52.0,
                   42.0 / 52.0,
                   41.0 / 52.0,
                   39.0 / 52.0};

if (c_name >= 1 && c_name <= 9) {
  p_school = values[c_name];
} else {
  p_school = values[0];
}

// Climate forcing
double s_clim_t = exp(amp_clim * clim);

// Term-time forcing
double s_term_t = 1.0 + amp_term * isSchool(t, c_name);
s_term_t /= (1.0 - amp_term * (1.0 - 2.0 * p_school)); 

// Force of infection
double lambda_t = beta0 * s_clim_t * s_term_t * (I1 + I2); 

if(debug && (t <= 52.0)) {
	Rprintf("t=%.2f, country=%d, school=%.1f\n\n", t, c_name, isSchool(t, c_name));
}

// Protection 
double omega = 2.0 * M_PI / 52.0; // Annual angular frequency (per week)
double prot_t = a1 * cos(omega * t) + b1 * sin(omega * t) + a2 * cos(2.0 * omega * t) + b2 * sin(2.0 * omega * t); 
prot_t = prot0 * (1.0 + amp_prot * prot_t);

// ODEs
DS = mu * (1.0 - prot_t) - (lambda_t + mu) * S; 
DE1 = lambda_t * S - (2.0 * sigma + mu) * E1; 
DE2 = 2.0 * sigma * (E1 - E2) - mu * E2; 
DI1 = 2.0 * sigma * E2 - (2.0 * gamma + mu) * I1;
DI2 = 2.0 * gamma * (I1 - I2) - mu * I2; 
DR = mu * (prot_t) + 2.0 * gamma * I2 - mu * R; 
DCC = 2.0 * gamma * I1;

//end_skel

