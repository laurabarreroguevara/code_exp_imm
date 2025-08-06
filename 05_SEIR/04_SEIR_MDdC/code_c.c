
/* pomp C snippet file: code_c */
/* Time: 2025-04-07 12:33:57.857 +0200 */
/* Salt: 79C72988D2E554408EF4539D */

#include <pomp.h>
#include <R_ext/Rdynload.h> 
static int n_seas = 4; 



/* C snippet: 'skeleton' */

#define mu (__p[__parindex[0]])
#define R0 (__p[__parindex[1]])
#define sigma (__p[__parindex[2]])
#define gamma (__p[__parindex[3]])
#define c_V (__p[__parindex[4]])
#define mu_eff (__p[__parindex[5]])
#define mu_dur (__p[__parindex[6]])
#define A_trans (__p[__parindex[7]])
#define phi_trans (__p[__parindex[8]])
#define seas_eff_1 (__p[__parindex[9]])
#define seas_eff_2 (__p[__parindex[10]])
#define seas_eff_3 (__p[__parindex[11]])
#define seas_eff_4 (__p[__parindex[12]])
#define S_0 (__p[__parindex[13]])
#define E_0 (__p[__parindex[14]])
#define I_0 (__p[__parindex[15]])
#define R_0 (__p[__parindex[16]])
#define V_1_0 (__p[__parindex[17]])
#define V_2_0 (__p[__parindex[18]])
#define V_3_0 (__p[__parindex[19]])
#define V_4_0 (__p[__parindex[20]])
#define V_1 (__x[__stateindex[0]])
#define V_2 (__x[__stateindex[1]])
#define V_3 (__x[__stateindex[2]])
#define V_4 (__x[__stateindex[3]])
#define S (__x[__stateindex[4]])
#define E (__x[__stateindex[5]])
#define I (__x[__stateindex[6]])
#define R (__x[__stateindex[7]])
#define DV_1 (__f[__stateindex[0]])
#define DV_2 (__f[__stateindex[1]])
#define DV_3 (__f[__stateindex[2]])
#define DV_4 (__f[__stateindex[3]])
#define DS (__f[__stateindex[4]])
#define DE (__f[__stateindex[5]])
#define DI (__f[__stateindex[6]])
#define DR (__f[__stateindex[7]])
 
void __pomp_skelfn (double *__f, const double *__x, const double *__p,
  const int *__stateindex, const int *__parindex, const int *__covindex,
  const double *__covars, double t) { 
  int seas_cur = (int) floor(fmod(t, n_seas)); // Current season
  double lambda, p_V_cur, seas_B; 
  int i; 
  
  // Pointer to state variables and derivatives 
  double *V_vec = (double *) &V_1; 
  double *DV_vec = (double *) &DV_1; 
  
  // Pointer to season-specific vaccine effectiveness/duration
  double *seas_vec = (double *) &seas_eff_1; 
  
  // Effective vaccine coverage
  p_V_cur = c_V * mu_eff * seas_vec[seas_cur]; 
  
  // FoI 
  seas_B = 1.0 + A_trans * cos(2 * M_PI * (t - phi_trans)); 
  lambda = R0 * (gamma + mu) * seas_B * I; 
  
  // ODE for S
  DS = mu * (1.0 - p_V_cur) - (lambda + mu) * S; 
  
  // ODEs for V states
  for(i = 0; i < n_seas; i++) {
    DV_vec[i] = mu * p_V_cur * (i == seas_cur) - (1.0 / (mu_dur * seas_vec[seas_cur]) + mu) * V_vec[i]; 
    DS +=  V_vec[i] / (mu_dur * seas_vec[seas_cur]); 
  }
  
  // ODEs for other state variables
  DE = lambda * S - (sigma + mu) * E; 
  DI = sigma * E - (gamma + mu) * I; 
  DR = gamma * I - mu * R; 
   
}
 

#undef mu
#undef R0
#undef sigma
#undef gamma
#undef c_V
#undef mu_eff
#undef mu_dur
#undef A_trans
#undef phi_trans
#undef seas_eff_1
#undef seas_eff_2
#undef seas_eff_3
#undef seas_eff_4
#undef S_0
#undef E_0
#undef I_0
#undef R_0
#undef V_1_0
#undef V_2_0
#undef V_3_0
#undef V_4_0
#undef V_1
#undef V_2
#undef V_3
#undef V_4
#undef S
#undef E
#undef I
#undef R
#undef DV_1
#undef DV_2
#undef DV_3
#undef DV_4
#undef DS
#undef DE
#undef DI
#undef DR
static int __pomp_load_stack = 0;
void __pomp_load_stack_incr (void) {++__pomp_load_stack;}
void __pomp_load_stack_decr (int *val) {*val = --__pomp_load_stack;} 

 
void R_init_code_c (DllInfo *info) {
  R_RegisterCCallable("code_c", "__pomp_load_stack_incr", (DL_FUNC) __pomp_load_stack_incr);
  R_RegisterCCallable("code_c", "__pomp_load_stack_decr", (DL_FUNC) __pomp_load_stack_decr);
  R_RegisterCCallable("code_c", "__pomp_skelfn", (DL_FUNC) __pomp_skelfn);
  

}
 