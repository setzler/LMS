#include <Rcpp.h>
using namespace Rcpp;

//' Computes spell length within unit
//' 
//' Within unit_id, this will create a counter that updates
//' when the variable stayer_var changes for when a time is missing
//'
//' @param unit_id An integer vector with the ID of the unit we consider
//' @param time An integer vector with the time considered
//' @param stayer_var  An integer with the variable that needs to stay constant
//' 
//' @export
// [[Rcpp::export]]
IntegerVector computeSpellTime(IntegerVector unit_id, IntegerVector time, IntegerVector stayer_var) {
  
  int n = unit_id.size();
  IntegerVector vout(n);
  
  int length = 1;
  vout[0]=1;
  for(int i=1; i<n; i++) {
    if (unit_id[i]!=unit_id[i-1]) length=0;
    if (time[i]!= time[i-1] +1) length=0; 
    if (stayer_var[i]!= stayer_var[i-1]) length=0;
    vout[i] = (++length);
  }
  return(vout);
}



//' Computes spell ID
//' 
//' Within unit_id, this will create a counter that updates
//' when the variable stayer_var changes for when a time is missing
//'
//' @param unit_id An integer vector with the ID of the unit we consider
//' @param time An integer vector with the time considered
//' @param stayer_var  An integer with the variable that needs to stay constant
//' 
//' @export
// [[Rcpp::export]]
IntegerVector computeSpellID(IntegerVector unit_id, IntegerVector time, IntegerVector stayer_var) {
  
  int n = unit_id.size();
  IntegerVector vout(n);
  
  int spell_id = 1;
  vout[0]=1;
  for(int i=1; i<n; i++) {
    if ((unit_id[i] != unit_id[i-1]) || (time[i] != time[i-1] +1) || (stayer_var[i] != stayer_var[i-1])) spell_id=spell_id+1;
    vout[i] = (spell_id);
  }
  return(vout);
}


//' Computes the minimum distance to the end of a spell
//' 
//' Within unit_id, this will create a counter that updates
//' when the variable stayer_var changes for when a time is missing
//'
//' @param unit_id An integer vector with the ID of the unit we consider
//' @param time An integer vector with the time considered
//' @param stayer_var  An integer with the variable that needs to stay constant
//' @param spell_end  An integer with end of spell location
//' 
//' @export
// [[Rcpp::export]]
IntegerVector computeDistanceToSpellEnd(IntegerVector unit_id, IntegerVector time, IntegerVector stayer_var, IntegerVector spell_end) {
  
  int n = unit_id.size();
  IntegerVector distance_to_end(n);
  
  // then we go backward, store when time is >= lag_length
  // and all other lags within that spell
  int current_distance_to_spell_end = -1;

  if (spell_end[n-1] > 0) {
    distance_to_end[n-1] = 0;
    current_distance_to_spell_end = 0;
  } else {
    distance_to_end[n-1] = -1;
    current_distance_to_spell_end = -1;
  }    

  for(int i=n-2; i>=0; i--) {
    // if we find an end of spell
    if (spell_end[i] > 0) {
      current_distance_to_spell_end = 0;
    } else {
      
      // if spell changes without an end of spell
      if (time[i]!= time[i+1]-1)            current_distance_to_spell_end = -1 ; 
      if (stayer_var[i]!= stayer_var[i+1])  current_distance_to_spell_end = -1 ; 
      // if spell does not change and not end of spell
      if (current_distance_to_spell_end>=0) current_distance_to_spell_end += 1;
    }
    
    distance_to_end[i] = current_distance_to_spell_end;
  }
  
  return(distance_to_end);
}
