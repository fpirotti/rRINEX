#include <Rcpp.h>
#include "rtklib.h"
using namespace Rcpp;


// [[Rcpp::export]]
List rtklibR_readrnx(CharacterVector &files, int flag=1, int index=1) {
  
  char string[1024];    
  double ver; 
  obs_t obs={0};
  nav_t nav={0};
  sta_t sta={""};
  gtime_t ts,te;
  double tint;
  int i,n;
  char obsfile[1024],navfile[1024]="";
  int nfiles = files.size();
  char opt[1024]="";
  if(nfiles<1){
    Rcout << "No files found, exiting..." << std::endl; 
    return R_NilValue;
  }
  
  // Rcout << "COMPILED FOR  " <<  NSYS << " SYSTEMS." << std::endl; 
  // Rcout << "COMPILED FOR  " <<  NFREQ  << " NFREQ ." << std::endl; 
  char tobs[NSYS][MAXOBSTYPE][4]={{""}};
  
  List headers  = List::create();

  for(int i=0; i < nfiles; i++){
    String s(files[i]);
     int res = readrnxt(s.get_cstring(),1,ts,te,tint,opt,&obs,&nav,&sta);
     if(res < 0) {
       Rcout << "Problems reading file " << s.get_cstring() <<  std::endl;
       return R_NilValue;
     }
     
     Rcout << res << std::endl;
  }

  return headers ;
}
