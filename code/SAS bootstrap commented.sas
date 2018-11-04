/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*This is a small SAS program to perform nonparametric bootstraps for a regression
/*It is not efficient nor general*/
/*Inputs: 																								*/
/*	- NumberOfLoops: the number of bootstrap iterations
/*	- Dataset: A SAS dataset containing the response and covariate										*/
/*	- XVariable: The covariate for our regression model (gen. continuous numeric)						*/
/*	- YVariable: The response variable for our regression model (gen. continuous numeric)				*/
/*Outputs:																								*/
/*	- ResultHolder: A SAS dataset with NumberOfLoops rows and two columns, RandomIntercept & RandomSlope*/
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

%macro regBoot(NumberOfLoops, DataSet, XVariable, YVariable);


/*Number of rows in my dataset*/
 	data _null_; /*doesn't create a data set*/
  	set &DataSet NOBS=size; /*creates temporary variable called 'size' that's value is the no. of observations of &DataSet*/
  	call symput("NROW",size); /*assigns the value of 'size' to the macro variable 'NROW*/
 	stop;
 	run;

/*loop over the number of randomisations required*/
%do i=1 %to &NumberOfLoops; /* equivalent to 'for' loop in R i.e. for(i in 1:n) */


/*Sample my data with replacement*/
	proc surveyselect data=&DataSet out=bootData seed=-23434 method=urs noprint sampsize=&NROW;
	/*samples from &DataSet, output is 'bootData', method is unrestricted random sampling, sample size is value of NROW*/
	run;

/*Conduct a regression on this randomised dataset and get parameter estimates*/
	proc reg data=bootData outest=ParameterEstimates  noprint; 
	Model &YVariable=&XVariable; /*outputs regression of x over y and outputs to 'ParameterEstimates' */
	run;
	quit;

/*Extract just the columns for slope and intercept for storage*/
	data Temp;
	set ParameterEstimates;
	keep Intercept &XVariable;
	run;

/*Create a new results dataset of the first iteration, append for following iterations*/
	data ResultHolder;
		%if &i=1 %then %do;
			set Temp;
		%end;
		%else %do;
			set ResultHolder Temp;
		%end;
	run;
	%end;
/*Rename the results something nice*/
data ResultHolder;
set ResultHolder;
rename Intercept=RandomIntercept &XVariable=RandomSlope;
run;
%mend;
/*importing fitness data set*/
PROC IMPORT OUT= Asmt2.fitness 
            DATAFILE= "C:\Users\p-mcl\OneDrive\Documents\Masters\MT5763\
Assignment 2\fitness.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/*Timing the macro*/
/*Remember to reference this code*/
%let _sdtm=%sysfunc(datetime());
/*Run the macro*/
%regBoot(NumberOfLoops=100, DataSet=Asmt2.fitness, XVariable=Age, YVariable=Oxygen);
%let _edtm=%sysfunc(datetime());
%let _runtm=%sysfunc(putn(&_edtm - &_sdtm, 12.4));
%put It took &_runtm seconds to run the program;
