
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This program performs nonparametric bootstraps for the parameter estimates produced by a linear regression
Inputs: 																								
	- NumberOfLoops: the number of bootstrap iterations
	- Dataset: A SAS dataset containing the response and covariate										
	- XVariable: The covariate for our regression model (gen. continuous numeric)						
	- YVariable: The response variable for our regression model (gen. continuous numeric)				
Outputs:																								
	- ResultHolder: A SAS dataset with NumberOfLoops rows and two columns, RandomIntercept & RandomSlope
  	- output.rtf: An RTF file which contains tables of the 95% confidence intervals and charts of the 
	  distribution of the intercept and X covariate parameter estimates 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

%macro SASBoot(NumberOfLoops, DataSet, XVariable, YVariable);


/* Load data set to RAM and generate random samples of the data */

/*       sasfile &DataSet load;*/
       proc surveyselect data=&DataSet out=BootData seed=-23434 noprint
       method=urs samprate=1 outhits rep=&NumberOfLoops;
       run;
/*       sasfile &DataSet close;*/

/*Calculate and store the parameter estimates*/

       proc reg data=BootData outest=ParameterEstimates(drop=_:) noprint;
       model &YVariable=&XVariable;
       by replicate;
       run;

/*Extract and store the columns for the intercept and X covariate*/

       data ResultHolder;
       set ParameterEstimates;
       keep Intercept &XVariable;
       run;

/*Calculate the means of the X covariate of every bootstrap sample*/

       proc univariate data=BootData noprint;
       var &XVariable;
       by replicate;
       output out=uniOut mean=mean&XVariable;
       run;

/*Calculate 95% confidence intervals for the mean of the intercept parameter estimates*/

       proc univariate data=ResultHolder;
       var Intercept;
       output out=InterceptCI pctlpts=2.5, 97.5 pctlpre=CI; 
       run;

/*Calculate 95% confidence intervals for the mean of X covariate parameter estimates*/

       proc univariate data=ResultHolder;
       var &XVariable;
       output out=XvarCI pctlpts=2.5, 97.5 pctlpre=CI; 
       run;

	   /*Create output RTF file*/

       ods rtf file="output.rtf" bodytitle startpage = never;
		title1 "Drunken Master 2";
		title2 "Results of SAS Bootstrap Program";

		title4 "95% Confidence Interval for the Intercept Parameter Estimate";

        /*Results for the intercept parameter estimate*/

        proc print data=InterceptCI;
        run;
		title "Distribution of the Intercept Parameter Estimate";
        proc gchart data=ResultHolder;
        vbar Intercept;
        run;

        /*Results for the X covariate parameter estimate*/

        ods startpage = now;
		title "95% Confidence Interval for the X Covariate Parameter Estimate";
        proc print data=XvarCI;
        run;

		title "Distribution of the X Covariate Parameter Estimate";
        proc gchart data=ResultHolder;
        vbar &XVariable;
        run;

        ods rtf close;

%MEND SASBoot; /*End of SASBoot macro*/
/*Importing the fitness data set*/

proc import out = Asmt2.fitness 
   datafile = "C:\Users\baf3\Desktop\fitness.csv" 
   dbms = CSV REPLACE;
   getnames = YES;
   datarow = 2; 
run;
/*Timing the macro (H, 2012)*/

%let _sdtm=%sysfunc(datetime());

/*Run the macro*/

%SASBoot(NumberOfLoops=1000, DataSet=Asmt2.fitness, XVariable=Weight, YVariable=Oxygen);
%let _edtm=%sysfunc(datetime());
%let _runtm=%sysfunc(putn(&_edtm - &_sdtm, 12.4));
%put It took &_runtm seconds to run the program;




