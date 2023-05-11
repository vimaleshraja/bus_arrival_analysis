proc import datafile= '~/MA3710/data/data.csv'
	out = data1
	dbms = csv;

proc sgplot data=data1;
vbox Secondslate / category=Driver group=TripType ;
run;

proc glm data=data1 plots=residuals;
class Driver TripType;
model Secondslate = Driver TripType Driver*TripType;
means Driver TripType Driver*TripType / cldiff tukey lines;
run;

