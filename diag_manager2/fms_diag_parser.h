#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <yaml.h>
typedef struct diag_files {
     char name [20];
     char frequnit [7];
     int freq;
     char timeunit [7];
     char unlimdim [8];
     char key [8];
} files;


typedef struct diag_fields {
     char name[20];
     char var[20];
     char files[20];
     int intkind;
     char skind[20];
     char reduction[20];
     char all[4];
     char region[50];
     char regcoord[50];
     char module[20];
     char key [8];
} fields;

 struct diag_files dfiles[5];
 struct diag_fields diags[3];
 yaml_event_t event;
 yaml_parser_t parser ;

 int r8 = 8;
 int r4 = 4;
 int i8 = -8;
 int i4 = -4;
 int istring = 19;
 int null_type_int = -999;

 

