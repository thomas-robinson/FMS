#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fms_diag_parser.h>
#include <yaml.h>




int main(int argc, char *argv[])
{
    yaml_parser_t parser;
    yaml_event_t event;
    yaml_token_t  token;   /* new variable */

//    struct parser_state state = {.state=START, .accepted=0, .error=0};
    struct diag_files dfiles[64];
    struct diag_fields diags[64];
    int topmap = 0;
    int fmap = 0;
    char * fieldorfile;
    int i = 0;
    FILE * fyaml = fopen("diag_yaml","r");
    char * key;
    char * value;
    int ikey = 0;
    int ivalue = 0;
    int files_flag = 0;
    int fields_flag = 0;
    int ifiles = -1;
    int ifields = -1;

    memset(dfiles, 0, sizeof(dfiles));

  /* Initialize parser */
  if(!yaml_parser_initialize(&parser))
    fputs("Failed to initialize parser!\n", stderr);
  if(fyaml == NULL)
    fputs("Failed to open file!\n", stderr);

  /* Set input file */
  yaml_parser_set_input_file(&parser, fyaml);

  /* BEGIN new code */
  do {
    if (!yaml_parser_parse(&parser, &event)) {
       printf("Parser error %d\n", parser.error);
       exit(EXIT_FAILURE);
    }

    switch(event.type)
    { 
    case YAML_NO_EVENT: puts("No event!"); break;
    /* Stream start/end */
    case YAML_STREAM_START_EVENT: puts("STREAM START"); break;
    case YAML_STREAM_END_EVENT:   puts("STREAM END");   break;
    /* Block delimeters */
    case YAML_DOCUMENT_START_EVENT: puts("<b>Start Document</b>"); break;
    case YAML_DOCUMENT_END_EVENT:   puts("<b>End Document</b>");   break;
    case YAML_SEQUENCE_START_EVENT: puts("<b>Start Sequence</b>"); break;
    case YAML_SEQUENCE_END_EVENT:   puts("<b>End Sequence</b>");   break;
    case YAML_MAPPING_START_EVENT:
        puts("<b>Start Mapping</b>");
        if (files_flag == 1 &&  fields_flag == 1) {printf("ERROR PARSING MAPPING\n"); return 1;}
        if (files_flag == 1) {ifiles++;}
        if (fields_flag == 1) {ifields++;}
        break;
    case YAML_MAPPING_END_EVENT:    puts("<b>End Mapping</b>");    break;
    /* Data */
    case YAML_ALIAS_EVENT:  printf("Got alias (anchor %s)\n", event.data.alias.anchor); break;
    case YAML_SCALAR_EVENT:  
        if (strcmp(event.data.scalar.value,"diag_files") == 0){
                files_flag = 1;
                fields_flag = 0;
                printf("Got scalar (file %s),files:%d Fields:%d\n", event.data.scalar.value,files_flag,fields_flag);
        }
        else if (strcmp(event.data.scalar.value,"diag_fields") == 0){
                files_flag = 0;
                fields_flag = 1;
                printf("Got scalar (field %s),files:%d Fields:%d\n", event.data.scalar.value,files_flag,fields_flag);                
        }
        else if (files_flag == 1){
                if (strcmp(event.data.scalar.value,"name") == 0){
                        strcpy(dfiles[ifiles].key,"        ");
                        strcpy(dfiles[ifiles].key,event.data.scalar.value);
                        printf("KEY NAME: %s \n", dfiles[ifiles].key);
                        ikey = 1;
                        ivalue = 0;
                }
                else if (strcmp(event.data.scalar.value,"frequnit") == 0){
                        strcpy(dfiles[ifiles].key,"        ");
                        strcpy(dfiles[ifiles].key,event.data.scalar.value);
                        printf("KEY FREQUNIT: %s \n", dfiles[ifiles].key);
                        ikey = 1;
                        ivalue = 0;
                }
                else if (strcmp(event.data.scalar.value,"freq") == 0){
                        strcpy(dfiles[ifiles].key,"        ");
                        strcpy(dfiles[ifiles].key,event.data.scalar.value);
                        printf("KEY FREQ: %s \n", dfiles[ifiles].key);
                        ikey = 1;
                        ivalue = 0;
                }
                else if (strcmp(event.data.scalar.value,"timeunit") == 0){
                        strcpy(dfiles[ifiles].key,"        ");
                        strcpy(dfiles[ifiles].key,event.data.scalar.value);
                        printf("KEY TIMEUNIT: %s \n", dfiles[ifiles].key);
                        ikey = 1;
                        ivalue = 0;
                }
                else if (strcmp(event.data.scalar.value,"unlimdim") == 0){
                        strcpy(dfiles[ifiles].key,"        ");
                        strcpy(dfiles[ifiles].key,event.data.scalar.value);
                        printf("KEY TIMEUNIT: %s \n", dfiles[ifiles].key);
                        ikey = 1;
                        ivalue = 0;
                }

                else if (strcmp(dfiles[ifiles].key,"name") == 0 && ikey == 1){
                        strcpy(dfiles[ifiles].name,event.data.scalar.value);
                        printf("%s : %s \n",dfiles[ifiles].key,dfiles[ifiles].name);
                        ikey = 0;
                        ivalue = 1;
                }
                else if (strcmp(dfiles[ifiles].key,"frequnit") == 0 && ikey == 1){
                        strcpy(dfiles[ifiles].frequnit,event.data.scalar.value);
//                        dfiles[ifiles].frequnit = event.data.scalar.value;
                        printf("%s : %s \n",dfiles[ifiles].key,dfiles[ifiles].frequnit);
                        ikey = 0;
                        ivalue = 1;
                }
                else if (strcmp(dfiles[ifiles].key,"freq") == 0 && ikey == 1){
                        dfiles[ifiles].freq = atoi(event.data.scalar.value);
                        printf("%s : %d \n",dfiles[ifiles].key,dfiles[ifiles].freq);
                        ikey = 0;
                        ivalue = 1;
                }
                else if (strcmp(dfiles[ifiles].key,"timeunit") == 0 && ikey == 1){
                        strcpy(dfiles[ifiles].timeunit,event.data.scalar.value);
//                        dfiles[ifiles].timeunit = event.data.scalar.value;
                        printf("%s : %s \n",dfiles[ifiles].key,dfiles[ifiles].timeunit);
                        ikey = 0;
                        ivalue = 1;
                }
                else if (strcmp(dfiles[ifiles].key,"unlimdim") == 0 && ikey == 1){
                        strcpy(dfiles[ifiles].unlimdim,event.data.scalar.value);
//                        dfiles[ifiles].unlimdim = event.data.scalar.value;
                        printf("%s : %s \n",dfiles[ifiles].key,dfiles[ifiles].unlimdim);
                        ikey = 0;
                        ivalue = 1;
                }
                else {

                        printf("ERROR :: SOMETHING IS WRONG \n");
                        return 1;
                }
                printf("Got scalar (value %s) %d \n", event.data.scalar.value,ikey);
        }
        else if (fields_flag == 1){
                printf("Got scalar (value %s)\n", event.data.scalar.value);
        }
        printf("Got scalar (value %s),files:%d Fields:%d\n", event.data.scalar.value,files_flag,fields_flag);
        break;
    }
    if(event.type != YAML_STREAM_END_EVENT)
      yaml_event_delete(&event);
  } while(event.type != YAML_STREAM_END_EVENT);
  yaml_event_delete(&event);
  /* END new code */

printf("%s %s %s %s %d \n",dfiles[0].unlimdim, dfiles[0].frequnit, dfiles[0].name, dfiles[0].timeunit, dfiles[0].freq) ;

  /* Cleanup */
  yaml_parser_delete(&parser);
  fclose(fyaml);
  printf("Files = %d, Fields = %d \n",ifiles+1, ifields+1); 
  return 0;


}
