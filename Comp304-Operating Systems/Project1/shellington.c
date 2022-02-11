#include <unistd.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <termios.h>            //termios, TCSANOW, ECHO, ICANON
#include <string.h>
#include <stdbool.h>
#include <errno.h>

#include <limits.h>
const char * sysname = "shellington";

enum return_codes {
	SUCCESS = 0,
	EXIT = 1,
	UNKNOWN = 2,
};
struct command_t {
	char *name;
	bool background;
	bool auto_complete;
	int arg_count;
	char **args;
	char *redirects[3]; // in/out redirection
	struct command_t *next; // for piping
};


///// My global variables and structs /////////////
typedef struct cNode {
	char name[50]; // 50 byte is enough for names since the point is about using shortcuts(no need to free individually)
	char *path; //I will use dynamically allocated memory using malloc (need to free individually)
	struct cNode* next; // to use it in linked list implementation
} cNode;

typedef struct commandList {
	int size; //size of the list
	cNode* head;
} commandList;
struct commandList* commandStack;

//Global Variables for bookmark
int bookmarkCount; //To know how many bookmarks are already in the array

typedef struct bookMark {
	char* commandN; //To hold command name
	char** commandA; //To hold command arg
	int countArg;
} bookMark;

struct bookMark *bookmarkArray[30]; // 30 bookmark is enough since the purpose of bookmark is about frequent use of commands 

//Global Variables For the awasome command !
char* diceArray[6];

/**
 * Prints a command struct
 * @param struct command_t *
 */
void print_command(struct command_t * command)
{
	int i=0;
	printf("Command: <%s>\n", command->name);
	printf("\tIs Background: %s\n", command->background?"yes":"no");
	printf("\tNeeds Auto-complete: %s\n", command->auto_complete?"yes":"no");
	printf("\tRedirects:\n");
	for (i=0;i<3;i++)
		printf("\t\t%d: %s\n", i, command->redirects[i]?command->redirects[i]:"N/A");
	printf("\tArguments (%d):\n", command->arg_count);
	for (i=0;i<command->arg_count;++i)
		printf("\t\tArg %d: %s\n", i, command->args[i]);
	if (command->next)
	{
		printf("\tPiped to:\n");
		print_command(command->next);
	}


}
/**
 * Release allocated memory of a command
 * @param  command [description]
 * @return         [description]
 */
int free_command(struct command_t *command)
{
	if (command->arg_count)
	{
		for (int i=0; i<command->arg_count; ++i)
			free(command->args[i]);
		free(command->args);
	}
	for (int i=0;i<3;++i)
		if (command->redirects[i])
			free(command->redirects[i]);
	if (command->next)
	{
		free_command(command->next);
		command->next=NULL;
	}
	free(command->name);
	free(command);
	return 0;
}
/**
 * Show the command prompt
 * @return [description]
 */
int show_prompt()
{
	char cwd[1024], hostname[1024];
    gethostname(hostname, sizeof(hostname));
	getcwd(cwd, sizeof(cwd));
	printf("%s@%s:%s %s$ ", getenv("USER"), hostname, cwd, sysname);
	return 0;
}
/**
 * Parse a command string into a command struct
 * @param  buf     [description]
 * @param  command [description]
 * @return         0
 */
int parse_command(char *buf, struct command_t *command)
{
	const char *splitters=" \t"; // split at whitespace
	int index, len;
	len=strlen(buf);
	while (len>0 && strchr(splitters, buf[0])!=NULL) // trim left whitespace
	{
		buf++;
		len--;
	}
	while (len>0 && strchr(splitters, buf[len-1])!=NULL)
		buf[--len]=0; // trim right whitespace

	if (len>0 && buf[len-1]=='?') // auto-complete
		command->auto_complete=true;
	if (len>0 && buf[len-1]=='&') // background
		command->background=true;

	char *pch = strtok(buf, splitters);
	command->name=(char *)malloc(strlen(pch)+1);
	if (pch==NULL)
		command->name[0]=0;
	else
		strcpy(command->name, pch);

	command->args=(char **)malloc(sizeof(char *));

	int redirect_index;
	int arg_index=0;
	char temp_buf[1024], *arg;
	while (1)
	{
		// tokenize input on splitters
		pch = strtok(NULL, splitters);
		if (!pch) break;
		arg=temp_buf;
		strcpy(arg, pch);
		len=strlen(arg);

		if (len==0) continue; // empty arg, go for next
		while (len>0 && strchr(splitters, arg[0])!=NULL) // trim left whitespace
		{
			arg++;
			len--;
		}
		while (len>0 && strchr(splitters, arg[len-1])!=NULL) arg[--len]=0; // trim right whitespace
		if (len==0) continue; // empty arg, go for next

		// piping to another command
		if (strcmp(arg, "|")==0)
		{
			struct command_t *c=malloc(sizeof(struct command_t));
			int l=strlen(pch);
			pch[l]=splitters[0]; // restore strtok termination
			index=1;
			while (pch[index]==' ' || pch[index]=='\t') index++; // skip whitespaces

			parse_command(pch+index, c);
			pch[l]=0; // put back strtok termination
			command->next=c;
			continue;
		}

		// background process
		if (strcmp(arg, "&")==0)
			continue; // handled before

		// handle input redirection
		redirect_index=-1;
		if (arg[0]=='<')
			redirect_index=0;
		if (arg[0]=='>')
		{
			if (len>1 && arg[1]=='>')
			{
				redirect_index=2;
				arg++;
				len--;
			}
			else redirect_index=1;
		}
		if (redirect_index != -1)
		{
			command->redirects[redirect_index]=malloc(len);
			strcpy(command->redirects[redirect_index], arg+1);
			continue;
		}

		// normal arguments
		if (len>2 && ((arg[0]=='"' && arg[len-1]=='"')
			|| (arg[0]=='\'' && arg[len-1]=='\''))) // quote wrapped arg
		{
			arg[--len]=0;
			arg++;
		}
		command->args=(char **)realloc(command->args, sizeof(char *)*(arg_index+1));
		command->args[arg_index]=(char *)malloc(len+1);
		strcpy(command->args[arg_index++], arg);
	}
	command->arg_count=arg_index;
	return 0;
}
void prompt_backspace()
{
	putchar(8); // go back 1
	putchar(' '); // write empty over
	putchar(8); // go back 1 again
}
/**
 * Prompt a command from the user
 * @param  buf      [description]
 * @param  buf_size [description]
 * @return          [description]
 */
int prompt(struct command_t *command)
{
	int index=0;
	char c;
	char buf[4096];
	static char oldbuf[4096];

    // tcgetattr gets the parameters of the current terminal
    // STDIN_FILENO will tell tcgetattr that it should write the settings
    // of stdin to oldt
    static struct termios backup_termios, new_termios;
    tcgetattr(STDIN_FILENO, &backup_termios);
    new_termios = backup_termios;
    // ICANON normally takes care that one line at a time will be processed
    // that means it will return if it sees a "\n" or an EOF or an EOL
    new_termios.c_lflag &= ~(ICANON | ECHO); // Also disable automatic echo. We manually echo each char.
    // Those new settings will be set to STDIN
    // TCSANOW tells tcsetattr to change attributes immediately.
    tcsetattr(STDIN_FILENO, TCSANOW, &new_termios);


    //FIXME: backspace is applied before printing chars
	show_prompt();
	int multicode_state=0;
	buf[0]=0;
  	while (1)
  	{
		c=getchar();
		// printf("Keycode: %u\n", c); // DEBUG: uncomment for debugging

		if (c==9) // handle tab
		{
			buf[index++]='?'; // autocomplete
			break;
		}

		if (c==127) // handle backspace
		{
			if (index>0)
			{
				prompt_backspace();
				index--;
			}
			continue;
		}
		if (c==27 && multicode_state==0) // handle multi-code keys
		{
			multicode_state=1;
			continue;
		}
		if (c==91 && multicode_state==1)
		{
			multicode_state=2;
			continue;
		}
		if (c==65 && multicode_state==2) // up arrow
		{
			int i;
			while (index>0)
			{
				prompt_backspace();
				index--;
			}
			for (i=0;oldbuf[i];++i)
			{
				putchar(oldbuf[i]);
				buf[i]=oldbuf[i];
			}
			index=i;
			continue;
		}
		else
			multicode_state=0;

		putchar(c); // echo the character
		buf[index++]=c;
		if (index>=sizeof(buf)-1) break;
		if (c=='\n') // enter key
			break;
		if (c==4) // Ctrl+D
			return EXIT;
  	}
  	if (index>0 && buf[index-1]=='\n') // trim newline from the end
  		index--;
  	buf[index++]=0; // null terminate string

  	strcpy(oldbuf, buf);

  	parse_command(buf, command);

  	//print_command(command); // DEBUG: uncomment for debugging

    // restore the old settings
    tcsetattr(STDIN_FILENO, TCSANOW, &backup_termios);
  	return SUCCESS;
}
int process_command(struct command_t *command);

//////////////// Iplementation OF HELPERS FOR COMMAND SHORT //////////////////////////////
// I use linked list for this particular command since it is easy to resize it.
// The structs for it are declared in the begining of the code as global variables and the stack(command list is initiliazd in the main function)
// We need a node struct which will hold three values; One is the name for the short and other is the pointer to the string which holds PATH and the last one is for holding next node(command)

cNode* searchCommand(char *name,commandList* list){ //This can be used both for checking if the name exist or taking the appropirate comment node
	if(list->size == 0){ // MEANS IT IS EMPTY
		return NULL;
	} else {
		cNode* currNode = list->head;
		while(currNode != NULL){
			if(strcmp(currNode->name,name)==0){
				return currNode; // Means it found it.(returns a pointer to it)
			}
			currNode = currNode->next;
		}
		return NULL;
	}
}

// I used a stacking implementation means that new commands would be added at top
int addToList(cNode *newCommand,commandList* list){
	cNode* inList = searchCommand(newCommand->name,list);
	if(inList== NULL){ //If the command(name) is not already in the list
		if(list->size == 0){
			list->head = newCommand;
			list->size = 1;
			newCommand->next = NULL;
			return 1;
		} else {
			cNode* temp = list->head;
			list->head = newCommand;
			list->size++;
			newCommand->next = temp;
			return 1;
		}
		return 0; // It means that there is a problem
	} else { // If the name is already in the list we need to overwrite
		char* temp = inList->path; //I need to hold the pointer of old path to free it
		inList->path = newCommand->path; // changing existing nodes path with the new one(overwrite)
		free(temp); // freeying old path
		free(newCommand); //freeying new command
		return 1;
	}
}

void freeCommands(commandList* list){ // This function frees all the commands in the list
	cNode* currNode = list->head;
	while(currNode!=NULL){
		cNode* temp = currNode->next;
		free(currNode->path);
		free(currNode);
		currNode = temp;
	}
}
int command_short(struct command_t *command){ //Returns 0 if succes otherwise -1
	if(command->arg_count > 1){
		if(strcmp(command->args[0],"set")==0){
			cNode* newC = malloc(sizeof(cNode));
			strcpy(newC->name,command->args[1]);
			char *pathC = malloc(sizeof(char)*PATH_MAX);
			if (getcwd(pathC,PATH_MAX)==NULL){
				free(pathC);
				free(newC);
				return -1;
			} else {
				newC->path = pathC;
				pathC=NULL;
				if(addToList(newC,commandStack) == 0) { //addtolist returns 1 if succes in adding
					free(newC->path);
					free(newC);
					return -1;
				}
				return 0;			
			}
		} else if(strcmp(command->args[0],"jump")==0){
			cNode* retCommand = searchCommand(command->args[1],commandStack);
			if(retCommand == NULL){ 
				return -1;
			} else {
				return chdir(retCommand->path);
			}
		} else { //First argument is invalid(not jump or set)
			return -1;
		}
	} else {
		return -1;
	}
}

//----------------------------------------------------------------------------------------------------------------------------//
///// ************* Implementation of Bookmark ********************/////
void printBookmarks(bookMark* bookmarkArray[]){ //Done
	int i;
	for(i=0;i<bookmarkCount;i++){
		if(bookmarkArray[i]->commandA!=NULL){
			printf("%d  %s ",i,bookmarkArray[i]->commandN);
			int k = 1;
			while(k < bookmarkArray[i]->countArg && bookmarkArray[i]->commandA[k]!=NULL){
				printf("'%s' ",bookmarkArray[i]->commandA[k]);
				k++;
			}
			printf("\n");
		}else {
			printf("%d  %s \n",i,bookmarkArray[i]->commandN);
		}
	}
}
void free_aBookmark(bookMark* aBook){ //Freeing the one bookmark's instances
	free(aBook->commandN);
	int i = 0; 
	for(i;i < aBook->countArg;i++){
		free(aBook->commandA[i]);
	}
	free(aBook);
}

void freeingBookmarks(bookMark* bookmarkArray[]){ //Freeing the bookmark array
	int i;
	for(i=0;i++;i<bookmarkCount){
		free_aBookmark(bookmarkArray[i]);
	}
}


int commandBookmark(struct command_t *command){
	if(strcmp(command->args[0],"-l")==0){ //Listing bookmarks
		printBookmarks(bookmarkArray);
		return 0; 
	} else if (strcmp(command->args[0],"-i")==0){ //executing a command at index i
		int index = atoi(command->args[1]); //Taking the index
		if(bookmarkCount <= index ){
			printf("Invalid index !\n");
			return -1;
		} else {
			int ret = execvp(bookmarkArray[index]->commandN, bookmarkArray[index]->commandA); //Executing the bookmark
			return 0;
		}
	} else if (strcmp(command->args[0],"-d")==0){ //Deleting the command at index i(and shifting)
		int index = atoi(command->args[1]);
		if(bookmarkCount <= index ){
			printf("Invalid index or bookmark is empty!\n");
			return 0;
		} else { // Means that It will remove one
			bookMark* d = bookmarkArray[index];
			// Deleting the command
			free_aBookmark(d);
			//shifting
			if(index == bookmarkCount-1){ //Means last index or first index with one element
				bookmarkArray[index] = NULL;
			} else {
				int i = index + 1;
				for (i;i<bookmarkCount;i++){
					bookmarkArray[i-1] = bookmarkArray[i];
				}
				bookmarkArray[bookmarkCount-1] = NULL; //after shift, we need to assig it to null
			}
			bookmarkCount--;//Decrementing bookmark count
		}
		return 0;
	} else { //Inserting new bookmark
		if(bookmarkCount<=30){ //It means there are enough empty space
			int count = command->arg_count;
			if(count > 1){
				if( (strlen(command->args[0])>2) && (strlen(command->args[count-1])>2) ){
					char* name = malloc(strlen(command->args[0])); //creating command Name
					strcpy(name,command->args[0]+1); //Getting rid of first "
					bookMark* newBook = malloc(sizeof(bookMark));
					newBook->commandN = name;
					char **cArg = malloc(sizeof(char *)*(count-1));
					int k;
					for(k=1;k<count-1;k++){ // Since last argument would have " at the and we need to determine it (if there is just one argument it does not enter for loop)
						char* arg = malloc(strlen(command->args[k])); //creating command arg intermediate arguments
						strcpy(arg,command->args[k]);
						cArg[k-1] = arg;
					}
					char* larg = malloc(strlen(command->args[count-1])); //creating command arg(last arg)
					strncpy(larg,command->args[count-1],strlen(command->args[count-1])-1); //Getting rid of last "
					larg[strlen(command->args[count-1])] = '\0';
					cArg[k-1] = larg;
					newBook->commandA = cArg;
					bookmarkArray[bookmarkCount] = newBook;
					
					bookmarkArray[bookmarkCount]->commandA=(char **)realloc(bookmarkArray[bookmarkCount]->commandA
																			, sizeof(char *)*(count+=1)); // Since FIRST ONE IS ACTUALLY command name
					/// Changing the  sturcture of bookmark->args to use it exec later
					// shift everything forward by 1
					for (int i=count-2;i>0;--i)
						bookmarkArray[bookmarkCount]->commandA[i]=bookmarkArray[bookmarkCount]->commandA[i-1];

					// set args[0] as a copy of name
					bookmarkArray[bookmarkCount]->commandA[0]=strdup(name);
					// set args[arg_count-1] (last) to NULL
					bookmarkArray[bookmarkCount]->commandA[count-1]=NULL;
					bookmarkArray[bookmarkCount]->countArg = count; // Assigning count size
					bookmarkCount++;
					
					return 0;
				} else {
					printf("Invalid command or argument! \n");
					return -1;
				}
			} else { //If there is just name/ not argument
				char* name = malloc(strlen(command->args[0])); //creating command Name 
				strcpy(name,command->args[0]);
				bookMark* newBook = malloc(sizeof(bookMark));
				newBook->commandN = name;
				bookmarkArray[bookmarkCount] = newBook;
				bookmarkCount++;
				return 0;
			}
		} else { // We need to print a message about bookmark is full
			printf("Bookmark list is full ! Delete some commands by bookmark -d (index) to add a new one \n");
			return -1;
		}
	
	}
}
//----------------------------------------------------------------------------------------------------------------------------//
///// ************* Implementation of Awesome Command********************/////
int read_dices_to_array(char* const filePath,char* diceArray[]){ //This function reads ascii dices in the txt file, and write them into diceArray by allocation memory for each 
	////////////// I took this code from stackoverflow
	FILE *file = fopen(filePath, "r");
	if(file!= NULL){
		int file_size;
		fseek(file, 0L, SEEK_END);
		file_size = ftell(file);
		rewind(file);
		char text[file_size + 1]; // I created it here to get rid of free operations
		fread(text, 1, file_size, file);
		text[file_size] = '\0';
		fclose(file);
		//////////////// Untill here
		char* dices = text;
		int j;
		int p=0; //pointer index (this will point to starting of each dice(dice 1,2,3,4,5,6))
		for(j=0;j<6;j++){
			char* aDice;
			int i = 0;
			char current = dices[p+i];
			while(current!='s'){ // While it standing in s
				i++; // when it standing on s it cannot be incremented so I initilizated it with 1
				current = dices[p+i];
			}
			diceArray[j] = malloc(sizeof(char)*(i-1));
			strncpy(diceArray[j],dices+p,i-1);
			diceArray[j][i-1] = '\0';
			p = p + i + 1; // It will be the beginning of the next one

		}
		return 0;
	} else {
		printf("%s cannot be opened ! \n",filePath);
		return -1;
	}

}
int freeDices(char* diceArray[]){ //This will free the memory which was allocated for dices
	int i;
	for(i=0;i<6;i++){
		free(diceArray[i]);
	}
	return 0;
}
int command_rollDice(struct command_t *command){ // rollDice guess(int) bet(int) example: rollDice 5 100 ; Here 5 is the guess, and 100 is the bet 
	if(command->arg_count > 1){
		int guess = atoi(command->args[0]);
		int bet = atoi(command->args[1]);
		if( (guess<7) && (guess>0)){
			int result = (rand() % 6) + 1;//generating random integer between 1-6
			printf("Your bet: %d$ --> You will get %d(x6)$ if you win ! \n",bet,bet*6);
			printf("Your Guess :%d  ***  %s \n",guess,diceArray[guess-1]);
			printf("Dice is being rolled... \n");
			printf("The result of the roll :%d *** \n",result);
			printf("%s \n",diceArray[result-1]);
			if(result == guess){ //win (shell is not happy)
				printf("You win :( Here, your money %d $! \n",bet*6);
			} else { //lose (shell is happy)
				printf("HAHA ! You Lose :) Maybe another time! \n");
			}
			return 0;
		} else {
			printf("Invalid number for guess! Guess should be between 1 and 6 ! \n");
			return -1;
		}
	} else {
		printf("Not enough argument(needs 2 int)! ExUsage: rollDice 1(guess) 50(bet) \n");
		return -1;
	}
	
}


//-----------------------------------REMIND ME-----------------------------------------------------------//
int remindMe(struct command_t *command){ // I am assuming the args are valid 
	int length = -1; // STARTED -1 BECASE IN FOR Loop I added 1 for each arg but there would be iretations -1 args
	int i = 1; 
	for(i; i < command->arg_count;i++){ // Calculating the message's length
		length = length + strlen(command->args[i]) + 1; // +1 here is for white space 
	}
	char message[length+1];
	int p =0;
	char space = ' ';
	for(i=1;i<command->arg_count;i++){
		int l = strlen(command->args[i]);
		strncpy(message+p,command->args[i],l);
		p = p + l; // this will update the adress that where the next arg will append
		if(i != command->arg_count-1){ // if it is not the last arg added
			strncpy(message+p,&space,1);
			p++;
		}
	}
	message[length] = '\0'; //Placing null at the and of the string
	// Untill here, the code is for creating a buffer which holds the message. Since command->args holds them seperately and the format is not useful for us.
	// Now the message has the format of full string --> For example: "It is a message !"
	
	
	// Preparing the content which will be written in a txt file to give it crontab command as an argument. And it will be okey after that step
	char* hour = strtok(command->args[0], "."); //Taking the hour part
	char* minute = strtok(NULL, "."); //Taking the minute part
	char buffer[400];
	int l1 = strlen("crontab -l | { cat; echo ");
	strncpy(buffer,"crontab -l | { cat; echo ",l1);
	strncpy(buffer+l1,message,1);
	l1++;
	strncpy(buffer+l1,minute,strlen(minute));
	l1 = l1 + strlen(minute);
	strncpy(buffer+l1,&space,1);
	l1++;
	strncpy(buffer+l1,hour,strlen(hour));
	l1 = l1 + strlen(hour);
	strncpy(buffer+l1," * * * env DISPLAY=:0 && export usr/bin/notify-send",strlen(" * * * env DISPLAY=:0 && export usr/bin/notify-send"));
	l1 = l1 + strlen(" * * * env DISPLAY=:0 && export usr/bin/notify-send");
	strncpy(buffer+l1,&space,1);
	l1++;
	strncpy(buffer+l1,message,length);
	l1 = l1 + length;
	strncpy(buffer+l1,message,1);
	l1++;
	strncpy(buffer+l1,";}",2);
	l1 = l1 + 2;
	buffer[l1] = '\0';
	printf("%s \n",buffer);
	
	// Untill here I preapre the content which will be written to crontab file with a exec crontab - command 
	// SInce i DO NOT HAVE TIME TO IMPLEMENT IT, I remove i t here. 
	
	return 0;
}

//----------------------------------------------------------------------------------------------------------------------------//

int process_command(struct command_t *command)
{
	int r;
	if (strcmp(command->name, "")==0) return SUCCESS;

	if (strcmp(command->name, "exit")==0)
		return EXIT;

	if (strcmp(command->name, "cd")==0)
	{
		if (command->arg_count > 0)
		{
			r=chdir(command->args[0]);
			if (r==-1)
				printf("-%s: %s: %s\n", sysname, command->name, strerror(errno));
			return SUCCESS;
		}
	}
	
	if(strcmp(command->name,"short")==0){ //If the command is short
		if(command_short(command)==-1)
			printf("Error in Command Short: %s not found ! \n",command->name);
		return SUCCESS;
	}
	
	if(strcmp(command->name,"bookmark")==0){ //If the command is bookmark
		if(commandBookmark(command)==-1)
			printf("Error in Command bookmark ! \n");
		return SUCCESS;
	}
	
	if(strcmp(command->name,"remindme")==0){
		remindMe(command);
	}
	
	if(strcmp(command->name,"rollDice")==0){ //If the command is my own command(your awesome command part)
		command_rollDice(command);
		return SUCCESS;
	}
	

	pid_t pid=fork();
	if (pid==0) // child
	{
		char path[500];
		/// This shows how to do exec with environ (but is not available on MacOs)
	    // extern char** environ; // environment variables
		// execvpe(command->name, command->args, environ); // exec+args+path+environ

		/// This shows how to do exec with auto-path resolve
		// add a NULL argument to the end of args, and the name to the beginning
		// as required by exec
		
		//printf("Before shift Args 0 : %s  args1: %s commandName:%s\n",command->args[0],command->args[1],command->name);
		
		// increase args size by 2
		command->args=(char **)realloc(
			command->args, sizeof(char *)*(command->arg_count+=2));

		// shift everything forward by 1
		for (int i=command->arg_count-2;i>0;--i)
			command->args[i]=command->args[i-1];

		// set args[0] as a copy of name
		command->args[0]=strdup(command->name);
		// set args[arg_count-1] (last) to NULL
		command->args[command->arg_count-1]=NULL;

		//execvp(command->name, command->args); // exec+args+path
		
		// Since all the unix commands always under /usr/bin/
		strcpy(path,"/usr/bin/");
		strcat(path,command->name);
		execv(path,command->args);
		//printf("Args 0 : %s  args1: %s arg2:%s\n",command->args[0],command->args[1],command->args[2]);
		exit(0);
		/// TODO: do your own exec with path resolving using execv()
	}
	else
	{
		if (!command->background)
			wait(0); // wait for child process to finish
		return SUCCESS;
	}

	// TODO: your implementation here

	printf("-%s: %s: command not found\n", sysname, command->name);
	return UNKNOWN;
}


int main()
{
	commandStack = malloc(sizeof(struct commandList)); // Allocating a memory for command stack.(will be used in short command)
	commandStack->size=0;
	commandStack->head=NULL;
	
	bookmarkCount = 0;
	
	char* const filePath="./dices.txt";
	read_dices_to_array(filePath,diceArray); //I call this here to prevent unneccesarry repeated readings when the command is executed each time 
	
	while (1)
	{
		struct command_t *command=malloc(sizeof(struct command_t));
		memset(command, 0, sizeof(struct command_t)); // set all bytes to 0
		
		int code;
		code = prompt(command);
		if (code==EXIT) break;

		code = process_command(command);
		if (code==EXIT) break;

		free_command(command);
	}
	//Freeing the memory for the command list and the commands
	freeCommands(commandStack);
	free(commandStack); 
	///////////////////////////////
	freeingBookmarks(bookmarkArray);
	/////////////////
	freeDices(diceArray); //Freeing dices
	//////////////////
	printf("\n");
	return 0;
}
