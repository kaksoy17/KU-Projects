#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdbool.h> 
#include <sys/time.h>
#include <unistd.h>
#include <ctype.h>
#include "pthread_sleep.c"


// Thread for lanes, police_officer and intersection
pthread_t police_thread;
pthread_t lane_threads[4];

//Mutex and Condition Variables
pthread_cond_t policeCond = PTHREAD_COND_INITIALIZER; //This will be signalled by one of the lanes
pthread_cond_t cond = PTHREAD_COND_INITIALIZER; // This will be broadcasted by the police
pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER; // Mutex for a counter which shows all the lanes has done its job 
pthread_mutex_t mtx_sim = PTHREAD_MUTEX_INITIALIZER; // This is for simulation time. If the simulation is over, all threads terminates themselves


int simulationTimeCounter = 0; // If it reaches simulation time, we simply stop the simulation
int needUpdate;
int lanes_done; // Initially 1 since we only check North and the police

// Creating global variables and setting their default values
int simulationTime = 60;
double p = 0.4;
int carCount = 1;
unsigned int mySeed = 7;  // Seven is the best lucky number ! 
FILE *mainPointer; 
char directions[4]={'N','S','E','W'};

struct car {
	int carID;
	struct car* next;
	time_t arrivalTime;
	time_t crossTime;
};
struct lane { //Queue for each lane
	int count; //Holding how many cars in the lane
	int laneID; // 0 --> NORTH
	struct car* front; //For dequeueing 
	struct car* tail; //For enqueueing
};

struct lane *northLane, *southLane, *westLane, *eastLane;
struct lane* larray[4];

// Thread Functions
void *Lane(void* arg); 
void *Police(); 

int setArrivalTime(struct car* aCar);
int setCrossTime(struct car* aCar);


int enqueue(struct lane* lane,struct car* car){ //  Input: Takes a car enqueue it , Output: 0 if succes, otherwise -1 
	int cLane = lane->count;
	if(car != NULL){
		if(cLane == 0){
			lane->front = car;
		} else {
			lane->tail->next = car;
		}
		lane->tail = car;
		return 0;
	} else {
		return -1;
	}	
} 
void* dequeue(struct lane* lane){ //Takes a lane and returns the car in the beginning of the lane.(if no car presents returns null)
	struct car* r;
	r = lane->front;
	int cLane = lane->count;
	if(cLane == 0){
		return NULL;
	} else if (cLane == 1){ // if only one car present
		lane->front = NULL;
		lane->tail = NULL;
		lane->count--;
		return r;
	} else {
		lane->front = r->next; // all cars shift by one
		lane->count--;
		return r;
	}
}
//////////////////////////// End of the queue operations ///////////////

bool nextBool(double probability) { // Generates True or False according to the probability. 
    double m = probability *  ((double) RAND_MAX + 1); //Taking the upper bound (purpose of +1 is (if the number is the max one, it should not return 0 if p == 1))
	return rand() <  m;
}

void *Lane(void* lanep){ //The function which each lane uses
	struct lane *lane_ptr = (struct lane*) lanep;
	int myId = lane_ptr->laneID; //Taking the id(indicates that which lane's thread is executing) --> Would be useful for later cases
	
	double prob = p;
	if(myId == 0) { //It means this is NORTH 
		prob = 1.0 - p;
	} else {
 		prob = p;
	}

	pthread_mutex_lock(&mtx_sim);
	while(simulationTimeCounter < simulationTime){ //If simulation is not over yet
		pthread_mutex_unlock(&mtx_sim);
		
		pthread_mutex_lock(&mtx);
		pthread_cond_wait(&cond,&mtx);
		
		bool next = nextBool(prob); //Whether we create a car or not
		if( (myId ==0) && (next == false)){ //20 SECOND CASE
			needUpdate = 3; //It does not do anything for 20 seconds
			if(lanes_done == 3){
				pthread_cond_signal(&policeCond);
			}
			
			//Checking whether the simulation will be over when north get awake (if it is over directly terminate)
			pthread_mutex_lock(&mtx_sim);

			if((simulationTime-simulationTimeCounter) >= 21){
				pthread_mutex_unlock(&mtx_sim);
				pthread_mutex_unlock(&mtx);
				pthread_sleep(20); //Sleeps for 20 seconds
			} else {
				pthread_mutex_unlock(&mtx);
				pthread_mutex_unlock(&mtx_sim);
				pthread_exit(0);
			}

			next = true; //After 20 seconds, the new car definitely arrives
				
			//We wait to for the next signal to continue
			pthread_mutex_lock(&mtx);
			pthread_cond_wait(&cond,&mtx);
			needUpdate = 4; // Set needUpdate back to 4 since north is back
		}
		
		//CriticalSection of LANES
		if(next){ //If returns true means create a car
			struct car* newCar = (struct car *) malloc(sizeof(struct car));
			newCar->carID = carCount; //Setting the car's id
			newCar->next = NULL;
			carCount++; //Preparing idcount for next car creation
			setArrivalTime(newCar);
			enqueue(lane_ptr,newCar); //Add into the lane
			lane_ptr->count++;
		}

		lanes_done++; // Incrementing lanes_done before releasing the lock of it
		if(lanes_done >= needUpdate){ // If all lanes_done, signal the police	
			pthread_cond_signal(&policeCond); 
		}
		// END OF CRITICAL SECTION
		pthread_mutex_unlock(&mtx);
		pthread_mutex_lock(&mtx_sim);
	}
	pthread_mutex_unlock(&mtx_sim); // In the last iteration of the while, it will not enter the while so it cannot be able to unlock it
	pthread_exit(0);
}

int findMax(int *array,int n){ //Finds the index of maximum number in an array
	int k=0;
	int max = array[0];
	for(int i=1;i<n;i++){
		if(array[i] > max){ //Since we go 0 to n-1 and ">" use instead of >=, in case of tie lower index would have the higher prioirity 
			max = array[i];
			k=i;
		}
	}
	return k;
}
 
// N > E > S > W -- In case of tie
int chooseNextLane(int currId,time_t currTime){ //This function takes the currId as  the ID of the current and returns the next lane which uses the intersection
	if(currId == -1){ // This will be work just initially
		return 0;
	}
	int currCars = larray[currId]->count;
	int array[4]={larray[0]->count,larray[2]->count,larray[1]->count,larray[3]->count}; //,0->north, 1->east, 2->south,3->west instead of N,S,E,W to provide N > E > S > W 
	int maxLane = findMax(array,4);
	// Taking waiting times of the oldest cars in other lanes 
	struct tm carTimeInfo; 
	struct tm currInfo = *localtime(&(currTime));
	struct car* aCar;
	int minDifference;
	int waitTime = 0;
	int waitTimeArray[4]; // ,0->north, 1->south, 2->east,3->west
	for(int i = 0;i<4;i++){
		if(i != currId){
			aCar = larray[i]->front; //The car in the front would be the oldest one
			if(aCar !=NULL ){
				carTimeInfo = *localtime(&(aCar->arrivalTime));
				minDifference = currInfo.tm_min - carTimeInfo.tm_min;
				if(minDifference ==0){ //If they are in the same minute
					waitTime = currInfo.tm_sec - carTimeInfo.tm_sec;
				}else{
					waitTime = (currInfo.tm_sec + 60*minDifference) - carTimeInfo.tm_sec;
				}
				waitTimeArray[i] = waitTime;
			} else {
				waitTimeArray[i] = 0; //Means no car in the lane
			}
		} else {
			waitTimeArray[i] = -1; //Means we are in the same lane
		}
	}
	int temp = waitTimeArray[1]; //to provide N > E > S > W like in the findMax among the array
	waitTimeArray[1] = waitTimeArray[2];
	waitTimeArray[2] = temp;
	
	int maxIndex = findMax(waitTimeArray,4); //Oldest car index(in which lane)
	int maxWaitTime = waitTimeArray[maxIndex]; //The oldest car maximum wait time
	////////////
	
	if((currCars != 0) && (array[maxLane] < 5) && (maxWaitTime < 20)){ //If no more 5 cars or no waiting greater than 20 seconds
		return currId;
	}
	
	if(maxWaitTime >= 20){ //This directly handles the case that they both true (b an c). Also if only maxWaitTime is true, this also handles
		maxLane = maxIndex;
	}//If it does not enter this if, it means maxWaitTime < 20 and maxLane >=5 
	
	if(maxLane == 1){ //SInce we change order of S and E, we need to fix it back
		return 2;
	} else if (maxLane == 2){
		return 1;
	} else {
		return maxLane;
	}
}

void printCommandLine(){
	time_t t = time(NULL);
	struct tm timeInfo = *localtime(&(t)); 
	printf("At time %d:%d:%d \n",timeInfo.tm_hour,timeInfo.tm_min,timeInfo.tm_sec);
	int cN = larray[0]->count;
	int sN = larray[1]->count;
	int eN = larray[2]->count;
	int wN = larray[3]->count;
	printf("   %d   \n",cN);
	printf("%d     %d\n",wN,eN);
	printf("   %d   \n",sN);
}

void *Police(){
	int currentId = -1;
	int passed_car = -1;
	pthread_mutex_lock(&mtx_sim);
	struct tm arrival;
	struct tm cross;
	while(simulationTimeCounter < simulationTime+1){
		pthread_mutex_unlock(&mtx_sim);

		pthread_mutex_lock(&mtx);
		while(lanes_done != needUpdate){ //If lanes_done == needUpdate, it means all the lanes have done their jobs 
			pthread_cond_wait(&policeCond,&mtx); //If not, this will wait for a lane to signal the police
		}
		
		//Creatin time_t which hold current time
		time_t currTime = time(NULL);
		//Choosing which lane to pass
		currentId = chooseNextLane(currentId,currTime);
		struct car* passed_car = (struct car*) dequeue(larray[currentId]); //Dequeing the car
		if(passed_car != NULL){
			int passsed_carId = passed_car->carID;
			
			arrival = *localtime(&(passed_car->arrivalTime)); //Taking the arrival time
			cross = *localtime(&(currTime)); //Taking the cross time
			//Writing to log file
			int waitTime=-1;
			int minDiff = cross.tm_min - arrival.tm_min;
			if(minDiff==0){ //If they are in the same minute
				waitTime = cross.tm_sec - arrival.tm_sec;
			} else {
				waitTime = (cross.tm_sec + 60*minDiff)- arrival.tm_sec;
			}
			fprintf(mainPointer,"%d		%c	 %d:%d:%d	 %d:%d:%d	  %d \n",passsed_carId,directions[currentId],arrival.tm_hour,arrival.tm_min,arrival.tm_sec,cross.tm_hour,cross.tm_min,cross.tm_sec,waitTime);
			free(passed_car); //Freeing the car
		}
		
		printCommandLine();
		//Critical Section of the police
		pthread_mutex_unlock(&mtx);
		pthread_sleep(1); //Sleeps for a second,
		pthread_mutex_lock(&mtx);
		lanes_done = 0;
		pthread_mutex_unlock(&mtx);
		pthread_cond_broadcast(&cond); //BroadCasting all of the threads which wait for the condition
		// End of the Critical Section //
		
		pthread_mutex_lock(&mtx_sim);
		simulationTimeCounter++;
	}
	pthread_mutex_unlock(&mtx_sim); //Unlocking the mutex for other threads to check wheter the simulation is over
	pthread_exit(0);
} //End of the police

int setArrivalTime(struct car* aCar){
  	time_t t = time(NULL);
	aCar->arrivalTime = t;
	return 0;
}

int laneInitializer(){
	//Allocating memory
	struct car* carN = (struct car *) malloc(sizeof(struct car));
	struct car* carS = (struct car *) malloc(sizeof(struct car));
	struct car* carW = (struct car *) malloc(sizeof(struct car));
	struct car* carE = (struct car *) malloc(sizeof(struct car));
	northLane = (struct lane *) malloc(sizeof(struct lane));
	southLane = (struct lane *) malloc(sizeof(struct lane));
	eastLane  = (struct lane *) malloc(sizeof(struct lane)); 
	westLane  = (struct lane *) malloc(sizeof(struct lane));
	larray[0] = northLane;
	larray[1] = southLane;
	larray[2] = eastLane;
	larray[3] = westLane;
	struct car* carray[4] = {carN,carS,carE,carW};
	
	time_t t = time(NULL); //Getting time of the day
  
	for (int i=0; i < 4 ; i++){ //Adding the cars to the lanes where they belong to
		carray[i]->arrivalTime = t;
		carray[i]->carID = carCount;
		carray[i]->next = NULL;
		carCount++;
		enqueue(larray[i],carray[i]);
		larray[i]->laneID = i; // Setting lane's id s / 0--> North, 1--> SOuth, 2--> East, 3-->West
		larray[i]->count = 1;
	}

}

int freeAllocated(struct lane* array[]) { //This func frees all the lanes and the cars which remained after the simulation is over
	for(int i=0;i<4;i++){
		struct lane* aLane = array[i];
		struct car* curr = aLane->front;
		while(curr != NULL) {
			struct car* temp1 = curr->next;
			free(curr);
			curr = temp1;
		}
		free(aLane);
	}
	printf("Memory is freed :) \n");
}


int main(int argc, char *argv[]){
	
	// Handling command line arguments
	if(argc == 1){
		printf("Simulation starts with default values ; ** Simulation time = %d seconds *** Probability p = %.2f ** \n",simulationTime,p);
	} else if (argc==3){ // It gives only one of them
		if(strcmp(argv[1],"-s") == 0){
			simulationTime = atoi(argv[2]);
		} else if (strcmp("-p",argv[1])==0){
			p = (double) atof(argv[2]);			
		} else {
			printf("Illegal argumentation. Example Usage --> -s 200 -p 0.4 \n");
		}
	} else if (argc == 5) { //If the user gives both -s and -p
		simulationTime = atoi(argv[2]);
		p = (double) atof(argv[4]);
		printf("Simulation starts with; ** Simulation time = %d seconds *** Probability p = %.2f  ** \n",simulationTime,p);
	} else {
		printf("Some arguments are mising : Example Usage --> -s 200 -p 0.4 \n");
	}
	if(p>1) {p = 0.4;}
	
	//Initializing the parameters
	srand(mySeed);
	needUpdate = 4; //This needs to be changed accordingly with number of active lanes
	lanes_done = needUpdate;
	
	//////// Thread Creation ////////
	int lane_p;
	laneInitializer(); //Initializing the lanes with one car in each

	mainPointer = fopen("part2Log.txt", "w");
	fprintf(mainPointer,"CarID	DIRECTION	ARRIVAL-TIME	CROSS-TIME	  Wait Time\n");
	
	
  	int police_p = pthread_create(&police_thread, NULL, Police, NULL);
    
	for (int i= 0; i < 4; i++)
	{
		lane_p = pthread_create(&lane_threads[i], NULL, Lane, larray[i]);
		if (lane_p){ // Error case for lane thread creation
			exit(-1);
		}
	}

	for (int i = 0; i< 4; i++)
	{	
		pthread_join(lane_threads[i], NULL);
	}
	pthread_join(police_thread, NULL);

	// Freeing memory
	freeAllocated(larray);
	pthread_exit(NULL);
}
