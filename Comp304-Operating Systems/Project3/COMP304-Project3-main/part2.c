/**
 * virtmem.c 
 */
#include <stdbool.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>

#define TLB_SIZE 16
#define PAGES 1024
#define PAGE_MASK 0x3ff

#define PAGE_SIZE 1024
#define OFFSET_BITS 10
#define OFFSET_MASK 0x3ff

#define MEMORY_SIZE 256 * PAGE_SIZE //We are using 256 pages instead of 1024

// Max number of characters per line of input file to read.
#define BUFFER_SIZE 10

struct tlbentry {
  unsigned char logical;
  unsigned char physical;
};

// TLB is kept track of as a circular array, with the oldest element being overwritten once the TLB is full.
struct tlbentry tlb[TLB_SIZE];
// number of inserts into TLB that have been completed. Use as tlbindex % TLB_SIZE for the index of the next TLB line to use.
int tlbindex = 0;

// pagetable[logical_page] is the physical page number for logical page. Value is -1 if that logical page isn't yet in the table.
int pagetable[PAGES]; //1024 pages
int counttable[PAGES]; // count table to find least recently used
int tlb_Counttable[TLB_SIZE]; //This works as a history, where the least recently used is minimum one


signed char main_memory[MEMORY_SIZE]; // 256 PAGES

// Pointer to memory mapped backing file
signed char *backing;

// Pointer to the file which is used as a log file(deneme.txt)
//FILE *main_file;

int max(int a, int b)
{
  if (a > b)
    return a;
  return b;
}

/* Returns the physical address or index from TLB or -1 if not present. */
int search_tlb(unsigned char logical_page, char* argum) {
    for(int i=0; i<TLB_SIZE; i++){
      if(tlb[i].logical == logical_page){
		  if (strcmp("0",argum)==0){ // FIFO
			  return tlb[i].physical;
		  } else if (strcmp("1",argum)==0){ // LRU
			  return i;
		  }
		}
	}
	return -1;
}

/* Adds the specified mapping to the TLB, replacing the oldest mapping (FIFO replacement). */
void add_to_tlb(unsigned char logical, unsigned char physical) {
    struct tlbentry tlbnew;
    tlbnew.logical = logical;
    tlbnew.physical = physical;

    int new_tlbindex = tlbindex % TLB_SIZE; //next index
    tlb[new_tlbindex] = tlbnew;
    tlbindex++;
}

// LRU implementation on TLB
void add_to_tlb_lru(unsigned char logical, unsigned char physical,int current_counter) {
	//finding which one to replace
	int min = 9999999; //Minimum would be -1 in the counttable
	int minIndex = -1;
	for(int k=0; k<TLB_SIZE; k++){
      if(tlb_Counttable[k] < min){
		  min = tlb_Counttable[k];
		  minIndex = k;
      } 
    }
	//Adding to tlb table
	struct tlbentry tlbnew;
    tlbnew.logical = logical;
    tlbnew.physical = physical;
    tlb[minIndex] = tlbnew;
	tlb_Counttable[minIndex] = current_counter;
	
	//fprintf(main_file,"%d is replaced where counter was : %d , with the new counter : %d \n",minIndex,min,current_counter);

}

/* This function updates tlb and page table after replacement*/
// Changes the tlb entry with the new one, if there is the replaced one
// Makes -1 physical adress of replaced one in the page table
void updateTables(int replaceAdres,int replacerLogical){
  	int replaced_logicalPage;
	//Updating page table
	int i;
	for (i = 0; i < PAGES; i++) {
    	if(pagetable[i] == replaceAdres){
			//printf("I change p. adress of  logical adress : %d ! Old pyhsical adress : %d  new : %d \n",i,replaceAdres,-1);
			replaced_logicalPage = i;
			pagetable[i] = -1; //Making it -1
		}
  	}
	
	//Updating TLB address as -1
	for(int k=0; k<TLB_SIZE; k++){
      if(tlb[k].logical == replaced_logicalPage){
		  tlb[k].physical = -1;
          k = TLB_SIZE; 
      } 
    }
}

// This updates TLB, pagetable and counttable after LRU replacements
void updateTablesLRU(int replacedLogical){
	pagetable[replacedLogical] = -1; // makes replaced ones as -1
	counttable[replacedLogical] = -1; //  makes replaced ones as -1
	//Updating TLB address as -1
	for(int k=0; k<TLB_SIZE; k++){
      if(tlb[k].logical == replacedLogical){
		  tlb[k].physical = -1;
		  tlb_Counttable[k] = -1;
          k = TLB_SIZE; //This will breaks the for loop 
      } 
    }
}

int findLeastRecent(){ // a basic algorithm to find smallest count in an array of int
	int min = 100000000;
	int i,index = 0;
	for (i = 0; i < PAGES; i++) {
		if((pagetable[i] != -1) && (counttable[i]<min)){ // since -1 will be the smallest and we don't need those pages since they do not have entry in the physical page
			index = i;
			min = counttable[i]; // new min
			//fprintf(main_file,"Min: %d Index: %d \n",min, index);
		}
	}
	return index;
}

int main(int argc, const char *argv[])
{
  if (argc != 5) {
    fprintf(stderr, "Usage ./virtmem backingstore input\n");
    exit(1);
  }
  
  const char *backing_filename = argv[3]; 
  int backing_fd = open(backing_filename, O_RDONLY);
  backing = mmap(0, MEMORY_SIZE, PROT_READ, MAP_PRIVATE, backing_fd, 0); 
  
  const char *input_filename = argv[4];
  FILE *input_fp = fopen(input_filename, "r");
  
  // Fill page table entries with -1 for initially empty table.
  int i;
  for (i = 0; i < PAGES; i++) {
    pagetable[i] = -1;
	counttable[i] = -1;
	if(i<16)
	  tlb_Counttable[i]=-1;
  }
  
  // Character buffer for reading lines of input file.
  char buffer[BUFFER_SIZE];
   
	// Data we need to keep track of to compute stats at end.
  int total_addresses = 0;
  int tlb_hits = 0;
  int page_faults = 0;
  int current_count = 0;

  //main_file = fopen("deneme.txt", "w");
	
  if(strcmp("0",argv[2]) == 0){ // Here, we are implementing FIFO
	printf("FIFO Replacement..\n");
		 
		  // Number of the next unallocated physical page in main memory
		  unsigned char nextPlace = 0;
		  bool replacement = false; //This would be true when the first replacement occur

	  	while (fgets(buffer, BUFFER_SIZE, input_fp) != NULL) {
			total_addresses++;
			int logical_address = atoi(buffer);

			/* TODO 
			/ Calculate the page offset and logical page number from logical_address */
			int offset = logical_address & OFFSET_MASK;
			int logical_page = (logical_address >> OFFSET_BITS) & PAGE_MASK;
			///////

			int physical_page = search_tlb(logical_page, argv[2]);
			if (physical_page != -1) {
			  // TLB hit
			  tlb_hits++;
			} else {
			  // TLB miss
			  physical_page = pagetable[logical_page];

			  if (physical_page == -1) {
				// Page fault
				// Operating System Concepts Page: 403
				page_faults++;
				printf("----------------------------------------------\n");
				//fprintf(main_file,"----------------------------------------------\n");
				printf("Page fault Occured !!! Fault number: %d \n", page_faults);	
				//fprintf(main_file, "Page fault Occured !!! Fault number: %d \n", page_faults);	

				//becomes a free page (4)
				physical_page = nextPlace;
				printf("New Page will be loaded into : %d \n", physical_page);  
				//fprintf(main_file,"New Page will be loaded into : %d \n", physical_page);  
				printf("----------------------------------------------\n");
				//fprintf(main_file,"----------------------------------------------\n");
				
				// Update page table for the replaced one's adress(makin it -1), the TLB (if the replaced one is there, make its pyhsical adress -1) 
			 	if(replacement == true){
					updateTables(physical_page,logical_page);
				}
			
				//copied to main memory (5)
				memcpy(main_memory + (physical_page * PAGE_SIZE), backing + (logical_page * PAGE_SIZE), PAGE_SIZE);

				//insert into the pagetable (6)
				pagetable[logical_page] = physical_page;
				
				// Updating next place(frame) where we put a page into memory 
				if(nextPlace == 255 ){ //After first 255 step, it means the memory is full now so we need starting to replace
					replacement = true; //It will shows there will be replacement after this line
				}
				nextPlace++; //We do not take the module or -255 since it is a unsigned char it directly goes to 0
			  
			  }
			  //tlb modified
			  add_to_tlb(logical_page, physical_page);
			}

			int physical_address = (physical_page << OFFSET_BITS) | offset;
			signed char value = main_memory[physical_page * PAGE_SIZE + offset];

			printf("Virtual address: %d Physical address: %d Value: %d\n",logical_address, physical_address, value);
			//fprintf(main_file, "Virtual address: %d Physical address: %d Value: %d\n",logical_address, physical_address, value);
		  }
	} else if (strcmp("1",argv[2])==0) { // Here, we are implementing LRU
		printf("LRU Replacement..\n");
		 
		// Number of the next unallocated physical page in main memory
		unsigned char nextPlace = 0;
		bool replacement = false; //This would be true when the first replacement occur

        while (fgets(buffer, BUFFER_SIZE, input_fp) != NULL) {
			total_addresses++;
            current_count++;
			int logical_address = atoi(buffer); 
			
			/* TODO 
			/ Calculate the page offset and logical page number from logical_address */
			int offset = logical_address & OFFSET_MASK;
			int logical_page = (logical_address >> OFFSET_BITS) & PAGE_MASK;
			///////

			int tlb_index = search_tlb(logical_page, argv[2]);
			int physical_page = -1;
			
			counttable[logical_page] = current_count; //Assigning or updating current count to countable entry to have the time when it is added

			if (tlb_index != -1) {
			  // TLB hit
			   physical_page = tlb[tlb_index].physical;
			   tlb_Counttable[tlb_index] = current_count; //Updating the count variable in the table
			   tlb_hits++;
			} else {
			  // TLB miss
			  physical_page = pagetable[logical_page];
			
			  if (physical_page == -1) {
				// Page fault
				// Operating System Concepts Page: 403
				page_faults++;
				printf("----------------------------------------------\n");
				//fprintf(main_file,"----------------------------------------------\n");
				printf("Page fault Occured !!! Fault number: %d \n", page_faults);	
				//fprintf(main_file, "Page fault Occured !!! Fault number: %d \n", page_faults);	

				// Update page table for the replaced one's adress(makin it -1), the TLB (if the replaced one is there, make its pyhsical adress -1) 
			 	int least_logical;
				if(replacement == true){ //If the memory full, we start replacements
					least_logical = findLeastRecent();
					physical_page = pagetable[least_logical]; //Taking the pyshical page adress which was belonging to replaced one
					updateTablesLRU(least_logical);//Replaced logical, replacer logical page
				} else {
					physical_page = nextPlace; //If the memory is not full yet, we use the space
				
				}		  
				  
				printf("New Page will be loaded into : %d \n", physical_page);  
				//fprintf(main_file,"New Page will be loaded into : %d \n", physical_page);  
				printf("----------------------------------------------\n");
				//fprintf(main_file,"----------------------------------------------\n"); 
			
				//copied to main memory (5)
				memcpy(main_memory + (physical_page * PAGE_SIZE), backing + (logical_page * PAGE_SIZE), PAGE_SIZE);

				//Update page table for the new entry
				pagetable[logical_page] = physical_page;
				
				// Updating next place(frame) where we put a page into memory 
				if(nextPlace == 255 ){ //After first 255 step, it means the memory is full now so we need starting to replace
					replacement = true; //It will shows there will be replacement after this line, After this being true, we will not use nextPlace anymore 
				}
				nextPlace++;
			  
			  }
			  //tlb modified
			  add_to_tlb_lru(logical_page, physical_page,current_count);
			}

			int physical_address = (physical_page << OFFSET_BITS) | offset;
			signed char value = main_memory[physical_page * PAGE_SIZE + offset];

			printf("Virtual address: %d Physical address: %d Value: %d\n",logical_address, physical_address, value);
			//fprintf(main_file, "Virtual address: %d Physical address: %d Value: %d\n",logical_address, physical_address, value);
		  }
	
	} else{
		printf("There is a problem with the arguments ! \n ");
		exit(0);
	}
  printf("\nNumber of Translated Addresses = %d\n", total_addresses);
  printf("Page Faults = %d\n", page_faults);
  printf("Page Fault Rate = %.3f\n", page_faults / (1. * total_addresses));
  printf("TLB Hits = %d\n", tlb_hits);
  printf("TLB Hit Rate = %.3f\n", tlb_hits / (1. * total_addresses));

  return 0;
}