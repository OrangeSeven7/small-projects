#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef struct arraylist {
    int *buffer;     // pointer to allocated memory
    int buffer_size; // the max number of integers the buffer can hold
    int length;      // the number of integers stored in the list
} arraylist;



////////////////////////////////////////////////////
//Functions that you need to implement:

void arraylist_add(arraylist *a, int x) {
    
    if (a == NULL){ return; }
    if (a->buffer == NULL){
	a->buffer = malloc(5*sizeof(int));
	if (a->buffer == NULL){ return; }
	a->buffer_size = 5;
	a->length = 0;
    }
    if(a->buffer_size < (a->length +1)){
     a->buffer_size*=2;
     int* y  = realloc(a->buffer, a->buffer_size*sizeof(int));
     if (y == NULL){return;}
     a->buffer = y;

    }
    a->buffer[a->length++] = x;
    return;
}

void arraylist_insert(arraylist *a, int index, int x) {
    
    if(a == NULL || index > a->length){ return; }
    if(a->buffer_size < a->length+1){
	a->buffer_size*=2;
	int* y  = realloc(a->buffer, a->buffer_size*sizeof(int));
     	if (y == NULL){return;}
     	a->buffer = y;
    }
    if(index == a->length){
	arraylist_add(a,x);
    }else{
    int i = a->length++;
    while(i > index){
	a->buffer[i] = a->buffer[i-1];
        i--;
    }
    a->buffer[i] = x;
    }
    return;

    // Storing the value x at the specified index in the arraylist.
    // Previously stored values should be moved back rather than overwritten.
}

void arraylist_free(arraylist *a) {
    free(a->buffer);
    return;
    // freeing any memory used by that arraylist
}

////////////////////////////////////////////////////


arraylist* arraylist_new() {
    arraylist *a = (arraylist *)malloc(sizeof(arraylist));
    a->buffer = NULL;
    a->buffer_size = 0;
    a->length = 0;

    return a;
}

void arraylist_remove(arraylist *a, int index) {
    int i;
    for(i = index; i < a->length-1; i++)
        a->buffer[i] = a->buffer[i+1];
    
    --a->length; 
}

int arraylist_get(arraylist *a, int index) {
    return(a->buffer[index]); 
}

void arraylist_print(arraylist *a) {
    printf("[");
    if (a->length > 0) {
        int i;
        for(i = 0; i < a->length-1; i++)
            printf("%d, ",arraylist_get(a,i));
        printf("%d", arraylist_get(a,a->length-1));
    }

    printf("]\n");
}

int main(int argc, char *argv[]) {
    // START OF TEST
    int i;
    arraylist *a = arraylist_new();

    arraylist_add(a, 0);
    arraylist_add(a, 1);
    arraylist_add(a, 2);
    arraylist_print(a);
    
    for (i = 0; i < a->length + 1; i++) {
        arraylist_insert(a, i, 100);
        printf("Insert position %d: ", i);
        arraylist_print(a);
        arraylist_remove(a, i);
    }
    printf("Clean: ");
    arraylist_print(a);

    arraylist_free(a);
    free(a);
    // END OF TEST
	
    return 0;
}
