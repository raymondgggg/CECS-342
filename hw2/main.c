/*
 * Author: Raymond Antonio Guevara Lozano
 * Purpose: Illustrate the dynamic dispatch that is often taken for granted in OO languages :)
 * Language:  C
 * Compiler: GCC 
 * NOTE: Because this code was compiled with GCC on Linux, certain parts may not work with other compilers on other OS
 * (e.g scanf translates to scanf_s in visual studio on windows).
 */
#include <stdio.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

struct Employee { // base 'class' in this example.
    void** vtable;
    int age;
};

struct HourlyEmployee{ // 'inherits' from the Employee struct.
    void **vtable;
    int age;

    double hourly_rate;
    double hours;
};

struct CommissionEmployee{ // 'inherits' from the Employee struct.
    void **vtable;
    int age;

    double sales_amount;
};

struct SeniorSalesman{ // 'inherits' from the Employee struct.
    void **vtable;
    int age;

    double sales_amount;
};

/************************************************
 *  Speak_X() functions, where "X" is the type of Employee.
 *  Note: SeniorSalesman uses the Speak_Commission function.
 ***********************************************/
void Speak_Hourly(struct Employee* e){ 
    struct HourlyEmployee *he = (struct HourlyEmployee*) e;
    printf("I work for %.2f dollars per hour\n", he->hourly_rate);
}

void Speak_Commission(struct Employee* e){
    struct CommissionEmployee *ce = (struct CommissionEmployee*) e;
    printf("I make commission on %.2f dollars in sales!\n", ce->sales_amount);
}

/************************************************
 *  GetPay_X() functions, where "X" is the type of Employee.
 ***********************************************/
double GetPay_Hourly(struct Employee* e){
    struct HourlyEmployee *he = (struct HourlyEmployee*) e;
    return he->hourly_rate * he->hours;
}

double GetPay_Commission(struct Employee *e){
    struct CommissionEmployee *ce = (struct CommissionEmployee*) e;
    return (0.1 * ce->sales_amount) + 40000;
}

double GetPay_Senior(struct Employee *e){
    struct SeniorSalesman *se = (struct SeniorSalesman*) e;
    if (se->age >= 40)
        return 0.2 * se->sales_amount + 50000 + 0.05 * se->sales_amount;
    return 0.2 * se->sales_amount + 50000;
}

/************************************************
 *  vtables_X, where "X" is the type of Employee.
 ***********************************************/
void *vtable_Hourly[2] = {Speak_Hourly, GetPay_Hourly};
void *vtable_Commission[2] = {Speak_Commission, GetPay_Commission};
void *vtable_Senior[2] = {Speak_Commission, GetPay_Senior};

/************************************************
 *  Default Constructors to initialize "Objects."
 ***********************************************/
void Construct_Hourly(struct HourlyEmployee *he){
    he->vtable = vtable_Hourly;
    he->age = 0;
    he->hourly_rate = 0;
    he->hours = 0;
}

void Construct_Commission(struct CommissionEmployee* ce){
    ce->vtable = vtable_Commission;
    ce->age = 0;
    ce->sales_amount = 0;
}

void Construct_Senior(struct SeniorSalesman* se){
    se->vtable = vtable_Senior;
    se->age = 0;
    se->sales_amount = 0;
}

int main(){
    char usrInput = 0;
    struct Employee* e = (struct Employee*) malloc(sizeof(struct Employee)); 
    printf("Choose type of Employee:\n1. Hourly\n2. Commission\n3. Senior Salesman\n");
    scanf("%c", &usrInput);

    int usrInt = usrInput - '0';  // getting the actual int value of the char that was inputted by the user.

    while (isdigit(usrInput) == false || usrInt < 0 || usrInt > 3){ // Validate user input to make sure it's not out of bounds or non-numeric.
        printf("Please give valid input\n");
        printf("Choose type of Employee:\n1. Hourly\n2. Commission\n3. Senior Salesman\n");
        scanf("%c", &usrInput);
        usrInt = usrInput - '0';
    } 

    int age;
    printf("How old is the employee? ");
    scanf("%d", &age);

    if (usrInt == 1){
        struct HourlyEmployee *h = malloc(sizeof(struct HourlyEmployee));
        Construct_Hourly(h);
        double rate = 0, hours = 0.0; 
        printf("Please enter the rate and hours respectively, separated by a space: ");
        scanf("%lf %lf", &rate, &hours);

        // set the values that were entered by the user.
        h->age = age;
        h->hourly_rate = rate;
        h->hours = hours;
        // Memory Management.
        e = realloc(e, sizeof(struct HourlyEmployee)); // resize orginal pointer. (not needed for program to work)
        e = (struct HourlyEmployee*) h; // have Employee pointer of new size point to HourlyEmployee pointer.
    }
    else if (usrInt == 2){
        struct CommissionEmployee *c = malloc(sizeof(struct CommissionEmployee));
        Construct_Commission(c);
        double sales_amount = 0.0;
        printf("Please enter the sales amount: ");
        scanf("%lf", &sales_amount);

        //set the values that were entered by the user.
        c->age = age;
        c->sales_amount = sales_amount;
        // Memory Management.
        e = realloc(e, sizeof(struct CommissionEmployee)); // resize original pointer. (not needed for program to work)
        e = (struct CommissionEmployee*) c; // have Employee pointer of new size point to CommissionEmployee pointer.
    }
    else{
        struct SeniorSalesman *s = malloc(sizeof(struct SeniorSalesman));
        Construct_Senior(s);
        double sales_amount;
        printf("Please enter the sales amount: ");
        scanf("%lf", &sales_amount);

        //set the values that were entered by the user.
        s->age = age;
        s->sales_amount = sales_amount;
        // Memory management.
        e = realloc(e, sizeof(struct SeniorSalesman)); // resize original pointer. (not needed for program to work)
        e = (struct SeniorSalesman*) s; // have Employee pointer of new size point to SeniorSalesman.
    }

    // function pointer calls using vtable to allow for dynamic dispatch.
    ((void(*)(struct Employee*)) e->vtable[0])((struct Employee *) e); 
    double pay = ((double (*)(struct Employee*)) e->vtable[1])((struct Employee *) e);

    // GetPay_x value display.
    printf("This Employee has made $%.2f\n", pay);

    free(e);// deallocate remaining heap memory
    return 0;
}