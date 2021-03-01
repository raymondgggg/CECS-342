#include <stdio.h>


struct Employee {
    void** vtable;
    int age;
};

struct HourlyEmployee{
    void **vtable;
    int age;

    double hourly_rate;
    double hours;
};

struct ComissionEmployee{
    void **vtable;
    int age;

    double sales_amount;
};

void Speak_Hourly(struct Employee* e){
    struct HourlyEmployee *he = (struct HourlyEmployee*) &e;
    printf("I work for %f dollars per hour", he->hourly_rate);
}

double GetPay_Hourly(struct Employee* e){
    struct HourlyEmployee *he = (struct HourlyEmployee*) &e;
    return he->hourly_rate * he->hours;
}

 void Construct_Hourly(struct HourlyEmployee *he){
    
 }


int main(){
    printf("hello world\n");

    return 0;
} 