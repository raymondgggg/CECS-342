#include <iostream>
using namespace std;

class Employee{
    int age;

public:
    int getAge() { return age;}
    virtual void Speak()  = 0 ;
    virtual double GetPay() = 0;

};

class HourlyEmployee : public Employee {

    double hourly_rate;
    double hours;

public:
    virtual void Speak() override {cout << "I work for " << hourly_rate << " dollars per hour :(";};
};

class CommissionEmployee : public Employee{
    double sales_amount;

public:
    virtual void Speak() override {cout << "I make commission on " << sales_amount << " dollars in sales!";};
};

int main(){
    // Employee *e = new HourlyEmployee;

    // cout << e->GetPay() << endl;


    return 0;
}