#include "basic.h"

int mod(int x, int y){
	int q = x / y;
	if (x < 0) x -= (q - 1)*y;
	return x%y;
}

void update_time(string& msg){
	msg.clear();
	total_time += time_step;
	second += time_step;
	if (abs(second-60)< time_step/2){
		second = 0;
		minute++;
	}
	if (minute == 60){
		minute = 0;
		hour++;
	}
	stringstream f;
	f << hour;
	msg = "time : " + f.str();
	stringstream f2;
	f2 << minute;
	msg =msg+ " "+ f2.str();
	stringstream f3;
	f3 << second;
	msg =msg+" "+f3.str();
}

/***************************************************/
bool Terminal::random_order(){
	int item_number = 1 + rand() % 3;
	unordered_map<int, int> items; 
	for (int i = 0; i < item_number; i++){
		int index = rand() % Product_number;
		int number = 1+rand() % 3;
		items.insert(pair<int, int>(index, number));
	}
		
	orders.push(Order(items, inCome, true, space_id));
	return true;
}

