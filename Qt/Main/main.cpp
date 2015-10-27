#include "basic.h"

const int Robot_Number = 20;
static const float draw_scale = 20;
double total_time = 0.0;
bool avoid_container = true;

const int show_time_rate = 10;
unordered_map<int, Product> products;//list of product
unordered_map<int, Terminal> terminals;
Signal signals;

int main(int argc, char * argv[])
{

	//generate random products
	for (int index = 0; index < Product_number; index++){
		int number1 = Product_min_number + rand() % (Product_max_number - Product_min_number);
		products.insert(pair<int, Product>(index, Product(index, 0, rand() % int(Product_max_weight), number1, 2 * number1, 3 * number1)));
	}
	while (true){
		cout << "Nothing" << endl;
	}
	return 0;
}

