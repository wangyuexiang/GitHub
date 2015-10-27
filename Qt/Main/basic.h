#ifndef BASIC_H
#define BASIC_H


#include "opencv2/opencv.hpp"
#include "opencv2/calib3d.hpp"

#include <fstream>
#include <vector>
#include <cv.hpp>
#include <time.h>
#include <numeric>
#include <queue>
#include <unordered_map>

//#define SHOW_DETAIL
using namespace cv;
using namespace std;

/*********************************************/
// time information
const static float time_step = 0.02f; //unit= second
extern double total_time;
static int hour = 8;
static int minute = 0;
static float second = 0;

// global environment variables
const static float pi = 4 * atan(1);
const static int DIMENSION = 2;

// space standard variables 
const static float Space_w = 1.5f;
const static float Space_l = 1.5f;

// robot variables;
const static float Speed_max = 2.0f;
const static float Speed_angle = pi / 2;
const static float Battery_max = 10800.0f;
const static float Consum_empty = 1 * time_step;//150 kg
const static float Consum_load = 3.5 * time_step;//150 + 375 kg

const static float Robot_h = 0.3f;
const static float Robot_w = 0.4f;
const static float Robot_l = 0.6f;

extern bool avoid_container;

// container standard variables
const static float Container_h = 3.0f;
const static float Container_w = 1.0f;
const static float Container_l = 1.0f;
const static float Container_capacity = 300.0f;
const static int item_cat_max = 4;

// map variables
const static int Map_width = 26;
const static int Map_length = 24;

//product variables
const static float Product_max_weight = 120.0f;
const static int Product_min_number = 5;
const static int Product_max_number = 20;
//terminal variables
static int max_handle = 10;

// storage variables
const static float Storage_saturation = 0.6f;
const int Product_number = 1000;

// order variables
const static int max_item_number = 5;

const static float Order_time_step = 8.0f;//secondes
const static float processing_time_step = 5.0f;//secondes

/*********************************************/
enum Action_key{ random_move, move, loading, release, wait_permission };//for robots

enum Signal_key{
	none,
	send_cont, rand_move,
	add, add_permitted, minus, minus_permitted, //load or remove a product at a terminal
	reserve_path, observe_path, cancel_path, path_permitted, erase_reserve
};

enum Status_R{
	available,
	working,
	in_error,
	charging,
	unknow
};

enum Status_T{
	online,
	problem,
	offline,
	control // super terminal
};
/****************************************************/

struct Product{
	int item;     // which kind of product,  not the id;
	int category; // optional now;
	float  weight;   // 

	int max_number[3]; // the maximum item number for small, 

	Product(){
		item = -1;
		category = -1;
		float weight = 0;
		max_number[0] = 0;
		max_number[1] = 0;
		max_number[2] = 0;
	}
	Product(int i, int c, float w, int number1, int number2, int number3){
		item = i;
		category = c;
		weight = w;
		max_number[0] = number1;
		max_number[1] = number2;
		max_number[2] = number3;
	}
};
extern unordered_map<int, Product> products;//list of product

/****************************************************/
enum terminal_flag{
	new_ord,
	cancel_ord,
	close,
	open

};
struct Message_T{
	terminal_flag flag;
	// todo : you can DIY this part.
};
/****************************************************/
struct Message{

	string sender_type; // "T" terminal,  "S" System, "R" Robot
	int send_id;
	int rece_id;

	Signal_key key;

	double time;
	int id1;
	int id2;
	Message(string mytype, int myid, int toid, Signal_key k, double t, int id1 = -1, int id2 = -1){
		sender_type = mytype;
		send_id = myid;
		rece_id = toid;

		key = k;
		time = t;
		this->id1 = id1;
		this->id2 = id2;
	}
};

//extern unordered_map<int, pair<Signal_key, double> > signals;

struct Signal{
	vector<Message>  to_system;
	vector<Message>  to_robot;
	vector<Message>  to_terminal;

	bool hasToRobotSignal(int id){
		for (vector<Message>::const_iterator it = to_robot.begin(); it < to_robot.end(); it++){
			if (it->rece_id == id) return true;
		}
		return false;
	}


};
extern Signal signals;

/*****************************************************/
struct Order{
	size_t priority;
	unordered_map<int, int> items;

	bool incoming; // incoming or outgoing
	int area;

	Order(){
		area = NULL;
	}

	Order(unordered_map<int, int> & i, size_t p, bool in, int a){
		items = i;
		priority = p;
		incoming = in;
		area = a;
	}

	friend bool operator< (Order n1, Order n2)
	{
		return n1.priority < n2.priority;
	}

};

/*****************************************************/
struct Terminal{
	int id;
	int space_id;
	priority_queue<Order> orders;
	bool inCome;

	long order_time_count;

	list<Order> handle;
	Status_T status;

	bool lock;

	Terminal(){
		order_time_count = 0;
		id = -1;
		space_id = -1;
		inCome = true;
		status = Status_T::offline;
		lock = false;
	}
	Terminal(int id, int space_id, bool in, Status_T status){
		order_time_count = 0;
		this->id = id;
		this->space_id = space_id;
		inCome = in;
		this->status = status;
		lock = false;
	}

	bool random_order();

	void process(){
		order_time_count += time_step;
		if (lock == false && order_time_count >= Order_time_step){
			random_order();
			order_time_count = 0;
			lock = true;
		}

		// handle terminal actions

		//pass orders to system signals
		while (!orders.empty() && handle.size() < max_handle){
			//signals.to_system.push_back(Message("T", this->id, 0, ));


		}





	}
};
extern unordered_map<int, Terminal> terminals;

#endif