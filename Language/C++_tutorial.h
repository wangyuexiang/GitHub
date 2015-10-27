preprocessor

using namespace std;

int foo = 0;
auto bar = foo;
decltype(foo) bar;



## Statements and flow control
##### if
if (x > 0)
	cout << "x is positve";
else if (x < 0)
	cout << "x is negative";
else
	cout << "x is 0";

##### while 
while (n > 0){
 cout << n <<",";
 --n;
}
 
##### do-while
	string str;
	do {
		cout << "Enter text: ";
		getline(cin, str);
		cout << "Your entered: " << str << '\n';
	} while (str != "goodbye")
		
##### for
	for (int n = 10; n > 0; n--){
		cout << n << ",";
	}

##### ranged-bassed for loop
	string str{"Hello!"}
	for (char c : str){
		cout << "[" << c << "]";
	}


###
##### 
# reference: ampersand(&)
# overhead

##### recursivity

long factorial (long a){
	if (a > 1) return (a * factorial (a-1));
	else return 1;
}
