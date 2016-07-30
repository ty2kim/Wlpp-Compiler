// Starter code for CS241 assignments 9-11
//
// C++ translation by Simon Parent (Winter 2011),
// based on Java code by Ondrej Lhotak,
// which was based on Scheme code by Gord Cormack.
// Modified July 3, 2012 by Gareth Davies
#include <stdlib.h>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <cstdlib>
#include <cstring>
using namespace std;

// The set of terminal symbols in the WLPP grammar.
const char *terminals[] = {
  "BOF", "BECOMES", "COMMA", "ELSE", "EOF", "EQ", "GE", "GT", "ID",
  "IF", "INT", "LBRACE", "LE", "LPAREN", "LT", "MINUS", "NE", "NUM",
  "PCT", "PLUS", "PRINTLN", "RBRACE", "RETURN", "RPAREN", "SEMI",
  "SLASH", "STAR", "WAIN", "WHILE", "AMP", "LBRACK", "RBRACK", "NEW",
  "DELETE", "NULL"
};
int isTerminal(const string &sym) {
  int idx;
  for(idx=0; idx<sizeof(terminals)/sizeof(char*); idx++)
    if(terminals[idx] == sym) return 1;
  return 0;
}

// Data structure for storing the parse tree.
class tree {
  public:
    string rule;
    vector<string> tokens;
    vector<tree*> children;
    ~tree() { 
		for(int i=0; i<children.size(); i++) delete children[i]; 
	}
};

// Call this to display an error message and exit the program.
void bail(const string &msg) {
  // You can also simply throw a string instead of using this function.
  throw string(msg);
}

// Read and return wlppi parse tree.
tree *readParse(const string &lhs) {
  // Read a line from standard input.
	//cout<<"*******************************************************"<<endl;
	//cout<<"lhs: "<<lhs<<endl;
  string line;
  getline(cin, line);
  if(cin.fail())
    bail("ERROR: Unexpected end of file.");
  tree *ret = new tree();
  // Tokenize the line.
  stringstream ss;
  ss << line;
  //cout<<"line: "<<line<<endl;
  //cout<<"--------convert line into tokens----------"<<endl;
  while(!ss.eof()) {
    string token;
    ss >> token;
	//cout<<"token: "<<token<<endl;
    if(token == "") {
		//cout<<"token == """<<endl;
		continue;
	}
    ret->tokens.push_back(token);
  }
  //cout<<"---------------updating rules-------------"<<endl;
  // Ensure that the rule is separated by single spaces.
  for(int idx=0; idx<ret->tokens.size(); idx++) {
    if(idx>0) ret->rule += " ";
    ret->rule += ret->tokens[idx];
  }
  //cout<<"rule: "<<ret->rule<<endl;
  //cout<<"-------------------------------------------"<<endl;
  // Recurse if lhs is a nonterminal.
  if(!isTerminal(lhs)) {
	  //cout<<lhs<<" is not a terminal so recurse on: ";
	  //for(int i=1; i<ret->tokens.size(); i++) cout<<ret->tokens[i]<<" ";
	  //cout<<endl;
	  for(int idx=1/*skip the lhs*/; idx<ret->tokens.size(); idx++) {
		  ret->children.push_back(readParse(ret->tokens[idx]));
	  }
  }
  return ret;
}

tree *parseTree;

map<string,string> st;
pair<map<string,string>::iterator,bool> ret;

void SymbolTable(tree *t){
	tree* tmp1;
	tree* tmp2;
	string s1;
	string s2;
	if(t->rule == "dcl type ID"){
		tmp1 = t->children[1];
		tmp2 = t->children[0];//->children[0];
		s1 = tmp1->tokens[1];
		if(tmp2->rule == "type INT STAR") s2 = tmp2->children[0]->tokens[1] + '*';
		else s2 = tmp2->children[0]->tokens[1];
		ret=st.insert(pair<string,string>(s1,s2));
		if(ret.second == false)
			bail("ERROR: the identifier "+s1+" is declared more tan once");
	}
	else if(t->rule == "factor ID" || t->rule == "lvalue ID"){
		tmp1 = t->children[0];
		s1 = tmp1->tokens[1];
		if(st.count(s1) == 0)
			bail("ERROR: the identifier "+s1+" is used without first having been declared");		
	}
	if(!isTerminal(t->tokens[0])){
		for(int i=0; i < t->children.size(); i++)
			SymbolTable(t->children[i]);
	}
	return;
}

void PrintSymbolTable(){
	for(map<string,string>::iterator it=st.begin(); it!=st.end(); ++it)
	    cerr<<it->first<<" "<<it->second<<endl;
}

// Compute symbols defined in t.

string expr_type(tree* t);
string lv_type(tree* t);

string factor_type(tree *t){
	//cout<<"factor"<<endl;
	//cout<<"t->rule: "<<t->rule<<endl;
	//factor ¡æ ID  
	//factor ¡æ NUM  
	//factor ¡æ NULL  
	//factor ¡æ LPAREN expr RPAREN  
	//factor ¡æ AMP lvalue
	//factor ¡æ STAR factor
	//factor ¡æ NEW INT LBRACK expr RBRACK
	if(t->tokens[1] == "ID"){
		string value = t->children[0]->tokens[1];
		map<string,string>::iterator it;
		it=st.find(value);
		return it->second;
	}
	else if(t->tokens[1] == "NUM"){
		return "int";
	}
	else if(t->tokens[1] == "NULL"){
		return "int*";
	}
	else if(t->tokens[1] == "LPAREN"){
		return expr_type(t->children[1]);
	}
	else if(t->tokens[1] == "AMP"){
		string l = lv_type(t->children[1]);
		if(l != "int")
			bail("ERROR: using & but the identifier is not int type");
		else return "int*";	}
	else if(t->tokens[1] == "STAR"){
		string f = factor_type(t->children[1]);
		if(f != "int*")
			bail("ERROR: using * but the identifier is not int* type");
		else return "int";
	}
	else if(t->tokens[1] == "NEW"){
		string e = expr_type(t->children[3]);
		if(e != "int")
			bail("ERROR: using new int but the expression is not int type");
		else return "int*";
	}
}

string term_type(tree *t){
	//cout<<"term"<<endl;
	//cout<<"t->rule: "<<t->rule<<endl;
	//term ¡æ factor 
	//term ¡æ term STAR factor 
	//term ¡æ term SLASH factor 
	//term ¡æ term PCT factor 
	if(t->tokens[1] == "factor"){
		return factor_type(t->children[0]);
	}
	else{ // t->tokens[1] == "term"
		string f1 = term_type(t->children[0]);
		string f2 = factor_type(t->children[2]);
		if(f1 == "int" && f2 == "int") return "int";
		else
			bail("ERROR: "+f1+" "+t->tokens[2]+" "+f2);
	}		
}

string expr_type(tree *t){
	//cout<<"expr"<<endl;
	//cout<<"t->rule: "<<t->rule<<endl;
	//expr ¡æ term 
	//expr ¡æ expr PLUS term 
	//expr ¡æ expr MINUS term 
	if(t->tokens[1] == "term"){
		return term_type(t->children[0]);
	}
	else{// t->tokens[1] == "expr"
		string t1 = expr_type(t->children[0]);
		string t2 = term_type(t->children[2]);
		if(t->tokens[2] == "PLUS"){
			if(t1 == "int" && t2 == "int") return "int";
			else if((t1 == "int" && t2 == "int*") || (t1 == "int*" && t2 == "int")) return "int*";
			else 
				bail("ERROR: "+t1+" PLUS "+t2);
		}
		else{ // t->tokens[2] == "MINUS"
			if((t1 == "int" && t2 == "int") || (t1 == "int*" && t2 == "int*")) return "int";
			else if(t1 == "int*" && t2 == "int") return "int*";
			else
				bail("ERROR: "+t1+" MINUS "+t2);
		}
	}
}

string lv_type(tree *t){
	//cout<<"lvalue"<<endl;
	//cout<<"t->rule: "<<t->rule<<endl;
	//lvalue ¡æ ID  
	//lvalue ¡æ STAR factor
	//lvalue ¡æ LPAREN lvalue RPAREN  
	if(t->tokens[1] == "ID"){
		string value = t->children[0]->tokens[1];
		map<string,string>::iterator it;
		it=st.find(value);
		return it->second;
	}
	else if(t->tokens[1] == "STAR"){
		string f = factor_type(t->children[1]);
		if(f != "int*")
			bail("ERROR: using * but the identifier is not int* type");
		else return "int";
	}
	else{// t->tokens[1] == "LPAREN"
		return lv_type(t->children[1]);
	}
}

void test_type(tree *t){
	//test ¡æ expr EQ expr  
	//test ¡æ expr NE expr  
	//test ¡æ expr LT expr 
	//test ¡æ expr LE expr  
	//test ¡æ expr GE expr	
	//test ¡æ expr GT expr 
	string e1 = expr_type(t->children[0]);
	string e2 = expr_type(t->children[2]);
	if(e1 != e2)
		bail("ERROR: "+e1+" "+t->tokens[2]+" "+e2);
}

void isValidStatements(tree *t){
	//cout<<"statements"<<endl;
	//cout<<"t->rule: "<<t->rule<<endl;
	/*statements ¡æ
	statements ¡æ statements statement  
	statement ¡æ lvalue BECOMES expr SEMI
	statement ¡æ IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE 
	statement ¡æ WHILE LPAREN test RPAREN LBRACE statements RBRACE 
	statement ¡æ PRINTLN LPAREN expr RPAREN SEMI
	statement ¡æ DELETE LBRACK RBRACK expr SEMI*/
	if(t->tokens.size() == 1) return;
	else if(t->tokens[1] == "statements"){
		isValidStatements(t->children[0]);
		isValidStatements(t->children[1]);
	}
	else if(t->tokens[1] == "lvalue"){
		if(lv_type(t->children[0]) != expr_type(t->children[2]))
			bail("ERROR: the types of lvalue and expressions does not match");
	}
	else if(t->tokens[1] == "IF"){
		test_type(t->children[2]);
		isValidStatements(t->children[5]);
		isValidStatements(t->children[9]);
	}
	else if(t->tokens[1] == "WHILE"){
		test_type(t->children[2]);
		isValidStatements(t->children[5]);
	}
	else if(t->tokens[1] == "PRINTLN"){
		string e1 = expr_type(t->children[2]);
		if(e1 != "int")
			bail("ERROR: using println expr, but the type of expr is not int. given: "+e1);
	}
	else{ // t->tokens[1] == "DELETE"
		string e2 = expr_type(t->children[3]);
		if(e2 != "int*")
			bail("ERROR: using delete [] expr, but the type of expr is not int*. given: "+e2);
	}
}

string dcl_type(tree *t){
	/*type ¡æ INT
	type ¡æ INT STAR*/
	if(t->children[0]->rule == "type INT STAR")
		return t->children[0]->children[0]->tokens[1]+'*';
	else
		return t->children[0]->children[0]->tokens[1];
}

void isValidDcls(tree *t){
	//dcls ¡æ 
	//dcls ¡æ dcls dcl BECOMES NUM SEMI
	//dcls ¡æ dcls dcl BECOMES NULL SEMI
	//dcl ¡æ type ID
	if(t->tokens.size() == 1) return;
	else if(t->tokens[1] == "dcls"){
		isValidDcls(t->children[0]);
		string d = dcl_type(t->children[1]); 
		if((d == "int" && t->tokens[4] == "NULL") || (d == "int*" && t->tokens[4] == "NUM"))
			bail("ERROR: "+d+" BECOMES "+t->tokens[4]);
	}
}

void genSymbols(tree *t) {
	SymbolTable(t);
	//PrintSymbolTable();
	tree* second_parameter = t->children[1]->children[5];
	string sp = dcl_type(second_parameter);
	if(sp == "int*")
		bail("ERROR: the second parameter of wain should be int. given "+sp);
	tree* dcls = t->children[1]->children[8];
	tree* statements = t->children[1]->children[9];
	tree* ret_expr = t->children[1]->children[11];
	isValidDcls(dcls);
	isValidStatements(statements);
	string ret_type = expr_type(ret_expr);
	if(ret_type != "int" && ret_type != "0")
		bail("ERROR: the return type should be int. given "+ret_type);
	return;
}

// Generate the code for the parse tree t.

void pop(string reg){
	cout<<"add $30, $30, $4"<<endl;
	cout<<"lw "<<reg<<", -4($30)"<<endl;
}

void push(string reg){
	cout<<"sw "<<reg<<", -4($30)"<<endl;
	cout<<"sub $30, $30, $4"<<endl;
}


map<string,int> symtbl;

void build_symtbl(){
	map<string,string>::iterator it = st.begin();
	int offset = 0;
	for(int i=0; i<st.size(); i++){
		offset = 4 * i;
		symtbl.insert(pair<string,int>(it->first, offset));
		it++;
	}
}

int add_counter = 0;
int offset;
int while_counter = 0;

void genCode(tree *t) {
	//procedure ¡æ INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE
 //   type ¡æ INT
 //   dcls ¡æ
 //   dcl ¡æ type ID
 //   statements ¡æ
 //   expr ¡æ term
 //   term ¡æ factor
 //   factor ¡æ ID

	//expr ¡æ expr PLUS term ;
	//expr ¡æ expr MINUS term ;
 //   term ¡æ term STAR factor ;
 //   term ¡æ term SLASH factor ;
 //   term ¡æ term PCT factor ;
 //   factor ¡æ NUM

	//statements ¡æ statements statement
   // statement ¡æ PRINTLN LPAREN expr RPAREN SEMI

//	dcls ¡æ dcls dcl BECOMES NUM SEMI
//statement ¡æ lvalue BECOMES expr SEMI
//lvalue ¡æ ID
//lvalue ¡æ LPAREN lvalue RPAREN
	if(t->tokens[0] == "S"){
		build_symtbl();
		//for(map<string,int>::iterator it=symtbl.begin(); it!=symtbl.end(); ++it)
		//    cerr<<it->first<<" "<<it->second<<endl;
		genCode(t->children[1]);
	}
	else if(t->tokens[0] == "procedure"){
		cout<<"lis $4"<<endl;
		cout<<".word 4"<<endl;
		cout<<"lis $11"<<endl;
		cout<<".word 1"<<endl;

		for(int i=0; i<symtbl.size(); i++) cout<<"sub $30, $30, $4"<<endl;
		cout<<"add $29, $30, $0"<<endl;

		push("$31");

		cout<<"sw $1, "<<add_counter<<"($29)"<<endl;
		add_counter+=4;
		cout<<"sw $2, "<<add_counter<<"($29)"<<endl;
		add_counter+=4;
		
		cout<<endl;
		
		genCode(t->children[8]); // dcls
		genCode(t->children[9]); // statements
		genCode(t->children[11]); // expr
		
		cout<<endl;

		pop("$31");
		cout<<"jr $31"<<endl;
		
		// print procedure
	cout<<"print:"<<endl;
		cout<<"sw $1, -4($30)"<<endl;
		cout<<"sw $2, -8($30)"<<endl;
		cout<<"sw $3, -12($30)"<<endl;
		cout<<"sw $4, -16($30)"<<endl;
		cout<<"sw $5, -20($30)"<<endl;
		cout<<"sw $6, -24($30)"<<endl;
		cout<<"sw $7, -28($30)"<<endl;
		cout<<"sw $8, -32($30)"<<endl;
		cout<<"sw $9, -36($30)"<<endl;
		cout<<"sw $10, -40($30)"<<endl;
		cout<<"lis $3"<<endl;
		cout<<".word -40"<<endl;
		cout<<"add $30, $30, $3"<<endl<<endl;

	    cout<<"lis $3"<<endl;
		cout<<".word 0xffff000c"<<endl;
		cout<<"lis $4"<<endl;
		cout<<".word 10"<<endl;
		cout<<"lis $5"<<endl;
		cout<<".word 4"<<endl;
		cout<<"add $6, $1, $0"<<endl;     
		cout<<"slt $7, $1, $0"<<endl;   
		cout<<"beq $7, $0, IfDone"<<endl;
		cout<<"lis $8"<<endl;
		cout<<".word 0x0000002d"<<endl; 
		cout<<"sw $8, 0($3)"<<endl;
		cout<<"sub $6, $0, $6"<<endl<<endl;      
	cout<<"IfDone:"<<endl;
		cout<<"add $9, $30, $0"<<endl<<endl;        

	cout<<"Loop:"<<endl;
		cout<<"divu $6, $4"<<endl;
		cout<<"mfhi $10"<<endl;
		cout<<"sw $10, -4($9)"<<endl;
		cout<<"mflo $6"<<endl;
		cout<<"sub $9, $9, $5"<<endl;
		cout<<"slt $10, $0, $6"<<endl;         
		cout<<"bne $10, $0, Loop"<<endl<<endl;
	
		cout<<"lis $7"<<endl;
		cout<<".word 48"<<endl;
	cout<<"Loop2:"<<endl;
		cout<<"lw $8, 0($9)"<<endl;
		cout<<"add $8, $8, $7"<<endl;
		cout<<"sw $8, 0($3)"<<endl;
		cout<<"add $9, $9, $5"<<endl;
		cout<<"bne $9, $30, Loop2"<<endl;
		cout<<"sw $4, 0($3)"<<endl<<endl;

		cout<<"lis $3"<<endl;
		cout<<".word 40"<<endl;
		cout<<"add $30, $30, $3"<<endl;
		cout<<"lw $1, -4($30)"<<endl;
		cout<<"lw $2, -8($30)"<<endl;
		cout<<"lw $3, -12($30)"<<endl;
		cout<<"lw $4, -16($30)"<<endl;
		cout<<"lw $5, -20($30)"<<endl;
		cout<<"lw $6, -24($30)"<<endl;	
		cout<<"lw $7, -28($30)"<<endl;
		cout<<"lw $8, -32($30)"<<endl;
		cout<<"lw $9, -36($30)"<<endl;
		cout<<"lw $10, -40($30)"<<endl;
		cout<<"jr $31"<<endl;
	}
	else if(t->tokens[0] == "dcls"){
		if(t->rule == "dcls dcls dcl BECOMES NUM SEMI"){
			genCode(t->children[0]);
			map<string,int>::iterator it = symtbl.find(t->children[1]->children[1]->tokens[1]);
			offset = it->second;
			cout<<"lis $3"<<endl;
			cout<<".word "<<t->children[3]->tokens[1]<<endl;
			cout<<"sw $3, "<<offset<<"($29)"<<endl;
		}
	}
	else if(t->tokens[0] == "statements"){
		if(t->rule == "statements statements statement"){
			genCode(t->children[0]);
			genCode(t->children[1]);
		}
	}
	else if(t->tokens[0] == "statement"){
//		test ¡æ expr LT expr
//statement ¡æ WHILE LPAREN test RPAREN LBRACE statements RBRACE
		if(t->rule == "statement PRINTLN LPAREN expr RPAREN SEMI"){
			genCode(t->children[2]);
			cout<<"lis $5"<<endl;
			cout<<".word print"<<endl;
			cout<<"add $1, $3, $0"<<endl;
			cout<<"jalr $5"<<endl;
		}
		else if(t->rule == "statement lvalue BECOMES expr SEMI"){
			genCode(t->children[0]);
			int temp_offset = offset;
			genCode(t->children[2]);
			cout<<"sw $3, "<<temp_offset<<"($29)"<<endl;
		}
		else if(t->rule == "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE"){
			cout<<endl;
			cout<<"while"<<while_counter<<":"<<endl;
			int temp = while_counter;
			while_counter++;
			genCode(t->children[2]);
			cout<<"bne $3, $11, endwhile"<<temp<<endl;
			genCode(t->children[5]);
			cout<<"beq $0, $0, while"<<temp<<endl<<endl;
			cout<<"endwhile"<<temp<<":"<<endl;
		}
	}
	else if(t->tokens[0] == "test"){
		if(t->rule == "test expr EQ expr"){
			genCode(t->children[0]);
			cout<<"sw $3, -4($30)"<<endl;
			cout<<"sub $30, $30, $4"<<endl;
			genCode(t->children[2]);
			cout<<"add $30, $30, $4"<<endl;
			cout<<"lw $5, -4($30)"<<endl;
			cout<<"bne $5, $3, 2"<<endl;
			cout<<"add $3, $11, $0"<<endl;
			cout<<"beq $0, $0, 1"<<endl;
			cout<<"add $3, $0, $0"<<endl;
		}
		else if(t->rule == "test expr NE expr"){
			genCode(t->children[0]);
			cout<<"sw $3, -4($30)"<<endl;
			cout<<"sub $30, $30, $4"<<endl;
			genCode(t->children[2]);
			cout<<"add $30, $30, $4"<<endl;
			cout<<"lw $5, -4($30)"<<endl;
			cout<<"beq $5, $3, 2"<<endl;
			cout<<"add $3, $11, $0"<<endl;
			cout<<"beq $0, $0, 1"<<endl;
			cout<<"add $3, $0, $0"<<endl;
		}
		else if(t->rule == "test expr LE expr"){
			genCode(t->children[0]);
			cout<<"sw $3, -4($30)"<<endl;
			cout<<"sub $30, $30, $4"<<endl;
			genCode(t->children[2]);
			cout<<"add $30, $30, $4"<<endl;
			cout<<"lw $5, -4($30)"<<endl;
			cout<<"beq $5, $3, 2"<<endl;
			cout<<"slt $3, $5, $3"<<endl;
			cout<<"bne $3, $11, 1"<<endl;
			cout<<"add $3, $11, $0"<<endl;
		}
		else if(t->rule == "test expr GE expr"){
			genCode(t->children[0]);
			cout<<"sw $3, -4($30)"<<endl;
			cout<<"sub $30, $30, $4"<<endl;
			genCode(t->children[2]);
			cout<<"add $30, $30, $4"<<endl;
			cout<<"lw $5, -4($30)"<<endl;
			cout<<"beq $5, $3, 2"<<endl;
			cout<<"slt $3, $3, $5"<<endl;
			cout<<"bne $3, $11, 1"<<endl;
			cout<<"add $3, $11, $0"<<endl;
		}
		else if(t->rule == "test expr LT expr"){
			genCode(t->children[0]);
			cout<<"sw $3, -4($30)"<<endl;
			cout<<"sub $30, $30, $4"<<endl;
			genCode(t->children[2]);
			cout<<"add $30, $30, $4"<<endl;
			cout<<"lw $5, -4($30)"<<endl;
			cout<<"slt $3, $5, $3"<<endl;
		}
		else if(t->rule == "test expr GT expr"){
			genCode(t->children[0]);
			cout<<"sw $3, -4($30)"<<endl;
			cout<<"sub $30, $30, $4"<<endl;
			genCode(t->children[2]);
			cout<<"add $30, $30, $4"<<endl;
			cout<<"lw $5, -4($30)"<<endl;
			cout<<"slt $3, $3, $5"<<endl;
		}
	}
	else if(t->tokens[0] == "expr"){
		if(t->rule == "expr term") genCode(t->children[0]);
		else if(t->rule == "expr expr PLUS term"){
			genCode(t->children[0]);
			cout<<"sw $3, -4($30)"<<endl;
			cout<<"sub $30, $30, $4"<<endl;
			genCode(t->children[2]);
			cout<<"add $30, $30, $4"<<endl;
			cout<<"lw $5, -4($30)"<<endl;
			cout<<"add $3, $5, $3"<<endl;
		}
		else if(t->rule == "expr expr MINUS term"){
			genCode(t->children[0]);
			cout<<"sw $3, -4($30)"<<endl;
			cout<<"sub $30, $30, $4"<<endl;
			genCode(t->children[2]);
			cout<<"add $30, $30, $4"<<endl;
			cout<<"lw $5, -4($30)"<<endl;
			cout<<"sub $3, $5, $3"<<endl;
		}
	}
	else if(t->tokens[0] == "term"){
		if(t->rule == "term factor") genCode(t->children[0]);
		else if(t->rule == "term term STAR factor"){
			genCode(t->children[0]);
			cout<<"sw $3, -4($30)"<<endl;
			cout<<"sub $30, $30, $4"<<endl;
			genCode(t->children[2]);
			cout<<"add $30, $30, $4"<<endl;
			cout<<"lw $5, -4($30)"<<endl;
			cout<<"mult $5, $3"<<endl;
			cout<<"mflo $3"<<endl;
		}
		else if(t->rule == "term term SLASH factor"){
			genCode(t->children[0]);
			cout<<"sw $3, -4($30)"<<endl;
			cout<<"sub $30, $30, $4"<<endl;
			genCode(t->children[2]);
			cout<<"add $30, $30, $4"<<endl;
			cout<<"lw $5, -4($30)"<<endl;
			cout<<"div $5, $3"<<endl;
			cout<<"mflo $3"<<endl;
		}
		else if(t->rule == "term term PCT factor"){
			genCode(t->children[0]);
			cout<<"sw $3, -4($30)"<<endl;
			cout<<"sub $30, $30, $4"<<endl;
			genCode(t->children[2]);
			cout<<"add $30, $30, $4"<<endl;
			cout<<"lw $5, -4($30)"<<endl;
			cout<<"div $5, $3"<<endl;
			cout<<"mfhi $3"<<endl;
		}
	}
	else if(t->tokens[0] == "factor"){
		if(t->rule == "factor ID"){
			map<string,int>::iterator it = symtbl.find(t->children[0]->tokens[1]);
			offset = it->second;
			cout<<"lw $3, "<<offset<<"($29)"<<endl;
		}
		else if(t->rule == "factor LPAREN expr RPAREN"){
			genCode(t->children[1]);
		}
		else if(t->rule == "factor NUM"){
			cout<<"lis $3"<<endl;
			cout<<".word "<<t->children[0]->tokens[1]<<endl;
		}
	}
	else if(t->tokens[0] == "lvalue"){
		if(t->rule == "lvalue ID"){
			map<string,int>::iterator it = symtbl.find(t->children[0]->tokens[1]);
			offset = it->second;
		}
		else if(t->rule == "lvalue LPAREN lvalue RPAREN"){
			genCode(t->children[1]);
		}
	}
}

int main() {
  // Main program.
  try {
    parseTree = readParse("S");
    genSymbols(parseTree);
    genCode(parseTree);
  } catch(string msg) {
    cerr << msg << endl;
  }
  if (parseTree) delete parseTree;
  return 0;
}