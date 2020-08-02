//缺陷规则程序

#include <stdio.h>
//#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>

using namespace std;


//规则1：switch里面的变量类型和case中的类型不匹配，比如enum和int混用
//代码：
enum DAY {
  MON=1, TES, THU
};

int func1() {
	enum DAY day;
	day = MON;
	switch(day) {
		case 1:
			return 1;
		default:
			return 0;
	}
}

int fn1() {
	char a1 = 't';
  switch(a1) {
     case 1:
     cout << 1;
        return 1;
     default:
     cout << 2;
        return 0;
   }
}



//规则2：空指针解引用

int func2(){
	int *p = NULL;
	int q = *p;
	cout<<q;
	return 0; 
}


//规则3：除零错，除（/）或模（%）运算的第二个操作数可能为0

int g(int x) {
	x = x - 5;
	int y = 100/x;
		if(y > 0) {
			int z = 100 / y;
		}
	return y;
}

int func3(){
	cout<<g(5);
	return 0; 
}

int fn3(){
    int x = 10;
    int y = x - 10;
    int z = 100%y;
    return 0;
}



//规则4：数组下标越界

int func4(){
	int a[10];
	a[10] = 0;
	cout<<a[10];
	return 0;
}


//规则5：custodial指针变量可能没被释放或返回

int func5(){
	char *p = new char[20]; 
	char *q = p; 
	p = new char[20]; 
	q = p + 0; 
	strcpy(p, "hello");
	return 0; 
}


//规则6：通过下标访问字符数组'\0'之后的内容

int func6(){
	char buf[20];
	strcpy(buf, "a");
	char c = buf[4]; 
	cout<<c;
	return 0; 
}


//规则7：指针变量double free

int func7(){
	int *p = (int*)malloc(sizeof(int));
	free(p);
	free(p);
	return 0; 
}


//规则8：声明有返回类型的函数在某些路径上没有return返回值

int f(int x) {
 	if(x > 5) {
return x;
}
}

int func8(){
	cout<<f(5);
	return 0; 
}


//规则9：比较运算中混合使用有符号数与无符号数

int func9(){
	int a = -1;
	unsigned int b = 1;
	cout<<(a > b);
	return 0; 
}

int fn9(){
    int a = -1;
    unsigned int b = 1;
    if (a < b) 
        cout << 1;
    else 
        cout << 2;
}



//规则10：成员变量域长度太小

enum color { red, green, yellow, blue, black};
struct abc { enum color c:2; };

int func10(){
	struct abc example;
	return 0; 
}


//规则11：switch表达式是枚举类型时，至少一个枚举常量不存在case中，同时，没有提供default分支

int func11() {
	enum DAY day;
	day = MON;
 	switch(day) {
  	case 1:
        return 1;
 	}
 	return 0;
}


//规则12：类成员函数返回值是指向成员的非const指针或引用，但成员的访问级比这个函数要低

class X {
private:
	int a;
public:
	void set(int n){
		a = n;
	}
	void get() const{
		cout<<a;
	}
	int *f() {return &a;}
};

int func12() {
	X x;
	x.set(1);
	*(x.f()) = 2;
	x.get();
 	return 0;
}


//规则13：类的const成员函数返回非const的指针

class X2 {
private:
	int q;
	int *p;
public:
	void set(int n){
		q=n;
		p=&q;
	}
	int *f() const{
		(*p)++;
		return p;
	}
};

int func13(){
	X2 x;
	x.set(5);
	cout<<*(x.f());
	return 0;	
}


//规则14：类的非静态指针成员变量未在析构函数中释放

class X3 {
public:
	X3(int len){
		if(len>0)
			p = new int[len];
		else
			p = NULL;
	}
 	~X3() {}
private:
	int *p;
};

int func14(){
	X3 x(5);
	return 0;	
}


//规则15：将指向继承类数组的指针赋值给指向基类的指针

class Base {
public:
	int a;
	void get() const{
		cout<<sizeof(a);
	}
};

class Derived: public Base{
public:
	double b;
	void get() const{
		cout<<sizeof(b);
	}
};

int func15(){
	Derived *d = new Derived[10];
	Base *b;
	b = d;
	b[0].get();
	return 0;	
}





/*
target datalayout = ""e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128""
target triple = ""x86_64-pc-linux-gnu""


@_ZStL8__ioinit = internal %class.std::ios_base::Init zeroinitializer
@.str = private constant [6 x i8] [i8 104, i8 101, i8 108, i8 108, i8 111, i8 0]
@.str1 = private constant [2 x i8] [i8 97, i8 0]
@llvm.global_ctors = appending [1 x {i32, void()*}] [{i32, void()*} {i32 65535, void() @_GLOBAL__I_a}]

define internal void @__cxx_global_var_init ( ) .text.startup {
 ; <label>:0
  call void @_ZNSt8ios_base4InitC1Ev ( %class.std::ios_base::Init* @_ZStL8__ioinit ), !dbg !5
  %1 = call i32 @__cxa_atexit ( void(i8*)* void(i8*)* bitcast (void(%class.std::ios_base::Init*) @_ZNSt8ios_base4InitD1Ev to void(i8*)*), i8* i8* getelementptr ( %class.std::ios_base::Init* @_ZStL8__ioinit ,  i32 0, i32 0 ), i8* @__dso_handle ), !dbg !6
  ret void, !dbg !7 }

define i32 @_Z5func1v ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %day = alloca i32 , align 4
  store i32 1 , i32* %day , align 4, !dbg !13
  switch i32 1 , label %3 [ [1 x <2 x i32>] [<2 x i32> [i32 1, i32 1]], label %2 ], !dbg !14
; <label>:2
  store i32 1 , i32* %1, !dbg !15
  br label %4, !dbg !16
; <label>:3
  store i32 0 , i32* %1, !dbg !17
  br label %4, !dbg !18
; <label>:4
  %5 = phi i32 [ [0, %3], [1, %2] ]
  ret i32 %5, !dbg !19 }

define i32 @_Z3fn1v ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %a1 = alloca i8 , align 1
  store i8 116 , i8* %a1 , align 1, !dbg !21
  switch i32 116 , label %4 [ [1 x <2 x i32>] [<2 x i32> [i32 1, i32 1]], label %2 ], !dbg !22
; <label>:2
  %3 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 1 ), !dbg !23
  store i32 1 , i32* %1, !dbg !24
  br label %6, !dbg !25
; <label>:4
  %5 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 2 ), !dbg !26
  store i32 0 , i32* %1, !dbg !27
  br label %6, !dbg !28
; <label>:6
  %7 = phi i32 [ [0, %4], [1, %2] ]
  ret i32 %7, !dbg !29 }

define i32 @_Z5func2v ( ) {
 ; <label>:0
  %p = alloca i32* , align 8
  %q = alloca i32 , align 4
  store i32* null , i32** %p , align 8, !dbg !31
  %1 = load i32* null , align 4, !dbg !32
  store i32 %1 , i32* %q , align 4, !dbg !33
  %2 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %1 ), !dbg !34
  ret i32 0, !dbg !35 }

define i32 @_Z1gi ( i32 %x ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %y = alloca i32 , align 4
  %z = alloca i32 , align 4
  store i32 %x , i32* %1 , align 4
  %2 = sub nsw i32 %x , 5, !dbg !39
  store i32 %2 , i32* %1 , align 4, !dbg !40
  %3 = div i32 100 , %2, !dbg !41
  store i32 %3 , i32* %y , align 4, !dbg !42
  %4 = icmp sgt i32 %3 , 0, !dbg !43
  br i1 %4 , label %5 , label %7, !dbg !44
; <label>:5
  %6 = div i32 100 , %3, !dbg !45
  store i32 %6 , i32* %z , align 4, !dbg !46
  br label %7, !dbg !47
; <label>:7
  ret i32 %3, !dbg !48 }

define i32 @_Z5func3v ( ) {
 ; <label>:0
  %1 = call i32 @_Z1gi ( i32 5 ), !dbg !50
  %2 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %1 ), !dbg !51
  ret i32 0, !dbg !52 }

define i32 @_Z3fn3v ( ) {
 ; <label>:0
  %x = alloca i32 , align 4
  %y = alloca i32 , align 4
  %z = alloca i32 , align 4
  store i32 10 , i32* %x , align 4, !dbg !54
  store i32 0 , i32* %y , align 4, !dbg !55
  store i32 undef , i32* %z , align 4, !dbg !56
  ret i32 0, !dbg !57 }

define i32 @_Z5func4v ( ) {
 ; <label>:0
  %a = alloca [10 x i32] , align 16
  %1 = getelementptr inbounds [10 x i32]* %a , i32 0, i64 10, !dbg !59
  store i32 0 , i32* %1 , align 4, !dbg !60
  %2 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 0 ), !dbg !61
  ret i32 0, !dbg !62 }

define i32 @_Z5func5v ( ) {
 ; <label>:0
  %p = alloca i8* , align 8
  %q = alloca i8* , align 8
  %1 = call i8* @_Znam ( i64 20 ), !dbg !64
  store i8* %1 , i8** %p , align 8, !dbg !65
  store i8* %1 , i8** %q , align 8, !dbg !66
  %2 = call i8* @_Znam ( i64 20 ), !dbg !67
  store i8* %2 , i8** %p , align 8, !dbg !68
  store i8* %2 , i8** %q , align 8, !dbg !69
  %3 = call i8* @strcpy ( i8* %2, i8* i8* getelementptr ( [6 x i8]* @.str ,  i32 0, i32 0 ) ), !dbg !70
  ret i32 0, !dbg !71 }

define i32 @_Z5func6v ( ) {
 ; <label>:0
  %buf = alloca [20 x i8] , align 16
  %c = alloca i8 , align 1
  %1 = getelementptr inbounds [20 x i8]* %buf , i32 0, i32 0, !dbg !73
  %2 = call i8* @strcpy ( i8* %1, i8* i8* getelementptr ( [2 x i8]* @.str1 ,  i32 0, i32 0 ) ), !dbg !74
  %4 = getelementptr inbounds [20 x i8]* %buf , i32 0, i64 4, !dbg !75
  %5 = load i8* %4 , align 1, !dbg !76
  store i8 %5 , i8* %c , align 1, !dbg !77
  %6 = call %class.std::basic_ostream* @_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_c ( %class.std::basic_ostream* @_ZSt4cout, i8 %5 ), !dbg !78
  ret i32 0, !dbg !79 }

define i32 @_Z5func7v ( ) {
 ; <label>:0
  %p = alloca i32* , align 8
  %1 = call i8* @malloc ( i64 4 ), !dbg !81
  %2 = bitcast i8* %1 to i32*, !dbg !82
  store i32* %2 , i32** %p , align 8, !dbg !83
  %3 = bitcast i32* %2 to i8*, !dbg !84
  call void @free ( i8* %3 ), !dbg !85
  call void @free ( i8* %3 ), !dbg !86
  ret i32 0, !dbg !87 }

define i32 @_Z1fi ( i32 %x ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  store i32 %x , i32* %1 , align 4
  %2 = icmp sgt i32 %x , 5, !dbg !89
  br i1 %2 , label %3 , label %4, !dbg !90
; <label>:3
  ret i32 %x, !dbg !91
; <label>:4
  call void @llvm.trap ( ), !dbg !92
  unreachable, !dbg !93 }

define i32 @_Z5func8v ( ) {
 ; <label>:0
  %1 = call i32 @_Z1fi ( i32 5 ), !dbg !95
  %2 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %1 ), !dbg !96
  ret i32 0, !dbg !97 }

define i32 @_Z5func9v ( ) {
 ; <label>:0
  %a = alloca i32 , align 4
  %b = alloca i32 , align 4
  store i32 -1 , i32* %a , align 4, !dbg !99
  store i32 1 , i32* %b , align 4, !dbg !100
  %1 = call %class.std::basic_ostream* @_ZNSolsEb ( %class.std::basic_ostream* @_ZSt4cout, i1 -1 ), !dbg !101
  ret i32 0, !dbg !102 }

define i32 @_Z3fn9v ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %a = alloca i32 , align 4
  %b = alloca i32 , align 4
  store i32 -1 , i32* %a , align 4, !dbg !104
  store i32 1 , i32* %b , align 4, !dbg !105
  br i1 0 , label %2 , label %4, !dbg !106
; <label>:2
  %3 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 1 ), !dbg !107
  br label %6, !dbg !108
; <label>:4
  %5 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 2 ), !dbg !109
  br label %6
; <label>:6
  call void @llvm.trap ( )
  unreachable
; <label>:7
  %8 = load i32* %1, !dbg !110
  ret i32 %8, !dbg !111 }

define i32 @_Z6func10v ( ) {
 ; <label>:0
  %example = alloca %struct.abc , align 4
  ret i32 0, !dbg !113 }

define i32 @_Z6func11v ( ) {
 ; <label>:0
  %1 = alloca i32 , align 4
  %day = alloca i32 , align 4
  store i32 1 , i32* %day , align 4, !dbg !115
  switch i32 1 , label %3 [ [1 x <2 x i32>] [<2 x i32> [i32 1, i32 1]], label %2 ], !dbg !116
; <label>:2
  store i32 1 , i32* %1, !dbg !117
  br label %4, !dbg !118
; <label>:3
  store i32 0 , i32* %1, !dbg !119
  br label %4, !dbg !120
; <label>:4
  %5 = phi i32 [ [0, %3], [1, %2] ]
  ret i32 %5, !dbg !121 }

define i32 @_Z6func12v ( ) {
 ; <label>:0
  %x = alloca %class.X , align 4
  call void @_ZN1X3setEi ( %class.X* %x, i32 1 ), !dbg !123
  %1 = call i32* @_ZN1X1fEv ( %class.X* %x ), !dbg !124
  store i32 2 , i32* %1 , align 4, !dbg !125
  call void @_ZNK1X3getEv ( %class.X* %x ), !dbg !126
  ret i32 0, !dbg !127 }

define linkonce_odr void @_ZN1X3setEi ( %class.X* %this, i32 %n ) {
 ; <label>:0
  %1 = alloca %class.X* , align 8
  %2 = alloca i32 , align 4
  store %class.X* %this , %class.X** %1 , align 8
  store i32 %n , i32* %2 , align 4
  %3 = getelementptr inbounds %class.X* %this , i32 0, i32 0, !dbg !148
  store i32 %n , i32* %3 , align 4, !dbg !149
  ret void, !dbg !150 }

define linkonce_odr i32* @_ZN1X1fEv ( %class.X* %this ) {
 ; <label>:0
  %1 = alloca %class.X* , align 8
  store %class.X* %this , %class.X** %1 , align 8
  %2 = getelementptr inbounds %class.X* %this , i32 0, i32 0, !dbg !152
  ret i32* %2, !dbg !153 }

define linkonce_odr void @_ZNK1X3getEv ( %class.X* %this ) {
 ; <label>:0
  %1 = alloca %class.X* , align 8
  store %class.X* %this , %class.X** %1 , align 8
  %2 = getelementptr inbounds %class.X* %this , i32 0, i32 0, !dbg !155
  %3 = load i32* %2 , align 4, !dbg !156
  %4 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %3 ), !dbg !157
  ret void, !dbg !158 }

define i32 @_Z6func13v ( ) {
 ; <label>:0
  %x = alloca %class.X2 , align 8
  call void @_ZN2X23setEi ( %class.X2* %x, i32 5 ), !dbg !160
  %1 = call i32* @_ZNK2X21fEv ( %class.X2* %x ), !dbg !161
  %2 = load i32* %1 , align 4, !dbg !162
  %3 = call %class.std::basic_ostream* @_ZNSolsEi ( %class.std::basic_ostream* @_ZSt4cout, i32 %2 ), !dbg !163
  ret i32 0, !dbg !164 }

define linkonce_odr void @_ZN2X23setEi ( %class.X2* %this, i32 %n ) {
 ; <label>:0
  %1 = alloca %class.X2* , align 8
  %2 = alloca i32 , align 4
  store %class.X2* %this , %class.X2** %1 , align 8
  store i32 %n , i32* %2 , align 4
  %3 = getelementptr inbounds %class.X2* %this , i32 0, i32 0, !dbg !182
  store i32 %n , i32* %3 , align 4, !dbg !183
  %4 = getelementptr inbounds %class.X2* %this , i32 0, i32 1, !dbg !184
  store i32* %3 , i32** %4 , align 8, !dbg !185
  ret void, !dbg !186 }

define linkonce_odr i32* @_ZNK2X21fEv ( %class.X2* %this ) {
 ; <label>:0
  %1 = alloca %class.X2* , align 8
  store %class.X2* %this , %class.X2** %1 , align 8
  %2 = getelementptr inbounds %class.X2* %this , i32 0, i32 1, !dbg !188
  %3 = load i32** %2 , align 8, !dbg !189
  %4 = load i32* %3 , align 4, !dbg !190
  %5 = add nsw i32 %4 , 1, !dbg !191
  store i32 %5 , i32* %3 , align 4, !dbg !192
  %6 = load i32** %2 , align 8, !dbg !193
  ret i32* %6, !dbg !194 }

define i32 @_Z6func14v ( ) {
 ; <label>:0
  %x = alloca %class.X3 , align 8
  %1 = alloca i32
  call void @_ZN2X3C1Ei ( %class.X3* %x, i32 5 ), !dbg !196
  store i32 1 , i32* %1
  call void @_ZN2X3D1Ev ( %class.X3* %x ), !dbg !197
  ret i32 0, !dbg !198 }

define linkonce_odr void @_ZN2X3C1Ei ( %class.X3* %this, i32 %len ) {
 ; <label>:0
  %1 = alloca %class.X3* , align 8
  %2 = alloca i32 , align 4
  store %class.X3* %this , %class.X3** %1 , align 8
  store i32 %len , i32* %2 , align 4
  call void @_ZN2X3C2Ei ( %class.X3* %this, i32 %len ), !dbg !210
  ret void, !dbg !211 }

define linkonce_odr void @_ZN2X3D1Ev ( %class.X3* %this ) {
 ; <label>:0
  %1 = alloca %class.X3* , align 8
  store %class.X3* %this , %class.X3** %1 , align 8
  call void @_ZN2X3D2Ev ( %class.X3* %this ), !dbg !213
  ret void, !dbg !214 }

define i32 @_Z6func15v ( ) {
 ; <label>:0
  %d = alloca %class.Derived* , align 8
  %b = alloca %class.Base* , align 8
  %1 = call i8* @_Znam ( i64 160 ), !dbg !216
  %2 = bitcast i8* %1 to %class.Derived*, !dbg !217
  store %class.Derived* %2 , %class.Derived** %d , align 8, !dbg !218
  %3 = bitcast %class.Derived* %2 to %class.Base*, !dbg !219
  store %class.Base* %3 , %class.Base** %b , align 8, !dbg !220
  call void @_ZNK4Base3getEv ( %class.Base* %3 ), !dbg !221
  ret i32 0, !dbg !222 }

define linkonce_odr void @_ZNK4Base3getEv ( %class.Base* %this ) {
 ; <label>:0
  %1 = alloca %class.Base* , align 8
  store %class.Base* %this , %class.Base** %1 , align 8
  %2 = call %class.std::basic_ostream* @_ZNSolsEm ( %class.std::basic_ostream* @_ZSt4cout, i64 4 ), !dbg !236
  ret void, !dbg !237 }

define linkonce_odr void @_ZN2X3D2Ev ( %class.X3* %this ) {
 ; <label>:0
  %1 = alloca %class.X3* , align 8
  store %class.X3* %this , %class.X3** %1 , align 8
  ret void, !dbg !239 }

define linkonce_odr void @_ZN2X3C2Ei ( %class.X3* %this, i32 %len ) {
 ; <label>:0
  %1 = alloca %class.X3* , align 8
  %2 = alloca i32 , align 4
  store %class.X3* %this , %class.X3** %1 , align 8
  store i32 %len , i32* %2 , align 4
  %3 = icmp sgt i32 %len , 0, !dbg !241
  br i1 %3 , label %4 , label %13, !dbg !242
; <label>:4
  %5 = sext i32 %len to i64, !dbg !243
  %6 = call {i64, i1} @llvm.umul.with.overflow.i64 ( i64 %5, i64 4 ), !dbg !244
  %7 = extractvalue {i64, i1} %6 1, !dbg !245
  %8 = extractvalue {i64, i1} %6 0, !dbg !246
  %9 = select i1 %7 , i64 -1 , i64 %8, !dbg !247
  %10 = call i8* @_Znam ( i64 %9 ), !dbg !248
  %11 = bitcast i8* %10 to i32*, !dbg !249
  %12 = getelementptr inbounds %class.X3* %this , i32 0, i32 0, !dbg !250
  store i32* %11 , i32** %12 , align 8, !dbg !251
  br label %15, !dbg !252
; <label>:13
  %14 = getelementptr inbounds %class.X3* %this , i32 0, i32 0, !dbg !253
  store i32* null , i32** %14 , align 8, !dbg !254
  br label %15
; <label>:15
  ret void, !dbg !255 }

define internal void @_GLOBAL__I_a ( ) .text.startup {
 ; <label>:0
  call void @__cxx_global_var_init ( ), !dbg !259
  ret void, !dbg !260 }


*/