declare i8* @concat(i8*, i8*)
declare i32 @strlen(i8*)
declare i8* @strcat(i8*, i8*)
declare i8* @strcpy(i8*, i8*)
declare i8* @malloc(i32)
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

@s- = internal constant [1 x i8] c" "
@s-0 = internal constant [2 x i8] c"= "
@s-1 = internal constant [9 x i8] c"hello */ "
@s-2 = internal constant [9 x i8] c"/* world "
@s-3 = internal constant [1 x i8] c" "


define i32 @main() {
%1 = alloca i32



%2 = call i32 @fac(i32 10)
call void @printInt(i32 %2)

%3 = call i32 @rfac(i32 10)
call void @printInt(i32 %3)

%4 = call i32 @mfac(i32 10)
call void @printInt(i32 %4)

%5 = call i32 @ifac(i32 10)
call void @printInt(i32 %5)

%r = bitcast [1 x i8]* @s- to i8*


%n2 = alloca i32
store i32 10, i32* %n2

%r2 = alloca i32
store i32 1, i32* %r2
br label %6
;<label>:6
%7 = load i32, i32* %n2

%8 = icmp sgt i32 %7, 0
br i1 %8, label %9, label %15
;<label>:9

%10 = load i32, i32* %r2
%11 = load i32, i32* %n2
%12 = mul i32 %10, %11

store i32 %12, i32* %r2
%13 = load i32, i32* %n2
%14 = sub i32 %13, 1
store i32 %14, i32* %n2

br label %6
;<label>:15

%16 = load i32, i32* %r2
call void @printInt(i32 %16)

%17 = bitcast [2 x i8]* @s-0 to i8*


%18 = call i8* @repStr(i8* %17,i32 60)
call void @printString(i8* %18)
%19 = bitcast [9 x i8]* @s-1 to i8*

call void @printString(i8* %19)
%20 = bitcast [9 x i8]* @s-2 to i8*

call void @printString(i8* %20)


store i32 0, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define i32 @fac(i32 %a) {
%1 = alloca i32
%2 = alloca i32
store i32 %a, i32* %2

%r = alloca i32
store i32 0, i32* %r

%n = alloca i32
store i32 0, i32* %n

store i32 1, i32* %r
%3 = load i32, i32* %2
store i32 %3, i32* %n
br label %4
;<label>:4
%5 = load i32, i32* %n

%6 = icmp sgt i32 %5, 0
br i1 %6, label %7, label %13
;<label>:7

%8 = load i32, i32* %r
%9 = load i32, i32* %n
%10 = mul i32 %8, %9

store i32 %10, i32* %r
%11 = load i32, i32* %n

%12 = sub i32 %11, 1

store i32 %12, i32* %n

br label %4
;<label>:13


%14 = load i32, i32* %r
store i32 %14, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define i32 @rfac(i32 %n) {
%1 = alloca i32
%2 = alloca i32
store i32 %n, i32* %2
%3 = load i32, i32* %2

%4 = icmp eq i32 %3, 0
br i1 %4, label %5, label %7
;<label>:5


store i32 1, i32* %1
br label %return
br label %14
;<label>:7

%8 = load i32, i32* %2
%9 = load i32, i32* %2

%10 = sub i32 %9, 1

%11 = call i32 @rfac(i32 %10)
%12 = mul i32 %8, %11

store i32 %12, i32* %1
br label %return
br label %14
;<label>:14



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define i32 @mfac(i32 %n) {
%1 = alloca i32
%2 = alloca i32
store i32 %n, i32* %2
%3 = load i32, i32* %2

%4 = icmp eq i32 %3, 0
br i1 %4, label %5, label %7
;<label>:5


store i32 1, i32* %1
br label %return
br label %14
;<label>:7

%8 = load i32, i32* %2
%9 = load i32, i32* %2

%10 = sub i32 %9, 1

%11 = call i32 @nfac(i32 %10)
%12 = mul i32 %8, %11

store i32 %12, i32* %1
br label %return
br label %14
;<label>:14



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define i32 @nfac(i32 %n) {
%1 = alloca i32
%2 = alloca i32
store i32 %n, i32* %2
%3 = load i32, i32* %2

%4 = icmp ne i32 %3, 0
br i1 %4, label %5, label %12
;<label>:5

%6 = load i32, i32* %2

%7 = sub i32 %6, 1

%8 = call i32 @mfac(i32 %7)
%9 = load i32, i32* %2
%10 = mul i32 %8, %9

store i32 %10, i32* %1
br label %return
br label %14
;<label>:12


store i32 1, i32* %1
br label %return
br label %14
;<label>:14



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define i32 @ifac(i32 %n) {
%1 = alloca i32
%2 = alloca i32
store i32 %n, i32* %2


%3 = load i32, i32* %2
%4 = call i32 @ifac2f(i32 1,i32 %3)
store i32 %4, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define i32 @ifac2f(i32 %l, i32 %h) {
%1 = alloca i32
%2 = alloca i32
%3 = alloca i32
store i32 %l, i32* %2
store i32 %h, i32* %3
%4 = load i32, i32* %2
%5 = load i32, i32* %3
%6 = icmp eq i32 %4, %5
br i1 %6, label %7, label %9
;<label>:7
%8 = load i32, i32* %2
store i32 %8, i32* %1
br label %return

;<label>:9
%10 = load i32, i32* %2
%11 = load i32, i32* %3
%12 = icmp sgt i32 %10, %11
br i1 %12, label %13, label %14
;<label>:13

store i32 1, i32* %1
br label %return

;<label>:14

%m = alloca i32
store i32 0, i32* %m
%15 = load i32, i32* %2
%16 = load i32, i32* %3
%17 = add i32 %15, %16


%18 = sdiv i32 %17, 2

store i32 %18, i32* %m

%19 = load i32, i32* %2
%20 = load i32, i32* %m
%21 = call i32 @ifac2f(i32 %19,i32 %20)
%22 = load i32, i32* %m

%23 = add i32 %22, 1

%24 = load i32, i32* %3
%25 = call i32 @ifac2f(i32 %23,i32 %24)
%26 = mul i32 %21, %25

store i32 %26, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define i8* @repStr(i8* %s, i32 %n) {
%1 = alloca i8*
%2 = alloca i8*
%3 = alloca i32
store i8* %s, i8** %2
store i32 %n, i32* %3

%4 = bitcast [1 x i8]* @s-3 to i8*
%r = alloca i8*
store i8* %4, i8** %r

%i = alloca i32
store i32 0, i32* %i
br label %5
;<label>:5
%6 = load i32, i32* %i
%7 = load i32, i32* %3
%8 = icmp slt i32 %6, %7
br i1 %8, label %9, label %15
;<label>:9

%10 = load i8*, i8** %r
%11 = load i8*, i8** %2
%12 = call i8* @concat(i8* %10, i8* %11)

store i8* %12, i8** %r
%13 = load i32, i32* %i
%14 = add i32 %13, 1
store i32 %14, i32* %i

br label %5
;<label>:15


%16 = load i8*, i8** %r
store i8* %16, i8** %1
br label %return



br label %return
return:
%r- = load i8*, i8** %1

ret i8* %r-
}