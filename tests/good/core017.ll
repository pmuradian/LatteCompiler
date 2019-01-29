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
@s-0 = internal constant [4 x i8] c"apa "
@s-1 = internal constant [5 x i8] c"true "
@s-2 = internal constant [6 x i8] c"false "


define i32 @main() {
%1 = alloca i32



%x = alloca i32
store i32 4, i32* %x

%2 = load i32, i32* %x
%3 = icmp sle i32 3, %2


%4 = icmp ne i32 4, 2

%5 = and i1 %4, 1
%6 = and i1 %3, %5
br i1 %6, label %7, label %8
;<label>:7


call void @printBool(i1 1)
br label %10
;<label>:8

%9 = bitcast [4 x i8]* @s-0 to i8*

call void @printString(i8* %9)
br label %10
;<label>:10



%11 = icmp eq i1 1, 1

%12 = call i1 @dontCallMe(i32 1)
%13 = or i1 %11, %12
call void @printBool(i1 %13)


%14 = mul i32 -1, 5
%15 = icmp slt i32 4, %14

%16 = call i1 @dontCallMe(i32 2)
%17 = and i1 %15, %16
call void @printBool(i1 %17)

%18 = load i32, i32* %x
%19 = icmp eq i32 4, %18


%20 = xor i1 0, 1

%21 = icmp eq i1 1, %20

%22 = and i1 %21, 1
%23 = and i1 %19, %22
call void @printBool(i1 %23)


%24 = call i1 @implies(i1 0,i1 0)
call void @printBool(i1 %24)


%25 = call i1 @implies(i1 0,i1 1)
call void @printBool(i1 %25)


%26 = call i1 @implies(i1 1,i1 0)
call void @printBool(i1 %26)


%27 = call i1 @implies(i1 1,i1 1)
call void @printBool(i1 %27)


store i32 0, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define i1 @dontCallMe(i32 %x) {
%1 = alloca i1
%2 = alloca i32
store i32 %x, i32* %2
%3 = load i32, i32* %2
call void @printInt(i32 %3)


store i1 1, i1* %1
br label %return



br label %return
return:
%r- = load i1, i1* %1

ret i1 %r-
}
define void @printBool(i1 %b) {
%1 = alloca i1
store i1 %b, i1* %1
%2 = load i1, i1* %1
br i1 %2, label %3, label %5
;<label>:3

%4 = bitcast [5 x i8]* @s-1 to i8*

call void @printString(i8* %4)
br label %7
;<label>:5

%6 = bitcast [6 x i8]* @s-2 to i8*

call void @printString(i8* %6)
br label %7
;<label>:7

br label %return



br label %return
return:
ret void
}
define i1 @implies(i1 %x, i1 %y) {
%1 = alloca i1
%2 = alloca i1
%3 = alloca i1
store i1 %x, i1* %2
store i1 %y, i1* %3

%4 = load i1, i1* %2
%5 = xor i1 %4, 1

%6 = load i1, i1* %2
%7 = load i1, i1* %3
%8 = icmp eq i1 %6, %7
%9 = or i1 %5, %8
store i1 %9, i1* %1
br label %return



br label %return
return:
%r- = load i1, i1* %1

ret i1 %r-
}