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
@s-0 = internal constant [7 x i8] c"string "
@s-1 = internal constant [2 x i8] c"  "
@s-2 = internal constant [14 x i8] c"concatenation "
@s-3 = internal constant [5 x i8] c"true "
@s-4 = internal constant [6 x i8] c"false "


define i32 @main() {
%1 = alloca i32



%x = alloca i32
store i32 56, i32* %x


%2 = mul i32 -1, 23%y = alloca i32
store i32 %2, i32* %y
%3 = load i32, i32* %x
%4 = load i32, i32* %y
%5 = add i32 %3, %4

call void @printInt(i32 %5)
%6 = load i32, i32* %x
%7 = load i32, i32* %y
%8 = sub i32 %6, %7

call void @printInt(i32 %8)
%9 = load i32, i32* %x
%10 = load i32, i32* %y
%11 = mul i32 %9, %10

call void @printInt(i32 %11)


%12 = sdiv i32 45, 2

call void @printInt(i32 %12)


%13 = srem i32 78, 3

call void @printInt(i32 %13)
%14 = load i32, i32* %x
%15 = load i32, i32* %y
%16 = sub i32 %14, %15

%17 = load i32, i32* %x
%18 = load i32, i32* %y
%19 = add i32 %17, %18

%20 = icmp sgt i32 %16, %19
call void @printBool(i1 %20)
%21 = load i32, i32* %x
%22 = load i32, i32* %y
%23 = sdiv i32 %21, %22

%24 = load i32, i32* %x
%25 = load i32, i32* %y
%26 = mul i32 %24, %25

%27 = icmp sle i32 %23, %26
call void @printBool(i1 %27)
%28 = bitcast [7 x i8]* @s-0 to i8*

%29 = bitcast [2 x i8]* @s-1 to i8*

%30 = call i8* @concat(i8* %28, i8* %29)

%31 = bitcast [14 x i8]* @s-2 to i8*

%32 = call i8* @concat(i8* %30, i8* %31)

call void @printString(i8* %32)


store i32 0, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define void @printBool(i1 %b) {
%1 = alloca i1
store i1 %b, i1* %1
%2 = load i1, i1* %1
br i1 %2, label %3, label %6
;<label>:3

%4 = bitcast [5 x i8]* @s-3 to i8*

call void @printString(i8* %4)
br label %return

br label %9
;<label>:6

%7 = bitcast [6 x i8]* @s-4 to i8*

call void @printString(i8* %7)
br label %return

br label %9
;<label>:9



br label %return
return:
ret void
}