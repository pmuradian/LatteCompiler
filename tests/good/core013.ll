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
@s-0 = internal constant [3 x i8] c"&& "
@s-1 = internal constant [3 x i8] c"|| "
@s-2 = internal constant [2 x i8] c"! "
@s-3 = internal constant [6 x i8] c"false "
@s-4 = internal constant [5 x i8] c"true "


define i32 @main() {
%1 = alloca i32


%2 = bitcast [3 x i8]* @s-0 to i8*

call void @printString(i8* %2)

%3 = mul i32 -1, 1
%4 = call i1 @test(i32 %3)

%5 = call i1 @test(i32 0)
%6 = and i1 %4, %5
call void @printBool(i1 %6)

%7 = mul i32 -1, 2
%8 = call i1 @test(i32 %7)

%9 = call i1 @test(i32 1)
%10 = and i1 %8, %9
call void @printBool(i1 %10)

%11 = call i1 @test(i32 3)

%12 = mul i32 -1, 5
%13 = call i1 @test(i32 %12)
%14 = and i1 %11, %13
call void @printBool(i1 %14)

%15 = call i1 @test(i32 234234)

%16 = call i1 @test(i32 21321)
%17 = and i1 %15, %16
call void @printBool(i1 %17)
%18 = bitcast [3 x i8]* @s-1 to i8*

call void @printString(i8* %18)

%19 = mul i32 -1, 1
%20 = call i1 @test(i32 %19)

%21 = call i1 @test(i32 0)
%22 = or i1 %20, %21
call void @printBool(i1 %22)

%23 = mul i32 -1, 2
%24 = call i1 @test(i32 %23)

%25 = call i1 @test(i32 1)
%26 = or i1 %24, %25
call void @printBool(i1 %26)

%27 = call i1 @test(i32 3)

%28 = mul i32 -1, 5
%29 = call i1 @test(i32 %28)
%30 = or i1 %27, %29
call void @printBool(i1 %30)

%31 = call i1 @test(i32 234234)

%32 = call i1 @test(i32 21321)
%33 = or i1 %31, %32
call void @printBool(i1 %33)
%34 = bitcast [2 x i8]* @s-2 to i8*

call void @printString(i8* %34)

call void @printBool(i1 1)

call void @printBool(i1 0)


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
%3 = xor i1 %2, 1

br i1 %3, label %4, label %6
;<label>:4

%5 = bitcast [6 x i8]* @s-3 to i8*

call void @printString(i8* %5)
br label %8
;<label>:6

%7 = bitcast [5 x i8]* @s-4 to i8*

call void @printString(i8* %7)
br label %8
;<label>:8

br label %return



br label %return
return:
ret void
}
define i1 @test(i32 %i) {
%1 = alloca i1
%2 = alloca i32
store i32 %i, i32* %2
%3 = load i32, i32* %2
call void @printInt(i32 %3)

%4 = load i32, i32* %2

%5 = icmp sgt i32 %4, 0
store i1 %5, i1* %1
br label %return



br label %return
return:
%r- = load i1, i1* %1

ret i1 %r-
}