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
@s-0 = internal constant [4 x i8] c"foo "


define i32 @main() {
%1 = alloca i32



%i = alloca i32
store i32 78, i32* %i


%i2 = alloca i32
store i32 1, i32* %i2
%2 = load i32, i32* %i2
call void @printInt(i32 %2)

%3 = load i32, i32* %i
call void @printInt(i32 %3)
br label %4
;<label>:4
%5 = load i32, i32* %i

%6 = icmp sgt i32 %5, 76
br i1 %6, label %7, label %14
;<label>:7

%8 = load i32, i32* %i
%9 = sub i32 %8, 1
store i32 %9, i32* %i
%10 = load i32, i32* %i
call void @printInt(i32 %10)

%11 = load i32, i32* %i

%12 = add i32 %11, 7
%i3 = alloca i32
store i32 %12, i32* %i3
%13 = load i32, i32* %i3
call void @printInt(i32 %13)

br label %4
;<label>:14

%15 = load i32, i32* %i
call void @printInt(i32 %15)
%16 = load i32, i32* %i

%17 = icmp sgt i32 %16, 4
br i1 %17, label %18, label %20
;<label>:18


%i4 = alloca i32
store i32 4, i32* %i4
%19 = load i32, i32* %i4
call void @printInt(i32 %19)
br label %22
;<label>:20

%21 = bitcast [4 x i8]* @s-0 to i8*

call void @printString(i8* %21)
br label %22
;<label>:22

%23 = load i32, i32* %i
call void @printInt(i32 %23)


store i32 0, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}