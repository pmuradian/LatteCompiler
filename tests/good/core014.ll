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



define i32 @main() {
%1 = alloca i32



%lo = alloca i32
store i32 0, i32* %lo
%hi = alloca i32
store i32 0, i32* %hi
%mx = alloca i32
store i32 0, i32* %mx

store i32 1, i32* %lo
%2 = load i32, i32* %lo
store i32 %2, i32* %hi

store i32 5000000, i32* %mx
%3 = load i32, i32* %lo
call void @printInt(i32 %3)
br label %4
;<label>:4
%5 = load i32, i32* %hi
%6 = load i32, i32* %mx
%7 = icmp slt i32 %5, %6
br i1 %7, label %8, label %16
;<label>:8

%9 = load i32, i32* %hi
call void @printInt(i32 %9)
%10 = load i32, i32* %lo
%11 = load i32, i32* %hi
%12 = add i32 %10, %11

store i32 %12, i32* %hi
%13 = load i32, i32* %hi
%14 = load i32, i32* %lo
%15 = sub i32 %13, %14

store i32 %15, i32* %lo

br label %4
;<label>:16



store i32 0, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}