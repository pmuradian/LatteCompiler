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



%2 = call i32 @fac(i32 5)
call void @printInt(i32 %2)


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