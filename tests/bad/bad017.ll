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


Argument mismatch in function call foo["i32","i32"]
define i32 @foo(i32 %y) {
%1 = alloca i32
%2 = alloca i32
store i32 %y, i32* %2

%3 = load i32, i32* %2
store i32 %3, i32* %1
br label %return



br label %return
return:
%5 = load i32, i32* %1

ret i32 %5
}