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



%x = alloca i32
store i32 0, i32* %x
%y = alloca i32
store i32 0, i32* %y

store i32 45, i32* %x

%2 = mul i32 -1, 36
store i32 %2, i32* %y
%3 = load i32, i32* %x
call void @printInt(i32 %3)
%4 = load i32, i32* %y
call void @printInt(i32 %4)


store i32 0, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}