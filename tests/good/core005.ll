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
store i32 56, i32* %y
%2 = load i32, i32* %y

%3 = add i32 %2, 45


%4 = icmp sle i32 %3, 2
br i1 %4, label %5, label %6
;<label>:5


store i32 1, i32* %x
br label %7
;<label>:6


store i32 2, i32* %x
br label %7
;<label>:7

%8 = load i32, i32* %x
call void @printInt(i32 %8)


store i32 0, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}