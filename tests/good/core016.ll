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



%y = alloca i32
store i32 17, i32* %y
br label %2
;<label>:2
%3 = load i32, i32* %y

%4 = icmp sgt i32 %3, 0
br i1 %4, label %5, label %8
;<label>:5
%6 = load i32, i32* %y

%7 = sub i32 %6, 2

store i32 %7, i32* %y
br label %2
;<label>:8

%9 = load i32, i32* %y

%10 = icmp slt i32 %9, 0
br i1 %10, label %11, label %13
;<label>:11


call void @printInt(i32 0)


store i32 0, i32* %1
br label %return

br label %15
;<label>:13


call void @printInt(i32 1)


store i32 0, i32* %1
br label %return

br label %15
;<label>:15



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}