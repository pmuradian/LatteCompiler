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



%2 = call i32 @ev(i32 17)
call void @printInt(i32 %2)


store i32 0, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define i32 @ev(i32 %y) {
%1 = alloca i32
%2 = alloca i32
store i32 %y, i32* %2
%3 = load i32, i32* %2

%4 = icmp sgt i32 %3, 0
br i1 %4, label %5, label %10
;<label>:5

%6 = load i32, i32* %2

%7 = sub i32 %6, 2

%8 = call i32 @ev(i32 %7)
store i32 %8, i32* %1
br label %return
br label %18
;<label>:10
%11 = load i32, i32* %2

%12 = icmp slt i32 %11, 0
br i1 %12, label %13, label %15
;<label>:13


store i32 0, i32* %1
br label %return
br label %17
;<label>:15


store i32 1, i32* %1
br label %return
br label %17
;<label>:17
br label %18
;<label>:18



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}