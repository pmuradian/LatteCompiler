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




%2 = call i32 @readInt()%x = alloca i32
store i32 %2, i32* %x


%3 = call i8* @readString()%y = alloca i8*
store i8* %3, i8** %y


%4 = call i8* @readString()%z = alloca i8*
store i8* %4, i8** %z
%5 = load i32, i32* %x

%6 = sub i32 %5, 5

call void @printInt(i32 %6)
%7 = load i8*, i8** %y
%8 = load i8*, i8** %z
%9 = call i8* @concat(i8* %7, i8* %8)

call void @printString(i8* %9)


store i32 0, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}