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




%2 = call i32 @foo()%x = alloca i32
store i32 %2, i32* %x
%3 = load i32, i32* %x
call void @printInt(i32 %3)


store i32 0, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define i32 @foo() {
%1 = alloca i32




store i32 10, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}