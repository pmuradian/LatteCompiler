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



call void @foo()


store i32 0, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define void @foo() {


%1 = bitcast [4 x i8]* @s-0 to i8*

call void @printString(i8* %1)
br label %return



br label %return
return:
ret void
}