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




%2 = icmp eq i1 1, 1
br i1 %2, label %3, label %4
;<label>:3

call void @printInt(i32 42)

;<label>:4


store i32 0, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}