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




define i32 @main() {
%1 = alloca i32



br i1 0, label %2, label %3
;<label>:2

store i32 0, i32* %1
br label %return

;<label>:3


br label %return
return:
%4 = load i32, i32* %1


ret i32 %4
}