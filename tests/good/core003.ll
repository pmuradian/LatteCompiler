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



define i32 @f() {
%1 = alloca i32



br i1 1, label %2, label %4
;<label>:2


store i32 0, i32* %1
br label %return
br label %5
;<label>:4


br label %5
;<label>:5



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define i32 @g() {
%1 = alloca i32



br i1 0, label %2, label %3
;<label>:2


br label %5
;<label>:3


store i32 0, i32* %1
br label %return
br label %5
;<label>:5



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}
define void @p() {





br label %return
return:
ret void
}
define i32 @main() {
%1 = alloca i32



call void @p()


store i32 0, i32* %1
br label %return



br label %return
return:
%r- = load i32, i32* %1

ret i32 %r-
}