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

e = internal constant [2 x i8] c"e "
main = internal constant [5 x i8] c"main "
printInt = internal constant [9 x i8] c"printInt "
printString = internal constant [12 x i8] c"printString "
error = internal constant [6 x i8] c"error "
readInt = internal constant [8 x i8] c"readInt "
readString = internal constant [11 x i8] c"readString "
1 = internal constant [2 x i8] c"1 "
x = internal constant [2 x i8] c"x "



%x = alloca i8*