declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()

define i32 @someFunction(i32 %a, i8* %b) {
%1 = alloca i32
%2 = alloca i8*
store i32 a, i32* %1
store i8* b, i8** %2
{

store i32 12, i32* %a
%k = alloca i32

%3 = load i32, i32* %k
%4 = add i32 0, %3
%j = alloca i32
store i32 %4, i32* %j
%5 = load i32, i32* %j

%6 = icmp eq i32 %5, 0
br i1 %6, label %7, label %8
;<label>:7
{

ret i32 1
}
[Variable {varType = "i32 i32 i8*", name = "someFunction", alias = "someFunction", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "void i32", name = "printInt", alias = "printInt", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "void i8*", name = "printString", alias = "printString", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "void", name = "error", alias = "error", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "i32 i32", name = "readInt", alias = "readInt", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "i8* i8*", name = "readString", alias = "readString", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "i32", name = "a", alias = "%1", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i8*", name = "b", alias = "%2", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "k", alias = "k", isFunc = False, level = 0, isUsed = False, isGenerated = False},Variable {varType = "i32", name = "k", alias = "k", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%4", alias = "%4", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "j", alias = "j", isFunc = False, level = 0, isUsed = False, isGenerated = False},Variable {varType = "i32", name = "j", alias = "j", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%6", alias = "%6", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%7", alias = "%7", isFunc = False, level = 0, isUsed = False, isGenerated = True}]
;<label> 8

%9 = load i32, i32* %a
%10 = icmp eq i32 1, %9
br i1 %10, label %11, label %12
;<label>:11
{

ret i32 1
}
[Variable {varType = "i32 i32 i8*", name = "someFunction", alias = "someFunction", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "void i32", name = "printInt", alias = "printInt", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "void i8*", name = "printString", alias = "printString", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "void", name = "error", alias = "error", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "i32 i32", name = "readInt", alias = "readInt", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "i8* i8*", name = "readString", alias = "readString", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "i32", name = "a", alias = "%1", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i8*", name = "b", alias = "%2", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "k", alias = "k", isFunc = False, level = 0, isUsed = False, isGenerated = False},Variable {varType = "i32", name = "k", alias = "k", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%4", alias = "%4", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "j", alias = "j", isFunc = False, level = 0, isUsed = False, isGenerated = False},Variable {varType = "i32", name = "j", alias = "j", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%6", alias = "%6", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%7", alias = "%7", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%8", alias = "%8", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "a", alias = "a", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%10", alias = "%10", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%11", alias = "%11", isFunc = False, level = 0, isUsed = False, isGenerated = True}]
;<label> 12
br label %13
;<label>:13
%14 = load i32, i32* %j

%15 = icmp slt i32 %14, 12
br i1 %15, label %16, label %19
;<label>:16

{
%17 = load i32, i32* %j
%18 = add i32 %17, 1
store i32 %18, i32* %j
}
[Variable {varType = "i32 i32 i8*", name = "someFunction", alias = "someFunction", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "void i32", name = "printInt", alias = "printInt", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "void i8*", name = "printString", alias = "printString", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "void", name = "error", alias = "error", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "i32 i32", name = "readInt", alias = "readInt", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "i8* i8*", name = "readString", alias = "readString", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "i32", name = "a", alias = "%1", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i8*", name = "b", alias = "%2", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "k", alias = "k", isFunc = False, level = 0, isUsed = False, isGenerated = False},Variable {varType = "i32", name = "k", alias = "k", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%4", alias = "%4", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "j", alias = "j", isFunc = False, level = 0, isUsed = False, isGenerated = False},Variable {varType = "i32", name = "j", alias = "j", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%6", alias = "%6", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%7", alias = "%7", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%8", alias = "%8", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "a", alias = "a", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%10", alias = "%10", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%11", alias = "%11", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%12", alias = "%12", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%13", alias = "%13", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "j", alias = "j", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%15", alias = "%15", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%16", alias = "%16", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%17", alias = "%17", isFunc = False, level = 1, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%18", alias = "%18", isFunc = False, level = 1, isUsed = False, isGenerated = True}]br label %13
;<label>:19

%20 = load i32, i32* %k
%21 = load i32, i32* %j
%22 = add i32 %20, %21


%23 = add i32 %22, 6

store i32 %23, i32* %j

%24 = mul i32 -1, 9
store i32 %24, i32* %j
%25 = load i32, i32* %k
%26 = mul i32 -1, %25
store i32 %26, i32* %k
%27 = load i32, i32* %k
call void @printInt(i32 %27)
%28 = load i32, i32* %k
ret i32 %28
}
[Variable {varType = "i32 i32 i8*", name = "someFunction", alias = "someFunction", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "void i32", name = "printInt", alias = "printInt", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "void i8*", name = "printString", alias = "printString", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "void", name = "error", alias = "error", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "i32 i32", name = "readInt", alias = "readInt", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "i8* i8*", name = "readString", alias = "readString", isFunc = True, level = 0, isUsed = True, isGenerated = False},Variable {varType = "i32", name = "a", alias = "%1", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i8*", name = "b", alias = "%2", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "k", alias = "k", isFunc = False, level = 0, isUsed = False, isGenerated = False},Variable {varType = "i32", name = "k", alias = "k", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%4", alias = "%4", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "j", alias = "j", isFunc = False, level = 0, isUsed = False, isGenerated = False},Variable {varType = "i32", name = "j", alias = "j", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%6", alias = "%6", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%7", alias = "%7", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%8", alias = "%8", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "a", alias = "a", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%10", alias = "%10", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%11", alias = "%11", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%12", alias = "%12", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%13", alias = "%13", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "j", alias = "j", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%15", alias = "%15", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%16", alias = "%16", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%17", alias = "%17", isFunc = False, level = 1, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%18", alias = "%18", isFunc = False, level = 1, isUsed = False, isGenerated = True},Variable {varType = "label", name = "%19", alias = "%19", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "k", alias = "k", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "j", alias = "j", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%22", alias = "%22", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%23", alias = "%23", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%24", alias = "%24", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "k", alias = "k", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "%26", alias = "%26", isFunc = False, level = 0, isUsed = False, isGenerated = True},Variable {varType = "i32", name = "k", alias = "k", isFunc = False, level = 0, isUsed = False, isGenerated = True}]}


