@dnl = internal constant [4 x i8] c"%d\0A\00"
@fnl = internal constant [6 x i8] c"%.1f\0A\00"
@d   = internal constant [3 x i8] c"%d\00"	
@lf  = internal constant [4 x i8] c"%lf\00"	

declare i32 @printf(i8*, ...) 
declare i32 @scanf(i8*, ...)
declare i32 @puts(i8*)

; For string concatenation
declare i64 @strlen(i8*)
declare i64 @strcpy(i8*, i8*)
declare i64 @strcat(i8*, i8*)
declare i8* @malloc(i64)

define void @printInt(i32 %x) {
       %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
       call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
       ret void
}

define void @printString(i8* %s) {
entry:  call i32 @puts(i8* %s)
	ret void
}

define i32 @readInt() {
entry:	%res = alloca i32
        %t1 = getelementptr [3 x i8], [3 x i8]* @d, i32 0, i32 0
	call i32 (i8*, ...) @scanf(i8* %t1, i32* %res)
	%t2 = load i32, i32* %res
	ret i32 %t2
}

define i8* @concat(i8* %str1, i8* %str2) {
entry:
  %str1_len = call i64 @strlen(i8* %str1)
  %str2_len = call i64 @strlen(i8* %str2)
  %sum_len = add i64 %str1_len, %str2_len
  %total_len = add i64 %sum_len, 1
  %new_str = call i8* @malloc(i64 %total_len)
  
  call i64 @strcpy(i8* %new_str, i8* %str1)
  call i64 @strcat(i8* %new_str, i8* %str2)
  
  ret i8* %new_str
}