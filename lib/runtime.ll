@dnl = internal constant [4 x i8] c"%d\0A\00"
@fnl = internal constant [6 x i8] c"%.1f\0A\00"
@d   = internal constant [3 x i8] c"%d\00"	
@lf  = internal constant [4 x i8] c"%lf\00"	

declare i32 @printf(i8*, ...) 
declare i32 @scanf(i8*, ...)
declare i32 @puts(i8*)

; For string concatenation
declare i64 @strlen(i8*)
declare i8* @malloc(i64)
declare void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i32, i1)

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
  %total_len = add i64 %str1_len, %str2_len
  %new_str = call i8* @malloc(i64 %total_len)
  
  %str1_end = getelementptr i8, i8* %str1, i64 %str1_len
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %new_str, i8* %str1, i64 %str1_len, i32 1, i1 false)
  
  %new_str_end = getelementptr i8, i8* %new_str, i64 %str1_len
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %new_str_end, i8* %str2, i64 %str2_len, i32 1, i1 false)
  
  ret i8* %new_str
}