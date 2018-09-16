subroutine blah(arg1,  arg2, sum, mult)
real,intent(in)  :: arg1
real,intent(in) ::  arg2
real,intent(out) ::  sum
real,intent(out) ::  mult
    
real :: a
!TODO_add_body
    
    sum = arg1+arg2
    mult = arg1 * arg2
    print *, arg1+arg2
    print *, arg1 * arg2
    call avg(arg1, arg2, a)
    print *, a    
    return
end subroutine blah

subroutine avg(arg1,  arg2, res)
real,intent(in)  :: arg1
real,intent(in) ::  arg2
real,intent(out) ::  res

res= (arg1+arg2)/2
!TODO_add_body
    
end subroutine avg