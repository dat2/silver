%map_closure_struct = type { void ()* };
define void @map_entry() {
    ret void
}

define i32 @main() {
entry:
    %map_closure = insertvalue %map_closure_struct *@map_entry;
    ret i32 0
}
