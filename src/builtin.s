String.nth:
    movzx rax, byte [rdi + rsi]
    ret

String.set:
    mov [rdi + rsi], dl
    ret

OS.time:
    push rbx
    sub rsp, 16
    mov rbx, rsp
    mov rdi, rbx
    xor esi, esi
    call gettimeofday
    imul rcx, qword [rbx], 1000
    mov rax, qword [rbx+8]
    mov esi, 1000
    cqo
    idiv rsi
    add rax, rcx
    add rsp, 16
    pop rbx
    ret

OS.listdir:
    push r14
    push rbx
    push rax
    mov r14, rdi
    call Array.new
    mov rbx, rax
    mov rdi, r14
    call opendir
    mov r14, rax
.OS.listdir.1:
    mov rdi, r14
    call readdir
    test rax, rax
    je .OS.listdir.3
    cmp byte [rax+19], 46
    jne .OS.listdir.2
    movzx ecx, byte [rax+20]
    test ecx, ecx
    je .OS.listdir.1
    cmp ecx, 46
    jne .OS.listdir.2
    cmp byte [rax+21], 0
    je .OS.listdir.1
.OS.listdir.2:
    add rax, 19
    mov rdi, rax
    call strdup
    mov rsi, rax
    mov rdi, rbx
    call Array.push
    jmp .OS.listdir.1
.OS.listdir.3:
    mov rdi, r14
    call closedir
    mov rax, rbx
    add rsp, 8
    pop rbx
    pop r14
    ret

Array.nth:
    mov rax, [rdi]
    mov rax, [rax + rsi*8]
    ret

Array.set:
    mov rax, [rdi]
    mov [rax + rsi*8], rdx
    ret

Array.push:
    push r14
    push rbx
    push rax
    mov r14, rsi
    mov rbx, rdi
    mov rax, [rdi]
    mov rcx, [rdi + 16]
    cmp rcx, [rdi + 8]
    jne .Array.push.1
    lea rdx, [rcx + rcx]
    mov rsi, 4
    test rcx, rcx
    cmovnz rsi, rdx
    mov [rbx + 8], rsi
    shl rsi, 3
    mov rdi, rax
    call realloc
    mov [rbx], rax
    mov rcx, [rbx + 16]
.Array.push.1:
    mov [rax + rcx*8], r14
    inc qword [rbx + 16]
    add rsp, 8
    pop rbx
    pop r14
    ret

Array.size:
    mov rax, [rdi + 16]
    ret

Array.free:
    push rbx
    mov rbx, rdi
    mov rdi, [rdi]
    call free
    mov rdi, rbx
    pop rbx
    jmp free
