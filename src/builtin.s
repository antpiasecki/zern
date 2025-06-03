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
.LBB5_1:
    mov rdi, r14
    call readdir
    test rax, rax
    je .LBB5_7
    cmp byte [rax+19], 46
    jne .LBB5_6
    movzx ecx, byte [rax+20]
    test ecx, ecx
    je .LBB5_1
    cmp ecx, 46
    jne .LBB5_6
    cmp byte [rax+21], 0
    je .LBB5_1
.LBB5_6:
    add rax, 19
    mov rdi, rax
    call strdup
    mov rsi, rax
    mov rdi, rbx
    call Array.push
    jmp .LBB5_1
.LBB5_7:
    mov rdi, r14
    call closedir
    mov rax, rbx
    add rsp, 8
    pop rbx
    pop r14
    ret

Array.new:
    mov rdi, 1
    mov rsi, 24
    jmp calloc

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
    jne .no_realloc
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
.no_realloc:
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

Math.isqrt:
    xor rax, rax
    mov rcx, 1
    mov rbx, rdi
    shl rcx, 62
.isqrt.1:
    cmp rcx, 0
    je .isqrt.5
    cmp rcx, rbx
    jbe .isqrt.2
    shr rcx, 2
    jmp .isqrt.1
.isqrt.2:
    cmp rcx, 0
    je .isqrt.5
    mov rdx, rax
    add rdx, rcx
    cmp rbx, rdx
    jb .isqrt.3
    sub rbx, rdx
    shr rax, 1
    add rax, rcx
    jmp .isqrt.4
.isqrt.3:
    shr rax, 1
.isqrt.4:
    shr rcx, 2
    jmp .isqrt.2
.isqrt.5:
    ret