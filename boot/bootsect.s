!
! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
! //SYS_SIZE 是要加载的滴答数（16 bytes）
! //0x3000 是 0x3000 bytes = 196 kB,远远超出现阶段linux的需要
!
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
! //根据bios启动惯例bootsect.s在0x7c00处被加载，并将它自己移动到0x90000处并且
! //转跳到此处
! 
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
! // 然后在他之后（0x90200）直接加载 ‘setup’，在0x10000处的系统--使用BIOS中断
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
! //注意！现在系统最大是 8*0xFFFF bytes（512KB） 大。这个甚至在将来应当不会有问题。
! //我想保持内核的简洁。这个512KB的内核大小应当是足够的，特别是这个内核像
! //minix一样没有包含缓冲区缓存
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.
! //加载器已经被尽可能的精简，在一个不可中断的循环里不断的读取错误。手动重启。
! //它加载的相当快因为它尽可能的一次读取所有扇区的信息。
!
.globl begtext, begdata, begbss, endtext, enddata, endbss  ! //.globl 伪指令，用于定义随后的标识符是外部的或全局的。
.text                                 ! //定义文本段(有的说当前代码段 无奈)
begtext:
.data					! //定义数据段
begdata:
.bss					! //定义未初始化数据段(Block Started by Symbol)
begbss:
.text					! //文本段：

SETUPLEN = 4				! nr of setup-sectors  //setup-sectors数(应该是吧,nr是个什么鬼，看名字是setup长度) -- 段地址 注意以下不一一标记
BOOTSEG  = 0x07c0			! original address of boot-sector //boot-sector起始地址
INITSEG  = 0x9000			! we move boot here - out of the way //将boot移动到此地址 --大概是避开的意思
SETUPSEG = 0x9020			! setup starts here // 从这里开始setup程序
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536). //(FFFF + 1) 系统在此处加载
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading  =0x4000 //停止加载的地址 (3FFFF+ 1)

! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc  //根文件系统在第一个硬盘的上的第一个分区。
ROOT_DEV = 0x306   
! // 0x306设备号指定根文件系统在第2个硬盘的第1个分区。作者当时写这个内核时候实在第二个硬盘安装的这个
! //内核，所以他写是第二个硬盘。此处在编译时可以根据自己文件系统所在的位置来更改这个 设备号 。
! //此处的设备号是linux老式的硬盘命名规则。
! //设备号 = 主设备号 * 256 + 次设备号 (DEV_NO = (major << 8) + minor)
! //(主设备号: 1-内存，2-磁盘，3-硬盘，4-ttyx，5-tty，6-并行口，7-非命名管道)

entry start
start:
	mov	ax,#BOOTSEG
	mov	ds,ax     		!dataseg=0x07c0_The original address
	mov	ax,#INITSEG
	mov	es,ax			!extraseg=0x9000_The boot address
	mov	cx,#256
	sub	si,si         	!si=0
	sub	di,di			!di=0
	rep					!连续执行串指令（CX不等于0），每执行一次cx减一
	movw				！串（字）传送，将si所指向的数据段的一个字送到di所指附加段内一个字的存储单元。如果CF=0,SI DI +2,如果CF=1,-2
!此段将si 0~256数据段内数据送到DI 0~256附加段内，以2为一单位
	jmpi	go,INITSEG		!段间转跳，INITSEG表示段地址，go是偏移地址
go:	mov	ax,cs
	mov	ds,ax
	mov	es,ax
! put stack at 0x9ff00.
	mov	ss,ax
	mov	sp,#0xFF00		! arbitrary value >>512

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.

load_setup:
	mov	dx,#0x0000		! drive 0, head 0
	mov	cx,#0x0002		! sector 2, track 0
	mov	bx,#0x0200		! address = 512, in INITSEG
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors  读入4扇区
	int	0x13			! read it
	jnc	ok_load_setup		! ok - continue
	mov	dx,#0x0000
	mov	ax,#0x0000		! reset the diskette
	int	0x13
	j	load_setup

ok_load_setup:

! Get disk drive parameters, specifically nr of sectors/track

	mov	dl,#0x00
	mov	ax,#0x0800		! AH=8 is get drive parameters
	int	0x13
	mov	ch,#0x00
	seg cs       			！返回段基址
	mov	sectors,cx
	mov	ax,#INITSEG
	mov	es,ax

! Print some inane message

	mov	ah,#0x03		! read cursor pos
	xor	bh,bh			!bh=0
	int	0x10
	
	mov	cx,#24
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)

	mov	ax,#SYSSEG
	mov	es,ax		! segment of 0x010000
	call	read_it
	call	kill_motor

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track
head:	.word 0			! current head
track:	.word 0			! current track

read_it:
	mov ax,es
	test ax,#0x0fff
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	ret
ok1_read:
	seg cs
	mov ax,sectors
	sub ax,sread
	mov cx,ax
	shl cx,#9
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ax,#0x1000
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	push ax
	push bx
	push cx
	push dx
	mov dx,track
	mov cx,sread
	inc cx
	mov ch,dl
	mov dx,head
	mov dh,dl
	mov dl,#0
	and dx,#0x0100
	mov ah,#2
	int 0x13
	jc bad_rt
	pop dx
	pop cx
	pop bx
	pop ax
	ret
bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading system ..."
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:
