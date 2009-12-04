
; **********************************************************************
; *           boot.asm for OMOS(Our Mini Operating System)             *
; **********************************************************************
; *                                             By Zhaomeng Peng, 2009 *
; **********************************************************************


org  07c00h			                ; Boot 状态, Bios 将把 Boot Sector 加载到内存地址为 0:7C00 处并开始执行

BaseOfStack	equ	07c00h	        ; Boot状态下堆栈基地址(栈底, 从这个位置向低地址生长)

%include	"load.inc"
;================================================================================================

	jmp short LABEL_START		      ; 跳转到启动部分
	nop				                    

; 下面是 FAT12 磁盘的头, 之所以包含它是因为下面用到了磁盘的一些信息
%include	"fat12hdr.inc"

LABEL_START:	
; 代码开始
; 初始化各个段寄存器
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, BaseOfStack           ;栈基址

	; 清屏
	mov	ax, 0600h		              ; AH = 6,  AL = 0h, 向上滚屏，清窗口
	mov	bx, 0700h		              ; 黑底白字(BH = 07h)
	mov	cx, 0			                ; 左上角: (0, 0)
	mov	dx, 0184fh		            ; 右下角: (80, 50)
	int	10h			                  ; int 10h
; ---------------------------------------------------------------------------
; int 10h 中断功能:
; 功能号：06H和07H
; 功能：初始化屏幕或滚屏
; 入口参数：AH＝06H—向上滚屏，07H—向下滚屏
;           AL＝滚动行数(0—清窗口)
;           BH＝空白区域的缺省属性
;           (CH、CL)＝窗口的左上角位置(Y坐标，X坐标)
;           (DH、DL)＝窗口的右下角位置(Y坐标，X坐标)
; 出口参数： 无 
;----------------------------------------------------------------------------

;============================================================================
;变量
;----------------------------------------------------------------------------
wRootDirSizeForLoop	dw	RootDirSectors	; Root Directory 占用的扇区数, 在循环中会递减至零.
wSectorNo		        dw	0		            ; 要读取的扇区号
bOdd			          db	0		            ; 奇数还是偶数
;============================================================================

;============================================================================
;字符串
;----------------------------------------------------------------------------
LoaderFileName		db	"LOADER  BIN", 0	; LOADER.BIN 之文件名,8+3,用来比较用的
; 为简化代码, 下面每个字符串的长度均为 MessageLength
MessageLength		  equ	9
BootMessage:		  db	"Booting  "       ; 9字节, 不够则用空格补齐. 序号 0
Message1		      db	"Ready.   "       ; 9字节, 不够则用空格补齐. 序号 1
Message2		      db	"No LOADER"       ; 9字节, 不够则用空格补齐. 序号 2
;============================================================================


	mov	dh, 0			                
	call	DispStr			            ; 显示序号为0的字符串"Booting  "
	
	xor	ah, ah	                  ; ┐
	xor	dl, dl	                  ; ├ int 13h功能，软驱复位
	int	13h	                      ; ┘

;============================================================================	
; 下面在软盘 A 盘的根目录下寻找文件 LOADER.BIN
	mov	word [wSectorNo], SectorNoOfRootDirectory   ; 开始读取根目录区
	
LABEL_SEARCH_IN_ROOT_DIR_BEGIN:     ; 在根目录下寻找文件
	cmp	word [wRootDirSizeForLoop], 0	; 是否将根目录区全部读完 
	jz	LABEL_NO_LOADERBIN		        ; 表示没有找到Loader.bin
	dec	word [wRootDirSizeForLoop]	  ; 循环次数减1
	mov	ax, BaseOfLoader
	mov	es, ax			                  ; es <- BaseOfLoader
	mov	bx, OffsetOfLoader	          ; bx <- OffsetOfLoader	于是, es:bx = BaseOfLoader:OffsetOfLoader
	                                  ; es:bx指向欲将Loader加载到的位置
	                                  
	mov	ax, [wSectorNo]	              ; ax <- Root Directory 中的某 Sector 号
	mov	cl, 1
	call	ReadSector                  ; 调用ReadSector，其中，ax=欲读扇区号，cl=欲读扇区数

;----------------------------------------------------------------------------
; 比较一个扇区
	mov	si, LoaderFileName	                ; ds:si -> "LOADER  BIN"
	mov	di, OffsetOfLoader	                ; es:di -> BaseOfLoader:0100 = BaseOfLoader*10h+100
	cld
	mov	dx, 16                              ; 16*32=512Bytes=1Sector，根目录中每个条目占32字节
LABEL_SEARCH_FOR_LOADERBIN:
	cmp	dx, 0					                      ; ┐ 循环次数控制,
	jz	LABEL_GO_TO_NEXT_SECTOR_IN_ROOT_DIR	; ├ 如果已经读完了一个 Sector,
	dec	dx					                        ; ┘ 就跳到下一个 Sector	
;----------------------------------------------------------------------------
; 比较文件
mov	cx, 11                                ; 文件名长11位
LABEL_CMP_FILENAME:
	cmp	cx, 0
	jz	LABEL_FILENAME_FOUND	              ; 比较了11次说明找到了
dec	cx
	lodsb				                            ; ds:si -> al
	cmp	al, byte [es:di]
	jz	LABEL_GO_ON
	jmp	LABEL_DIFFERENT		                  ; 只要发现不一样的字符就表明本 DirectoryEntry 不是
                                          ; 要找的 LOADER.BIN
LABEL_GO_ON:
	inc	di
	jmp	LABEL_CMP_FILENAME	                ;	继续循环

LABEL_DIFFERENT:
	and	di, 0FFE0h		                      ; 最后五位清零，使之对齐
	add	di, 20h			                        ; 根目录中每个条目占32(20h)字节，指向下一个条目
	mov	si, LoaderFileName	                
	jmp	LABEL_SEARCH_FOR_LOADERBIN          

LABEL_GO_TO_NEXT_SECTOR_IN_ROOT_DIR:      ; 读取下一个扇区
	add	word [wSectorNo], 1
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN

LABEL_NO_LOADERBIN:                       ; 未找到文件 LOADER.BIN
	mov	dh, 2			                          ; 显示序号为2的字符串：
	call	DispStr			                      ; "No LOADER."

	jmp	$			                              ; 没有找到 LOADER.BIN, 死循环在这里

LABEL_FILENAME_FOUND:			                ; 找到 LOADER.BIN 后便来到这里继续
                                          ; [es:di]这个时候还是指向找到的那个条目
	mov	ax, RootDirSectors
	and	di, 0FFE0h		                      ; 最后五位清零，di -> 当前条目的开始
	add	di, 01Ah		                        ; 定位到簇号DIR_FstClus,要加26，参见条目格式
	mov	cx, word [es:di]
	push	cx			                          ; 保存此 Sector 在 FAT 中的序号
	add	cx, ax                              ; X+RootDirSectors+19-2(=DeltaSectorNo=17) = Real Sector Number
	add	cx, DeltaSectorNo	                  ; 这句完成时 cl 里面变成 LOADER.BIN 的起始扇区号 (从 0 开始数的序号)
	mov	ax, BaseOfLoader
	mov	es, ax			                        ; es <- BaseOfLoader
	mov	bx, OffsetOfLoader	                ; bx <- OffsetOfLoader	
	                                        ; 于是, es:bx = BaseOfLoader:OffsetOfLoader = BaseOfLoader * 10h + OffsetOfLoader
	mov	ax, cx			                        ; ax <- Sector 号

LABEL_GO_ON_LOADING_FILE:
	push	ax			  ; ┓ 
	push	bx			  ; ┃ 
	mov	ah, 0Eh			; ┃ 每读一个扇区就在 "Booting  " 后面打一个点, 形成这样的效果:
	mov	al, '.'			; ┃
	mov	bl, 0Fh			; ┃ 
	int	10h			    ; ┃ Booting ......
	pop	bx			    ; ┃
	pop	ax			    ; ┛
; ---------------------------------------------------------------------------
; int 10h 中断功能:
; 功能号：0EH
; 功能：显示字符(光标前移),光标跟随字符移动
; 入口参数：AL = 字符
　　				BL = 前景色
; 出口参数：无 
;----------------------------------------------------------------------------

	mov	cl, 1
	call	ReadSector                        ; 调用ReadSector，其中，ax=欲读扇区号，cl=欲读扇区数
	pop	ax			                            ; 取出此 Sector 在 FAT 中的序号
	call	GetFATEntry                       ; 根据簇号找到文件所在的下一个扇区号
	cmp	ax, 0FFFh                           ; 如果找到的下一个簇号为0FFFh，说明文件已经全部加载完毕
	jz	LABEL_FILE_LOADED
	push	ax			                          ; 保存 Sector 在 FAT 中的序号
	mov	dx, RootDirSectors                  
	add	ax, dx
	add	ax, DeltaSectorNo
	add	bx, [BPB_BytsPerSec]
	jmp	LABEL_GO_ON_LOADING_FILE
LABEL_FILE_LOADED:

	mov	dh, 1			    ; 显示序号为1的字符串
	call	DispStr			; "Ready."

; *****************************************************************************************************
	jmp	BaseOfLoader:OffsetOfLoader	 ; 这一句正式跳转到已加载到内存中的 LOADER.BIN 的开始处
						                       ; 开始执行 LOADER.BIN 的代码
						                       ; Boot Sector 的使命到此结束
; *****************************************************************************************************


;============================================================================
; 函数名: DispStr
; 作用:显示一个字符串,表示启动状态
;	函数开始时 dh 中应该是字符串序号(0-2),见上面字符串部分
;----------------------------------------------------------------------------
DispStr:
	mov	ax, MessageLength
	mul	dh                 ; 找到序号为dh的字符串，同时也作为起始行
	add	ax, BootMessage    ; BootMessage是基地址,加ax得到序号dh字符串基地址
	mov	bp, ax			       ; ┐
	mov	ax, ds			       ; ├ ES:BP = 串地址
	mov	es, ax			       ; ┘
	mov	cx, MessageLength	 ; CX = 串长度
	mov	ax, 01301h		     ; AH = 13,  AL = 01h
	mov	bx, 0007h		       ; 页号为0(BH = 0) 黑底白字(BL = 07h)
	mov	dl, 0              ; 起始列为0，起始行为dh，同时作为消息字符串号，巧妙吧！
	int	10h			           ; int 10h
;----------------------------------------------------------------------------
; int 10h 中断功能:
; 功能号：13H
; 功能：显示字符串
; 入口参数：ES:BP = 串地址
;           CX = 串长度
;           DH， DL = 起始行列
;           BH = 页号
;           AL = 0，BL = 属性
; 出口参数：无 
;----------------------------------------------------------------------------
	ret
;============================================================================

;============================================================================
; 函数名: ReadSector
;----------------------------------------------------------------------------
; 作用:
;	从第 ax 个 Sector 开始, 将 cl 个 Sector 读入 es:bx 中
ReadSector:
	; -----------------------------------------------------------------------
	; 怎样由扇区号求扇区在磁盘中的位置 (扇区号 -> 柱面号, 起始扇区, 磁头号)
	; -----------------------------------------------------------------------
	; 设扇区号为 x
	;                          ┌ 柱面号 = y >> 1
	;       x           ┌ 商 y ┤
	; -------------- => ┤      └ 磁头号 = y & 1
	;  每磁道扇区数     │
	;                   └ 余 z => 起始扇区号 = z + 1
	push	bp
	mov	bp, sp
	sub	esp, 2			          ; 辟出两个字节的堆栈区域保存要读的扇区数: byte [bp-2]

	mov	byte [bp-2], cl
	push	bx			            ; 保存 bx
	mov	bl, [BPB_SecPerTrk]	  ; bl: 除数
	div	bl			              ; y 在 al 中, z 在 ah 中
	inc	ah			              ; z ++
	mov	cl, ah			          ; cl <- 起始扇区号
	mov	dh, al			          ; dh <- y
	shr	al, 1			            ; y >> 1 (其实是 y/BPB_NumHeads, 这里BPB_NumHeads=2)
	mov	ch, al			          ; ch <- 柱面号
	and	dh, 1			            ; dh & 1 = 磁头号
	pop	bx			              ; 恢复 bx
	                          ; 至此, "柱面号, 起始扇区, 磁头号" 全部得到
	mov	dl, [BS_DrvNum]		    ; 驱动器号 (0 表示 A 盘)
.GoOnReading:
	mov	ah, 2			            ; 读
	mov	al, byte [bp-2]		    ; 读 al 个扇区
	int	13h                   ; int 13h中断读取数据
	jc	.GoOnReading		      ; 如果读取错误 CF 会被置为 1, 这时就不停地读, 直到正确为止

	add	esp, 2
	pop	bp

	ret
;============================================================================

;============================================================================
; 函数名: GetFATEntry
;----------------------------------------------------------------------------
; 作用:
;	找到序号为 ax 的 Sector 在 FAT 中的条目, 结果放在 ax 中
;	需要注意的是, 中间需要读 FAT 的扇区到 es:bx 处, 所以函数一开始保存了 es 和 bx
GetFATEntry:
	push	es
	push	bx
	push	ax                      ; 暂存簇号
	mov	ax, BaseOfLoader	        ; ┓
	sub	ax, 0100h		              ; ┣ 在 BaseOfLoader 后面留出 4K 空间用于存放 FAT
	mov	es, ax			              ; ┛
	pop	ax
	mov	byte [bOdd], 0
	mov	bx, 3
	mul	bx			                  ; dx:ax = ax * 3
	mov	bx, 2
	div	bx			                  ; dx:ax / 2  ==>  ax <- 商, dx <- 余数
	cmp	dx, 0
	jz	LABEL_EVEN
	mov	byte [bOdd], 1
LABEL_EVEN:                     ; 偶数
	xor	dx, dx			              ; 现在 ax 中是 FATEntry 在 FAT 中的偏移量. 
	                              ; 下面来计算 FATEntry 在哪个扇区中(FAT占用不止一个扇区)
	mov	bx, [BPB_BytsPerSec]
	div	bx			                  ; dx:ax / BPB_BytsPerSec  ==>	ax <- 商   (FATEntry 所在的扇区相对于 FAT 来说的扇区号)
					                      ;				                      dx <- 余数 (FATEntry 在扇区内的偏移)。
	push	dx
	mov	bx, 0			                ; bx <- 0	于是, es:bx = (BaseOfLoader - 100):00 = (BaseOfLoader - 100) * 10h
	add	ax, SectorNoOfFAT1	      ; 此句执行之后的 ax 就是 FATEntry 所在的扇区号
	mov	cl, 2
	call	ReadSector		          ; 读取 FATEntry 所在的扇区, 一次读两个, 避免在边界发生错误, 因为一个 FATEntry 可能跨越两个扇区
	pop	dx
	add	bx, dx
	mov	ax, [es:bx]
	cmp	byte [bOdd], 1
	jnz	LABEL_EVEN_2
	shr	ax, 4
LABEL_EVEN_2:
	and	ax, 0FFFh

LABEL_GET_FAT_ENRY_OK:

	pop	bx
	pop	es
	ret
;============================================================================

times 	510-($-$$)	db	0	; 填充剩下的空间，使生成的二进制代码恰好为512字节
dw 	0xaa55				; 结束标志
