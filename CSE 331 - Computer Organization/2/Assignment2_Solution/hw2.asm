.globl main
.data
.eqv Max_Size 100    # Constant Max_Size = 100
arr: .word 0:100     # Array initiaized with 100 size, have all zero => "arr[100] = {0}" 
arraySize: .word 0   # Will be stored in s6 with AssignNumbers label.
num: .word 0         # Will be stored in s7 with AssignNumbers label.
returnVal: .word 0   # Will be stored in t8

#------------------------
# REGISTER MAPPINGS:   
# 		arraySize: $s6
#		num:       $s7
#		returnVal: $t8
#------------------------
# ====================================================================== #
# ======================= >   DEFINED MACROS   < ======================= #
# ====================================================================== #
.macro print_str (%str)
	.data
myLabel: .asciiz %str
	.text
	li $v0, 4
	la $a0, myLabel
	syscall
	.end_macro
# ====================================================================== #	
.macro print_int (%x)
	li $v0, 1
	add $a0, $zero, %x
	syscall
	.end_macro
# ====================================================================== #

######################################################################## ##
## ===================================================================== ##
.text                                                                    ##      
main:                                                                    ##      
	li $t0, 1 # i = 1// initially__for while loop                        ##                                                  
	li $t1, 0 # [index] of Arr__to iter in Arr index__ *4 byte           ##                                                               
	lw $t8, returnVal   # int returnVal;                                 ##                      
																		 ##                              	                                                                 
	jal TakeSize                                                         ##                 
	jal TakeNum                                                          ##                
	jal AssignNumbers                                                    ##                      
	jal While_Loop                                                       ##                   
## ===================================================================== ##	
######################################################################## ##
TakeSize:
	#print_str("Enter size: ")    # print str
	li $v0, 5        # cin >> num;
	syscall
	sw $v0, arraySize    # Assigned s6 as ArrSize
	jr $ra	
# ====================================================================== #
TakeNum:
	#print_str("Enter num: ")    # print str
	li $v0, 5        # cin >> arraySize;
	syscall
	sw $v0, num    # Assigned s7 as Num
	jr $ra	
# ====================================================================== #
AssignNumbers:
	lw $s6, arraySize    # arraySize =  Load arraySize from memory into a CPU register
    lw $s7, num          # Load num from memory into a CPU register
# ====================================================================== #
TakeArrElements:
	#print_str("Enter arr[i]: ")    # print str
	li $v0, 5        # cin >> arr[i];
	syscall
	move $s4, $v0
	# ===> After take the arr element, (store)write it to Arr.
	#__ I used sh instead sw: because sw eq 8 byte but arr eq 4, thus sh = sw/2byte = 4byte
	sw $s4, arr($t1) # store the taken num from user at v0 to Arr(index=$t1)
	addi $t1, $t1, 4  # +4 means next index of array 4 byte for word in MIPS
	jr $ra	
# ====================================================================== #
# ===> While Loop To Take Arr Elements
While_Loop:
	beq  $t0, $s6, Exit   # while(i != arraySize);
	addi $t0,$t0,1   # i++
	jal TakeArrElements
	j While_Loop
# ====================================================================== #
Exit:
	li	$v0, 10		# syscall_exit
	syscall
# ====================================================================== #
CheckReturnValue:                                                        #
	beq  $t8, $zero, _NotPos  # If FALSE goto _NotPos                    #
	bne  $t8, $zero, _Pos     # If TRUE goto _Pos                        #
	jr $ra	                                                             #
# ---------------------------------------------------------------------- #
_Pos:                                                                    #
	print_str("Possible!\n") # print_str() is a macro predefined         #
	jal Exit                                                             #
# ---------------------------------------------------------------------- #
_NotPos:    															 #
	print_str("Not Possible!\n") # print_str() is a macro predefined     #                                                            #
	jal Exit                                                             #
# ====================================================================== #







