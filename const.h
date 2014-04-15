#ifndef CONST_H
#define CONST_H
enum {
    EXP_POSTFIX = 1024,
    POSTFIX_ARR, 
    POSTFIX_CALL,
    POSTFIX_DOT, 
    POSTFIX_PTR, 
    EXP_CAST,    
    INITR_NORM,  
    INITR_ARR,   
    DECLR_FUNC,  
    DECLR_ARR,   
    STMT_EXP,    
    STMT_COMP,   
    STMT_IF,     
    STMT_WHILE,  
    STMT_FOR,    
    STMT_CONT,   
    STMT_BREAK,  
    STMT_RET,
    NS_ID,
    NS_TAG
};

#define MAX_CHDN        1024
#define MAX_DEBUG_PRINT_BUFF 1024
#define MAX_DEBUG_PRINT_LVL  1024
#define MAX_TABLE_SIZE  1021
#define MAX_ERROR_BUFF  1024
#define INT_SIZE        4
#define CHAR_SIZE       1
#define FLAG_FUNC_CHK        (1 << 0)
#define FLAG_FUNC_DEF        (1 << 1)
#endif
