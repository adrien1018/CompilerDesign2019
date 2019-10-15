#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "header.h"
#include "vector.h"

int main( int argc, char *argv[] )
{
    FILE *source, *target;
    Program program;
    SymbolTable symtab;

    if( argc == 3){
        source = fopen(argv[1], "r");
        target = fopen(argv[2], "w");
        if( !source ){
            printf("can't open the source file\n");
            exit(2);
        }
        else if( !target ){
            printf("can't open the target file\n");
            exit(2);
        }
        else{
            program = parser(source);
            fclose(source);
            symtab = build(program);
            check(&program, &symtab);
            optimize(&program);
            gencode(&program, target, &symtab);
        }
    }
    else{
        printf("Usage: %s source_file target_file\n", argv[0]);
    }


    return 0;
}


/********************************************* 
  Scanning 
 *********************************************/
Token getNumericToken( FILE *source, char c )
{
    Token token;
    int i = 0;

    while( isdigit(c) ) {
        token.tok[i++] = c;
        c = fgetc(source);
    }

    if( c != '.' ){
        ungetc(c, source);
        token.tok[i] = '\0';
        token.type = IntValue;
        return token;
    }

    token.tok[i++] = '.';

    c = fgetc(source);
    if( !isdigit(c) ){
        ungetc(c, source);
        printf("Expect a digit : %c\n", c);
        exit(1);
    }

    while( isdigit(c) ){
        token.tok[i++] = c;
        c = fgetc(source);
    }

    ungetc(c, source);
    token.tok[i] = '\0';
    token.type = FloatValue;
    return token;
}

Token scanner( FILE *source )
{
    char c = ' ';
    int i;
    Token token;

    while( isspace(c) ) c = fgetc(source);
    if (feof(source)) { // EOF
        token.tok[0] = '\0';
        token.type = EOFsymbol;
        return token;
    }

    // numeric
    if( isdigit(c) ) return getNumericToken(source, c);

    // string
    token.tok[0] = c;
    if (islower(c)) {
        for (i = 1; i < 65; i++) {
            token.tok[i] = c = fgetc(source);
            if (!islower(c)) {
                token.tok[i] = 0;
                ungetc(c, source);
                break;
            }
            if (i == 64) {
                token.tok[65] = '\0';
                printf("Token too long : %s\n", token.tok);
                exit(1);
            }
        }
        if (i == 1) {
            switch (token.tok[0]) {
                case 'f': token.type = FloatDeclaration;   break;
                case 'i': token.type = IntegerDeclaration; break;
                case 'p': token.type = PrintOp;            break;
                default:  token.type = VariableName;
            }
        } else {
            token.type = VariableName;
        }
    } else {
        token.tok[1] = '\0';
        switch(token.tok[0]){
            case '=': token.type = AssignmentOp; break;
            case '+': token.type = PlusOp;       break;
            case '-': token.type = MinusOp;      break;
            case '*': token.type = MulOp;        break;
            case '/': token.type = DivOp;        break;
            case EOF: token.type = EOFsymbol;    break;
            default: {
                printf("Invalid character : %c(%d)\n", token.tok[0], (int)token.tok[0]);
                exit(1);
            }
        }
    }
    return token;
}


/********************************************************
  Parsing
 *********************************************************/
Declaration parseDeclaration( FILE *source, Token token )
{
    Token token2;
    switch(token.type){
        case FloatDeclaration:
        case IntegerDeclaration:
            token2 = scanner(source);
            if (strcmp(token2.tok, "f") == 0 ||
                    strcmp(token2.tok, "i") == 0 ||
                    strcmp(token2.tok, "p") == 0) {
                printf("Syntax Error: %s cannot be used as id\n", token2.tok);
                exit(1);
            }
            return makeDeclarationNode( token, token2 );
        default:
            printf("Syntax Error: Expect Declaration %s\n", token.tok);
            exit(1);
    }
}

Declarations *parseDeclarations( FILE *source, Token *next_token )
{
    Token token = scanner(source);
    Declaration decl;
    Declarations *decls;
    switch(token.type){
        case FloatDeclaration:
        case IntegerDeclaration:
            decl = parseDeclaration(source, token);
            decls = parseDeclarations(source, next_token);
            return makeDeclarationTree( decl, decls );
        case PrintOp:
        case EOFsymbol:
        case VariableName:
            *next_token = token;
            return NULL;
        default:
            printf("Syntax Error: Expect declarations %s\n", token.tok);
            exit(1);
    }
}

Expression *tokenToExpression( const Token* token )
{
    Expression *value = (Expression *)malloc( sizeof(Expression) );
    value->leftOperand = value->rightOperand = NULL;

    switch(token->type){
        case VariableName:
            value->v.type = Identifier;
            strcpy(value->v.val.id, token->tok);
            break;
        case IntValue:
            value->v.type = IntConst;
            value->v.val.ivalue = atoi(token->tok);
            break;
        case FloatValue:
            value->v.type = FloatConst;
            value->v.val.fvalue = atof(token->tok);
            break;
        default:; // will not happen
    }

    return value;
}

DEFINE_VECTOR(Expression*, exp)
DEFINE_VECTOR(TokenType, token)

void popOperator(Vector_exp* exp_stk, Vector_token* opr_stk,
    const ValueType typetable[], const Operation oprtable[]) {
  Expression *expr;
  int type;
  if (Vector_exp_size(exp_stk) < 2) {
    printf("Syntax error: operand not enough\n");
    exit(1);
  }
  expr = (Expression*)malloc(sizeof(Expression));
  type = (int)*Vector_token_back(opr_stk);
  expr->v.type = typetable[type];
  expr->v.val.op = oprtable[type];
  expr->rightOperand = *Vector_exp_back(exp_stk);
  Vector_exp_pop(exp_stk);
  expr->leftOperand = *Vector_exp_back(exp_stk);
  *Vector_exp_back(exp_stk) = expr;
  Vector_token_pop(opr_stk);
}

Expression* finishExpression(Vector_exp* exp_stk, Vector_token* opr_stk,
    const ValueType typetable[], const Operation oprtable[]) {
  Expression* ret;
  while (Vector_token_size(opr_stk)) {
    popOperator(exp_stk, opr_stk, typetable, oprtable);
  }
  if (Vector_exp_size(exp_stk) != 1) {
    printf("Syntax error: too many operands\n");
    exit(1);
  }
  ret = *Vector_exp_back(exp_stk);
  Vector_exp_destroy(exp_stk);
  Vector_token_destroy(opr_stk);
  return ret;
}

Expression *parseExpression( FILE *source, Token* next_token ) {
  static int first = 1;
  static int precedence[12] = {0}; // 12 = # of TokenType
  static ValueType typetable[12];
  static Operation oprtable[12];
  Vector_exp exp_stk;
  Vector_token opr_stk;
  Token token;
  if (first) {
    precedence[(int)PlusOp] = 1;
    precedence[(int)MinusOp] = 1;
    precedence[(int)MulOp] = 2;
    precedence[(int)DivOp] = 2;
    typetable[(int)PlusOp] = PlusNode;
    typetable[(int)MinusOp] = MinusNode;
    typetable[(int)MulOp] = MulNode;
    typetable[(int)DivOp] = DivNode;
    oprtable[(int)PlusOp] = Plus;
    oprtable[(int)MinusOp] = Minus;
    oprtable[(int)MulOp] = Mul;
    oprtable[(int)DivOp] = Div;
    first = 0;
  }
  Vector_exp_init(&exp_stk);
  Vector_token_init(&opr_stk);
  while (1) {
    token = scanner(source);
    switch (token.type) {
      case VariableName:
      case IntValue:
      case FloatValue: {
        if (Vector_exp_size(&exp_stk) == Vector_token_size(&opr_stk) + 1) {
          *next_token = token;
          return finishExpression(&exp_stk, &opr_stk, typetable, oprtable);
        } else {
          Vector_exp_push(&exp_stk, tokenToExpression(&token));
        }
        break;
      }
      case PlusOp:
      case MinusOp:
      case MulOp:
      case DivOp: {
        while (Vector_token_size(&opr_stk) &&
               precedence[(int)*Vector_token_back(&opr_stk)] >=
               precedence[(int)token.type]) {
          popOperator(&exp_stk, &opr_stk, typetable, oprtable);
        }
        Vector_token_push(&opr_stk, token.type);
        break;
      }
      case PrintOp:
      case EOFsymbol: {
        *next_token = token;
        return finishExpression(&exp_stk, &opr_stk, typetable, oprtable);
      }
      default: {
        printf("Syntax Error: Expect a numeric value, an operator or an identifier %s\n",
            token.tok);
        exit(1);
      }
    }
  }
}

Statement parseStatement( FILE *source, Token token, Token* ret_token )
{
    Token next_token;
    Expression *expr;

    switch(token.type){
        case VariableName:
            next_token = scanner(source);
            if(next_token.type == AssignmentOp){
                expr = parseExpression(source, ret_token);
                return makeAssignmentNode(token.tok, expr);
            }
            else{
                printf("Syntax Error: Expect an assignment op %s\n", next_token.tok);
                exit(1);
            }
        case PrintOp:
            next_token = scanner(source);
            if(next_token.type == VariableName) {
                *ret_token = scanner(source);
                return makePrintNode(next_token.tok);
            }
            else{
                printf("Syntax Error: Expect an identifier %s\n", next_token.tok);
                exit(1);
            }
        default:
            printf("Syntax Error: Expect a statement %s\n", token.tok);
            exit(1);
    }
}

Statements *parseStatements( FILE * source, Token token )
{

    Statement stmt;
    Statements *stmts;

    switch(token.type){
        case VariableName:
        case PrintOp:
            stmt = parseStatement(source, token, &token);
            stmts = parseStatements(source, token);
            return makeStatementTree(stmt , stmts);
        case EOFsymbol:
            return NULL;
        default:
            printf("Syntax Error: Expect statements %s\n", token.tok);
            exit(1);
    }
}


/*********************************************************************
  Build AST
 **********************************************************************/
Declaration makeDeclarationNode( Token declare_type, Token identifier )
{
    Declaration tree_node;

    switch(declare_type.type){
        case FloatDeclaration:
            tree_node.type = Float;
            break;
        case IntegerDeclaration:
            tree_node.type = Int;
            break;
        default:
            break;
    }
    strcpy(tree_node.name, identifier.tok);

    return tree_node;
}

Declarations *makeDeclarationTree( Declaration decl, Declarations *decls )
{
    Declarations *new_tree = (Declarations *)malloc( sizeof(Declarations) );
    new_tree->first = decl;
    new_tree->rest = decls;

    return new_tree;
}


Statement makeAssignmentNode( const char* id, Expression *expr )
{
    Statement stmt;
    AssignmentStatement assign;

    stmt.type = Assignment;
    strcpy(assign.id, id);
    assign.expr = expr;
    stmt.stmt.assign = assign;

    return stmt;
}

Statement makePrintNode( const char* id )
{
    Statement stmt;
    stmt.type = Print;
    strcpy(stmt.stmt.variable, id);

    return stmt;
}

Statements *makeStatementTree( Statement stmt, Statements *stmts )
{
    Statements *new_tree = (Statements *)malloc( sizeof(Statements) );
    new_tree->first = stmt;
    new_tree->rest = stmts;

    return new_tree;
}

/* parser */
Program parser( FILE *source )
{
    Program program;
    Token next;

    program.declarations = parseDeclarations(source, &next);
    program.statements = parseStatements(source, next);

    return program;
}


/********************************************************
  Build symbol table
 *********************************************************/
void InitializeTable(SymbolTable *table) {
  table->size = 0;
}

int FindEntry(const SymbolTable* table, const char* t) {
  int i;
  for (i = 0; i < table->size; i++) {
    if (!strcmp(t, table->table[i].name)) return i;
  }
  return -1;
}

void add_table( SymbolTable *table, const char* c, DataType t )
{
    int index = FindEntry(table, c);
    if (index != -1) {
        printf("Error : id %s has been declared\n", c);//error
        //exit(1); //?
    }
    if (table->size == 26) {
        printf("Error : only 26 variables are allowed\n");
        exit(1);
    }
    strcpy(table->table[table->size].name, c);
    table->table[table->size].type = t;
    table->size++;
}

SymbolTable build( Program program )
{
    SymbolTable table;
    Declarations *decls = program.declarations;
    Declaration current;

    InitializeTable(&table);

    while(decls !=NULL){
        current = decls->first;
        add_table(&table, current.name, current.type);
        decls = decls->rest;
    }

    return table;
}


/********************************************************************
  Type checking
 *********************************************************************/

void convertType( Expression * old, DataType type )
{
    if(old->type == Float && type == Int){
        printf("error : can't convert float to integer\n");
        exit(1);
        //return;
    }
    if(old->type == Int && type == Float){
        Expression *tmp = (Expression *)malloc( sizeof(Expression) );
        if(old->v.type == Identifier)
            printf("convert to float %s \n",old->v.val.id);
        else
            printf("convert to float %d \n", old->v.val.ivalue);
        tmp->v = old->v;
        tmp->leftOperand = old->leftOperand;
        tmp->rightOperand = old->rightOperand;
        tmp->type = old->type;

        Value v;
        v.type = IntToFloatConvertNode;
        v.val.op = IntToFloatConvert;
        old->v = v;
        old->type = Int;
        old->leftOperand = tmp;
        old->rightOperand = NULL;
    }
}

DataType generalize( Expression *left, Expression *right )
{
    if(left->type == Float || right->type == Float){
        printf("generalize : float\n");
        return Float;
    }
    printf("generalize : int\n");
    return Int;
}

DataType lookup_table( SymbolTable *table, const char* c )
{
    int id = FindEntry(table, c);
    if (id == -1) {
        printf("Error : identifier %s is not declared\n", c);//error
        exit(1);
    }
    return table->table[id].type;
}

void checkexpression( Expression * expr, SymbolTable * table )
{
    const char* c;
    if(expr->leftOperand == NULL && expr->rightOperand == NULL){
        switch(expr->v.type){
            case Identifier:
                c = expr->v.val.id;
                printf("identifier : %s\n",c);
                expr->type = lookup_table(table, c);
                break;
            case IntConst:
                printf("constant : int\n");
                expr->type = Int;
                break;
            case FloatConst:
                printf("constant : float\n");
                expr->type = Float;
                break;
                //case PlusNode: case MinusNode: case MulNode: case DivNode:
            default:
                break;
        }
    }
    else{
        Expression *left = expr->leftOperand;
        Expression *right = expr->rightOperand;

        checkexpression(left, table);
        checkexpression(right, table);

        DataType type = generalize(left, right);
        convertType(left, type);//left->type = type;//converto
        convertType(right, type);//right->type = type;//converto
        expr->type = type;
    }
}

void checkstmt( Statement *stmt, SymbolTable * table )
{
    switch(stmt->type) {
      case Assignment: {
        AssignmentStatement assign = stmt->stmt.assign;
        printf("assignment : %s \n",assign.id);
        checkexpression(assign.expr, table);
        stmt->stmt.assign.type = lookup_table(table, assign.id);
        if (assign.expr->type == Float && stmt->stmt.assign.type == Int) {
            printf("error : can't convert float to integer\n");
        } else {
            convertType(assign.expr, stmt->stmt.assign.type);
        }
        break;
      }
      case Print: {
        printf("print : %s \n",stmt->stmt.variable);
        lookup_table(table, stmt->stmt.variable);
        break;
      }
    }
}

void check( Program *program, SymbolTable * table )
{
    Statements *stmts = program->statements;
    while(stmts != NULL){
        checkstmt(&stmts->first,table);
        stmts = stmts->rest;
    }
}

/***********************************************************************
  Optimization
 ************************************************************************/
void foldexpr(Expression* expr) {
  switch (expr->v.type) {
    case Identifier:
    case IntConst:
    case FloatConst: return;
    case PlusNode:
    case MinusNode:
    case MulNode:
    case DivNode:               foldexpr(expr->rightOperand); // fall through
    case IntToFloatConvertNode: foldexpr(expr->leftOperand);
  }
  print_expr(expr); puts("before");
  switch (expr->v.type) {
    case PlusNode:
    case MinusNode:
    case MulNode:
    case DivNode: {
      Value* lhs = &expr->leftOperand->v;
      Value* rhs = &expr->rightOperand->v;
      if (lhs->type != rhs->type) return;
      if (lhs->type == IntConst) {
        // folding divide by zero will change the DC program behavior, so not do it
        if (lhs->val.ivalue == 0) return;
        switch (expr->v.type) {
          // in fact, overflow will change the program behavior...
          // but there's no specification about AC's numerical range,
          // so let's ignore this issue
          case PlusNode:  expr->v.val.ivalue = lhs->val.ivalue + rhs->val.ivalue; puts("+"); break;
          case MinusNode: expr->v.val.ivalue = lhs->val.ivalue - rhs->val.ivalue; puts("-"); break;
          case MulNode:   expr->v.val.ivalue = lhs->val.ivalue * rhs->val.ivalue; puts("*"); break;
          case DivNode:   expr->v.val.ivalue = lhs->val.ivalue / rhs->val.ivalue; puts("/"); break;
          default: __builtin_unreachable();
        }
        expr->v.type = IntConst;
      } else if (lhs->type == FloatConst) {
        if (lhs->val.fvalue == 0) return;
        switch (expr->v.type) {
          // again, the precision issue...
          case PlusNode:  expr->v.val.fvalue = lhs->val.fvalue + rhs->val.fvalue; break;
          case MinusNode: expr->v.val.fvalue = lhs->val.fvalue - rhs->val.fvalue; break;
          case MulNode:   expr->v.val.fvalue = lhs->val.fvalue * rhs->val.fvalue; break;
          case DivNode:   expr->v.val.fvalue = lhs->val.fvalue / rhs->val.fvalue; break;
          default: __builtin_unreachable();
        }
        expr->v.type = FloatConst;
      }
      free(expr->leftOperand);
      free(expr->rightOperand);
      expr->leftOperand = expr->rightOperand = NULL;
      break;
    }
    case IntToFloatConvertNode: {
      Value* val = &expr->leftOperand->v;
      if (val->type == IntConst) {
        expr->v.type = FloatConst;
        expr->v.val.fvalue = (float)val->val.ivalue;
        free(expr->leftOperand);
        expr->leftOperand = NULL;
      }
      break;
    }
    default: __builtin_unreachable();
  }
  print_expr(expr); puts("after");
}

void optimize(const Program* prog)
{
    Statements *stmts = prog->statements;
    while(stmts != NULL){
        if (stmts->first.type == Assignment) {
            foldexpr(stmts->first.stmt.assign.expr);
        }
        stmts = stmts->rest;
    }
}

/***********************************************************************
  Code generation
 ************************************************************************/
void fprint_op( FILE *target, ValueType op )
{
    switch(op){
        case MinusNode:
            fprintf(target,"-\n");
            break;
        case PlusNode:
            fprintf(target,"+\n");
            break;
        case MulNode:
            fprintf(target,"*\n");
            break;
        case DivNode:
            fprintf(target,"/\n");
            break;
        default:
            fprintf(target,"Error in fprintf_op ValueType = %d\n",op);
            break;
    }
}

void fprint_expr( FILE *target, Expression *expr, const SymbolTable* table)
{

    if(expr->leftOperand == NULL){
        switch( (expr->v).type ){
            case Identifier:
                fprintf(target,"l%c\n",'a' + FindEntry(table, expr->v.val.id));
                break;
            case IntConst:
                fprintf(target,"%d\n",(expr->v).val.ivalue);
                break;
            case FloatConst:
                fprintf(target,"%f\n", (expr->v).val.fvalue);
                break;
            default:
                fprintf(target,"Error In fprint_left_expr. (expr->v).type=%d\n",(expr->v).type);
                break;
        }
    }
    else{
        fprint_expr(target, expr->leftOperand, table);
        if(expr->rightOperand == NULL){
            fprintf(target,"5k\n");
        }
        else{
            //	fprint_right_expr(expr->rightOperand);
            fprint_expr(target, expr->rightOperand, table);
            fprint_op(target, (expr->v).type);
        }
    }
}

void gencode(const Program* prog, FILE * target, const SymbolTable* table)
{
    Statements *stmts = prog->statements;
    Statement stmt;

    while(stmts != NULL){
        stmt = stmts->first;
        switch(stmt.type){
            case Print:
                fprintf(target,"l%c\n",'a' + FindEntry(table, stmt.stmt.variable));
                fprintf(target,"p\n");
                break;
            case Assignment:
                fprint_expr(target, stmt.stmt.assign.expr, table);
                print_expr(stmt.stmt.assign.expr);
                puts("");
                /*
                   if(stmt.stmt.assign.type == Int){
                   fprintf(target,"0 k\n");
                   }
                   else if(stmt.stmt.assign.type == Float){
                   fprintf(target,"5 k\n");
                   }*/
                fprintf(target,"s%c\n",'a' + FindEntry(table, stmt.stmt.assign.id));
                fprintf(target,"0 k\n");
                break;
        }
        stmts=stmts->rest;
    }

}


/***************************************
  For our debug,
  you can omit them.
 ****************************************/
void print_expr(Expression *expr)
{
    if(expr == NULL)
        return;
    else{
        print_expr(expr->leftOperand);
        switch((expr->v).type){
            case Identifier:
                printf("%s ", (expr->v).val.id);
                break;
            case IntConst:
                printf("%d ", (expr->v).val.ivalue);
                break;
            case FloatConst:
                printf("%f ", (expr->v).val.fvalue);
                break;
            case PlusNode:
                printf("+ ");
                break;
            case MinusNode:
                printf("- ");
                break;
            case MulNode:
                printf("* ");
                break;
            case DivNode:
                printf("/ ");
                break;
            case IntToFloatConvertNode:
                printf("(float) ");
                break;
            default:
                printf("error ");
                break;
        }
        print_expr(expr->rightOperand);
    }
}

void test_parser( FILE *source )
{
    Declarations *decls;
    Statements *stmts;
    Declaration decl;
    Statement stmt;
    Program program = parser(source);

    decls = program.declarations;

    while(decls != NULL){
        decl = decls->first;
        if(decl.type == Int)
            printf("i ");
        if(decl.type == Float)
            printf("f ");
        printf("%s ",decl.name);
        decls = decls->rest;
    }

    stmts = program.statements;

    while(stmts != NULL){
        stmt = stmts->first;
        if(stmt.type == Print){
            printf("p %s ", stmt.stmt.variable);
        }

        if(stmt.type == Assignment){
            printf("%s = ", stmt.stmt.assign.id);
            print_expr(stmt.stmt.assign.expr);
        }
        stmts = stmts->rest;
    }

}
