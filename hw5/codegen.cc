#include "codegen.h"

#include <cassert>
#include <iostream>
#include <variant>

#include "ast.h"

namespace {

template <class T>
T &GetAttribute(AstNode *id, std::vector<TableEntry> &tab) {
  assert(id->node_type == IDENTIFIER_NODE);
  auto &value = std::get<IdentifierSemanticValue>(id->semantic_value);
  return tab[std::get<Identifier>(value.identifier).first].GetValue<T>();
}

inline IRInsr::Register Reg(size_t x) {
  return IRInsr::Register(x, true);
}

}  // namespace

inline void CodeGen::InitState(FunctionAttr& attr) {
  cur_stack_ = attr.sp_offset = 0;
  cur_register_ = attr.tot_pseudo_reg = attr.params.size();
  attr.label = labels_.size();
  labels_.emplace_back(ir_.size(), true);
}
inline size_t CodeGen::AllocStack(FunctionAttr& attr, size_t sz) {
  cur_stack_ += sz;
  if (cur_stack_ > attr.sp_offset) attr.sp_offset = cur_stack_;
  return cur_stack_;
}
inline size_t CodeGen::AllocRegister(FunctionAttr& attr) {
  if (cur_register_ == attr.tot_pseudo_reg) attr.tot_pseudo_reg++;
  return cur_register_++;
}

void CodeGen::VisitRelopExpr(AstNode* expr, FunctionAttr& attr, size_t dest) {
  size_t start_reg = cur_register_;
  switch (expr->node_type) {
    case CONVERSION_NODE: {
      size_t chval = AllocRegister(attr);
    }
    case EXPR_NODE: {
      auto& value = std::get<ExprSemanticValue>(expr->semantic_value);
      DataType child_type = expr->child.front()->data_type;
      if (value.kind == BINARY_OPERATION) {
        BinaryOperator op = std::get<BinaryOperator>(value.op);
        if (op == BINARY_OP_OR || op == BINARY_OP_AND) { // short-circuit
          // assuming both type are BOOL (int with only 0/1)
          VisitRelopExpr(expr->child.front(), attr, dest);
          ir_.emplace_back(op == BINARY_OP_OR ? INSR_BNE : INSR_BEQ,
                           IRInsr::kNoRD, dest, Reg(rv64::kZero),
                           IRInsr::kLabel, labels_.size());
          VisitRelopExpr(expr->child.front(), attr, dest);
          labels_.emplace_back(ir_.size());
          break;
        }
        if (child_type == INT_TYPE) { // TODO: BOOL_TYPE
          size_t chval = AllocRegister(attr);
          VisitRelopExpr(expr->child.front(), attr, dest);
          VisitRelopExpr(*std::next(expr->child.begin()), attr, chval);
          switch (op) {
            case BINARY_OP_OR:
            case BINARY_OP_AND: __builtin_unreachable();
            case BINARY_OP_LT:
              ir_.emplace_back(INSR_SLT, dest, dest, chval); break;
            case BINARY_OP_LE:
              ir_.emplace_back(INSR_SLT, dest, chval, dest);
              ir_.emplace_back(INSR_XORI, dest, dest, IRInsr::kConst, 1); break;
            case BINARY_OP_GT:
              ir_.emplace_back(INSR_SLT, dest, chval, dest); break;
            case BINARY_OP_GE:
              ir_.emplace_back(INSR_SLT, dest, dest, chval);
              ir_.emplace_back(INSR_XORI, dest, dest, IRInsr::kConst, 1); break;
            case BINARY_OP_EQ:
              ir_.emplace_back(INSR_XOR, dest, dest, chval);
              ir_.emplace_back(INSR_SLTIU, dest, dest, IRInsr::kConst, 1); break;
            case BINARY_OP_NE:
              ir_.emplace_back(INSR_XOR, dest, dest, chval);
              ir_.emplace_back(INSR_SLTU, dest, Reg(rv64::kZero), dest); break;
            case BINARY_OP_ADD:
              ir_.emplace_back(INSR_ADDW, dest, dest, chval); break;
            case BINARY_OP_SUB:
              ir_.emplace_back(INSR_SUBW, dest, dest, chval); break;
            case BINARY_OP_MUL:
              ir_.emplace_back(INSR_MULW, dest, dest, chval); break;
            case BINARY_OP_DIV:
              ir_.emplace_back(INSR_DIVW, dest, dest, chval); break;
          }
        } else if (child_type == FLOAT_TYPE) {
          size_t chval1 = AllocRegister(attr);
          size_t chval2 = expr->data_type == INT_TYPE ? AllocRegister(attr) : dest;
          VisitRelopExpr(expr->child.front(), attr, chval1);
          VisitRelopExpr(*std::next(expr->child.begin()), attr, chval2);
          switch (op) {
            case BINARY_OP_OR:
            case BINARY_OP_AND: __builtin_unreachable();
            case BINARY_OP_LT:
              ir_.emplace_back(INSR_FLT_S, dest, chval1, chval2); break;
            case BINARY_OP_LE:
              ir_.emplace_back(INSR_FLE_S, dest, chval1, chval2); break;
            case BINARY_OP_GT:
              ir_.emplace_back(INSR_FLE_S, dest, chval2, chval1); break;
            case BINARY_OP_GE:
              ir_.emplace_back(INSR_FLT_S, dest, chval2, chval1); break;
            case BINARY_OP_EQ:
              ir_.emplace_back(INSR_FEQ_S, dest, chval1, chval2); break;
            case BINARY_OP_NE:
              ir_.emplace_back(INSR_FEQ_S, dest, chval1, chval2);
              ir_.emplace_back(INSR_XORI, dest, dest, IRInsr::kConst, 1); break;
            case BINARY_OP_ADD:
              ir_.emplace_back(INSR_FADD_S, dest, chval1, chval2); break;
            case BINARY_OP_SUB:
              ir_.emplace_back(INSR_FSUB_S, dest, chval1, chval2); break;
            case BINARY_OP_MUL:
              ir_.emplace_back(INSR_FMUL_S, dest, chval1, chval2); break;
            case BINARY_OP_DIV:
              ir_.emplace_back(INSR_FDIV_S, dest, chval1, chval2); break;
          }
        } else {
          throw;
        }
      } else { // UNARY_OPERATION
        VisitRelopExpr(expr->child.front(), attr, dest);
        UnaryOperator op = std::get<UnaryOperator>(value.op);
        if (child_type == INT_TYPE) { // TODO: BOOL_TYPE
          switch (op) {
            case UNARY_OP_POSITIVE: break; // do nothing
            case UNARY_OP_NEGATIVE:
              ir_.emplace_back(INSR_SUBW, dest, Reg(rv64::kZero), dest); break;
            case UNARY_OP_LOGICAL_NEGATION:
              ir_.emplace_back(INSR_SLTIU, dest, dest, IRInsr::kConst, 1); break;
          }
        } else if (child_type == FLOAT_TYPE) {
          switch (op) {
            case UNARY_OP_POSITIVE: break; // do nothing
            case UNARY_OP_NEGATIVE:
              ir_.emplace_back(INSR_FSGNJN_S, dest, dest, dest); break;
            case UNARY_OP_LOGICAL_NEGATION: throw;
          }
        } else {
          throw;
        }
      }
      break;
    }
    case IDENTIFIER_NODE: {
      break;
    }
    case STMT_NODE: {
      break;
    }
  }
  cur_register_ = start_reg;
}

void CodeGen::VisitStatement(AstNode *stmt, FunctionAttr& attr) {
  if (stmt->node_type == BLOCK_NODE) return VisitBlock(stmt, attr);
  if (stmt->node_type != STMT_NODE) return;
  auto &value = std::get<StmtSemanticValue>(stmt->semantic_value);
  switch (value.kind) {
    case WHILE_STMT:
    case FOR_STMT:
    case IF_STMT:
      VisitStatement(*std::prev(stmt->child.end()), attr);
      break;
    case IF_ELSE_STMT:
      VisitStatement(*std::prev(stmt->child.end(), 2), attr);
      VisitStatement(*std::prev(stmt->child.end()), attr);
      break;
  }
}

void CodeGen::VisitStmtList(AstNode *stmt_list, FunctionAttr& attr) {
  for (AstNode *stmt : stmt_list->child) VisitStatement(stmt, attr);
}

void CodeGen::VisitVariableDecl(AstNode *decl, FunctionAttr& attr) {
  for (auto it = std::next(decl->child.begin()); it != decl->child.end();
       it++) {
    VariableAttr& var_attr = GetAttribute<VariableAttr>(*it, tab_);
    if (var_attr.IsArray()) {
      var_attr.offset = AllocStack(attr, var_attr.size);
    } else {
      auto& value = std::get<IdentifierSemanticValue>((*it)->semantic_value);
      var_attr.offset = AllocRegister(attr);
      if (value.kind == WITH_INIT_ID) {
        AstNode* init_val = (*it)->child.front();
        VisitRelopExpr(init_val, attr, var_attr.offset);
      }
    }
  }
}

void CodeGen::VisitDeclList(AstNode *decl_list, FunctionAttr& attr) {
  // TODO: Remove type-decls after parsing
  for (AstNode *decl : decl_list->child) {
    DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
    if (kind == VARIABLE_DECL) VisitVariableDecl(decl, attr);
  }
}

void CodeGen::VisitBlock(AstNode *block, FunctionAttr& attr) {
  size_t stack_tmp = cur_stack_;
  for (AstNode *nd : block->child) {
    switch (nd->node_type) {
      case STMT_LIST_NODE:
        VisitStmtList(nd, attr);
        break;
      case VARIABLE_DECL_LIST_NODE:
        VisitDeclList(nd, attr);
        break;
    }
  }
  cur_stack_ = stack_tmp;
}

void CodeGen::VisitFunctionDecl(AstNode *decl) {
  AstNode *id = *std::next(decl->child.begin());
  FunctionAttr &attr = GetAttribute<FunctionAttr>(id, tab_);
  AstNode *block = *std::prev(decl->child.end());
  InitState(attr);
  for (size_t i = 0, ival = 0, fval = 0, stk = 0; i < attr.params.size(); i++) {
    VariableAttr& param = tab_[attr.params[i]].GetValue<VariableAttr>();
    if (param.IsArray() || param.data_type == INT_TYPE) {
      if (ival >= 8) {
        ir_.emplace_back(param.IsArray() ? INSR_LD : INSR_LW,
                         i, Reg(rv64::kSp), stk++ * 8);
      } else {
        ir_.emplace_back(PINSR_MV, i, Reg(rv64::kA0 + ival));
      }
      ival++;
    } else {
      if (fval >= 8) {
        ir_.emplace_back(INSR_FLW, i, Reg(rv64::kSp), stk++ * 8);
      } else {
        ir_.emplace_back(PINSR_FMV_S, i, Reg(rv64::kFa0 + ival));
      }
      fval++;
    }
    param.local = true;
    param.is_param = true;
    param.offset = i;
  }
  ir_.emplace_back(PINSR_PUSH_SP);
  VisitBlock(block, attr);
  std::cerr << "sp_offset = " << attr.sp_offset << '\n';
}

void CodeGen::VisitGlobalDecl(AstNode *decl) {
  if (decl->node_type == VARIABLE_DECL_LIST_NODE) {
    for (AstNode *child : decl->child) VisitGlobalDecl(child);
    return;
  }
  DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
  if (kind == FUNCTION_DECL) VisitFunctionDecl(decl);
}

void CodeGen::VisitProgram(AstNode *prog) {
  for (AstNode *decl : prog->child) VisitGlobalDecl(decl);
}

void CodeGen::GenerateVariableDecl(AstNode *var_decl, bool global) {
  if (global) {
    if (section_ != DATA_SECTION) {
      // TODO: generate `.data` marker
      ofs_ << ".data\n";
    }
    for (auto it = std::next(var_decl->child.begin());
         it != var_decl->child.end(); ++it) {
      AstNode *nd = *it;
      assert(nd->node_type == IDENTIFIER_NODE);
      auto &value = std::get<IdentifierSemanticValue>(nd->semantic_value);
      const TableEntry &entry =
          tab_[std::get<Identifier>(value.identifier).first];
      const VariableAttr &attr = entry.GetValue<VariableAttr>();
      if (attr.IsArray()) {
        // size_t sz = attr.GetSize();
        // TODO: generate `<id>: .space 4 * sz`
      } else {
        // TODO: generate `<id>: .word`
      }
    }
  } else {
    // TODO: declaring local variables
  }
}

void CodeGen::GenerateFunctionDecl(AstNode *func_decl) {
  if (section_ != TEXT_SECTION) {
    // TODO: generate `.text` marker
    ofs_ << ".text\n";
  }
}

void CodeGen::GenerateGlobalDecl(AstNode *decl) {
  if (decl->node_type == VARIABLE_DECL_LIST_NODE) {
    for (AstNode *child : decl->child) GenerateGlobalDecl(child);
    return;
  }
  DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
  switch (kind) {
    case VARIABLE_DECL:
      GenerateVariableDecl(decl, true);
      break;
    case FUNCTION_DECL:
      GenerateFunctionDecl(decl);
      break;
  }
}

void CodeGen::GenerateProgram(AstNode *prog) {
  for (AstNode *decl : prog->child) GenerateGlobalDecl(decl);
}
