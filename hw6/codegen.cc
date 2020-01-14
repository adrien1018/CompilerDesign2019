#include "codegen.h"

#include <cassert>
#include <cstring>
#include <queue>

#include "analysis.h"
#include "ast.h"
#include "utils.h"

namespace {

const size_t kNoDest = -(size_t)1;

template <class T>
T& GetAttribute(AstNode* id, std::vector<TableEntry>& tab) {
  assert(id->node_type == IDENTIFIER_NODE);
  auto& value = std::get<IdentifierSemanticValue>(id->semantic_value);
  return tab[std::get<Identifier>(value.identifier).first].GetValue<T>();
}

inline uint32_t GetConst(AstNode* expr, DataType type = UNKNOWN_TYPE) {
  auto& value = std::get<ConstValue>(expr->semantic_value);
  uint32_t x = 0;
  if (expr->data_type == INT_TYPE || expr->data_type == BOOLEAN_TYPE) {
    Debug_("GetConst (int)", std::get<int32_t>(value), "\n");
    if (type != FLOAT_TYPE) {
      memcpy(&x, &std::get<int32_t>(value), 4);
    } else {
      FloatType f = std::get<int32_t>(value);
      memcpy(&x, &f, 4);
    }
  } else if (expr->data_type == FLOAT_TYPE) {
    Debug_("GetConst (float)", std::get<FloatType>(value), ", size ",
           sizeof(FloatType), "\n");
    if (type == FLOAT_TYPE || type == UNKNOWN_TYPE) {
      memcpy(&x, &std::get<FloatType>(value), 4);
    } else {
      int32_t i = std::get<FloatType>(value);
      memcpy(&x, &i, 4);
    }
  } else {
    assert(false);
  }
  Debug_("GetConst (hex)", x, "\n");
  return x;
}

inline IRInsr::Register Reg(size_t x) { return IRInsr::Register(x, true); }

}  // namespace

inline size_t CodeGen::InsertLabel(bool func) {
  size_t ret = labels_.size();
  labels_.emplace_back(ir_.size(), func);
  return ret;
}
inline void CodeGen::InitState(FunctionAttr& attr) {
  max_.stk = cur_.stk = 0;
  max_.reg = cur_.reg = {0, 0};
  attr.label = labels_.size();
  InsertLabel(true);
}
inline size_t CodeGen::AllocStack(size_t sz) {
  cur_.stk += sz;
  if (cur_.stk > max_.stk) max_.stk = cur_.stk;
  return cur_.stk - sz;
}
inline size_t CodeGen::AllocRegister(DataType type = INT_TYPE) {
  if (type == FLOAT_TYPE) {
    if (cur_.reg.freg == max_.reg.freg) max_.reg.freg++;
    return cur_.reg.freg++;
  } else {
    if (cur_.reg.ireg == max_.reg.ireg) max_.reg.ireg++;
    return cur_.reg.ireg++;
  }
}

void CodeGen::VisitConversion(AstNode* expr, size_t dest) {
  auto& value = std::get<ConversionSemanticValue>(expr->semantic_value);
  size_t chval = value.to == FLOAT_TYPE || value.from == FLOAT_TYPE
                     ? AllocRegister(value.from)
                     : dest;
  Debug_("Conversion ", value.from, "->", value.to, "\n");
  VisitRelopExpr(expr->child.front(), chval);
  switch (value.from) {
    case FLOAT_TYPE: {
      if (value.to == INT_TYPE) {
        ir_.emplace_back(INSR_FCVT_W_S, dest, chval, IRInsr::kRoundingMode,
                         rv64::kRTZ);
        ir_.emplace_back(INSR_ADDIW, dest, dest, IRInsr::kConst, 0);
      } else if (value.to == BOOLEAN_TYPE) {
        size_t tmp = AllocRegister(FLOAT_TYPE);
        ir_.emplace_back(INSR_FMV_W_X, tmp, Reg(rv64::kZero));
        ir_.emplace_back(INSR_FEQ_S, dest, chval, tmp);
        ir_.emplace_back(INSR_XORI, dest, dest, IRInsr::kConst, 1);
      }
      break;
    }
    case INT_TYPE:
    case BOOLEAN_TYPE: {
      if (value.to == BOOLEAN_TYPE) {
        if (value.from == INT_TYPE) {
          ir_.emplace_back(INSR_SLTU, dest, Reg(rv64::kZero), chval);
        }
      } else if (value.to == FLOAT_TYPE) {
        ir_.emplace_back(INSR_FCVT_S_W, dest, chval);
      }
      break;
    }
  }
}

void CodeGen::VisitOpr(AstNode* expr, size_t dest) {
  auto& value = std::get<ExprSemanticValue>(expr->semantic_value);
  DataType child_type = expr->child.front()->data_type;
  if (value.kind == BINARY_OPERATION) {
    BinaryOperator op = std::get<BinaryOperator>(value.op);
    if (op == BINARY_OP_OR || op == BINARY_OP_AND) {  // short-circuit
      assert(child_type == BOOLEAN_TYPE);
      assert((*std::next(expr->child.begin()))->data_type == BOOLEAN_TYPE);
      VisitRelopExpr(expr->child.front(), dest);
      size_t now_label = ir_.size();
      ir_.emplace_back(op == BINARY_OP_OR ? INSR_BNE : INSR_BEQ, IRInsr::kNoRD,
                       dest, Reg(rv64::kZero), IRInsr::kLabel, 0);
      VisitRelopExpr(*std::next(expr->child.begin()), dest);
      ir_[now_label].imm = InsertLabel();
      return;
    }
    Debug_("Child type: ", (int)child_type, "\n");
    if (child_type == INT_TYPE || child_type == BOOLEAN_TYPE) {
      size_t chval = AllocRegister(INT_TYPE);
      VisitRelopExpr(expr->child.front(), dest);
      VisitRelopExpr(*std::next(expr->child.begin()), chval);
      switch (op) {
        case BINARY_OP_OR:
        case BINARY_OP_AND:
          __builtin_unreachable();
        case BINARY_OP_LT:
          ir_.emplace_back(INSR_SLT, dest, dest, chval);
          break;
        case BINARY_OP_LE:
          ir_.emplace_back(INSR_SLT, dest, chval, dest);
          ir_.emplace_back(INSR_XORI, dest, dest, IRInsr::kConst, 1);
          break;
        case BINARY_OP_GT:
          ir_.emplace_back(INSR_SLT, dest, chval, dest);
          break;
        case BINARY_OP_GE:
          ir_.emplace_back(INSR_SLT, dest, dest, chval);
          ir_.emplace_back(INSR_XORI, dest, dest, IRInsr::kConst, 1);
          break;
        case BINARY_OP_EQ:
          ir_.emplace_back(INSR_XOR, dest, dest, chval);
          ir_.emplace_back(INSR_SLTIU, dest, dest, IRInsr::kConst, 1);
          break;
        case BINARY_OP_NE:
          ir_.emplace_back(INSR_XOR, dest, dest, chval);
          ir_.emplace_back(INSR_SLTU, dest, Reg(rv64::kZero), dest);
          break;
        case BINARY_OP_ADD:
          ir_.emplace_back(INSR_ADDW, dest, dest, chval);
          break;
        case BINARY_OP_SUB:
          ir_.emplace_back(INSR_SUBW, dest, dest, chval);
          break;
        case BINARY_OP_MUL:
          ir_.emplace_back(INSR_MULW, dest, dest, chval);
          break;
        case BINARY_OP_DIV:
          ir_.emplace_back(INSR_DIVW, dest, dest, chval);
          break;
      }
    } else if (child_type == FLOAT_TYPE) {
      size_t chval1 = AllocRegister(FLOAT_TYPE);
      size_t chval2 =
          expr->data_type != FLOAT_TYPE ? AllocRegister(FLOAT_TYPE) : dest;
      VisitRelopExpr(expr->child.front(), chval1);
      VisitRelopExpr(*std::next(expr->child.begin()), chval2);
      switch (op) {
        case BINARY_OP_OR:
        case BINARY_OP_AND:
          __builtin_unreachable();
        case BINARY_OP_LT:
          ir_.emplace_back(INSR_FLT_S, dest, chval1, chval2);
          break;
        case BINARY_OP_LE:
          ir_.emplace_back(INSR_FLE_S, dest, chval1, chval2);
          break;
        case BINARY_OP_GT:
          ir_.emplace_back(INSR_FLT_S, dest, chval2, chval1);
          break;
        case BINARY_OP_GE:
          ir_.emplace_back(INSR_FLE_S, dest, chval2, chval1);
          break;
        case BINARY_OP_EQ:
          ir_.emplace_back(INSR_FEQ_S, dest, chval1, chval2);
          break;
        case BINARY_OP_NE:
          ir_.emplace_back(INSR_FEQ_S, dest, chval1, chval2);
          ir_.emplace_back(INSR_XORI, dest, dest, IRInsr::kConst, 1);
          break;
        case BINARY_OP_ADD:
          ir_.emplace_back(INSR_FADD_S, dest, chval1, chval2);
          break;
        case BINARY_OP_SUB:
          ir_.emplace_back(INSR_FSUB_S, dest, chval1, chval2);
          break;
        case BINARY_OP_MUL:
          ir_.emplace_back(INSR_FMUL_S, dest, chval1, chval2);
          break;
        case BINARY_OP_DIV:
          ir_.emplace_back(INSR_FDIV_S, dest, chval1, chval2);
          break;
      }
    } else {
      assert(false);
    }
  } else {  // UNARY_OPERATION
    VisitRelopExpr(expr->child.front(), dest);
    UnaryOperator op = std::get<UnaryOperator>(value.op);
    if (child_type == INT_TYPE || child_type == BOOLEAN_TYPE) {
      switch (op) {
        case UNARY_OP_POSITIVE:
          break;  // do nothing
        case UNARY_OP_NEGATIVE:
          ir_.emplace_back(INSR_SUBW, dest, Reg(rv64::kZero), dest);
          break;
        case UNARY_OP_LOGICAL_NEGATION:
          ir_.emplace_back(INSR_SLTIU, dest, dest, IRInsr::kConst, 1);
          break;
      }
    } else if (child_type == FLOAT_TYPE) {
      switch (op) {
        case UNARY_OP_POSITIVE:
          break;  // do nothing
        case UNARY_OP_NEGATIVE:
          ir_.emplace_back(INSR_FSGNJN_S, dest, dest, dest);
          break;
        case UNARY_OP_LOGICAL_NEGATION:
          assert(false);
      }
    } else {
      assert(false);
    }
  }
}

void CodeGen::LoadConst(uint64_t x, size_t dest) {
  Debug_("LoadConst ", x, "\n");
  if ((x >> 32) == 0) {  // 32-bit values
    if (x >= 0xfffff800 || x < 0x800) {
      ir_.emplace_back(INSR_ADDIW, dest, Reg(rv64::kZero), IRInsr::kConst,
                       (int32_t)x);
    } else {
      uint32_t tx = (x & 0xfffff800) + (x & 0x800);
      ir_.emplace_back(INSR_LUI, dest, IRInsr::kConst, tx >> 12);
      if (x != tx) {
        ir_.emplace_back(INSR_ADDIW, dest, dest, IRInsr::kConst,
                         (int32_t)((uint32_t)x - tx));
      }
    }
  } else {
    std::vector<uint8_t> res;
    for (int i = 0; i < 8; i++) res.push_back(x >> (i * 8) & 255);
    ir_.emplace_back(INSR_LD, dest, IRInsr::kData, data_.size());
    data_.emplace_back(std::move(res));
  }
}

void CodeGen::VisitConst(AstNode* expr, size_t dest) {
  auto& value = std::get<ConstValue>(expr->semantic_value);
  if (expr->data_type == CONST_STRING_TYPE) {
    ir_.emplace_back(PINSR_LA, dest, IRInsr::kData, data_.size());
    data_.emplace_back(std::get<std::string>(value));
  } else if (expr->data_type == INT_TYPE || expr->data_type == FLOAT_TYPE ||
             expr->data_type == BOOLEAN_TYPE) {
    size_t chval = expr->data_type != FLOAT_TYPE ? dest : AllocRegister();
    LoadConst(GetConst(expr), chval);
    if (expr->data_type == FLOAT_TYPE) {
      ir_.emplace_back(INSR_FMV_W_X, dest, chval);
    }
  } else {
    assert(false);
  }
}

// Load address to dest; if is slice, return true
bool CodeGen::VisitArray(AstNode* expr, size_t dest) {
  Debug_("VisitArray");
  FuncSpace start_space = cur_;
  auto& value = std::get<IdentifierSemanticValue>(expr->semantic_value);
  const TableEntry& entry = tab_[std::get<Identifier>(value.identifier).first];
  const VariableAttr& var_attr = entry.GetValue<VariableAttr>();
  size_t reg = AllocRegister(INT_TYPE), i = 0;
  if (expr->child.size()) {
    for (AstNode* dim : expr->child) {
      if (i != 0) {
        VisitRelopExpr(dim, reg);
        ir_.emplace_back(INSR_ADD, dest, dest, reg);
      } else {
        VisitRelopExpr(dim, dest);
      }
      if (++i == var_attr.dims.size()) {
        ir_.emplace_back(INSR_SLLI, dest, dest, IRInsr::kConst, 2);
      } else {
        LoadConst(var_attr.dims[i], reg);
        ir_.emplace_back(INSR_MUL, dest, dest, reg);
      }
    }
    if (i < var_attr.dims.size()) {
      uint64_t x = 4;
      for (i++; i < var_attr.dims.size(); i++) x *= var_attr.dims[i];
      LoadConst(x, reg);
      ir_.emplace_back(INSR_MUL, dest, dest, reg);
    }
  } else {
    ir_.emplace_back(PINSR_MV, dest, Reg(rv64::kZero));
  }
  if (var_attr.is_param) {
    ir_.emplace_back(INSR_ADD, dest, dest, var_attr.offset);
  } else if (var_attr.local) {
    ir_.emplace_back(INSR_ADD, dest, dest, Reg(rv64::kSp));
    ir_.emplace_back(INSR_ADDI, dest, dest, IRInsr::kConst, var_attr.offset);
  } else {
    ir_.emplace_back(PINSR_LA, reg, IRInsr::kData, var_attr.offset);
    ir_.emplace_back(INSR_ADD, dest, dest, reg);
  }
  if (!opt_.register_alloc) cur_ = start_space;
  return expr->child.size() != var_attr.dims.size();
}

void CodeGen::VisitIdentifier(AstNode* expr, size_t dest) {
  auto& value = std::get<IdentifierSemanticValue>(expr->semantic_value);
  const TableEntry& entry = tab_[std::get<Identifier>(value.identifier).first];
  const VariableAttr& var_attr = entry.GetValue<VariableAttr>();
  assert(var_attr.data_type == FLOAT_TYPE || var_attr.data_type == INT_TYPE);
  if (var_attr.IsArray()) {
    FuncSpace start_space = cur_;
    size_t reg = AllocRegister(INT_TYPE);
    if (!VisitArray(expr, reg)) {
      // not slice, load its content
      if (var_attr.data_type == FLOAT_TYPE) {
        ir_.emplace_back(INSR_FLW, dest, reg, IRInsr::kConst, 0);
      } else {
        ir_.emplace_back(INSR_LW, dest, reg, IRInsr::kConst, 0);
      }
    } else {
      ir_.emplace_back(PINSR_MV, dest, reg);
    }
    if (!opt_.register_alloc) cur_ = start_space;
  } else {
    if (var_attr.local) {
      if (var_attr.data_type == FLOAT_TYPE) {
        ir_.emplace_back(PINSR_FMV_S, dest, var_attr.offset);
      } else {
        ir_.emplace_back(PINSR_MV, dest, var_attr.offset);
      }
    } else {
      if (var_attr.data_type == FLOAT_TYPE) {
        ir_.emplace_back(INSR_FLW, dest, IRInsr::kData, var_attr.offset);
      } else {
        ir_.emplace_back(INSR_LW, dest, IRInsr::kData, var_attr.offset);
      }
    }
  }
}

void CodeGen::VisitFunctionCall(AstNode* expr, size_t dest = kNoDest) {
  AstNode* id_node = *expr->child.begin();
  auto& value = std::get<IdentifierSemanticValue>(id_node->semantic_value);
  size_t id = std::get<Identifier>(value.identifier).first;
  AstNode* params = *std::next(expr->child.begin());
  const FunctionAttr* func_attr;
  size_t func_label;
  DataType return_type;
  Debug_("Function call ", id, '\n');
  if (id > ~kBuiltinFunctionNum) {  // built-in function
    func_attr = nullptr;
    func_label = id;
    return_type = kBuiltinFunction[~id].return_type;
  } else {
    func_attr = &tab_[id].GetValue<FunctionAttr>();
    func_label = func_attr->label;
    return_type = func_attr->return_type;
  }
  size_t ival = 0, fval = 0, i = 0;
  std::vector<size_t> stk_store;
  for (auto it = params->child.begin(); it != params->child.end(); ++it, i++) {
    FuncSpace start_space = cur_;
    DataType type;
    if (func_attr) {
      VariableAttr& fattr = tab_[func_attr->params[i]].GetValue<VariableAttr>();
      type = fattr.IsArray() ? INT_TYPE : fattr.data_type;
    } else {
      type = (*it)->data_type;
    }
    if (type == CONST_STRING_TYPE || type == INT_TYPE || type == INT_PTR_TYPE ||
        type == FLOAT_PTR_TYPE || type == BOOLEAN_TYPE) {
      size_t reg = return_type == INT_TYPE ? dest : AllocRegister(INT_TYPE);
      VisitRelopExpr(*it, reg);
      if (ival >= 8) {
        stk_store.push_back(ir_.size());
        ir_.emplace_back(type == INT_TYPE ? INSR_SW : INSR_SD, IRInsr::kNoRD,
                         Reg(rv64::kSp), reg, IRInsr::kConst, 0);
      } else {
        ir_.emplace_back(PINSR_MV, Reg(rv64::kA0 + ival), reg);
      }
      ival++;
    } else if (type == FLOAT_TYPE) {
      size_t reg = return_type == FLOAT_TYPE ? dest : AllocRegister(FLOAT_TYPE);
      VisitRelopExpr(*it, reg);
      if (fval >= 8) {
        stk_store.push_back(ir_.size());
        ir_.emplace_back(INSR_FSW, IRInsr::kNoRD, Reg(rv64::kSp), reg,
                         IRInsr::kConst, 0);
      } else {
        ir_.emplace_back(PINSR_FMV_S, Reg(rv64::kFa0 + fval), reg);
      }
      fval++;
    } else {
      assert(false);
    }
    if (!opt_.register_alloc) cur_ = start_space;
  }
  size_t stk = (stk_store.size() + 1) / 2 * 2;  // 16-byte align
  for (size_t i = 0; i < stk_store.size(); i++) {
    ir_[stk_store[i]].imm = (stk - i) * -8;
  }
  if (stk) {
    ir_.emplace_back(INSR_ADDI, Reg(rv64::kSp), Reg(rv64::kSp), IRInsr::kConst,
                     (int64_t)stk * -8);
  }
  ir_.emplace_back(PINSR_CALL, IRInsr::kLabel, func_label);
  if (dest != kNoDest) {
    switch (return_type) {
      case INT_TYPE:
        ir_.emplace_back(PINSR_MV, dest, Reg(rv64::kA0));
        break;
      case FLOAT_TYPE:
        ir_.emplace_back(PINSR_FMV_S, dest, Reg(rv64::kFa0));
        break;
      default:
        assert(false);
    }
  }
  if (stk) {
    ir_.emplace_back(INSR_ADDI, Reg(rv64::kSp), Reg(rv64::kSp), IRInsr::kConst,
                     stk * 8);
  }
}

void CodeGen::VisitRelopExpr(AstNode* expr, size_t dest) {
  if (dest == kNoDest && expr->node_type != STMT_NODE) return;
  FuncSpace start_space = cur_;
  switch (expr->node_type) {
    case CONVERSION_NODE:
      VisitConversion(expr, dest);
      break;
    case EXPR_NODE:
      VisitOpr(expr, dest);
      break;
    case CONST_VALUE_NODE:
      VisitConst(expr, dest);
      break;
    case IDENTIFIER_NODE:
      VisitIdentifier(expr, dest);
      break;
    case STMT_NODE:
      VisitFunctionCall(expr, dest);
      break;
    default:
      assert(false);
  }
  if (!opt_.register_alloc) cur_ = start_space;
}

void CodeGen::VisitRelopExprList(AstNode* expr_list, size_t dest) {
  size_t i = 0;
  for (AstNode* expr : expr_list->child) {
    VisitRelopExpr(expr, ++i == expr_list->child.size() ? dest : kNoDest);
  }
}

void CodeGen::VisitAssignment(AstNode* expr) {
  Debug_("VisitAssignment\n");
  FuncSpace start_space = cur_;
  AstNode* var = expr->child.front();
  auto& value = std::get<IdentifierSemanticValue>(var->semantic_value);
  const TableEntry& entry = tab_[std::get<Identifier>(value.identifier).first];
  const VariableAttr& var_attr = entry.GetValue<VariableAttr>();
  assert(var_attr.data_type == FLOAT_TYPE || var_attr.data_type == INT_TYPE);
  size_t valreg = AllocRegister(var_attr.data_type);
  VisitRelopExpr(*std::next(expr->child.begin()), valreg);
  if (var_attr.IsArray()) {
    size_t reg = AllocRegister(INT_TYPE);
    if (VisitArray(expr->child.front(), reg)) assert(false);
    if (var_attr.data_type == FLOAT_TYPE) {
      ir_.emplace_back(INSR_FSW, IRInsr::kNoRD, reg, valreg, IRInsr::kConst, 0);
    } else {
      ir_.emplace_back(INSR_SW, IRInsr::kNoRD, reg, valreg, IRInsr::kConst, 0);
    }
  } else {
    if (var_attr.local) {
      if (var_attr.data_type == FLOAT_TYPE) {
        ir_.emplace_back(PINSR_FMV_S, var_attr.offset, valreg);
      } else {
        ir_.emplace_back(PINSR_MV, var_attr.offset, valreg);
      }
    } else {
      if (var_attr.data_type == FLOAT_TYPE) {
        ir_.emplace_back(INSR_FSW, IRInsr::kNoRD, 0, valreg, IRInsr::kData,
                         var_attr.offset);
      } else {
        ir_.emplace_back(INSR_SW, IRInsr::kNoRD, 0, valreg, IRInsr::kData,
                         var_attr.offset);
      }
    }
  }
  if (!opt_.register_alloc) cur_ = start_space;
}

void CodeGen::VisitAssignmentList(AstNode* stmt_list) {
  Debug_((int)stmt_list->node_type, "\n");
  assert(stmt_list->node_type == ASSIGN_EXPR_LIST_NODE);
  for (AstNode* stmt : stmt_list->child) VisitAssignment(stmt);
}

void CodeGen::VisitStatement(AstNode* stmt, FunctionAttr& attr) {
  if (stmt->node_type == BLOCK_NODE) return VisitBlock(stmt, attr);
  if (stmt->node_type != STMT_NODE) return;
  auto& value = std::get<StmtSemanticValue>(stmt->semantic_value);
  auto it = stmt->child.begin();
  switch (value.kind) {
    case WHILE_STMT: {
      FuncSpace start_space = cur_;
      size_t reg = AllocRegister(INT_TYPE);
      size_t jump_label = InsertLabel();
      VisitRelopExprList(*it, reg);  // while expr
      size_t now_label = ir_.size();
      ir_.emplace_back(INSR_BEQ, IRInsr::kNoRD, reg, Reg(rv64::kZero),
                       IRInsr::kLabel, 0);
      if (!opt_.register_alloc) cur_ = start_space;
      VisitStatement(*++it, attr);  // while block
      ir_.emplace_back(PINSR_J, IRInsr::kLabel, jump_label);
      ir_[now_label].imm = InsertLabel();  // beq
      break;
    }
    case IF_ELSE_STMT: {
      FuncSpace start_space = cur_;
      size_t reg = AllocRegister(INT_TYPE);
      VisitRelopExprList(*it, reg);  // if expr
      size_t now_label = ir_.size();
      ir_.emplace_back(INSR_BEQ, IRInsr::kNoRD, reg, Reg(rv64::kZero),
                       IRInsr::kLabel, 0);
      if (!opt_.register_alloc) cur_ = start_space;
      VisitStatement(*++it, attr);          // if block
      ir_[now_label].imm = labels_.size();  // beq; get label num here
      now_label = ir_.size();
      ir_.emplace_back(PINSR_J, IRInsr::kLabel, 0);
      InsertLabel();                       // beq
      VisitStatement(*++it, attr);         // else block
      ir_[now_label].imm = InsertLabel();  // j
      break;
    }
    case IF_STMT: {
      FuncSpace start_space = cur_;
      size_t reg = AllocRegister(INT_TYPE);
      VisitRelopExprList(*it, reg);  // if expr
      size_t now_label = ir_.size();
      ir_.emplace_back(INSR_BEQ, IRInsr::kNoRD, reg, Reg(rv64::kZero),
                       IRInsr::kLabel, 0);
      if (!opt_.register_alloc) cur_ = start_space;
      VisitStatement(*++it, attr);         // if block
      ir_[now_label].imm = InsertLabel();  // beq
      break;
    }
    case FOR_STMT: {
      VisitAssignmentList(*it);  // for init
      size_t jump_label = InsertLabel(), now_label = kNoDest;
      if ((*++it)->child.size()) {
        FuncSpace start_space = cur_;
        size_t reg = AllocRegister(INT_TYPE);
        VisitRelopExprList(*it, reg);  // for expr
        now_label = ir_.size();
        ir_.emplace_back(INSR_BEQ, IRInsr::kNoRD, reg, Reg(rv64::kZero),
                         IRInsr::kLabel, 0);
        if (!opt_.register_alloc) cur_ = start_space;
      }
      auto for_stmt = ++it;
      VisitStatement(*++it, attr);     // for block
      VisitAssignmentList(*for_stmt);  // for continue
      ir_.emplace_back(PINSR_J, IRInsr::kLabel, jump_label);
      if (now_label != kNoDest) {
        ir_[now_label].imm = InsertLabel();  // beq
      }
      break;
    }
    case RETURN_STMT: {
      if (stmt->child.front()->child.size()) {
        // fprintf(stderr, "%d\n", (*stmt->child.begin())->node_type);
        Debug_("Return\n");
        FuncSpace start_space = cur_;
        size_t reg = AllocRegister(attr.return_type);
        VisitRelopExprList(*it, reg);
        size_t retreg = attr.return_type == INT_TYPE ? rv64::kA0 : rv64::kFa0;
        ir_.emplace_back(attr.return_type == INT_TYPE ? PINSR_MV : PINSR_FMV_S,
                         Reg(retreg), reg);
        // fprintf(stderr, "meow %d %d %d\n", attr.return_type,
        // (int)attr.params.size(), (int)reg);
        if (!opt_.register_alloc) cur_ = start_space;
      }
      ir_.emplace_back(PINSR_RET);
      break;
    }
    case ASSIGN_STMT: {
      VisitAssignment(stmt);
      break;
    }
    case FUNCTION_CALL_STMT: {
      FuncSpace start_space = cur_;
      VisitFunctionCall(stmt);
      if (!opt_.register_alloc) cur_ = start_space;
      break;
    }
  }
}

void CodeGen::VisitStmtList(AstNode* stmt_list, FunctionAttr& attr) {
  for (AstNode* stmt : stmt_list->child) VisitStatement(stmt, attr);
}

void CodeGen::VisitVariableDecl(AstNode* decl, bool global) {
  Debug_("VisitVariableDecl\n");
  for (auto it = std::next(decl->child.begin()); it != decl->child.end();
       it++) {
    VariableAttr& var_attr = GetAttribute<VariableAttr>(*it, tab_);
    if (global) {
      var_attr.offset = data_.size();
      if (var_attr.IsArray()) {
        data_.push_back(var_attr.size);
      } else {
        auto& value = std::get<IdentifierSemanticValue>((*it)->semantic_value);
        if (value.kind == WITH_INIT_ID) {
          AstNode* init_val = (*it)->child.front();
          if (init_val->node_type == CONST_VALUE_NODE) {
            uint32_t x = GetConst(init_val, var_attr.data_type);
            std::vector<uint8_t> v(4);
            for (int i = 0; i < 4; i++) v[i] = x >> (i * 8) & 255;
            data_.emplace_back(std::move(v));
          } else {
            // should not be here because of constant folding!
            assert(false);
          }
        } else {
          data_.push_back(var_attr.size);
        }
      }
    } else {
      if (var_attr.IsArray()) {
        var_attr.offset = AllocStack(var_attr.size);
      } else {
        auto& value = std::get<IdentifierSemanticValue>((*it)->semantic_value);
        var_attr.offset = AllocRegister(var_attr.data_type);
        if (value.kind == WITH_INIT_ID) {
          AstNode* init_val = (*it)->child.front();
          VisitRelopExpr(init_val, var_attr.offset);
        }
      }
    }
    var_attr.local = !global;
    var_attr.is_param = false;
  }
  Debug_("Done VisitVariableDecl\n");
}

void CodeGen::VisitDeclList(AstNode* decl_list, bool global) {
  // TODO: Remove type-decls after in semantics phase
  for (AstNode* decl : decl_list->child) {
    DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
    if (kind == VARIABLE_DECL) VisitVariableDecl(decl, global);
  }
}

void CodeGen::VisitBlock(AstNode* block, FunctionAttr& attr) {
  Debug_("VisitBlock\n");
  FuncSpace start_space = cur_;
  for (AstNode* nd : block->child) {
    switch (nd->node_type) {
      case STMT_LIST_NODE:
        VisitStmtList(nd, attr);
        break;
      case VARIABLE_DECL_LIST_NODE:
        VisitDeclList(nd, false);
        break;
    }
  }
  if (!opt_.register_alloc) cur_ = start_space;
}

void CodeGen::VisitFunctionDecl(AstNode* decl) {
  Debug_("VisitFunctionDecl\n");
  AstNode* id = *std::next(decl->child.begin());
  {
    auto& value = std::get<IdentifierSemanticValue>(id->semantic_value);
    func_.push_back(std::get<Identifier>(value.identifier));
  }
  FunctionAttr& attr = GetAttribute<FunctionAttr>(id, tab_);
  AstNode* block = *std::prev(decl->child.end());
  InitState(attr);
  Debug_(labels_.size(), ' ', labels_.back().is_func, '\n');
  size_t ival = 0, fval = 0, stk = 0;
  for (auto& attr_param : attr.params) {
    VariableAttr& param = tab_[attr_param].GetValue<VariableAttr>();
    size_t reg;
    if (param.IsArray() || param.data_type == INT_TYPE) {
      reg = AllocRegister(INT_TYPE);
      if (ival >= 8) {
        ir_.emplace_back(param.IsArray() ? INSR_LD : INSR_LW, reg,
                         Reg(rv64::kSp), IRInsr::kConst, stk++ * 8);
      } else {
        ir_.emplace_back(PINSR_MV, reg, Reg(rv64::kA0 + ival));
      }
      ival++;
    } else {
      reg = AllocRegister(FLOAT_TYPE);
      if (fval >= 8) {
        ir_.emplace_back(INSR_FLW, reg, Reg(rv64::kSp), IRInsr::kConst,
                         stk++ * 8);
      } else {
        ir_.emplace_back(PINSR_FMV_S, reg, Reg(rv64::kFa0 + fval));
      }
      fval++;
    }
    param.local = true;
    param.is_param = true;
    param.offset = reg;
  }
  ir_.emplace_back(PINSR_PUSH_SP);
  VisitBlock(block, attr);
  if (ir_.back().op != PINSR_RET) {
    ir_.emplace_back(PINSR_RET);
  }
  attr.sp_offset = max_.stk;
  attr.tot_preg = max_.reg;
  if (opt_.register_alloc) OptFunctionRegAlloc(attr);
  Debug_(max_.stk, ' ', max_.reg.ireg, ' ', max_.reg.freg, '\n');
}

void CodeGen::VisitGlobalDecl(AstNode* decl) {
  Debug_("VisitGlobalDecl\n");
  if (decl->node_type == VARIABLE_DECL_LIST_NODE) {
    FunctionAttr attr;  // unused
    VisitDeclList(decl, true);
    return;
  }
  DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
  switch (kind) {
    case FUNCTION_DECL:
      VisitFunctionDecl(decl);
      break;
    default:
      assert(false);
  }
}

void CodeGen::VisitProgram(AstNode* prog) {
  for (AstNode* decl : prog->child) VisitGlobalDecl(decl);
#if CODEGEN_DEBUG
  Debug_(labels_.size(), ' ', labels_.back().is_func, '\n');
  PrintIR();
#endif
}

// ------ Optimization ------

namespace {

struct InsrWriteReg_ {
  bool arr[kPseudoInsr];
  constexpr InsrWriteReg_() : arr{} {
    for (Opcode i :
         {INSR_LUI,      INSR_AUIPC,     INSR_JAL,      INSR_JALR,
          INSR_LB,       INSR_LH,        INSR_LW,       INSR_LD,
          INSR_LBU,      INSR_LHU,       INSR_LWU,      INSR_ADDI,
          INSR_SLTI,     INSR_SLTIU,     INSR_XORI,     INSR_ORI,
          INSR_ANDI,     INSR_SLLI,      INSR_SRLI,     INSR_SRAI,
          INSR_ADDIW,    INSR_SLLIW,     INSR_SRLIW,    INSR_SRAIW,
          INSR_ADD,      INSR_SUB,       INSR_SLL,      INSR_SLT,
          INSR_SLTU,     INSR_XOR,       INSR_SRL,      INSR_SRA,
          INSR_OR,       INSR_AND,       INSR_ADDW,     INSR_SUBW,
          INSR_SLLW,     INSR_SRLW,      INSR_SRAW,     INSR_MUL,
          INSR_MULH,     INSR_MULHSU,    INSR_MULHU,    INSR_DIV,
          INSR_DIVU,     INSR_REM,       INSR_REMU,     INSR_MULW,
          INSR_DIVW,     INSR_DIVUW,     INSR_REMW,     INSR_REMUW,
          INSR_FLW,      INSR_FLD,       INSR_FMADD_S,  INSR_FMSUB_S,
          INSR_FNMADD_S, INSR_FNMSUB_S,  INSR_FMADD_D,  INSR_FMSUB_D,
          INSR_FNMADD_D, INSR_FNMSUB_D,  INSR_FADD_S,   INSR_FSUB_S,
          INSR_FMUL_S,   INSR_FDIV_S,    INSR_FSQRT_S,  INSR_FSGNJ_S,
          INSR_FSGNJN_S, INSR_FSGNJX_S,  INSR_FMIN_S,   INSR_FMAX_S,
          INSR_FADD_D,   INSR_FSUB_D,    INSR_FMUL_D,   INSR_FDIV_D,
          INSR_FSQRT_D,  INSR_FSGNJ_D,   INSR_FSGNJN_D, INSR_FSGNJX_D,
          INSR_FMIN_D,   INSR_FMAX_D,    INSR_FCVT_W_S, INSR_FCVT_WU_S,
          INSR_FCVT_L_S, INSR_FCVT_LU_S, INSR_FCVT_W_D, INSR_FCVT_WU_D,
          INSR_FCVT_L_D, INSR_FCVT_LU_D, INSR_FCVT_S_W, INSR_FCVT_S_WU,
          INSR_FCVT_S_L, INSR_FCVT_S_LU, INSR_FCVT_D_W, INSR_FCVT_D_WU,
          INSR_FCVT_D_L, INSR_FCVT_D_LU, INSR_FCVT_S_D, INSR_FCVT_D_S,
          INSR_FMV_X_W,  INSR_FMV_X_D,   INSR_FMV_W_X,  INSR_FMV_D_X,
          INSR_FEQ_S,    INSR_FLT_S,     INSR_FLE_S,    INSR_FEQ_D,
          INSR_FLT_D,    INSR_FLE_D,     INSR_FCLASS_S, INSR_FCLASS_D,
          PINSR_LA,      PINSR_MV,       PINSR_FMV_S,   PINSR_LI}) {
      arr[i] = true;
    }
  }
  bool operator[](Opcode i) const { return arr[i]; }
};
struct InsrReadReg_ {
  uint8_t arr[kPseudoInsr];
  constexpr InsrReadReg_() : arr{} {
    for (Opcode i :
         {INSR_JALR,     INSR_LB,        INSR_LH,       INSR_LW,
          INSR_LD,       INSR_LBU,       INSR_LHU,      INSR_LWU,
          INSR_ADDI,     INSR_SLTI,      INSR_SLTIU,    INSR_XORI,
          INSR_ORI,      INSR_ANDI,      INSR_SLLI,     INSR_SRLI,
          INSR_SRAI,     INSR_ADDIW,     INSR_SLLIW,    INSR_SRLIW,
          INSR_SRAIW,    INSR_FLW,       INSR_FLD,      INSR_FSQRT_S,
          INSR_FCVT_W_S, INSR_FCVT_WU_S, INSR_FCVT_L_S, INSR_FCVT_LU_S,
          INSR_FCVT_W_D, INSR_FCVT_WU_D, INSR_FCVT_L_D, INSR_FCVT_LU_D,
          INSR_FCVT_S_W, INSR_FCVT_S_WU, INSR_FCVT_S_L, INSR_FCVT_S_LU,
          INSR_FCVT_D_W, INSR_FCVT_D_WU, INSR_FCVT_D_L, INSR_FCVT_D_LU,
          INSR_FCVT_S_D, INSR_FCVT_D_S,  INSR_FMV_X_W,  INSR_FMV_X_D,
          INSR_FMV_W_X,  INSR_FMV_D_X,   INSR_FCLASS_S, INSR_FCLASS_D,
          PINSR_MV,      PINSR_FMV_S}) {
      arr[i] = 1;
    }
    for (Opcode i : {INSR_BEQ,      INSR_BNE,      INSR_BLT,      INSR_BGE,
                     INSR_BLTU,     INSR_BGEU,     INSR_SB,       INSR_SH,
                     INSR_SW,       INSR_SD,       INSR_ADD,      INSR_SUB,
                     INSR_SLL,      INSR_SLT,      INSR_SLTU,     INSR_XOR,
                     INSR_SRL,      INSR_SRA,      INSR_OR,       INSR_AND,
                     INSR_ADDW,     INSR_SUBW,     INSR_SLLW,     INSR_SRLW,
                     INSR_SRAW,     INSR_MUL,      INSR_MULH,     INSR_MULHSU,
                     INSR_MULHU,    INSR_DIV,      INSR_DIVU,     INSR_REM,
                     INSR_REMU,     INSR_MULW,     INSR_DIVW,     INSR_DIVUW,
                     INSR_REMW,     INSR_REMUW,    INSR_FSW,      INSR_FSD,
                     INSR_FADD_S,   INSR_FSUB_S,   INSR_FMUL_S,   INSR_FDIV_S,
                     INSR_FSGNJ_S,  INSR_FSGNJN_S, INSR_FSGNJX_S, INSR_FMIN_S,
                     INSR_FMAX_S,   INSR_FADD_D,   INSR_FSUB_D,   INSR_FMUL_D,
                     INSR_FDIV_D,   INSR_FSQRT_D,  INSR_FSGNJ_D,  INSR_FSGNJN_D,
                     INSR_FSGNJX_D, INSR_FMIN_D,   INSR_FMAX_D,   INSR_FEQ_S,
                     INSR_FLT_S,    INSR_FLE_S,    INSR_FEQ_D,    INSR_FLT_D,
                     INSR_FLE_D}) {
      arr[i] = 2;
    }
    for (Opcode i :
         {INSR_FMADD_S, INSR_FMSUB_S, INSR_FNMADD_S, INSR_FNMSUB_S,
          INSR_FMADD_D, INSR_FMSUB_D, INSR_FNMADD_D, INSR_FNMSUB_D}) {
      arr[i] = 3;
    }
  }
  uint8_t operator[](Opcode i) const { return arr[i]; }
};
struct InsrFloatOpr_ {
  bool arr[kPseudoInsr][4];
  constexpr InsrFloatOpr_() : arr{} {
    constexpr std::pair<Opcode, uint8_t> table[] = {
#define X3(x) \
  {x, 0}, {x, 1}, {x, 2}, { x, 3 }
#define X2(x) \
  {x, 0}, {x, 1}, { x, 2 }
#define X1(x) {x, 0}, {x, 1}
        {INSR_FLW, 0},      {INSR_FLD, 0},
        {INSR_FSW, 2},      {INSR_FSD, 2},
        X3(INSR_FMADD_S),   X3(INSR_FMSUB_S),
        X3(INSR_FNMADD_S),  X3(INSR_FNMSUB_S),
        X3(INSR_FMADD_D),   X3(INSR_FMSUB_D),
        X3(INSR_FNMADD_D),  X3(INSR_FNMSUB_D),
        X2(INSR_FADD_S),    X2(INSR_FSUB_S),
        X2(INSR_FMUL_S),    X2(INSR_FDIV_S),
        X1(INSR_FSQRT_S),   X2(INSR_FSGNJ_S),
        X2(INSR_FSGNJN_S),  X2(INSR_FSGNJX_S),
        X2(INSR_FMIN_S),    X2(INSR_FMAX_S),
        X2(INSR_FADD_D),    X2(INSR_FSUB_D),
        X2(INSR_FMUL_D),    X2(INSR_FDIV_D),
        X1(INSR_FSQRT_D),   X2(INSR_FSGNJ_D),
        X2(INSR_FSGNJN_D),  X2(INSR_FSGNJX_D),
        X2(INSR_FMIN_D),    X2(INSR_FMAX_D),
        {INSR_FCVT_W_S, 1}, {INSR_FCVT_WU_S, 1},
        {INSR_FCVT_L_S, 1}, {INSR_FCVT_LU_S, 1},
        {INSR_FCVT_W_D, 1}, {INSR_FCVT_WU_D, 1},
        {INSR_FCVT_L_D, 1}, {INSR_FCVT_LU_D, 1},
        {INSR_FCVT_S_W, 0}, {INSR_FCVT_S_WU, 0},
        {INSR_FCVT_S_L, 0}, {INSR_FCVT_S_LU, 0},
        {INSR_FCVT_D_W, 0}, {INSR_FCVT_D_WU, 0},
        {INSR_FCVT_D_L, 0}, {INSR_FCVT_D_LU, 0},
        X1(INSR_FCVT_S_D),  X1(INSR_FCVT_D_S),
        {INSR_FMV_X_W, 1},  {INSR_FMV_X_D, 1},
        {INSR_FMV_W_X, 0},  {INSR_FMV_D_X, 0},
        {INSR_FEQ_S, 1},    {INSR_FEQ_S, 2},
        {INSR_FLT_S, 1},    {INSR_FLT_S, 2},
        {INSR_FLE_S, 1},    {INSR_FLE_S, 2},
        {INSR_FEQ_D, 1},    {INSR_FEQ_D, 2},
        {INSR_FLT_D, 1},    {INSR_FLT_D, 2},
        {INSR_FLE_D, 1},    {INSR_FLE_D, 2},
        {INSR_FCLASS_S, 1}, {INSR_FCLASS_D, 1},
        X1(PINSR_FMV_S)
#undef X1
#undef X2
#undef X3
    };
    for (auto i : table) arr[i.first][i.second] = true;
  }
  const bool* operator[](Opcode i) const { return arr[i]; }
};
const InsrWriteReg_ kWriteRegTable;
const InsrReadReg_ kReadRegTable;
const InsrFloatOpr_ kFloatOprTable;

class LifeTime {
 public:
  size_t l, r;
  bool freg;
  LifeTime() : l(size_t(0) - 1), r(0), freg(false) {}
  void UpdateL(size_t x) { if (l > x) l = x; }
  void UpdateR(size_t x) { if (r < x) r = x; }
};

class BasicBlock {
 public:
  struct RW {
    bool read_first, write;
    size_t last_read_pos, write_pos;
    RW(bool r, bool w, size_t rp, size_t wp)
        : read_first(r), write(w), last_read_pos(rp), write_pos(wp) {}
  };
  std::unordered_map<size_t, RW> rw;
  void Read(size_t reg, size_t pos) {
    auto it = rw.emplace(reg, RW(true, false, pos, 0));
    if (!it.second) it.first->second.last_read_pos = pos;
  }
  bool Write(size_t reg, size_t pos) {
    auto it = rw.emplace(reg, RW(false, true, 0, pos));
    if (!it.second) {
      it.first->second.write_pos = pos;
      if (it.first->second.write) return true;
      it.first->second.write = true;
    }
    return false;
  }
};

inline size_t MapReg(const IRInsr& insr, int x,
                     const std::vector<size_t>& ireg_map,
                     const std::vector<size_t>& freg_map) {
  const std::vector<size_t>& map =
      kFloatOprTable[insr.op][x] ? freg_map : ireg_map;
  switch (x) {
    case 0:
      return map[insr.rd.id];
    case 1:
      if (insr.rs1.id == kNoDest) return 0;
      return map[insr.rs1.id];
    case 2:
      return map[insr.rs2.id];
    // case 3: return map[insr.rs3.id];
    default:
      assert(false);
  }
  __builtin_unreachable();
}

BasicBlock AnalyzeBasicBlock(std::vector<IRInsr>& ir, size_t l, size_t r,
                             std::vector<LifeTime>& lifetime,
                             std::vector<size_t>& ireg_map,
                             std::vector<size_t>& freg_map) {
  // vector of (insr_id, rd=0/rs1=1/rs2=2/rs3=3)
  std::unordered_map<size_t, std::vector<std::pair<size_t, uint8_t>>> preg_insr;
  BasicBlock bb;
  for (size_t i = l; i < r; i++) {
    auto UpdateRead = [&](int x) {
      size_t reg = MapReg(ir[i], x, ireg_map, freg_map);
      bb.Read(reg, i);
      lifetime[reg].UpdateR(i);
      preg_insr[reg].emplace_back(i, x);
    };
    switch (kReadRegTable[ir[i].op]) {
      // case 3: if (!ir[i].rs3.is_real) UpdateRead(3);
      case 2:
        if (!ir[i].rs2.is_real) UpdateRead(2);
      case 1:
        if (!ir[i].rs1.is_real) UpdateRead(1);
      case 0:
        break;
      default:
        assert(false);
    }
    if (kWriteRegTable[ir[i].op] && !ir[i].rd.is_real) {
      size_t reg = MapReg(ir[i], 0, ireg_map, freg_map);
      if (bb.Write(reg, i)) { // the previous write is BB-local
        auto& mp = lifetime[reg].freg ? freg_map : ireg_map;
        size_t new_reg = lifetime.size();
        size_t new_reg_t = mp.size();
        // add a register for the previous write
        mp.push_back(new_reg);
        lifetime.emplace_back(lifetime[reg]);
        lifetime[reg] = LifeTime();
        // rename the register
        for (auto& x : preg_insr[reg]) {
          switch (x.second) {
            case 0:
              ir[x.first].rd.id = new_reg_t;
              break;
            case 1:
              ir[x.first].rs1.id = new_reg_t;
              break;
            case 2:
              ir[x.first].rs2.id = new_reg_t;
              break;
            // case 3: ir[x.first].rs3.id = new_reg_t; break;
            default:
              assert(false);
          }
        }
      }
      lifetime[reg].UpdateL(i);
      preg_insr[reg].clear();
      preg_insr[reg].emplace_back(i, 0);
    }
  }
  return bb;
}

constexpr size_t kEmpty = -(size_t)1;
class LifeTimeCalc {
  const std::vector<size_t>& bb_boundary_;
  const std::vector<std::array<size_t, 2>>& bb_edge_;
  const std::vector<BasicBlock>& bb_rw_;
  std::vector<LifeTime>& lifetimes_;
  std::vector<uint8_t> ans_;

  static constexpr uint8_t kReadVis = 1;
  static constexpr uint8_t kReadAns = 2;
  static constexpr uint8_t kWriteAns = 4;
  static constexpr uint8_t kOK = kReadAns | kWriteAns;
  bool OK(size_t x) {
    return (ans_[x] & (kReadAns | kWriteAns)) == (kReadAns | kWriteAns);
  }
  void DFSRead(size_t x, size_t reg) {
    ans_[x] |= kReadVis;
    auto it = bb_rw_[x].rw.find(reg);
    if (it != bb_rw_[x].rw.end() && it->second.read_first) ans_[x] |= kReadAns;
    if (it == bb_rw_[x].rw.end() || !it->second.write) {
      for (size_t i = 0; i < 2 && bb_edge_[x][i] != kEmpty; i++) {
        if (!(ans_[bb_edge_[x][i]] & kReadVis)) {
          DFSRead(bb_edge_[x][i], reg);
        }
        ans_[x] |= kReadAns & ans_[bb_edge_[x][i]];
      }
    }
  }
  void DFSWrite(size_t x, size_t reg, bool root = true) {
    ans_[x] |= kWriteAns;
    auto it = bb_rw_[x].rw.find(reg);
    if (root || it == bb_rw_[x].rw.end() || !it->second.write) {
      for (size_t i = 0; i < 2 && bb_edge_[x][i] != kEmpty; i++) {
        if (ans_[bb_edge_[x][i]] & kWriteAns) continue;
        DFSWrite(bb_edge_[x][i], reg, false);
      }
    }
  }
 public:
  LifeTimeCalc(const std::vector<size_t>& bb_boundary,
               const std::vector<std::array<size_t, 2>>& bb_edge,
               const std::vector<BasicBlock>& bb_rw,
               std::vector<LifeTime>& lifetimes)
      : bb_boundary_(bb_boundary),
        bb_edge_(bb_edge),
        bb_rw_(bb_rw),
        lifetimes_(lifetimes),
        ans_(bb_edge.size()) {}
  void operator()(size_t reg) {
    std::fill(ans_.begin(), ans_.end(), 0);
    for (size_t i = 0; i < bb_edge_.size(); i++) {
      if (ans_[i] & kReadVis) continue;
      DFSRead(i, reg);
    }
    for (size_t i = 0; i < bb_edge_.size(); i++) {
      auto it = bb_rw_[i].rw.find(reg);
      if (it == bb_rw_[i].rw.end() || !it->second.write) continue;
      DFSWrite(i, reg);
    }
    size_t first, last;
    for (first = 0; first < bb_edge_.size(); first++) {
      if (OK(first)) break;
    }
    if (first == bb_edge_.size()) return;
    for (last = bb_edge_.size() - 1;; last--) {
      if (OK(last)) break;
    }
    auto it = bb_rw_[first].rw.find(reg);
    lifetimes_[reg].l = it == bb_rw_[first].rw.end() || it->second.read_first
                            ? bb_boundary_[first]
                            : it->second.write_pos;
    lifetimes_[reg].r =
        (bb_edge_[last][0] != kEmpty && OK(bb_edge_[last][0])) ||
                (bb_edge_[last][1] != kEmpty && OK(bb_edge_[last][1]))
            ? bb_boundary_[last + 1]
            : bb_rw_[last].rw.find(reg)->second.last_read_pos;
  }
};

struct LifeTimeReg {
  size_t id, l, r;
  bool operator<(const LifeTimeReg& x) const {
    return r < x.r;
  }
};

struct Node {
  size_t id, r;
  bool operator<(const Node& x) const { return r > x.r; }
};

// at most K registers
size_t AssignRegister(size_t offset, size_t K, std::vector<LifeTimeReg>& vec,
                      std::vector<size_t>& ret) {
  std::sort(vec.begin(), vec.end());
  std::priority_queue<Node> pq;
  for (auto& i : vec) {
    if (pq.size() && pq.top().r <= i.l) {
      size_t id = pq.top().id;
      pq.pop();
      ret[i.id] = offset + id;
      pq.push({id, i.r});
    } else if (pq.size() < K) {
      ret[i.id] = offset + pq.size();
      pq.push({pq.size(), i.r});
    }
  }
  return pq.size();
}

// without limit; minimize registers used
size_t AssignRegister(size_t offset, std::vector<LifeTimeReg>& vec,
                      std::vector<size_t>& ret) {
  std::sort(vec.begin(), vec.end(), [](auto a, auto b) { return a.l < b.l; });
  std::priority_queue<Node> pq;
  for (auto& i : vec) {
    if (pq.size() && pq.top().r <= i.l) {
      size_t id = pq.top().id;
      pq.pop();
      ret[i.id] = offset + id;
      pq.push({id, i.r});
    } else {
      ret[i.id] = offset + pq.size();
      pq.push({pq.size(), i.r});
    }
  }
  return pq.size();
}

} // namespace

void CodeGen::OptFunctionRegAlloc(FunctionAttr& attr) {
  size_t first = labels_[attr.label].ir_pos, last = ir_.size();
  std::vector<size_t> bb_boundary, label_bb;
  std::vector<std::array<size_t, 2>> bb_edge;
  {  // identify basic blocks
    bb_boundary.push_back(first);
    for (size_t i = attr.label; i < labels_.size(); i++) {
      bb_boundary.push_back(labels_[i].ir_pos);
    }
    bb_boundary.push_back(last);
    size_t mid = bb_boundary.size();
    for (size_t i = first; i < last; i++) {
      if (kRV64InsrFormat[ir_[i].op] == B_TYPE ||
          kRV64InsrFormat[ir_[i].op] == J_TYPE || ir_[i].op == INSR_JALR ||
          ir_[i].op == PINSR_J || ir_[i].op == PINSR_RET) {
        bb_boundary.push_back(i + 1);
      }
    }
    std::inplace_merge(bb_boundary.begin(), bb_boundary.begin() + mid,
                       bb_boundary.end());
    assert(std::is_sorted(bb_boundary.begin(), bb_boundary.end()));
    bb_boundary.resize(std::unique(bb_boundary.begin(), bb_boundary.end()) -
                       bb_boundary.begin());
    // basic block ID for each label
    for (size_t i = attr.label, j = 0; i < labels_.size(); i++) {
      while (j + 1 < bb_boundary.size() &&
             bb_boundary[j + 1] <= labels_[i].ir_pos) {
        j++;
      }
      label_bb.push_back(j);
    }
  }
  // summary of each basic block
  size_t orig_reg = attr.tot_preg.ireg + attr.tot_preg.freg;
  std::vector<LifeTime> lifetimes(orig_reg);
  std::vector<size_t> ireg_map, freg_map;
  for (size_t i = 0; i < attr.tot_preg.ireg; i++) ireg_map.push_back(i);
  for (size_t i = 0; i < attr.tot_preg.freg; i++) {
    freg_map.push_back(i + attr.tot_preg.ireg);
    lifetimes[i + attr.tot_preg.ireg].freg = true;
  }
  std::vector<BasicBlock> bb_rw;
  for (size_t i = 0; i + 1 < bb_boundary.size(); i++) {
    bb_rw.emplace_back(AnalyzeBasicBlock(ir_, bb_boundary[i],
                                         bb_boundary[i + 1], lifetimes,
                                         ireg_map, freg_map));
    PrintIR();
  }
  bb_rw.emplace_back();
  attr.tot_preg.ireg = ireg_map.size();
  attr.tot_preg.freg = freg_map.size();

  // construct graph of basic blocks
  for (size_t i = 1; i < bb_boundary.size(); i++) {
    auto& insr = ir_[bb_boundary[i] - 1];
    if (kRV64InsrFormat[insr.op] == B_TYPE) {
      assert(insr.imm_type == IRInsr::kLabel);
      bb_edge.push_back({i, label_bb[insr.imm - attr.label]});
    } else if (kRV64InsrFormat[insr.op] == J_TYPE || insr.op == INSR_JALR) {
      assert(false);
    } else if (insr.op == PINSR_J) {
      assert(insr.imm_type == IRInsr::kLabel);
      bb_edge.push_back({label_bb[insr.imm - attr.label], kEmpty});
    } else if (insr.op == PINSR_RET) {
      bb_edge.push_back({bb_boundary.size() - 1, kEmpty});
    } else {
      bb_edge.push_back({i, kEmpty});
    }
  }
  bb_edge.push_back({kEmpty, kEmpty});

  { // calculate lifetime of each register
    LifeTimeCalc calc(bb_boundary, bb_edge, bb_rw, lifetimes);
    for (size_t i = 0; i < orig_reg; i++) calc(i);
#if CODEGEN_DEBUG
  for (size_t i = 0; i < bb_rw.size(); i++) {
    std::cerr << i << "->" << (int)bb_edge[i][0] << ',' << (int)bb_edge[i][1]
              << ':';
    for (auto& j : bb_rw[i].rw) {
      std::cerr << '(' << j.first << ',' << j.second.read_first << ','
                << j.second.write << ')';
    }
    std::cerr << '\n';
  }
  for (size_t i = 0; i < lifetimes.size(); i++) {
    std::cerr << i << '(' << lifetimes[i].freg << "):" << lifetimes[i].l << '-'
              << lifetimes[i].r << '\n';
  }
  std::cerr << std::flush;
#endif
  }

  std::vector<size_t> call_prefix(last - first + 1);
  for (size_t i = first; i < last; i++) {
    call_prefix[i + 1 - first] =
        call_prefix[i - first] + (ir_[i].op == PINSR_CALL);
  }
  std::vector<size_t> reg_map(lifetimes.size(), kEmpty);
  std::vector<LifeTimeReg> int_reg, float_reg;
  for (size_t i = 0; i < lifetimes.size(); i++) {
    if (lifetimes[i].l >= lifetimes[i].r) {
      reg_map[i] = 0;
    } else if (call_prefix[lifetimes[i].r - first] ==
               call_prefix[lifetimes[i].l - first]) {
      // no function call between
      if (lifetimes[i].freg) {
        float_reg.push_back({i, lifetimes[i].l, lifetimes[i].r});
      } else {
        int_reg.push_back({i, lifetimes[i].l, lifetimes[i].r});
      }
    }
  }
  size_t ireg_t = AssignRegister(0, 5, int_reg, reg_map);
  size_t freg_t = AssignRegister(0, 10, float_reg, reg_map);
  int_reg.clear(); float_reg.clear();
  for (size_t i = 0; i < lifetimes.size(); i++) {
    if (reg_map[i] != kEmpty) continue;
    if (lifetimes[i].freg) {
      float_reg.push_back({i, lifetimes[i].l, lifetimes[i].r});
    } else {
      int_reg.push_back({i, lifetimes[i].l, lifetimes[i].r});
    }
  }
  size_t ireg_s = AssignRegister(ireg_t, int_reg, reg_map);
  size_t freg_s = AssignRegister(freg_t, float_reg, reg_map);
  for (auto& i : reg_map) {
    if (i == kEmpty) i = 0;
  }

  for (size_t i = first; i < last; i++) {
    uint8_t read = kReadRegTable[ir_[i].op];
    if (kWriteRegTable[ir_[i].op] && !ir_[i].rd.is_real) {
      size_t reg = MapReg(ir_[i], 0, ireg_map, freg_map);
      ir_[i].rd.id = reg_map[reg];
    }
    switch (read) {
      //case 3:
      //  if (kWriteRegTable[ir_[i].op] && !ir_[i].rs3.is_real) {
      //    size_t reg = MapReg(ir_[i], 3, ireg_map, freg_map);
      //    ir_[i].rs3.id = reg_map[reg];
      //  }
      case 2:
        if (kWriteRegTable[ir_[i].op] && !ir_[i].rs2.is_real) {
          size_t reg = MapReg(ir_[i], 2, ireg_map, freg_map);
          ir_[i].rs2.id = reg_map[reg];
        }
      case 1:
        if (kWriteRegTable[ir_[i].op] && !ir_[i].rs1.is_real) {
          size_t reg = MapReg(ir_[i], 1, ireg_map, freg_map);
          ir_[i].rs1.id = reg_map[reg];
        }
      case 0: break;
      default: assert(false);
    }
  }
  attr.tot_preg.ireg = std::max((size_t)1, ireg_t + ireg_s);
  attr.tot_preg.freg = std::max((size_t)1, freg_t + freg_s);
  FuncRegInfo info;
  for (size_t i = 0; i < ireg_t; i++) info.int_caller.push_back(1);
  for (size_t i = 0; i < ireg_s; i++) info.int_caller.push_back(0);
  if (info.int_caller.empty()) info.int_caller.emplace_back(0);
  for (size_t i = 0; i < freg_t; i++) info.float_caller.push_back(1);
  for (size_t i = 0; i < freg_s; i++) info.float_caller.push_back(0);
  if (info.float_caller.empty()) info.float_caller.emplace_back(0);
  func_reg_info_.emplace_back(std::move(info));
}

#if CODEGEN_DEBUG
namespace {

std::string RegString(const IRInsr::Register& reg) {
  if (reg.is_real) {
    if (reg.id & 128) return rv64::kFloatRegisterName[reg.id ^ 128];
    return rv64::kIntRegisterName[reg.id];
  }
  return "R" + std::to_string(reg.id);
}

}  // namespace

void CodeGen::PrintIR() {
  for (size_t i = 0, j = 0, k = 0; i < ir_.size(); i++) {
    while (j < labels_.size() && labels_[j].ir_pos == i) {
      if (labels_[j].is_func) {
        auto& attr = tab_[func_[k].first].GetValue<FunctionAttr>();
        fprintf(stderr, "%s (%zu %zu): ", std::string(func_[k].second).c_str(),
                attr.tot_preg.ireg, attr.tot_preg.freg);
        k++;
      } else {
        fprintf(stderr, ".L%03zu: ", j);
      }
      j++;
    }
    fprintf(stderr, "%s, RD:%s, RS1:%s, RS2:%s, ",
            kRV64InsrCode[ir_[i].op].c_str(), RegString(ir_[i].rd).c_str(),
            RegString(ir_[i].rs1).c_str(), RegString(ir_[i].rs2).c_str());
    switch (ir_[i].imm_type) {
      case IRInsr::kConst:
        fprintf(stderr, "%ld\n", ir_[i].imm);
        break;
      case IRInsr::kLabel:
        fprintf(stderr, ".L%ld\n", ir_[i].imm);
        break;
      case IRInsr::kData:
        fprintf(stderr, ".D%ld\n", ir_[i].imm);
        break;
      case IRInsr::kRoundingMode:
        fprintf(stderr, "%s\n", rv64::kRoundingModeName[ir_[i].imm].c_str());
        break;
    }
  }
  for (size_t i = 0; i < data_.size(); i++) {
    fprintf(stderr, ".D%zu: ", i);
    switch (data_[i].index()) {
      case 0:
        fprintf(stderr, ".hex ");
        for (uint8_t j : std::get<std::vector<uint8_t>>(data_[i])) {
          fprintf(stderr, "%02x", j);
        }
        break;
      case 1:
        fprintf(stderr, ".string \"%s\"",
                std::get<std::string>(data_[i]).c_str());
        break;
      case 2:
        fprintf(stderr, ".zero %zu", std::get<size_t>(data_[i]));
        break;
    }
    fprintf(stderr, "\n");
  }
  fprintf(stderr, "\n");
}
#endif  // CODEGEN_DEBUG
