#include "codegen.h"

#include <cassert>
#include <cstring>

#include "analysis.h"
#include "ast.h"
#include "utils.h"

namespace {

template <class T>
T& GetAttribute(AstNode* id, std::vector<TableEntry>& tab) {
  assert(id->node_type == IDENTIFIER_NODE);
  auto& value = std::get<IdentifierSemanticValue>(id->semantic_value);
  return tab[std::get<Identifier>(value.identifier).first].GetValue<T>();
}

inline uint32_t GetConst(AstNode* expr) {
  auto& value = std::get<ConstValue>(expr->semantic_value);
  uint32_t x;
  if (expr->data_type == INT_TYPE) {
    memcpy(&x, &std::get<int32_t>(value), 4);
  } else {
    memcpy(&x, &std::get<FloatType>(value), 4);
  }
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
  cur_stack_ = attr.sp_offset = 0;
  cur_register_ = attr.tot_pseudo_reg = attr.params.size();
  attr.label = labels_.size();
  InsertLabel(true);
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

void CodeGen::VisitConversion(AstNode* expr, FunctionAttr& attr, size_t dest) {
  auto& value = std::get<ConversionSemanticValue>(expr->semantic_value);
  size_t chval = value.to == FLOAT_TYPE || value.from == FLOAT_TYPE
                     ? AllocRegister(attr)
                     : dest;
  VisitRelopExpr(expr->child.front(), attr, chval);
  switch (value.from) {
    case FLOAT_TYPE: {
      if (value.to == INT_TYPE) {
        ir_.emplace_back(INSR_FCVT_W_S, dest, chval, IRInsr::kRoundingMode,
                         rv64::kRTZ);
        ir_.emplace_back(INSR_ADDIW, dest, dest, IRInsr::kConst, 0);
      } else if (value.to == BOOLEAN_TYPE) {
        size_t tmp = AllocRegister(attr);
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
          // TODO: check if this is unnecessary?
        }
      } else if (value.to == FLOAT_TYPE) {
        ir_.emplace_back(INSR_FCVT_S_W, dest, chval);
      }
      break;
    }
  }
}

void CodeGen::VisitOpr(AstNode* expr, FunctionAttr& attr, size_t dest) {
  auto& value = std::get<ExprSemanticValue>(expr->semantic_value);
  DataType child_type = expr->child.front()->data_type;
  if (value.kind == BINARY_OPERATION) {
    BinaryOperator op = std::get<BinaryOperator>(value.op);
    if (op == BINARY_OP_OR || op == BINARY_OP_AND) {  // short-circuit
      assert(child_type == BOOLEAN_TYPE);
      assert((*std::next(expr->child.begin()))->data_type == BOOLEAN_TYPE);
      VisitRelopExpr(expr->child.front(), attr, dest);
      size_t now_label = ir_.size();
      ir_.emplace_back(op == BINARY_OP_OR ? INSR_BNE : INSR_BEQ, IRInsr::kNoRD,
                       dest, Reg(rv64::kZero), IRInsr::kLabel, 0);
      VisitRelopExpr(expr->child.front(), attr, dest);
      ir_[now_label].imm = InsertLabel();
      return;
    }
    if (child_type == INT_TYPE || child_type == BOOLEAN_TYPE) {
      size_t chval = AllocRegister(attr);
      VisitRelopExpr(expr->child.front(), attr, dest);
      VisitRelopExpr(*std::next(expr->child.begin()), attr, chval);
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
      size_t chval1 = AllocRegister(attr);
      size_t chval2 = expr->data_type == INT_TYPE ? AllocRegister(attr) : dest;
      VisitRelopExpr(expr->child.front(), attr, chval1);
      VisitRelopExpr(*std::next(expr->child.begin()), attr, chval2);
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
          ir_.emplace_back(INSR_FLE_S, dest, chval2, chval1);
          break;
        case BINARY_OP_GE:
          ir_.emplace_back(INSR_FLT_S, dest, chval2, chval1);
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
    VisitRelopExpr(expr->child.front(), attr, dest);
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
  if ((x >> 32) == 0) {  // 32-bit values
    if (x >= 0xfffff800 || x < 0x800) {
      ir_.emplace_back(INSR_ADDIW, dest, Reg(rv64::kZero), IRInsr::kConst,
                       (int32_t)x);
    } else {
      uint32_t tx = (x & 0xfffffc00) + (x & 0x400);
      ir_.emplace_back(INSR_LUI, dest, IRInsr::kConst, tx >> 12);
      if (x != tx) {
        ir_.emplace_back(INSR_ADDIW, dest, Reg(rv64::kZero), IRInsr::kConst,
                         (int32_t)(x - tx));
      }
    }
  } else {
    std::vector<uint8_t> res;
    for (int i = 0; i < 8; i++) res.push_back(x >> (i * 8) & 255);
    ir_.emplace_back(INSR_LD, dest, IRInsr::kData, data_.size());
    data_.emplace_back(std::move(res));
  }
}

void CodeGen::VisitConst(AstNode* expr, FunctionAttr& attr, size_t dest) {
  auto& value = std::get<ConstValue>(expr->semantic_value);
  if (expr->data_type == CONST_STRING_TYPE) {
    ir_.emplace_back(PINSR_LA, dest, IRInsr::kData, data_.size());
    data_.emplace_back(std::get<std::string>(value));
  } else if (expr->data_type == INT_TYPE || expr->data_type == FLOAT_TYPE) {
    size_t chval = expr->data_type == INT_TYPE ? dest : AllocRegister(attr);
    LoadConst(GetConst(expr), chval);
    if (expr->data_type == FLOAT_TYPE) {
      ir_.emplace_back(INSR_FMV_W_X, dest, chval);
    }
  } else {
    assert(false);
  }
}

// Load address to dest; if is slice, return true
bool CodeGen::VisitArray(AstNode* expr, FunctionAttr& attr, size_t dest) {
  size_t start_reg = cur_register_;
  auto& value = std::get<IdentifierSemanticValue>(expr->semantic_value);
  const TableEntry& entry = tab_[std::get<Identifier>(value.identifier).first];
  const VariableAttr& var_attr = entry.GetValue<VariableAttr>();
  size_t reg = AllocRegister(attr), i = 0;
  for (AstNode* dim : expr->child) {
    if (i == 0) {
      VisitRelopExpr(dim, attr, reg);
      ir_.emplace_back(INSR_ADD, dest, dest, reg);
    } else {
      VisitRelopExpr(dim, attr, dest);
    }
    if (++i == var_attr.dims.size()) {
      ir_.emplace_back(INSR_SLLI, dest, dest, IRInsr::kConst, 2);
    } else {
      LoadConst(var_attr.dims[i], reg);
      ir_.emplace_back(INSR_MUL, dest, dest, reg);
    }
  }
  uint64_t x = 4;
  for (i++; i < var_attr.dims.size(); i++) x *= var_attr.dims[i];
  LoadConst(x, reg);
  ir_.emplace_back(INSR_MUL, dest, dest, reg);
  if (var_attr.is_param) {
    ir_.emplace_back(INSR_ADD, dest, dest, var_attr.offset);
  } else if (var_attr.local) {
    ir_.emplace_back(INSR_ADD, dest, dest, Reg(rv64::kSp));
    ir_.emplace_back(INSR_ADDI, dest, dest, IRInsr::kConst,
                     -(int64_t)var_attr.offset);
  } else {
    ir_.emplace_back(PINSR_LA, reg, IRInsr::kData, var_attr.offset);
    ir_.emplace_back(INSR_ADD, dest, dest, reg);
  }
  cur_register_ = start_reg;
  return i != var_attr.dims.size();
}

void CodeGen::VisitIdentifier(AstNode* expr, FunctionAttr& attr, size_t dest) {
  auto& value = std::get<IdentifierSemanticValue>(expr->semantic_value);
  const TableEntry& entry = tab_[std::get<Identifier>(value.identifier).first];
  const VariableAttr& var_attr = entry.GetValue<VariableAttr>();
  assert(var_attr.data_type == FLOAT_TYPE || var_attr.data_type == INT_TYPE);
  if (var_attr.IsArray()) {
    size_t start_reg = cur_register_;
    size_t reg = AllocRegister(attr);
    if (!VisitArray(expr, attr, reg)) {
      // not slice, load its content
      if (var_attr.data_type == FLOAT_TYPE) {
        ir_.emplace_back(INSR_FLW, dest, reg, IRInsr::kConst, 0);
      } else {
        ir_.emplace_back(INSR_LW, dest, reg, IRInsr::kConst, 0);
      }
    }
    cur_register_ = start_reg;
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

void CodeGen::VisitFunctionCall(AstNode* expr, FunctionAttr& attr,
                                size_t dest) {
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
    DataType type =
        func_attr
            ? tab_[func_attr->params[i]].GetValue<VariableAttr>().data_type
            : (*it)->data_type;
    VisitRelopExpr(*it, attr, dest);
    if (type == CONST_STRING_TYPE || type == INT_TYPE || type == INT_PTR_TYPE ||
        type == FLOAT_PTR_TYPE) {
      if (ival >= 8) {
        stk_store.push_back(ir_.size());
        ir_.emplace_back(type == INT_TYPE ? INSR_SW : INSR_SD, IRInsr::kNoRD,
                         Reg(rv64::kSp), dest, IRInsr::kConst, 0);
      } else {
        ir_.emplace_back(PINSR_MV, Reg(rv64::kA0 + ival), dest);
      }
      ival++;
    } else if (type == FLOAT_TYPE) {
      if (fval >= 8) {
        stk_store.push_back(ir_.size());
        ir_.emplace_back(INSR_SW, IRInsr::kNoRD, Reg(rv64::kSp), dest,
                         IRInsr::kConst, 0);
      } else {
        ir_.emplace_back(PINSR_FMV_S, Reg(rv64::kFa0 + fval), dest);
      }
      fval++;
    } else {
      assert(false);
    }
  }
  size_t stk = (stk_store.size() + 1) / 2 * 2;  // 16-byte align
  for (size_t i = 0; i < stk_store.size(); i++) {
    ir_[stk_store[i]].imm = (stk - i) * -8;
  }
  if (stk) {
    ir_.emplace_back(INSR_ADDI, Reg(rv64::kSp), Reg(rv64::kSp), IRInsr::kConst,
                     stk * -8);
  }
  ir_.emplace_back(PINSR_CALL, IRInsr::kLabel, func_label);
  switch (return_type) {
    case INT_TYPE:
      ir_.emplace_back(PINSR_MV, dest, Reg(rv64::kA0));
      break;
    case FLOAT_TYPE:
      ir_.emplace_back(PINSR_FMV_S, dest, Reg(rv64::kFa0));
      break;
    case VOID_TYPE: break;
    default:
      assert(false);
  }
}

void CodeGen::VisitRelopExpr(AstNode* expr, FunctionAttr& attr, size_t dest) {
  size_t start_reg = cur_register_;
  switch (expr->node_type) {
    case CONVERSION_NODE:
      VisitConversion(expr, attr, dest);
      break;
    case EXPR_NODE:
      VisitOpr(expr, attr, dest);
      break;
    case CONST_VALUE_NODE:
      VisitConst(expr, attr, dest);
      break;
    case IDENTIFIER_NODE:
      VisitIdentifier(expr, attr, dest);
      break;
    case STMT_NODE:
      VisitFunctionCall(expr, attr, dest);
      break;
    default:
      assert(false);
  }
  cur_register_ = start_reg;
}

void CodeGen::VisitAssignment(AstNode* expr, FunctionAttr& attr) {
  Debug_("VisitAssignment\n");
  size_t start_reg = cur_register_;
  size_t valreg = AllocRegister(attr);
  VisitRelopExpr(*std::next(expr->child.begin()), attr, valreg);
  AstNode* var = expr->child.front();
  auto& value = std::get<IdentifierSemanticValue>(var->semantic_value);
  const TableEntry& entry = tab_[std::get<Identifier>(value.identifier).first];
  const VariableAttr& var_attr = entry.GetValue<VariableAttr>();
  assert(var_attr.data_type == FLOAT_TYPE || var_attr.data_type == INT_TYPE);
  if (var_attr.IsArray()) {
    size_t reg = AllocRegister(attr);
    if (VisitArray(expr, attr, reg)) assert(false);
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
  cur_register_ = start_reg;
}

void CodeGen::VisitStatement(AstNode* stmt, FunctionAttr& attr) {
  if (stmt->node_type == BLOCK_NODE) return VisitBlock(stmt, attr);
  if (stmt->node_type != STMT_NODE) return;
  auto& value = std::get<StmtSemanticValue>(stmt->semantic_value);
  auto it = stmt->child.begin();
  switch (value.kind) {
    case WHILE_STMT: {
      size_t now_reg = cur_register_;
      size_t reg = AllocRegister(attr);
      size_t jump_label = InsertLabel();
      VisitRelopExpr(*it, attr, reg);  // while expr
      size_t now_label = ir_.size();
      ir_.emplace_back(INSR_BEQ, IRInsr::kNoRD, reg, Reg(rv64::kZero),
                       IRInsr::kLabel, 0);
      cur_register_ = now_reg;
      VisitStatement(*++it, attr);  // while block
      ir_.emplace_back(PINSR_J, IRInsr::kLabel, jump_label);
      ir_[now_label].imm = InsertLabel();  // beq
      break;
    }
    case IF_ELSE_STMT: {
      size_t now_reg = cur_register_;
      size_t reg = AllocRegister(attr);
      VisitRelopExpr(*it, attr, reg);  // if expr
      size_t now_label = ir_.size();
      ir_.emplace_back(INSR_BEQ, IRInsr::kNoRD, reg, Reg(rv64::kZero),
                       IRInsr::kLabel, 0);
      cur_register_ = now_reg;
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
      size_t now_reg = cur_register_;
      size_t reg = AllocRegister(attr);
      VisitRelopExpr(*it, attr, reg);  // if expr
      size_t now_label = ir_.size();
      ir_.emplace_back(INSR_BEQ, IRInsr::kNoRD, reg, Reg(rv64::kZero),
                       IRInsr::kLabel, 0);
      cur_register_ = now_reg;
      VisitStatement(*++it, attr);         // if block
      ir_[now_label].imm = InsertLabel();  // beq
      break;
    }
    case FOR_STMT: // TODO
      break;
    case RETURN_STMT: {
      if (stmt->child.size()) {
        size_t now_reg = cur_register_;
        size_t reg = AllocRegister(attr);
        VisitRelopExpr(*it, attr, reg);
        size_t retreg = attr.return_type == INT_TYPE ? rv64::kA0 : rv64::kFa0;
        ir_.emplace_back(PINSR_MV, Reg(retreg), reg);
        cur_register_ = now_reg;
      }
      ir_.emplace_back(PINSR_RET);
      break;
    }
    case ASSIGN_STMT: {
      VisitAssignment(stmt, attr);
      break;
    }
    case FUNCTION_CALL_STMT: {
      size_t now_reg = cur_register_;
      VisitFunctionCall(stmt, attr, AllocRegister(attr));
      cur_register_ = now_reg;
      break;
    }
  }
}

void CodeGen::VisitStmtList(AstNode* stmt_list, FunctionAttr& attr) {
  for (AstNode* stmt : stmt_list->child) VisitStatement(stmt, attr);
}

void CodeGen::VisitVariableDecl(AstNode* decl, FunctionAttr& attr,
                                bool global) {
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
            uint32_t x = GetConst(init_val);
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
    var_attr.local = !global;
    var_attr.is_param = false;
  }
  Debug_("Done VisitVariableDecl\n");
}

void CodeGen::VisitDeclList(AstNode* decl_list, FunctionAttr& attr,
                            bool global) {
  // TODO: Remove type-decls after in semantics phase
  for (AstNode* decl : decl_list->child) {
    DeclKind kind = std::get<DeclSemanticValue>(decl->semantic_value).kind;
    if (kind == VARIABLE_DECL) VisitVariableDecl(decl, attr, global);
  }
}

void CodeGen::VisitBlock(AstNode* block, FunctionAttr& attr) {
  Debug_("VisitBlock\n");
  size_t stack_tmp = cur_stack_;
  for (AstNode* nd : block->child) {
    switch (nd->node_type) {
      case STMT_LIST_NODE:
        VisitStmtList(nd, attr);
        break;
      case VARIABLE_DECL_LIST_NODE:
        VisitDeclList(nd, attr, false);
        break;
    }
  }
  cur_stack_ = stack_tmp;
}

void CodeGen::VisitFunctionDecl(AstNode* decl) {
  Debug_("VisitFunctionDecl\n");
  AstNode* id = *std::next(decl->child.begin());
  {
    auto& value = std::get<IdentifierSemanticValue>(id->semantic_value);
    func_.push_back(std::get<Identifier>(value.identifier));
  }
  Debug_("aaaa");
  FunctionAttr& attr = GetAttribute<FunctionAttr>(id, tab_);
  AstNode* block = *std::prev(decl->child.end());
  InitState(attr);
  for (size_t i = 0, ival = 0, fval = 0, stk = 0; i < attr.params.size(); i++) {
    VariableAttr& param = tab_[attr.params[i]].GetValue<VariableAttr>();
    if (param.IsArray() || param.data_type == INT_TYPE) {
      if (ival >= 8) {
        ir_.emplace_back(param.IsArray() ? INSR_LD : INSR_LW, i, Reg(rv64::kSp),
                         stk++ * 8);
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
  Debug_("sp_offset = ", attr.sp_offset, '\n');
}

void CodeGen::VisitGlobalDecl(AstNode* decl) {
  Debug_("VisitGlobalDecl\n");
  if (decl->node_type == VARIABLE_DECL_LIST_NODE) {
    FunctionAttr attr;  // unused
    VisitDeclList(decl, attr, true);
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
#ifdef CODEGEN_DEBUG
  PrintIR();
#endif
}

#ifdef CODEGEN_DEBUG
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
  for (size_t i = 0, j = 0; i < ir_.size(); i++) {
    if (j < labels_.size() && labels_[j].ir_pos == i) {
      printf(".%c%03zu: ", "LF"[labels_[j].is_func], j);
      j++;
    }
    printf("%s, RD:%s, RS1:%s, RS2:%s, ",
           kRV64InsrCode.find(ir_[i].op)->second.c_str(),
           RegString(ir_[i].rd).c_str(), RegString(ir_[i].rs1).c_str(),
           RegString(ir_[i].rs2).c_str());
    switch (ir_[i].imm_type) {
      case IRInsr::kConst:
        printf("%ld\n", ir_[i].imm);
        break;
      case IRInsr::kLabel:
        printf(".L%ld\n", ir_[i].imm);
        break;
      case IRInsr::kData:
        printf(".D%ld\n", ir_[i].imm);
        break;
      case IRInsr::kRoundingMode:
        printf("%s\n", rv64::kRoundingModeName[ir_[i].imm].c_str());
        break;
    }
  }
  for (size_t i = 0; i < data_.size(); i++) {
    printf(".D%zu: ", i);
    switch (data_[i].index()) {
      case 0:
        printf(".hex ");
        for (uint8_t j : std::get<std::vector<uint8_t>>(data_[i])) {
          printf("%02x", j);
        }
        break;
      case 1:
        printf(".string \"%s\"", std::get<std::string>(data_[i]).c_str());
        break;
      case 2:
        printf(".zero %zu", std::get<size_t>(data_[i]));
        break;
    }
    puts("");
  }
  puts("");
}
#endif
