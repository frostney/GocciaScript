unit Goccia.Bytecode.OpCodeNames;

{$I Goccia.inc}

interface

uses
  Goccia.Bytecode;

function OpCodeName(const AOp: UInt8): string;

implementation

uses
  SysUtils;

function OpCodeName(const AOp: UInt8): string;
begin
  case TGocciaOpCode(AOp) of
    OP_LOAD_CONST:                     Result := 'OP_LOAD_CONST';
    OP_LOAD_TRUE:                      Result := 'OP_LOAD_TRUE';
    OP_LOAD_FALSE:                     Result := 'OP_LOAD_FALSE';
    OP_LOAD_INT:                       Result := 'OP_LOAD_INT';
    OP_MOVE:                           Result := 'OP_MOVE';
    OP_GET_LOCAL:                      Result := 'OP_GET_LOCAL';
    OP_SET_LOCAL:                      Result := 'OP_SET_LOCAL';
    OP_GET_UPVALUE:                    Result := 'OP_GET_UPVALUE';
    OP_SET_UPVALUE:                    Result := 'OP_SET_UPVALUE';
    OP_CLOSE_UPVALUE:                  Result := 'OP_CLOSE_UPVALUE';
    OP_JUMP:                           Result := 'OP_JUMP';
    OP_JUMP_IF_TRUE:                   Result := 'OP_JUMP_IF_TRUE';
    OP_JUMP_IF_FALSE:                  Result := 'OP_JUMP_IF_FALSE';
    OP_JUMP_IF_NULLISH:                Result := 'OP_JUMP_IF_NULLISH';
    OP_JUMP_IF_NOT_NULLISH:            Result := 'OP_JUMP_IF_NOT_NULLISH';
    OP_CLOSURE:                        Result := 'OP_CLOSURE';
    OP_PUSH_HANDLER:                   Result := 'OP_PUSH_HANDLER';
    OP_POP_HANDLER:                    Result := 'OP_POP_HANDLER';
    OP_THROW:                          Result := 'OP_THROW';
    OP_RETURN:                         Result := 'OP_RETURN';
    OP_GET_THIS_BINDING:               Result := 'OP_GET_THIS_BINDING';
    OP_LOAD_UNDEFINED:                 Result := 'OP_LOAD_UNDEFINED';
    OP_ARRAY_POP:                      Result := 'OP_ARRAY_POP';
    OP_NEW_ARRAY:                      Result := 'OP_NEW_ARRAY';
    OP_ARRAY_PUSH:                     Result := 'OP_ARRAY_PUSH';
    OP_ARRAY_GET:                      Result := 'OP_ARRAY_GET';
    OP_ARRAY_SET:                      Result := 'OP_ARRAY_SET';
    OP_NEW_OBJECT:                     Result := 'OP_NEW_OBJECT';
    OP_GET_PROP_CONST:                 Result := 'OP_GET_PROP_CONST';
    OP_SET_PROP_CONST:                 Result := 'OP_SET_PROP_CONST';
    OP_DELETE_PROP_CONST:              Result := 'OP_DELETE_PROP_CONST';
    OP_GET_LENGTH:                     Result := 'OP_GET_LENGTH';
    OP_ARG_COUNT:                      Result := 'OP_ARG_COUNT';
    OP_PACK_ARGS:                      Result := 'OP_PACK_ARGS';
    OP_NOP:                            Result := 'OP_NOP';
    OP_LINE:                           Result := 'OP_LINE';
    OP_ADD_INT:                        Result := 'OP_ADD_INT';
    OP_SUB_INT:                        Result := 'OP_SUB_INT';
    OP_MUL_INT:                        Result := 'OP_MUL_INT';
    OP_DIV_INT:                        Result := 'OP_DIV_INT';
    OP_MOD_INT:                        Result := 'OP_MOD_INT';
    OP_NEG_INT:                        Result := 'OP_NEG_INT';
    OP_ADD_FLOAT:                      Result := 'OP_ADD_FLOAT';
    OP_SUB_FLOAT:                      Result := 'OP_SUB_FLOAT';
    OP_MUL_FLOAT:                      Result := 'OP_MUL_FLOAT';
    OP_DIV_FLOAT:                      Result := 'OP_DIV_FLOAT';
    OP_MOD_FLOAT:                      Result := 'OP_MOD_FLOAT';
    OP_NEG_FLOAT:                      Result := 'OP_NEG_FLOAT';
    OP_EQ_INT:                         Result := 'OP_EQ_INT';
    OP_NEQ_INT:                        Result := 'OP_NEQ_INT';
    OP_LT_INT:                         Result := 'OP_LT_INT';
    OP_GT_INT:                         Result := 'OP_GT_INT';
    OP_LTE_INT:                        Result := 'OP_LTE_INT';
    OP_GTE_INT:                        Result := 'OP_GTE_INT';
    OP_CONCAT:                         Result := 'OP_CONCAT';
    OP_GET_GLOBAL:                     Result := 'OP_GET_GLOBAL';
    OP_SET_GLOBAL:                     Result := 'OP_SET_GLOBAL';
    OP_HAS_GLOBAL:                     Result := 'OP_HAS_GLOBAL';
    OP_CALL:                           Result := 'OP_CALL';
    OP_CALL_METHOD:                    Result := 'OP_CALL_METHOD';
    OP_CONSTRUCT:                      Result := 'OP_CONSTRUCT';
    OP_GET_ITER:                       Result := 'OP_GET_ITER';
    OP_ITER_NEXT:                      Result := 'OP_ITER_NEXT';
    OP_TO_STRING:                      Result := 'OP_TO_STRING';
    OP_NEW_CLASS:                      Result := 'OP_NEW_CLASS';
    OP_CLASS_SET_SUPER:                Result := 'OP_CLASS_SET_SUPER';
    OP_CLASS_ADD_METHOD_CONST:         Result := 'OP_CLASS_ADD_METHOD_CONST';
    OP_CLASS_SET_FIELD_INITIALIZER:    Result := 'OP_CLASS_SET_FIELD_INITIALIZER';
    OP_CLASS_DECLARE_PRIVATE_STATIC_CONST: Result := 'OP_CLASS_DECLARE_PRIVATE_STATIC_CONST';
    OP_CLASS_EXEC_STATIC_BLOCK:        Result := 'OP_CLASS_EXEC_STATIC_BLOCK';
    OP_TO_PRIMITIVE:                   Result := 'OP_TO_PRIMITIVE';
    OP_UNPACK:                         Result := 'OP_UNPACK';
    OP_LOAD_NULL:                      Result := 'OP_LOAD_NULL';
    OP_LOAD_HOLE:                      Result := 'OP_LOAD_HOLE';
    OP_CHECK_TYPE:                     Result := 'OP_CHECK_TYPE';
    OP_EQ_FLOAT:                       Result := 'OP_EQ_FLOAT';
    OP_NEQ_FLOAT:                      Result := 'OP_NEQ_FLOAT';
    OP_LT_FLOAT:                       Result := 'OP_LT_FLOAT';
    OP_GT_FLOAT:                       Result := 'OP_GT_FLOAT';
    OP_LTE_FLOAT:                      Result := 'OP_LTE_FLOAT';
    OP_GTE_FLOAT:                      Result := 'OP_GTE_FLOAT';
    OP_NOT:                            Result := 'OP_NOT';
    OP_TO_BOOL:                        Result := 'OP_TO_BOOL';
    OP_DEFINE_ACCESSOR_CONST:          Result := 'OP_DEFINE_ACCESSOR_CONST';
    OP_DEFINE_ACCESSOR_DYNAMIC:        Result := 'OP_DEFINE_ACCESSOR_DYNAMIC';
    OP_COLLECTION_OP:                  Result := 'OP_COLLECTION_OP';
    OP_VALIDATE_VALUE:                 Result := 'OP_VALIDATE_VALUE';
    OP_THROW_TYPE_ERROR_CONST:         Result := 'OP_THROW_TYPE_ERROR_CONST';
    OP_DEFINE_GLOBAL_CONST:            Result := 'OP_DEFINE_GLOBAL_CONST';
    OP_FINALIZE_ENUM:                  Result := 'OP_FINALIZE_ENUM';
    OP_SUPER_GET_CONST:                Result := 'OP_SUPER_GET_CONST';
    OP_SETUP_AUTO_ACCESSOR_CONST:      Result := 'OP_SETUP_AUTO_ACCESSOR_CONST';
    OP_BEGIN_DECORATORS:               Result := 'OP_BEGIN_DECORATORS';
    OP_APPLY_CLASS_DECORATOR:          Result := 'OP_APPLY_CLASS_DECORATOR';
    OP_FINISH_DECORATORS:              Result := 'OP_FINISH_DECORATORS';
    OP_APPLY_ELEMENT_DECORATOR_CONST:  Result := 'OP_APPLY_ELEMENT_DECORATOR_CONST';
    OP_EQ:                             Result := 'OP_EQ';
    OP_NEQ:                            Result := 'OP_NEQ';
    OP_LT:                             Result := 'OP_LT';
    OP_GT:                             Result := 'OP_GT';
    OP_LTE:                            Result := 'OP_LTE';
    OP_GTE:                            Result := 'OP_GTE';
    OP_TYPEOF:                         Result := 'OP_TYPEOF';
    OP_IS_INSTANCE:                    Result := 'OP_IS_INSTANCE';
    OP_HAS_PROPERTY:                   Result := 'OP_HAS_PROPERTY';
    OP_TO_NUMBER:                      Result := 'OP_TO_NUMBER';
    OP_NEG:                            Result := 'OP_NEG';
    OP_BNOT:                           Result := 'OP_BNOT';
    OP_GET_INDEX:                      Result := 'OP_GET_INDEX';
    OP_SET_INDEX:                      Result := 'OP_SET_INDEX';
    OP_DEL_INDEX:                      Result := 'OP_DEL_INDEX';
    OP_ADD:                            Result := 'OP_ADD';
    OP_SUB:                            Result := 'OP_SUB';
    OP_MUL:                            Result := 'OP_MUL';
    OP_DIV:                            Result := 'OP_DIV';
    OP_MOD:                            Result := 'OP_MOD';
    OP_POW:                            Result := 'OP_POW';
    OP_BAND:                           Result := 'OP_BAND';
    OP_BOR:                            Result := 'OP_BOR';
    OP_BXOR:                           Result := 'OP_BXOR';
    OP_SHL:                            Result := 'OP_SHL';
    OP_SHR:                            Result := 'OP_SHR';
    OP_USHR:                           Result := 'OP_USHR';
    OP_IMPORT:                         Result := 'OP_IMPORT';
    OP_EXPORT:                         Result := 'OP_EXPORT';
    OP_AWAIT:                          Result := 'OP_AWAIT';
    OP_IMPORT_META:                    Result := 'OP_IMPORT_META';
    OP_DYNAMIC_IMPORT:                 Result := 'OP_DYNAMIC_IMPORT';
    OP_USING_INIT:                     Result := 'OP_USING_INIT';
    OP_USING_DISPOSE:                  Result := 'OP_USING_DISPOSE';
    OP_YIELD:                          Result := 'OP_YIELD';
    OP_MATCH_VALUE:                    Result := 'OP_MATCH_VALUE';
    OP_MATCH_HAS_PROPERTY:             Result := 'OP_MATCH_HAS_PROPERTY';
  else
    Result := Format('OP_UNKNOWN_%d', [AOp]);
  end;
end;

end.
