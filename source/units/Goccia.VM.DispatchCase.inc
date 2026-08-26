{ Shared opcode case for ExecuteClosureRegistersInternal. Included once.
  Continue in this file is goto LDispatchNext so the dual prod/instrumented
  loop heads can share one case without duplicating it. }

          case TGocciaOpCode(Op) of
      OP_LOAD_CONST:
        begin
          Constant := Template.GetConstantUnchecked(DecodeBx(Instruction));
          case Constant.Kind of
            // Keep numeric constants in the VM's scalar representation.  The
            // previous ConstantToValue -> ValueToRegister round trip allocated
            // a short-lived boxed Number for every execution of the
            // instruction.
            bckInteger:
              FRegisters[A] := VMIntResult(Constant.IntValue);
            bckFloat:
              FRegisters[A] := RegisterFromDouble(Constant.FloatValue);
            // ES2026 §13.2.8.3: template objects are lazily built and cached.
            bckTemplateObject:
              FRegisters[A] := ValueToRegister(BuildTemplateObjectConstant(
                Template, DecodeBx(Instruction)));
            bckString:
              begin
                LeftValue := TGocciaValue(
                  Template.GetStringConstantCache(DecodeBx(Instruction)));
                if not Assigned(LeftValue) then
                begin
                  LeftValue := TGocciaStringLiteralValue.Create(
                    Constant.StringValue);
                  Template.SetStringConstantCache(
                    DecodeBx(Instruction), LeftValue);
                end;
                FRegisters[A] := RegisterObject(LeftValue);
              end;
          else
            FRegisters[A] := ValueToRegister(ConstantToValue(Constant));
          end;
        end;

      OP_LOAD_CHAR:
        if DecodeBx(Instruction) <= 127 then
          FRegisters[A] := RegisterObject(
            CachedASCIIStringValue(
              TASCIIStringCodeUnit(DecodeBx(Instruction))))
        else
          FRegisters[A] := RegisterObject(TGocciaStringLiteralValue.Create(
            UTF16CodeUnitImmediateToString(DecodeBx(Instruction))));

      OP_LOAD_REGEXP:
        FRegisters[A] := ValueToRegister(
          BuildRegExpLiteralConstant(Template, DecodeBx(Instruction)));

      OP_LOAD_UNDEFINED:
        FRegisters[A] := RegisterUndefined;

      OP_GET_THIS_BINDING:
        // ES2026 §9.4.3 ResolveThisBinding falls through to GetThisBinding
        // on the surrounding environment record.  At Script top level the
        // global env's [[GlobalThisValue]] is the global object; at Module
        // top level the module env's binding resolves to undefined.  The
        // active FGlobalScope already encodes that distinction (the
        // module loader rewires FGlobalScope to the module scope while
        // executing module bodies), so reading ThisValue here is correct
        // for both kinds without needing a compile-time flag.
        if Assigned(FGlobalScope) then
          FRegisters[A] := VMValueToRegisterFast(FGlobalScope.ThisValue)
        else
          FRegisters[A] := RegisterUndefined;

      OP_LOAD_TRUE:
        FRegisters[A] := RegisterBoolean(True);

      OP_LOAD_FALSE:
        FRegisters[A] := RegisterBoolean(False);

      OP_LOAD_NULL:
        FRegisters[A] := RegisterNull;

      OP_LOAD_HOLE:
        FRegisters[A] := RegisterHole;

      OP_CHECK_TYPE:
        VMStrictTypeCheckRegisterValue(GetRegister(A), TGocciaLocalType(B));

      OP_TO_PRIMITIVE:
      begin
        KeyIndex := DecodeBx(Instruction);
        if FRegisters[KeyIndex].Kind <> grkObject then
          FRegisters[A] := FRegisters[KeyIndex]
        else
          SetRegisterFast(A, ToPrimitive(GetRegisterFast(KeyIndex)));
      end;

      OP_TO_OBJECT:
        SetRegister(A, ToObject(GetRegister(B)));

      // ES2026 §7.1.19 ToPropertyKey(argument)
      OP_TO_PROPERTY_KEY:
        SetRegister(A, ToPropertyKey(RegisterToValue(FRegisters[B])));

      OP_ENUM_KEYS:
        SetRegister(A, ForInEntriesArray(GetRegister(B)));

      OP_ENUM_ENTRY:
      begin
        if TryForInEntryKey(GetRegister(C), ForInKey) then
        begin
          FRegisters[A] := VMValueToRegisterFast(
            TGocciaStringLiteralValue.Create(ForInKey));
          FRegisters[B] := RegisterBoolean(True);
        end
        else
        begin
          FRegisters[A] := RegisterUndefined;
          FRegisters[B] := RegisterBoolean(False);
        end;
      end;

      OP_LOAD_INT:
        FRegisters[A] := RegisterInt(DecodesBx(Instruction));

      OP_MOVE:
        SetRegisterRaw(A, FRegisters[B]);

      OP_GET_LOCAL:
      begin
        FRegisters[A] := GetLocalRegister(DecodeBx(Instruction));
        if FRegisters[A].Kind = grkHole then
          ThrowReferenceError('Cannot access lexical binding before initialization');
      end;

      OP_SET_LOCAL:
        SetLocalRaw(DecodeBx(Instruction), FRegisters[A]);

      OP_GET_UPVALUE:
      begin
        if Assigned(FCurrentClosure) then
        begin
          Desc := Template.GetUpvalueDescriptor(DecodeBx(Instruction));
          ResolvedDynamicVarScope := ResolveDynamicUpvalueScope(
            DecodeBx(Instruction), Desc.Name);
          if Assigned(ResolvedDynamicVarScope) then
          begin
            FRegisters[A] := VMValueToRegisterFast(
              ResolvedDynamicVarScope.GetValue(Desc.Name));
            goto LDispatchNext;
          end;

          Upvalue := FCurrentClosure.GetUpvalue(DecodeBx(Instruction));
          if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
          begin
            if Upvalue.Cell.Value.Kind = grkHole then
              ThrowReferenceError('Cannot access lexical binding before initialization');
            SetRegisterRaw(A, Upvalue.Cell.Value)
          end
          else
            FRegisters[A] := RegisterUndefined;
        end
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_SET_UPVALUE:
      begin
        if Assigned(FCurrentClosure) then
        begin
          Upvalue := FCurrentClosure.GetUpvalue(DecodeBx(Instruction));
          if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
          begin
            if Upvalue.Cell.Value.Kind = grkHole then
              ThrowReferenceError('Cannot access lexical binding before initialization');
            Upvalue.Cell.Value := FRegisters[A];
          end;
        end;
      end;

      OP_SET_UPVALUE_DYNAMIC:
      begin
        Desc := Template.GetUpvalueDescriptor(DecodeBx(Instruction));
        ResolvedDynamicVarScope := ResolveDynamicUpvalueScope(
          DecodeBx(Instruction), Desc.Name);
        if Assigned(ResolvedDynamicVarScope) then
          ResolvedDynamicVarScope.AssignBinding(Desc.Name,
            RegisterToValue(FRegisters[A]))
        else if Assigned(FCurrentClosure) then
        begin
          Upvalue := FCurrentClosure.GetUpvalue(DecodeBx(Instruction));
          if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
          begin
            if Upvalue.Cell.Value.Kind = grkHole then
              ThrowReferenceError(
                'Cannot access lexical binding before initialization');
            Upvalue.Cell.Value := FRegisters[A];
          end;
        end;
      end;

      OP_RESOLVE_UPVALUE_REF:
      begin
        Desc := Template.GetUpvalueDescriptor(B);
        ResolvedDynamicVarScope := ResolveDynamicUpvalueScope(B, Desc.Name);
        if Assigned(ResolvedDynamicVarScope) then
          SetRegister(A, TGocciaResolvedEnvironmentReferenceValue.Create(
            ResolvedDynamicVarScope, C <> 0))
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_SET_UPVALUE_REF:
      begin
        Desc := Template.GetUpvalueDescriptor(C);
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is
             TGocciaResolvedEnvironmentReferenceValue) then
        begin
          ResolvedEnvironmentReference :=
            TGocciaResolvedEnvironmentReferenceValue(
              FRegisters[B].ObjectValue);
          ResolvedEnvironmentReference.Scope.SetOwnMutableBinding(Desc.Name,
            RegisterToValue(FRegisters[A]),
            ResolvedEnvironmentReference.Strict);
        end
        else if Assigned(FCurrentClosure) then
        begin
          Upvalue := FCurrentClosure.GetUpvalue(C);
          if Assigned(Upvalue) and Assigned(Upvalue.Cell) then
          begin
            if Upvalue.Cell.Value.Kind = grkHole then
              ThrowReferenceError(
                'Cannot access lexical binding before initialization');
            Upvalue.Cell.Value := FRegisters[A];
          end;
        end;
      end;

      OP_SET_GLOBAL_STATIC:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) and
           not FGlobalScope.TryAssignExistingBinding(GlobalName,
             RegisterToValue(FRegisters[A])) then
          ThrowReferenceError(Format(SErrorUndefinedVariable, [GlobalName]));
      end;

      OP_CLOSE_UPVALUE:
      begin
        KeyIndex := DecodeBx(Instruction);
        if KeyIndex < FLocalCellCount then
          FLocalCells[KeyIndex] := nil;
      end;

      OP_ARG_COUNT:
        FRegisters[A] := RegisterInt(FArgCount);

      OP_LOAD_ARGUMENT:
        if (B < FArgCount) then
          SetRegisterRaw(A, FArguments[B])
        else
          FRegisters[A] := RegisterUndefined;

      OP_CHECK_DERIVED_THIS:
        if not FCurrentConstructorSuperCalled then
          ThrowReferenceError(
            SErrorSuperConstructorNotCalled);

      OP_CREATE_ARGUMENTS:
        SetRegister(A, CreateArgumentsObjectFromCurrentFrame(B <> 0, C));

      OP_PACK_ARGS:
      begin
        ArgsArray := TGocciaArrayValue.Create;
        for I := B to FArgCount - 1 do
          ArgsArray.Elements.Add(RegisterToValue(FArguments[I]));
        FRegisters[A] := RegisterObject(ArgsArray);
      end;

      OP_JUMP:
      begin
        JumpOffset := DecodeAx(Instruction);
        Inc(Frame.IP, JumpOffset);
        if JumpOffset < 0 then
          CheckExecutionTimeout;
      end;

      OP_JUMP_IF_TRUE:
        if RegisterToBoolean(FRegisters[A]) then
        begin
          if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(InstructionStartIP),
              Template.DebugInfo.GetColumnForPC(InstructionStartIP), 0);
          JumpOffset := DecodesBx(Instruction);
          Inc(Frame.IP, JumpOffset);
          if JumpOffset < 0 then
            CheckExecutionTimeout;
        end
        else if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(InstructionStartIP),
            Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);

      OP_JUMP_IF_FALSE:
        if not RegisterToBoolean(FRegisters[A]) then
        begin
          if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(InstructionStartIP),
              Template.DebugInfo.GetColumnForPC(InstructionStartIP), 0);
          JumpOffset := DecodesBx(Instruction);
          Inc(Frame.IP, JumpOffset);
          if JumpOffset < 0 then
            CheckExecutionTimeout;
        end
        else if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(InstructionStartIP),
            Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);

      OP_JUMP_IF_NUM_NOT_LTE_IMM:
        begin
          if FRegisters[A].Kind = grkInt then
            NumericComparisonResult := FRegisters[A].IntValue <= Int16(B)
          else if FRegisters[A].Kind = grkFloat then
            NumericComparisonResult := FRegisters[A].FloatValue <= Int16(B)
          else
            raise Exception.Create(
              'Invalid non-numeric source for OP_JUMP_IF_NUM_NOT_LTE_IMM');
          if not NumericComparisonResult then
          begin
            if FCoverageEnabled and
               (TGocciaCoverageTracker.Instance <> nil) and
               Assigned(Template.DebugInfo) then
              TGocciaCoverageTracker.Instance.RecordBranchHit(
                Template.DebugInfo.SourceFile,
                Template.DebugInfo.GetLineForPC(InstructionStartIP),
                Template.DebugInfo.GetColumnForPC(InstructionStartIP), 0);
            JumpOffset := Int16(C);
            Inc(Frame.IP, JumpOffset);
            if JumpOffset < 0 then
              CheckExecutionTimeout;
          end
          else if FCoverageEnabled and
                  (TGocciaCoverageTracker.Instance <> nil) and
                  Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(InstructionStartIP),
              Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);
        end;

      OP_JUMP_IF_NOT_LT:
        begin
          if (FRegisters[A].Kind = grkInt) and (FRegisters[B].Kind = grkInt) then
          begin
            if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
            NumericComparisonResult := FRegisters[A].IntValue <
              FRegisters[B].IntValue;
          end
          else if RegisterIsNumericScalar(FRegisters[A]) and
                  RegisterIsNumericScalar(FRegisters[B]) then
          begin
            if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
            NumericComparisonResult := RegisterToDouble(FRegisters[A]) <
              RegisterToDouble(FRegisters[B]);
          end
          else
          begin
            if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
            LeftValue := GetRegisterFast(A);
            RightValue := GetRegisterFast(B);
            if (LeftValue is TGocciaStringLiteralValue) and
               (RightValue is TGocciaStringLiteralValue) then
              NumericComparisonResult :=
                Goccia.Arithmetic.CompareStringValues(
                  TGocciaStringLiteralValue(LeftValue).Value,
                  TGocciaStringLiteralValue(RightValue).Value) < 0
            else
              NumericComparisonResult := VMRootedBinaryPredicate(
                @Goccia.Arithmetic.LessThan, LeftValue, RightValue);
          end;
          if not NumericComparisonResult then
          begin
            if FCoverageEnabled and
               (TGocciaCoverageTracker.Instance <> nil) and
               Assigned(Template.DebugInfo) then
              TGocciaCoverageTracker.Instance.RecordBranchHit(
                Template.DebugInfo.SourceFile,
                Template.DebugInfo.GetLineForPC(InstructionStartIP),
                Template.DebugInfo.GetColumnForPC(InstructionStartIP), 0);
            JumpOffset := Int16(C);
            Inc(Frame.IP, JumpOffset);
            if JumpOffset < 0 then
              CheckExecutionTimeout;
          end
          else if FCoverageEnabled and
                  (TGocciaCoverageTracker.Instance <> nil) and
                  Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(InstructionStartIP),
              Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);
        end;

      OP_JUMP_IF_NULLISH:
        if RegisterMatchesNullishKind(FRegisters[A], B) then
        begin
          if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(InstructionStartIP),
              Template.DebugInfo.GetColumnForPC(InstructionStartIP), 0);
          Inc(Frame.IP, C);
        end
        else if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(InstructionStartIP),
            Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);

      OP_JUMP_IF_NOT_NULLISH:
        if not RegisterMatchesNullishKind(FRegisters[A], B) then
        begin
          if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(InstructionStartIP),
              Template.DebugInfo.GetColumnForPC(InstructionStartIP), 0);
          Inc(Frame.IP, C);
        end
        else if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(InstructionStartIP),
            Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);

      OP_PUSH_HANDLER:
        FHandlerStack.Push(Frame.IP + DecodeBx(Instruction), A, FFrameDepth);

      OP_PUSH_FINALLY_HANDLER:
        FHandlerStack.Push(Frame.IP + DecodeBx(Instruction), A, FFrameDepth,
          bhkFinally);

      OP_POP_HANDLER:
        if not FHandlerStack.IsEmpty then
          FHandlerStack.Pop;

      OP_ADD_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue +
            FRegisters[C].IntValue)
        else
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) +
            RegisterToDouble(FRegisters[C]));

      OP_ADD_FLOAT:
        FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) +
          RegisterToDouble(FRegisters[C]));

      OP_SUB_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue -
            FRegisters[C].IntValue)
        else
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) -
            RegisterToDouble(FRegisters[C]));

      OP_SUB_FLOAT:
        FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) -
          RegisterToDouble(FRegisters[C]));

      OP_SUB_NUM_IMM:
        if FRegisters[B].Kind = grkInt then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue - Int16(C))
        else if FRegisters[B].Kind = grkFloat then
          FRegisters[A] := VMNumberRegister(FRegisters[B].FloatValue - Int16(C))
        else
          raise Exception.Create(
            'Invalid non-numeric source for OP_SUB_NUM_IMM');

      OP_ADD_NUM_IMM:
        if FRegisters[B].Kind = grkInt then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue + Int16(C))
        else if FRegisters[B].Kind = grkFloat then
          FRegisters[A] := VMNumberRegister(FRegisters[B].FloatValue + Int16(C))
        else
          raise Exception.Create(
            'Invalid non-numeric source for OP_ADD_NUM_IMM');

      OP_MUL_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue *
            FRegisters[C].IntValue)
        else
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) *
            RegisterToDouble(FRegisters[C]));

      OP_MUL_FLOAT:
        FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) *
          RegisterToDouble(FRegisters[C]));

      OP_DIV_INT, OP_DIV_FLOAT:
        FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) /
          RegisterToDouble(FRegisters[C]));

      OP_MOD_INT, OP_MOD_FLOAT:
        FRegisters[A] := VMModuloRegister(RegisterToDouble(FRegisters[B]),
          RegisterToDouble(FRegisters[C]));

      OP_EQ_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue = FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) = RegisterToDouble(FRegisters[C]));

      OP_EQ_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) = RegisterToDouble(FRegisters[C]));

      OP_NEQ_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue <> FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) <> RegisterToDouble(FRegisters[C]));

      OP_NEQ_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) <> RegisterToDouble(FRegisters[C]));

      OP_LT_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue < FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) < RegisterToDouble(FRegisters[C]));

      OP_LT_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) < RegisterToDouble(FRegisters[C]));

      OP_GT_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue > FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) > RegisterToDouble(FRegisters[C]));

      OP_GT_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) > RegisterToDouble(FRegisters[C]));

      OP_LTE_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue <= FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) <= RegisterToDouble(FRegisters[C]));

      OP_LTE_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) <= RegisterToDouble(FRegisters[C]));

      OP_GTE_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue >= FRegisters[C].IntValue)
        else
          FRegisters[A] := RegisterBoolean(
            RegisterToDouble(FRegisters[B]) >= RegisterToDouble(FRegisters[C]));

      OP_GTE_FLOAT:
        FRegisters[A] := RegisterBoolean(
          RegisterToDouble(FRegisters[B]) >= RegisterToDouble(FRegisters[C]));

      OP_NEG_INT, OP_NEG_FLOAT:
        FRegisters[A] := VMNumberRegister(-RegisterToDouble(FRegisters[B]));

      OP_CONCAT:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaStringLiteralValue) and
           (FRegisters[C].Kind = grkObject) and
           (FRegisters[C].ObjectValue is TGocciaStringLiteralValue) then
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            TGocciaStringLiteralValue(FRegisters[B].ObjectValue).Value +
            TGocciaStringLiteralValue(FRegisters[C].ObjectValue).Value))
        else
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            VMRegisterToStringFast(FRegisters[B]).Value +
            VMRegisterToStringFast(FRegisters[C]).Value));
      end;

      OP_NEW_ARRAY:
        SetRegister(A, TGocciaArrayValue.Create(nil, B));

      OP_ARRAY_POP:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaArrayValue) then
        begin
          if TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count = 0 then
            FRegisters[A] := RegisterUndefined
          else
          begin
            FRegisters[A] := VMValueToRegisterFast(TGocciaArrayValue(
              FRegisters[B].ObjectValue).Elements[
                TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count - 1]);
            TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Delete(
              TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count - 1);
            if FRegisters[A].Kind = grkHole then
              FRegisters[A] := RegisterUndefined;
          end;
        end
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_ARRAY_PUSH:
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaArrayValue) then
          TGocciaArrayValue(FRegisters[A].ObjectValue).Elements.Add(
            RegisterToValue(FRegisters[B]));

      OP_ARRAY_GET:
        ExecGetComputedProperty(A, FRegisters[B], FRegisters[C],
          ELEMENT_GET_OPTIONS);

      OP_ARRAY_SET:
        ExecSetComputedProperty(A, FRegisters[B], FRegisters[C],
          ELEMENT_SET_OPTIONS);

      OP_GET_LENGTH:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaArrayValue) then
          FRegisters[A] := VMNumberRegister(
            TGocciaArrayValue(FRegisters[B].ObjectValue).GetLength)
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaStringLiteralValue) then
          FRegisters[A] := VMNumberRegister(UTF16CodeUnitLength(
            TGocciaStringLiteralValue(FRegisters[B].ObjectValue).Value))
        else
          FRegisters[A] := RegisterInt(0);
      end;

      OP_NEW_OBJECT:
      begin
        if TGocciaObjectValue.SharedObjectPrototype = nil then
          TGocciaObjectValue.InitializeSharedPrototype;
        FRegisters[A] := RegisterObject(TGocciaVMLiteralObjectValue.Create(
          TGocciaObjectValue.SharedObjectPrototype,
          DecodeBx(Instruction)));
      end;

      OP_NEW_CLASS:
      begin
        FRegisters[A] := RegisterObject(TGocciaVMClassValue.Create(Self,
          Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue, nil));
        TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.DefineProperty(
          PROP_CONSTRUCTOR, TGocciaPropertyDescriptorData.Create(
            FRegisters[A].ObjectValue, [pfConfigurable, pfWritable]));
      end;

      OP_SET_CLASS_SOURCE_CONST:
      begin
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaClassValue) then
          TGocciaClassValue(FRegisters[A].ObjectValue).SetSourceText(
            Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue);
      end;

      OP_CLASS_SET_SUPER:
      begin
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) and
           (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaClassValue) then
        begin
          TGocciaVMClassValue(FRegisters[A].ObjectValue).SuperClass :=
            TGocciaClassValue(FRegisters[B].ObjectValue);
          TGocciaVMClassValue(FRegisters[A].ObjectValue).NativeSuperConstructor :=
            nil;
          // Set [[Prototype]] of derived class constructor to superclass
          TGocciaVMClassValue(FRegisters[A].ObjectValue).SetConstructorPrototype(
            TGocciaObjectValue(FRegisters[B].ObjectValue));
          // Set .prototype chain: DerivedClass.prototype.[[Prototype]] = SuperClass.prototype
          TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.Prototype :=
            TGocciaClassValue(FRegisters[B].ObjectValue).Prototype;
        end
        else if (FRegisters[A].Kind = grkObject) and
                (FRegisters[A].ObjectValue is TGocciaVMClassValue) and
                (FRegisters[B].Kind = grkNull) then
        begin
          TGocciaVMClassValue(FRegisters[A].ObjectValue).SuperClass := nil;
          TGocciaVMClassValue(FRegisters[A].ObjectValue).NativeSuperConstructor :=
            TGocciaFunctionBase.GetSharedPrototype;
          TGocciaVMClassValue(FRegisters[A].ObjectValue).SetConstructorPrototype(
            TGocciaFunctionBase.GetSharedPrototype);
          TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.Prototype := nil;
        end
        else if (FRegisters[A].Kind = grkObject) and
                (FRegisters[A].ObjectValue is TGocciaVMClassValue) and
                (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaObjectValue) and
                FRegisters[B].ObjectValue.IsConstructable then
        begin
          // Native constructor superclass: preserve static and prototype
          // inheritance links, and remember the constructor for instantiation.
          TGocciaVMClassValue(FRegisters[A].ObjectValue).LinkNativeSuperConstructor(
            TGocciaObjectValue(FRegisters[B].ObjectValue));
          RightValue := FRegisters[B].ObjectValue.GetProperty(PROP_PROTOTYPE);
          if RightValue is TGocciaNullLiteralValue then
            TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.Prototype := nil
          else if RightValue is TGocciaObjectValue then
            TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.Prototype :=
              TGocciaObjectValue(RightValue)
          else
            ThrowTypeError(
              'Superclass prototype must be an object or null',
              'set the superclass prototype property to an object or null');
        end
        else if (FRegisters[A].Kind = grkObject) and
                (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
          ThrowTypeError(Format(SErrorValueNotConstructor,
            [RegisterToValue(FRegisters[B]).TypeName]),
            SSuggestNotConstructorType);
      end;

      OP_CLASS_ADD_METHOD_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(B).StringValue;
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
        begin
          if IsBytecodePrivateKey(GlobalName) then
            DeclareBytecodePrivateNameForClass(
              FRegisters[A].ObjectValue, GlobalName);
          SetBytecodeHomeObject(RegisterToValue(FRegisters[C]),
            FRegisters[A].ObjectValue);
          if GlobalName = PROP_CONSTRUCTOR then
          begin
            TGocciaVMClassValue(FRegisters[A].ObjectValue).SetVMConstructor(
              RegisterToValue(FRegisters[C]));
          end
          else
            // ES §14.3.7: class prototype methods are non-enumerable
            TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype.DefineProperty(
              GlobalName, TGocciaPropertyDescriptorData.Create(
                RegisterToValue(FRegisters[C]), [pfConfigurable, pfWritable]));
        end
        else if (FRegisters[A].Kind = grkObject) and Assigned(FRegisters[A].ObjectValue) then
          SetPropertyValue(FRegisters[A].ObjectValue, GlobalName, RegisterToValue(FRegisters[C]))
        else
          SetPropertyValue(GetRegister(A), GlobalName, GetRegister(C));
      end;

      OP_CLASS_SET_FIELD_INITIALIZER:
      begin
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
        begin
          SetBytecodeHomeObject(RegisterToValue(FRegisters[B]),
            FRegisters[A].ObjectValue);
          if RegisterToValue(FRegisters[B]) is TGocciaBytecodeFunctionValue then
            DeclareBytecodePrivateNamesFromTemplate(
              FRegisters[A].ObjectValue,
              TGocciaBytecodeFunctionValue(RegisterToValue(FRegisters[B]))
                .FClosure.Template);
          TGocciaVMClassValue(FRegisters[A].ObjectValue).SetMethodInitializers(
            [RegisterToValue(FRegisters[B])]);
        end;
      end;

      OP_CLASS_DECLARE_PRIVATE_STATIC_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(B).StringValue;
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
        begin
          DeclareBytecodePrivateNameForClass(FRegisters[A].ObjectValue,
            GlobalName, True);
          TGocciaVMClassValue(FRegisters[A].ObjectValue).AddPrivateStaticProperty(
            BytecodePrivateRuntimeKey(GlobalName,
              TGocciaVMClassValue(FRegisters[A].ObjectValue)
                .PrivateBrandToken),
            TGocciaUndefinedLiteralValue.UndefinedValue);
        end;
      end;

      // ES2022 §15.7.14: execute static block closure with this = class
      OP_CLASS_EXEC_STATIC_BLOCK:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaBytecodeFunctionValue) then
        begin
          SetBytecodeHomeObject(RegisterToValue(FRegisters[B]),
            FRegisters[A].ObjectValue);
          PushFrame(B, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
          SetupNewFrame(
            TGocciaBytecodeFunctionValue(FRegisters[B].ObjectValue).FClosure,
            FRegisters[A], TGocciaRegisterArray(nil), 0,
            RegisterUndefined, RegisterUndefined, RegisterUndefined, True, True,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
          goto LDispatchNext;
        end
        else if (FRegisters[B].Kind = grkObject) and
                Assigned(FRegisters[B].ObjectValue) and
                FRegisters[B].ObjectValue.IsCallable then
        begin
          CallArgs := AcquireArguments(0);
          try
            InvokeFunctionValue(RegisterToValue(FRegisters[B]),
              CallArgs, RegisterToValue(FRegisters[A]));
          finally
            ReleaseArguments(CallArgs);
          end;
        end;
      end;

      OP_GET_LOCAL_PROP_CONST:
      begin
        FRegisters[A] := GetLocalRegister(B);
        if FRegisters[A].Kind = grkHole then
          ThrowReferenceError('Cannot access lexical binding before initialization');
        B := A;
        goto LGetPropConstShared;
      end;

      OP_GET_PROP_CONST:
      LGetPropConstShared:
        if (FRegisters[B].Kind = grkObject) and Assigned(FRegisters[B].ObjectValue) and
           (FRegisters[B].ObjectValue is TGocciaObjectValue) then
        begin
          // Hot shape: per-site inline cache keyed by the name-constant
          // index, validated against (own-map identity, map entry version).
          // Hits and fills serve only own plain data properties on
          // ordinary-lookup receivers; everything else degrades to the
          // generic GetPropertyValue path. Sites whose MissStreak saturated
          // are megamorphic: they skip the cache and use the uncached
          // own-data fast path. A nil slot (out-of-range constant index in
          // corrupt bytecode) runs fully uncached.
          PropertyReadCache := Template.PropertyReadCacheSlot(C);
          if Assigned(PropertyReadCache) and
             (PropertyReadCache^.MissStreak <
              PROPERTY_READ_CACHE_POLYMORPHIC_LIMIT) and
             VMPropertyReadCacheableReceiver(FRegisters[B].ObjectValue) and
             VMTryGetCachedOwnDataProperty(
               TGocciaObjectValue(FRegisters[B].ObjectValue),
               PropertyReadCache, GlobalBindingValue) then
            SetRegisterFast(A, GlobalBindingValue)
          else
          begin
            ProtoReadCache := Template.ProtoReadCacheSlot(C);
            if Assigned(ProtoReadCache) and
               (ProtoReadCache^.MissStreak <
                PROPERTY_READ_CACHE_POLYMORPHIC_LIMIT) and
               VMTryGetCachedProtoProperty(
                 TGocciaObjectValue(FRegisters[B].ObjectValue),
                 ProtoReadCache, GlobalBindingValue) then
              SetRegisterFast(A, GlobalBindingValue)
            else
            begin
              GlobalName := Template.GetConstantUnchecked(C).StringValue;
              if VMPropertyReadCacheableReceiver(FRegisters[B].ObjectValue) and
                 (not IsBytecodePrivateKey(GlobalName)) then
              begin
                // One own-map probe establishes own-data / own-non-data /
                // absent; no fallback tier re-hashes the same name on this
                // receiver.
                case VMProbeOwnProperty(
                  TGocciaObjectValue(FRegisters[B].ObjectValue), GlobalName,
                  KeyIndex, PrivateDescriptor) of
                  oppData:
                  // A not-yet-materialized lazy descriptor (the only
                  // TGocciaPropertyDescriptorData subclass) must not be read raw
                  // or cached here: route its first touch through
                  // GetPropertyValue, which materializes it and replaces the
                  // entry in place with a plain descriptor so later reads cache
                  // normally.
                  if PrivateDescriptor.ClassType =
                     TGocciaPropertyDescriptorData then
                  begin
                    if Assigned(PropertyReadCache) and
                       (PropertyReadCache^.MissStreak <
                        PROPERTY_READ_CACHE_POLYMORPHIC_LIMIT) then
                      VMPrimeOwnPropertyCache(
                        TGocciaObjectValue(FRegisters[B].ObjectValue),
                        KeyIndex, PropertyReadCache);
                    SetRegisterFast(A,
                      TGocciaPropertyDescriptorData(PrivateDescriptor).Value);
                  end
                  else
                    SetRegister(A, GetPropertyValue(
                      FRegisters[B].ObjectValue, GlobalName));
                  oppNonData:
                  begin
                    // Accessor/exotic own descriptor: never cacheable here;
                    // converge the own tier toward dormant.
                    if Assigned(PropertyReadCache) and
                       (PropertyReadCache^.MissStreak <
                        PROPERTY_READ_CACHE_POLYMORPHIC_LIMIT) then
                      Inc(PropertyReadCache^.MissStreak);
                    ServeOwnNonDataProperty(A, FRegisters[B].ObjectValue,
                      PrivateDescriptor);
                  end;
                else
                  // oppAbsent: own absence is established, so the proto
                  // fill may skip its own re-probe (see the core's
                  // contract); deeper or exotic resolutions stay generic.
                  if Assigned(ProtoReadCache) and
                     (ProtoReadCache^.MissStreak <
                      PROPERTY_READ_CACHE_POLYMORPHIC_LIMIT) and
                     VMFillProtoReadCache(
                       TGocciaObjectValue(FRegisters[B].ObjectValue),
                       GlobalName, ProtoReadCache, GlobalBindingValue) then
                    SetRegisterFast(A, GlobalBindingValue)
                  else
                    SetRegister(A, GetPropertyValue(FRegisters[B].ObjectValue,
                      GlobalName));
                end;
              end
              else
                SetRegister(A, GetPropertyValue(FRegisters[B].ObjectValue,
                  GlobalName));
            end;
          end;
        end
        else if (FRegisters[B].Kind = grkObject) and
                Assigned(FRegisters[B].ObjectValue) then
          SetRegister(A, GetPropertyValue(FRegisters[B].ObjectValue,
            Template.GetConstantUnchecked(C).StringValue))
        else
          SetRegister(A, GetPropertyValue(GetRegister(B),
            Template.GetConstantUnchecked(C).StringValue));

      OP_SET_PROP_CONST:
        if (FRegisters[A].Kind = grkObject) and Assigned(FRegisters[A].ObjectValue) then
        begin
          RightValue := RegisterToValue(FRegisters[C]);
          if FRegisters[A].ObjectValue is TGocciaVMClassValue then
            SetBytecodeHomeObject(RightValue,
              RegisterToValue(FRegisters[A]));
          // Own writable-data write IC: (shape, entry index), same receiver
          // gate as the read cache. Hits skip the name hash. Accessors,
          // proxies, private fields, deletion, and non-writable descriptors
          // fall through to SetPropertyValue / AssignProperty.
          PropertyWriteCache := Template.PropertyWriteCacheSlot(B);
          if not (Assigned(PropertyWriteCache) and
             (PropertyWriteCache^.MissStreak <
              PROPERTY_WRITE_CACHE_POLYMORPHIC_LIMIT) and
             VMPropertyReadCacheableReceiver(FRegisters[A].ObjectValue) and
             VMTrySetCachedOwnWritableDataProperty(
               TGocciaObjectValue(FRegisters[A].ObjectValue),
               PropertyWriteCache, RightValue)) then
          begin
            GlobalName := Template.GetConstantUnchecked(B).StringValue;
            if IsBytecodePrivateKey(GlobalName) then
              SetPropertyValue(FRegisters[A].ObjectValue, GlobalName, RightValue)
            else if VMPropertyReadCacheableReceiver(FRegisters[A].ObjectValue) then
            begin
              if Assigned(PropertyWriteCache) and
                 (PropertyWriteCache^.MissStreak <
                  PROPERTY_WRITE_CACHE_POLYMORPHIC_LIMIT) then
              begin
                case VMProbeOwnProperty(
                  TGocciaObjectValue(FRegisters[A].ObjectValue), GlobalName,
                  KeyIndex, PrivateDescriptor) of
                  oppData:
                  if (PrivateDescriptor.ClassType =
                      TGocciaPropertyDescriptorData) and
                     PrivateDescriptor.Writable then
                  begin
                    TGocciaPropertyDescriptorData(PrivateDescriptor).Value :=
                      RightValue;
                    VMPrimeOwnPropertyWriteCache(
                      TGocciaObjectValue(FRegisters[A].ObjectValue),
                      KeyIndex, PropertyWriteCache);
                  end
                  else
                    SetPropertyValue(FRegisters[A].ObjectValue, GlobalName,
                      RightValue);
                  oppNonData:
                  begin
                    Inc(PropertyWriteCache^.MissStreak);
                    SetPropertyValue(FRegisters[A].ObjectValue, GlobalName,
                      RightValue);
                  end;
                else
                  if FRegisters[A].ObjectValue is TGocciaVMLiteralObjectValue then
                  begin
                    if not TGocciaVMLiteralObjectValue(
                      FRegisters[A].ObjectValue)
                      .TrySetLiteralDataPropertyFast(GlobalName, RightValue) then
                      SetPropertyValue(FRegisters[A].ObjectValue, GlobalName,
                        RightValue);
                  end
                  else
                    SetPropertyValue(FRegisters[A].ObjectValue, GlobalName,
                      RightValue);
                end;
              end
              else if not VMTrySetOwnWritableDataProperty(
                TGocciaObjectValue(FRegisters[A].ObjectValue), GlobalName,
                RightValue) then
              begin
                if FRegisters[A].ObjectValue is TGocciaVMLiteralObjectValue then
                begin
                  if not TGocciaVMLiteralObjectValue(FRegisters[A].ObjectValue)
                    .TrySetLiteralDataPropertyFast(GlobalName, RightValue) then
                    SetPropertyValue(FRegisters[A].ObjectValue, GlobalName,
                      RightValue);
                end
                else
                  SetPropertyValue(FRegisters[A].ObjectValue, GlobalName,
                    RightValue);
              end;
            end
            else
              SetPropertyValue(FRegisters[A].ObjectValue, GlobalName, RightValue);
          end;
        end
        else
          SetPropertyValue(GetRegister(A),
            Template.GetConstantUnchecked(B).StringValue,
            GetRegister(C));

      OP_SET_PROP_CONST_LOOSE:
      begin
        GlobalName := Template.GetConstantUnchecked(B).StringValue;
        RightValue := RegisterToValue(FRegisters[C]);
        TargetValue := GetRegister(A);
        if (TargetValue is TGocciaClassValue) or
           (TargetValue is TGocciaObjectValue) then
          SetBytecodeHomeObject(RightValue, TargetValue);
        SetPropertyValueLoose(TargetValue, GlobalName, RightValue);
      end;

      OP_DEFINE_STATIC_PROP_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(B).StringValue;
        RightValue := RegisterToValue(FRegisters[C]);
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaObjectValue) then
        begin
          if FRegisters[A].ObjectValue is TGocciaVMClassValue then
            SetBytecodeHomeObject(RightValue, RegisterToValue(FRegisters[A]),
              True);
          if IsBytecodePrivateKey(GlobalName) then
          begin
            if (FRegisters[A].ObjectValue is TGocciaInstanceValue) then
            begin
              if (not TGocciaInstanceValue(FRegisters[A].ObjectValue)
                    .TryGetRawPrivateProperty(GlobalName, TargetValue)) and
                 (not TGocciaInstanceValue(FRegisters[A].ObjectValue)
                    .Extensible) then
                ThrowTypeError(
                  'Cannot add private elements to a non-extensible object',
                  SSuggestObjectNotExtensible);
            end
            else if (FRegisters[A].ObjectValue is TGocciaObjectValue) and
                    (not TryGetRawObjectPrivateDescriptor(
                      TGocciaObjectValue(FRegisters[A].ObjectValue),
                      GlobalName, PrivateDescriptor)) and
                    (not TGocciaObjectValue(FRegisters[A].ObjectValue)
                      .Extensible) then
              ThrowTypeError(
                'Cannot add private elements to a non-extensible object',
                SSuggestObjectNotExtensible);
            SetRawPrivateValue(FRegisters[A].ObjectValue, GlobalName,
              RightValue);
            goto LDispatchNext;
          end;
          TGocciaObjectValue(FRegisters[A].ObjectValue).DefineProperty(
            GlobalName,
            TGocciaPropertyDescriptorData.Create(
              RightValue, [pfEnumerable, pfConfigurable, pfWritable]));
        end
        else
          SetPropertyValue(GetRegister(A), GlobalName, RightValue);
      end;

      OP_DEFINE_STATIC_PROP_DYNAMIC:
      begin
        RightValue := RegisterToValue(FRegisters[C]);
        TargetValue := GetRegister(A);
        if TargetValue is TGocciaObjectValue then
        begin
          PropKey := ClassifyPropertyKey(FRegisters[B], False);
          if TargetValue is TGocciaVMClassValue then
            SetBytecodeHomeObject(RightValue, TargetValue, True);

          if PropKey.Kind = pkkSymbol then
            TGocciaObjectValue(TargetValue).DefineSymbolProperty(
              PropKey.Symbol,
              TGocciaPropertyDescriptorData.Create(
                RightValue, [pfEnumerable, pfConfigurable, pfWritable]))
          else
          begin
            GlobalName := PropertyKeyName(PropKey);
            if IsBytecodePrivateKey(GlobalName) then
            begin
              if TargetValue is TGocciaInstanceValue then
              begin
                if (not TGocciaInstanceValue(TargetValue)
                      .TryGetRawPrivateProperty(GlobalName, LeftValue)) and
                   (not TGocciaInstanceValue(TargetValue).Extensible) then
                  ThrowTypeError(
                    'Cannot add private elements to a non-extensible object',
                    SSuggestObjectNotExtensible);
              end
              else if (not TryGetRawObjectPrivateDescriptor(
                       TGocciaObjectValue(TargetValue), GlobalName,
                       PrivateDescriptor)) and
                      (not TGocciaObjectValue(TargetValue).Extensible) then
                ThrowTypeError(
                  'Cannot add private elements to a non-extensible object',
                  SSuggestObjectNotExtensible);
              SetRawPrivateValue(TargetValue, GlobalName, RightValue);
              goto LDispatchNext;
            end;

            TGocciaObjectValue(TargetValue).DefineProperty(
              GlobalName,
              TGocciaPropertyDescriptorData.Create(
                RightValue, [pfEnumerable, pfConfigurable, pfWritable]));
          end;
        end
        else
          SetPropertyValue(TargetValue,
            KeyToPropertyNameRegister(FRegisters[B]), RightValue);
      end;

      OP_DEFINE_PROP_DYNAMIC:
      begin
        RightValue := RegisterToValue(FRegisters[C]);
        TargetValue := GetRegister(A);
        if TargetValue is TGocciaObjectValue then
        begin
          PropKey := ClassifyPropertyKey(FRegisters[B], False);
          if PropKey.Kind = pkkSymbol then
            TGocciaObjectValue(TargetValue).DefineSymbolProperty(
              PropKey.Symbol,
              TGocciaPropertyDescriptorData.Create(
                RightValue, [pfEnumerable, pfConfigurable, pfWritable]))
          else
            TGocciaObjectValue(TargetValue).DefineProperty(
              PropertyKeyName(PropKey),
              TGocciaPropertyDescriptorData.Create(
                RightValue, [pfEnumerable, pfConfigurable, pfWritable]));
        end
        else
          SetPropertyValue(TargetValue,
            KeyToPropertyNameRegister(FRegisters[B]), RightValue);
      end;

      OP_DEFINE_STATIC_METHOD_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(B).StringValue;
        RightValue := RegisterToValue(FRegisters[C]);
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaObjectValue) then
        begin
          if IsBytecodePrivateKey(GlobalName) and
             (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
          begin
            DeclareBytecodePrivateNameForClass(FRegisters[A].ObjectValue,
              GlobalName, True);
            SetBytecodeHomeObject(RightValue, RegisterToValue(FRegisters[A]),
              True);
            TGocciaVMClassValue(FRegisters[A].ObjectValue).AddPrivateStaticMethod(
              BytecodePrivateRuntimeKey(GlobalName,
                TGocciaVMClassValue(FRegisters[A].ObjectValue)
                  .PrivateBrandToken),
              RightValue);
            goto LDispatchNext;
          end;
          if FRegisters[A].ObjectValue is TGocciaVMClassValue then
            SetBytecodeHomeObject(RightValue, RegisterToValue(FRegisters[A]),
              True);
          TGocciaObjectValue(FRegisters[A].ObjectValue).DefineProperty(
            GlobalName,
            TGocciaPropertyDescriptorData.Create(
              RightValue, [pfConfigurable, pfWritable]));
        end
        else
          SetPropertyValue(GetRegister(A), GlobalName, RightValue);
      end;

      OP_DEFINE_DATA_PROP:
        DefineDataPropertyByKey(RegisterToValue(FRegisters[A]),
          FRegisters[B], RegisterToValue(FRegisters[C]));

      OP_DEFINE_METHOD_PROP:
        DefineMethodPropertyByKey(RegisterToValue(FRegisters[A]),
          FRegisters[B], RegisterToValue(FRegisters[C]));

      OP_DEFINE_CLASS_METHOD_DYNAMIC:
      begin
        RightValue := RegisterToValue(FRegisters[C]);
        TargetValue := GetRegister(A);
        if TargetValue is TGocciaObjectValue then
        begin
          PropKey := ClassifyPropertyKey(FRegisters[B], False);
          if TargetValue is TGocciaClassValue then
            SetBytecodeHomeObject(RightValue, TargetValue, True)
          else
            SetBytecodeHomeObject(RightValue, TargetValue);

          if PropKey.Kind = pkkSymbol then
            TGocciaObjectValue(TargetValue).DefineSymbolProperty(
              PropKey.Symbol,
              TGocciaPropertyDescriptorData.Create(
                RightValue, [pfConfigurable, pfWritable]))
          else
            TGocciaObjectValue(TargetValue).DefineProperty(
              PropertyKeyName(PropKey),
              TGocciaPropertyDescriptorData.Create(
                RightValue, [pfConfigurable, pfWritable]));
        end
        else
          SetPropertyValue(TargetValue,
            KeyToPropertyNameRegister(FRegisters[B]), RightValue);
      end;

      OP_SET_OBJECT_PROTO:
        SetObjectLiteralPrototype(RegisterToValue(FRegisters[A]),
          RegisterToValue(FRegisters[B]));

      OP_DELETE_PROP_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        if FRegisters[A].Kind = grkNull then
          ThrowTypeError(Format(SErrorCannotReadPropertiesOfNull,
            [GlobalName]),
            SSuggestCheckNullBeforeAccess)
        else if FRegisters[A].Kind = grkUndefined then
          ThrowTypeError(Format(SErrorCannotReadPropertiesOfUndefined,
            [GlobalName]),
            SSuggestCheckNullBeforeAccess)
        else if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaStringLiteralValue) and
           IsNonConfigurableStringExoticProperty(
             TGocciaStringLiteralValue(FRegisters[A].ObjectValue),
             GlobalName) then
          ThrowTypeError(Format(SErrorCannotDeletePropertyOf,
            [GlobalName,
             TGocciaStringLiteralValue(FRegisters[A].ObjectValue).Value]),
            SSuggestCannotDeleteNonConfigurable)
        else if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaObjectValue) then
        begin
          if TGocciaObjectValue(FRegisters[A].ObjectValue).DeleteProperty(
            GlobalName) then
            FRegisters[A] := RegisterBoolean(True)
          else
            ThrowTypeError(Format(SErrorCannotDeletePropertyOf,
              [GlobalName, '[object Object]']),
              SSuggestCannotDeleteNonConfigurable);
        end
        else
          FRegisters[A] := RegisterBoolean(True);
      end;

      OP_DELETE_PROP_CONST_LOOSE:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if FRegisters[A].Kind = grkNull then
          ThrowTypeError(Format(SErrorCannotReadPropertiesOfNull,
            [GlobalName]),
            SSuggestCheckNullBeforeAccess)
        else if FRegisters[A].Kind = grkUndefined then
          ThrowTypeError(Format(SErrorCannotReadPropertiesOfUndefined,
            [GlobalName]),
            SSuggestCheckNullBeforeAccess)
        else if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaStringLiteralValue) and
           IsNonConfigurableStringExoticProperty(
             TGocciaStringLiteralValue(FRegisters[A].ObjectValue),
             GlobalName) then
          FRegisters[A] := RegisterBoolean(False)
        else if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaObjectValue) then
        begin
          if TGocciaObjectValue(FRegisters[A].ObjectValue).DeleteProperty(
            GlobalName) then
            FRegisters[A] := RegisterBoolean(True)
          else
            FRegisters[A] := RegisterBoolean(False);
        end
        else
          FRegisters[A] := RegisterBoolean(True);
      end;

      OP_UNPACK:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaArrayValue) then
        begin
          ArgsArray := TGocciaArrayValue.Create;
          for I := C to TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count - 1 do
            ArgsArray.Elements.Add(
              TGocciaArrayValue(FRegisters[B].ObjectValue).GetElement(I));
          FRegisters[A] := RegisterObject(ArgsArray);
        end
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_GET_INDEX:
        ExecGetComputedProperty(A, FRegisters[B], FRegisters[C],
          MEMBER_GET_OPTIONS);

      OP_SET_INDEX:
        ExecSetComputedProperty(A, FRegisters[B], FRegisters[C],
          MEMBER_SET_OPTIONS);

      OP_GET_WITH_BINDING:
        SetRegister(A, GetWithBindingValue(GetRegister(B), GetRegister(C),
          False));

      OP_GET_WITH_BINDING_STRICT:
        SetRegister(A, GetWithBindingValue(GetRegister(B), GetRegister(C),
          True));

      OP_SET_WITH_BINDING:
        SetWithBindingValue(GetRegister(A), GetRegister(B), GetRegister(C),
          True);

      OP_SET_WITH_BINDING_LOOSE:
        SetWithBindingValue(GetRegister(A), GetRegister(B), GetRegister(C),
          False);

      OP_SET_INDEX_LOOSE:
      begin
        // ES2026 §6.2.5.6 PutValue step 3.a precedes step 3.c: reject a nullish
        // base before SetIndexValueLoose classifies (and possibly coerces) the key.
        // Sloppy mode does not relax this — only the "assignment failed" case at
        // step 3.e is strict-only.
        RequireCoercibleBaseRegister(FRegisters[A], FRegisters[B], True);

        // Both the value and a boxed-primitive target are materialized fresh
        // here, then held across SetIndexValueLoose's key coercion, which
        // re-enters guest code. Root both so a collection forced from the key's
        // hook cannot sweep them before the store.
        RightValue := RegisterToValue(FRegisters[C]);
        TargetValue := GetRegister(A);
        OperandRoots.Initialize;
        OperandRoots.Add(RightValue);
        OperandRoots.Add(TargetValue);
        try
          if (TargetValue is TGocciaClassValue) or
             (TargetValue is TGocciaObjectValue) then
            SetBytecodeHomeObject(RightValue, TargetValue);
          if not ((TargetValue is TGocciaArrayValue) and
                  (FRegisters[B].Kind = grkInt) and
                  (FRegisters[B].IntValue >= 0) and
                  (FRegisters[B].IntValue <= High(Integer)) and
                  TGocciaArrayValue(TargetValue).TryAppendDenseElementFast(
                    FRegisters[B].IntValue, RightValue)) then
            SetIndexValueLoose(TargetValue, FRegisters[B], RightValue);
        finally
          OperandRoots.Clear;
        end;
      end;

      OP_ADD:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then
            TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue +
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then
            TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) +
            RegisterToDouble(FRegisters[C]));
        end
        else begin
          if FProfilingOpcodes then
            TGocciaProfiler.Instance.RecordScalarMiss;
          if (((FRegisters[B].Kind = grkObject) and
                  (FRegisters[B].ObjectValue is TGocciaStringLiteralValue)) or
                 ((FRegisters[C].Kind = grkObject) and
                  (FRegisters[C].ObjectValue is TGocciaStringLiteralValue))) and
                (not ((FRegisters[B].Kind = grkObject) and
                      Assigned(FRegisters[B].ObjectValue) and
                      (not FRegisters[B].ObjectValue.IsPrimitive))) and
                (not ((FRegisters[C].Kind = grkObject) and
                      Assigned(FRegisters[C].ObjectValue) and
                      (not FRegisters[C].ObjectValue.IsPrimitive))) then
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            VMRegisterToStringFast(FRegisters[B]).Value +
            VMRegisterToStringFast(FRegisters[C]).Value))
        else
        begin
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            SetRegisterFast(A, TGocciaStringLiteralValue.Create(
              TGocciaStringLiteralValue(LeftValue).Value +
              TGocciaStringLiteralValue(RightValue).Value))
          else if LeftValue.IsPrimitive and RightValue.IsPrimitive then
          begin
            if (LeftValue is TGocciaStringLiteralValue) or
               (RightValue is TGocciaStringLiteralValue) then
              SetRegisterFast(A, TGocciaStringLiteralValue.Create(
                LeftValue.ToStringLiteral.Value + RightValue.ToStringLiteral.Value))
            else
              SetRegisterFast(A, EvaluateAddition(LeftValue, RightValue));
          end
          else
            // Rooted: at least one operand is an object, so EvaluateAddition
            // re-enters guest code. See VMRootedBinaryValue.
            SetRegister(A, VMRootedBinaryValue(@EvaluateAddition,
              LeftValue, RightValue));
        end;
        end;
      end;

      OP_SUB:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue -
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) -
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMRootedBinaryValue(@EvaluateSubtraction,
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_INC:
        if FRegisters[B].Kind = grkInt then
          SetRegisterRaw(A, VMIntResult(FRegisters[B].IntValue + 1))
        else if FRegisters[B].Kind = grkFloat then
          SetRegisterRaw(A, VMNumberRegister(FRegisters[B].FloatValue + 1.0))
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaBigIntValue) then
          SetRegister(A, TGocciaBigIntValue.Create(
            TGocciaBigIntValue(FRegisters[B].ObjectValue).Value.Add(TBigInteger.One)))
        else
          SetRegister(A, VMNumberValue(GetRegisterFast(B).ToNumberLiteral.Value + 1));

      OP_DEC:
        if FRegisters[B].Kind = grkInt then
          SetRegisterRaw(A, VMIntResult(FRegisters[B].IntValue - 1))
        else if FRegisters[B].Kind = grkFloat then
          SetRegisterRaw(A, VMNumberRegister(FRegisters[B].FloatValue - 1.0))
        else if (FRegisters[B].Kind = grkObject) and
                (FRegisters[B].ObjectValue is TGocciaBigIntValue) then
          SetRegister(A, TGocciaBigIntValue.Create(
            TGocciaBigIntValue(FRegisters[B].ObjectValue).Value.Subtract(TBigInteger.One)))
        else
          SetRegister(A, VMNumberValue(GetRegisterFast(B).ToNumberLiteral.Value - 1));

      OP_INC_NUMERIC:
        case FRegisters[B].Kind of
          grkInt:
            SetRegisterRaw(A, VMIntResult(FRegisters[B].IntValue + 1));
          grkFloat:
            SetRegisterRaw(A, VMNumberRegister(FRegisters[B].FloatValue + 1.0));
          grkBoolean:
            if FRegisters[B].BoolValue then
              SetRegisterRaw(A, RegisterInt(2))
            else
              SetRegisterRaw(A, RegisterInt(1));
          grkNull:
            SetRegisterRaw(A, RegisterInt(1));
          grkUndefined, grkHole:
            SetRegister(A, TGocciaNumberLiteralValue.NaNValue);
        else
          LeftValue := ToPrimitive(GetRegisterFast(B), tphNumber);
          if LeftValue is TGocciaBigIntValue then
            SetRegister(A, TGocciaBigIntValue.Create(
              TGocciaBigIntValue(LeftValue).Value.Add(TBigInteger.One)))
          else
            SetRegister(A, VMNumberValue(LeftValue.ToNumberLiteral.Value + 1));
        end;

      OP_DEC_NUMERIC:
        case FRegisters[B].Kind of
          grkInt:
            SetRegisterRaw(A, VMIntResult(FRegisters[B].IntValue - 1));
          grkFloat:
            SetRegisterRaw(A, VMNumberRegister(FRegisters[B].FloatValue - 1.0));
          grkBoolean:
            if FRegisters[B].BoolValue then
              SetRegisterRaw(A, RegisterInt(0))
            else
              SetRegisterRaw(A, RegisterInt(-1));
          grkNull:
            SetRegisterRaw(A, RegisterInt(-1));
          grkUndefined, grkHole:
            SetRegister(A, TGocciaNumberLiteralValue.NaNValue);
        else
          LeftValue := ToPrimitive(GetRegisterFast(B), tphNumber);
          if LeftValue is TGocciaBigIntValue then
            SetRegister(A, TGocciaBigIntValue.Create(
              TGocciaBigIntValue(LeftValue).Value.Subtract(TBigInteger.One)))
          else
            SetRegister(A, VMNumberValue(LeftValue.ToNumberLiteral.Value - 1));
        end;

      OP_POST_INC_NUMERIC:
        case FRegisters[B].Kind of
          grkInt:
          begin
            FRegisters[A] := FRegisters[B];
            if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
              FLocalCells[A].Value := FRegisters[A];
            FRegisters[B] := VMIntResult(FRegisters[B].IntValue + 1);
            if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
              FLocalCells[B].Value := FRegisters[B];
          end;
          grkFloat:
          begin
            FRegisters[A] := FRegisters[B];
            if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
              FLocalCells[A].Value := FRegisters[A];
            FRegisters[B] := VMNumberRegister(FRegisters[B].FloatValue + 1.0);
            if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
              FLocalCells[B].Value := FRegisters[B];
          end;
          grkBoolean:
          begin
            if FRegisters[B].BoolValue then
            begin
              FRegisters[A] := RegisterInt(1);
              if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
                FLocalCells[A].Value := FRegisters[A];
              FRegisters[B] := RegisterInt(2);
              if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
                FLocalCells[B].Value := FRegisters[B];
            end
            else
            begin
              FRegisters[A] := RegisterInt(0);
              if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
                FLocalCells[A].Value := FRegisters[A];
              FRegisters[B] := RegisterInt(1);
              if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
                FLocalCells[B].Value := FRegisters[B];
            end;
          end;
          grkNull:
          begin
            FRegisters[A] := RegisterInt(0);
            if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
              FLocalCells[A].Value := FRegisters[A];
            FRegisters[B] := RegisterInt(1);
            if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
              FLocalCells[B].Value := FRegisters[B];
          end;
          grkUndefined, grkHole:
          begin
            SetRegister(A, TGocciaNumberLiteralValue.NaNValue);
            SetRegister(B, TGocciaNumberLiteralValue.NaNValue);
          end;
        else
          LeftValue := ToPrimitive(GetRegisterFast(B), tphNumber);
          if LeftValue is TGocciaBigIntValue then
          begin
            SetRegisterFast(A, LeftValue);
            SetRegister(B, TGocciaBigIntValue.Create(
              TGocciaBigIntValue(LeftValue).Value.Add(TBigInteger.One)));
          end
          else
          begin
            NumericValue := LeftValue.ToNumberLiteral.Value;
            SetRegister(A, VMNumberValue(NumericValue));
            SetRegister(B, VMNumberValue(NumericValue + 1));
          end;
        end;

      OP_POST_DEC_NUMERIC:
        case FRegisters[B].Kind of
          grkInt:
          begin
            FRegisters[A] := FRegisters[B];
            if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
              FLocalCells[A].Value := FRegisters[A];
            FRegisters[B] := VMIntResult(FRegisters[B].IntValue - 1);
            if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
              FLocalCells[B].Value := FRegisters[B];
          end;
          grkFloat:
          begin
            FRegisters[A] := FRegisters[B];
            if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
              FLocalCells[A].Value := FRegisters[A];
            FRegisters[B] := VMNumberRegister(FRegisters[B].FloatValue - 1.0);
            if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
              FLocalCells[B].Value := FRegisters[B];
          end;
          grkBoolean:
          begin
            if FRegisters[B].BoolValue then
            begin
              FRegisters[A] := RegisterInt(1);
              if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
                FLocalCells[A].Value := FRegisters[A];
              FRegisters[B] := RegisterInt(0);
              if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
                FLocalCells[B].Value := FRegisters[B];
            end
            else
            begin
              FRegisters[A] := RegisterInt(0);
              if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
                FLocalCells[A].Value := FRegisters[A];
              FRegisters[B] := RegisterInt(-1);
              if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
                FLocalCells[B].Value := FRegisters[B];
            end;
          end;
          grkNull:
          begin
            FRegisters[A] := RegisterInt(0);
            if (A < FLocalCellCount) and Assigned(FLocalCells[A]) then
              FLocalCells[A].Value := FRegisters[A];
            FRegisters[B] := RegisterInt(-1);
            if (B < FLocalCellCount) and Assigned(FLocalCells[B]) then
              FLocalCells[B].Value := FRegisters[B];
          end;
          grkUndefined, grkHole:
          begin
            SetRegister(A, TGocciaNumberLiteralValue.NaNValue);
            SetRegister(B, TGocciaNumberLiteralValue.NaNValue);
          end;
        else
          LeftValue := ToPrimitive(GetRegisterFast(B), tphNumber);
          if LeftValue is TGocciaBigIntValue then
          begin
            SetRegisterFast(A, LeftValue);
            SetRegister(B, TGocciaBigIntValue.Create(
              TGocciaBigIntValue(LeftValue).Value.Subtract(TBigInteger.One)));
          end
          else
          begin
            NumericValue := LeftValue.ToNumberLiteral.Value;
            SetRegister(A, VMNumberValue(NumericValue));
            SetRegister(B, VMNumberValue(NumericValue - 1));
          end;
        end;

      OP_MUL:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue *
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) *
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMRootedBinaryValue(@EvaluateMultiplication,
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_DIV:
      begin
        if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMNumberRegister(RegisterToDouble(FRegisters[B]) /
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMRootedBinaryValue(@EvaluateDivision,
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_MOD:
      begin
        if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMModuloRegister(RegisterToDouble(FRegisters[B]),
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMRootedBinaryValue(@EvaluateModulo,
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_POW:
      begin
        if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMPowerRegister(RegisterToDouble(FRegisters[B]),
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMRootedBinaryValue(@EvaluateExponentiation,
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_NEG:
        if RegisterIsNumericScalar(FRegisters[B]) then
          FRegisters[A] := VMNumberRegister(-RegisterToDouble(FRegisters[B]))
        else
        begin
          // ES2026 §13.5.5 UnaryMinus invokes ToNumeric (= ToPrimitive
          // then a BigInt? branch).  Apply ToPrimitive so boxed BigInts
          // (Object(1n)) unbox to their primitive and take the
          // BigInt::unaryMinus path; without it the box's
          // ToNumberLiteral coerces to NaN and we lose the BigInt.
          LeftValue := ToPrimitive(GetRegisterFast(B), tphNumber);
          if LeftValue is TGocciaBigIntValue then
            SetRegister(A, TGocciaBigIntValue.Create(
              TGocciaBigIntValue(LeftValue).Value.Negate))
          else
            SetRegister(A, VMNumberValue(-LeftValue.ToNumberLiteral.Value));
        end;

      OP_BAND:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(
            LongInt(FRegisters[B].IntValue) and
            LongInt(FRegisters[C].IntValue))
        else
          SetRegister(A, VMRootedBinaryValue(@EvaluateBitwiseAnd,
            GetRegister(B), GetRegister(C)));

      OP_BOR:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(
            LongInt(FRegisters[B].IntValue) or
            LongInt(FRegisters[C].IntValue))
        else
          SetRegister(A, VMRootedBinaryValue(@EvaluateBitwiseOr,
            GetRegister(B), GetRegister(C)));

      OP_BXOR:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(
            LongInt(FRegisters[B].IntValue) xor
            LongInt(FRegisters[C].IntValue))
        else
          SetRegister(A, VMRootedBinaryValue(@EvaluateBitwiseXor,
            GetRegister(B), GetRegister(C)));

      OP_SHL:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(LongInt(
            LongWord(FRegisters[B].IntValue) shl
            (LongWord(FRegisters[C].IntValue) and 31)))
        else
          SetRegister(A, VMRootedBinaryValue(@EvaluateLeftShift,
            GetRegister(B), GetRegister(C)));

      OP_SHR:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(SignedRightShiftInt32(
            LongInt(FRegisters[B].IntValue),
            LongWord(FRegisters[C].IntValue)))
        else
          SetRegister(A, VMRootedBinaryValue(@EvaluateRightShift,
            GetRegister(B), GetRegister(C)));

      OP_USHR:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(Int64(LongWord(
            FRegisters[B].IntValue) shr
            (LongWord(FRegisters[C].IntValue) and 31)))
        else
          SetRegister(A, VMRootedBinaryValue(@EvaluateUnsignedRightShift,
            GetRegister(B), GetRegister(C)));

      OP_BNOT:
        if FRegisters[B].Kind = grkInt then
          FRegisters[A] := RegisterInt(not LongInt(FRegisters[B].IntValue))
        else
          SetRegister(A, EvaluateBitwiseNot(GetRegister(B)));

      OP_EQ:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue = FRegisters[C].IntValue)
        else if (FRegisters[B].Kind = grkObject) and
           (FRegisters[C].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaStringLiteralValue) and
           (FRegisters[C].ObjectValue is TGocciaStringLiteralValue) then
          FRegisters[A] := RegisterBoolean(UTF16StringsEqual(
            TGocciaStringLiteralValue(FRegisters[B].ObjectValue).Value,
            TGocciaStringLiteralValue(FRegisters[C].ObjectValue).Value))
        else
          SetRegister(A, GetRegister(B).IsEqual(GetRegister(C)));

      OP_NEQ:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue <> FRegisters[C].IntValue)
        else if (FRegisters[B].Kind = grkObject) and
           (FRegisters[C].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaStringLiteralValue) and
           (FRegisters[C].ObjectValue is TGocciaStringLiteralValue) then
          FRegisters[A] := RegisterBoolean(not UTF16StringsEqual(
            TGocciaStringLiteralValue(FRegisters[B].ObjectValue).Value,
            TGocciaStringLiteralValue(FRegisters[C].ObjectValue).Value))
        else
          SetRegister(A, GetRegister(B).IsNotEqual(GetRegister(C)));

      OP_LOOSE_EQ:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue = FRegisters[C].IntValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FromBoolean(
            VMRootedBinaryPredicate(@Goccia.Arithmetic.IsLooselyEqual,
              GetRegister(B), GetRegister(C))));

      OP_LOOSE_NEQ:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue <> FRegisters[C].IntValue)
        else
          SetRegister(A, TGocciaBooleanLiteralValue.FromBoolean(
            VMRootedBinaryPredicate(@Goccia.Arithmetic.IsNotLooselyEqual,
              GetRegister(B), GetRegister(C))));

      OP_LT:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(FRegisters[B].IntValue <
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(RegisterToDouble(FRegisters[B]) <
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            FRegisters[A] := RegisterBoolean(
              Goccia.Arithmetic.CompareStringValues(
                TGocciaStringLiteralValue(LeftValue).Value,
                TGocciaStringLiteralValue(RightValue).Value) < 0)
          else
            FRegisters[A] := RegisterBoolean(VMRootedBinaryPredicate(
              @Goccia.Arithmetic.LessThan, LeftValue, RightValue));
        end;
      end;

      OP_GT:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(FRegisters[B].IntValue >
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(RegisterToDouble(FRegisters[B]) >
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            FRegisters[A] := RegisterBoolean(
              Goccia.Arithmetic.CompareStringValues(
                TGocciaStringLiteralValue(LeftValue).Value,
                TGocciaStringLiteralValue(RightValue).Value) > 0)
          else
            FRegisters[A] := RegisterBoolean(VMRootedBinaryPredicate(
              @Goccia.Arithmetic.GreaterThan, LeftValue, RightValue));
        end;
      end;

      OP_LTE:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(FRegisters[B].IntValue <=
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(RegisterToDouble(FRegisters[B]) <=
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            FRegisters[A] := RegisterBoolean(
              Goccia.Arithmetic.CompareStringValues(
                TGocciaStringLiteralValue(LeftValue).Value,
                TGocciaStringLiteralValue(RightValue).Value) <= 0)
          else
            FRegisters[A] := RegisterBoolean(VMRootedBinaryPredicate(
              @Goccia.Arithmetic.LessThanOrEqual, LeftValue, RightValue));
        end;
      end;

      OP_GTE:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(FRegisters[B].IntValue >=
            FRegisters[C].IntValue);
        end
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(RegisterToDouble(FRegisters[B]) >=
            RegisterToDouble(FRegisters[C]));
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          LeftValue := GetRegisterFast(B);
          RightValue := GetRegisterFast(C);
          if (LeftValue is TGocciaStringLiteralValue) and
             (RightValue is TGocciaStringLiteralValue) then
            FRegisters[A] := RegisterBoolean(
              Goccia.Arithmetic.CompareStringValues(
                TGocciaStringLiteralValue(LeftValue).Value,
                TGocciaStringLiteralValue(RightValue).Value) >= 0)
          else
            FRegisters[A] := RegisterBoolean(VMRootedBinaryPredicate(
              @Goccia.Arithmetic.GreaterThanOrEqual, LeftValue, RightValue));
        end;
      end;

      OP_TYPEOF:
        case FRegisters[B].Kind of
          grkUndefined:
            SetRegister(A, TGocciaStringLiteralValue.Create('undefined'));
          grkNull, grkHole:
            SetRegister(A, TGocciaStringLiteralValue.Create('object'));
          grkBoolean:
            SetRegister(A, TGocciaStringLiteralValue.Create('boolean'));
          grkInt, grkFloat:
            SetRegister(A, TGocciaStringLiteralValue.Create('number'));
        else
          SetRegister(A, TGocciaStringLiteralValue.Create(GetRegister(B).TypeOf));
        end;

      OP_IS_INSTANCE:
      begin
        ObjectConstructorValue := VMGlobalObjectConstructor(FGlobalScope);
        FunctionConstructorValue := VMGlobalFunctionConstructor(FGlobalScope);
        SetRegister(A, VMInstanceOfValue(GetRegister(B), GetRegister(C),
          ObjectConstructorValue, FunctionConstructorValue));
      end;

      OP_HAS_PROPERTY:
        SetRegister(A, HasPropertyValue(GetRegister(B), GetRegister(C)));

      OP_HAS_WITH_BINDING:
        SetRegister(A, HasWithBindingValue(GetRegister(B), GetRegister(C)));

      OP_MATCH_HAS_PROPERTY:
        SetRegister(A, MatchHasPropertyValue(GetRegister(B), GetRegister(C)));

      OP_MATCH_EXTRACTOR:
        SetRegister(A, MatchExtractorValue(GetRegister(B), GetRegister(C)));

      OP_MATCH_VALUE:
      begin
        // The subject is materialized fresh for scalar registers and is used
        // after GetCustomMatcher, which reads matcher[Symbol.customMatcher] and
        // can run a user getter/proxy trap. Root the subject so a collection
        // forced from that lookup cannot sweep it before the matcher sees it.
        LeftValue := GetRegister(B);
        RightValue := GetRegister(C);
        OperandRoots.Initialize;
        OperandRoots.Add(LeftValue);
        try
          CustomMatcherValue := GetCustomMatcher(RightValue);
          if Assigned(CustomMatcherValue) then
          begin
            if not CustomMatcherValue.IsCallable then
              ThrowTypeError('Symbol.customMatcher must be callable');
            CallArgs := AcquireArguments(2);
            try
              MatchHintObject := TGocciaObjectValue.Create;
              MatchHintObject.AssignProperty(PROP_MATCH_TYPE,
                TGocciaStringLiteralValue.Create('boolean'));
              CallArgs.Add(LeftValue);
              CallArgs.Add(MatchHintObject);
              MatchResultValue := InvokeFunctionValue(CustomMatcherValue,
                CallArgs, RightValue);
              SetRegister(A, MatchResultValue.ToBooleanLiteral);
            finally
              ReleaseArguments(CallArgs);
            end;
          end
          else if RightValue is TGocciaClassValue then
          begin
            ObjectConstructorValue := VMGlobalObjectConstructor(FGlobalScope);
            FunctionConstructorValue := VMGlobalFunctionConstructor(FGlobalScope);
            if VMBuiltinConstructorMatchValue(RightValue, LeftValue,
              FGlobalScope, BuiltinConstructorMatch) then
              SetRegister(A, TGocciaBooleanLiteralValue.Create(BuiltinConstructorMatch))
            else
              SetRegister(A, VMInstanceOfValue(LeftValue, RightValue,
                ObjectConstructorValue, FunctionConstructorValue));
          end
          else if VMBuiltinConstructorMatchValue(RightValue, LeftValue,
            FGlobalScope, BuiltinConstructorMatch) then
            SetRegister(A, TGocciaBooleanLiteralValue.Create(BuiltinConstructorMatch))
          else
            SetRegister(A, TGocciaBooleanLiteralValue.Create(
              MatchValueEquals(LeftValue, RightValue)));
        finally
          OperandRoots.Clear;
        end;
      end;

      OP_TO_NUMBER:
        case FRegisters[B].Kind of
          grkInt, grkFloat:
            FRegisters[A] := FRegisters[B];
          grkBoolean:
            if FRegisters[B].BoolValue then
              FRegisters[A] := RegisterInt(1)
            else
              FRegisters[A] := RegisterInt(0);
          grkNull:
            FRegisters[A] := RegisterInt(0);
          grkUndefined, grkHole:
            FRegisters[A] := RegisterObject(TGocciaNumberLiteralValue.NaNValue);
        else
          SetRegister(A, GetRegister(B).ToNumberLiteral);
        end;

      OP_TO_NUMERIC:
        case FRegisters[B].Kind of
          grkInt, grkFloat:
            FRegisters[A] := FRegisters[B];
          grkBoolean:
            if FRegisters[B].BoolValue then
              FRegisters[A] := RegisterInt(1)
            else
              FRegisters[A] := RegisterInt(0);
          grkNull:
            FRegisters[A] := RegisterInt(0);
          grkUndefined, grkHole:
            FRegisters[A] := RegisterObject(TGocciaNumberLiteralValue.NaNValue);
        else
          LeftValue := ToPrimitive(GetRegisterFast(B), tphNumber);
          if LeftValue is TGocciaBigIntValue then
            SetRegisterFast(A, LeftValue)
          else
            SetRegister(A, LeftValue.ToNumberLiteral);
        end;

      OP_TO_STRING:
        SetRegisterFast(A, VMRegisterToStringFast(FRegisters[B]));

      OP_DEL_INDEX:
        ExecDeleteComputedProperty(A, FRegisters[B], FRegisters[C], True);

      OP_DEL_INDEX_LOOSE:
        ExecDeleteComputedProperty(A, FRegisters[B], FRegisters[C], False);

      OP_CLOSURE:
      begin
        ChildTemplate := Template.GetFunctionUnchecked(DecodeBx(Instruction));
        if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and
           Assigned(ChildTemplate.DebugInfo) and
           (ChildTemplate.DebugInfo.LineMapCount > 0) then
          TGocciaCoverageTracker.Instance.RegisterFunction(
            ChildTemplate.DebugInfo.SourceFile, ChildTemplate.Name,
            ChildTemplate.DebugInfo.CoverageLine,
            ChildTemplate.DebugInfo.CoverageColumn);
        ChildClosure := TGocciaBytecodeClosure.Create(
          ChildTemplate, ChildTemplate.UpvalueCount);
        ChildClosure.GlobalScope := FGlobalScope;
        ChildClosure.DynamicVarScope := FCurrentDynamicVarScope;
        if ChildTemplate.IsArrow and Assigned(FCurrentClosure) then
        begin
          ChildClosure.HomeObject := FCurrentClosure.HomeObject;
          ChildClosure.HomeClass := FCurrentClosure.HomeClass;
          ChildClosure.NewTarget := FCurrentNewTarget;
          if Assigned(FCurrentClosure.Template) and
             FCurrentClosure.Template.IsArrow then
            ChildClosure.AllowsNewTarget := FCurrentClosure.AllowsNewTarget
          else
            ChildClosure.AllowsNewTarget :=
              Assigned(FCurrentClosure.FunctionValue) and
              not TemplateUsesGlobalEvalEnvironment(FCurrentClosure.Template);
        end
        else
          ChildClosure.AllowsNewTarget := True;
        for I := 0 to ChildTemplate.UpvalueCount - 1 do
        begin
          Desc := ChildTemplate.GetUpvalueDescriptor(I);
          if Desc.IsLocal then
            ChildClosure.SetUpvalue(I, TGocciaBytecodeUpvalue.Create(
              GetLocalCell(Desc.Index)))
          else if Assigned(FCurrentClosure) then
          begin
            ChildClosure.SetUpvalue(I, FCurrentClosure.GetUpvalue(Desc.Index));
            ChildClosure.SetDynamicVarUpvalue(I,
              ((FCurrentDynamicVarScope <>
                FCurrentClosure.DynamicVarScope) and
               Assigned(FCurrentDynamicVarScope)) or
              FCurrentClosure.IsDynamicVarUpvalue(Desc.Index));
          end;
        end;
        BytecodeFunction := TGocciaBytecodeFunctionValue.Create(Self, ChildClosure);
        // ES2026 §10.2.5 MakeConstructor: install own `prototype` data property
        // for `function`/`function*` declarations and expressions (including
        // async generators).  The prototype is a fresh ordinary object whose
        // `constructor` data property back-references the function.
        if ChildTemplate.HasOwnPrototype then
          InstallFunctionPrototype(BytecodeFunction,
            BytecodeFunctionIntrinsicKind(ChildTemplate));
        SetRegister(A, BytecodeFunction);
      end;

      OP_CALL_SELF_NUM:
      begin
        CheckExecutionTimeout;
        PushClosedNumericFrame(A, B, C, Frame, Template, PrevCovLine,
          ProfileEntryTimestamp, ClosedNumericInitializedRegisterTop);
        goto LDispatchNext;
      end;

      OP_CALL:
      begin
        CheckExecutionTimeout;
        if ((C and CALL_FLAG_DIRECT_EVAL) <> 0) and
           (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaNativeFunctionValue) and
           TGocciaNativeFunctionValue(FRegisters[A].ObjectValue).DirectEvalHost and
           IsCurrentRealmEvalFunction(FRegisters[A].ObjectValue, FRealm) then
        begin
          EvalSourceValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          if (C and CALL_FLAG_SPREAD) <> 0 then
          begin
	            if (FRegisters[B].Kind = grkObject) and
	               (FRegisters[B].ObjectValue is TGocciaArrayValue) and
	               (TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count > 0) then
	              EvalSourceValue := TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty('0');
          end
          else if B > 0 then
            EvalSourceValue := GetRegister(A + 1);
          SetRegister(A, ExecuteDirectEval(EvalSourceValue, Template,
            UInt32(InstructionStartIP), Template.StrictCode));
          goto LDispatchNext;
        end;
        if ((C and CALL_FLAG_SPREAD) = 0) and
           (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaNativeFunctionValue) and
           (TGocciaNativeFunctionValue(FRegisters[A].ObjectValue).
             CreationRealm = CurrentRealm) and
           (B = 1) and
           (FRegisters[A + 1].Kind = grkObject) and
           (FRegisters[A + 1].ObjectValue is TGocciaStringLiteralValue) then
        begin
          case TGocciaNativeFunctionValue(FRegisters[A].ObjectValue).
            IntrinsicKind of
            nikDecodeURI:
              begin
                SetRegisterFast(A, TGocciaStringLiteralValue.Create(
                  DecodeURI(TGocciaStringLiteralValue(
                    FRegisters[A + 1].ObjectValue).Value)));
                goto LDispatchNext;
              end;
            nikDecodeURIComponent:
              begin
                SetRegisterFast(A, TGocciaStringLiteralValue.Create(
                  DecodeURIComponent(TGocciaStringLiteralValue(
                    FRegisters[A + 1].ObjectValue).Value)));
                goto LDispatchNext;
              end;
          end;
        end;
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaBoundFunctionValue) then
        begin
          BoundFunction := TGocciaBoundFunctionValue(FRegisters[A].ObjectValue);
          if BoundFunction.OriginalFunction is TGocciaBytecodeFunctionValue then
          begin
            BytecodeFunction := TGocciaBytecodeFunctionValue(BoundFunction.OriginalFunction);
            if Assigned(BytecodeFunction.FClosure) and
               Assigned(BytecodeFunction.FClosure.Template) and
               (not BytecodeFunction.FClosure.Template.IsAsync) and
               (not BytecodeFunction.FClosure.Template.IsGenerator) then
            begin
              if (C and 1) = 0 then
              begin
                SetLength(RegisterArgs, BoundFunction.BoundArgCount + B);
                for I := 0 to BoundFunction.BoundArgCount - 1 do
                  RegisterArgs[I] := ValueToRegister(BoundFunction.GetBoundArg(I));
                for I := 0 to B - 1 do
                  RegisterArgs[BoundFunction.BoundArgCount + I] := FRegisters[A + 1 + I];
                CallThisRegister := ValueToRegister(BoundFunction.BoundThis);
                if not BytecodeFunction.FStrictThis then
                  CallThisRegister := CoerceNonStrictThisRegister(
                    CallThisRegister,
                    BytecodeClosureGlobalThis(BytecodeFunction.FClosure,
                      FGlobalThisValue),
                    BytecodeClosureExecutionRealm(BytecodeFunction.FClosure,
                      FRealm));
                if (C and CALL_FLAG_TAIL) <> 0 then
                  PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                    InitialFrameStackCount, SavedHandlerCount)
                else
                  PushFrame(A, Frame.IP, Template, PrevCovLine,
                    ProfileEntryTimestamp);
                SetupNewFrame(BytecodeFunction.FClosure,
                  CallThisRegister, RegisterArgs,
                  Length(RegisterArgs), RegisterUndefined, RegisterUndefined,
                  RegisterUndefined, False, True,
                  Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                goto LDispatchNext;
              end
              else if (FRegisters[B].Kind = grkObject) and
                      (FRegisters[B].ObjectValue is TGocciaArrayValue) then
              begin
                SetLength(RegisterArgs,
                  BoundFunction.BoundArgCount +
                  TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count);
                for I := 0 to BoundFunction.BoundArgCount - 1 do
                  RegisterArgs[I] := VMValueToRegisterFast(BoundFunction.GetBoundArg(I));
	                for I := 0 to TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count - 1 do
	                  RegisterArgs[BoundFunction.BoundArgCount + I] := VMValueToRegisterFast(
	                    TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty(IntToStr(I)));
                CallThisRegister := ValueToRegister(BoundFunction.BoundThis);
                if not BytecodeFunction.FStrictThis then
                  CallThisRegister := CoerceNonStrictThisRegister(
                    CallThisRegister,
                    BytecodeClosureGlobalThis(BytecodeFunction.FClosure,
                      FGlobalThisValue),
                    BytecodeClosureExecutionRealm(BytecodeFunction.FClosure,
                      FRealm));
                if (C and CALL_FLAG_TAIL) <> 0 then
                  PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                    InitialFrameStackCount, SavedHandlerCount)
                else
                  PushFrame(A, Frame.IP, Template, PrevCovLine,
                    ProfileEntryTimestamp);
                SetupNewFrame(BytecodeFunction.FClosure,
                  CallThisRegister, RegisterArgs,
                  Length(RegisterArgs), RegisterUndefined, RegisterUndefined,
                  RegisterUndefined, False, True,
                  Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                goto LDispatchNext;
              end;
            end;
          end;
        end;

        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaBytecodeFunctionValue) then
        begin
          BytecodeFunction := TGocciaBytecodeFunctionValue(FRegisters[A].ObjectValue);
          if Assigned(BytecodeFunction.FClosure) and
             Assigned(BytecodeFunction.FClosure.Template) and
             (not BytecodeFunction.FClosure.Template.IsAsync) and
             (not BytecodeFunction.FClosure.Template.IsGenerator) then
          begin
            if not BytecodeFunction.FStrictThis then
            begin
              CallGlobalThisValue := BytecodeClosureGlobalThis(
                BytecodeFunction.FClosure, FGlobalThisValue);
              if Assigned(CallGlobalThisValue) then
                CallThisRegister := VMValueToRegisterFast(CallGlobalThisValue)
              else
                CallThisRegister := RegisterUndefined;
            end
            else
              CallThisRegister := RegisterUndefined;
            if (C and 1) = 0 then
            begin
              if B <= 3 then
              begin
                // Fixed-arg fast path: capture up to three arguments by value
                // before any frame push or tail-call window reuse, so they
                // survive AcquireRegisters' fill, and skip the RegisterArgs
                // staging array entirely. SetupNewFrame consumes only the first
                // B of these (bounded by AArgCount).
                if B >= 1 then FixedArg0 := FRegisters[A + 1]
                else FixedArg0 := RegisterUndefined;
                if B >= 2 then FixedArg1 := FRegisters[A + 2]
                else FixedArg1 := RegisterUndefined;
                if B >= 3 then FixedArg2 := FRegisters[A + 3]
                else FixedArg2 := RegisterUndefined;
                if (C and CALL_FLAG_TAIL) <> 0 then
                  PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                    InitialFrameStackCount, SavedHandlerCount)
                else
                  PushFrame(A, Frame.IP, Template, PrevCovLine,
                    ProfileEntryTimestamp);
                SetupNewFrame(BytecodeFunction.FClosure,
                  CallThisRegister, TGocciaRegisterArray(nil), B,
                  FixedArg0, FixedArg1, FixedArg2, True, True,
                  Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              end
              else
              begin
                SetLength(RegisterArgs, B);
                for I := 0 to B - 1 do
                  RegisterArgs[I] := FRegisters[A + 1 + I];
                if (C and CALL_FLAG_TAIL) <> 0 then
                  PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                    InitialFrameStackCount, SavedHandlerCount)
                else
                  PushFrame(A, Frame.IP, Template, PrevCovLine,
                    ProfileEntryTimestamp);
                SetupNewFrame(BytecodeFunction.FClosure,
                  CallThisRegister, RegisterArgs, B,
                  RegisterUndefined, RegisterUndefined, RegisterUndefined, False, True,
                  Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              end;
              goto LDispatchNext;
            end
            else if (FRegisters[B].Kind = grkObject) and
                    (FRegisters[B].ObjectValue is TGocciaArrayValue) then
            begin
              SetLength(RegisterArgs,
                TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count);
	              for I := 0 to High(RegisterArgs) do
	                RegisterArgs[I] := ValueToRegister(
	                  TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty(IntToStr(I)));
              if (C and CALL_FLAG_TAIL) <> 0 then
                PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                  InitialFrameStackCount, SavedHandlerCount)
              else
                PushFrame(A, Frame.IP, Template, PrevCovLine,
                  ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                CallThisRegister, RegisterArgs, Length(RegisterArgs),
                RegisterUndefined, RegisterUndefined, RegisterUndefined, False, True,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              goto LDispatchNext;
            end;
          end;
        end;

        if (C and 1) = 1 then
          CallArgs := AcquireArguments
        else
          CallArgs := AcquireArguments(B);
        try
          if (C and 1) = 1 then
          begin
	            if GetRegister(B) is TGocciaArrayValue then
	              for I := 0 to TGocciaArrayValue(GetRegister(B)).Elements.Count - 1 do
	                CallArgs.Add(TGocciaArrayValue(GetRegister(B)).GetProperty(IntToStr(I)));
          end
          else
            for I := 0 to B - 1 do
              CallArgs.Add(GetRegister(A + 1 + I));
          if not (Assigned(GetRegister(A)) and
                  (GetRegister(A).IsCallable or
                   (GetRegister(A) is TGocciaProxyValue))) then
            ThrowNotCallableHere(GetRegister(A), nil);
          if (GetRegister(A) is TGocciaNativeFunctionValue) or
             (GetRegister(A) is TGocciaFunctionConstructorClassValue) or
             (GetRegister(A) is TGocciaBoundFunctionValue) or
             (GetRegister(A) is TGocciaProxyValue) then
          begin
            if TGocciaCallStack.Instance <> nil then
              SavedConstructFrameOk :=
                TGocciaCallStack.Instance.TryGetTopFrame(SavedConstructFrame)
            else
              SavedConstructFrameOk := False;
            EnterCurrentInstructionCallSite(PreviousCallSite);
            // Stamp the executing frame with this call's position so an error a
            // native callee creates captures the call site (deferred frames are
            // 0:0). Use the recorded call-site column, matching the tree-walk
            // evaluator's per-call frame (the instruction line map resolves only
            // to the enclosing statement). Snapshotted above and restored below.
            StampCallSiteLocation(CurrentCallSite);
            try
              SetRegister(A, InvokeFunctionValue(GetRegister(A), CallArgs,
                TGocciaUndefinedLiteralValue.UndefinedValue));
            finally
              LeaveGocciaCallSite(PreviousCallSite);
            end;
            if SavedConstructFrameOk and (TGocciaCallStack.Instance <> nil) then
              TGocciaCallStack.Instance.SetTopFrame(SavedConstructFrame);
          end
          else
            SetRegister(A, InvokeFunctionValue(GetRegister(A), CallArgs,
              TGocciaUndefinedLiteralValue.UndefinedValue));
        finally
          ReleaseArguments(CallArgs);
        end;
      end;

      OP_CALL_METHOD:
      begin
        CheckExecutionTimeout;
        if ((C and CALL_FLAG_SPREAD) = 0) and (B = 2) and
           (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaNativeFunctionValue) and
           (TGocciaNativeFunctionValue(FRegisters[A].ObjectValue).
             IntrinsicKind = nikStringFromCharCode) and
           (TGocciaNativeFunctionValue(FRegisters[A].ObjectValue).
             CreationRealm = CurrentRealm) and
           (FRegisters[A + 1].Kind = grkInt) and
           (FRegisters[A + 2].Kind = grkInt) then
        begin
          SetRegisterFast(A, TGocciaStringLiteralValue.Create(
            UTF16CodeUnitPairToString(
              Cardinal(FRegisters[A + 1].IntValue and $FFFF),
              Cardinal(FRegisters[A + 2].IntValue and $FFFF))));
          goto LDispatchNext;
        end;
        if (C and 1) = 0 then
        begin
          if (FRegisters[A - 1].Kind = grkObject) and
             (FRegisters[A].Kind = grkObject) and
             (FRegisters[A].ObjectValue is TGocciaNativeFunctionValue) then
          begin
            { Identity, not name: an own `bind`, `call` or `apply` on a function
              object is a different function — `Reflect.apply` assigned as one,
              or the two statics node:async_hooks installs — and matching on the
              name alone silently redirected the call into the intrinsic. }
            CalleeIntrinsicKind :=
              TGocciaNativeFunctionValue(FRegisters[A].ObjectValue).IntrinsicKind;
            if (CalleeIntrinsicKind = nikFunctionBind) and
               (FRegisters[A - 1].ObjectValue is TGocciaFunctionBase) then
            begin
              case B of
                0:
                  FRegisters[A] := RegisterObject(
                    TGocciaBoundFunctionValue.CreateWithoutArgs(
                      FRegisters[A - 1].ObjectValue,
                      TGocciaUndefinedLiteralValue.UndefinedValue));
                1:
                  FRegisters[A] := RegisterObject(
                    TGocciaBoundFunctionValue.CreateWithoutArgs(
                      FRegisters[A - 1].ObjectValue,
                      RegisterToValue(FRegisters[A + 1])));
                2:
                  FRegisters[A] := RegisterObject(
                    TGocciaBoundFunctionValue.CreateWithSingleArg(
                      FRegisters[A - 1].ObjectValue,
                      RegisterToValue(FRegisters[A + 1]),
                      RegisterToValue(FRegisters[A + 2])));
              else
                BytecodeFunction := nil;
              end;
              if B <= 2 then
                goto LDispatchNext;
            end;

            if FRegisters[A - 1].ObjectValue is TGocciaBytecodeFunctionValue then
            begin
              BytecodeFunction := TGocciaBytecodeFunctionValue(FRegisters[A - 1].ObjectValue);
              if Assigned(BytecodeFunction.FClosure) and
                 Assigned(BytecodeFunction.FClosure.Template) and
                 (not BytecodeFunction.FClosure.Template.IsAsync) and
                 (not BytecodeFunction.FClosure.Template.IsGenerator) then
              begin
                if CalleeIntrinsicKind = nikFunctionCall then
                begin
                  if B = 0 then
                    CallThisRegister := RegisterUndefined
                  else
                    CallThisRegister := FRegisters[A + 1];
                  if not BytecodeFunction.FStrictThis then
                    CallThisRegister := CoerceNonStrictThisRegister(
                      CallThisRegister,
                      BytecodeClosureGlobalThis(BytecodeFunction.FClosure,
                        FGlobalThisValue),
                      BytecodeClosureExecutionRealm(BytecodeFunction.FClosure,
                        FRealm));
                  PushFrame(A, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
                  case B of
                    0:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, TGocciaRegisterArray(nil), 0,
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    1:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, TGocciaRegisterArray(nil), 0,
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    2:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, TGocciaRegisterArray(nil), 1,
                        FRegisters[A + 2], RegisterUndefined, RegisterUndefined,
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    3:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, TGocciaRegisterArray(nil), 2,
                        FRegisters[A + 2], FRegisters[A + 3], RegisterUndefined,
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    4:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, TGocciaRegisterArray(nil), 3,
                        FRegisters[A + 2], FRegisters[A + 3], FRegisters[A + 4],
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                  else
                    begin
                      SetLength(RegisterArgs, B - 1);
                      for I := 1 to B - 1 do
                        RegisterArgs[I - 1] := FRegisters[A + 1 + I];
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, RegisterArgs, Length(RegisterArgs),
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        False, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    end;
                  end;
                  goto LDispatchNext;
                end
                // The dense hole-free gate is what makes the direct element
                // reads below legal: no read can reach an accessor, so the
                // argument values cannot be produced by guest code that
                // allocates (and collects) while the earlier ones sit in plain
                // locals, and no read order is observable. A holey array or one
                // whose length was grown past its element count falls through
                // to the generic call, which runs Function.prototype.apply and
                // therefore CreateListFromArrayLike — ascending Get order, a
                // rooted arguments collection, and the spec argument count,
                // exactly as the interpreter does.
                else if (CalleeIntrinsicKind = nikFunctionApply) and (B >= 2) and
                        (FRegisters[A + 2].Kind = grkObject) and
                        (FRegisters[A + 2].ObjectValue is TGocciaArrayValue) and
                        IsDenseHoleFreeArgumentArray(
                          TGocciaArrayValue(FRegisters[A + 2].ObjectValue)) then
                begin
                  ArgsArray := TGocciaArrayValue(FRegisters[A + 2].ObjectValue);
                  CallThisRegister := FRegisters[A + 1];
                  if not BytecodeFunction.FStrictThis then
                    CallThisRegister := CoerceNonStrictThisRegister(
                      CallThisRegister,
                      BytecodeClosureGlobalThis(BytecodeFunction.FClosure,
                        FGlobalThisValue),
                      BytecodeClosureExecutionRealm(BytecodeFunction.FClosure,
                        FRealm));
                  PushFrame(A, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
                  case ArgsArray.Elements.Count of
                    0:
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, TGocciaRegisterArray(nil), 0,
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    1:
                      begin
                        ApplyArgRegister0 :=
                          VMValueToRegisterFast(ArgsArray.Elements[0]);
                        SetupNewFrame(BytecodeFunction.FClosure,
                          CallThisRegister, TGocciaRegisterArray(nil), 1,
                          ApplyArgRegister0, RegisterUndefined, RegisterUndefined,
                          True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                      end;
                    2:
                      begin
                        ApplyArgRegister0 :=
                          VMValueToRegisterFast(ArgsArray.Elements[0]);
                        ApplyArgRegister1 :=
                          VMValueToRegisterFast(ArgsArray.Elements[1]);
                        SetupNewFrame(BytecodeFunction.FClosure,
                          CallThisRegister, TGocciaRegisterArray(nil), 2,
                          ApplyArgRegister0, ApplyArgRegister1, RegisterUndefined,
                          True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                      end;
                    3:
                      begin
                        ApplyArgRegister0 :=
                          VMValueToRegisterFast(ArgsArray.Elements[0]);
                        ApplyArgRegister1 :=
                          VMValueToRegisterFast(ArgsArray.Elements[1]);
                        ApplyArgRegister2 :=
                          VMValueToRegisterFast(ArgsArray.Elements[2]);
                        SetupNewFrame(BytecodeFunction.FClosure,
                          CallThisRegister, TGocciaRegisterArray(nil), 3,
                          ApplyArgRegister0, ApplyArgRegister1, ApplyArgRegister2,
                          True, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                      end;
                  else
                    begin
                      SetLength(RegisterArgs, ArgsArray.Elements.Count);
                      for I := 0 to High(RegisterArgs) do
                        RegisterArgs[I] :=
                          VMValueToRegisterFast(ArgsArray.Elements[I]);
                      SetupNewFrame(BytecodeFunction.FClosure,
                        CallThisRegister, RegisterArgs, Length(RegisterArgs),
                        RegisterUndefined, RegisterUndefined, RegisterUndefined,
                        False, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                    end;
                  end;
                  goto LDispatchNext;
                end;
              end;
            end;
          end;
        end;

        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaBytecodeFunctionValue) then
        begin
          BytecodeFunction := TGocciaBytecodeFunctionValue(FRegisters[A].ObjectValue);
          if Assigned(BytecodeFunction.FClosure) and
             Assigned(BytecodeFunction.FClosure.Template) and
             (not BytecodeFunction.FClosure.Template.IsAsync) and
             (not BytecodeFunction.FClosure.Template.IsGenerator) then
          begin
            CallThisRegister := FRegisters[A - 1];
            if not BytecodeFunction.FStrictThis then
              CallThisRegister := CoerceNonStrictThisRegister(
                CallThisRegister,
                BytecodeClosureGlobalThis(BytecodeFunction.FClosure,
                  FGlobalThisValue),
                BytecodeClosureExecutionRealm(BytecodeFunction.FClosure,
                  FRealm));
            if (C and 1) = 0 then
            begin
              SetLength(RegisterArgs, B);
              for I := 0 to B - 1 do
                RegisterArgs[I] := FRegisters[A + 1 + I];
              if (C and CALL_FLAG_TAIL) <> 0 then
                PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                  InitialFrameStackCount, SavedHandlerCount)
              else
                PushFrame(A, Frame.IP, Template, PrevCovLine,
                  ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                CallThisRegister, RegisterArgs, B,
                RegisterUndefined, RegisterUndefined, RegisterUndefined, False, True,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              goto LDispatchNext;
            end
            else if (FRegisters[B].Kind = grkObject) and
                    (FRegisters[B].ObjectValue is TGocciaArrayValue) then
            begin
              SetLength(RegisterArgs,
                TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count);
	              for I := 0 to High(RegisterArgs) do
	                RegisterArgs[I] := VMValueToRegisterFast(
	                  TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty(IntToStr(I)));
              if (C and CALL_FLAG_TAIL) <> 0 then
                PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                  InitialFrameStackCount, SavedHandlerCount)
              else
                PushFrame(A, Frame.IP, Template, PrevCovLine,
                  ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                CallThisRegister, RegisterArgs, Length(RegisterArgs),
                RegisterUndefined, RegisterUndefined, RegisterUndefined, False, True,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              goto LDispatchNext;
            end;
          end;
        end;

        if (C and 1) = 1 then
          CallArgs := AcquireArguments
        else
          CallArgs := AcquireArguments(B);
        try
          if (C and 1) = 1 then
          begin
            if GetRegister(B) is TGocciaArrayValue then
              for I := 0 to TGocciaArrayValue(GetRegister(B)).Elements.Count - 1 do
                CallArgs.Add(TGocciaArrayValue(GetRegister(B)).GetProperty(IntToStr(I)));
          end
          else
            for I := 0 to B - 1 do
              CallArgs.Add(GetRegister(A + 1 + I));
          if not (Assigned(GetRegister(A)) and
                  (GetRegister(A).IsCallable or
                   (GetRegister(A) is TGocciaProxyValue))) then
            ThrowNotCallableHere(GetRegister(A), GetRegister(A - 1));
          if (GetRegister(A) is TGocciaNativeFunctionValue) or
             (GetRegister(A) is TGocciaFunctionConstructorClassValue) or
             (GetRegister(A) is TGocciaBoundFunctionValue) or
             (GetRegister(A) is TGocciaProxyValue) then
          begin
            if TGocciaCallStack.Instance <> nil then
              SavedConstructFrameOk :=
                TGocciaCallStack.Instance.TryGetTopFrame(SavedConstructFrame)
            else
              SavedConstructFrameOk := False;
            EnterCurrentInstructionCallSite(PreviousCallSite);
            // See OP_CALL: stamp the recorded call-site position for a native
            // callee's created error; snapshotted above, restored below.
            StampCallSiteLocation(CurrentCallSite);
            try
              SetRegister(A, InvokeFunctionValue(GetRegister(A), CallArgs,
                GetRegister(A - 1)));
            finally
              LeaveGocciaCallSite(PreviousCallSite);
            end;
            if SavedConstructFrameOk and (TGocciaCallStack.Instance <> nil) then
              TGocciaCallStack.Instance.SetTopFrame(SavedConstructFrame);
          end
          else
            SetRegister(A, InvokeFunctionValue(GetRegister(A), CallArgs,
              GetRegister(A - 1)));
        finally
          ReleaseArguments(CallArgs);
        end;
      end;

      OP_CONSTRUCT:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaVMClassValue) then
        begin
          SetLength(RegisterArgs, C);
          for I := 0 to C - 1 do
            RegisterArgs[I] := FRegisters[B + 1 + I];
          FRegisters[A] := TGocciaVMClassValue(FRegisters[B].ObjectValue)
            .InstantiateRegisters(RegisterArgs);
        end
        else
        begin
          if not MayBeConstructor(GetRegister(B)) then
            ThrowNotConstructorHere(GetRegister(B));
          { A native constructor can capture a stack trace (`new Error(...)`),
            and this frame carries no position of its own. Snapshot the top
            frame, stamp it at the construct site so a trace captured during
            construction locates the `new`, then restore it on success so the
            stamp does not leak onto a later throw in the same function
            (`new Map(); JSON.parse('{')`). On a throw the frame unwinds. }
          if TGocciaCallStack.Instance <> nil then
            SavedConstructFrameOk :=
              TGocciaCallStack.Instance.TryGetTopFrame(SavedConstructFrame)
          else
            SavedConstructFrameOk := False;
          StampCallSiteLocation(CurrentCallSite);
          CallArgs := AcquireArguments(C);
          try
            for I := 0 to C - 1 do
              CallArgs.Add(GetRegister(B + 1 + I));
            EnterCurrentInstructionCallSite(PreviousCallSite);
            try
              SetRegister(A, ConstructValue(GetRegister(B), CallArgs));
            finally
              LeaveGocciaCallSite(PreviousCallSite);
            end;
          finally
            ReleaseArguments(CallArgs);
          end;
          if SavedConstructFrameOk and (TGocciaCallStack.Instance <> nil) then
            TGocciaCallStack.Instance.SetTopFrame(SavedConstructFrame);
        end;
      end;

      OP_CONSTRUCT_SPREAD:
      begin
        SpreadArray := TGocciaArrayValue(FRegisters[C].ObjectValue);
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaVMClassValue) then
        begin
          SetLength(RegisterArgs, SpreadArray.Elements.Count);
          for I := 0 to SpreadArray.Elements.Count - 1 do
            RegisterArgs[I] := VMValueToRegisterFast(
              SpreadArray.GetProperty(IntToStr(I)));
          FRegisters[A] := TGocciaVMClassValue(FRegisters[B].ObjectValue)
            .InstantiateRegisters(RegisterArgs);
        end
        else
        begin
          if not MayBeConstructor(GetRegister(B)) then
            ThrowNotConstructorHere(GetRegister(B));
          // Snapshot/stamp/restore as in OP_CONSTRUCT, so a successful spread
          // construct does not leave the caller frame stamped for a later throw.
          if TGocciaCallStack.Instance <> nil then
            SavedConstructFrameOk :=
              TGocciaCallStack.Instance.TryGetTopFrame(SavedConstructFrame)
          else
            SavedConstructFrameOk := False;
          StampCallSiteLocation(CurrentCallSite);
          CallArgs := AcquireArguments(SpreadArray.Elements.Count);
          try
            for I := 0 to SpreadArray.Elements.Count - 1 do
              CallArgs.Add(SpreadArray.GetProperty(IntToStr(I)));
            EnterCurrentInstructionCallSite(PreviousCallSite);
            try
              SetRegister(A, ConstructValue(GetRegister(B), CallArgs));
            finally
              LeaveGocciaCallSite(PreviousCallSite);
            end;
          finally
            ReleaseArguments(CallArgs);
          end;
          if SavedConstructFrameOk and (TGocciaCallStack.Instance <> nil) then
            TGocciaCallStack.Instance.SetTopFrame(SavedConstructFrame);
        end;
      end;

      OP_GET_ITER:
        SetRegister(A, GetIteratorValue(GetRegister(B), C <> 0));

      OP_ITER_NEXT:
      begin
        if (FRegisters[C].Kind = grkObject) and
           (FRegisters[C].ObjectValue is TGocciaIteratorValue) then
        begin
          IterResult := TGocciaIteratorValue(FRegisters[C].ObjectValue).DirectNext(DoneFlag);
          if DoneFlag then
            FRegisters[A] := RegisterUndefined
          else
            FRegisters[A] := VMValueToRegisterFast(IterResult);
          if DoneFlag then
            FRegisters[B] := RegisterBoolean(True)
          else
            FRegisters[B] := RegisterBoolean(False);
        end
        else if (FRegisters[C].Kind = grkObject) and
                (FRegisters[C].ObjectValue is TGocciaObjectValue) then
        begin
          IterResult := FRegisters[C].ObjectValue;
          NextMethod := IterResult.GetProperty(PROP_NEXT);
          if not Assigned(NextMethod) or
             (NextMethod is TGocciaUndefinedLiteralValue) or
             not NextMethod.IsCallable then
          begin
            FRegisters[A] := RegisterUndefined;
            FRegisters[B] := RegisterBoolean(True);
          end
          else
          begin
            CallArgs := AcquireArguments;
            try
              IterResult := InvokeCallable(NextMethod, CallArgs, IterResult);
            finally
              ReleaseArguments(CallArgs);
            end;

            IterResult := AwaitValue(IterResult);
            if IterResult.IsPrimitive then
              ThrowTypeError(Format(SErrorIteratorResultNotObject, [IterResult.ToStringLiteral.Value]),
                SSuggestIteratorResultObject);

            DoneValue := IterResult.GetProperty(PROP_DONE);
            if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
            begin
              FRegisters[A] := RegisterUndefined;
              FRegisters[B] := RegisterBoolean(True);
            end
            else
            begin
              FRegisters[A] := VMValueToRegisterFast(IterResult.GetProperty(PROP_VALUE));
              FRegisters[B] := RegisterBoolean(False);
            end;
          end;
        end
        else
        begin
          FRegisters[A] := RegisterUndefined;
          FRegisters[B] := RegisterBoolean(True);
        end;
      end;

      OP_ASYNC_ITER_NEXT:
      begin
        if (FRegisters[C].Kind = grkObject) and
           (FRegisters[C].ObjectValue is TGocciaIteratorValue) then
        begin
          IterResult := TGocciaIteratorValue(FRegisters[C].ObjectValue).DirectNext(DoneFlag);
          SetRegister(A, CreateIteratorResult(IterResult, DoneFlag));
        end
        else if (FRegisters[C].Kind = grkObject) and
                (FRegisters[C].ObjectValue is TGocciaObjectValue) then
        begin
          IterResult := FRegisters[C].ObjectValue;
          NextMethod := IterResult.GetProperty(PROP_NEXT);
          if not Assigned(NextMethod) or
             (NextMethod is TGocciaUndefinedLiteralValue) or
             not NextMethod.IsCallable then
            ThrowTypeError(SErrorAsyncIteratorNextNotCallable,
              SSuggestAsyncIteratorProtocol);

          CallArgs := AcquireArguments;
          try
            IterResult := InvokeCallable(NextMethod, CallArgs, IterResult);
          finally
            ReleaseArguments(CallArgs);
          end;
          SetRegister(A, IterResult);
        end
        else
          SetRegister(A, CreateIteratorResult(
            TGocciaUndefinedLiteralValue.UndefinedValue, True));
      end;

      OP_ITER_UNPACK:
      begin
        IterResult := GetRegister(C);
        if IterResult.IsPrimitive then
          ThrowTypeError(Format(SErrorIteratorResultNotObject,
            [IterResult.ToStringLiteral.Value]), SSuggestIteratorResultObject);

        DoneValue := IterResult.GetProperty(PROP_DONE);
        if Assigned(DoneValue) and DoneValue.ToBooleanLiteral.Value then
        begin
          FRegisters[A] := RegisterUndefined;
          FRegisters[B] := RegisterBoolean(True);
        end
        else
        begin
          IteratorElementValue := IterResult.GetProperty(PROP_VALUE);
          if not Assigned(IteratorElementValue) then
            IteratorElementValue := TGocciaUndefinedLiteralValue.UndefinedValue;
          FRegisters[A] := VMValueToRegisterFast(IteratorElementValue);
          FRegisters[B] := RegisterBoolean(False);
        end;
      end;

      OP_SET_FUNCTION_NAME:
        SetFunctionNameFromKey(GetRegister(A), GetRegister(B), C);

      OP_ITER_CLOSE:
        if FRegisters[A].Kind = grkObject then
        begin
          if C = ITER_CLOSE_PRESERVE_UNLESS_GENERATOR_RETURN then
          begin
            if (FRegisters[B].Kind = grkObject) and
               Assigned(GActiveBytecodeGenerator) and
               Assigned(GActiveBytecodeGenerator.FReturnSentinel) and
               (FRegisters[B].ObjectValue =
                GActiveBytecodeGenerator.FReturnSentinel) then
              CloseRawIterator(FRegisters[A].ObjectValue)
            else
              CloseRawIteratorPreservingError(FRegisters[A].ObjectValue);
          end
          else if C = ITER_CLOSE_PRESERVE_ERROR then
            CloseRawIteratorPreservingError(FRegisters[A].ObjectValue, B <> 0)
          else if B <> 0 then
            CloseRawAsyncIterator(FRegisters[A].ObjectValue)
          else
            CloseRawIterator(FRegisters[A].ObjectValue);
        end;

      OP_AWAIT:
      begin
        if Assigned(FCurrentAsyncPromise) and Assigned(Template) and
           Template.IsAsync then
        begin
          if Template.IsGenerator and Assigned(GActiveBytecodeGenerator) then
            AwaitContinuation := GActiveBytecodeGenerator
          else if not Template.IsGenerator then
            AwaitContinuation := TGocciaBytecodeGeneratorObjectValue.CreateRegisters(
              Self, FCurrentClosure, GetLocalRegister(0),
              CurrentArgumentsSnapshot, False)
          else
            AwaitContinuation := nil;

          if Assigned(AwaitContinuation) then
          begin
            AwaitContinuation.CaptureContinuation(Frame, SavedHandlerCount,
              PrevCovLine, A, Frame.IP);
            AwaitContinuation.FState := bgsSuspendedYield;
            AwaitPromise := PromiseResolveIntrinsic(GetRegister(B));
            AwaitPromise.InvokeThen(
              TGocciaVMAsyncAwaitContinuationValue.Create(Self,
                AwaitContinuation, FCurrentAsyncPromise, bgrkNext,
                Template.IsGenerator),
              TGocciaVMAsyncAwaitContinuationValue.Create(Self,
                AwaitContinuation, FCurrentAsyncPromise, bgrkThrow,
                Template.IsGenerator));
            raise EGocciaBytecodeAsyncSuspend.Create('');
          end;
        end;
        SetRegister(A, AwaitValue(GetRegister(B)));
      end;

      OP_YIELD:
      begin
        if Assigned(GActiveBytecodeGenerator) then
        begin
          if (C and 1) <> 0 then
            GActiveBytecodeGenerator.HandleYieldDelegate(
              FRegisters[A], B, Frame, SavedHandlerCount, PrevCovLine,
              InstructionStartIP)
          else
            GActiveBytecodeGenerator.HandleYield(
              FRegisters[A], B, Frame, SavedHandlerCount, PrevCovLine,
              Frame.IP);
        end
        else if A <> B then
          FRegisters[B] := FRegisters[A];
      end;

      OP_SETUP_AUTO_ACCESSOR_CONST:
        SetupAutoAccessorValue(Template.GetConstantUnchecked(C).StringValue,
          B, RegisterToValue(FRegisters[A]));

      OP_SETUP_AUTO_ACCESSOR_DYNAMIC:
        SetupAutoAccessorValueByKey(RegisterToValue(FRegisters[A]),
          Template.GetConstantUnchecked(C).StringValue, B);

      OP_BEGIN_DECORATORS:
        BeginDecorators(RegisterToValue(FRegisters[A]), RegisterToValue(FRegisters[A + 1]));

      OP_APPLY_ELEMENT_DECORATOR_CONST:
        if B <> 0 then
          ApplyElementDecorator(RegisterToValue(FRegisters[A]),
            Template.GetConstantUnchecked(C).StringValue,
            RegisterToValue(FRegisters[B]))
        else
          ApplyElementDecorator(RegisterToValue(FRegisters[A]),
            Template.GetConstantUnchecked(C).StringValue);

      OP_APPLY_CLASS_DECORATOR:
        ApplyClassDecorator(RegisterToValue(FRegisters[A]));

      OP_FINISH_DECORATORS:
        SetRegister(A, FinishDecorators(RegisterToValue(FRegisters[A])));

      OP_GET_GLOBAL:
      begin
        if Assigned(FCurrentDynamicVarScope) or not Assigned(FGlobalScope) then
        begin
          GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
          if HasDynamicVarBinding(FCurrentDynamicVarScope, GlobalName) then
            FRegisters[A] := VMValueToRegisterFast(
              FCurrentDynamicVarScope.GetValue(GlobalName))
          else if Assigned(FGlobalScope) and
                  FGlobalScope.TryGetBindingValue(GlobalName, GlobalBindingValue) then
            FRegisters[A] := VMValueToRegisterFast(GlobalBindingValue)
          else
            FRegisters[A] := RegisterUndefined;
        end
        else
        begin
          // Per-site inline cache keyed by the name-constant index.  It serves
          // either an own lexical-map entry or an ordinary global object's own
          // plain-data entry.  Both modes re-read the live value by a
          // version-validated entry index; exotic objects, accessors, lazy
          // descriptors, and dynamic scopes remain on the named lookup path.
          GlobalReadCache := Template.GlobalReadCacheSlot(DecodeBx(Instruction));
          if Assigned(GlobalReadCache) and
             (GlobalReadCache^.Scope = Pointer(FGlobalScope)) and
             (GlobalReadCache^.ObjectValue = nil) and
             FGlobalScope.TryGetLexicalValueAt(GlobalReadCache^.EntryIndex,
               GlobalReadCache^.Version, GlobalBindingValue) then
            FRegisters[A] := VMValueToRegisterFast(GlobalBindingValue)
          else if Assigned(GlobalReadCache) and
             (GlobalReadCache^.Scope = Pointer(FGlobalScope)) and
             (FGlobalScope.ThisValue is TGocciaObjectValue) and
             (GlobalReadCache^.ObjectValue =
               Pointer(FGlobalScope.ThisValue)) and
             VMGlobalObjectBindingCacheStillPrecedes(FGlobalScope,
               GlobalReadCache) and
             VMTryGetCachedGlobalOwnDataProperty(
               TGocciaObjectValue(FGlobalScope.ThisValue),
               GlobalReadCache^.EntryIndex, GlobalReadCache^.Version,
               GlobalBindingValue) then
            FRegisters[A] := VMValueToRegisterFast(GlobalBindingValue)
          else
          begin
            GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
            if Assigned(GlobalReadCache) then
            begin
              GlobalReadCache^.Scope := nil;
              GlobalReadCache^.ObjectValue := nil;
              GlobalReadCache^.ObjectBindingKind :=
                GLOBAL_READ_OBJECT_BINDING_NONE;
              if FGlobalScope.TryGetBindingValueFillCache(GlobalName,
                GlobalReadCache^.EntryIndex, GlobalReadCache^.Version,
                GlobalBindingValue) then
              begin
                GlobalBindingEntryIndex := GlobalReadCache^.EntryIndex;
                GlobalBindingVersion := GlobalReadCache^.Version;
                if (FGlobalScope.ThisValue is TGocciaObjectValue) and
                   ((not FGlobalScope.ContainsOwnLexicalBinding(GlobalName) and
                     FGlobalScope.ContainsOwnVarBinding(GlobalName)) or
                    (FGlobalScope.IsBuiltInBinding(GlobalName) and
                     FGlobalScope.IsGlobalObjectBackedBinding(GlobalName))) and
                   VMTryGetGlobalOwnDataPropertyFillCache(
                     TGocciaObjectValue(FGlobalScope.ThisValue), GlobalName,
                     GlobalReadCache^.EntryIndex,
                     GlobalReadCache^.Version) then
                begin
                  GlobalReadCache^.Scope := Pointer(FGlobalScope);
                  GlobalReadCache^.ObjectValue :=
                    Pointer(FGlobalScope.ThisValue);
                  if FGlobalScope.ContainsOwnVarBinding(GlobalName) then
                    GlobalReadCache^.ObjectBindingKind :=
                      GLOBAL_READ_OBJECT_BINDING_VAR
                  else
                  begin
                    GlobalReadCache^.ObjectBindingKind :=
                      GLOBAL_READ_OBJECT_BINDING_BUILTIN;
                    GlobalReadCache^.BindingEntryIndex :=
                      GlobalBindingEntryIndex;
                    GlobalReadCache^.BindingVersion :=
                      GlobalBindingVersion;
                  end;
                end
                else if GlobalReadCache^.EntryIndex >= 0 then
                  GlobalReadCache^.Scope := Pointer(FGlobalScope);
                FRegisters[A] := VMValueToRegisterFast(GlobalBindingValue);
              end
              else
                FRegisters[A] := RegisterUndefined;
            end
            else if FGlobalScope.TryGetBindingValue(GlobalName,
              GlobalBindingValue) then
              FRegisters[A] := VMValueToRegisterFast(GlobalBindingValue)
            else
              FRegisters[A] := RegisterUndefined;
          end;
        end;
      end;

      OP_SET_GLOBAL:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if HasDynamicVarBinding(FCurrentDynamicVarScope, GlobalName) then
          FCurrentDynamicVarScope.AssignBinding(GlobalName,
            RegisterToValue(FRegisters[A]))
        else if Assigned(FGlobalScope) then
        begin
          if not FGlobalScope.TryAssignExistingBinding(GlobalName,
            RegisterToValue(FRegisters[A])) then
          begin
            if ((GlobalName = PROP_GOCCIA) or (GlobalName = PROP_GLOBAL_THIS)) and
               (FGlobalScope.ThisValue is TGocciaObjectValue) and
               TGocciaObjectValue(FGlobalScope.ThisValue).HasProperty(GlobalName) then
            begin
              CurrentInstructionDebugLocation(DebugLine, DebugColumn);
              raise TGocciaTypeError.Create(
                Format(SErrorAssignToConstant, [GlobalName]),
                DebugLine, DebugColumn,
                '', nil, SSuggestUseLetNotConst);
            end;
            ThrowReferenceError(Format(SErrorUndefinedVariable, [GlobalName]));
          end;
        end;
      end;

      OP_SET_GLOBAL_LOOSE:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if HasDynamicVarBinding(FCurrentDynamicVarScope, GlobalName) then
          FCurrentDynamicVarScope.AssignBinding(GlobalName,
            RegisterToValue(FRegisters[A]), 0, 0, True)
        else if Assigned(FGlobalScope) then
        begin
          GlobalBindingValue := RegisterToValue(FRegisters[A]);
          if FGlobalScope.ContainsOwnVarBinding(GlobalName) and
             (FGlobalScope.ThisValue is TGocciaObjectValue) and
             VMTrySetOwnWritableDataProperty(
               TGocciaObjectValue(FGlobalScope.ThisValue), GlobalName,
               GlobalBindingValue) then
            goto LDispatchNext;
          if (not FGlobalScope.TryAssignExistingBinding(GlobalName,
            GlobalBindingValue, True)) and
             (FGlobalScope.ThisValue is TGocciaObjectValue) then
          begin
            if ((GlobalName = PROP_GOCCIA) or (GlobalName = PROP_GLOBAL_THIS)) and
               TGocciaObjectValue(FGlobalScope.ThisValue).HasProperty(GlobalName) then
            begin
              CurrentInstructionDebugLocation(DebugLine, DebugColumn);
              raise TGocciaTypeError.Create(
                Format(SErrorAssignToConstant, [GlobalName]),
                DebugLine, DebugColumn,
                '', nil, SSuggestUseLetNotConst);
            end;
            TGocciaObjectValue(FGlobalScope.ThisValue).AssignPropertyWithReceiver(
              GlobalName, GlobalBindingValue, FGlobalScope.ThisValue);
          end;
        end;
      end;

      OP_HAS_GLOBAL:
      begin
        if not Assigned(FCurrentDynamicVarScope) and Assigned(FGlobalScope) then
        begin
          GlobalReadCache := Template.GlobalReadCacheSlot(
            DecodeBx(Instruction));
          if Assigned(GlobalReadCache) and
             (GlobalReadCache^.Scope = Pointer(FGlobalScope)) and
             (((GlobalReadCache^.ObjectValue = nil) and
               FGlobalScope.HasLexicalBindingAt(
                 GlobalReadCache^.EntryIndex, GlobalReadCache^.Version)) or
              ((FGlobalScope.ThisValue is TGocciaObjectValue) and
               (GlobalReadCache^.ObjectValue =
                 Pointer(FGlobalScope.ThisValue)) and
               VMGlobalObjectBindingCacheStillPrecedes(FGlobalScope,
                 GlobalReadCache) and
               VMTryGetCachedGlobalOwnDataProperty(
                 TGocciaObjectValue(FGlobalScope.ThisValue),
                 GlobalReadCache^.EntryIndex, GlobalReadCache^.Version,
                 GlobalBindingValue))) then
          begin
            FRegisters[A] := RegisterBoolean(True);
            goto LDispatchNext;
          end;
        end;

        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        FRegisters[A] := RegisterBoolean(
          HasDynamicVarBinding(FCurrentDynamicVarScope, GlobalName) or
          (Assigned(FGlobalScope) and FGlobalScope.Contains(GlobalName)));
      end;

      OP_DELETE_GLOBAL:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if HasDynamicVarBinding(FCurrentDynamicVarScope, GlobalName) then
          FRegisters[A] := RegisterBoolean(
            FCurrentDynamicVarScope.DeleteBinding(GlobalName))
        else if Assigned(FGlobalScope) then
          FRegisters[A] := RegisterBoolean(FGlobalScope.DeleteBinding(GlobalName))
        else
          FRegisters[A] := RegisterBoolean(True);
      end;

      OP_IMPORT:
        begin
          if Assigned(Template.DebugInfo) and
             (Template.DebugInfo.SourceFile <> '') then
            GlobalName := Template.DebugInfo.SourceFile
          else
            GlobalName := FCurrentModuleSourcePath;
          SetRegister(A, ImportModuleValue(
            Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue,
            GlobalName));
        end;

      OP_IMPORT_DEFER:
        begin
          if Assigned(Template.DebugInfo) and
             (Template.DebugInfo.SourceFile <> '') then
            GlobalName := Template.DebugInfo.SourceFile
          else
            GlobalName := FCurrentModuleSourcePath;
          SetRegister(A, ImportDeferredModuleNamespaceValue(
            Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue,
            GlobalName));
        end;

      OP_IMPORT_SOURCE:
        begin
          if Assigned(Template.DebugInfo) and
             (Template.DebugInfo.SourceFile <> '') then
            GlobalName := Template.DebugInfo.SourceFile
          else
            GlobalName := FCurrentModuleSourcePath;
          SetRegister(A, ImportModuleSourceValue(
            Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue,
            GlobalName));
        end;

      OP_GET_IMPORT_BINDING:
        begin
          GlobalName := Template.GetConstantUnchecked(
            DecodeBx(Instruction)).StringValue;
          if (FRegisters[A].Kind = grkObject) and
             (FRegisters[A].ObjectValue is TGocciaModuleNamespaceObject) then
          begin
            if not TGocciaModuleNamespaceObject(
               FRegisters[A].ObjectValue).TryGetExportValue(
               GlobalName, GlobalBindingValue) then
            begin
              if Assigned(TGocciaModuleNamespaceObject(
                 FRegisters[A].ObjectValue).Module) then
                ThrowSyntaxError(Format('Module "%s" has no export named "%s"',
                  [TGocciaModuleNamespaceObject(FRegisters[A].ObjectValue)
                     .Module.Path, GlobalName]))
              else
                ThrowSyntaxError(Format('Module has no export named "%s"',
                  [GlobalName]));
            end;
            SetRegister(A, GlobalBindingValue);
          end
          else
            SetRegister(A, GetPropertyValue(GetRegister(A), GlobalName));
        end;

      OP_EXPORT:
        begin
          if Assigned(Template.DebugInfo) and
             (Template.DebugInfo.SourceFile <> '') then
            GlobalName := Template.DebugInfo.SourceFile
          else
            GlobalName := FCurrentModuleSourcePath;
          ExportBindingValue(
            Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue,
            GetRegister(A), GlobalName);
        end;

      // ES2026 §13.3.12.1 — import.meta binds lexically to the defining module
      OP_IMPORT_META:
        if Assigned(Template.DebugInfo) and (Template.DebugInfo.SourceFile <> '') then
          SetRegister(A, GetOrCreateImportMeta(Template.DebugInfo.SourceFile,
            FResolveModuleURL))
        else
          SetRegister(A, GetOrCreateImportMeta(FCurrentModuleSourcePath,
            FResolveModuleURL));

      // ES2026 §13.3.12.1 — new.target reads the current frame's newTarget
      OP_NEW_TARGET:
        if Assigned(FCurrentNewTarget) then
          SetRegister(A, FCurrentNewTarget)
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);

      // ES2026 §13.3.10.1 ImportCall — import(specifier)
      OP_DYNAMIC_IMPORT:
      begin
        DynImportPromise := TGocciaPromiseValue.Create;
        if (TGarbageCollector.Instance <> nil) then
          TGarbageCollector.Instance.AddTempRoot(DynImportPromise);
        try
          try
            if Assigned(Template.DebugInfo) and (Template.DebugInfo.SourceFile <> '') then
              GlobalName := Template.DebugInfo.SourceFile
            else
              GlobalName := FCurrentModuleSourcePath;

            SpecifierString := ToPrimitive(RegisterToValue(FRegisters[B]),
              tphString).ToStringLiteral.Value;
            case C of
              Ord(icpEvaluation):
                if (TGocciaMicrotaskQueue.Instance <> nil) then
                begin
                  DynImportTask.Handler := TGocciaVMDynamicImportStartValue.Create(
                    Self, DynImportPromise, SpecifierString, GlobalName);
                  DynImportTask.Value :=
                    TGocciaUndefinedLiteralValue.UndefinedValue;
                  DynImportTask.ResultPromise := nil;
                  DynImportTask.ReactionType := prtFulfill;
                  TGocciaMicrotaskQueue.Instance.Enqueue(DynImportTask);
                end
                else
                  ResolveDynamicImportPromise(DynImportPromise,
                    SpecifierString, GlobalName);
              Ord(icpSource):
                DynImportPromise.Resolve(ImportModuleSourceValue(
                  SpecifierString, GlobalName));
              Ord(icpDefer):
                DynImportPromise.Resolve(ImportDeferredModuleNamespaceValue(
                  SpecifierString, GlobalName));
            else
              raise Exception.CreateFmt(
                'Unsupported dynamic import phase: %d', [C]);
            end;
          except
            on E: EGocciaBytecodeThrow do
              DynImportPromise.Reject(E.ThrownValue);
            on E: TGocciaThrowValue do
              DynImportPromise.Reject(E.Value);
            on E: TGocciaSyntaxError do
              DynImportPromise.Reject(
                CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
            on E: TGocciaTypeError do
              DynImportPromise.Reject(
                CreateErrorObject(TYPE_ERROR_NAME, E.Message));
            on E: TGocciaReferenceError do
              DynImportPromise.Reject(
                CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
            on E: TGocciaTimeoutError do
              raise;
            on E: TGocciaInstructionLimitError do
              raise;
            on E: TGocciaMemoryLimitError do
              raise;
            on E: EGocciaCapabilityAuditDeliveryError do
              raise;
            on E: Exception do
            begin
              if IsEngineIntegrityFault(E) then
                raise;
              DynImportPromise.Reject(
                CreateErrorObject(ERROR_NAME, E.Message));
            end;
          end;
          SetRegister(A, DynImportPromise);
        finally
          if (TGarbageCollector.Instance <> nil) then
            TGarbageCollector.Instance.RemoveTempRoot(DynImportPromise);
        end;
      end;

      // ES2026 §13.3.10.1 ImportCall — import(specifier, options)
      OP_DYNAMIC_IMPORT_OPTIONS,
      OP_DYNAMIC_IMPORT_SOURCE_OPTIONS,
      OP_DYNAMIC_IMPORT_DEFER_OPTIONS:
      begin
        DynImportPromise := TGocciaPromiseValue.Create;
        if (TGarbageCollector.Instance <> nil) then
          TGarbageCollector.Instance.AddTempRoot(DynImportPromise);
        try
          try
            if Assigned(Template.DebugInfo) and (Template.DebugInfo.SourceFile <> '') then
              GlobalName := Template.DebugInfo.SourceFile
            else
              GlobalName := FCurrentModuleSourcePath;

            SpecifierString := ToPrimitive(RegisterToValue(FRegisters[B]),
              tphString).ToStringLiteral.Value;
            AttributeType := DynamicImportAttributeType(
              RegisterToValue(FRegisters[C]));
            SpecifierString := EncodeImportSpecifierAttribute(
              SpecifierString, AttributeType);
            case TGocciaOpCode(Op) of
              OP_DYNAMIC_IMPORT_OPTIONS:
                if (TGocciaMicrotaskQueue.Instance <> nil) then
                begin
                  DynImportTask.Handler := TGocciaVMDynamicImportStartValue.Create(
                    Self, DynImportPromise, SpecifierString, GlobalName);
                  DynImportTask.Value :=
                    TGocciaUndefinedLiteralValue.UndefinedValue;
                  DynImportTask.ResultPromise := nil;
                  DynImportTask.ReactionType := prtFulfill;
                  TGocciaMicrotaskQueue.Instance.Enqueue(DynImportTask);
                end
                else
                  ResolveDynamicImportPromise(DynImportPromise, SpecifierString,
                    GlobalName);
              OP_DYNAMIC_IMPORT_SOURCE_OPTIONS:
                DynImportPromise.Resolve(ImportModuleSourceValue(
                  SpecifierString, GlobalName));
              OP_DYNAMIC_IMPORT_DEFER_OPTIONS:
                DynImportPromise.Resolve(ImportDeferredModuleNamespaceValue(
                  SpecifierString, GlobalName));
            end;
          except
            on E: EGocciaBytecodeThrow do
              DynImportPromise.Reject(E.ThrownValue);
            on E: TGocciaThrowValue do
              DynImportPromise.Reject(E.Value);
            on E: TGocciaSyntaxError do
              DynImportPromise.Reject(
                CreateErrorObject(SYNTAX_ERROR_NAME, E.Message));
            on E: TGocciaTypeError do
              DynImportPromise.Reject(
                CreateErrorObject(TYPE_ERROR_NAME, E.Message));
            on E: TGocciaReferenceError do
              DynImportPromise.Reject(
                CreateErrorObject(REFERENCE_ERROR_NAME, E.Message));
            on E: TGocciaTimeoutError do
              raise;
            on E: TGocciaInstructionLimitError do
              raise;
            on E: TGocciaMemoryLimitError do
              raise;
            on E: EGocciaCapabilityAuditDeliveryError do
              raise;
            on E: Exception do
            begin
              if IsEngineIntegrityFault(E) then
                raise;
              DynImportPromise.Reject(
                CreateErrorObject(ERROR_NAME, E.Message));
            end;
          end;
          SetRegister(A, DynImportPromise);
        finally
          if (TGarbageCollector.Instance <> nil) then
            TGarbageCollector.Instance.RemoveTempRoot(DynImportPromise);
        end;
      end;

      // TC39 Explicit Resource Management: OP_USING_INIT
      // A=dest (dispose method), B=value, C=flags (0=sync, 1=async)
      // Validates value has [Symbol.dispose]/[Symbol.asyncDispose], stores method in A.
      // For null/undefined, stores null. Throws TypeError if not disposable.
      OP_USING_INIT:
      begin
        LeftValue := RegisterToValue(FRegisters[B]);
        if (LeftValue is TGocciaUndefinedLiteralValue) or
           (LeftValue is TGocciaNullLiteralValue) then
          FRegisters[A] := RegisterNull
        else
        begin
          if C = 1 then
          begin
            RightValue := nil;
            if LeftValue is TGocciaObjectValue then
            begin
              RightValue := TGocciaObjectValue(LeftValue).GetSymbolProperty(
                TGocciaSymbolValue.WellKnownAsyncDispose);
              if Assigned(RightValue) and
                 not (RightValue is TGocciaUndefinedLiteralValue) and
                 not (RightValue is TGocciaNullLiteralValue) then
              begin
                if not RightValue.IsCallable then
                  RightValue := GetDisposeMethod(LeftValue, dhAsyncDispose)
                else
                  RightValue := TGocciaVMAsyncDisposeMethodValue.Create(
                    RightValue);
              end
              else
              begin
                RightValue := TGocciaObjectValue(LeftValue).GetSymbolProperty(
                  TGocciaSymbolValue.WellKnownDispose);
                if Assigned(RightValue) and
                   not (RightValue is TGocciaUndefinedLiteralValue) and
                   not (RightValue is TGocciaNullLiteralValue) then
                begin
                  if not RightValue.IsCallable then
                    RightValue := GetDisposeMethod(LeftValue, dhAsyncDispose)
                  else
                    RightValue := TGocciaVMSyncDisposeFallbackValue.Create(
                      RightValue);
                end
                else
                  RightValue := nil;
              end;
            end;
          end
          else
            RightValue := GetDisposeMethod(LeftValue, dhSyncDispose);
          if not Assigned(RightValue) then
          begin
            if C = 1 then
              raise EGocciaBytecodeThrow.Create(
                CreateErrorObject(TYPE_ERROR_NAME,
                  'Value is not disposable (missing [Symbol.asyncDispose] and [Symbol.dispose])'))
            else
              raise EGocciaBytecodeThrow.Create(
                CreateErrorObject(TYPE_ERROR_NAME,
                  'Value is not disposable (missing [Symbol.dispose])'));
          end;
          SetRegister(A, RightValue);
        end;
      end;

      // TC39 Explicit Resource Management: OP_USING_DISPOSE
      // A=errorAccum, B=disposeMethod, C=resource
      // Calls disposeMethod.call(resource). On error, wraps with SuppressedError
      // if errorAccum already holds an error.
      // TC39 Explicit Resource Management: OP_USING_DISPOSE
      // A=errorAccum, B=disposeMethod (overwritten with call result), C=resource
      // Calls disposeMethod.call(resource). Stores result in B for OP_AWAIT.
      // On error, wraps with SuppressedError in A.
      OP_USING_DISPOSE:
      begin
        // Stamp the disposal site onto the top frame so an auto-SuppressedError
        // created below (a double fault: dispose throws while an error is
        // pending) records this location instead of the deferred frame's 0:0,
        // matching the tree-walk interpreter. Snapshot/restore so it does not
        // perturb the location seen by later instructions.
        if TGocciaCallStack.Instance <> nil then
          SavedConstructFrameOk :=
            TGocciaCallStack.Instance.TryGetTopFrame(SavedConstructFrame)
        else
          SavedConstructFrameOk := False;
        StampCurrentInstructionLocation;
        try
        LeftValue := RegisterToValue(FRegisters[B]); // dispose method
        if Assigned(LeftValue) and not (LeftValue is TGocciaNullLiteralValue) and
           not (LeftValue is TGocciaUndefinedLiteralValue) and
           LeftValue.IsCallable then
        begin
          try
            // Clear B before the call so that if it throws, the follow-up
            // OP_AWAIT sees null instead of the stale dispose function.
            FRegisters[B] := RegisterNull;
            RightValue := TGocciaFunctionBase(LeftValue).CallNoArgs(
              RegisterToValue(FRegisters[C]));
            // Store result in B so a follow-up OP_AWAIT can await it
            if Assigned(RightValue) then
              SetRegister(B, RightValue);
          except
            on E: EGocciaBytecodeThrow do
            begin
              RightValue := RegisterToValue(FRegisters[A]);
              if Assigned(RightValue) and
                 (RightValue <> TGocciaHoleValue.HoleValue) then
                SetRegister(A, CreateSuppressedErrorObject(E.ThrownValue, RightValue))
              else
                SetRegister(A, E.ThrownValue);
            end;
            on E: TGocciaThrowValue do
            begin
              RightValue := RegisterToValue(FRegisters[A]);
              if Assigned(RightValue) and
                 (RightValue <> TGocciaHoleValue.HoleValue) then
                SetRegister(A, CreateSuppressedErrorObject(E.Value, RightValue))
              else
                SetRegister(A, E.Value);
            end;
            on E: TGocciaTimeoutError do
              raise;
            on E: TGocciaInstructionLimitError do
              raise;
            on E: TGocciaMemoryLimitError do
              raise;
            on E: EGocciaCapabilityAuditDeliveryError do
              raise;
            on E: Exception do
            begin
              if IsEngineIntegrityFault(E) then
                raise;
              // Preserve typed error names for native Goccia exceptions
              if E is TGocciaTypeError then
                LeftValue := CreateErrorObject(TYPE_ERROR_NAME, E.Message)
              else if E is TGocciaReferenceError then
                LeftValue := CreateErrorObject(REFERENCE_ERROR_NAME, E.Message)
              else if E is TGocciaSyntaxError then
                LeftValue := CreateErrorObject(SYNTAX_ERROR_NAME, E.Message)
              else
                LeftValue := CreateErrorObject(ERROR_NAME, E.Message);
              RightValue := RegisterToValue(FRegisters[A]);
              if Assigned(RightValue) and
                 (RightValue <> TGocciaHoleValue.HoleValue) then
                SetRegister(A, CreateSuppressedErrorObject(LeftValue, RightValue))
              else
                SetRegister(A, LeftValue);
            end;
          end;
        end;
        finally
          if SavedConstructFrameOk and (TGocciaCallStack.Instance <> nil) then
            TGocciaCallStack.Instance.SetTopFrame(SavedConstructFrame);
        end;
      end;

      OP_THROW: raise EGocciaBytecodeThrow.Create(GetRegister(A));

      OP_NOT:
        FRegisters[A] := RegisterBoolean(not RegisterToBoolean(FRegisters[B]));

      OP_TO_BOOL:
        FRegisters[A] := RegisterBoolean(RegisterToBoolean(FRegisters[B]));

      OP_DEFINE_ACCESSOR_CONST:
      begin
        GlobalName := Template.GetConstantUnchecked(C).StringValue;
        if (B and ACCESSOR_FLAG_STATIC) <> 0 then
        begin
          if IsBytecodePrivateKey(GlobalName) then
            DeclareBytecodePrivateNameForClass(
              RegisterToValue(FRegisters[A]), GlobalName, True);
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineStaticSetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]))
          else
            DefineStaticGetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]));
        end
        else
        begin
          if IsBytecodePrivateKey(GlobalName) then
            DeclareBytecodePrivateNameForClass(
              RegisterToValue(FRegisters[A]), GlobalName);
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineSetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]))
          else
            DefineGetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]));
        end;
      end;

      OP_DEFINE_ACCESSOR_DYNAMIC:
      begin
        if (B and ACCESSOR_FLAG_STATIC) <> 0 then
        begin
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineStaticSetterPropertyByKey(RegisterToValue(FRegisters[A]),
              RegisterToValue(FRegisters[C]), RegisterToValue(FRegisters[A + 1]))
          else
            DefineStaticGetterPropertyByKey(RegisterToValue(FRegisters[A]),
              RegisterToValue(FRegisters[C]), RegisterToValue(FRegisters[A + 1]));
        end
        else
        begin
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineSetterPropertyByKey(RegisterToValue(FRegisters[A]),
              RegisterToValue(FRegisters[C]), RegisterToValue(FRegisters[A + 1]))
          else
            DefineGetterPropertyByKey(RegisterToValue(FRegisters[A]),
              RegisterToValue(FRegisters[C]), RegisterToValue(FRegisters[A + 1]));
        end;
      end;

      OP_COLLECTION_OP:
      begin
        case B of
          COLLECTION_OP_SPREAD_OBJECT:
            if (FRegisters[A].Kind = grkObject) and
               (FRegisters[A].ObjectValue is TGocciaObjectValue) then
              SpreadObjectIntoValue(TGocciaObjectValue(FRegisters[A].ObjectValue),
                RegisterToValue(FRegisters[C]));

          COLLECTION_OP_OBJECT_REST:
            begin
              if (A + 1 < FRegisterCount) and
                 (FRegisters[A + 1].Kind = grkObject) and
                 (FRegisters[A + 1].ObjectValue is TGocciaArrayValue) then
                SetRegister(A, ObjectRestValue(RegisterToValue(FRegisters[C]),
                  TGocciaArrayValue(FRegisters[A + 1].ObjectValue)))
              else
                SetRegister(A, ObjectRestValue(RegisterToValue(FRegisters[C]), nil));
            end;

          COLLECTION_OP_SPREAD_ITERABLE_INTO_ARRAY:
            begin
              DoneValue := IterableToArray(RegisterToValue(FRegisters[C]));
              if (FRegisters[A].Kind = grkObject) and
                 (FRegisters[A].ObjectValue is TGocciaArrayValue) and
                 (DoneValue is TGocciaArrayValue) then
	                for I := 0 to TGocciaArrayValue(DoneValue).Elements.Count - 1 do
	                  TGocciaArrayValue(FRegisters[A].ObjectValue).Elements.Add(
	                    TGocciaArrayValue(DoneValue).GetProperty(IntToStr(I)));
            end;

          COLLECTION_OP_TRY_ITERABLE_TO_ARRAY:
            begin
              if TryIterableToArray(RegisterToValue(FRegisters[C]), SpreadArray) then
                SetRegister(A, SpreadArray)
              else
                FRegisters[A] := RegisterUndefined;
            end;

        else
          raise Exception.CreateFmt('Unsupported collection helper mode: %d', [B]);
        end;
      end;

      OP_VALIDATE_VALUE:
      begin
        case B of
          VALIDATE_OP_REQUIRE_OBJECT:
            begin
              if FRegisters[A].Kind in [grkNull, grkUndefined] then
                ThrowTypeError(Format(SErrorCannotDestructureNotObject, [RegisterToValue(FRegisters[A]).ToStringLiteral.Value]),
                  SSuggestDestructureRequiresObject);
            end;

          // ES2026 §6.2.5.5 GetValue step 3.a on a computed member base. C holds
          // the key register, still uncoerced (this runs before OP_TO_PROPERTY_KEY).
          VALIDATE_OP_REQUIRE_OBJECT_FOR_MEMBER:
            RequireCoercibleBaseRegister(FRegisters[A], FRegisters[C], False);

          VALIDATE_OP_REQUIRE_ITERABLE:
            // Operand C is the iteration bound emitted by the compiler
            // for array destructuring (see ITERABLE_LIMIT_UNBOUNDED in
            // Goccia.Bytecode):
            //   0..254  = exact element count to consume; 0 means
            //             "consume zero elements" for `const [] = iter`
            //             then close;
            //   255     = unbounded (rest pattern present or pattern
            //             length exceeds the encoding range).
            // IterableToArray's ALimit uses -1 = unbounded, 0+ = exact
            // count, so translate the sentinel here.
            if C = ITERABLE_LIMIT_UNBOUNDED then
              SetRegister(A, IterableToArray(RegisterToValue(FRegisters[A]),
                False, -1))
            else
              SetRegister(A, IterableToArray(RegisterToValue(FRegisters[A]),
                False, C));
        else
          raise Exception.CreateFmt('Unsupported validation mode: %d', [B]);
        end;
      end;

      OP_THROW_TYPE_ERROR_CONST:
        ThrowTypeError(Template.GetConstantUnchecked(C).StringValue);

      OP_THROW_TYPE_ERROR_CONST_LONG:
        ThrowTypeError(
          Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue);

      OP_DEFINE_GLOBAL_VAR_DECL_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) then
          FGlobalScope.DefineVariableBinding(GlobalName,
            TGocciaUndefinedLiteralValue.UndefinedValue, False);
      end;

      OP_DEFINE_GLOBAL_VAR_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        GlobalBindingValue := GetRegister(A);
        // Top-level var names are instantiated before body execution.
        // Initializers inside loops can therefore use ordinary assignment
        // resolution instead of repeating CreateGlobalVarBinding each time.
        if Assigned(FGlobalScope) and
           FGlobalScope.ContainsOwnVarBinding(GlobalName) then
        begin
          if (FGlobalScope.ThisValue is TGocciaObjectValue) and
             VMTrySetOwnWritableDataProperty(
               TGocciaObjectValue(FGlobalScope.ThisValue), GlobalName,
               GlobalBindingValue) then
            Continue
          else if (FGlobalScope.ThisValue is TGocciaObjectValue) and
             TGocciaObjectValue(FGlobalScope.ThisValue).HasOwnProperty(
               GlobalName) then
          begin
            if Template.StrictCode then
              TGocciaObjectValue(FGlobalScope.ThisValue).AssignProperty(
                GlobalName, GlobalBindingValue)
            else
              TGocciaObjectValue(FGlobalScope.ThisValue).
                AssignPropertyWithReceiver(GlobalName, GlobalBindingValue,
                  FGlobalScope.ThisValue);
          end
          else
            FGlobalScope.AssignBinding(GlobalName, GlobalBindingValue, 0, 0,
              not Template.StrictCode);
        end
        else
          DefineGlobalBinding(GlobalName, GlobalBindingValue, dtVar,
            not Template.StrictCode);
      end;

      OP_DEFINE_GLOBAL_LET_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        DefineGlobalBinding(GlobalName, GetRegister(A), dtLet);
      end;

      OP_DEFINE_GLOBAL_CONST_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        DefineGlobalBinding(GlobalName, GetRegister(A), dtConst);
      end;

      OP_DEFINE_GLOBAL_FUNCTION_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) then
          FGlobalScope.CreateGlobalFunctionBinding(GlobalName, GetRegister(A),
            False);
      end;

      OP_PREDECLARE_GLOBAL_LET_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) then
          FGlobalScope.PredeclareLexicalBinding(GlobalName, dtLet);
      end;

      OP_PREDECLARE_GLOBAL_CONST_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) then
          FGlobalScope.PredeclareLexicalBinding(GlobalName, dtConst);
      end;

      OP_FINALIZE_ENUM:
        SetRegister(A, FinalizeEnumValue(GetRegister(A),
          Template.GetConstantUnchecked(C).StringValue));

      OP_SUPER_GET_CONST:
        if A > 0 then
          SetRegister(A, GetSuperPropertyValue(GetRegister(A + 1),
            GetRegister(A - 1), Template.GetConstantUnchecked(C).StringValue,
            B <> 0))
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);

      OP_SUPER_GET:
        if A > 0 then
          SetRegister(A, GetSuperPropertyValueByKey(GetRegister(A + 1),
            GetRegister(A - 1), GetRegister(C), B <> 0))
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);

      OP_SUPER_SET:
        if A > 0 then
          SetSuperPropertyValueByKey(GetRegister(A + 1), GetRegister(A - 1),
            GetRegister(B), GetRegister(C))
        else
          ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
            SSuggestCheckNullBeforeAccess);

      OP_SUPER_BASE:
        SetRegister(A, ResolveSuperPropertyBaseValue(GetRegister(B),
          GetRegister(C)));

      OP_SUPER_GET_BASE:
        if A > 0 then
          SetRegister(A, GetSuperPropertyValueFromBase(GetRegister(A + 1),
            GetRegister(A - 1), GetRegister(C)))
        else
          SetRegister(A, TGocciaUndefinedLiteralValue.UndefinedValue);

      OP_SUPER_SET_BASE:
        if A > 0 then
          SetSuperPropertyBaseValueByKey(GetRegister(A + 1),
            GetRegister(A - 1), GetRegister(B), GetRegister(C))
        else
          ThrowTypeError(SErrorCannotSetPropertyOnNonObject,
            SSuggestCheckNullBeforeAccess);

      OP_RETURN:
      begin
        ReturnValue := FRegisters[A];
        if Assigned(GActiveBytecodeGenerator) and
           (GActiveBytecodeGenerator.FClosure = AClosure) then
          GActiveBytecodeGenerator.FReturnRequiresAwait := B <> 0;
        if FClosedNumericFrameStackCount >
           InitialClosedNumericFrameCount then
        begin
          ResultReg := PopClosedNumericFrame(Frame, Template, PrevCovLine,
            ProfileEntryTimestamp);
          SetRegisterRaw(ResultReg, ReturnValue);
          goto LDispatchNext;
        end;
        // Outermost frame: let the finally block handle teardown
        if FFrameStackCount <= InitialFrameStackCount then
        begin
          FLastClosureThisValue := GetLocalRegister(0);
          Exit(ReturnValue);
        end;
        // Intermediate trampoline frame: tear down and pop to parent
        TeardownCurrentFrame(Template, ProfileEntryTimestamp,
          FFrameStack[FFrameStackCount - 1].HandlerCount);
        ResultReg := PopFrame(Frame, Template, PrevCovLine, ProfileEntryTimestamp);
        SetRegisterRaw(ResultReg, ReturnValue);
        goto LDispatchNext;
      end;
        else
          raise Exception.CreateFmt('Unsupported Goccia VM opcode in minimal executor: %d', [Op]);
        end;
