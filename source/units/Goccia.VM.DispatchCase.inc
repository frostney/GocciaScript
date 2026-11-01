{ The opcode case of ExecuteClosureRegistersInternal's dispatch loop.
  Included once. Continue in this file fetches the next instruction without
  the memory-pressure check at the end of the loop body. Break does not end
  execution: the loop around the exception handlers enters dispatch again at
  the same instruction, so end it with Exit or by raising. }

          case TGocciaOpCode(Op) of
      // Hot arms. FPC lays the arms out in this order, so these come first
      // and stay together, and an edit to any arm after them cannot move them
      // (issue #1917). They are the arms the AWFY workloads execute most, by
      // their mean share of executed opcodes, highest first, except
      // OP_ITER_NEXT: FPC places the function's temporaries in the order it
      // compiles the arms, and with that arm first the dispatch function's
      // stack frame is no larger than with the old order, on AArch64 too,
      // where frequency order cost 336 bytes per native re-entry. A change to
      // the set or the order is a measured layout change: see 'Dispatch
      // layout' in docs/bytecode-vm.md, and scripts/check-vm-dispatch-layout.ts,
      // which holds the list.
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
              // A `value` getter runs guest code (see FRegisters).
              GuestCallValue := IterResult.GetProperty(PROP_VALUE);
              FRegisters[A] := VMValueToRegisterFast(GuestCallValue);
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

      OP_GET_PROP_CONST, OP_GET_LOCAL_PROP_CONST:
      begin
        // OP_GET_LOCAL_PROP_CONST loads the local into A, then reads the
        // property of A as OP_GET_PROP_CONST reads the property of B.
        if TGocciaOpCode(Op) = OP_GET_LOCAL_PROP_CONST then
        begin
          FRegisters[A] := GetLocalRegister(B);
          if FRegisters[A].Kind = grkHole then
            ThrowUninitializedLocal(Template, InstructionStartIP, B);
          B := A;
        end;
        if (FRegisters[B].Kind = grkObject) and Assigned(FRegisters[B].ObjectValue) and
           VMPropertyReadCacheableReceiver(FRegisters[B].ObjectValue) then
        begin
          // Hot shape: per-site inline cache keyed by the name-constant
          // index, validated against (own-map identity, map entry version).
          // Hits and fills serve only own plain data properties on
          // ordinary-lookup receivers; everything else degrades to the
          // generic GetPropertyValue path. Sites whose MissStreak saturated
          // are megamorphic: they skip the cache and use the uncached
          // own-data fast path. A nil slot (out-of-range constant index in
          // corrupt bytecode) runs fully uncached.
          // The gate above is the three exact classes the caches serve, not
          // `is TGocciaObjectValue`: no tier below does anything for another
          // receiver class but call GetPropertyValue, which the next branch
          // does without walking the class chain first.
          PropertyReadCache := Template.PropertyReadCacheSlot(C);
          if Assigned(PropertyReadCache) and
             (PropertyReadCache^.MissStreak <
              PROPERTY_READ_CACHE_POLYMORPHIC_LIMIT) and
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
              Constant := Template.GetConstantUnchecked(C);
              GlobalName := Constant.StringValue;
              if Constant.Kind <> bckPrivateName then
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
                SetRegister(A, GetPrivatePropertyValue(
                  FRegisters[B].ObjectValue, GlobalName));
            end;
          end;
        end
        else
          // Every other receiver, a primitive or an object the caches do not
          // serve, out of line so this arm stays small.
          ExecGetUncachedConstProperty(Template, A, B, C);
      end;

      OP_GET_LOCAL:
      begin
        FRegisters[A] := GetLocalRegister(DecodeBx(Instruction));
        if FRegisters[A].Kind = grkHole then
          ThrowUninitializedLocal(Template, InstructionStartIP,
            DecodeBx(Instruction));
      end;

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
          if (JumpOffset < 0) and FThreadPolls^.TimeoutArmed then
            PollExecutionTimeout(
              PGocciaTimeoutPollState(FThreadPolls^.TimeoutState));
        end
        else if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(InstructionStartIP),
            Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);

      OP_JUMP:
      begin
        JumpOffset := DecodeAx(Instruction);
        Inc(Frame.IP, JumpOffset);
        if (JumpOffset < 0) and FThreadPolls^.TimeoutArmed then
          PollExecutionTimeout(
            PGocciaTimeoutPollState(FThreadPolls^.TimeoutState));
      end;

      OP_MOVE:
        SetRegisterRaw(A, FRegisters[B]);

      OP_CALL_METHOD:
      begin
        if FThreadPolls^.TimeoutArmed then
          PollExecutionTimeout(
            PGocciaTimeoutPollState(FThreadPolls^.TimeoutState));
        // See OP_CALL: an exact class comparison decides a bytecode callee.
        CalleeIsBytecodeFunction := (FRegisters[A].Kind = grkObject) and
          Assigned(FRegisters[A].ObjectValue) and
          (FRegisters[A].ObjectValue.ClassType = TGocciaBytecodeFunctionValue);
        if (not CalleeIsBytecodeFunction) and
           ((C and CALL_FLAG_SPREAD) = 0) and (B = 2) and
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
          Continue;
        end;
        if (not CalleeIsBytecodeFunction) and ((C and 1) = 0) then
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
              // Binding reads the target's length and name, which can be
              // getters that run guest code (see FRegisters).
              case B of
                0:
                  begin
                    GuestCallValue := TGocciaBoundFunctionValue.CreateWithoutArgs(
                      FRegisters[A - 1].ObjectValue,
                      TGocciaUndefinedLiteralValue.UndefinedValue);
                    FRegisters[A] := RegisterObject(GuestCallValue);
                  end;
                1:
                  begin
                    GuestCallValue := TGocciaBoundFunctionValue.CreateWithoutArgs(
                      FRegisters[A - 1].ObjectValue,
                      RegisterToValue(FRegisters[A + 1]));
                    FRegisters[A] := RegisterObject(GuestCallValue);
                  end;
                2:
                  begin
                    GuestCallValue := TGocciaBoundFunctionValue.CreateWithSingleArg(
                      FRegisters[A - 1].ObjectValue,
                      RegisterToValue(FRegisters[A + 1]),
                      RegisterToValue(FRegisters[A + 2]));
                    FRegisters[A] := RegisterObject(GuestCallValue);
                  end;
              else
                BytecodeFunction := nil;
              end;
              if B <= 2 then
                Continue;
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
                  // The first operand is the callee's `this`; the rest are its
                  // arguments, read straight out of the caller's registers.
                  if B <= 1 then
                    SetupNewFrame(BytecodeFunction.FClosure,
                      CallThisRegister, nil, 0, True,
                      Frame, Template, PrevCovLine, ProfileEntryTimestamp)
                  else
                    SetupNewFrame(BytecodeFunction.FClosure,
                      CallThisRegister, @FRegisters[A + 2], B - 1, True,
                      Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                  Continue;
                end
                // The dense hole-free gate is what makes the direct element
                // reads below legal: no read can reach an accessor, so the
                // argument values cannot be produced by guest code that
                // allocates (and collects) while the earlier ones sit in plain
                // locals, and no read order is observable. A holey array or one
                // whose length was grown past its element count falls through
                // to the generic call, which runs Function.prototype.apply and
                // therefore CreateListFromArrayLike — ascending Get order, a
                // rooted arguments collection, and the spec argument count.
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
                  // Up to three arguments are staged in a stack array, which
                  // spares the common short apply a heap staging array.
                  if ArgsArray.Elements.Count <= Length(ApplyArgRegisters) then
                  begin
                    for I := 0 to ArgsArray.Elements.Count - 1 do
                      ApplyArgRegisters[I] :=
                        VMValueToRegisterFast(ArgsArray.Elements[I]);
                    SetupNewFrame(BytecodeFunction.FClosure,
                      CallThisRegister, @ApplyArgRegisters[0],
                      ArgsArray.Elements.Count, True,
                      Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                  end
                  else
                  begin
                    SetLength(RegisterArgs, ArgsArray.Elements.Count);
                    for I := 0 to High(RegisterArgs) do
                      RegisterArgs[I] :=
                        VMValueToRegisterFast(ArgsArray.Elements[I]);
                    SetupNewFrame(BytecodeFunction.FClosure,
                      CallThisRegister, PGocciaRegister(RegisterArgs),
                      Length(RegisterArgs), True,
                      Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                  end;
                  Continue;
                end;
              end;
            end;
          end;
        end;

        if CalleeIsBytecodeFunction then
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
              // See OP_CALL: the arguments are read straight out of the
              // caller's registers.
              if (C and CALL_FLAG_TAIL) <> 0 then
                PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                  InitialFrameStackCount, SavedHandlerCount)
              else
                PushFrame(A, Frame.IP, Template, PrevCovLine,
                  ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                CallThisRegister, @FRegisters[A + 1], B, True,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              Continue;
            end
            else if (FRegisters[B].Kind = grkObject) and
                    (FRegisters[B].ObjectValue is TGocciaArrayValue) then
            begin
              SetLength(RegisterArgs,
                TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count);
	              for I := 0 to High(RegisterArgs) do
	                RegisterArgs[I] := VMValueToRegisterFast(
	                  TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty(IntegerToString(I)));
              if (C and CALL_FLAG_TAIL) <> 0 then
                PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                  InitialFrameStackCount, SavedHandlerCount)
              else
                PushFrame(A, Frame.IP, Template, PrevCovLine,
                  ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                CallThisRegister, PGocciaRegister(RegisterArgs),
                Length(RegisterArgs), True,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              Continue;
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
                CallArgs.Add(TGocciaArrayValue(GetRegister(B)).GetProperty(IntegerToString(I)));
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
            if FCallStack <> nil then
              SavedConstructFrameOk :=
                FCallStack.TryGetTopFrame(SavedConstructFrame)
            else
              SavedConstructFrameOk := False;
            // See OP_CALL: also stamps the recorded call-site position for a
            // native callee's created error; snapshotted above, restored below.
            EnterNativeCallSite(PreviousCallSite);
            try
              SetRegister(A, InvokeFunctionValue(GetRegister(A), CallArgs,
                GetRegister(A - 1)));
            finally
              LeaveGocciaCallSite(PreviousCallSite);
            end;
            if SavedConstructFrameOk and (FCallStack <> nil) then
              FCallStack.SetTopFrame(SavedConstructFrame);
          end
          else
            SetRegister(A, InvokeFunctionValue(GetRegister(A), CallArgs,
              GetRegister(A - 1)));
        finally
          ReleaseArguments(CallArgs);
        end;
      end;

      OP_LOAD_INT:
        FRegisters[A] := RegisterInt(DecodesBx(Instruction));

      OP_RETURN:
      begin
        ReturnValue := FRegisters[A];
        if Assigned(EntryGenerator) then
          EntryGenerator.FReturnRequiresAwait := B <> 0;
        if FClosedNumericFrameStackCount >
           InitialClosedNumericFrameCount then
        begin
          ResultReg := PopClosedNumericFrame(Frame, Template, PrevCovLine,
            ProfileEntryTimestamp);
          SetRegisterRaw(ResultReg, ReturnValue);
          Continue;
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
        Continue;
      end;
      OP_SET_PROP_CONST:
        if (FRegisters[A].Kind = grkObject) and Assigned(FRegisters[A].ObjectValue) then
        begin
          RightValue := RegisterToValue(FRegisters[C]);
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
            Constant := Template.GetConstantUnchecked(B);
            GlobalName := Constant.StringValue;
            if Constant.Kind = bckPrivateName then
              SetPrivatePropertyValue(FRegisters[A].ObjectValue, GlobalName,
                RightValue)
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
        else if Template.GetConstantUnchecked(B).Kind = bckPrivateName then
          SetPrivatePropertyValue(GetRegister(A),
            Template.GetConstantUnchecked(B).StringValue,
            GetRegister(C))
        else
          SetPropertyValue(GetRegister(A),
            Template.GetConstantUnchecked(B).StringValue,
            GetRegister(C));

      // A register compared with a signed Int16 immediate, one arm for the
      // six opcodes, so the dispatch loop stays small; the comparison is in
      // ImmediateComparisonJumps.
      OP_JUMP_IF_NOT_LT_IMM, OP_JUMP_IF_NOT_GT_IMM, OP_JUMP_IF_NOT_LTE_IMM,
      OP_JUMP_IF_NOT_GTE_IMM, OP_JUMP_IF_NOT_STRICT_EQ_IMM,
      OP_JUMP_IF_STRICT_EQ_IMM:
        begin
          NumericComparisonResult := ImmediateComparisonJumps(
            TGocciaOpCode(Op), A, Int16(B));
          if NumericComparisonResult then
          begin
            JumpOffset := Int16(C);
            Inc(Frame.IP, JumpOffset);
            if (JumpOffset < 0) and FThreadPolls^.TimeoutArmed then
              PollExecutionTimeout(
                PGocciaTimeoutPollState(FThreadPolls^.TimeoutState));
          end;
          if FCoverageEnabled then
            RecordFusedBranchHit(Template, InstructionStartIP,
              NumericComparisonResult);
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
        else if VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
        begin
          if FProfilingOpcodes then
            TGocciaProfiler.Instance.RecordScalarHit;
          VMStoreNumberRegister(@FRegisters[A], LeftDouble + RightDouble);
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
          begin
            if (FRegisters[B].Kind = grkObject) and
               (FRegisters[B].ObjectValue is TGocciaStringLiteralValue) then
              SetRegisterFast(A, TGocciaStringLiteralValue.Concatenate(
                TGocciaStringLiteralValue(FRegisters[B].ObjectValue),
                VMRegisterToStringFast(FRegisters[C]).Value))
            else
              SetRegisterFast(A, TGocciaStringLiteralValue.Create(
                VMRegisterToStringFast(FRegisters[B]).Value +
                VMRegisterToStringFast(FRegisters[C]).Value));
          end
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

      OP_ADD_FLOAT:
      begin
        if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
          RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
            RightDouble);
        VMStoreNumberRegister(@FRegisters[A], LeftDouble + RightDouble);
      end;

      OP_ARRAY_SET:
        ExecSetComputedProperty(A, FRegisters[B], FRegisters[C],
          ELEMENT_SET_OPTIONS);

      OP_ARRAY_GET:
        ExecGetComputedProperty(A, FRegisters[B], FRegisters[C],
          ELEMENT_GET_OPTIONS);

      OP_MUL_FLOAT:
      begin
        if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
          RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
            RightDouble);
        VMStoreNumberRegister(@FRegisters[A], LeftDouble * RightDouble);
      end;

      OP_LOAD_UNDEFINED:
        FRegisters[A] := RegisterUndefined;

      OP_MUL:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMIntProductResult(FRegisters[B].IntValue,
            FRegisters[C].IntValue);
        end
        else if VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          VMStoreNumberRegister(@FRegisters[A], LeftDouble * RightDouble);
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMRootedBinaryValue(@EvaluateMultiplication,
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

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

      OP_ADD_NUM_IMM:
        if FRegisters[B].Kind = grkInt then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue + Int16(C))
        else if FRegisters[B].Kind = grkFloat then
          FRegisters[A] := VMNumberRegister(FRegisters[B].FloatValue + Int16(C))
        else if VMBoxedNumberRegister(FRegisters[B]) then
          FRegisters[A] := VMNumberRegister(TGocciaNumberLiteralValue(
            FRegisters[B].ObjectValue).Value + Int16(C))
        else
          raise Exception.Create(
            'Invalid non-numeric source for OP_ADD_NUM_IMM');

      OP_SUB:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue -
            FRegisters[C].IntValue);
        end
        else if VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          VMStoreNumberRegister(@FRegisters[A], LeftDouble - RightDouble);
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMRootedBinaryValue(@EvaluateSubtraction,
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
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

      OP_LOAD_FALSE:
        FRegisters[A] := RegisterBoolean(False);

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
            NumericComparisonResult := NumericScalarToDouble(FRegisters[A]) <
              NumericScalarToDouble(FRegisters[B]);
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
            if (JumpOffset < 0) and FThreadPolls^.TimeoutArmed then
              PollExecutionTimeout(
                PGocciaTimeoutPollState(FThreadPolls^.TimeoutState));
          end
          else if FCoverageEnabled and
                  (TGocciaCoverageTracker.Instance <> nil) and
                  Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(InstructionStartIP),
              Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);
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
          if (JumpOffset < 0) and FThreadPolls^.TimeoutArmed then
            PollExecutionTimeout(
              PGocciaTimeoutPollState(FThreadPolls^.TimeoutState));
        end
        else if FCoverageEnabled and (TGocciaCoverageTracker.Instance <> nil) and Assigned(Template.DebugInfo) then
          TGocciaCoverageTracker.Instance.RecordBranchHit(
            Template.DebugInfo.SourceFile,
            Template.DebugInfo.GetLineForPC(InstructionStartIP),
            Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);

      // A proven Number can also arrive boxed: NaN, the infinities and -0
      // are kept as number values rather than scalar registers.
      OP_SUB_NUM_IMM:
        if FRegisters[B].Kind = grkInt then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue - Int16(C))
        else if FRegisters[B].Kind = grkFloat then
          FRegisters[A] := VMNumberRegister(FRegisters[B].FloatValue - Int16(C))
        else if VMBoxedNumberRegister(FRegisters[B]) then
          FRegisters[A] := VMNumberRegister(TGocciaNumberLiteralValue(
            FRegisters[B].ObjectValue).Value - Int16(C))
        else
          raise Exception.Create(
            'Invalid non-numeric source for OP_SUB_NUM_IMM');

      OP_LTE:
      begin
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(FRegisters[B].IntValue <=
            FRegisters[C].IntValue);
        end
        else if VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(LeftDouble <= RightDouble);
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
          begin
            // ToPrimitive can run guest code (see FRegisters).
            GuestCallBoolean := VMRootedBinaryPredicate(
              @Goccia.Arithmetic.LessThanOrEqual, LeftValue, RightValue);
            FRegisters[A] := RegisterBoolean(GuestCallBoolean);
          end;
        end;
      end;

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
          case VMStrictEqualRegisters(FRegisters[B], FRegisters[C]) of
            vseEqual:
              FRegisters[A] := RegisterBoolean(True);
            vseNotEqual:
              FRegisters[A] := RegisterBoolean(False);
          else
            SetRegister(A, GetRegister(B).IsEqual(GetRegister(C)));
          end;

      OP_SET_LOCAL:
        SetLocalRaw(DecodeBx(Instruction), FRegisters[A]);

      OP_LOAD_CHAR:
        if DecodeBx(Instruction) <= 127 then
          FRegisters[A] := RegisterObject(
            CachedASCIIStringValue(
              TASCIIStringCodeUnit(DecodeBx(Instruction))))
        else
          FRegisters[A] := RegisterObject(TGocciaStringLiteralValue.Create(
            UTF16CodeUnitImmediateToString(DecodeBx(Instruction))));

      OP_LOAD_HOLE:
        FRegisters[A] := RegisterHole;

      OP_ADD_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue +
            FRegisters[C].IntValue)
        else
        begin
          if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
             LeftDouble, RightDouble) then
            RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
              RightDouble);
          VMStoreNumberRegister(@FRegisters[A], LeftDouble + RightDouble);
        end;

      OP_SUB_FLOAT:
      begin
        if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
          RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
            RightDouble);
        VMStoreNumberRegister(@FRegisters[A], LeftDouble - RightDouble);
      end;

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
            // A private name loads as its TGocciaPrivateNameKeyValue, the
            // only key value that names a private element.
            bckString, bckPrivateName:
              begin
                LeftValue := TGocciaValue(
                  Template.GetStringConstantCache(DecodeBx(Instruction)));
                if not Assigned(LeftValue) then
                begin
                  if Constant.Kind = bckPrivateName then
                    LeftValue := TGocciaPrivateNameKeyValue.Create(
                      Constant.StringValue)
                  else
                    LeftValue := TGocciaStringLiteralValue.Create(
                      Constant.StringValue);
                  Template.SetStringConstantCache(
                    DecodeBx(Instruction), LeftValue);
                end;
                FRegisters[A] := RegisterObject(LeftValue);
              end;
          else
            FRegisters[A] := ValueToRegister(ConstantToValue(Constant^));
          end;
        end;

      OP_GET_GLOBAL, OP_GET_GLOBAL_DECLARED:
      begin
        if not Assigned(FGlobalScope) then
          FRegisters[A] := RegisterUndefined
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
          // A module environment's import binding, read by a function the
          // module created while it was linked: resolve it through the cached
          // binding, skipping the scope's name lookups.
          else if Assigned(GlobalReadCache) and
             (GlobalReadCache^.Scope = Pointer(FGlobalScope)) and
             (GlobalReadCache^.ObjectBindingKind =
               GLOBAL_READ_OBJECT_BINDING_IMPORT) and
             FGlobalScope.LexicalEntryVersionIs(GlobalReadCache^.Version) and
             TGocciaModuleImportBinding(GlobalReadCache^.ObjectValue)
               .TryGetValue(GlobalBindingValue) then
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
                GlobalBindingValue, GlobalImportBinding,
                GlobalImportVersion) then
              begin
                GlobalBindingEntryIndex := GlobalReadCache^.EntryIndex;
                GlobalBindingVersion := GlobalReadCache^.Version;
                if Assigned(GlobalImportBinding) then
                begin
                  GlobalReadCache^.Scope := Pointer(FGlobalScope);
                  GlobalReadCache^.ObjectValue := Pointer(GlobalImportBinding);
                  GlobalReadCache^.ObjectBindingKind :=
                    GLOBAL_READ_OBJECT_BINDING_IMPORT;
                  GlobalReadCache^.Version := GlobalImportVersion;
                end
                else if (FGlobalScope.ThisValue is TGocciaObjectValue) and
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
              else if TGocciaOpCode(Op) = OP_GET_GLOBAL_DECLARED then
                ThrowReferenceError(Format(SErrorUndefinedVariable,
                  [GlobalName]), SSuggestDeclareBeforeUse)
              else
                FRegisters[A] := RegisterUndefined;
            end
            else if FGlobalScope.TryGetBindingValue(GlobalName,
              GlobalBindingValue) then
              FRegisters[A] := VMValueToRegisterFast(GlobalBindingValue)
            else if TGocciaOpCode(Op) = OP_GET_GLOBAL_DECLARED then
              // ES2026 §6.2.5.5 GetValue step 2: the declared binding is
              // gone (a configurable global its var redeclared was deleted),
              // so the reference is unresolvable.
              ThrowReferenceError(Format(SErrorUndefinedVariable,
                [GlobalName]), SSuggestDeclareBeforeUse)
            else
              FRegisters[A] := RegisterUndefined;
          end;
        end;
      end;

      OP_HAS_GLOBAL:
      begin
        if Assigned(FGlobalScope) then
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
            Continue;
          end;
        end;

        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        // Contains can materialize a lazy global (a shim) or reach a Proxy
        // trap on the global object's prototype chain, either of which runs
        // guest code (see FRegisters).
        GuestCallBoolean := Assigned(FGlobalScope) and
          FGlobalScope.Contains(GlobalName);
        FRegisters[A] := RegisterBoolean(GuestCallBoolean);
      end;

      OP_GET_UPVALUE:
      begin
        if Assigned(FCurrentClosure) then
        begin
          Upvalue := FCurrentClosure.GetUpvalue(DecodeBx(Instruction));
          if Assigned(Upvalue) then
          begin
            if Upvalue.Value.Kind = grkHole then
              ThrowUninitializedUpvalue(Template, DecodeBx(Instruction));
            SetRegisterRaw(A, Upvalue.Value)
          end
          else
            FRegisters[A] := RegisterUndefined;
        end
        else
          FRegisters[A] := RegisterUndefined;
      end;

      OP_LOAD_NULL:
        FRegisters[A] := RegisterNull;

      OP_LOAD_TRUE:
        FRegisters[A] := RegisterBoolean(True);

      OP_CONSTRUCT:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaVMClassValue) then
        begin
          SetLength(RegisterArgs, C);
          for I := 0 to C - 1 do
            RegisterArgs[I] := FRegisters[B + 1 + I];
          // The constructor runs guest code (see FRegisters).
          GuestCallRegister := TGocciaVMClassValue(FRegisters[B].ObjectValue)
            .InstantiateRegisters(RegisterArgs);
          FRegisters[A] := GuestCallRegister;
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
            (`new Map(); JSON.parse('{')`). On a throw, the handler that
            catches it in this frame clears the stamp (HandleExceptionUnwind). }
          if FCallStack <> nil then
            SavedConstructFrameOk :=
              FCallStack.TryGetTopFrame(SavedConstructFrame)
          else
            SavedConstructFrameOk := False;
          StampCurrentCallSiteLocation;
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
          if SavedConstructFrameOk and (FCallStack <> nil) then
            FCallStack.SetTopFrame(SavedConstructFrame);
        end;
      end;

      OP_PUSH_FINALLY_HANDLER:
      begin
        if FHandlerStack.Count >= FHandlerStack.Capacity then
          GrowHandlerStack(FHandlerStack.Count + 1);
        FHandlerStack.Push(Frame.IP + DecodeBx(Instruction), A, FFrameDepth,
          bhkFinally);
      end;

      OP_POP_HANDLER:
        if not FHandlerStack.IsEmpty then
          FHandlerStack.Pop;

      OP_BAND:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(
            LongInt(FRegisters[B].IntValue) and
            LongInt(FRegisters[C].IntValue))
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
          FRegisters[A] := RegisterInt(
            VMRegisterToInt32(FRegisters[B]) and
            VMRegisterToInt32(FRegisters[C]))
        else
          SetRegister(A, VMRootedBinaryValue(@EvaluateBitwiseAnd,
            GetRegister(B), GetRegister(C)));

      OP_CALL:
      begin
        if FThreadPolls^.TimeoutArmed then
          PollExecutionTimeout(
            PGocciaTimeoutPollState(FThreadPolls^.TimeoutState));
        // A bytecode function is the usual callee and is neither a native nor
        // a bound function. Its class is sealed, so one exact class comparison
        // decides it: a bytecode callee skips the two `is` tests below, and
        // any other callee skips the one for a bytecode function, each of
        // which would walk a whole class chain to fail.
        CalleeIsBytecodeFunction := (FRegisters[A].Kind = grkObject) and
          Assigned(FRegisters[A].ObjectValue) and
          (FRegisters[A].ObjectValue.ClassType = TGocciaBytecodeFunctionValue);
        if (not CalleeIsBytecodeFunction) and
           ((C and CALL_FLAG_SPREAD) = 0) and
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
                Continue;
              end;
            nikDecodeURIComponent:
              begin
                SetRegisterFast(A, TGocciaStringLiteralValue.Create(
                  DecodeURIComponent(TGocciaStringLiteralValue(
                    FRegisters[A + 1].ObjectValue).Value)));
                Continue;
              end;
          end;
        end;
        if (not CalleeIsBytecodeFunction) and
           (FRegisters[A].Kind = grkObject) and
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
                  CallThisRegister, PGocciaRegister(RegisterArgs),
                  Length(RegisterArgs), True,
                  Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                Continue;
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
	                    TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty(IntegerToString(I)));
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
                  CallThisRegister, PGocciaRegister(RegisterArgs),
                  Length(RegisterArgs), True,
                  Frame, Template, PrevCovLine, ProfileEntryTimestamp);
                Continue;
              end;
            end;
          end;
        end;

        if CalleeIsBytecodeFunction then
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
              // The arguments are read straight out of the caller's registers:
              // SetupNewFrame copies them into the callee's argument window
              // before it acquires (and so may move or clear) any register,
              // or, when the callee never reads one and no arena grows,
              // straight into the callee's registers.
              if (C and CALL_FLAG_TAIL) <> 0 then
                PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                  InitialFrameStackCount, SavedHandlerCount)
              else
                PushFrame(A, Frame.IP, Template, PrevCovLine,
                  ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                CallThisRegister, @FRegisters[A + 1], B, True,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              Continue;
            end
            else if (FRegisters[B].Kind = grkObject) and
                    (FRegisters[B].ObjectValue is TGocciaArrayValue) then
            begin
              SetLength(RegisterArgs,
                TGocciaArrayValue(FRegisters[B].ObjectValue).Elements.Count);
	              for I := 0 to High(RegisterArgs) do
	                RegisterArgs[I] := ValueToRegister(
	                  TGocciaArrayValue(FRegisters[B].ObjectValue).GetProperty(IntegerToString(I)));
              if (C and CALL_FLAG_TAIL) <> 0 then
                PrepareTailCallFrameReuse(Template, ProfileEntryTimestamp,
                  InitialFrameStackCount, SavedHandlerCount)
              else
                PushFrame(A, Frame.IP, Template, PrevCovLine,
                  ProfileEntryTimestamp);
              SetupNewFrame(BytecodeFunction.FClosure,
                CallThisRegister, PGocciaRegister(RegisterArgs),
                Length(RegisterArgs), True,
                Frame, Template, PrevCovLine, ProfileEntryTimestamp);
              Continue;
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
	                CallArgs.Add(TGocciaArrayValue(GetRegister(B)).GetProperty(IntegerToString(I)));
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
            if FCallStack <> nil then
              SavedConstructFrameOk :=
                FCallStack.TryGetTopFrame(SavedConstructFrame)
            else
              SavedConstructFrameOk := False;
            // Also stamps the executing frame with this call's position so an
            // error a native callee creates captures the call site (deferred
            // frames are 0:0). Uses the recorded call-site column (the
            // instruction line map resolves only to the enclosing
            // statement). Snapshotted above and
            // restored below.
            EnterNativeCallSite(PreviousCallSite);
            try
              SetRegister(A, InvokeFunctionValue(GetRegister(A), CallArgs,
                TGocciaUndefinedLiteralValue.UndefinedValue));
            finally
              LeaveGocciaCallSite(PreviousCallSite);
            end;
            if SavedConstructFrameOk and (FCallStack <> nil) then
              FCallStack.SetTopFrame(SavedConstructFrame);
          end
          else
            SetRegister(A, InvokeFunctionValue(GetRegister(A), CallArgs,
              TGocciaUndefinedLiteralValue.UndefinedValue));
        finally
          ReleaseArguments(CallArgs);
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
        else if VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(LeftDouble >= RightDouble);
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
          begin
            // ToPrimitive can run guest code (see FRegisters).
            GuestCallBoolean := VMRootedBinaryPredicate(
              @Goccia.Arithmetic.GreaterThanOrEqual, LeftValue, RightValue);
            FRegisters[A] := RegisterBoolean(GuestCallBoolean);
          end;
        end;
      end;

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
          case VMStrictEqualRegisters(FRegisters[B], FRegisters[C]) of
            vseEqual:
              FRegisters[A] := RegisterBoolean(False);
            vseNotEqual:
              FRegisters[A] := RegisterBoolean(True);
          else
            SetRegister(A, GetRegister(B).IsNotEqual(GetRegister(C)));
          end;

      OP_MOD:
      begin
        if VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMModuloRegister(LeftDouble, RightDouble);
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMRootedBinaryValue(@EvaluateModulo,
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      OP_NOT:
        FRegisters[A] := RegisterBoolean(not RegisterToBoolean(FRegisters[B]));

      OP_DIV:
      begin
        if VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          VMStoreNumberRegister(@FRegisters[A], LeftDouble / RightDouble);
        end
        else
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarMiss;
          SetRegister(A, VMRootedBinaryValue(@EvaluateDivision,
            GetRegisterFast(B), GetRegisterFast(C)));
        end;
      end;

      // Cold arms: every other opcode, in no performance-relevant order. A
      // new opcode's arm goes here.

      OP_LOAD_REGEXP:
        FRegisters[A] := ValueToRegister(
          BuildRegExpLiteralConstant(Template, DecodeBx(Instruction)));

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
        if (C = TO_PROPERTY_KEY_KEEP_INT) and (FRegisters[B].Kind = grkInt) then
          SetRegisterRaw(A, FRegisters[B])
        else
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

      OP_SET_UPVALUE:
      begin
        if Assigned(FCurrentClosure) then
        begin
          Upvalue := FCurrentClosure.GetUpvalue(DecodeBx(Instruction));
          if Assigned(Upvalue) then
          begin
            if Upvalue.Value.Kind = grkHole then
              ThrowUninitializedUpvalue(Template, DecodeBx(Instruction));
            Upvalue.Value := FRegisters[A];
          end;
        end;
      end;

      OP_CHECK_BINDING_INITIALIZED:
        if A = CHECK_BINDING_GLOBAL then
        begin
          // The global scope's own lookup raises the binding's named
          // ReferenceError while it is uninitialized.
          if Assigned(FGlobalScope) then
            FGlobalScope.TryGetBindingValue(Template.GetConstantUnchecked(
              DecodeBx(Instruction)).StringValue, GlobalBindingValue);
        end
        else if Assigned(FCurrentClosure) then
        begin
          Upvalue := FCurrentClosure.GetUpvalue(DecodeBx(Instruction));
          if Assigned(Upvalue) and (Upvalue.Value.Kind = grkHole) then
            ThrowUninitializedUpvalue(Template, DecodeBx(Instruction));
        end;

      OP_SET_GLOBAL_STATIC:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) and
           not FGlobalScope.TryAssignExistingBinding(GlobalName,
             RegisterToValue(FRegisters[A])) then
          ThrowReferenceError(Format(SErrorUndefinedVariable, [GlobalName]),
            SSuggestDeclareBeforeUse);
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

      OP_JUMP_IF_NUM_NOT_LTE_IMM:
        begin
          if FRegisters[A].Kind = grkInt then
            NumericComparisonResult := FRegisters[A].IntValue <= Int16(B)
          else if FRegisters[A].Kind = grkFloat then
            NumericComparisonResult := FRegisters[A].FloatValue <= Int16(B)
          else if VMBoxedNumberRegister(FRegisters[A]) then
            NumericComparisonResult := TGocciaNumberLiteralValue(
              FRegisters[A].ObjectValue).Value <= Int16(B)
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
            if (JumpOffset < 0) and FThreadPolls^.TimeoutArmed then
              PollExecutionTimeout(
                PGocciaTimeoutPollState(FThreadPolls^.TimeoutState));
          end
          else if FCoverageEnabled and
                  (TGocciaCoverageTracker.Instance <> nil) and
                  Assigned(Template.DebugInfo) then
            TGocciaCoverageTracker.Instance.RecordBranchHit(
              Template.DebugInfo.SourceFile,
              Template.DebugInfo.GetLineForPC(InstructionStartIP),
              Template.DebugInfo.GetColumnForPC(InstructionStartIP), 1);
        end;

      OP_PUSH_HANDLER:
      begin
        if FHandlerStack.Count >= FHandlerStack.Capacity then
          GrowHandlerStack(FHandlerStack.Count + 1);
        FHandlerStack.Push(Frame.IP + DecodeBx(Instruction), A, FFrameDepth);
      end;

      OP_SUB_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(FRegisters[B].IntValue -
            FRegisters[C].IntValue)
        else
        begin
          if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
             LeftDouble, RightDouble) then
            RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
              RightDouble);
          VMStoreNumberRegister(@FRegisters[A], LeftDouble - RightDouble);
        end;

      OP_MUL_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntProductResult(FRegisters[B].IntValue,
            FRegisters[C].IntValue)
        else
        begin
          if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
             LeftDouble, RightDouble) then
            RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
              RightDouble);
          VMStoreNumberRegister(@FRegisters[A], LeftDouble * RightDouble);
        end;

      OP_DIV_INT, OP_DIV_FLOAT:
      begin
        if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
          RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
            RightDouble);
        VMStoreNumberRegister(@FRegisters[A], LeftDouble / RightDouble);
      end;

      OP_MOD_INT, OP_MOD_FLOAT:
      begin
        if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
          RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
            RightDouble);
        FRegisters[A] := VMModuloRegister(LeftDouble, RightDouble);
      end;

      OP_EQ_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue = FRegisters[C].IntValue)
        else
        begin
          if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
             LeftDouble, RightDouble) then
            RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
              RightDouble);
          FRegisters[A] := RegisterBoolean(LeftDouble = RightDouble);
        end;

      OP_EQ_FLOAT:
      begin
        if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
          RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
            RightDouble);
        FRegisters[A] := RegisterBoolean(LeftDouble = RightDouble);
      end;

      OP_NEQ_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue <> FRegisters[C].IntValue)
        else
        begin
          if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
             LeftDouble, RightDouble) then
            RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
              RightDouble);
          FRegisters[A] := RegisterBoolean(LeftDouble <> RightDouble);
        end;

      OP_NEQ_FLOAT:
      begin
        if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
          RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
            RightDouble);
        FRegisters[A] := RegisterBoolean(LeftDouble <> RightDouble);
      end;

      OP_LT_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue < FRegisters[C].IntValue)
        else
        begin
          if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
             LeftDouble, RightDouble) then
            RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
              RightDouble);
          FRegisters[A] := RegisterBoolean(LeftDouble < RightDouble);
        end;

      OP_LT_FLOAT:
      begin
        if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
          RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
            RightDouble);
        FRegisters[A] := RegisterBoolean(LeftDouble < RightDouble);
      end;

      OP_GT_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue > FRegisters[C].IntValue)
        else
        begin
          if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
             LeftDouble, RightDouble) then
            RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
              RightDouble);
          FRegisters[A] := RegisterBoolean(LeftDouble > RightDouble);
        end;

      OP_GT_FLOAT:
      begin
        if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
          RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
            RightDouble);
        FRegisters[A] := RegisterBoolean(LeftDouble > RightDouble);
      end;

      OP_LTE_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue <= FRegisters[C].IntValue)
        else
        begin
          if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
             LeftDouble, RightDouble) then
            RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
              RightDouble);
          FRegisters[A] := RegisterBoolean(LeftDouble <= RightDouble);
        end;

      OP_LTE_FLOAT:
      begin
        if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
          RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
            RightDouble);
        FRegisters[A] := RegisterBoolean(LeftDouble <= RightDouble);
      end;

      OP_GTE_INT:
        if (FRegisters[B].Kind = grkInt) and (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterBoolean(
            FRegisters[B].IntValue >= FRegisters[C].IntValue)
        else
        begin
          if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
             LeftDouble, RightDouble) then
            RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
              RightDouble);
          FRegisters[A] := RegisterBoolean(LeftDouble >= RightDouble);
        end;

      OP_GTE_FLOAT:
      begin
        if not VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
          RegistersToDoubles(FRegisters[B], FRegisters[C], LeftDouble,
            RightDouble);
        FRegisters[A] := RegisterBoolean(LeftDouble >= RightDouble);
      end;

      OP_NEG_INT, OP_NEG_FLOAT:
      begin
        // ToNumber of an object operand runs guest code (see FRegisters).
        LeftDouble := RegisterToDouble(FRegisters[B]);
        FRegisters[A] := VMNumberRegister(-LeftDouble);
      end;

      OP_CONCAT:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaStringLiteralValue) and
           (FRegisters[C].Kind = grkObject) and
           (FRegisters[C].ObjectValue is TGocciaStringLiteralValue) then
          SetRegisterFast(A, TGocciaStringLiteralValue.Concatenate(
            TGocciaStringLiteralValue(FRegisters[B].ObjectValue),
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

      OP_GET_LENGTH:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaArrayValue) then
          // The length property, which can be past the stored elements.
          FRegisters[A] := VMNumberRegister(
            TGocciaArrayValue(FRegisters[B].ObjectValue).StoredLength)
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
        if Assigned(FCurrentClosure) and
           (FCurrentClosure.PrivateClass is TGocciaClassValue) then
          TGocciaVMClassValue(FRegisters[A].ObjectValue).OuterPrivateClass :=
            TGocciaClassValue(FCurrentClosure.PrivateClass);
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
        Constant := Template.GetConstantUnchecked(B);
        GlobalName := Constant.StringValue;
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
        begin
          SetBytecodeHomeObject(RegisterToValue(FRegisters[C]),
            FRegisters[A].ObjectValue);
          if Constant.Kind = bckPrivateName then
          begin
            // A private method is a private element of the prototype, which
            // StampBytecodePrivateBrands copies to each instance's brand.
            DeclareBytecodePrivateNameForClass(
              FRegisters[A].ObjectValue, GlobalName);
            TGocciaVMClassValue(FRegisters[A].ObjectValue).Prototype
              .SetPrivateElement(GlobalName,
                TGocciaPropertyDescriptorData.Create(
                  RegisterToValue(FRegisters[C]),
                  [pfConfigurable, pfWritable]));
          end
          else if GlobalName = PROP_CONSTRUCTOR then
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
        else if Constant.Kind = bckPrivateName then
          SetPrivatePropertyValue(GetRegister(A), GlobalName, GetRegister(C))
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

      // Runs a static block (ES2026 §15.7.11) or a static field initializer
      // (§15.7.10) with this = class and leaves its return value in B. Both
      // are methods whose [[HomeObject]] is the class itself, not its
      // prototype, so `super.x` there reads the class's [[Prototype]].
      OP_CLASS_EXEC_STATIC_BLOCK:
      begin
        if (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaBytecodeFunctionValue) then
        begin
          SetBytecodeHomeObject(RegisterToValue(FRegisters[B]),
            FRegisters[A].ObjectValue, True);
          // Copied out of the register window first: where a register is
          // passed by reference (Win64), SetupNewFrame would otherwise read
          // `this` through the window after acquiring registers moved it.
          CallThisRegister := FRegisters[A];
          PushFrame(B, Frame.IP, Template, PrevCovLine, ProfileEntryTimestamp);
          SetupNewFrame(
            TGocciaBytecodeFunctionValue(FRegisters[B].ObjectValue).FClosure,
            CallThisRegister, nil, 0, True,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
          Continue;
        end
        else if (FRegisters[B].Kind = grkObject) and
                Assigned(FRegisters[B].ObjectValue) and
                FRegisters[B].ObjectValue.IsCallable then
        begin
          CallArgs := AcquireArguments(0);
          try
            SetRegister(B, InvokeFunctionValue(RegisterToValue(FRegisters[B]),
              CallArgs, RegisterToValue(FRegisters[A])));
          finally
            ReleaseArguments(CallArgs);
          end;
        end;
      end;

      OP_SET_PROP_CONST_LOOSE:
      begin
        Constant := Template.GetConstantUnchecked(B);
        GlobalName := Constant.StringValue;
        RightValue := RegisterToValue(FRegisters[C]);
        TargetValue := GetRegister(A);
        if Constant.Kind = bckPrivateName then
          SetPrivatePropertyValue(TargetValue, GlobalName, RightValue)
        else
          SetPropertyValueLoose(TargetValue, GlobalName, RightValue);
      end;

      // Defines a field: static fields on the class, `<fields>` instance
      // fields on the receiver. ES2026 §7.3.33 DefineField never calls
      // MakeMethod on the value, so neither definition opcode sets a home
      // object; the initializer itself is the method (§15.7.10).
      OP_DEFINE_STATIC_PROP_CONST:
      begin
        Constant := Template.GetConstantUnchecked(B);
        GlobalName := Constant.StringValue;
        RightValue := RegisterToValue(FRegisters[C]);
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaObjectValue) then
        begin
          if Constant.Kind = bckPrivateName then
          begin
            DefinePrivateFieldValue(
              TGocciaObjectValue(FRegisters[A].ObjectValue), GlobalName,
              RightValue);
            Continue;
          end;
          DefineOwnDataProperty(
            TGocciaObjectValue(FRegisters[A].ObjectValue), GlobalName,
            RightValue, [pfEnumerable, pfConfigurable, pfWritable]);
        end
        else if Constant.Kind = bckPrivateName then
          SetPrivatePropertyValue(GetRegister(A), GlobalName, RightValue)
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
          if PropKey.Kind = pkkSymbol then
            DefineOwnDataSymbolProperty(TGocciaObjectValue(TargetValue),
              PropKey.Symbol, RightValue, [pfEnumerable, pfConfigurable, pfWritable])
          else
          begin
            GlobalName := PropertyKeyName(PropKey);
            // A private field past the 8-bit constant operand
            // (EmitDefineStaticPropertyByName).
            if IsPrivateNameKeyRegister(FRegisters[B]) then
            begin
              DefinePrivateFieldValue(TGocciaObjectValue(TargetValue),
                GlobalName, RightValue);
              Continue;
            end;

            DefineOwnDataProperty(TGocciaObjectValue(TargetValue),
              GlobalName, RightValue, [pfEnumerable, pfConfigurable, pfWritable]);
          end;
        end
        else if IsPrivateNameKeyRegister(FRegisters[B]) then
          SetPrivatePropertyValue(TargetValue,
            TGocciaPrivateNameKeyValue(FRegisters[B].ObjectValue).Value,
            RightValue)
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
            DefineOwnDataSymbolProperty(TGocciaObjectValue(TargetValue),
              PropKey.Symbol, RightValue, [pfEnumerable, pfConfigurable, pfWritable])
          else
            DefineOwnDataProperty(TGocciaObjectValue(TargetValue),
              PropertyKeyName(PropKey), RightValue, [pfEnumerable, pfConfigurable, pfWritable]);
        end
        else
          SetPropertyValue(TargetValue,
            KeyToPropertyNameRegister(FRegisters[B]), RightValue);
      end;

      OP_DEFINE_STATIC_METHOD_CONST:
      begin
        Constant := Template.GetConstantUnchecked(B);
        GlobalName := Constant.StringValue;
        RightValue := RegisterToValue(FRegisters[C]);
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaObjectValue) then
        begin
          if (Constant.Kind = bckPrivateName) and
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
            Continue;
          end;
          if FRegisters[A].ObjectValue is TGocciaVMClassValue then
            SetBytecodeHomeObject(RightValue, RegisterToValue(FRegisters[A]),
              True);
          DefineOwnDataProperty(
            TGocciaObjectValue(FRegisters[A].ObjectValue), GlobalName,
            RightValue, [pfConfigurable, pfWritable]);
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
            DefineOwnDataSymbolProperty(TGocciaObjectValue(TargetValue),
              PropKey.Symbol, RightValue, [pfConfigurable, pfWritable])
          else
            DefineOwnDataProperty(TGocciaObjectValue(TargetValue),
              PropertyKeyName(PropKey), RightValue,
              [pfConfigurable, pfWritable]);
        end
        else
          SetPropertyValue(TargetValue,
            KeyToPropertyNameRegister(FRegisters[B]), RightValue);
      end;

      // A computed instance method: installed on the prototype, which is its
      // home object, while the class in A supplies its private names.
      OP_CLASS_ADD_METHOD_DYNAMIC:
      begin
        RightValue := RegisterToValue(FRegisters[C]);
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[A].ObjectValue is TGocciaVMClassValue) then
        begin
          TargetValue := TGocciaVMClassValue(FRegisters[A].ObjectValue)
            .Prototype;
          PropKey := ClassifyPropertyKey(FRegisters[B], False);
          SetBytecodeHomeObject(RightValue, FRegisters[A].ObjectValue);
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
        end;
      end;

      // A function or class created in a class body joins the class's
      // private environment; a class's own then links to it.
      OP_SET_PRIVATE_CLASS:
        if (FRegisters[A].Kind = grkObject) and
           (FRegisters[B].Kind = grkObject) and
           (FRegisters[B].ObjectValue is TGocciaVMClassValue) then
        begin
          if FRegisters[A].ObjectValue is TGocciaBytecodeFunctionValue then
            TGocciaBytecodeFunctionValue(FRegisters[A].ObjectValue).FClosure
              .PrivateClass := TGocciaVMClassValue(FRegisters[B].ObjectValue)
          else if FRegisters[A].ObjectValue is TGocciaVMClassValue then
            TGocciaVMClassValue(FRegisters[A].ObjectValue).OuterPrivateClass :=
              TGocciaVMClassValue(FRegisters[B].ObjectValue);
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
          grkUndefined:
            SetRegister(A, TGocciaNumberLiteralValue.NaNValue);
          // ES2026 §13.4 Update Expressions: each form reads the old value with
          // GetValue before ToNumeric. The four fused update opcodes can take
          // a local's own register as B, where a hole is a lexical binding
          // that is still uninitialized (§9.1.1.1.6 GetBindingValue step 2).
          grkHole:
            ThrowUninitializedLocal(Template, InstructionStartIP, B);
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
          grkUndefined:
            SetRegister(A, TGocciaNumberLiteralValue.NaNValue);
          // Temporal dead zone: see OP_INC_NUMERIC.
          grkHole:
            ThrowUninitializedLocal(Template, InstructionStartIP, B);
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
          grkUndefined:
          begin
            SetRegister(A, TGocciaNumberLiteralValue.NaNValue);
            SetRegister(B, TGocciaNumberLiteralValue.NaNValue);
          end;
          // Temporal dead zone: see OP_INC_NUMERIC.
          grkHole:
            ThrowUninitializedLocal(Template, InstructionStartIP, B);
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
          grkUndefined:
          begin
            SetRegister(A, TGocciaNumberLiteralValue.NaNValue);
            SetRegister(B, TGocciaNumberLiteralValue.NaNValue);
          end;
          // Temporal dead zone: see OP_INC_NUMERIC.
          grkHole:
            ThrowUninitializedLocal(Template, InstructionStartIP, B);
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

      OP_POW:
      begin
        if VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := VMPowerRegister(LeftDouble, RightDouble);
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
          FRegisters[A] := VMNumberRegister(
            -NumericScalarToDouble(FRegisters[B]))
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

      OP_BOR:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(
            LongInt(FRegisters[B].IntValue) or
            LongInt(FRegisters[C].IntValue))
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
          FRegisters[A] := RegisterInt(
            VMRegisterToInt32(FRegisters[B]) or
            VMRegisterToInt32(FRegisters[C]))
        else
          SetRegister(A, VMRootedBinaryValue(@EvaluateBitwiseOr,
            GetRegister(B), GetRegister(C)));

      OP_BXOR:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(
            LongInt(FRegisters[B].IntValue) xor
            LongInt(FRegisters[C].IntValue))
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
          FRegisters[A] := RegisterInt(
            VMRegisterToInt32(FRegisters[B]) xor
            VMRegisterToInt32(FRegisters[C]))
        else
          SetRegister(A, VMRootedBinaryValue(@EvaluateBitwiseXor,
            GetRegister(B), GetRegister(C)));

      OP_SHL:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(LongInt(
            LongWord(FRegisters[B].IntValue) shl
            (LongWord(FRegisters[C].IntValue) and 31)))
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
          FRegisters[A] := RegisterInt(LongInt(
            LongWord(VMRegisterToInt32(FRegisters[B])) shl
            (LongWord(VMRegisterToInt32(FRegisters[C])) and 31)))
        else
          SetRegister(A, VMRootedBinaryValue(@EvaluateLeftShift,
            GetRegister(B), GetRegister(C)));

      OP_SHR:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := RegisterInt(SignedRightShiftInt32(
            LongInt(FRegisters[B].IntValue),
            LongWord(FRegisters[C].IntValue)))
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
          FRegisters[A] := RegisterInt(SignedRightShiftInt32(
            VMRegisterToInt32(FRegisters[B]),
            LongWord(VMRegisterToInt32(FRegisters[C]))))
        else
          SetRegister(A, VMRootedBinaryValue(@EvaluateRightShift,
            GetRegister(B), GetRegister(C)));

      OP_USHR:
        if (FRegisters[B].Kind = grkInt) and
           (FRegisters[C].Kind = grkInt) then
          FRegisters[A] := VMIntResult(Int64(LongWord(
            FRegisters[B].IntValue) shr
            (LongWord(FRegisters[C].IntValue) and 31)))
        else if RegisterIsNumericScalar(FRegisters[B]) and
           RegisterIsNumericScalar(FRegisters[C]) then
          FRegisters[A] := VMIntResult(Int64(LongWord(
            VMRegisterToInt32(FRegisters[B])) shr
            (LongWord(VMRegisterToInt32(FRegisters[C])) and 31)))
        else
          SetRegister(A, VMRootedBinaryValue(@EvaluateUnsignedRightShift,
            GetRegister(B), GetRegister(C)));

      OP_BNOT:
        if FRegisters[B].Kind = grkInt then
          FRegisters[A] := RegisterInt(not LongInt(FRegisters[B].IntValue))
        else if FRegisters[B].Kind = grkFloat then
          FRegisters[A] := RegisterInt(
            not NumberToInt32(FRegisters[B].FloatValue))
        else
          SetRegister(A, EvaluateBitwiseNot(GetRegister(B)));

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
        else if VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(LeftDouble < RightDouble);
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
          begin
            // ToPrimitive can run guest code (see FRegisters).
            GuestCallBoolean := VMRootedBinaryPredicate(
              @Goccia.Arithmetic.LessThan, LeftValue, RightValue);
            FRegisters[A] := RegisterBoolean(GuestCallBoolean);
          end;
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
        else if VMNumericScalarPair(@FRegisters[B], @FRegisters[C],
           LeftDouble, RightDouble) then
        begin
          if FProfilingOpcodes then TGocciaProfiler.Instance.RecordScalarHit;
          FRegisters[A] := RegisterBoolean(LeftDouble > RightDouble);
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
          begin
            // ToPrimitive can run guest code (see FRegisters).
            GuestCallBoolean := VMRootedBinaryPredicate(
              @Goccia.Arithmetic.GreaterThan, LeftValue, RightValue);
            FRegisters[A] := RegisterBoolean(GuestCallBoolean);
          end;
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
          // A getter can return a fresh matcher, and the hint object is
          // held only here while its string is made.
          OperandRoots.Add(CustomMatcherValue);
          if Assigned(CustomMatcherValue) then
          begin
            if not CustomMatcherValue.IsCallable then
              ThrowTypeError('Symbol.customMatcher must be callable');
            CallArgs := AcquireArguments(2);
            try
              MatchHintObject := TGocciaObjectValue.Create;
              OperandRoots.Add(MatchHintObject);
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
           (ChildTemplate.DebugInfo.LineMapCount > 0) and
           (ChildTemplate.Name <> STATIC_FIELD_TEMPLATE_NAME) then
          TGocciaCoverageTracker.Instance.RegisterFunction(
            ChildTemplate.DebugInfo.SourceFile, ChildTemplate.Name,
            ChildTemplate.DebugInfo.CoverageLine,
            ChildTemplate.DebugInfo.CoverageColumn);
        ChildClosure := TGocciaBytecodeClosure.Create(
          ChildTemplate, ChildTemplate.UpvalueCount);
        ChildClosure.GlobalScope := FGlobalScope;
        // ES2026 §10.2.3 OrdinaryFunctionCreate: every function captures the
        // running PrivateEnvironment, whatever kind of function it is. Class
        // element definitions then move a method into its own class body.
        if Assigned(FCurrentClosure) then
          ChildClosure.PrivateClass := FCurrentClosure.PrivateClass;
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
            ChildClosure.SetUpvalue(I, GetLocalCell(Desc.Index))
          else if Assigned(FCurrentClosure) then
            ChildClosure.SetUpvalue(I, FCurrentClosure.GetUpvalue(Desc.Index));
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
        if FThreadPolls^.TimeoutArmed then
          PollExecutionTimeout(
            PGocciaTimeoutPollState(FThreadPolls^.TimeoutState));
        PushClosedNumericFrame(A, B, C, Frame, Template, PrevCovLine,
          ProfileEntryTimestamp);
        Continue;
      end;

      OP_DIRECT_EVAL:
      begin
        if FThreadPolls^.TimeoutArmed then
          PollExecutionTimeout(
            PGocciaTimeoutPollState(FThreadPolls^.TimeoutState));
        ChildClosure := ExecuteDirectEvalSite(A, DecodeBx(Instruction),
          Template, Frame.IP, InstructionStartIP);
        // Eval code compiled at a strict call site runs as a frame of its
        // own, which returns its completion value to the callee register.
        if Assigned(ChildClosure) then
        begin
          PushFrame(A, Frame.IP, Template, PrevCovLine,
            ProfileEntryTimestamp);
          SetupNewFrame(ChildClosure, RegisterUndefined, nil, 0, True,
            Frame, Template, PrevCovLine, ProfileEntryTimestamp);
          Continue;
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
              SpreadArray.GetProperty(IntegerToString(I)));
          // The constructor runs guest code (see FRegisters).
          GuestCallRegister := TGocciaVMClassValue(FRegisters[B].ObjectValue)
            .InstantiateRegisters(RegisterArgs);
          FRegisters[A] := GuestCallRegister;
        end
        else
        begin
          if not MayBeConstructor(GetRegister(B)) then
            ThrowNotConstructorHere(GetRegister(B));
          // Snapshot/stamp/restore as in OP_CONSTRUCT, so a successful spread
          // construct does not leave the caller frame stamped for a later throw.
          if FCallStack <> nil then
            SavedConstructFrameOk :=
              FCallStack.TryGetTopFrame(SavedConstructFrame)
          else
            SavedConstructFrameOk := False;
          StampCurrentCallSiteLocation;
          CallArgs := AcquireArguments(SpreadArray.Elements.Count);
          try
            for I := 0 to SpreadArray.Elements.Count - 1 do
              CallArgs.Add(SpreadArray.GetProperty(IntegerToString(I)));
            EnterCurrentInstructionCallSite(PreviousCallSite);
            try
              SetRegister(A, ConstructValue(GetRegister(B), CallArgs));
            finally
              LeaveGocciaCallSite(PreviousCallSite);
            end;
          finally
            ReleaseArguments(CallArgs);
          end;
          if SavedConstructFrameOk and (FCallStack <> nil) then
            FCallStack.SetTopFrame(SavedConstructFrame);
        end;
      end;

      OP_GET_ITER:
        SetRegister(A, GetIteratorValue(GetRegister(B), C <> 0));

      OP_ASYNC_ITER_NEXT:
      begin
        if (FRegisters[C].Kind = grkObject) and
           (FRegisters[C].ObjectValue is TGocciaIteratorValue) then
        begin
          IterResult := TGocciaIteratorValue(FRegisters[C].ObjectValue).DirectNext(DoneFlag);
          SetRegister(A, CreateIteratorResult(IterResult, DoneFlag));
        end
        else if (FRegisters[C].Kind = grkObject) and
                (FRegisters[C].ObjectValue is TGocciaVMAsyncFromSyncIteratorValue) then
          // GetIteratorValue's own CreateAsyncFromSyncIterator result: no code
          // can reach it to replace its next, so the step is called directly.
          SetRegister(A, TGocciaVMAsyncFromSyncIteratorValue(
            FRegisters[C].ObjectValue).NextStep(False, nil))
        else if (FRegisters[C].Kind = grkObject) and
                (FRegisters[C].ObjectValue is TGocciaVMAsyncIteratorRecordValue) then
        begin
          // GetIteratorValue's own record of an async iterator: its next is
          // the captured [[NextMethod]], called on [[Iterator]] with no
          // argument (ES2026 §14.7.5.7 ForIn/OfBodyEvaluation step 6.a).
          CallArgs := AcquireArguments;
          try
            IterResult := TGocciaVMAsyncIteratorRecordValue(
              FRegisters[C].ObjectValue).Next(CallArgs, nil);
          finally
            ReleaseArguments(CallArgs);
          end;
          SetRegister(A, IterResult);
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
        if C = ITER_CLOSE_ASYNC_RETURN then
        begin
          IteratorElementValue := CallAsyncIteratorReturn(GetRegister(A));
          if Assigned(IteratorElementValue) then
            SetRegister(B, IteratorElementValue)
          else
            FRegisters[B] := RegisterHole;
        end
        else if C = ITER_CLOSE_IS_GENERATOR_RETURN then
          FRegisters[A] := RegisterBoolean(
            (FRegisters[B].Kind = grkObject) and
            Assigned(GActiveBytecodeGenerator) and
            Assigned(GActiveBytecodeGenerator.FReturnSentinel) and
            (FRegisters[B].ObjectValue =
             GActiveBytecodeGenerator.FReturnSentinel))
        else if C = ITER_CLOSE_ASYNC_RESULT then
        begin
          // ES2026 §7.4.14 AsyncIteratorClose step 7.
          IteratorElementValue := GetRegister(B);
          if (IteratorElementValue is TGocciaUndefinedLiteralValue) or
             (IteratorElementValue is TGocciaNullLiteralValue) or
             IteratorElementValue.IsPrimitive then
            ThrowTypeError(SErrorIteratorReturnObject,
              SSuggestIteratorResultObject);
        end
        else if FRegisters[A].Kind = grkObject then
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
            CloseRawIteratorPreservingError(FRegisters[A].ObjectValue)
          else
            CloseRawIterator(FRegisters[A].ObjectValue);
        end;

      OP_AWAIT:
      begin
        if Assigned(FCurrentAsyncPromise) and Assigned(Template) and
           Template.IsAsync then
        begin
          AwaitContinuation := GActiveBytecodeGenerator;
          if Template.IsGenerator then
          begin
            // The generator object is this body's continuation.
          end
          else if Assigned(EntryGenerator) and
                  (AwaitContinuation = EntryGenerator) and
                  (FFrameStackCount <= InitialFrameStackCount) and
                  (AwaitContinuation.FClosure = FCurrentClosure) then
            // This activation is the one an earlier await suspended: this
            // native entry was started by resuming that continuation, and
            // the await is in the entry's own frame, not a nested call's. It
            // suspends into the same continuation again, with the same
            // arguments, rather than into a fresh copy per await.
            AwaitContinuation.FThisValue := GetLocalRegister(0)
          else
            AwaitContinuation := TGocciaBytecodeGeneratorObjectValue.CreateRegisters(
              Self, FCurrentClosure, GetLocalRegister(0),
              CurrentArgumentsSnapshot, False);

          if Assigned(AwaitContinuation) then
          begin
            AwaitContinuation.CaptureContinuation(Frame, SavedHandlerCount,
              PrevCovLine, A, Frame.IP);
            AwaitContinuation.FState := bgsSuspendedYield;
            AttachAwaitContinuation(Self, AwaitContinuation, GetRegister(B),
              Template.IsGenerator);
            // A caller that asked for it learns of the suspension from the
            // flag, and the entry returns through the same teardown an
            // exception would take, without one.
            if Assigned(ASuspension) and
               (FFrameStackCount <= InitialFrameStackCount) and
               (FClosedNumericFrameStackCount <=
                InitialClosedNumericFrameCount) then
            begin
              ASuspension^.Kind := bskAwait;
              Exit(RegisterUndefined);
            end;
            raise EGocciaBytecodeAsyncSuspend.Create('');
          end;
        end;
        SetRegister(A, AwaitValue(GetRegister(B)));
      end;

      OP_YIELD:
      begin
        AwaitContinuation := GActiveBytecodeGenerator;
        if Assigned(AwaitContinuation) then
        begin
          if (C and 1) <> 0 then
            AwaitContinuation.HandleYieldDelegate(
              FRegisters[A], B, Frame, SavedHandlerCount, PrevCovLine,
              InstructionStartIP)
          else
            AwaitContinuation.HandleYield(
              FRegisters[A], B, Frame, SavedHandlerCount, PrevCovLine,
              Frame.IP);
          if AwaitContinuation.FYieldPending then
          begin
            // The body yielded. A resumption that asked for it learns of the
            // suspension from ASuspension, and the entry returns through the
            // same teardown an exception would take, without one.
            AwaitContinuation.FYieldPending := False;
            if Assigned(ASuspension) and
               (FFrameStackCount <= InitialFrameStackCount) and
               (FClosedNumericFrameStackCount <=
                InitialClosedNumericFrameCount) then
            begin
              ASuspension^.Kind := bskYield;
              ASuspension^.YieldValue := AwaitContinuation.FPendingYieldValue;
              ASuspension^.YieldIndex := AwaitContinuation.FPendingYieldIndex;
              Exit(RegisterUndefined);
            end;
            raise EGocciaBytecodeYield.Create(
              AwaitContinuation.FPendingYieldValue,
              AwaitContinuation.FPendingYieldIndex);
          end;
        end
        else if A <> B then
          FRegisters[B] := FRegisters[A];
      end;

      OP_SETUP_AUTO_ACCESSOR_CONST:
        SetupAutoAccessorValue(Template.GetConstantUnchecked(C).StringValue,
          B, RegisterToValue(FRegisters[A]));

      // R[A+1] of the decorator opcodes after OP_BEGIN_DECORATORS holds the
      // session OP_BEGIN_DECORATORS stored in R[B] for the same class.
      OP_BEGIN_DECORATORS:
        SetRegister(B, BeginDecorators(RegisterToValue(FRegisters[A]),
          RegisterToValue(FRegisters[A + 1])));

      OP_APPLY_ELEMENT_DECORATOR_CONST:
        if B <> 0 then
          ApplyElementDecorator(RegisterToValue(FRegisters[A + 1]),
            RegisterToValue(FRegisters[A]),
            Template.GetConstantUnchecked(C).StringValue,
            RegisterToValue(FRegisters[B]))
        else
          ApplyElementDecorator(RegisterToValue(FRegisters[A + 1]),
            RegisterToValue(FRegisters[A]),
            Template.GetConstantUnchecked(C).StringValue);

      OP_APPLY_CLASS_DECORATOR:
        ApplyClassDecorator(RegisterToValue(FRegisters[A + 1]),
          RegisterToValue(FRegisters[A]));

      OP_FINISH_DECORATORS:
        SetRegister(A, FinishDecorators(RegisterToValue(FRegisters[A + 1]),
          RegisterToValue(FRegisters[A])));

      OP_SET_GLOBAL:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) then
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
            ThrowReferenceError(Format(SErrorUndefinedVariable, [GlobalName]),
              SSuggestDeclareBeforeUse);
          end;
        end;
      end;

      OP_SET_GLOBAL_LOOSE:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        if Assigned(FGlobalScope) then
        begin
          GlobalBindingValue := RegisterToValue(FRegisters[A]);
          if FGlobalScope.ContainsOwnVarBinding(GlobalName) and
             (FGlobalScope.ThisValue is TGocciaObjectValue) and
             VMTrySetOwnWritableDataProperty(
               TGocciaObjectValue(FGlobalScope.ThisValue), GlobalName,
               GlobalBindingValue) then
            Continue;
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

      OP_DELETE_GLOBAL:
      begin
        GlobalName := Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue;
        // Deleting through the global object can reach a Proxy trap on its
        // prototype chain, which runs guest code (see FRegisters).
        if Assigned(FGlobalScope) then
          GuestCallBoolean := FGlobalScope.DeleteBinding(GlobalName)
        else
          GuestCallBoolean := True;
        FRegisters[A] := RegisterBoolean(GuestCallBoolean);
      end;

      OP_IMPORT:
        begin
          if Assigned(Template.DebugInfo) and
             (Template.DebugInfo.SourceFile <> '') then
            GlobalName := Template.DebugInfo.SourceFile
          else
            GlobalName := FCurrentModuleSourcePath;
          { The import declaration is the call site of what its load decides
            or refuses, as for import(). }
          CurrentInstructionDebugLocation(DebugLine, DebugColumn);
          EnterGocciaCallSite(GlobalName, DebugLine, DebugColumn,
            PreviousCallSite);
          try
            SetRegister(A, ImportModuleValue(
              Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue,
              GlobalName));
          finally
            LeaveGocciaCallSite(PreviousCallSite);
          end;
        end;

      OP_IMPORT_DEFER:
        begin
          if Assigned(Template.DebugInfo) and
             (Template.DebugInfo.SourceFile <> '') then
            GlobalName := Template.DebugInfo.SourceFile
          else
            GlobalName := FCurrentModuleSourcePath;
          { The import declaration is the call site of what its load decides
            or refuses, as for import(). }
          CurrentInstructionDebugLocation(DebugLine, DebugColumn);
          EnterGocciaCallSite(GlobalName, DebugLine, DebugColumn,
            PreviousCallSite);
          try
            SetRegister(A, ImportDeferredModuleNamespaceValue(
              Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue,
              GlobalName));
          finally
            LeaveGocciaCallSite(PreviousCallSite);
          end;
        end;

      OP_IMPORT_SOURCE:
        begin
          if Assigned(Template.DebugInfo) and
             (Template.DebugInfo.SourceFile <> '') then
            GlobalName := Template.DebugInfo.SourceFile
          else
            GlobalName := FCurrentModuleSourcePath;
          { The import declaration is the call site of what its load decides
            or refuses, as for import(). }
          CurrentInstructionDebugLocation(DebugLine, DebugColumn);
          EnterGocciaCallSite(GlobalName, DebugLine, DebugColumn,
            PreviousCallSite);
          try
            SetRegister(A, ImportModuleSourceValue(
              Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue,
              GlobalName));
          finally
            LeaveGocciaCallSite(PreviousCallSite);
          end;
        end;

      OP_GET_IMPORT_BINDING:
        begin
          GlobalName := Template.GetConstantUnchecked(B).StringValue;
          if (FRegisters[A].Kind = grkObject) and
             (FRegisters[A].ObjectValue is TGocciaModuleNamespaceObject) then
          begin
            if not TGocciaModuleNamespaceObject(
               FRegisters[A].ObjectValue).TryGetExportValue(
               GlobalName, GlobalBindingValue) then
            begin
              if Assigned(TGocciaModuleNamespaceObject(
                 FRegisters[A].ObjectValue).Module) then
                ThrowMissingImportExport(
                  Template.GetConstantUnchecked(C).StringValue, GlobalName)
              else
                ThrowSyntaxError(Format('Module has no export named "%s"',
                  [GlobalName]));
            end;
            SetRegister(A, GlobalBindingValue);
          end
          else
            SetRegister(A, GetPropertyValue(GetRegister(A), GlobalName));
        end;

      // ES2026 §9.1.1.5.5 CreateImportBinding(envRec, N, M, N2): the global
      // scope keeps an immutable indirect binding to the module's export, so
      // a later script against that scope reads the export's current value.
      // An export the module cannot resolve leaves the predeclared name
      // uninitialized.
      OP_CREATE_GLOBAL_IMPORT_BINDING:
        begin
          GlobalName := Template.GetConstantUnchecked(C).StringValue;
          if Assigned(FGlobalScope) and (FRegisters[A].Kind = grkObject) and
             (FRegisters[A].ObjectValue is TGocciaModuleNamespaceObject) and
             TGocciaModuleNamespaceObject(
               FRegisters[A].ObjectValue).CanResolveExport(GlobalName) then
            FGlobalScope.CreateImportBinding(
              Template.GetConstantUnchecked(B).StringValue,
              TGocciaModuleNamespaceObject(FRegisters[A].ObjectValue).Module,
              GlobalName);
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

      // Marks the next dynamic import's specifier as computed (ADR 0122).
      OP_COMPUTED_IMPORT_SPECIFIER:
        FComputedDynamicImportPending := True;

      // ES2026 §13.3.10.1 ImportCall — import(specifier)
      OP_DYNAMIC_IMPORT:
      begin
        DynImportComputed := FComputedDynamicImportPending;
        FComputedDynamicImportPending := False;
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
            if DynImportComputed then
              SpecifierString := MarkComputedImportSpecifier(SpecifierString);
            { The import() is the call site of whatever its load decides or
              refuses (audit events, PermissionDenied); a deferred start
              re-enters it. }
            CurrentCallExpressionLocation(DebugLine, DebugColumn);
            EnterGocciaCallSite(GlobalName, DebugLine, DebugColumn,
              PreviousCallSite);
            try
              case C of
                Ord(icpEvaluation):
                  if (TGocciaMicrotaskQueue.Instance <> nil) then
                  begin
                    DynImportTask.Handler :=
                      TGocciaVMDynamicImportStartValue.Create(Self,
                        DynImportPromise, SpecifierString, GlobalName,
                        DebugLine, DebugColumn);
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
            finally
              LeaveGocciaCallSite(PreviousCallSite);
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
              CurrentCallExpressionLocation(DebugLine, DebugColumn);
              DynImportPromise.Reject(CreateImportCallErrorObject(ERROR_NAME,
                E.Message, GlobalName, DebugLine, DebugColumn));
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
        DynImportComputed := FComputedDynamicImportPending;
        FComputedDynamicImportPending := False;
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
            if DynImportComputed then
              SpecifierString := MarkComputedImportSpecifier(SpecifierString);
            CurrentCallExpressionLocation(DebugLine, DebugColumn);
            EnterGocciaCallSite(GlobalName, DebugLine, DebugColumn,
              PreviousCallSite);
            try
              case TGocciaOpCode(Op) of
                OP_DYNAMIC_IMPORT_OPTIONS:
                  if (TGocciaMicrotaskQueue.Instance <> nil) then
                  begin
                    DynImportTask.Handler :=
                      TGocciaVMDynamicImportStartValue.Create(Self,
                        DynImportPromise, SpecifierString, GlobalName,
                        DebugLine, DebugColumn);
                    DynImportTask.Value :=
                      TGocciaUndefinedLiteralValue.UndefinedValue;
                    DynImportTask.ResultPromise := nil;
                    DynImportTask.ReactionType := prtFulfill;
                    TGocciaMicrotaskQueue.Instance.Enqueue(DynImportTask);
                  end
                  else
                    ResolveDynamicImportPromise(DynImportPromise,
                      SpecifierString, GlobalName);
                OP_DYNAMIC_IMPORT_SOURCE_OPTIONS:
                  DynImportPromise.Resolve(ImportModuleSourceValue(
                    SpecifierString, GlobalName));
                OP_DYNAMIC_IMPORT_DEFER_OPTIONS:
                  DynImportPromise.Resolve(ImportDeferredModuleNamespaceValue(
                    SpecifierString, GlobalName));
              end;
            finally
              LeaveGocciaCallSite(PreviousCallSite);
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
              CurrentCallExpressionLocation(DebugLine, DebugColumn);
              DynImportPromise.Reject(CreateImportCallErrorObject(ERROR_NAME,
                E.Message, GlobalName, DebugLine, DebugColumn));
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
        // pending) records this location instead of the deferred frame's 0:0.
        // Snapshot/restore so it does not
        // perturb the location seen by later instructions.
        if FCallStack <> nil then
          SavedConstructFrameOk :=
            FCallStack.TryGetTopFrame(SavedConstructFrame)
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
            // GetDisposeMethod admits any callable, a callable Proxy too.
            RightValue := DispatchCallNoArgs(LeftValue,
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
          if SavedConstructFrameOk and (FCallStack <> nil) then
            FCallStack.SetTopFrame(SavedConstructFrame);
        end;
      end;

      OP_THROW: raise EGocciaBytecodeThrow.Create(GetRegister(A));

      OP_TO_BOOL:
        FRegisters[A] := RegisterBoolean(RegisterToBoolean(FRegisters[B]));

      OP_DEFINE_ACCESSOR_CONST:
      begin
        Constant := Template.GetConstantUnchecked(C);
        GlobalName := Constant.StringValue;
        if (B and ACCESSOR_FLAG_AUTO) <> 0 then
          DefineAutoAccessorHalf(RegisterToValue(FRegisters[A]), GlobalName,
            nil, RegisterToValue(FRegisters[A + 1]), B)
        else if (B and ACCESSOR_FLAG_STATIC) <> 0 then
        begin
          if Constant.Kind = bckPrivateName then
            DeclareBytecodePrivateNameForClass(
              RegisterToValue(FRegisters[A]), GlobalName, True);
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineStaticSetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]),
              Constant.Kind = bckPrivateName)
          else
            DefineStaticGetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]),
              Constant.Kind = bckPrivateName);
        end
        else
        begin
          if Constant.Kind = bckPrivateName then
            DeclareBytecodePrivateNameForClass(
              RegisterToValue(FRegisters[A]), GlobalName);
          if (B and ACCESSOR_FLAG_SETTER) <> 0 then
            DefineSetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]),
              Constant.Kind = bckPrivateName)
          else
            DefineGetterProperty(RegisterToValue(FRegisters[A]), GlobalName,
              RegisterToValue(FRegisters[A + 1]),
              Constant.Kind = bckPrivateName);
        end;
      end;

      OP_DEFINE_ACCESSOR_DYNAMIC:
      begin
        if (B and ACCESSOR_FLAG_AUTO) <> 0 then
        begin
          RightValue := RegisterToValue(FRegisters[C]);
          if RightValue is TGocciaSymbolValue then
            DefineAutoAccessorHalf(RegisterToValue(FRegisters[A]), '',
              RightValue, RegisterToValue(FRegisters[A + 1]), B)
          else
            DefineAutoAccessorHalf(RegisterToValue(FRegisters[A]),
              RightValue.ToStringLiteral.Value, nil,
              RegisterToValue(FRegisters[A + 1]), B);
        end
        else if (B and ACCESSOR_FLAG_STATIC) <> 0 then
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
            if (FRegisters[A].Kind = grkObject) and
               (FRegisters[A].ObjectValue is TGocciaArrayValue) then
              SpreadIterableIntoArray(
                TGocciaArrayValue(FRegisters[A].ObjectValue),
                RegisterToValue(FRegisters[C]))
            else
              IterableToArray(RegisterToValue(FRegisters[C]));

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

      OP_THROW_UNDEFINED_VARIABLE:
        ThrowReferenceError(Format(SErrorUndefinedVariable,
          [Template.GetConstantUnchecked(DecodeBx(Instruction)).StringValue]),
          SSuggestDeclareBeforeUse);

      OP_DEFINE_GLOBAL_VAR_DECL_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        // ES2026 §19.2.1.3 step 18: eval code's var binding is deletable.
        if Assigned(FGlobalScope) then
          FGlobalScope.DefineVariableBinding(GlobalName,
            TGocciaUndefinedLiteralValue.UndefinedValue, False,
            Template.GlobalEvalCode);
      end;

      OP_DEFINE_GLOBAL_VAR_LONG:
      begin
        GlobalName := Template.GetConstantUnchecked(
          DecodeBx(Instruction)).StringValue;
        GlobalBindingValue := GetRegister(A);
        // ES2026 §14.3.2.1: in eval code the binding exists from the
        // declaration instantiation, so the initializer is a PutValue to it.
        if Template.GlobalEvalCode then
        begin
          if Assigned(FGlobalScope) then
            FGlobalScope.AssignBinding(GlobalName, GlobalBindingValue, 0, 0,
              not Template.StrictCode);
          Continue;
        end;
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
        // ES2026 §19.2.1.3 step 17: eval code's function binding is deletable.
        if Assigned(FGlobalScope) then
          FGlobalScope.CreateGlobalFunctionBinding(GlobalName, GetRegister(A),
            Template.GlobalEvalCode);
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

        else
          // OP_CALL_SELF is a reserved runtime-only opcode outside
          // TGocciaOpCode (Goccia.Bytecode), so it is dispatched here. The
          // arguments are read straight out of the caller's registers, as in
          // OP_CALL: SetupNewFrame copies them before it acquires registers,
          // or straight into them when no arena grows.
          if Op = OP_CALL_SELF then
          begin
            if FThreadPolls^.TimeoutArmed then
              PollExecutionTimeout(
                PGocciaTimeoutPollState(FThreadPolls^.TimeoutState));
            PushFrame(A, Frame.IP, Template, PrevCovLine,
              ProfileEntryTimestamp);
            SetupNewFrame(FCurrentClosure, RegisterUndefined, @FRegisters[B],
              C, True, Frame, Template, PrevCovLine, ProfileEntryTimestamp);
            Continue;
          end
          else
            raise Exception.CreateFmt('Unsupported Goccia VM opcode in minimal executor: %d', [Op]);
        end;
