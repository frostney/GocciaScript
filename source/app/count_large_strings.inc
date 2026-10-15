{$IF defined(COUNT_LARGE_STRINGS)}
threadvar
  LargeStringAllocs, LargeStringFrees: Integer;

procedure RecordLargeStringAlloc(Len: SizeInt); inline;
begin
  if Len >= 90000 then
  begin
    Inc(LargeStringAllocs);
    WriteLn(StdErr, 'LargeStringAlloc: len=', Len, ' total_live=', LargeStringAllocs - LargeStringFrees);
  end;
end;

procedure RecordLargeStringFree(Len: SizeInt); inline;
begin
  if Len >= 90000 then
  begin
    Inc(LargeStringFrees);
    WriteLn(StdErr, 'LargeStringFree: len=', Len, ' total_live=', LargeStringAllocs - LargeStringFrees);
  end;
end;

procedure ReportLargeStrings; inline;
begin
  WriteLn(StdErr, 'LargeStrings: allocs=', LargeStringAllocs, ' frees=', LargeStringFrees, ' live=', LargeStringAllocs - LargeStringFrees);
end;
{$ENDIF}
