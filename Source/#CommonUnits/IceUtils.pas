unit IceUtils;

interface

uses SysUtils, Windows, Messages;

type
  TTimeScale = (tsSys, tsLoc); // шкала времени для FileTime функций: локальная/UTC
  TFileVersion = array[1..4] of Word; // версия файла

const
  NL = #13#10;
  VersionFmt = '%d.%d.%d.%d';

  function IfTh(AValue: Boolean; const ATrue: string; const AFalse: string = ''): string; overload; inline;
  function IfTh(AValue: Boolean; const ATrue: Integer; const AFalse: Integer = 0): Integer; overload; inline;
  function IfTh(AValue: Boolean; const ATrue: Cardinal; const AFalse: Cardinal = 0): Integer; overload; inline;
  function IfTh(AValue: Boolean; const ATrue: Pointer; const AFalse: Pointer = nil): Pointer; overload; inline;
  function IfTh(AValue: Boolean; const ATrue: Double; const AFalse: Double = 0): Double; overload; inline;
  // filesystem
  function GetFileTime(FileName: string; Scale: TTimeScale; pCreation, pLastAccess, pLastWrite: PDateTime): Boolean;
  function SetFileTime(FileName: string; Scale: TTimeScale; Creation, LastAccess, LastWrite: TDateTime): Boolean;
  function GetFileSize(FileName: string): Int64;
  function GetFileVersion(const Path: string): TFileVersion;
  function FormatFileVersion(const FileVer: TFileVersion; VerFmt: string = VersionFmt): string;
  // strings
  function ThousandsDivide(num: Integer): string; overload; inline;
  function ThousandsDivide(num: Int64): string; overload; inline;
  // binary
  procedure ZeroMem(var Dest; count: Integer); inline;
  // classes and stuff
  procedure CloseAndZeroHandle(var Handle: THandle); inline;
  function LastErrMsg: string; inline;
  function CopyTextToCB(const Text: string; hWnd: HWND = 0): Boolean;

implementation

// ********* Файлы, имена файлов ********* \\

// получение размера файла только по имени (а не по хэндлу, как GetFileSize, + поддерживает размеры > 4 Гб
function GetFileSize(FileName: string): Int64;
var attr: WIN32_FILE_ATTRIBUTE_DATA;
begin
  Result := 0;
  if not GetFileAttributesEx(PChar(FileName), GetFileExInfoStandard, @attr) then Exit;
  Result := (Int64(attr.nFileSizeHigh) shl (SizeOf(attr.nFileSizeHigh)*8)) or Int64(attr.nFileSizeLow);
end;

// получает версию указанного файла в виде 4-х чисел
function GetFileVersion(const Path: string): TFileVersion;
var
  H, Len: DWORD;
  Data: array of Byte;
  pffi: PVSFixedFileInfo;
begin
  FillChar(Result, SizeOf(Result), 0);

  Len := GetFileVersionInfoSize(PChar(Path), H);
  if Len = 0 then Exit;

  SetLength(Data, Len);
  if not GetFileVersionInfo(PChar(Path), H, Len, @Data[0]) then Exit;
  if not VerQueryValue(@Data[0], '\', Pointer(pffi), Len) then Exit;

  Result[1] := HiWord(pffi^.dwFileVersionMS);
  Result[2] := LoWord(pffi^.dwFileVersionMS);
  Result[3] := HiWord(pffi^.dwFileVersionLS);
  Result[4] := LoWord(pffi^.dwFileVersionLS);
end;

// Форматирует версию по указанному шаблону
function FormatFileVersion(const FileVer: TFileVersion; VerFmt: string): string;
begin
  Result := Format(VerFmt, [FileVer[1], FileVer[2], FileVer[3], FileVer[4]]);
end;

// ********* Временные метки файлов ********* \\

// преобразования DateTime <-> TFileTime, используемый виндой
function DateTimeToFileTime(Scale: TTimeScale; aDate: TDateTime): TFileTime;
var st: TSystemTime;
begin
  DateTimeToSystemTime(aDate, st);
  SystemTimeToFileTime(st, Result);
  if Scale = tsLoc then LocalFileTimeToFileTime(Result, Result);
end;

function FileTimeToDateTime(Scale: TTimeScale; aTime: TFileTime): TDateTime;
var st: TSystemTime;
begin
  if Scale = tsLoc then FileTimeToLocalFileTime(aTime, aTime);
  FileTimeToSystemTime(aTime, st);
  Result := SystemTimeToDateTime(st);
end;

// получение временной метки файла в удобном виде
function GetFileTime(FileName: string; Scale: TTimeScale; pCreation, pLastAccess, pLastWrite: PDateTime): Boolean;
var attr: WIN32_FILE_ATTRIBUTE_DATA;
begin
  Result := GetFileAttributesEx(PChar(FileName), GetFileExInfoStandard, @attr);
  if not Result then Exit;
  if pCreation   <> nil then pCreation^   := FileTimeToDateTime(Scale, attr.ftCreationTime);
  if pLastWrite  <> nil then pLastWrite^  := FileTimeToDateTime(Scale, attr.ftLastWriteTime);
  if pLastAccess <> nil then pLastAccess^ := FileTimeToDateTime(Scale, attr.ftLastAccessTime);
end;

// присвоение временных меток файлу, для ненужных полей передавать 0 в параметре
function SetFileTime(FileName: string; Scale: TTimeScale; Creation, LastAccess, LastWrite: TDateTime): Boolean;
var f: Integer;
    Cr, Acc, Wr: TFileTime;
begin
  f := FileOpen(FileName, fmOpenWrite or fmShareDenyNone);
  Result := f <> -1;
  if not Result then Exit;
  if Creation   <> 0 then Cr := DateTimeToFileTime(Scale, Creation);
  if LastWrite  <> 0 then Wr := DateTimeToFileTime(Scale, LastWrite);
  if LastAccess <> 0 then Acc := DateTimeToFileTime(Scale, LastAccess);
  Result := Windows.SetFileTime(f, IfTh(Creation   <> 0, @Cr, nil),
                                   IfTh(LastWrite  <> 0, @Wr, nil),
                                   IfTh(LastAccess <> 0, @Acc, nil));
  FileClose(f);
end;

// ********* костыли тернарного оператора ********* \\

function IfTh(AValue: Boolean; const ATrue: string; const AFalse: string): string;
begin
  if AValue then Result := ATrue else Result := AFalse;
end;

function IfTh(AValue: Boolean; const ATrue: Integer; const AFalse: Integer): Integer;
begin
  if AValue then Result := ATrue else Result := AFalse;
end;

function IfTh(AValue: Boolean; const ATrue: Cardinal; const AFalse: Cardinal): Integer;
begin
  if AValue then Result := ATrue else Result := AFalse;
end;

function IfTh(AValue: Boolean; const ATrue: Pointer; const AFalse: Pointer): Pointer;
begin
  if AValue then Result := ATrue else Result := AFalse;
end;

function IfTh(AValue: Boolean; const ATrue: Double; const AFalse: Double): Double;
begin
  if AValue then Result := ATrue else Result := AFalse;
end;

// ********* Строки ********* \\

// Возвращает число в строковом формате с разделёнными тысячными разрядами
function ThousandsDivide(num: Integer): string;
begin
  Result := Format('%.0n', [num+0.0]);
end;

function ThousandsDivide(num: Int64): string;
begin
  Result := Format('%.0n', [num+0.0]);
end;

// ********* Разное ********* \\

// Заполнение буфера нулями, отличие от ZeroMemory - inline и другие типы параметров
procedure ZeroMem(var Dest; count: Integer);
begin
  FillChar(Dest, count, 0);
end;

// Закрытие и обнуление хэндла. Не производит проверку на успешность!
procedure CloseAndZeroHandle(var Handle: THandle);
begin
  if Handle > 0 then CloseHandle(Handle);
  Handle := 0;
end;

function LastErrMsg: string;
begin
  Result := SysErrorMessage(GetLastError);
end;

// Copy the text unsing WinAPI only (Clipboard object requires Forms unit)
// hWnd is a handle of a window which will occupy the clipboard
function CopyTextToCB(const Text: string; hWnd: HWND): Boolean;
var
  hMem: HGLOBAL;
  Ptr: PChar;
begin
  Result := False;

  if Text = '' then Exit;

  // determine window handle which will occupy the clipboard
  if hWnd = 0 then
    hWnd := GetForegroundWindow;

  // occupy the clipboard
  if not OpenClipboard(hWnd) then Exit;
  try
    EmptyClipboard;

    // globally allocate some memory for the string including trailing zero
    hMem := GlobalAlloc(GMEM_MOVEABLE, (Length(Text)+1)*SizeOf(Char));
    if hMem = 0 then Exit;

    Ptr := PChar(GlobalLock(hMem));
    if Ptr <> nil then
    begin
      Move(Text[1], Ptr^, Length(Text)*SizeOf(Char));
      (Ptr + Length(Text))^ := #0;
      GlobalUnlock(hMem);
    end;

    // set the data, manually free allocated memory on error
    Result := SetClipboardData({$IFDEF UNICODE}CF_UNICODETEXT{$ELSE}CF_TEXT{$ENDIF}, hMem) <> 0;
    if not Result then
      GlobalFree(hMem);
  finally
    // release the clipboard
    CloseClipboard;
  end;
end;

end.
