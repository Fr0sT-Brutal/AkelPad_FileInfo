unit IceUtils;

interface

uses
  Winapi.Windows, Winapi.Messages, PseudoRTL.SysUtils;

type
  TTimeScale = (tsSys, tsLoc); // шкала времени для FileTime функций: локальная/UTC
  TFileVersion = array[1..4] of Word; // версия файла
  TReplTokenCallback = reference to function(var Token: string): Boolean;

const
  NL = #13#10;
  VersionFmt = '%d.%d.%d.%d';
  DateTimeFmt = '%.2d.%.2d.%.4d %.2d:%.2d';

  function IfTh(AValue: Boolean; const ATrue: string; const AFalse: string = ''): string; overload; inline;
  function IfTh(AValue: Boolean; const ATrue: Integer; const AFalse: Integer = 0): Integer; overload; inline;
  function IfTh(AValue: Boolean; const ATrue: Cardinal; const AFalse: Cardinal = 0): Integer; overload; inline;
  function IfTh(AValue: Boolean; const ATrue: Pointer; const AFalse: Pointer = nil): Pointer; overload; inline;
  function IfTh(AValue: Boolean; const ATrue: Double; const AFalse: Double = 0): Double; overload; inline;
  // filesystem
  function GetFileTime(FileName: string; Scale: TTimeScale; pCreation, pLastAccess, pLastWrite: PSystemTime): Boolean;
  function GetFileSize(FileName: string): Int64;
  function GetFileVersion(const Path: string): TFileVersion;
  function FormatFileVersion(const FileVer: TFileVersion; VerFmt: string = VersionFmt): string;
  function SystemTimeToStr(aTime: TSystemTime): string;
  // strings
  function ThousandsDivide(num: Integer): string; overload; inline;
  function ThousandsDivide(num: Int64): string; overload; inline;
  function ReplaceTokens(const Patt: string; TokenStart, TokenEnd: Char; EatUnmatched: Boolean;
                         Callback: TReplTokenCallback): string;
  function FindStr(const AText: string; const AValues: array of string): Integer;
  // binary
  procedure ZeroMem(var Dest; count: Integer); inline;
  // classes and stuff
  procedure CloseAndZeroHandle(var Handle: THandle); inline;
  function LastErrMsg: string;
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

function SystemTimeToStr(aTime: TSystemTime): string;
begin
  Result := Format(DateTimeFmt,
    [aTime.wDay, aTime.wMonth, aTime.wYear, aTime.wHour, aTime.wMonth]);
end;

// получение временных меток файла в структуры PSystemTime
function GetFileTime(FileName: string; Scale: TTimeScale; pCreation, pLastAccess, pLastWrite: PSystemTime): Boolean;

procedure SetSysTime(FileTime: TFileTime; Scale: TTimeScale; pST: PSystemTime); inline;
begin
  if pST = nil then Exit;
  if Scale = tsLoc then FileTimeToLocalFileTime(FileTime, FileTime);
  FileTimeToSystemTime(FileTime, pST^);
end;

var attr: WIN32_FILE_ATTRIBUTE_DATA;
begin
  Result := GetFileAttributesEx(PChar(FileName), GetFileExInfoStandard, @attr);
  if not Result then Exit;
  SetSysTime(attr.ftCreationTime  , Scale, pCreation  );
  SetSysTime(attr.ftLastWriteTime , Scale, pLastWrite );
  SetSysTime(attr.ftLastAccessTime, Scale, pLastAccess);
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

// Аналог PosEx, сделан, чтобы не подключать StrUtils
function PosOfs(const SubStr, Str: string; Offset: Integer): Integer;
var
  SubLen, SrcLen, Len, I, J: Integer;
  C1: Char;
begin
  Result := 0;
  if (Pointer(SubStr) = nil) or (Pointer(Str) = nil) then Exit;
  SrcLen := Length(Str);
  SubLen := Length(SubStr);
  if (SubLen <= 0) or (SrcLen <= 0) or (SrcLen < SubLen) or (Offset <= 0) then Exit;
  // find SubStr[1] in S[1 .. SrcLen - SubLen + 1]
  Len := SrcLen - SubLen + 1;
  C1 := PChar(SubStr)[0];
  for I := Offset - 1 to Len - 1 do
  begin
    if PChar(Str)[I] = C1 then
    begin
      Result := I + 1;
      for J := 1 to SubLen - 1 do
      begin
        if PChar(Str)[I+J] <> PChar(SubStr)[J] then
        begin
          Result := 0;
          Break;
        end;
      end;
      if Result <> 0 then
        Break;
    end;
  end;
end;

// Находит все вхождения строк между символами TokenStart и TokenEnd в строке Patt
// и заменяет их на результат выполнения функции Callback
// Флаг EatUnmatched определяет, удалять ли вхождения, для которых Callback
// возвращает False (если EatUnmatched = False, такие вхождения остаются как были)
function ReplaceTokens(const Patt: string; TokenStart, TokenEnd: Char; EatUnmatched: Boolean;
                       Callback: TReplTokenCallback): string;
var token: String;
    curr, txt_beg, pos_beg, pos_end: Integer;
begin
  curr := 1; txt_beg := 1; Result := '';
  // пока в шаблоне есть токены
  while curr <= Length(Patt) do
  begin
    // выделяем токен (символы, обрамлённые TokenStart и TokenEnd)
    pos_beg := PosOfs(TokenStart, Patt, curr);
    if pos_beg = 0 then Break;
    pos_end := PosOfs(TokenEnd, Patt, pos_beg+1);
    if pos_end = 0 then Break;
    token := Copy(patt, pos_beg+1, pos_end-pos_beg-1);
    // вызываем callback функцию и действуем по её результатам
    if not Callback(token) then
      if EatUnmatched // съедаем или оставляем необработанный токен
        then Result := Result + Copy(Patt, txt_beg, pos_beg-txt_beg)
        else begin Inc(curr); Continue; end
    else
      Result := Result + Copy(Patt, txt_beg, pos_beg-txt_beg) + token;
    // двигаемся дальше
    curr := pos_end+1;
    txt_beg := pos_end+1;
  end;
  // добавляем оставшийся хвост текста после последнего токена
  Result := Result + Copy(Patt, txt_beg, Length(Patt));
end;

// копипаста из StrUtils, только намного быстрее за счёт обычного сравнения
function FindStr(const AText: string; const AValues: array of string): Integer;
var I: Integer;
begin
  for I := Low(AValues) to High(AValues) do
    if AText = AValues[I] then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
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
