unit IceUtils;

interface

uses SysUtils, Windows, Messages;

type
  // для применения в MessageBox их надо сдвинуть на 16 влево
  // MB_ICONHAND = $00000010;
  // MB_ICONQUESTION = $00000020;
  // MB_ICONEXCLAMATION = $00000030;
  // MB_ICONASTERISK = $00000040;
  TMsgBoxIcon = (iNone, iStop, iQstn, iWarn, iInfo);

  TTimeScale = (tsSys, tsLoc); // шкала времени для FileTime функций: локальная/UTC

const
  MsgBoxTitles: array [TMsgBoxIcon] of string = ('','Ошибка!','Вопрос','Внимание!','Информация');

  bARI = MB_ABORTRETRYIGNORE;
  bOK  = MB_OK;
  bOC  = MB_OKCANCEL;
  bRC  = MB_RETRYCANCEL;
  bYN  = MB_YESNO;
  bYNC = MB_YESNOCANCEL;

  NL = #13#10;

  // dialog windows
  function MsgBox(               const txt: string; style: Integer=bOK): Integer; overload;
  function MsgBox(Hwnd: THandle; const txt: string; style: Integer=bOK): Integer; overload;
  function MsgBox(               const txt: string; Icon: TMsgBoxIcon; style: Integer=bOK): Integer; overload;
  function MsgBox(Hwnd: THandle; const txt: string; Icon: TMsgBoxIcon; style: Integer=bOK): Integer; overload;
  function IfTh(AValue: Boolean; const ATrue: string; const AFalse: string = ''): string; overload; inline;
  function IfTh(AValue: Boolean; const ATrue: Integer; const AFalse: Integer = 0): Integer; overload; inline;
  function IfTh(AValue: Boolean; const ATrue: Cardinal; const AFalse: Cardinal = 0): Integer; overload; inline;
  function IfTh(AValue: Boolean; const ATrue: Pointer; const AFalse: Pointer = nil): Pointer; overload; inline;
  function IfTh(AValue: Boolean; const ATrue: Double; const AFalse: Double = 0): Double; overload; inline;
  // filesystem
  function GetFileTime(FileName: string; Scale: TTimeScale; pCreation, pLastAccess, pLastWrite: PDateTime): Boolean;
  function SetFileTime(FileName: string; Scale: TTimeScale; Creation, LastAccess, LastWrite: TDateTime): Boolean;
  function GetFileSize(FileName: string): Int64;
  // strings
  function ThousandsDivide(num: Integer): string; overload; inline;
  function ThousandsDivide(num: Int64): string; overload; inline;
  // binary
  procedure ZeroMem(var Dest; count: Integer); inline;
  // classes and stuff
  procedure CloseAndZeroHandle(var Handle: THandle); inline;
  function LastErrMsg: string; inline;

implementation

// ********* Message box ********* \\

function MsgBox(const txt: string; style: Integer): Integer;
begin
  Result := MsgBox(0, txt, style);
end;

function MsgBox(Hwnd: THandle; const txt: string; style: Integer): Integer;
begin
  Result := MessageBox(Hwnd, PChar(txt), PChar(ExtractFileName(ParamStr(0))), style);
end;

function MsgBox(const txt: string; Icon: TMsgBoxIcon; style: Integer): Integer; overload;
begin
  Result := MsgBox(0, txt, Icon, style);
end;

function MsgBox(Hwnd: THandle; const txt: string; Icon: TMsgBoxIcon; style: Integer): Integer;
begin
  Result := MessageBox(Hwnd, PChar(txt), PChar(MsgBoxTitles[icon]), style+Integer(icon) shl 4);
end;

procedure Error(const msg: string);
begin
  raise Exception.Create(msg);
end;

// ********* Файлы, имена файлов ********* \\

// получение размера файла только по имени (а не по хэндлу, как GetFileSize, + поддерживает размеры > 4 Гб
function GetFileSize(FileName: string): Int64;
var attr: WIN32_FILE_ATTRIBUTE_DATA;
begin
  Result := 0;
  if not GetFileAttributesEx(PChar(FileName), GetFileExInfoStandard, @attr) then Exit;
  Result := (Int64(attr.nFileSizeHigh) shl (SizeOf(attr.nFileSizeHigh)*8)) or Int64(attr.nFileSizeLow);
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

end.
