unit PseudoRTL.SysUtils;

interface

uses
  Winapi.Windows;

type
  Exception = class
  private
    FMessage: string;
  public
    constructor Create(const Message: string);
    property Message: string read FMessage;
  end;

  function FileExists(const FileName: string; FollowLink: Boolean = True): Boolean;
  function SysErrorMessage(ErrorCode: Cardinal): string;
  procedure FreeAndNil(var Obj); inline;
  function Format(const Patt: string; const Args: array of const): string;
  function IntToStr(i: Integer): string; overload;
  function IntToStr(i: Int64): string; overload;
  procedure Error(const Message: string);

implementation

{ File attribute constants }

const
  faInvalid     = -1;
  faReadOnly    = $00000001;
  faHidden      = $00000002 platform; // only a convention on POSIX
  faSysFile     = $00000004 platform; // on POSIX system files are not regular files and not directories
  faVolumeID    = $00000008 platform deprecated;  // not used in Win32
  faDirectory   = $00000010;
  faArchive     = $00000020 platform;
  faNormal      = $00000080;
  faTemporary   = $00000100 platform;
  faSymLink     = $00000400 platform; // Available on POSIX and Vista and above
  faCompressed  = $00000800 platform;
  faEncrypted   = $00004000 platform;
  faVirtual     = $00010000 platform;
  faAnyFile     = $000001FF;

function FileExists(const FileName: string; FollowLink: Boolean = True): Boolean;
  function ExistsLockedOrShared(const Filename: string): Boolean;
  var
    FindData: TWin32FindData;
    LHandle: THandle;
  begin
    { Either the file is locked/share_exclusive or we got an access denied }
    LHandle := FindFirstFile(PChar(Filename), FindData);
    if LHandle <> INVALID_HANDLE_VALUE then
    begin
      Winapi.Windows.FindClose(LHandle);
      Result := FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0;
    end
    else
      Result := False;
  end;

var
  Flags: Cardinal;
  Handle: THandle;
  LastError: Cardinal;
begin
  Flags := GetFileAttributes(PChar(FileName));

  if Flags <> INVALID_FILE_ATTRIBUTES then
  begin
    if faSymLink and Flags <> 0 then
    begin
      if not FollowLink then
        Exit(True)
      else
      begin
        if faDirectory and Flags <> 0 then
          Exit(False)
        else
        begin
          Handle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil,
            OPEN_EXISTING, 0, 0);
          if Handle <> INVALID_HANDLE_VALUE then
          begin
            CloseHandle(Handle);
            Exit(True);
          end;
          LastError := GetLastError;
          Exit(LastError = ERROR_SHARING_VIOLATION);
        end;
      end;
    end;

    Exit(faDirectory and Flags = 0);
  end;

  LastError := GetLastError;
  Result := (LastError <> ERROR_FILE_NOT_FOUND) and
    (LastError <> ERROR_PATH_NOT_FOUND) and
    (LastError <> ERROR_INVALID_NAME) and ExistsLockedOrShared(Filename);
end;

// Exceptions

function ExceptionClass(P: PExceptionRecord): TClass;
begin
  Result := Exception;
end;

function ExceptionObject(P: PExceptionRecord): TObject;
begin
  Result := Exception.Create('');
end;

function SysErrorMessage(ErrorCode: Cardinal): string;
var
  Buffer: PChar;
  Len: Integer;
begin
  { Obtain the formatted message for the given Win32 ErrorCode
    Let the OS initialize the Buffer variable. Need to LocalFree it afterward.
  }
  Len := FormatMessage(
    FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_IGNORE_INSERTS or
    FORMAT_MESSAGE_ARGUMENT_ARRAY or
    FORMAT_MESSAGE_ALLOCATE_BUFFER, nil, ErrorCode, 0, @Buffer, 0, nil);

  try
    { Convert to Delphi string }
    SetString(Result, Buffer, Len);
  finally
    { Free the OS allocated memory block }
    LocalFree(HLOCAL(Buffer));
  end;
end;

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

function Format(const Patt: string; const Args: array of const): string;
var
  Buffer: PChar;
  Len, i: Integer;
  InternalArgs: array of Pointer;
begin
  SetLength(InternalArgs, Length(Args));
  for i := Low(InternalArgs) to High(InternalArgs) do
    InternalArgs[i] := Pointer(Args[i].VPointer);
  { Obtain the formatted message for the given Win32 ErrorCode
    Let the OS initialize the Buffer variable. Need to LocalFree it afterward.
  }
  Len := FormatMessage(
    FORMAT_MESSAGE_FROM_STRING or
    FORMAT_MESSAGE_ARGUMENT_ARRAY or
    FORMAT_MESSAGE_ALLOCATE_BUFFER, PChar(Patt), 0, 0, @Buffer, 0, Pointer(InternalArgs));
  try
    { Convert to Delphi string }
    SetString(Result, Buffer, Len);
  finally
    { Free the OS allocated memory block }
    LocalFree(HLOCAL(Buffer));
  end;
end;

function IntToStr(i: Integer): string;
begin
  Str(i, Result);
end;

function IntToStr(i: Int64): string;
begin
  Str(i, Result);
end;

procedure Error(const Message: string);
begin
  raise Exception.Create(Message);
end;

{ Exception }

constructor Exception.Create(const Message: string);
begin
  FMessage := Message;
end;

initialization
  ExceptClsProc := @ExceptionClass;
  ExceptObjProc := @ExceptionObject;

end.
