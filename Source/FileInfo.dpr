        (*****************************************
            FileInfo plugin for AkelPad editor
                         © Fr0sT

  Shows properties of a currently edited file as long as some
  statistics for its contents.
  Something similar to Stats plugin but provides more info.
  Additional functions:
    + Browse for file in Explorer
    + Copy path to clipboard
    + Rename file (!)
    + Copy statistics to clipboard

         *****************************************)

{
 IN PROGRESS:

 TO DO:
   * Report - if selection present
   * Default icon when couldn't get - ?
   * "I did a test on this text: Text for test."
   * Commands: Open in assoc program, Show explorer menu, System props
   * возможность копировать только имя / только путь
   * Значок "крестик", когда файл не сохранён, можно помещать в то место,
     где выводится иконка файла, и там же справа, где указывается имя сохранённого файла,
     выводить надпись "Документ не сохранён в файл", но не в поле, а лейблом, как сейчас,
     чтобы не сбивало с толку.

 ???
   * icons for buttons
   * error on renaming - clear stats?
   * Вообще, чтобы минимизировать интерфейс, можно было бы лейбл "Путь" преобразовать в
     "гиперссылку" и тогда отпала бы необходимость в кнопке "Обзор", открывающей файл в Проводнике.
   * А кнопку "копир. путь" можно было бы сделать стандартным значком копирования в буфер справа
     от поля с именем файла

}

library FileInfo;

{$R *.RES}
{$R 'Dialog.res' 'Dialog.rc'}
{$R Icon.res}

uses
  Windows, Messages, SysUtils, Character, CommCtrl, ShellApi,
  IceUtils, ResDialog,
  AkelDLL_h in '#AkelDefs\AkelDLL_h.pas',
  AkelEdit_h in '#AkelDefs\AkelEdit_h.pas',
  Lang in 'Lang.pas';

// Global constants

const
  PluginName: AnsiString = 'FileInfo';

type
  // statistics that are counted in the separate thread
  TDocCountState = (cntNotActual, cntInProcess, cntCompleted);

  TDocCountStats = record
    State: TDocCountState;     // Current counting state
                               //   cntNotActual - the values are obsolete and must not be shown
                               //   cntInProcess - the count is running, some values would be shown and some wouldn't
                               //   cntCompleted - the count has finished normally, show all values
    Lines,                     // without word wrap
    Chars,                     // total chars
    Words,                     // according to Akel settings
    WhiteSpaces,               // spaces, tabs, etc
    Latin,                     // latin letters
    Letters,                   // all letters
    Surrogates                 // surrogate pairs
      : Int64;
  end;

  // full set of file and document properties and stats
  TFileStats = record
    // file props
    FileName: string;          // Full file path
    FileSize: Int64;
    Created,
    Modified: TDateTime;
    hIcon: HICON;              // Shell icon handle
    // general document info
    CodePage: Integer;         //
    Selection,
    IsModified: Boolean;
    // counters
    Counters: TDocCountStats;
  end;

  // internal data and structures for counting

  TAkelData = record
    hMainWnd,        // main Akel window
    hEditWnd: HWND;  // current editor window
  end;

  // current counting state
  TCountProgress = record
    PercentDone: Cardinal;
    Counters: TDocCountStats;
  end;
  PCountProgress = ^TCountProgress;

  TCountCallback = procedure(var AkelData: TAkelData; var CountProgress: TCountProgress; var Continue: Boolean);

  TThreadState = (stInactive, stRunning, stTerminated);

  // counter thread
  TCountThread = class
  strict private
    FhThread: THandle;
    FidThread: DWORD;
    FhTargetWnd: HWND;
    FAkelData: TAkelData;
    FState: TThreadState;
    FFileName: string;
  private
    procedure CountCallback(var AkelData: TAkelData; var CountProgress: TCountProgress; var Continue: Boolean);
    function Execute: DWORD;
  public
    procedure Run(TargetWnd: HWND; const AkelData: TAkelData; const FileStats: TFileStats);
    procedure Stop;

    property State: TThreadState read FState;
  end;

const
  WordsPerCycle = 2000;  {}
  CharsPerCycle = 5000;  {}

  MSG_BASE           = WM_APP + $FF0;
  // Count thread -> main dialog window
  //   Update counters.
  //   wParam: TThreadState
  //   lParam: pointer to TCountProgress record
  MSG_UPD_COUNT      = MSG_BASE + 1;
  // Count thread -> main dialog window
  //   Shell icon of the file is extracted, show it
  //   wParam: HICON
  //   lParam: 0
  MSG_ICON_EXTRACTED = MSG_BASE + 2;
  // "Filename" edit control -> File props dialog
  //   User commanded to rename the file
  MSG_RENAME_FILE    = MSG_BASE + 3;

type
  TPluginCommand = (cmdBrowse, cmdCopyPath, cmdRename, cmdGetReport);
const
  PluginCommands: array[TPluginCommand] of string =
    ('browse', 'copypath', 'rename', 'report');
  BrowseCmd = 'explorer.exe /e, /select, %s';
  ReportPattProp = '%s: %s'+NL;       // prop name / prop value
  ReportPattAll = '== %s =='+NL+      // filename / file props / doc props
                  '%s'+NL+
                  '*****'+NL+
                  '%s';

// Interface

{$I Dialog.inc}  // dialog IDs

type
  // tab pages
  TTabPage = (tabFile, tabDoc);

  // dialog classes
  TPageDlg = class;

  TMainDlg = class(TResDialog)
  strict private
    FPages: array[TTabPage] of TPageDlg;
    FAppIcon: HICON;
    FTabPageCaptions: array[TTabPage] of string;
    procedure DoDialogProc(msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT); override;
  public
    constructor Create(const pd: TPLUGINDATA);
    destructor Destroy; override;
  end;

  TPageDlg = class(TResDialog)
  strict private
    FOwner: TMainDlg;
    FPage: TTabPage;
    FValuesWereSet: Boolean;
    procedure DoDialogProc(msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT); override;
  public
    constructor Create(const pd: TPLUGINDATA; Owner: TMainDlg; Page: TTabPage);
    procedure SetValues;
  end;

const
  TabPageIDs: array[TTabPage] of Integer =
    (IDD_INFO_FILE, IDD_INFO_DOC);

// Global variables

var
  FileStats: TFileStats;
  MainDlg: TMainDlg;
  CountThread: TCountThread;
  AkelData: TAkelData;
  hiWarn, hiErr: HICON;
  PrevEditWndProc: TFNWndProc;
  PluginCommand: TPluginCommand = TPluginCommand(-1);

{$REGION 'SERVICE FUNCTIONS'}

// Retrieve file properties.
function GetFileInfo(const AkelData: TAkelData; var FileStats: TFileStats): Boolean;
var ei: TEDITINFO;
    crInit: TAECHARRANGE;
begin
  // clear file info fields
  DestroyIcon(FileStats.hIcon);
  FileStats.FileName := '';
  FileStats.FileSize := 0;
  FileStats.Created := 0;
  FileStats.Modified := 0;
  FileStats.hIcon := 0;
  FileStats.CodePage := 0;
  FileStats.Selection := False;
  FileStats.IsModified := False;

  ZeroMem(ei, SizeOf(ei));
  if (AkelData.hMainWnd = 0) or
     (AkelData.hEditWnd = 0) or
     (SendMessage(AkelData.hMainWnd, AKD_GETEDITINFO, 0, LPARAM(@ei)) = 0) then
       Exit(False);

  if ei.wszFile <> nil then
    FileStats.FileName := string(ei.wszFile)
  else if ei.szFile <> nil then
    FileStats.FileName := string(AnsiString(ei.szFile));
  if FileStats.FileName <> '' then
  begin
    FileStats.FileSize := GetFileSize(FileStats.FileName);
    GetFileTime(FileStats.FileName, tsLoc, @FileStats.Created, nil, @FileStats.Modified);
  end;

  FileStats.IsModified := ei.bModified;
  FileStats.CodePage := ei.nCodePage;

  // Check if there's selection present
  SendMessage(AkelData.hEditWnd, AEM_GETINDEX, AEGI_FIRSTSELCHAR, LPARAM(@crInit.ciMin));
  SendMessage(AkelData.hEditWnd, AEM_GETINDEX, AEGI_LASTSELCHAR,  LPARAM(@crInit.ciMax));
  FileStats.Selection := (AEC_IndexCompare(crInit.ciMin, crInit.ciMax) <> 0);

  Result := True;
end;

// Extract shell icon of the file
// ! Executes in the context of the counter thread !
function ExtractShellIcon(const FileName: string): HICON;
var ShInf: TSHFileInfo;
begin
  if FileStats.FileName <> '' then  {}
  begin
    ZeroMem(ShInf, SizeOf(ShInf));
    SHGetFileInfo(PChar(FileName), FILE_ATTRIBUTE_NORMAL, ShInf, SizeOf(ShInf),
                  SHGFI_USEFILEATTRIBUTES or SHGFI_ICON or SHGFI_LARGEICON); {}//check result
//    if ShInf.hIcon then

    Result := ShInf.hIcon;
    {}// load empty icon
  end;
end;

// Retrieve document properties.
// ! Executes in the context of the counter thread !
// Acts on the basis of CountData.CountState (and changes it when count stage is changing).
// Based on the Stats plugin code by Instructor
procedure GetDocInfo(var AkelData: TAkelData; CountCallback: TCountCallback);
var
  line1, line2, WordCnt, CharCnt, tmp: Integer;
  Selection, Wrap, ColumnSel: Boolean;  // current document modes
  CountProgress: TCountProgress;
  IsFirst, IsLast: Boolean;
  CurrChar: WideChar;
  crInit: TAECHARRANGE;
  crCount: TAECHARRANGE;
  ciWordStart: TAECHARINDEX;
  ciWordEnd: TAECHARINDEX;
  ciCount: TAECHARINDEX;
  isChars: TAEINDEXSUBTRACT;
  CharsProcessed: Int64;                // how much chars have been already processed
const
  PercentPerStage = 100/2;  // how much total percents a single count stage occupies

// Launch the given callback function (if any).
// Returns False if the process must be interrupted.
// Uses external variables: CountCallback, CountData, CountProgress
function RunCallback: Boolean;
begin
  Result := True;
  if not Assigned(CountCallback) then Exit;
  CountCallback(AkelData, CountProgress, Result);
end;

// Recalculate current percent and launch callback
// Uses external variables: CountData, CountProgress, CharsProcessed, WordCnt, CharCnt
function ReportWordCount: Boolean;
begin
  // update the counters
  Inc(CountProgress.Counters.Words, WordCnt);
  Inc(CharsProcessed, CharCnt);
  WordCnt := 0;
  CharCnt := 0;
  // get current percent value
  CountProgress.PercentDone := Trunc(PercentPerStage*0 + CharsProcessed*PercentPerStage/CountProgress.Counters.Chars);
  Result := RunCallback;
end;

// Recalculate current percent and launch callback
// Uses external variables: CountData, CountProgress, CharsProcessed, WordCnt, CharCnt
function ReportCharCount: Boolean;
begin
  // update the counters
  Inc(CharsProcessed, CharCnt);
  CharCnt := 0;
  // get current percent value
  CountProgress.PercentDone := Trunc(PercentPerStage*1 + CharsProcessed*PercentPerStage/CountProgress.Counters.Chars);
  Result := RunCallback;
end;

begin
  ZeroMem(CountProgress, SizeOf(CountProgress));
  CountProgress.Counters.State := cntInProcess;

  // *** get basic document info ***

  SendMessage(AkelData.hEditWnd, AEM_GETINDEX, AEGI_FIRSTSELCHAR, LPARAM(@crInit.ciMin));
  SendMessage(AkelData.hEditWnd, AEM_GETINDEX, AEGI_LASTSELCHAR,  LPARAM(@crInit.ciMax));

  // Check if there's selection and wrapping present
  Selection := (AEC_IndexCompare(crInit.ciMin, crInit.ciMax) <> 0);
  Wrap := SendMessage(AkelData.hEditWnd, AEM_GETWORDWRAP, 0, LPARAM(nil)) <> 0;
  ColumnSel := SendMessage(AkelData.hEditWnd, AEM_GETCOLUMNSEL, 0, 0) <> 0;

  // lines count
  if not Selection then
  begin
    SendMessage(AkelData.hEditWnd, AEM_GETINDEX, AEGI_FIRSTCHAR, LPARAM(@crInit.ciMin));
    SendMessage(AkelData.hEditWnd, AEM_GETINDEX, AEGI_LASTCHAR, LPARAM(@crInit.ciMax));
  end;
  line1 := crInit.ciMin.nLine;
  line2 := crInit.ciMax.nLine;
  if Wrap then
  begin
    line1 := SendMessage(AkelData.hEditWnd, AEM_GETUNWRAPLINE, WPARAM(line1), 0);
    line2 := SendMessage(AkelData.hEditWnd, AEM_GETUNWRAPLINE, WPARAM(line2), 0);
  end;
  // empty document - special case
  if line2 = 0 then
    CountProgress.Counters.Lines := 0
  else
    CountProgress.Counters.Lines := line2 - line1 + 1;

  // If selection is present and caret is on the 1st char of the next line -
  // excess line appears, remove it from the counter
  if Selection and AEC_IsFirstCharInLine(crInit.ciMax) then
    if CountProgress.Counters.Lines > 0 then
      Dec(CountProgress.Counters.Lines);

  // total chars count
  isChars.ciChar1 := @crInit.ciMax;
  isChars.ciChar2 := @crInit.ciMin;
  isChars.bColumnSel := BOOL(-1);
  isChars.nNewLine := AELB_ASOUTPUT;
  CountProgress.Counters.Chars := SendMessage(AkelData.hEditWnd, AEM_INDEXSUBTRACT, 0, LPARAM(@isChars));

  if not RunCallback then Exit;

  // *** word count ***

  // init data
  CharsProcessed := 0;
  WordCnt := 0; CharCnt := 0; // how much words/chars we've processed during current cycle

  // there's no selection
  if not Selection then
  begin
    ciCount := crInit.ciMin;

    repeat
      // returns number of characters skipped to the next word
      tmp := SendMessage(AkelData.hEditWnd, AEM_GETNEXTBREAK, AEWB_RIGHTWORDEND, LPARAM(@ciCount));
      if tmp = 0 then Break; // EOF - finish the cycle

      Inc(CharCnt, tmp);
      Inc(WordCnt);

      // check whether it is time to run callback (WordsPerCycle limit reached)
      if WordCnt > WordsPerCycle then
        if not ReportWordCount then Exit;
    until False;

    // final report
    if not ReportWordCount then Exit;
  end // if not Selection
  // selection is present
  else
  begin
    crCount.ciMin := crInit.ciMin;
    crCount.ciMax := crInit.ciMax;
    IsFirst := True;

    if ColumnSel then
    begin
      repeat
        // EOF - finish the cycle
        if AEC_IndexCompare(crCount.ciMin, crCount.ciMax) >= 0 then Break;

        ciWordEnd := crCount.ciMin;
        repeat
          // returns number of characters skipped to the next word
          tmp := SendMessage(AkelData.hEditWnd, AEM_GETNEXTBREAK, AEWB_RIGHTWORDEND, LPARAM(@ciWordEnd));
          if tmp = 0 then Break; // EOF - finish the cycle

          // word ends beyond the selection - finish the *inner* cycle
          if not ((ciWordEnd.nLine = crCount.ciMin.nLine) and
                  (ciWordEnd.nCharInLine <= crCount.ciMin.lpLine.nSelEnd)) then Break;

          if IsFirst then
          begin
            IsFirst := False;
            ciWordStart := ciWordEnd;
            if SendMessage(AkelData.hEditWnd, AEM_GETPREVBREAK, AEWB_LEFTWORDSTART, LPARAM(@ciWordStart)) <> 0 then
              if AEC_IndexCompare(crCount.ciMin, ciWordStart) <= 0 then
                Inc(WordCnt);
          end
          else
            Inc(WordCnt);

          Inc(CharCnt, tmp);

          // word ends on the end of selection - finish the *inner* cycle (?)
          if ciWordEnd.nCharInLine = crCount.ciMin.lpLine.nSelEnd then
            Break;

          // check whether it is time to run callback (WordsPerCycle limit reached)
          if WordCnt > WordsPerCycle then
            if not ReportWordCount then Exit;
        until False;

        //Next line
        IsFirst := True;
        if AEC_NextLine(crCount.ciMin) <> nil then
          crCount.ciMin.nCharInLine := crCount.ciMin.lpLine.nSelStart;

        // check whether it is time to run callback (WordsPerCycle limit reached)
        if WordCnt > WordsPerCycle then
          if not ReportWordCount then Exit;
      until False;

      // final report
      if not ReportWordCount then Exit;
    end // if ColumnSel
    else
    begin
      repeat
        ciWordEnd := crCount.ciMin;
        // returns number of characters skipped to the next word
        tmp := SendMessage(AkelData.hEditWnd, AEM_GETNEXTBREAK, AEWB_RIGHTWORDEND, LPARAM(@ciWordEnd));
        if (tmp = 0) or (AEC_IndexCompare(ciWordEnd, crCount.ciMax) > 0) then Break; // EOF - finish the cycle

        if IsFirst then
        begin
          IsFirst := False;
          ciWordStart := ciWordEnd;
          if SendMessage(AkelData.hEditWnd, AEM_GETPREVBREAK, AEWB_LEFTWORDSTART, LPARAM(@ciWordStart)) <> 0 then
            if AEC_IndexCompare(crCount.ciMin, ciWordStart) <= 0 then
              Inc(WordCnt);
        end
        else
          Inc(WordCnt);

        if AEC_IndexCompare(ciWordEnd, crCount.ciMax) = 0 then Break;  // EOF - finish the cycle

        //Next word
        crCount.ciMin := ciWordEnd;

        // check whether it is time to run callback (WordsPerCycle limit reached)
        if WordCnt > WordsPerCycle then
          if not ReportWordCount then Exit;
      until False;

      // final report
      if not ReportWordCount then Exit;
    end;
  end; // if Selection

  // *** char count ***

  // init data
  ciCount := crInit.ciMin;
  CharsProcessed := 0;
  CharCnt := 0; // how much words/chars we've processed during current cycle

  repeat
    if AEC_IndexCompare(ciCount, crInit.ciMax) >= 0 then Break; // EOF - finish the cycle

    if Selection then
      if ciCount.nCharInLine < ciCount.lpLine.nSelStart then
        ciCount.nCharInLine := ciCount.lpLine.nSelStart;

    if not Selection
      then IsLast := not  (ciCount.nCharInLine < ciCount.lpLine.nLineLen)
      else IsLast := not ((ciCount.nCharInLine < ciCount.lpLine.nLineLen) and
                          (ciCount.nCharInLine < ciCount.lpLine.nSelEnd));

    if not IsLast then
    begin
      if AEC_IndexLen(ciCount) = 1 then  // wide char
      begin
        CurrChar := ciCount.lpLine.wpLine[ciCount.nCharInLine];
        if TCharacter.IsWhiteSpace(CurrChar) then
          Inc(CountProgress.Counters.WhiteSpaces)
        else
        if CharInSet(CurrChar, ['A'..'Z', 'a'..'z']) then
          Inc(CountProgress.Counters.Latin)
        else
        if TCharacter.IsLetter(CurrChar) then
          Inc(CountProgress.Counters.Letters);
        {}//...
      end
      else                               // surrogate pair
      begin
        Inc(CountProgress.Counters.Surrogates);
        {}//... check for letters ...
      end;
    end
    else
    begin
      {}{
      if (ciCount.lpLine->nLineBreak == AELB_R || ciCount.lpLine->nLineBreak == AELB_N)
        ++nCharLatinOther;
      else if (ciCount.lpLine->nLineBreak == AELB_RN)
        nCharLatinOther+=2;
      else if (ciCount.lpLine->nLineBreak == AELB_RRN)
        nCharLatinOther+=3;
      }
      AEC_NextLine(ciCount);
    end;

    Inc(CharCnt);
    AEC_NextChar(ciCount);

    // check whether it is time to run callback (WordsPerCycle limit reached)
    if CharCnt > CharsPerCycle then
      if not ReportCharCount then Exit;
  until False;

  Inc(CountProgress.Counters.Letters, CountProgress.Counters.Latin);
  // all the counts are finished
  CountProgress.Counters.State := cntCompleted;
  ReportCharCount;
end;

type
  TMsgBoxIcon = (iNone, iStop, iQstn, iWarn, iInfo);

// Show message box with standard caption, given text, icon and OK button
procedure MsgBox(const Txt: string; Icon: TMsgBoxIcon = iNone);
const
  MsgBoxIconFlags: array[TMsgBoxIcon] of UINT =
    (0, MB_ICONHAND, MB_ICONQUESTION, MB_ICONEXCLAMATION, MB_ICONASTERISK);
var
  hwndParent: HWND;
begin
  // determine parent window
  if (MainDlg <> nil) and (MainDlg.DlgHwnd <> 0)
    then hwndParent := MainDlg.DlgHwnd
    else hwndParent := AkelData.hMainWnd;
  // show the box, don't care about the result
  MessageBox(hwndParent, PChar(Txt), PChar(string(PluginName) + ' plugin'), MB_OK + MsgBoxIconFlags[Icon]);
end;

{$ENDREGION}

{$REGION 'PLUGIN FUNCTIONS'}

// open the file in explorer
procedure Browse;
var si: TStartupInfo;
    pi: TProcessInformation;
    Cmd: string;
begin
  if FileStats.FileName = '' then
  begin
    MsgBox(LangString(idPgFileLabelWarnMsgNotAFile), iStop);
    Exit;
  end;
  ZeroMem(pi, SizeOf(pi));
  ZeroMem(si, SizeOf(si));
  si.cb := SizeOf(si);
  Cmd := Format(BrowseCmd, [FileStats.FileName]);
  CreateProcess(nil, PWideChar(Cmd), nil, nil, False, 0, nil, nil, si, pi);
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);
end;

// copy full file path to clipboard
procedure CopyPath;
begin
  if FileStats.FileName = '' then
  begin
    MsgBox(LangString(idPgFileLabelWarnMsgNotAFile), iStop);
    Exit;
  end;
  CopyTextToCB(FileStats.FileName, AkelData.hMainWnd);
end;

type
  // Class with static method just to not mess with some special object.
  // Class (static) methods could work as "procedure of object"
  THandler = class
    class procedure InputBoxDialogProc(Sender: TResDialog; msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT);
  end;

class procedure THandler.InputBoxDialogProc(Sender: TResDialog; msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT);
var
  Fn: string;
  pPoint, pFn: PChar;
  PointPos: Integer;
  hEdit: HWND;
begin
  case msg of
    // dialog created and is going to be shown
    // Select only name of the file (no extension) in edit control
    RDM_DLGOPENING:
      begin
        Fn := Sender.ItemText[IDC_EDT_NEWNAME];
        pFn := PChar(Fn);
        pPoint := StrRScan(pFn, '.');
        if pPoint = nil
          then PointPos := Length(Fn)
          else PointPos := pPoint - pFn;
        hEdit := Sender.Item[IDC_EDT_NEWNAME];
        SetFocus(hEdit);
        SendMessage(hEdit, EM_SETSEL, 0, PointPos);
        Res := LRESULT(False); // ! return False in response to WM_INITDIALOG prevents system
                               // from default focusing (there's no other way to control selection
                               // in the edit)
      end;
  end; // case
end;

// rename the current file
//   NewName - name to rename to (without path). If empty, an InputBox will be shown
function Rename(NewName: string): Boolean;
var
  NewFullName: string;
  InputBox: TResDialog;
  hwndParent: HWND;
  Point64: TPOINT64;
  Sel: TAESELECTION;
  Caret: TAECHARINDEX;
  nCodePage: Integer;
  bBOM: BOOL;
  ei: TEDITINFO;
  od: TOPENDOCUMENT;
begin
  Result := False;

  if FileStats.FileName = '' then
  begin
    MsgBox(LangString(idPgFileLabelWarnMsgNotAFile), iStop);
    Exit;
  end;

  // ask a user for a new file name
  if NewName = '' then
  begin
    if (MainDlg <> nil) and (MainDlg.DlgHwnd <> 0)
      then hwndParent := MainDlg.DlgHwnd
      else hwndParent := AkelData.hEditWnd;

    InputBox := TResDialog.Create(IDD_DLG_INPUTBOX, hwndParent);
    InputBox.Caption := LangString(idInputBoxCaption);
    InputBox.SetItemData([ItemData(IDC_STC_INPUT_LABEL,  LangString(idInputBoxLabel)),
                          ItemData(IDC_EDT_NEWNAME,      ExtractFileName(FileStats.FileName)),
                          ItemData(IDC_BTN_INPUT_OK,     LangString(idInputBoxBtnOK)),
                          ItemData(IDC_BTN_INPUT_CANCEL, LangString(idInputBoxBtnCancel))]);
    InputBox.OnDialogProc := THandler.InputBoxDialogProc;
    if InputBox.ShowModal = IDOK then
      NewName := InputBox.ItemText[IDC_EDT_NEWNAME];
    FreeAndNil(InputBox);
    if NewName = '' then Exit;
  end;

  // check the new file name
  NewFullName := ExtractFilePath(FileStats.FileName) + ExtractFileName(NewName); // strip the path from NewName
  if NewFullName = FileStats.FileName then Exit; // dest = source
  if FileExists(NewFullName) then
  begin
    MsgBox(Format(LangString(idMsgFileExists), [NewFullName]), iStop);
    Exit;
  end;

  // Save document state, close it, rename, reopen and reset the saved state.
  // Based on the RenameFile.js script by Instructor

  // Get document state
  SendMessage(AkelData.hEditWnd, AEM_GETSCROLLPOS, 0, LPARAM(@Point64));
  SendMessage(AkelData.hEditWnd, AEM_GETSEL, WPARAM(@Caret), LPARAM(@Sel));
  if SendMessage(AkelData.hMainWnd, AKD_GETEDITINFO, 0, LPARAM(@ei)) <> 0 then
  begin
    nCodePage := ei.nCodePage;
    bBOM := ei.bBOM;
  end
  else
  begin
    nCodePage := 0;
    bBOM := False;
  end;

  // Close
  if not LongBool(SendMessage(AkelData.hMainWnd, WM_COMMAND, IDM_WINDOW_FILECLOSE, 0)) then Exit;

  // Rename
  if not MoveFile(PChar(FileStats.FileName), PChar(NewFullName)) then
  begin
    MsgBox(LastErrMsg, iStop);
    Exit;
  end;

  // Re-open
  ZeroMem(od, SizeOf(od));
  od.pFile := PChar(NewFullName);
  od.nCodePage := nCodePage;
  od.bBOM := bBOM;
  SendMessage(AkelData.hMainWnd, AKD_OPENDOCUMENT, 0, LPARAM(@od));

  // Restore document position and selection
  Sel.dwFlags := Sel.dwFlags or AESELT_LOCKSCROLL or AESELT_INDEXUPDATE;
  SendMessage(AkelData.hEditWnd, AEM_SETSEL, WPARAM(@Caret), LPARAM(@Sel));
  SendMessage(AkelData.hEditWnd, AEM_SETSCROLLPOS, 0, LPARAM(@Point64));

  Result := True;
end;

// We have to use callback to have FileStats updated.
// We don't care about progress so perform only final assignment
procedure CountCallback(var AkelData: TAkelData; var CountProgress: TCountProgress; var Continue: Boolean);
begin
  if CountProgress.Counters.State <> cntInProcess then
    FileStats.Counters := CountProgress.Counters;
end;

procedure GetReport;
var
  FileProps, DocProps, Total: string;
  CPInfo: TCPInfoEx;
begin
  if FileStats.Counters.State <> cntCompleted then
  begin
    MsgBox(LangString(idMsgCountNotCompleted));
    Exit;
  end;

  if FileStats.FileName <> '' then
    FileProps :=
      Format(ReportPattProp, [LangString(idPgFileLabelPath),     ExtractFilePath(FileStats.FileName)]) +
      Format(ReportPattProp, [LangString(idPgFileLabelSize),     IfTh(FileStats.FileSize <> 0, Format(LangString(idPgFileTextSizePatt), [ThousandsDivide(FileStats.FileSize)]), '')]) +
      Format(ReportPattProp, [LangString(idPgFileLabelCreated),  IfTh(FileStats.Created <> 0,  DateTimeToStr(FileStats.Created), '')]) +
      Format(ReportPattProp, [LangString(idPgFileLabelModified), IfTh(FileStats.Modified <> 0, DateTimeToStr(FileStats.Modified), '')]) +
      IfTh(FileStats.IsModified, '(!) ' + LangString(idPgFileLabelWarnMsgNotSaved))
  else
    FileProps := '(Not a file)';

  DocProps :=
      Format(ReportPattProp, [LangString(idPgDocLabelCodePage), IfTh(GetCPInfoEx(FileStats.CodePage, 0, CPInfo), CPInfo.CodePageName, IntToStr(FileStats.CodePage))]) +
      LangString(idPgDocLabelChars) + NL +
      Format(ReportPattProp, [LangString(idPgDocLabelCharsTotal), IntToStr(FileStats.Counters.Chars)]) +
      Format(ReportPattProp, [LangString(idPgDocLabelCharsNoSp),  IntToStr(FileStats.Counters.Chars - FileStats.Counters.WhiteSpaces)]) +
      Format(ReportPattProp, [LangString(idPgDocLabelLines),      IntToStr(FileStats.Counters.Lines)]) +
      Format(ReportPattProp, [LangString(idPgDocLabelWords),      IntToStr(FileStats.Counters.Words)]) +
      Format(ReportPattProp, [LangString(idPgDocLabelLatin),      IntToStr(FileStats.Counters.Latin)]) +
      Format(ReportPattProp, [LangString(idPgDocLabelLetters),    IntToStr(FileStats.Counters.Letters)]) +
      Format(ReportPattProp, [LangString(idPgDocLabelSurr),       IntToStr(FileStats.Counters.Surrogates)]) +
      '';

  Total := Format(ReportPattAll,
                  [IfTh(FileStats.FileName = '', '(!) ' + LangString(idPgFileLabelWarnMsgNotAFile), ExtractFileName(FileStats.FileName)),
                   FileProps, DocProps]);

  CopyTextToCB(Total);
end;

{$ENDREGION}

{$REGION 'COUNTING THREAD'}

// Broker function allowing to use class method in the API calls
// lParameter = TCountThread object
function ThreadProc(lParameter: Pointer): DWORD; stdcall;
begin
  Result := TCountThread(lParameter).Execute;
end;

// broker function allowing to use class method as the GetDocInfo callback
procedure ThreadCountCallback(var AkelData: TAkelData; var CountProgress: TCountProgress; var Continue: Boolean);
begin
  CountThread.CountCallback(AkelData, CountProgress, Continue);
end;

// counting thread methods

procedure TCountThread.CountCallback(var AkelData: TAkelData; var CountProgress: TCountProgress; var Continue: Boolean);
begin
  // make counters obsolete if the thread is being terminated
  if FState = stTerminated then
    CountProgress.Counters.State := cntNotActual
  // change the thread state if count process is completed
  else
    if CountProgress.Counters.State = cntCompleted then
      FState := stInactive;
  // send current counters and thread state to the main window
  Continue := (FState <> stTerminated);
  SendMessage(FhTargetWnd, MSG_UPD_COUNT, WPARAM(FState), LPARAM(@CountProgress));
end;

// Launch the counting thread
procedure TCountThread.Run(TargetWnd: HWND; const AkelData: TAkelData; const FileStats: TFileStats);
begin
  if FState = stRunning then Exit;
  FAkelData := AkelData;
  FhTargetWnd := TargetWnd;
  FFileName := FileStats.FileName;
  FhThread := CreateThread(nil, 0, @ThreadProc, Self, 0, FidThread);
  SetThreadPriority(FhThread, THREAD_PRIORITY_BELOW_NORMAL);
end;

// Stop the counting thread (do not wait for it, thread could run for some time
// after this procedure is finished!)
procedure TCountThread.Stop;
begin
  if FState <> stRunning then Exit;
  FState := stTerminated;
end;

// Main procedure
function TCountThread.Execute: DWORD;
begin
  FState := stRunning;

  // retrieve shell icon here instead of the main thread because it lasts 0.5..1 sec sometimes
  SendMessage(FhTargetWnd, MSG_ICON_EXTRACTED, WPARAM(ExtractShellIcon(FFileName)), 0);

  // thread cycle
  GetDocInfo(FAkelData, ThreadCountCallback);

  // if exiting normally, return "OK", otherwise return error
  Result := IfTh(FState = stTerminated, DWORD(-1), 0);

  // clear the data
  CloseAndZeroHandle(FhThread);
  FidThread := 0;
  FhTargetWnd := 0;
  FState := stInactive;
  ZeroMem(FAkelData, SizeOf(FAkelData));
end;

{$ENDREGION}

{$REGION 'DIALOGS'}

// TMainDlg

constructor TMainDlg.Create(const pd: TPLUGINDATA);
var pg: TTabPage;
begin
  inherited Create(pd.hInstanceDLL, IDD_DLG_MAIN, pd.hMainWnd);

  FAppIcon := pd.hMainIcon;
  fShowTooltips := True;

  SetItemData([ItemData(IDC_BTN_MAIN_OK, LangString(idMainBtnOK)),
               ItemData(IDC_BTN_REPORT, LangString(idMainBtnReport), LangString(idMainBtnReportTip))]);
  FTabPageCaptions[tabFile] := LangString(idPgFileTitle);
  FTabPageCaptions[tabDoc] := LangString(idPgDocTitle);

  for pg := Low(TTabPage) to High(TTabPage) do
  begin
    FPages[pg] := TPageDlg.Create(pd, Self, pg);
    FPages[pg].Persistent := True;
  end;
end;

destructor TMainDlg.Destroy;
var pg: TTabPage;
begin
  for pg := Low(TTabPage) to High(TTabPage) do
    FreeAndNil(FPages[pg]);
  inherited;
end;

procedure TMainDlg.DoDialogProc(msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT);
var hwndTab: HWND;
    pg: TTabPage;
    tabItem: TTCItem;
    NotifyHdr: TNMHdr;
    pProgr: PCountProgress;
begin
  case msg of
    // dialog created and is going to be shown
    RDM_DLGOPENING:
      begin
        SendMessage(DlgHwnd, WM_SETICON, ICON_SMALL, Windows.LPARAM(FAppIcon));
        Caption := IfTh(FileStats.Selection, LangString(idTitleSelProps), LangString(idTitleFileProps));

        hwndTab := Item[IDC_TAB];
        // init tabs
        for pg := Low(TTabPage) to High(TTabPage) do
        begin
          // add pages to tab control
          ZeroMem(tabItem, SizeOf(tabItem));
          tabItem.mask := TCIF_TEXT;
          tabItem.pszText := PChar(FTabPageCaptions[pg]);
          SendMessage(hwndTab, TCM_INSERTITEM, Integer(pg), Windows.LPARAM(@tabItem));
          // parent window handle changes every time so set the actual one
          FPages[pg].ParentHwnd := hwndTab;
        end;
        // select the probably required page (text stats if selection is present)
        SendMessage(hwndTab, TCM_SETCURSEL, IfTh(FileStats.Selection, Integer(tabDoc), Integer(tabFile)), 0);

        // imitate page change to init the page dialog
        NotifyHdr.hwndFrom := DlgHwnd;
        NotifyHdr.idFrom := IDC_TAB;
        NotifyHdr.code := TCN_SELCHANGE;
        SendMessage(DlgHwnd, WM_NOTIFY, 0, Windows.LPARAM(@NotifyHdr));

        FPages[tabDoc].ItemText[IDC_BTN_STOP] := LangString(idPgDocBtnAbort);
        CountThread.Run(DlgHwnd, AkelData, FileStats); // start counting
      end;

    // dialog is closing - stop the thread
    RDM_DLGCLOSING:
      begin
        CountThread.Stop;
      end;

    // tab page changes
    WM_NOTIFY:
      begin
        NotifyHdr := PNMHdr(lParam)^;
        case NotifyHdr.code of
          // page is about to change, hide the current page
          TCN_SELCHANGING:
            begin
              hwndTab := Item[NotifyHdr.idFrom];
              pg := TTabPage(SendMessage(hwndTab, TCM_GETCURSEL, 0, 0));
              FPages[pg].Show(SW_HIDE);
              Res := LRESULT(False); // must return false to allow page change
            end;
          // page was changed, show the current page
          TCN_SELCHANGE:
            begin
              hwndTab := Item[NotifyHdr.idFrom];
              pg := TTabPage(SendMessage(hwndTab, TCM_GETCURSEL, 0, 0));
              FPages[pg].Show(SW_SHOW);
              FPages[pg].SetValues;
              Res := LRESULT(True);
            end;
          else
        end; // case NotifyHdr
      end;

    // message from counting thread - update progress
    MSG_UPD_COUNT:
      begin
        pProgr := PCountProgress(lParam);
        FileStats.Counters := pProgr.Counters;

        // change values on the doc page only if it is visible
        if not IsWindowVisible(FPages[tabDoc].DlgHwnd) then Exit;
        FPages[tabDoc].SetValues;
        if TThreadState(wParam) = stRunning then
          SendMessage(FPages[tabDoc].Item[IDC_PGB_PROCESS], PBM_SETPOS, pProgr.PercentDone, 0);

        Res := LRESULT(True);
      end;

    MSG_ICON_EXTRACTED:
      begin
        FileStats.hIcon := HICON(wParam);
        if IsWindowVisible(FPages[tabFile].DlgHwnd) then
          FPages[tabFile].SetValues;
      end;

    // notifications from child controls
    WM_COMMAND:
      case HiWord(wParam) of
        BN_CLICKED:
          case LoWord(wParam) of
            // "Report" button
            IDC_BTN_REPORT: GetReport;
          end;
      end;

  end; // case msg
end;

// Custom handler for "Filename" edit control which handles Ctrl-S combination.
// This is the only way of catching keyboard events as for dialog boxes OS hides
// it completely (WM_CHAR won't arrive to DlgProc).
function FileNameEditWndProc(hWnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var ks: TKeyboardState;
begin
  case msg of
    WM_KEYDOWN:
      if Char(wParam) = 'S' then
      begin
        // check modifiers state
        GetKeyboardState(ks);
        if (ks[VK_CONTROL] and $80 <> 0) and
           (ks[VK_MENU] and $80 = 0) and (ks[VK_SHIFT] and $80 = 0) and
           (ks[VK_LWIN] and $80 = 0) and (ks[VK_RWIN] and $80 = 0)
        then
          // if edit is modified, send command to the parent dialog
          if SendMessage(hWnd, EM_GETMODIFY, 0, 0) <> 0 then
          begin
            PostMessage(GetParent(hWnd), MSG_RENAME_FILE, 0, 0);
            Exit(0);
          end;
      end;
  end;
  Result := CallWindowProc(PrevEditWndProc, hWnd, Msg, wParam, lParam);
end;

// TPageDlg

constructor TPageDlg.Create(const pd: TPLUGINDATA; Owner: TMainDlg; Page: TTabPage);
begin
  inherited Create(pd.hInstanceDLL, TabPageIDs[Page], pd.hMainWnd);
  FOwner := Owner;
  FPage := Page;
  fShowTooltips := True;
  case FPage of
    tabFile:
        SetItemData([
              ItemData(IDC_STC_RENAMEHINT, LangString(idPgFileLabelRenameHint)),
              ItemData(IDC_STC_FILEPATH,   LangString(idPgFileLabelPath)),
              ItemData(IDC_STC_FILESIZE,   LangString(idPgFileLabelSize)),
              ItemData(IDC_STC_CREATED,    LangString(idPgFileLabelCreated)),
              ItemData(IDC_STC_MODIFIED,   LangString(idPgFileLabelModified)),
              ItemData(IDC_BTN_COPYPATH,   LangString(idPgFileBtnCopyPath), LangString(idPgFileBtnCopyPathTip)),
              ItemData(IDC_BTN_BROWSE,     LangString(idPgFileBtnBrowse), LangString(idPgFileBtnBrowseTip)),
              // just to have these items allocated
              ItemData(IDC_EDT_FILENAME, '', LangString(idPgFileTextFileNameTip)),
              ItemData(IDC_EDT_FILEPATH, ''),
              ItemData(IDC_EDT_CREATED, ''),
              ItemData(IDC_EDT_MODIFIED, '')
             ]);
    tabDoc:
        SetItemData([
              ItemData(IDC_STC_CODEPAGE,  LangString(idPgDocLabelCodePage)),
              ItemData(IDC_STC_CHARS,     LangString(idPgDocLabelChars)),
              ItemData(IDC_STC_CHARSTOT,  LangString(idPgDocLabelCharsTotal)),
              ItemData(IDC_STC_CHARSNOSP, LangString(idPgDocLabelCharsNoSp)),
              ItemData(IDC_STC_LINES,     LangString(idPgDocLabelLines)),
              ItemData(IDC_STC_WORDS,     LangString(idPgDocLabelWords)),
              ItemData(IDC_STC_LATIN,     LangString(idPgDocLabelLatin)),
              ItemData(IDC_STC_LETTERS,   LangString(idPgDocLabelLetters)),
              ItemData(IDC_STC_SURR,      LangString(idPgDocLabelSurr)),
              ItemData(IDC_BTN_STOP,      LangString(idPgDocBtnCount)),
              // just to have these items allocated
              ItemData(IDC_EDT_CODEPAGE,  ''),
              ItemData(IDC_EDT_CHARSTOT,  ''),
              ItemData(IDC_EDT_CHARSNOSP, ''),
              ItemData(IDC_EDT_LINES,     ''),
              ItemData(IDC_EDT_WORDS,     ''),
              ItemData(IDC_EDT_LATIN,     ''),
              ItemData(IDC_EDT_LETTERS,   ''),
              ItemData(IDC_EDT_SURR,      '')
             ]);
  end;
end;

// set text values form FileStats record
procedure TPageDlg.SetValues;
var ShowWarnMsg: Boolean;
    CPInfo: TCPInfoEx;
    hdc: Windows.HDC;
    Size: TSize;
    Rect: TRect;
    Path: string;
const // determined from user32.dll's resources
  ID_APPLICATION = 100;
  ID_WARNING     = 101;
  ID_QUESTION    = 102;
  ID_ERROR       = 103;
  ID_INFORMATION = 104;
  ID_WINLOGO     = 105;
  ID_SHIELD      = 106;
begin
  case FPage of
    tabFile:
      begin
        if not FValuesWereSet then // some values might change (during count process) and some might not
        begin
          Path := ExtractFilePath(FileStats.FileName);
          ItemText[IDC_EDT_FILENAME] := ExtractFileName(FileStats.FileName);
          ItemText[IDC_EDT_FILEPATH] := Path;
          ItemText[IDC_EDT_FILESIZE] := IfTh(FileStats.FileSize <> 0, Format(LangString(idPgFileTextSizePatt), [ThousandsDivide(FileStats.FileSize)]), '');
          ItemText[IDC_EDT_CREATED]  := IfTh(FileStats.Created <> 0,  DateTimeToStr(FileStats.Created), '');
          ItemText[IDC_EDT_MODIFIED] := IfTh(FileStats.Modified <> 0, DateTimeToStr(FileStats.Modified), '');
          SendMessage(Item[IDC_EDT_FILENAME], EM_SETMODIFY, WPARAM(False), 0);
          SendMessage(Item[IDC_EDT_FILENAME], EM_SETREADONLY, WPARAM(FileStats.FileName = ''), 0);
          // check if path is longer than "FilePath" edit width and set tooltip if yes
          hdc := GetDC(Item[IDC_EDT_FILEPATH]);
          GetTextExtentPoint(hdc, PChar(Path), Length(Path), Size);
          ReleaseDC(Item[IDC_EDT_FILEPATH], hdc);
          GetWindowRect(Item[IDC_EDT_FILEPATH], Rect);
          if Size.cx >= Rect.Right - Rect.Left then
            ItemTooltip[IDC_EDT_FILEPATH] := Path;
          // the content is not saved to file
          if FileStats.FileName = '' then
          begin
            if hiErr = 0 then
              hiErr := LoadImage(GetModuleHandle(user32), MakeIntResource(ID_ERROR),
                                 IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
            SendMessage(Item[IDC_IMG_WARNMSG], STM_SETICON, WPARAM(hiErr), 0);
            ItemText[IDC_STC_WARNMSG] := LangString(idPgFileLabelWarnMsgNotAFile);
            ShowWarnMsg := True;
            EnableWindow(Item[IDC_BTN_COPYPATH], False);
            EnableWindow(Item[IDC_BTN_BROWSE], False);
          end
          // there are some changes unsaved
          else if FileStats.IsModified then
          begin
            if hiWarn = 0 then
              hiWarn := LoadImage(GetModuleHandle(user32), MakeIntResource(ID_WARNING),
                                 IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
            SendMessage(Item[IDC_IMG_WARNMSG], STM_SETICON, WPARAM(hiWarn), 0);
            ItemText[IDC_STC_WARNMSG] := LangString(idPgFileLabelWarnMsgNotSaved);
            ShowWarnMsg := True;
          end
          else
            ShowWarnMsg := False;
          ItemVisible[IDC_IMG_WARNMSG] := ShowWarnMsg;
          ItemVisible[IDC_STC_WARNMSG] := ShowWarnMsg;
          FValuesWereSet := True;
        end;

        // update these values always
        SendMessage(Item[IDC_IMG_FILEICON], STM_SETICON, WPARAM(FileStats.hIcon), 0);
      end;

    tabDoc:
      begin
        if not FValuesWereSet then // some values might change (during count process) and some might not
        begin
          ItemText[IDC_EDT_CODEPAGE] :=
            IfTh(GetCPInfoEx(FileStats.CodePage, 0, CPInfo), CPInfo.CodePageName, IntToStr(FileStats.CodePage));
          FValuesWereSet := True;
        end;
        // update these values always
        ItemText[IDC_EDT_CHARSTOT]  := IfTh(FileStats.Counters.State <> cntNotActual, ThousandsDivide(FileStats.Counters.Chars));
        ItemText[IDC_EDT_CHARSNOSP] := IfTh(FileStats.Counters.State =  cntCompleted, ThousandsDivide(FileStats.Counters.Chars - FileStats.Counters.WhiteSpaces));
        ItemText[IDC_EDT_LINES]     := IfTh(FileStats.Counters.State <> cntNotActual, ThousandsDivide(FileStats.Counters.Lines));
        ItemText[IDC_EDT_WORDS]     := IfTh(FileStats.Counters.State <> cntNotActual, ThousandsDivide(FileStats.Counters.Words));
        ItemText[IDC_EDT_LETTERS]   := IfTh(FileStats.Counters.State <> cntNotActual, ThousandsDivide(FileStats.Counters.Letters));
        ItemText[IDC_EDT_LATIN]     := IfTh(FileStats.Counters.State <> cntNotActual, ThousandsDivide(FileStats.Counters.Latin));
        ItemText[IDC_EDT_SURR]      := IfTh(FileStats.Counters.State <> cntNotActual, ThousandsDivide(FileStats.Counters.Surrogates));

        // Show progress bar only when counting is running
        ItemVisible[IDC_PGB_PROCESS] := (FileStats.Counters.State = cntInProcess);
        ItemText[IDC_BTN_STOP] := IfTh(FileStats.Counters.State = cntInProcess, LangString(idPgDocBtnAbort), LangString(idPgDocBtnCount));
        // Do not show "Start/stop" button if counting has been successfully completed
        ItemVisible[IDC_BTN_STOP] := (FileStats.Counters.State <> cntCompleted);
      end;
  end;

end;

procedure TPageDlg.DoDialogProc(msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT);
var
  hwndTab: HWND;
  TabClientArea: TRect;
begin
  case msg of
    // dialog is showing - set position inside tab page
    RDM_DLGOPENING:
      begin
        // calculate tab's client area
        hwndTab := ParentHwnd;
        GetClientRect(hwndTab, TabClientArea);
        SendMessage(hwndTab, TCM_ADJUSTRECT, Windows.WPARAM(False), Windows.LPARAM(@TabClientArea));
        SetWindowPos(DlgHwnd, 0, TabClientArea.Left, TabClientArea.Top,
                     TabClientArea.Right - TabClientArea.Left, TabClientArea.Bottom - TabClientArea.Top,
                     SWP_NOZORDER);
        // subclass edit control to catch keyboard events
        if FPage = tabFile then
          PrevEditWndProc := TFNWndProc(SetWindowLongPtr(Item[IDC_EDT_FILENAME], GWLP_WNDPROC,
                                                         LONG_PTR(@FileNameEditWndProc)));
      end;

    RDM_DLGCLOSED:
      ;

    // notification from control
    WM_COMMAND:
      case HiWord(wParam) of

        BN_CLICKED: // Button
          case LoWord(wParam) of
            // (Doc page) "Start/stop count" button. Run/stop the thread
            IDC_BTN_STOP:
              if CountThread.State = stRunning then
              begin
                CountThread.Stop;
                ItemVisible[IDC_PGB_PROCESS] := False;
                ItemText[IDC_BTN_STOP] := LangString(idPgDocBtnCount);
                Res := LRESULT(True);
              end
              else
              begin
                FileStats.Counters.State := cntNotActual;
                SetValues;
                CountThread.Run(FOwner.DlgHwnd, AkelData, FileStats);
                SendMessage(Item[IDC_PGB_PROCESS], PBM_SETPOS, 0, 0);
                ItemVisible[IDC_PGB_PROCESS] := True;
                ItemText[IDC_BTN_STOP] := LangString(idPgDocBtnAbort);
                Res := LRESULT(True);
              end;
            // (File page) "Copy path" button
            IDC_BTN_COPYPATH:
              CopyPath;
            // (File page) "Browse" button
            IDC_BTN_BROWSE:
              Browse;
          end; // case LoWord

        EN_CHANGE: // Edit
          case LoWord(wParam) of
            // (File page) "Filename" editor has changed. Show rename hint. Additionally check
            // "modified" flag as this notification is sent even when nothing changes
            IDC_EDT_FILENAME:
              ItemVisible[IDC_STC_RENAMEHINT] := (SendMessage(Item[IDC_EDT_FILENAME], EM_GETMODIFY, 0, 0) <> 0);
          end; // case LoWord

      end; // case HiWord

    // (File page) command from "Filename" edit control: user requested file renaming.
    MSG_RENAME_FILE:
      begin
        if not Rename(ItemText[IDC_EDT_FILENAME]) then Exit;
        // refresh file props
        GetFileInfo(AkelData, FileStats);
        FileStats.hIcon := ExtractShellIcon(FileStats.FileName);
        FValuesWereSet := False;
        SetValues;
      end;

  end; // case msg
end;

{$ENDREGION}

{$REGION 'MAIN PLUGIN FUNCTIONS'}

// initialize stuff with given PLUGINDATA members
procedure Init(var pd: TPLUGINDATA);
// Parameters for current call are kept in the following form:
//   pd.lParam - pointer to array of INT_PTR (let's call it Params)
//   Params[0] = size of the whole array in bytes including the 0-th element itself.
//               So Length(Params) = Params[0] div SizeOf(INT_PTR), ParamCount = Length(Params) - 1
//   Params[1] = pointer to parameter string OR the value of numerical parameter
//   ...
type
  TAkelParams = array[0..$FFFF] of INT_PTR;
  PAkelParams = ^TAkelParams;
var
  AkelParams: PAkelParams;
  sCmd: string;
  TmpCmd: TPluginCommand;
begin
  ZeroMem(AkelData, SizeOf(AkelData));
  AkelData.hMainWnd := pd.hMainWnd;
  AkelData.hEditWnd := pd.hWndEdit;
  SetCurrLang(PRIMARYLANGID(pd.wLangModule));

  // check for parameter
  AkelParams := PAkelParams(pd.lParam);
  if (AkelParams <> nil) and (AkelParams^[0] div SizeOf(INT_PTR) - 1 = 1) then
  begin
    sCmd := string(PChar(AkelParams^[1])); // returns empty string if parameter is NULL
    // determine command
    for TmpCmd := Low(TPluginCommand) to High(TPluginCommand) do
      if sCmd = PluginCommands[TmpCmd] then
      begin
        PluginCommand := TmpCmd;
        Break;
      end;
  end;

  // create dialog and thread only when no command specified
  if PluginCommand = TPluginCommand(-1) then
  begin
    MainDlg := TMainDlg.Create(pd);
    MainDlg.Persistent := True;
    CountThread := TCountThread.Create;
  end;

  //...
end;

// do main work here
procedure Execute(var pd: TPLUGINDATA);
begin
  if not GetFileInfo(AkelData, FileStats) then
  begin
    MsgBox(LangString(idMsgGetPropsFail), iStop);
    Exit;
  end;

  // execute command
  case PluginCommand of
    cmdBrowse:
      Browse;
    cmdCopyPath:
      CopyPath;
    cmdRename:
      Rename('');
    cmdGetReport:
      begin
        GetDocInfo(AkelData, CountCallback);
        GetReport;
      end;
    // no command specified - show GUI and launch thread
    else
      if MainDlg.ShowModal = -1 then
        MsgBox(LangString(idMsgShowDlgFail) + NL + LastErrMsg, iStop);
  end; // case command
end;

// cleanup
procedure Finish;
begin
  // ! If dialog window is closed before counting thread finishes its work, the
  // message loop of the host application regains control. And it waits for the
  // plugin's Main function to complete thus not processing any messages. So
  // SendMessage calls from GetDocInfo to the edit window cause deadlock.
  // By executing message processing manually here we launch main message loop
  // again and again until the thread is finished.
  if CountThread <> nil then
    while CountThread.State <> stInactive do
      MainDlg.ProcessMessages;

  FreeAndNil(CountThread);
  DestroyIcon(FileStats.hIcon);
  FreeAndNil(MainDlg);
  DestroyIcon(hiWarn);
  DestroyIcon(hiErr);
end;

// Identification
procedure DllAkelPadID(var pv: TPLUGINVERSION); cdecl;
begin
  pv.dwAkelDllVersion := AkelDLL;
  pv.dwExeMinVersion3x := MakeIdentifier(-1, -1, -1, -1);
  pv.dwExeMinVersion4x := MakeIdentifier(4, 7, 0, 0);
  pv.pPluginName := PAnsiChar(PluginName);
end;

// Plugin extern function
procedure Main(var pd: TPLUGINDATA); cdecl;
begin
  // Function doesn't support autoload
  pd.dwSupport := pd.dwSupport or PDS_NOAUTOLOAD;
  if (pd.dwSupport and PDS_GETSUPPORT) <> 0 then
    Exit;

  // Init stuff
  Init(pd);

  // Do main job here
  Execute(pd);

  // Cleanup
  Finish;
end;

// Entry point
procedure CustomDllProc(dwReason: DWORD);
begin
  case dwReason of
    DLL_PROCESS_ATTACH: ;
    DLL_PROCESS_DETACH: ;
    DLL_THREAD_ATTACH:  ;
    DLL_THREAD_DETACH:  ;
  end;
end;

{$ENDREGION}

exports
  DllAkelPadID,
  Main;

begin
  DllProc := @CustomDllProc;
  CustomDllProc(DLL_PROCESS_ATTACH);
  IsMultiThread := True; // ! we create thread not with TThread so we have to set this flag
                         // manually to avoid troubles with memory manager
end.
