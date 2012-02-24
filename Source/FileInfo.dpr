        (*****************************************
            FileInfo plugin for AkelPad editor
                         © Fr0sT

  Shows properties of a currently edited file as long as some
  statistics for its contents .
  Something similar to Stats plugin but provides more info.

         *****************************************)

{
 TO DO:
   * parameters
   * buttons for Browse, Copy path, Open in assoc program, show explorer menu
   * Default icon when couldn't get - ?
   * full report as text
   * codepage description
}

library FileInfo;

{$R *.RES}
{$R 'Dialog.res' 'Dialog.rc'}
{$R 'Icon.res'}

uses
  Windows, Messages, SysUtils, Character, CommCtrl, ShellApi,
  IceUtils, ResDialog,
  AkelDLL_h in '#AkelDefs\AkelDLL_h.pas',
  AkelEdit_h in '#AkelDefs\AkelEdit_h.pas';

// Global constants

const
  PluginName: AnsiString = 'FileInfo';

type
  // statistics that are counted in the separate thread
  TDocCountStats = record
    Actual: Boolean;           // Whether the values have their meaning (True if the count
                               // process has finished normally or is running, False if
                               // it was interrupted)
    Lines,                     // Without word wrap
    Chars,                     // Total chars
    Words,                     // words according to Akel settings
    WhiteSpaces,               // Spaces, tabs, etc
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

  TCountData = record
    hMainWnd, hEditWnd: HWND;  // main window and edit window
    Complete: Boolean;         // True if count process is finished
  end;
  PCountData = ^TCountData;

  // current counting state
  TCountProgress = record
    PercentDone: Cardinal;
    Counters: TDocCountStats;
  end;
  PCountProgress = ^TCountProgress;

  TCountCallback = procedure(var CountData: TCountData; var CountProgress: TCountProgress; var Continue: Boolean);

  TThreadState = (stInactive, stRunning, stTerminated);

  // counter thread
  TCountThread = class
  strict private
    FhThread: THandle;
    FidThread: DWORD;
    FhTargetWnd: HWND;
    FCountData: TCountData;
    FState: TThreadState;
    FFileName: string;
  private
    procedure CountCallback(var CountData: TCountData; var CountProgress: TCountProgress; var Continue: Boolean);
    function Execute: DWORD;
  public
    procedure Run(TargetWnd: HWND; const CountData: TCountData; const FileStats: TFileStats);
    procedure Stop;

    property State: TThreadState read FState;
  end;

const
  WordsPerCycle = 1000;  {}
  CharsPerCycle = 5000;  {}

  // Count thread -> main dialog window
  //   Update counters.
  //   wParam: TThreadState
  //   lParam: pointer to TCountProgress record. CountProgress.PercentDone = 100
  //           means the thread has finished normally
  MSG_UPD_COUNT = WM_APP + $FF1;
  // Count thread -> main dialog window
  //   Shell icon of the file is extracted, show it
  //   wParam: HICON
  //   lParam: 0
  MSG_ICON_EXTRACTED = WM_APP + $FF2;

{$REGION 'Localization'}

// String resources
type
  TStringID = 
  (
    // dialog window labels and captions
    idTitleFileProps,
    idTitleSelProps,
    idPgFileTitle,
    idPgDocTitle,
    idPgFileLabelPath,
    idPgFileLabelSize,
    idPgFileLabelCreated,
    idPgFileLabelModified,
    idPgFileTextSizePatt,
    idPgFileLabelWarnMsgNotSaved,
    idPgFileLabelWarnMsgNotAFile,
    idPgDocLabelCodePage,
    idPgDocLabelLines,
    idPgDocLabelChars,
    idPgDocLabelWords,
    idPgDocLabelCharsNoSp,
    idPgDocLabelLatin,
    idPgDocLabelLetters,
    idPgDocLabelSurr,
    idPgDocBtnCount,
    idPgDocBtnAbort,
    idMainBtnOK,
    //...
    // messages
    idMsgGetPropsFail,
    idMsgShowDlgFail,
    // other
    idBla
  );
  TLangStrings = array[TStringID] of string;
  TLangData = record
    LangId: LANGID;
    Strings: TLangStrings;
  end;
const
  LangData: array[1..2] of TLangData =
  (
    // ru
    (
      LangId: LANG_RUSSIAN;
      Strings: (
        'Свойства файла',
        'Свойства выделенного текста',
        'Файл',
        'Текст',
        'Путь',
        'Размер',
        'Создан',
        'Изменён',
        '%s байт',
        'Есть несохранённые изменения',
        'Документ не сохранён в файл',
        'Кодировка',
        'Строки',
        'Символы',
        'Слова',
        'Символы без пробелов',
        'Латинские буквы',
        'Буквы',
        'Суррогатные пары',
        'Подсчитать',
        'Прервать',
        'ОК',
        'Ошибка при получении свойств',
        'Ошибка при показе диалога',
        'Блабла'
      );
    ),
    // en
    (
      LangId: LANG_ENGLISH;
      Strings: (
        'File statistics',
        'Selection statistics',
        'File',
        'Text',
        'Path',
        'Size',
        'Created',
        'Modified',
        '%s byte(s)',
        'There are some changes unsaved',
        'Document is not saved to a file',
        'Codepage',
        'Lines',
        'Chars',
        'Words',
        'Chars without spaces',
        'Latin letters',
        'Letters',
        'Surrogate pairs',
        'Count',
        'Abort',
        'OK',
        'Error retrieving statistics',
        'Error showing the dialog',
        'Blabla'
      );
    )
  );
var
  CurrLangId: LANGID = LANG_NEUTRAL;

// Returns a string with given ID corresponding to current langID
function LangString(StrId: TStringID): string;
var i: Integer;
begin
  for i := Low(LangData) to High(LangData) do
    if LangData[i].LangId = CurrLangId then
      Exit(LangData[i].Strings[StrId]);
  // lang ID not found - switch to English and re-run
  CurrLangId := LANG_ENGLISH;
  Result := LangString(StrId);
end;

{$ENDREGION}

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
  CountData: TCountData;  {}
  hiWarn, hiErr: HICON;

// ***** SERVICE FUNCTIONS ***** \\

// Retrieve file properties.
function GetFileInfo(var CountData: TCountData; var FileStats: TFileStats): Boolean;
var ei: TEDITINFO;
    crInit: TAECHARRANGE;
begin
  DestroyIcon(FileStats.hIcon);
  Finalize(FileStats);
  ZeroMem(FileStats, SizeOf(FileStats));

  ZeroMem(ei, SizeOf(ei));
  if (CountData.hMainWnd = 0) or
     (CountData.hEditWnd = 0) or
     (SendMessage(CountData.hMainWnd, AKD_GETEDITINFO, 0, LPARAM(@ei)) = 0) then
       Exit(False);

  if ei.wszFile <> nil then
    FileStats.FileName := string(ei.wszFile)
  else if ei.szFile <> nil then
    FileStats.FileName := string(AnsiString(ei.szFile));
  if FileStats.FileName <> '' then  {}
  begin
    FileStats.FileSize := GetFileSize(FileStats.FileName);
    GetFileTime(FileStats.FileName, tsLoc, @FileStats.Created, nil, @FileStats.Modified);
  end;

  FileStats.IsModified := ei.bModified;
  FileStats.CodePage := ei.nCodePage;
  // Check if there's selection present
  SendMessage(CountData.hEditWnd, AEM_GETINDEX, AEGI_FIRSTSELCHAR, LPARAM(@crInit.ciMin));
  SendMessage(CountData.hEditWnd, AEM_GETINDEX, AEGI_LASTSELCHAR,  LPARAM(@crInit.ciMax));
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
procedure GetDocInfo(var CountData: TCountData; CountCallback: TCountCallback);
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

// Launch the given callback function (if any).
// Returns False if the process must be interrupted.
// Uses external variables: CountCallback, CountData, CountProgress
// Based on the Stats plugin code by Instructor
function RunCallback: Boolean;
begin
  Result := True;
  if not Assigned(CountCallback) then Exit;
  CountCallback(CountData, CountProgress, Result);
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
  // still in progress - get current percent value
  if CountProgress.PercentDone < 100 then
  begin
    CountProgress.PercentDone := Trunc(CharsProcessed*100/CountProgress.Counters.Chars);
    if CountProgress.PercentDone > 99 then // we'll reach 100% only when AEM_GETNEXTBREAK return 0
      CountProgress.PercentDone := 99;
  end;
  Result := RunCallback;
end;

// Recalculate current percent and launch callback
// Uses external variables: CountData, CountProgress, CharsProcessed, WordCnt, CharCnt
function ReportCharCount: Boolean;
begin
  // update the counters
  Inc(CharsProcessed, CharCnt);
  CharCnt := 0;
  // still in progress - get current percent value
  if CountProgress.PercentDone < 100 then
  begin
    CountProgress.PercentDone := Trunc(CharsProcessed*100/CountProgress.Counters.Chars);
    if CountProgress.PercentDone > 99 then // we'll reach 100% only when AEM_GETNEXTBREAK return 0
      CountProgress.PercentDone := 99;
  end;
  Result := RunCallback;
end;

begin
  ZeroMem(CountProgress, SizeOf(CountProgress));
  CountProgress.Counters.Actual := True;
  CountData.Complete := False;

  // *** get basic document info ***

  SendMessage(CountData.hEditWnd, AEM_GETINDEX, AEGI_FIRSTSELCHAR, LPARAM(@crInit.ciMin));
  SendMessage(CountData.hEditWnd, AEM_GETINDEX, AEGI_LASTSELCHAR,  LPARAM(@crInit.ciMax));

  // Check if there's selection and wrapping present
  Selection := (AEC_IndexCompare(crInit.ciMin, crInit.ciMax) <> 0);
  Wrap := SendMessage(CountData.hEditWnd, AEM_GETWORDWRAP, 0, LPARAM(nil)) <> 0;
  ColumnSel := SendMessage(CountData.hEditWnd, AEM_GETCOLUMNSEL, 0, 0) <> 0;

  // lines count
  if not Selection then
  begin
    SendMessage(CountData.hEditWnd, AEM_GETINDEX, AEGI_FIRSTCHAR, LPARAM(@crInit.ciMin));
    SendMessage(CountData.hEditWnd, AEM_GETINDEX, AEGI_LASTCHAR, LPARAM(@crInit.ciMax));
  end;
  line1 := crInit.ciMin.nLine;
  line2 := crInit.ciMax.nLine;
  if Wrap then
  begin
    line1 := SendMessage(CountData.hEditWnd, AEM_GETUNWRAPLINE, WPARAM(line1), 0);
    line2 := SendMessage(CountData.hEditWnd, AEM_GETUNWRAPLINE, WPARAM(line2), 0);
  end;
  // empty document - special case
  if line2 = 0 then
    CountProgress.Counters.Lines := 0
  else
    CountProgress.Counters.Lines := line2 - line1 + 1;

  {}// selection present, if caret is on the 1st char - excess line

  // total chars count
  isChars.ciChar1 := @crInit.ciMax;
  isChars.ciChar2 := @crInit.ciMin;
  isChars.bColumnSel := BOOL(-1);
  isChars.nNewLine := AELB_ASOUTPUT;
  CountProgress.Counters.Chars := SendMessage(CountData.hEditWnd, AEM_INDEXSUBTRACT, 0, LPARAM(@isChars));

  CountProgress.PercentDone := 100;
  if not RunCallback then Exit;

  // *** word count ***

  // init data
  CountProgress.PercentDone := 0;
  CharsProcessed := 0;
  WordCnt := 0; CharCnt := 0; // how much words/chars we've processed during current cycle

  // there's no selection
  if not Selection then
  begin
    ciCount := crInit.ciMin;

    repeat
      // returns number of characters skipped to the next word
      tmp := SendMessage(CountData.hEditWnd, AEM_GETNEXTBREAK, AEWB_RIGHTWORDEND, LPARAM(@ciCount));

      // EOF - finish the cycle
      if tmp = 0 then
      begin
        CountProgress.PercentDone := 100;
        Break;
      end;

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
        if AEC_IndexCompare(crCount.ciMin, crCount.ciMax) >= 0 then
        begin
          CountProgress.PercentDone := 100;
          Break;
        end;

        ciWordEnd := crCount.ciMin;
        repeat
          // returns number of characters skipped to the next word
          tmp := SendMessage(CountData.hEditWnd, AEM_GETNEXTBREAK, AEWB_RIGHTWORDEND, LPARAM(@ciWordEnd));
          // EOF - finish the cycle
          if tmp = 0 then
          begin
            CountProgress.PercentDone := 100;
            Break;
          end;

          // word ends beyond the selection - finish the *inner* cycle
          if not ((ciWordEnd.nLine = crCount.ciMin.nLine) and
                  (ciWordEnd.nCharInLine <= crCount.ciMin.lpLine.nSelEnd)) then Break;

          if IsFirst then
          begin
            IsFirst := False;
            ciWordStart := ciWordEnd;
            if SendMessage(CountData.hEditWnd, AEM_GETPREVBREAK, AEWB_LEFTWORDSTART, LPARAM(@ciWordStart)) <> 0 then
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
        tmp := SendMessage(CountData.hEditWnd, AEM_GETNEXTBREAK, AEWB_RIGHTWORDEND, LPARAM(@ciWordEnd));
        // EOF - finish the cycle
        if (tmp = 0) or (AEC_IndexCompare(ciWordEnd, crCount.ciMax) > 0) then
        begin
          CountProgress.PercentDone := 100;
          Break;
        end;

        if IsFirst then
        begin
          IsFirst := False;
          ciWordStart := ciWordEnd;
          if SendMessage(CountData.hEditWnd, AEM_GETPREVBREAK, AEWB_LEFTWORDSTART, LPARAM(@ciWordStart)) <> 0 then
            if AEC_IndexCompare(crCount.ciMin, ciWordStart) <= 0 then
              Inc(WordCnt);
        end
        else
          Inc(WordCnt);

        if AEC_IndexCompare(ciWordEnd, crCount.ciMax) = 0 then
        begin
          CountProgress.PercentDone := 100;
          Break;
        end;

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
  CountProgress.PercentDone := 0;
  CharsProcessed := 0;
  CharCnt := 0; // how much words/chars we've processed during current cycle

  repeat
    // EOF - finish the cycle
    if AEC_IndexCompare(ciCount, crInit.ciMax) >= 0 then
    begin
      CountProgress.PercentDone := 100;
      Break;
    end;

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
        //...
      end
      else                               // surrogate pair
      begin
        Inc(CountProgress.Counters.Surrogates);
        //... check for letters ...
      end;
    end
    else
    begin
      {
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
  CountData.Complete := True;
  ReportCharCount;
end;

// *** COUNTING THREAD ***

// Broker function allowing to use class method in the API calls
// lParameter = TCountThread object
function ThreadProc(lParameter: Pointer): DWORD; stdcall;
begin
  Result := TCountThread(lParameter).Execute;
end;

// broker function allowing to use class method as the GetDocInfo callback
procedure CountCallbackProc(var CountData: TCountData; var CountProgress: TCountProgress; var Continue: Boolean);
begin
  CountThread.CountCallback(CountData, CountProgress, Continue);
end;

// counting thread methods

procedure TCountThread.CountCallback(var CountData: TCountData; var CountProgress: TCountProgress; var Continue: Boolean);
begin
  if FState = stTerminated then
  begin
    // Send final count state
    CountProgress.Counters.Actual := False;
    SendMessage(FhTargetWnd, MSG_UPD_COUNT, WPARAM(FState), LPARAM(@CountProgress));
    Continue := False;
    Exit;
  end;

  // change the thread state if count process is completed
  if CountData.Complete then
    FState := stInactive;

  // send current counters and thread state to the main window
  SendMessage(FhTargetWnd, MSG_UPD_COUNT, WPARAM(FState), LPARAM(@CountProgress));

  Continue := True;
end;

// Launch the counting thread
procedure TCountThread.Run(TargetWnd: HWND; const CountData: TCountData; const FileStats: TFileStats);
begin
  if FState = stRunning then Exit;
  FCountData := CountData;
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
  GetDocInfo(FCountData, CountCallbackProc);

  // if exiting normally, return "OK", otherwise return error
  Result := IfTh(FState = stTerminated, DWORD(-1), 0);

  // clear the data
  CloseAndZeroHandle(FhThread);
  FidThread := 0;
  FhTargetWnd := 0;
  FState := stInactive;
  ZeroMem(FCountData, SizeOf(FCountData));
end;

// ***** DIALOGS ***** \\

// TMainDlg

constructor TMainDlg.Create(const pd: TPLUGINDATA);
var pg: TTabPage;
begin
  inherited Create(pd.hInstanceDLL, IDD_DLG_MAIN, pd.hMainWnd);

  FAppIcon := pd.hMainIcon;

  ItemText[IDC_BTN_OK] := LangString(idMainBtnOK);
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
var hwndTab, hwndPb: HWND;
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
        CountThread.Run(DlgHwnd, CountData, FileStats); // start counting
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
              if not FPages[pg].Show(SW_HIDE) then
                MsgBox(LastErrMsg);
              Res := LRESULT(False); // must return false to allow page change
            end;
          // page was changed, show the current page
          TCN_SELCHANGE:
            begin
              hwndTab := Item[NotifyHdr.idFrom];
              pg := TTabPage(SendMessage(hwndTab, TCM_GETCURSEL, 0, 0));
              if not FPages[pg].Show(SW_NORMAL) then
                MsgBox(LastErrMsg);
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
        // thread is not active, change button caption
        if TThreadState(wParam) <> stRunning then
          FPages[tabDoc].ItemText[IDC_BTN_STOP] := LangString(idPgDocBtnCount);

        // change values on the doc page only if it is visible
        if not IsWindowVisible(FPages[tabDoc].DlgHwnd) then Exit;
        FPages[tabDoc].SetValues;
        hwndPb := FPages[tabDoc].Item[IDC_PGB_PROCESS];
        // return progress bar to zero if thread is not active
        if TThreadState(wParam) <> stRunning then
          SendMessage(hwndPb, PBM_SETPOS, 0, 0)
        else
          SendMessage(hwndPb, PBM_SETPOS, pProgr.PercentDone, 0);

        Res := LRESULT(True);
      end;

    MSG_ICON_EXTRACTED:
      begin
        FileStats.hIcon := HICON(wParam);
        FPages[tabFile].SetValues;
      end;

    WM_COMMAND:
      begin
//  mlog.AddLine(mkinfo, 'main command');

      end;

  end; // case msg
end;

// TPageDlg

constructor TPageDlg.Create(const pd: TPLUGINDATA; Owner: TMainDlg; Page: TTabPage);
begin
  inherited Create(pd.hInstanceDLL, TabPageIDs[Page], pd.hMainWnd);
  FOwner := Owner;
  FPage := Page;
  case FPage of
    tabFile:
        SetItemData([
              ItemData(IDC_STC_FILEPATH, LangString(idPgFileLabelPath)),
              ItemData(IDC_STC_FILESIZE, LangString(idPgFileLabelSize)),
              ItemData(IDC_STC_CREATED,  LangString(idPgFileLabelCreated)),
              ItemData(IDC_STC_MODIFIED, LangString(idPgFileLabelModified)),

              ItemData(IDC_EDT_FILENAME, ''),
              ItemData(IDC_EDT_FILEPATH, ''),
              ItemData(IDC_EDT_CREATED, ''),
              ItemData(IDC_EDT_MODIFIED, '')
             ]);
    tabDoc:
        SetItemData([
              ItemData(IDC_STC_CODEPAGE,  LangString(idPgDocLabelCodePage)),
              ItemData(IDC_STC_LINES,     LangString(idPgDocLabelLines)),
              ItemData(IDC_STC_CHARS,     LangString(idPgDocLabelChars)),
              ItemData(IDC_STC_WORDS,     LangString(idPgDocLabelWords)),
              ItemData(IDC_STC_CHARSNOSP, LangString(idPgDocLabelCharsNoSp)),
              ItemData(IDC_STC_LATIN,     LangString(idPgDocLabelLatin)),
              ItemData(IDC_STC_LETTERS,   LangString(idPgDocLabelLetters)),
              ItemData(IDC_STC_SURR,      LangString(idPgDocLabelSurr)),
              ItemData(IDC_BTN_STOP,      LangString(idPgDocBtnCount)),

              ItemData(IDC_EDT_CODEPAGE,  ''),
              ItemData(IDC_EDT_LINES,     ''),
              ItemData(IDC_EDT_CHARS,     ''),
              ItemData(IDC_EDT_WORDS,     ''),
              ItemData(IDC_EDT_CHARSNOSP, ''),
              ItemData(IDC_EDT_LATIN,     ''),
              ItemData(IDC_EDT_LETTERS,   ''),
              ItemData(IDC_EDT_SURR,      '')
             ]);
  end;
end;

// set text values form FileStats record
procedure TPageDlg.SetValues;
var ShowWarnMsg: Boolean;
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
          ItemText[IDC_EDT_FILENAME] := ExtractFileName(FileStats.FileName);
          ItemText[IDC_EDT_FILEPATH] := FileStats.FileName;
          ItemText[IDC_EDT_FILESIZE] := IfTh(FileStats.FileSize <> 0, Format(LangString(idPgFileTextSizePatt), [ThousandsDivide(FileStats.FileSize)]), '');
          ItemText[IDC_EDT_CREATED]  := IfTh(FileStats.Created <> 0,  DateTimeToStr(FileStats.Created), '');
          ItemText[IDC_EDT_MODIFIED] := IfTh(FileStats.Modified <> 0, DateTimeToStr(FileStats.Modified), '');
          // the content is not saved to file
          if FileStats.FileName = '' then
          begin
            if hiErr = 0 then
              hiErr := LoadImage(GetModuleHandle(user32), MakeIntResource(ID_ERROR),
                                 IMAGE_ICON, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON),
                                 LR_DEFAULTCOLOR);
            SendMessage(Item[IDC_IMG_WARNMSG], STM_SETICON, WPARAM(hiErr), 0);
            ItemText[IDC_STC_WARNMSG] := LangString(idPgFileLabelWarnMsgNotAFile);
            ShowWarnMsg := True;
          end
          // there are some changes unsaved
          else if FileStats.IsModified then
          begin
            if hiWarn = 0 then
              hiWarn := LoadImage(GetModuleHandle(user32), MakeIntResource(ID_WARNING),
                                 IMAGE_ICON, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON),
                                 LR_DEFAULTCOLOR);
            SendMessage(Item[IDC_IMG_WARNMSG], STM_SETICON, WPARAM(hiWarn), 0);
            ItemText[IDC_STC_WARNMSG] := LangString(idPgFileLabelWarnMsgNotSaved);
            ShowWarnMsg := True;
          end
          else
            ShowWarnMsg := False;
          ShowWindow(Item[IDC_IMG_WARNMSG], IfTh(ShowWarnMsg, SW_NORMAL, SW_HIDE));
          ShowWindow(Item[IDC_STC_WARNMSG], IfTh(ShowWarnMsg, SW_NORMAL, SW_HIDE));
          FValuesWereSet := True;
        end;
        // update these values always
        SendMessage(Item[IDC_IMG_FILEICON], STM_SETICON, WPARAM(FileStats.hIcon), 0);
      end;
    tabDoc:
      begin
        if not FValuesWereSet then // some values might change (during count process) and some might not
        begin
          ItemText[IDC_EDT_CODEPAGE]  := IntToStr(FileStats.CodePage); {}
          FValuesWereSet := True;
        end;
        // update these values always
        ItemText[IDC_EDT_LINES]     := IfTh(FileStats.Counters.Actual, ThousandsDivide(FileStats.Counters.Lines));
        ItemText[IDC_EDT_CHARS]     := IfTh(FileStats.Counters.Actual, ThousandsDivide(FileStats.Counters.Chars));
        ItemText[IDC_EDT_WORDS]     := IfTh(FileStats.Counters.Actual, ThousandsDivide(FileStats.Counters.Words));
        ItemText[IDC_EDT_CHARSNOSP] := IfTh(FileStats.Counters.Actual, ThousandsDivide(FileStats.Counters.Chars - FileStats.Counters.WhiteSpaces));
        ItemText[IDC_EDT_LETTERS]   := IfTh(FileStats.Counters.Actual, ThousandsDivide(FileStats.Counters.Letters));
        ItemText[IDC_EDT_LATIN]     := IfTh(FileStats.Counters.Actual, ThousandsDivide(FileStats.Counters.Latin));
        ItemText[IDC_EDT_SURR]      := IfTh(FileStats.Counters.Actual, ThousandsDivide(FileStats.Counters.Surrogates));
      end;
  end;

end;

procedure TPageDlg.DoDialogProc(msg: UINT; wParam: WPARAM; lParam: LPARAM; out Res: LRESULT);
var
  hwndTab: HWND;
  TabClientArea: TRect;
begin
  case msg of
    // dialog is showing - set position inside tab page and load values to controls
    RDM_DLGOPENING:
      begin
        // calculate tab's client area
        hwndTab := ParentHwnd;
        GetClientRect(hwndTab, TabClientArea);
        SendMessage(hwndTab, TCM_ADJUSTRECT, Windows.WPARAM(False), Windows.LPARAM(@TabClientArea));
        SetWindowPos(DlgHwnd, 0, TabClientArea.Left, TabClientArea.Top,
                     TabClientArea.Right - TabClientArea.Left, TabClientArea.Bottom - TabClientArea.Top,
                     SWP_NOZORDER);
        SetValues;
      end;

    RDM_DLGCLOSED:
;
//        mlog.AddLine(mkinfo, itos(integer(sender)) + ' closed');

    // notification from control
    WM_COMMAND:
      case HiWord(wParam) of
        BN_CLICKED:
          case LOWORD(wParam) of
            // start/stop counting button
            IDC_BTN_STOP:
              if CountThread.State = stRunning then
              begin
                CountThread.Stop;
                ItemText[IDC_BTN_STOP] := LangString(idPgDocBtnCount);
                Res := LRESULT(True);
              end
              else
              begin
                FileStats.Counters.Actual := False;
                SetValues;
                CountThread.Run(FOwner.DlgHwnd, CountData, FileStats);
                ItemText[IDC_BTN_STOP] := LangString(idPgDocBtnAbort);
                Res := LRESULT(True);
              end;
          end;
      end;
  end;
end;

// ***** MAIN PLUGIN FUNCTIONS ***** \\

// initialize stuff with given PLUGINDATA members
procedure Init(var pd: TPLUGINDATA);
begin
  CurrLangId := PRIMARYLANGID(pd.wLangModule);
  ZeroMem(CountData, SizeOf(CountData));
  CountData.hMainWnd := pd.hMainWnd;
  CountData.hEditWnd := pd.hWndEdit;

  MainDlg := TMainDlg.Create(pd);
  MainDlg.Persistent := True;

  CountThread := TCountThread.Create;

  //...
end;

// do main work here
procedure Execute(var pd: TPLUGINDATA);
begin
  if not GetFileInfo(CountData, FileStats) then
  begin
    MsgBox(LangString(idMsgGetPropsFail), iStop);
    Exit;
  end;

  if MainDlg.ShowModal = -1 then
  begin
    MsgBox(LangString(idMsgShowDlgFail) + NL + LastErrMsg, iStop);
    Exit;
  end;
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

exports
  DllAkelPadID,
  Main;

begin
  DllProc := @CustomDllProc;
  CustomDllProc(DLL_PROCESS_ATTACH);
  IsMultiThread := True; // ! we create thread not with TThread so we have to set this flag
                         // manually to avoid troubles with memory manager
end.
